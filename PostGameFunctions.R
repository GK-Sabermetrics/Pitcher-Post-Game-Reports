library(tidyverse)
library(kableExtra)
library(gridExtra)
library(gtsummary)
library(pak)
library(kableExtra)
library(knitr)
library(pandoc)
library(scales)
library(ggrepel)
library(ggplot2)

Pitch_Mov_Plot = function(pitcher_data){
  
  pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
  
  pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))
  
  ggplot(pitcher_data, aes(x = HB, y = IVB)) +
    labs(title = "Pitch Movement" ,color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" )  +
    xlim(-25, 25) + ylim(-25, 25) +
    geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
    geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
    coord_fixed() +
    geom_point(aes(fill = Pitch), size = 3, alpha = .75, color = 'black', pch = 21) +
    scale_fill_manual(values = pcolors) +
    theme_bw() + theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank()) # axis.title = element_text(size = 6))
  
}

Pitch_Rel_Plot = function(pitcher_data){
  
  pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
  pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))
  
  PitchRelease = pitcher_data %>% 
    group_by(Pitch) %>% 
    summarise(
      RelHeight = mean(RelHeight*12, na.rm = T),
      RelSide = mean(RelSide*12, na.rm = T),
      ArmAng = mean(ArmDeg, na.rm = T)
    )
  
  # PITCHING RUBBER COORDS
  rubber_xmin <- -9 
  rubber_xmax <- 9   
  rubber_ymin <- 4   
  rubber_ymax <- 4.5 
  
  df <- data.frame(x = 0.5, y = 0)
  theta <- seq(0, pi, length.out = 100)  # Change the range to create a semi-circle that is right-side up for the mound!
  r <- 40  # The horizontal range from -40 to 40 for the mound!
  # Calculate the x and y coordinates
  mound <- data.frame(
    x = r * cos(theta),
    y = 4 * sin(theta)
  )

  ggplot(PitchRelease, aes(x = RelSide, y = RelHeight)) +
    xlim(-40,40) + ylim(0,80)+
    labs(title = "Pitch Release Points" ,color = "") +
    geom_rect(aes(xmin = rubber_xmin, xmax = rubber_xmax, ymin = rubber_ymin, ymax = rubber_ymax), 
              fill = "white", color = "black") +
    geom_polygon(data = mound, aes(x = x, y = y), fill = "#8B4513") +
    geom_segment(aes(x = 0, y = 60, xend = RelSide, yend = RelHeight), linetype = 2) +
    geom_point(aes(fill = Pitch), size = 4, alpha = .75, color = 'black', pch = 21) +
    geom_text_repel(aes(x = RelSide, y = RelHeight, label = paste(round(ArmAng,1),"º", sep = "")), size = 3) +
    scale_fill_manual(values = pcolors) +
    theme_bw() + theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank())

}

# Pitch Velocity
pitch_velo_breakdown = function(pitcher_data){
  
  pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
  pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))
  
  
  pitch_velo = pitcher_data %>% 
    group_by(Date, Pitcher, Pitch, Inning) %>% 
    summarise(
      Avg = mean(Velo, na.rm = T)
    )
  
  pitch_velo_plot = 
  ggplot(pitch_velo, aes(x = Inning, y = Avg, color = Pitch)) +
    geom_point(size = 2, alpha = .75) +
    scale_x_continuous(labels = as.numeric(pitch_velo$Inning), breaks = pitch_velo$Inning) +
    geom_line() +
    scale_color_manual(values = pcolors) +
    labs(title = "Velo by Inning", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + 
    theme_bw() + theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
    theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank())

}

# Pitch Distribution Plot 
Pitch_Dist_Plot = function(pitcher_data) {
  
  pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")
  
  Dist_Data = pitcher_data %>% 
    mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
    arrange(Pitch)
  
  pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
  pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))
  
  ggplot(Dist_Data) +
    geom_density(aes(x = Velo, fill = Pitch), alpha = .75) +
    geom_point(data = pitcher_data %>% group_by(Pitch) %>% filter(n() < 2), 
               aes(x = Velo, y = 0.03 ,fill = Pitch), color = 'black', size = 4, pch = 21, alpha = .7) +
    facet_wrap(~Pitch, ncol = 1, strip.position = 'left', scales = 'free_y') +
    scale_fill_manual(values = pcolors) +
    labs(title = "Pitch Velo Distribution" ,fill = "") + # , na.rm = TRUE)+
    theme_bw() + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      # strip.text = element_text(color = 'black', face = 'bold'),
      strip.text = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"),
      panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"),
      # panel.background = element_rect(fill = '#f5f5f5'),
      # plot.background = element_rect(fill = '#f5f5f5'),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5) #,
      # aspect.ratio = 1/3
    ) 
  
}

# Pitch Loc RHH

pitcher_loc_vs_rhh = function(pitcher_data){
  
  vs_rhh = pitcher_data %>% filter(BatterSide == "Right")
  
  pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
  pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))
  
  if(nrow(vs_rhh) > 0) {
    plp_rhh <- 
      ggplot(data = vs_rhh, 
             aes(x = PlateLocSide, y = PlateLocHeight, fill = Pitch)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs RHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(size =3, alpha = .75, pch = 21) +
      scale_fill_manual(values = pcolors) + # , na.rm = TRUE)+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      facet_wrap(~FilterCol, nrow = 1) +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      )
  } else {
    plp_rhh <- 
      ggplot(data = vs_rhh, 
             aes(x = PlateLocSide, y = PlateLocHeight, color = Pitch)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs RHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      annotate(x = 0, y = 2.5, geom = 'text', label = 'No Pitch Data')+
      # facet_wrap(~filter_col, nrow = 1)+
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      )
  }

}

# Pitch Loc vs LHH

pitcher_loc_vs_lhh = function(pitcher_data){
  
  vs_lhh = pitcher_data %>% filter(BatterSide == "Left")
  
  pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
  pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))
  
  if(nrow(vs_lhh) > 0) {
    plp_lhh <- 
      ggplot(data = vs_lhh, 
             aes(x = PlateLocSide, y = PlateLocHeight, fill = Pitch)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs LHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(size =3, alpha = .75, pch = 21) +
      scale_fill_manual(values = pcolors) + # , na.rm = TRUE)+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      facet_wrap(~FilterCol, nrow = 1) +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      )
  } else {
    plp_lhh <- 
      ggplot(data = vs_lhh, 
             aes(x = PlateLocSide, y = PlateLocHeight, color = Pitch)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs LHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      annotate(x = 0, y = 2.5, geom = 'text', label = 'No Pitch Data')+
      # facet_wrap(~filter_col, nrow = 1)+
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      )
  }
  
}

pitcher_pitch_breakdown <- function(pitcher_data){
  # CREATES ANOTHER USAGE BREAKDOWN
  
  pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
  pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))
  
  pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")
  
  test <- pitcher_data %>%
    mutate(filter_2 = ifelse(Count == "0-0", 'First Pitch',
                             ifelse(Strikes == 2, '2 Strikes',
                                    ifelse(Count %in% c('1-0','2-0','3-0','2-1','3-1'), 'Behind',
                                           ifelse(Count %in% c('0-1','0-2', '1-2' ), 'Ahead', ''
                                           ))  ))    
    )%>%
    filter(filter_2 != '') %>%
    group_by(Pitch, filter_2, BatterSide) %>%
    dplyr::summarise(P = n() ) %>%
    group_by(filter_2, BatterSide) %>%
    mutate(percentage = round(P / sum(P),3)*100) %>%
    mutate(BatterSide = gsub("Left",'LHH',BatterSide),
           BatterSide = gsub('Right','RHH',BatterSide)) %>% 
    mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
    arrange(Pitch)
  
  breakdown<-ggplot(test %>%
                      mutate(filter_2 = factor(filter_2, levels = c('First Pitch', '2 Strikes', 'Ahead', 'Behind') )), 
                    aes(x = "", y = percentage, fill = Pitch)) +
    geom_col(color = "black") +
    geom_text(aes(label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), fontface = 'bold', color = 'white', size = 3) +
    #  coord_polar(theta = "y")+
    theme_void()+
    theme(strip.text = element_text(size = 11, face = 'bold'))+
    facet_wrap(BatterSide~filter_2, nrow=2) + 
    theme(legend.position="none")+
    scale_fill_manual(values = pcolors)

}



PostGameReport = function(gamefile, pitcher){
  
  #### Data Pull ####
  pitcher_data = gamefile[gamefile$Pitcher == pitcher, ]
  
  date = unique(pitcher_data$Date)
  
  #### Game Stats ####
  game_stats = pitcher_data %>% 
    group_by(Date) %>% 
    summarise(
      'IPb' = ((sum(OutsOnPlay)+length(which(PAOutcome == 'Strikeout')))/3),
      'IP' = ifelse(IPb %% 1 == 0, IPb + 0.0, 
                    ifelse(between(IPb %% 1, .0, .34), IPb - .2333333, IPb -.4666666)) %>% as.numeric(),
      'P' = n(),
      'BF' = length(which(Count == "0-0")),
      'K' = length(which(PAOutcome == 'Strikeout')),
      'BB' = length(which(PAOutcome == 'Walk')),
      'HBP' = length(which(PitchCall == 'HitByPitch')),
      'BIP' = length(which(PitchCall == 'InPlay')),
      'H' = length(which(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun'))),
      'XBH' = length(which(PlayResult %in% c('Double', 'Triple', 'HomeRun'))),
      'R' = sum(RunsScored),
      'BAA' = sprintf((H / (BF - BB - HBP - sum(PlayResult =='Sacrifice'))), fmt = '%#.3f')
    ) %>% .[, -c(2)]
  
  pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")
  
  #### Pitch Metrics ####
  pitch_metrics = pitcher_data %>% 
    group_by(Pitch) %>% 
    summarise(
      "#" = n(),
      Usage = percent(n()/length(.$Pitch)),#3
      Max = floor(max(Velo, na.rm = TRUE)) %>% as.integer(),
      Avg = floor(mean(Velo, na.rm = TRUE)) %>% as.integer(),
      Spin = mean(Spin, na.rm = T) %>% as.integer(),
      Tilt = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
        as.numeric() %>% mean(na.rm = T) %>%
        as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
        format(format = "%k:%M", tz = 'UTC'),
      "Arm º" = mean(ArmDeg, na.rm = T) %>% round(1),
      HB = mean(HB, na.rm = T) %>% round(2),
      IVB = mean(IVB, na.rm = T) %>% round(2),
      VAA = mean(VertApprAngle, na.rm = T) %>% round(2),
      HAA = mean(HorzApprAngle, na.rm = T) %>% round(2),
      Ext = mean(Extension, na.rm = T) %>% round(2)
    ) %>% 
    mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
    arrange(Pitch)
  
  pitch_stats = 
   pitcher_data %>% 
    group_by(Pitch) %>% 
    summarise(
      "#" = n(),
      CStrk = length(which(PitchCall == 'StrikeCalled')),
      Swings = sum(Swing),
      Whiffs = length(which(PitchCall == 'StrikeSwinging')),
      'Zone%' = percent((length(which(InZone == 1)))/n()),
      'Strk%' = percent(sum(FullStrike)/n()),
      'Chase%' = percent(sum(InZone == 0 & Swing == 1)/n()),
      'Whiff%' = percent(Whiffs/Swings),
      'CSW%' = percent((CStrk + Whiffs)/n()),
      BIP = sum(PitchCall == 'InPlay', na.rm = T),
      'Avg EV' = mean(ExitSpeed[BBE==1], na.rm = T) %>% round(1),
      'Hard %' = percent(sum(ExitSpeed >= 90, na.rm = T)/sum(BBE,na.rm = T))
    ) %>% 
     mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
     arrange(Pitch)
  
  #### Usage R ####
  usage_r = 
  pitcher_data %>% 
    filter(BatterSide == 'Right') %>% 
    group_by(Pitch) %>% 
    summarise(
      '#' = n(),
      '%' = n(),
      '1P' = sum(PitchofPA == 1, na.rm = T),
      '1P%' = sum(`1P`),
      '2K' = sum(Strikes == 2, na.rm = T),
      '2K%' = sum(`2K`),
      'Strk%' = percent(sum(FullStrike)/n()),
      'Whiff%' = percent(sum(PitchCall == 'StrikeSwinging', na.rm = T)/sum(Swing))
    ) %>% 
    mutate(`%` = percent(`%`/sum(`%`)),
           `1P%` = percent(`1P%`/sum(`1P%`)),
           `2K%` = percent(`2K%`/sum(`2K%`))) %>% 
    mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
    arrange(Pitch) %>% select(-`1P`, -`2K`)
  
  #### Usage L ####
  usage_l = 
    pitcher_data %>% 
    filter(BatterSide == 'Left') %>% 
    group_by(Pitch) %>% 
    summarise(
      '#' = n(),
      '%' = n(),
      '1P' = sum(PitchofPA == 1, na.rm = T),
      '1P%' = sum(`1P`),
      '2K' = sum(Strikes == 2, na.rm = T),
      '2K%' = sum(`2K`),
      'Strk%' = percent(sum(FullStrike)/n()),
      'Whiff%' = percent(sum(PitchCall == 'StrikeSwinging', na.rm = T)/sum(Swing))
    ) %>% 
    mutate(`%` = percent(`%`/sum(`%`)),
           `1P%` = percent(`1P%`/sum(`1P%`)),
           `2K%` = percent(`2K%`/sum(`2K%`))) %>% 
    mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
    arrange(Pitch) %>% select(-`1P`, -`2K`)
  
  pitch_movement_plot <- Pitch_Mov_Plot(pitcher_data = pitcher_data)
  
  pitch_release_plot = Pitch_Rel_Plot(pitcher_data = pitcher_data)
  
  pitch_velo_plot = pitch_velo_breakdown(pitcher_data = pitcher_data)
  
  pitch_dist_plot = Pitch_Dist_Plot(pitcher_data = pitcher_data)
  
  plp_rhh = pitcher_loc_vs_rhh(pitcher_data = pitcher_data)
  
  plp_lhh = pitcher_loc_vs_lhh(pitcher_data = pitcher_data)
  
  breakdown = pitcher_pitch_breakdown(pitcher_data = pitcher_data)
  
  #### Parameters ####
  params = list(
    pitcher = pitcher,
    date = date,
    opponent = opponent,
    game_stats = game_stats,
    usage_r = usage_r,
    usage_l = usage_l,
    pitch_metrics = pitch_metrics,
    pitch_stats = pitch_stats,
    pitch_movement_plot = pitch_movement_plot,
    pitch_release_plot = pitch_release_plot,
    pitch_velo_plot = pitch_velo_plot,
    pitch_dist_plot = pitch_dist_plot,
    plp_rhh = plp_rhh,
    plp_lhh = plp_lhh,
    breakdown = breakdown
  )
 
  file_date = pitcher_data$Date[1]
  
  p = gsub(", ","-", pitcher)
  
  opp = opponent
  
  rmarkdown::render(input = "PostGameReport.Rmd",
                    output_file = paste0(file_date, '_', p, '_vs_', opp,'.pdf'),
                    params = params,
                    output_dir = "/Users/garrettkemp/Documents/Mercer Baseball/Reports/Pitcher Reports",
                    quiet = TRUE) 
}


