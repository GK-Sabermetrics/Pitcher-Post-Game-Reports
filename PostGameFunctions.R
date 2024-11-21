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
      HB = mean(HB, na.rm = T) %>% round(2),
      IVB = mean(IVB, na.rm = T) %>% round(2),
      VAA = mean(VertApprAngle, na.rm = T) %>% round(2),
      HAA = mean(HorzApprAngle, na.rm = T) %>% round(2),
      Ext = mean(Extension, na.rm = T) %>% round(2)
    ) %>% 
    mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
    arrange(Pitch)
  
  pitch_movement_plot <- Pitch_Mov_Plot(pitcher_data = pitcher_data)
  
  pitch_release_plot = Pitch_Rel_Plot(pitcher_data = pitcher_data)
  
  #### Parameters ####
  params = list(
    pitcher = pitcher,
    date = date,
    opponent = opponent,
    game_stats = game_stats,
    pitch_metrics = pitch_metrics,
    pitch_movement_plot = pitch_movement_plot,
    pitch_release_plot = pitch_release_plot
  )
 
  rmarkdown::render(input = "PostGameReport.Rmd",
                    params = params) 
}

