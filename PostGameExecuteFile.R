suppressWarnings(suppressMessages({
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
}))

file = read.csv("20241031-MercerUniversity-Private-1_unverified (1).csv")

gamefile = file %>% 
  filter(TaggedPitchType != 'Other' & PitcherTeam == "MER_BEA") %>% 
  mutate(
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs",
    Pitch = TaggedPitchType,
    Pitch = recode(Pitch, Fastball = "FB", TwoSeamFastBall = "2SFB", Sinker = 'SI', 
                   Cutter = 'CT', Splitter = 'SP', ChangeUp = 'CH', Slider = 'SL',
                   Curveball = 'CB', KnuckleBall = 'KC'),
    PitchCall = recode(PitchCall, BallCalled = 'Ball', BallinDirt = 'Ball',
                       FoulBallNotFieldable = 'Foul', FoulBallFieldable = 'Foul'),
    Top.Bottom = recode(Top.Bottom, Top = "T", Bottom = "B"),
    Inn = paste(Top.Bottom, Inning, sep = " "),
    KorBB = recode(KorBB, Strikeout = 'Strikeout', Walk = 'Walk', Undefined = ""),
    ArmRad = atan2(RelHeight, RelSide),
    ArmDeg = ArmRad * (180/pi),
    ArmDeg = ifelse(PitcherThrows == 'Right', ArmDeg, 180-ArmDeg),
    InZone = ifelse(between(PlateLocHeight, 1.3795,3.7205) & between(PlateLocSide, -.828, .828), 1, 0),
    Swing = ifelse(PitchCall %in% c('StrikeSwinging', 'Foul', 'InPlay'),1,0),
    FullStrike = ifelse(PitchCall %in% c('StrikeSwinging', 'Foul', 'StrikeCalled'),1,0),
    BBE = ifelse(PitchCall == 'InPlay' | PitchCall == 'Foul' & OutsOnPlay == 1, 1,0),
    FilterCol = ifelse(PitchCall == 'StrikeSwinging', 'Whiff', 'All')
  ) %>% 
  rename(
    PAOutcome = KorBB,
    PitchType = TaggedPitchType,
    HitType = TaggedHitType,
    Velo = RelSpeed,
    Spin = SpinRate,
    IVB = InducedVertBreak,
    HB = HorzBreak
  )

# Use this to run one report
pitcher = "Cole, Lawson"
opponent = 'Mercer'

#pitchers = unique(gamefile$Pitcher[gamefile$PitcherTeam == "MER_BEA"])

# This script loads all the helper functions to create the reports
source('PostGameFunctions.R')
#

#for (pitcher in pitchers) {
#
#  PostGameReport(gamefile = gamefile, pitcher)
#
#}

PostGameReport(gamefile = gamefile, pitcher)


