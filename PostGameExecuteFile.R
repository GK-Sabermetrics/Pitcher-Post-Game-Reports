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
    ArmDeg = ifelse(PitcherThrows == 'Right', ArmDeg, 180-ArmDeg)
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

pitcher = "Gaspard, Jackson"
#date = "12/12/24"
opponent = 'My Balls'

# This script loads all the helper functions to create the reports
source('PostGameFunctions.R')
#

PostGameReport(gamefile = gamefile, pitcher)

