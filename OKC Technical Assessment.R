library(tidyverse)

shots_data <- read_csv("shots_data.csv")

shots_data <- shots_data |> 
  mutate(
    corner_threes = ifelse(abs(y) <= 7.8 & abs(x) >= 22.0, 1, 0),
    non_corner_threes = ifelse(abs(y) > 7.8 & abs(x) >= 23.75, 1, 0),
    two_pointers = ifelse(corner_threes != 1 & non_corner_threes != 1, 1, 0)
  )

shots_data |> 
  mutate(
    threeptmade = ifelse((corner_threes == 1 | non_corner_threes == 1) & fgmade == 1, 1, 0),
    zone = case_when(
      corner_threes == 1 ~ 'Corner',
      non_corner_threes == 1 ~ 'Non-Corner',
      two_pointers == 1 ~ 'Two-Pointers'
    )
  ) |> 
  group_by(team, zone) |> 
  summarise(
    FGA = sum(corner_threes + non_corner_threes + two_pointers),
    FGM = sum(fgmade),
    threepointers = sum(threeptmade),
    EFGperc = (FGM + (0.5*threepointers))/FGA
  )

shots_data |> 
  group_by(team) |> 
  summarise(
    total_FGA = sum(corner_threes + non_corner_threes + two_pointers),
    corner_three_perc = sum(corner_threes)/total_FGA,
    non_corner_three_perc = sum(non_corner_threes)/total_FGA,
    two_pointers_perc = sum(two_pointers)/total_FGA
  )




