# this will be the placeholder file for processing shot data and then visualizing all shots taken for each team and constructing global and local PWHL xShots plots


#Libraries

#--------------------------------Libraries
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(fastRhockey)
library(rvest)
library(ggpubr)
library(patchwork)
library(ggtext)
library(ggh4x)
library(tidylog)
library(ggplot2)
library(sportyR)
library(dplyr)
library(zoo)
library(purrr)
library(readr)
library(tidyr)


col_scheme <- "brighter" #choose "official", "brighter" or "colourblind"
`%notin%` <- negate(`%in%`)

if (col_scheme == "official") {
  team_colours <- structure(c("#154734", "#2E1A47", "#862633", "#00B2A9", "#A6192E", "#307FE2"), .Names = c("Boston", "Minnesota", "Montreal", "New York", "Ottawa", "Toronto"))
  
} else if (col_scheme == "colourblind") {
  #colourblind palette created using this tool https://davidmathlogic.com/colorblind/#%23007326-%2300DCB7-%232AAFEC-%23FF483A-%236F2AAB-%23730606
  team_colours <- structure(c("#007326", "#6F2AAB", "#730606", "#00DCB7", "#FF483A", "#2AAFEC"), .Names = c("Boston", "Minnesota", "Montreal", "New York", "Ottawa", "Toronto"))
} else if (col_scheme == "brighter") {
  #colourblind palette created using this tool https://davidmathlogic.com/colorblind/#%23007326-%2300DCB7-%232AAFEC-%23FF483A-%236F2AAB-%23730606
  team_colours <- structure(c("#1C5940", "#352056", "#6D1620", "#00B2A9", "#D31818", "#307FE2"), .Names = c("Boston", "Minnesota", "Montreal", "New York", "Ottawa", "Toronto"))
}

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
pwhl_logo <- get_png(here("data/pwhl_logo.png"))




#--------------------------------Data

games <- pwhl_schedule(2024) %>%
  as.tibble(.) %>%
  mutate(game_num = row_number(),
         game_played = ifelse(str_detect(game_status, "Final"), TRUE, FALSE),
         game_date = lubridate::ymd(paste0("2024-", str_sub(game_date, 6)))) %>%
  mutate(across(c(game_id, home_team_id, away_team_id, home_score, away_score), as.numeric)) %>%
  arrange(game_id)

game_results <- games %>%
  mutate(
    winning_team_id = ifelse(home_score > away_score, home_team_id, away_team_id),
    home_team_win = ifelse(home_score > away_score, TRUE, FALSE)
  ) %>%
  select(game_id, home_team_id, away_team_id, winning_team_id, home_team_win) 

write_csv(game_results, here("data/game_results.csv"))
  

if (file.exists(here("data/game_plays.csv"))) {
  game_plays <- read_csv(here("data/game_plays.csv"))  %>%
    mutate(time_of_period = str_remove(time_of_period, ":00$"))
} else {
  game_plays <- tibble()
  for (i in 1:nrow(games[games$game_played==TRUE,])) {
    id <- as.numeric(games$game_id[[i]])
    temp <- pwhl_pbp(id) %>%
      mutate(across(c(period_of_game, empty_net, game_winner, penalty_shot, insurance, short_handed, penalty_length), as.numeric))
    game_plays <- game_plays %>%
      bind_rows(temp) 
  }
  game_plays <- game_plays %>%
    mutate(game_date = mdy(paste(game_date, ", 2024"))) %>%  
    arrange(game_date)
   write_csv(game_plays, here("data/game_plays.csv"))
}




# FIRST GATHER TIBBLES FOR EXPECTED GOAL AND PENALTIES

pp_minutes <- game_plays %>%
  filter(event == "penalty") %>%  # Focus on penalty events only
  group_by(game_id, game_date, home_team_id, away_team_id) %>%
  summarize(
    home_pp_min_for = sum(ifelse(team_id == away_team_id, penalty_length, 0)),  # Penalty length is against the home team
    away_pp_min_for = sum(ifelse(team_id == home_team_id, penalty_length, 0))
  ) %>%
  ungroup()



PWHL_shots_2024 <- read_csv(here("data/PWHL_shots_2024"))

# Subtract 100 from all x_coord values
PWHL_shots_2024$x_coord_right <- PWHL_shots_2024$x_coord_right - 100
PWHL_shots_2024$y_coord_right <- PWHL_shots_2024$y_coord_right - 85/2

y_net <- 0
x_net <- 89
x_region_345_circle <- 120
y_region_345_circle <- 0
x_region_678_circle <- 115
y_region_678_circle <- 0


#provided xG for each region via analysis
# region        xG 
# 0    region_1  0.084842
# 1    region_2  0.189633
# 2    region_3  0.020739
# 3    region_4  0.134533
# 4    region_5  0.059088
# 5    region_6  0.018506
# 6    region_7  0.058387
# 7    region_8  0.082456
# 8    region_9  0.033699
# 9   region_10  0.025245
# 10  region_11  0.042244
# 11  region_12  0.043209
# 12  region_13  0.062576
# 13  region_14  0.010337
region_xg_values <- c(
  region_1_xG = 0.084842,
  region_2_xG = 0.189633,
  region_3_xG = 0.020739,
  region_4_xG = 0.134533,
  region_5_xG = 0.059088,
  region_6_xG = 0.018506,
  region_7_xG = 0.058387,
  region_8_xG = 0.082456,
  region_9_xG = 0.033699,
  region_10_xG = 0.025245,
  region_11_xG = 0.042244,
  region_12_xG = 0.043209,
  region_13_xG = 0.062576,
  region_14_xG = 0.010337
)

game_shot_summary <- PWHL_shots_2024 %>%
  mutate(distance = sqrt((x_coord_right-x_net)^2+(y_coord_right)^2),
         distance_345 = sqrt((x_coord_right-x_region_345_circle)^2+(y_coord_right)^2),
         distance_789 = sqrt((x_coord_right-x_region_678_circle)^2+(y_coord_right)^2),
         angle = atan(y_coord_right/(x_coord_right-x_net))*180/pi,
         region_1 = ifelse(
           x_coord_right > x_net, TRUE, FALSE),
         region_2 = ifelse(
           distance < 5 & region_1 == FALSE, TRUE, FALSE),
         region_3 = ifelse(
           !region_1 & !region_2 & 
             distance_345 < 50 &
             y_coord_right >= -0.765*x_coord_right+73.085, TRUE, FALSE),
         region_4 = ifelse(
           !region_1 & !region_2 & !region_3 &
             distance_345 < 50 &
             y_coord_right >= 0.765*x_coord_right-73.085, TRUE, FALSE),
         region_5 = ifelse(
           !region_1 & !region_2 & !region_3 & !region_4 &
             distance_345 < 50, TRUE, FALSE),
         region_6 = ifelse(
           distance_345 > 50 & 
             y_coord_right >= -0.765*x_coord_right+73.085, TRUE, FALSE),
         region_7 = ifelse(
           !region_6 & distance_345 > 50 & distance_789 < 70 &
             y_coord_right >= -0.332*x_coord_right+29.548, TRUE, FALSE),
         region_8 = ifelse(
           !region_6 & !region_7 & distance_345 > 50 & distance_789 < 70 &
             y_coord_right >= 0.332*x_coord_right-29.548, TRUE, FALSE),
         region_9 = ifelse(
           !region_6 & !region_7 & !region_8 & distance_345 > 50 & distance_789 < 70 &
             y_coord_right < 0.332*x_coord_right-29.548 &
             y_coord_right >= 0.765*x_coord_right-73.085, TRUE, FALSE),
         region_10 = ifelse(
           distance_345 > 50 & 
             y_coord_right < 0.765*x_coord_right-73.085, TRUE, FALSE),
         region_11 = ifelse(
           !region_6 & distance_789 > 70 & x_coord_right > 25 &
             y_coord_right >= -0.332*x_coord_right+29.548, TRUE, FALSE),
         region_12 = ifelse(
           distance_789 > 70 & x_coord_right > 25 &
             y_coord_right < -0.332*x_coord_right+29.548 &
             y_coord_right >= 0.332*x_coord_right-29.548, TRUE, FALSE),
         region_13 = ifelse(
           !region_10 & distance_789 > 70 & x_coord_right > 25 &
             y_coord_right < 0.332*x_coord_right-29.548, TRUE, FALSE),
         region_14 = ifelse(x_coord_right <= 25, TRUE, FALSE)
  ) %>%
  mutate(expected_goals = case_when(
    region_1 == TRUE ~ region_xg_values["region_1_xG"],
    region_2 == TRUE ~ region_xg_values["region_2_xG"],
    region_3 == TRUE ~ region_xg_values["region_3_xG"],
    region_4 == TRUE ~ region_xg_values["region_4_xG"],
    region_5 == TRUE ~ region_xg_values["region_5_xG"],
    region_6 == TRUE ~ region_xg_values["region_6_xG"],
    region_7 == TRUE ~ region_xg_values["region_7_xG"],
    region_8 == TRUE ~ region_xg_values["region_8_xG"],
    region_9 == TRUE ~ region_xg_values["region_9_xG"],
    region_10 == TRUE ~ region_xg_values["region_10_xG"],
    region_11 == TRUE ~ region_xg_values["region_11_xG"],
    region_12 == TRUE ~ region_xg_values["region_12_xG"],
    region_13 == TRUE ~ region_xg_values["region_13_xG"],
    region_14 == TRUE ~ region_xg_values["region_14_xG"]
  )) %>%
  group_by(game_id, game_date, home_team_id, away_team_id) %>%
  summarize(
    home_shots = sum(ifelse(team_id == home_team_id, 1, 0)),
    home_shots_xG = sum(ifelse(team_id == home_team_id, expected_goals, 0)),
    home_goals = sum(ifelse(team_id == home_team_id & goal == TRUE, 1, 0)),
    away_shots = sum(ifelse(team_id == away_team_id, 1, 0)),
    away_shots_xG = sum(ifelse(team_id == away_team_id, expected_goals, 0)),
    away_goals = sum(ifelse(team_id == away_team_id & goal == TRUE, 1, 0))
  ) %>%
  ungroup() 




#### NOW GOING TO LOOK AT EXPECTED GOALS  ON EV AND PP####
game_shot_summary_PP_EV <- PWHL_shots_2024 %>%
  mutate(distance = sqrt((x_coord_right-x_net)^2+(y_coord_right)^2),
         distance_345 = sqrt((x_coord_right-x_region_345_circle)^2+(y_coord_right)^2),
         distance_789 = sqrt((x_coord_right-x_region_678_circle)^2+(y_coord_right)^2),
         angle = atan(y_coord_right/(x_coord_right-x_net))*180/pi,
         region_1 = ifelse(
           x_coord_right > x_net, TRUE, FALSE),
         region_2 = ifelse(
           distance < 5 & region_1 == FALSE, TRUE, FALSE),
         region_3 = ifelse(
           !region_1 & !region_2 & 
             distance_345 < 50 &
             y_coord_right >= -0.765*x_coord_right+73.085, TRUE, FALSE),
         region_4 = ifelse(
           !region_1 & !region_2 & !region_3 &
             distance_345 < 50 &
             y_coord_right >= 0.765*x_coord_right-73.085, TRUE, FALSE),
         region_5 = ifelse(
           !region_1 & !region_2 & !region_3 & !region_4 &
             distance_345 < 50, TRUE, FALSE),
         region_6 = ifelse(
           distance_345 > 50 & 
             y_coord_right >= -0.765*x_coord_right+73.085, TRUE, FALSE),
         region_7 = ifelse(
           !region_6 & distance_345 > 50 & distance_789 < 70 &
             y_coord_right >= -0.332*x_coord_right+29.548, TRUE, FALSE),
         region_8 = ifelse(
           !region_6 & !region_7 & distance_345 > 50 & distance_789 < 70 &
             y_coord_right >= 0.332*x_coord_right-29.548, TRUE, FALSE),
         region_9 = ifelse(
           !region_6 & !region_7 & !region_8 & distance_345 > 50 & distance_789 < 70 &
             y_coord_right < 0.332*x_coord_right-29.548 &
             y_coord_right >= 0.765*x_coord_right-73.085, TRUE, FALSE),
         region_10 = ifelse(
           distance_345 > 50 & 
             y_coord_right < 0.765*x_coord_right-73.085, TRUE, FALSE),
         region_11 = ifelse(
           !region_6 & distance_789 > 70 & x_coord_right > 25 &
             y_coord_right >= -0.332*x_coord_right+29.548, TRUE, FALSE),
         region_12 = ifelse(
           distance_789 > 70 & x_coord_right > 25 &
             y_coord_right < -0.332*x_coord_right+29.548 &
             y_coord_right >= 0.332*x_coord_right-29.548, TRUE, FALSE),
         region_13 = ifelse(
           !region_10 & distance_789 > 70 & x_coord_right > 25 &
             y_coord_right < 0.332*x_coord_right-29.548, TRUE, FALSE),
         region_14 = ifelse(x_coord_right <= 25, TRUE, FALSE)
  ) %>%
  mutate(expected_goals = case_when(
    region_1 == TRUE ~ region_xg_values["region_1_xG"],
    region_2 == TRUE ~ region_xg_values["region_2_xG"],
    region_3 == TRUE ~ region_xg_values["region_3_xG"],
    region_4 == TRUE ~ region_xg_values["region_4_xG"],
    region_5 == TRUE ~ region_xg_values["region_5_xG"],
    region_6 == TRUE ~ region_xg_values["region_6_xG"],
    region_7 == TRUE ~ region_xg_values["region_7_xG"],
    region_8 == TRUE ~ region_xg_values["region_8_xG"],
    region_9 == TRUE ~ region_xg_values["region_9_xG"],
    region_10 == TRUE ~ region_xg_values["region_10_xG"],
    region_11 == TRUE ~ region_xg_values["region_11_xG"],
    region_12 == TRUE ~ region_xg_values["region_12_xG"],
    region_13 == TRUE ~ region_xg_values["region_13_xG"],
    region_14 == TRUE ~ region_xg_values["region_14_xG"]
  )) %>%
  group_by(game_id, game_date, home_team_id, away_team_id) %>%
  summarize(
    home_shots = sum(ifelse(team_id == home_team_id, 1, 0)),
    home_shots_xG = sum(ifelse(team_id == home_team_id, expected_goals, 0)),
    home_shots_xG_PP = sum(ifelse(team_id == home_team_id & power_play == 1 & !is.na(power_play), expected_goals, 0)),
    home_shots_xG_EV = sum(ifelse(team_id == home_team_id & (power_play != 1 | is.na(power_play)), expected_goals, 0)),
    home_goals = sum(ifelse(team_id == home_team_id & goal == TRUE, 1, 0)),
    away_shots = sum(ifelse(team_id == away_team_id, 1, 0)),
    away_shots_xG = sum(ifelse(team_id == away_team_id, expected_goals, 0)),
    away_shots_xG_PP = sum(ifelse(team_id == away_team_id & power_play == 1 & !is.na(power_play), expected_goals, 0)),
    away_shots_xG_EV = sum(ifelse(team_id == away_team_id & (power_play != 1 | is.na(power_play)), expected_goals, 0)),
    away_goals = sum(ifelse(team_id == away_team_id & goal == TRUE, 1, 0))
  ) %>%
  ungroup() 













#### FIRST, WE FILL LOOK AT SHARE OF MAXIMUM ALLOTTED POWER PLAY TIME ####
generate_weighted_pp_share_percent <- function(team_id) {
  pp_minutes %>%
    filter(home_team_id == team_id | away_team_id == team_id) %>%
    mutate(
      team_id = team_id,
      pp_min_for = ifelse(home_team_id == team_id, home_pp_min_for, away_pp_min_for),
      pp_min_against = ifelse(home_team_id == team_id, away_pp_min_for, home_pp_min_for)
    ) %>%
    arrange(game_date) %>%
    mutate(
      # Calculate the weighted shooting percentage for all previous games
      weighted_pp_share_percentage = map_dbl(1:n(), ~ {
        game_window <- 1:.x  # Consider all games up to the current game
        weights <- seq_along(game_window)  # Generate linear weights (1, 2, ..., n)
        weighted_pp_min_for <- sum(pp_min_for[game_window] * weights)
        weighted_pp_min_against <- sum(pp_min_against[game_window] * weights)
        weighted_pp_share_percentage <- weighted_pp_min_for / (weighted_pp_min_for + weighted_pp_min_against)
        return(weighted_pp_share_percentage)
      })
    ) %>%
    # filter(!is.na(rolling_pp_min_for_percent)) %>%
    # select(team_id, game_id, weighted_pp_share_percentage, pp_min_for, pp_min_against)
    select(game_date, game_id, team_id, weighted_pp_share_percentage)
}

# Generate tibbles for teams 2 to 6 and store them in a list
team_ids <- 1:6
weighted_pp_share_percent_list <- map(team_ids, generate_weighted_pp_share_percent)

# Assign each tibble to a variable dynamically
names(weighted_pp_share_percent_list) <- paste0("weighted_pp_share_percentage_team_", team_ids)

list2env(weighted_pp_share_percent_list, envir = .GlobalEnv)

# Save each team's data to a CSV file
walk2(
  weighted_pp_share_percent_list,
  paste0(here("data/weighted_pp_share_percentage_team_"), team_ids, ".csv"),
  write_csv
)






############ NOW ONTO SHOT AND SAVE % CSVs ##################
generate_weighted_shooting_percent <- function(team_id) {
  game_shot_summary %>%
    filter(home_team_id == team_id | away_team_id == team_id) %>%
    mutate(
      team_id = team_id,
      shots_for = ifelse(home_team_id == team_id, home_shots, away_shots),
      goals_for = ifelse(home_team_id == team_id, home_goals, away_goals)
    ) %>%
    arrange(game_date) %>%
    mutate(
      # Calculate the weighted shooting percentage considering shot volume
      weighted_shooting_percentage = map_dbl(1:n(), ~ {
        game_window <- 1:.x  # Consider all games up to the current game
        weights <- seq_along(game_window)  # Generate linear weights (1, 2, ..., n)
        
        # Calculate the weighted totals for goals and shots
        weighted_goals <- sum(goals_for[game_window] * weights)
        weighted_shots <- sum(shots_for[game_window] * weights)
        
        # Compute the weighted shooting percentage
        weighted_shooting_percentage <- weighted_goals / weighted_shots
        return(weighted_shooting_percentage)
      })
    ) %>%
    # select(team_id, game_id, weighted_shooting_percentage, shots_for, goals_for)
    select(game_date, game_id, team_id, weighted_shooting_percentage)
}

# Generate weighted shooting percentage for teams 1 to 6
team_ids <- 1:6
weighted_shooting_percent_list <- map(team_ids, generate_weighted_shooting_percent)

# Assign each tibble to a variable dynamically
names(weighted_shooting_percent_list) <- paste0("weighted_shooting_percentage_team_", team_ids)

list2env(weighted_shooting_percent_list, envir = .GlobalEnv)

# Save each team's data to a CSV file
walk2(
  weighted_shooting_percent_list,
  paste0(here("data/weighted_shooting_percentage_team_"), team_ids, ".csv"),
  write_csv
)





##### SAVE PERCENTAGE #####
generate_weighted_save_percent <- function(team_id) {
  game_shot_summary %>%
    filter(home_team_id == team_id | away_team_id == team_id) %>%
    mutate(
      team_id = team_id,
      shots_against = ifelse(home_team_id == team_id, away_shots, home_shots),
      goals_against = ifelse(home_team_id == team_id, away_goals, home_goals),
      save_percentage = 1 - (goals_against / shots_against)  # Calculate save percentage for each game
    ) %>%
    arrange(game_date) %>%
    mutate(
      # Calculate the weighted save percentage for all previous games
      weighted_save_percentage = map_dbl(1:n(), ~ {
        game_window <- 1:.x  # Consider all games up to the current game
        weights <- seq_along(game_window)  # Generate linear weights (1, 2, ..., n)
        weighted_goals_against <- sum(goals_against[game_window] * weights)
        weighted_shots_against <- sum(shots_against[game_window] * weights)
        weighted_save_percentage <- 1 - (weighted_goals_against/weighted_shots_against)
        return(weighted_save_percentage)
      })
    ) %>%
    # select(team_id, game_id, weighted_save_percentage, shots_against, goals_against)
    select(game_date, game_id, team_id, weighted_save_percentage)
}

# Generate weighted save percentage for teams 1 to 6
team_ids <- 1:6
weighted_save_percent_list <- map(team_ids, generate_weighted_save_percent)

# Assign each tibble to a variable dynamically
names(weighted_save_percent_list) <- paste0("weighted_save_percentage_team_", team_ids)

list2env(weighted_save_percent_list, envir = .GlobalEnv)

# Save each team's data to a CSV file
walk2(
  weighted_save_percent_list,
  paste0(here("data/weighted_save_percentage_team_"), team_ids, ".csv"),
  write_csv
)



#### SHOT ATTEMPT FOR PERCENTAGE ####
generate_weighted_shots_for_percent <- function(team_id) {
  game_shot_summary %>%
    filter(home_team_id == team_id | away_team_id == team_id) %>%
    mutate(
      team_id = team_id,
      shots_for = ifelse(home_team_id == team_id, home_shots, away_shots),
      shots_against = ifelse(home_team_id == team_id, away_shots, home_shots),
      shots_for_percentage = shots_for/(shots_for+shots_against)  # Calculate shots for percentage for each game
    ) %>%
    arrange(game_date) %>%
    mutate(
      # Calculate the weighted save percentage for all previous games
      weighted_shots_for_percentage = map_dbl(1:n(), ~ {
        game_window <- 1:.x  # Consider all games up to the current game
        weights <- seq_along(game_window)  # Generate linear weights (1, 2, ..., n)
        weighted_shots_for <- sum(shots_for[game_window] * weights)
        weighted_shots_against <- sum(shots_against[game_window] * weights)
        weighted_shots_for_percentage <- weighted_shots_for / (weighted_shots_against + weighted_shots_for)
        return(weighted_shots_for_percentage)
      })
    ) %>%
    # select(team_id, game_id, weighted_shots_for_percentage, shots_for, shots_against)
   select(game_date, game_id, team_id, weighted_shots_for_percentage)
  
}

# Generate weighted save percentage for teams 1 to 6
team_ids <- 1:6
weighted_shots_for_percent_list <- map(team_ids, generate_weighted_shots_for_percent)

# Assign each tibble to a variable dynamically
names(weighted_shots_for_percent_list) <- paste0("weighted_shots_for_percentage_team_", team_ids)

list2env(weighted_shots_for_percent_list, envir = .GlobalEnv)

# Save each team's data to a CSV file
walk2(
  weighted_shots_for_percent_list,
  paste0(here("data/weighted_shots_for_percentage_team_"), team_ids, ".csv"),
  write_csv
)






#### EXPECTED GOAL POWER PLAY PERCENTAGE ####
generate_weighted_xG_PP_percent <- function(team_id) {
  game_shot_summary_PP_EV %>%
    filter(home_team_id == team_id | away_team_id == team_id) %>%
    mutate(
      team_id = team_id,
      xG_PP_for = ifelse(home_team_id == team_id, home_shots_xG_PP, away_shots_xG_PP),
      xG_PK_against = ifelse(home_team_id == team_id, away_shots_xG_PP, home_shots_xG_PP)
      ) %>%
    arrange(game_date) %>%
    mutate(
      # Calculate the weighted xG PP percentage for all previous games
      weighted_xG_PP_percentage = map_dbl(1:n(), ~ {
        game_window <- 1:.x  # Consider all games up to the current game
        weights <- seq_along(game_window)  # Generate linear weights (1, 2, ..., n)
        weighted_xG_PP_for <- sum(xG_PP_for[game_window] * weights)
        weighted_xG_PK_against <- sum(xG_PK_against[game_window] * weights)
        weighted_xG_PP_percentage <- weighted_xG_PP_for / (weighted_xG_PP_for + weighted_xG_PK_against)
        return(weighted_xG_PP_percentage)
      })
    ) %>%
    # select(team_id, game_id, weighted_xG_PP_percentage, xG_PP_for, xG_PK_against)
    select(game_date, game_id, team_id, weighted_xG_PP_percentage)
  
}

# Generate weighted save percentage for teams 1 to 6
team_ids <- 1:6
weighted_xG_PP_percent_list <- map(team_ids, generate_weighted_xG_PP_percent)

# Assign each tibble to a variable dynamically
names(weighted_xG_PP_percent_list) <- paste0("weighted_xG_PP_percentage_team_", team_ids)

list2env(weighted_xG_PP_percent_list, envir = .GlobalEnv)

# Save each team's data to a CSV file
walk2(
  weighted_xG_PP_percent_list,
  paste0(here("data/weighted_xG_PP_percentage_team_"), team_ids, ".csv"),
  write_csv
)





#### EXPECTED GOAL EVEN STRENGTH PERCENTAGE ####
generate_weighted_xG_EV_percent <- function(team_id) {
  game_shot_summary_PP_EV %>%
    filter(home_team_id == team_id | away_team_id == team_id) %>%
    mutate(
      team_id = team_id,
      xG_EV_for = ifelse(home_team_id == team_id, home_shots_xG_EV, away_shots_xG_EV),
      xG_EV_against = ifelse(home_team_id == team_id, away_shots_xG_EV, home_shots_xG_EV)
    ) %>%
    arrange(game_date) %>%
    mutate(
      # Calculate the weighted xG PP percentage for all previous games
      weighted_xG_EV_percentage = map_dbl(1:n(), ~ {
        game_window <- 1:.x  # Consider all games up to the current game
        weights <- seq_along(game_window)  # Generate linear weights (1, 2, ..., n)
        weighted_xG_EV_for <- sum(xG_EV_for[game_window] * weights)
        weighted_xG_EV_against <- sum(xG_EV_against[game_window] * weights)
        weighted_xG_EV_percentage <- weighted_xG_EV_for / (weighted_xG_EV_for + weighted_xG_EV_against)
        return(weighted_xG_EV_percentage)
      })
    ) %>%
    # select(team_id, game_id, weighted_xG_EV_percentage, xG_EV_for, xG_EV_against)
    select(game_date, game_id, team_id, weighted_xG_EV_percentage)
  
}

# Generate weighted save percentage for teams 1 to 6
team_ids <- 1:6
weighted_xG_EV_percent_list <- map(team_ids, generate_weighted_xG_EV_percent)

# Assign each tibble to a variable dynamically
names(weighted_xG_EV_percent_list) <- paste0("weighted_xG_EV_percentage_team_", team_ids)

list2env(weighted_xG_EV_percent_list, envir = .GlobalEnv)

# Save each team's data to a CSV file
walk2(
  weighted_xG_EV_percent_list,
  paste0(here("data/weighted_xG_EV_percentage_team_"), team_ids, ".csv"),
  write_csv
)
































# 
# library(dplyr)
# library(purrr)
# library(readr)
# 
# # Define the team IDs
# team_ids <- 1:6
# 
# # Define the statistic names
# statistic_names <- c("shooting_percentage", "save_percentage", "xG_EV_percentage", "xG_PP_percentage", "shots_for_percentage", "pp_share_percentage")
# 
# # Read the game results
# game_results <- read_csv("data/game_results.csv", col_types = cols())
# 
# # Function to read and combine the statistics for one team
# combine_team_statistics <- function(team_id) {
#   # Read in each statistic CSV for the given team
#   statistic_data <- map(statistic_names, function(stat) {
#     file_path <- paste0("data/weighted_", stat, "_team_", team_id, ".csv")
#     read_csv(file_path, col_types = cols()) %>%
#       select(game_date, game_id, team_id, paste0("weighted_", stat)) %>% # Select relevant columns 
#       mutate(team_id = as.integer(team_id)) # Ensure team_id is of integer type
#   })
#   
#   # Combine all the statistics by game_date and game_id, avoiding duplicates
#   combined_data <- reduce(statistic_data, left_join, by = c("game_date", "game_id", "team_id"))
#   
#   # Add the win column by joining with game_results
#   combined_data_with_results <- combined_data %>%
#     left_join(game_results, by = "game_id") %>%
#     mutate(win = ifelse(team_id == winning_team_id, TRUE, FALSE)) %>%
#     select(-winning_team_id) # Remove unnecessary column if needed
#   
#   return(combined_data_with_results)
# }
# 
# # Combine and write the statistics for each team
# map(team_ids, function(team_id) {
#   team_data <- combine_team_statistics(team_id)
#   write_csv(team_data, paste0("data/test_weighted_statistics_team_", team_id, ".csv"))
# })
# 
# 
# test_weighted_statistics_team_1 <- combine_team_statistics(1)
# 
# 











library(dplyr)
library(purrr)
library(readr)

# Define the team IDs
team_ids <- 1:6

# Define the statistic names
statistic_names <- c("shooting_percentage", "save_percentage", "xG_EV_percentage", "xG_PP_percentage", "shots_for_percentage", "pp_share_percentage")

# Read the game results
game_results <- read_csv("data/game_results.csv", col_types = cols())

# Function to shift rows and prepare data for one team
shift_and_prepare_data <- function(team_id) {
  # Read in each statistic CSV for the given team
  statistic_data <- map(statistic_names, function(stat) {
    file_path <- paste0("data/weighted_", stat, "_team_", team_id, ".csv")
    read_csv(file_path, col_types = cols()) %>%
      select(game_date, game_id, team_id, paste0("weighted_", stat)) %>%
      mutate(team_id = as.integer(team_id)) # Ensure team_id is of integer type
  })
  
  # Combine all the statistics by game_date and game_id, avoiding duplicates
  combined_data <- reduce(statistic_data, left_join, by = c("game_date", "game_id", "team_id"))
  
  # Add the win column by joining with game_results
  combined_data_with_results <- combined_data %>%
    left_join(game_results, by = "game_id") %>%
    mutate(win = ifelse(team_id == winning_team_id, TRUE, FALSE)) %>%
    select(-winning_team_id) %>% # Remove unnecessary column if needed
    arrange(game_date, game_id) # Ensure the data is ordered by game_date
  
  # Shift rows down by 1 for each statistic column
  # shifted_data <- combined_data_with_results %>%
  #   arrange(game_date, game_id) %>%
  #   mutate(across(starts_with("weighted_"), ~lag(.x, 1))) %>%
  #   mutate(across(starts_with("weighted_"), ~ifelse(is.na(.x), NA, .x))) # Keep NA for the first row
  
  # Remove the first row and keep the last 14 rows
  # truncated_data <- shifted_data %>%
  #   slice(-(1:10)) %>% # Remove the first 10 rows
  #   slice_tail(n = 14) # Keep the last 14 rows
  
  
  # truncated_data <- shifted_data %>%
  #   filter(game_date > as.Date("2024-02-15"))
  
  return(combined_data_with_results)
}

# Combine and write the statistics for each team
map(team_ids, function(team_id) {
  team_data <- shift_and_prepare_data(team_id)
  write_csv(team_data, paste0("data/processed_statistics_team_", team_id, ".csv"))
})




# 
statistics_team_1 <- shift_and_prepare_data(1)
statistics_team_2 <- shift_and_prepare_data(2)
statistics_team_3 <- shift_and_prepare_data(3)
statistics_team_4 <- shift_and_prepare_data(4)
statistics_team_5 <- shift_and_prepare_data(5)
statistics_team_6 <- shift_and_prepare_data(6)





combined_statistics <- bind_rows(
  statistics_team_1,
  statistics_team_2,
  statistics_team_3,
  statistics_team_4,
  statistics_team_5,
  statistics_team_6
)

# Select only the relevant columns: game_id, weighted stats, and win
# final_statistics <- combined_statistics %>%
#   select(game_date, game_id, starts_with("weighted_"), win)

final_statistics_model_1 <- combined_statistics %>%
  filter(game_date > as.Date("2024-01-20")) %>%
  arrange(game_date, game_id) %>%
  select(weighted_xG_EV_percentage, weighted_shooting_percentage, weighted_save_percentage, win) 
  

# Optionally, write the combined data to a new CSV file
write_csv(final_statistics_model_1, "data/model_1_data.csv")





final_statistics_meta_model <- combined_statistics %>%
  mutate(is_home_team = ifelse(team_id == home_team_id, TRUE, FALSE)) %>%
  select(game_date, game_id, starts_with("weighted_"), is_home_team, home_team_win) %>%
  arrange(game_date, game_id) 


# Optionally, write the combined data to a new CSV file
write_csv(final_statistics_meta_model, "data/meta_model_data.csv")




# 
# 
# 
# #checking histogram of predictor variable: Average Distance
# # pdf("HistogramAvgDistance.pdf")
# hist(final_statistics_meta_model$weighted_shooting_percentage, prob=TRUE, main = "Histogram: Shooting %", 
#      xlab='Shooting %')
# hist(final_statistics_meta_model$weighted_save_percentage, prob=TRUE, main = "Histogram: Save %", 
#      xlab='Save %')
# hist(final_statistics_meta_model$weighted_xG_EV_percentage, prob=TRUE, main = "Histogram: xG EV %", 
#      xlab='xG EV %')
# hist(final_statistics_meta_model$weighted_xG_PP_percentage, prob=TRUE, main = "Histogram: xG PP %", 
#      xlab='xG PP %')
# hist(final_statistics_meta_model$weighted_shots_for_percentage, prob=TRUE, main = "Histogram: shots for %", 
#      xlab='shots for %')
# hist(final_statistics_meta_model$weighted_pp_share_percentage, prob=TRUE, main = "Histogram: pp share %", 
#      xlab='pp share %')
# # dev.off()
# 
# pairs(~ weighted_xG_EV_percentage + weighted_shots_for_percentage + weighted_shooting_percentage + weighted_save_percentage, data=final_statistics_meta_model)
# 
# 
# 
# library(car)
# 
# # Example model
# model <- lm(weighted_shooting_percentage ~ weighted_xG_EV_percentage + 
#               weighted_shots_for_percentage + weighted_save_percentage, 
#             data = final_statistics_meta_model)
# vif(model)
# 
# 
# 
# # Fit logistic regression model
# logit_model <- glm(home_team_win ~ weighted_xG_EV_percentage + weighted_shots_for_percentage, 
#                    data = final_statistics_meta_model, family = "binomial")
# 
# # Display the summary to check p-values for significance
# summary(logit_model)




