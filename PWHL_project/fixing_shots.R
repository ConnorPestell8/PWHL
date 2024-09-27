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

















PWHL_shots <- game_plays %>%
  filter(event == "shot" | event == "goal") %>%
  select(game_id, game_date, team_id, home_team_id, away_team_id, period_of_game, time_of_period, event, empty_net, x_coord, x_coord_right, y_coord, y_coord_right, goal, power_play)



# Create a logical vector to identify rows to remove
rows_to_remove <- logical(nrow(PWHL_shots))

# Loop through the rows of the dataframe
for (i in seq_len(nrow(PWHL_shots))) {
  # Check if the current row is a goal with an empty net
  if (PWHL_shots$event[i] == "goal" && PWHL_shots$empty_net[i] == 1) {
    # Mark the next row (the corresponding shot) for removal
    rows_to_remove[i - 1] <- TRUE
  }
}

# Filter out the marked rows from the dataframe
PWHL_shots_filtered <- PWHL_shots[!rows_to_remove, ] %>% filter(event == 'shot')

# 
# PWHL_shots_filtered$x_coord_right <- PWHL_shots_filtered$x_coord_right - 100
# PWHL_shots_filtered$y_coord_right <- PWHL_shots_filtered$y_coord_right - 85/2




# Filter for rows where the 'event' column contains 'shot'
PWHL_shots_filtered_good <- PWHL_shots_filtered %>%
  filter(event == "shot", !game_id %in% c(3, 9, 23, 35, 38, 40, 43, 71)) %>%
  select(game_id, game_date, team_id, home_team_id, away_team_id, period_of_game, time_of_period, x_coord, x_coord_right, y_coord, y_coord_right, goal, power_play)

# Subtract 100 from all x_coord values
# PWHL_shots_filtered_good$x_coord_right <- PWHL_shots_filtered_good$x_coord_right - 100
# PWHL_shots_filtered_good$y_coord_right <- PWHL_shots_filtered_good$y_coord_right - 85/2
 
# #shots are now from 25 to 100 max
# PWHL_shots <- PWHL_shots %>%
#   filter(x_coord_right > 25)
# 
# 
# write_csv(PWHL_shots, here("data/PWHL_shots"))



ggplot(PWHL_shots_filtered_good, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()      # Ensures that one unit on the x-axis is equal in length to one unit on the y-axis


####### FIXING GAME 3 SHOTS ############

# game_3_shots <- PWHL_shots_filtered %>%
#   filter(game_id == 3, event == 'shot') %>%
#   select(game_id, team_id, home_team_id, away_team_id, period_of_game, x_coord, x_coord_right, y_coord, y_coord_right)


game_3_shots <- PWHL_shots_filtered %>%
  filter(game_id == 3, event == 'shot') %>%
  mutate(
    x_coord_right = ifelse(
      period_of_game == 2 | period_of_game == 4,  # If period is 2 or 4, keep x_coord_right unchanged
      x_coord_right,
      ifelse(
        x_coord < 100,
        100 + (100 - x_coord),  # Condition when x_coord is less than 100 in periods 1 or 3
        100 - (100 - x_coord)   # Condition when x_coord is 100 or greater in periods 1 or 3
      )
    ),
    y_coord_right = ifelse(
      period_of_game == 2 | period_of_game == 4,  # If period is 2 or 4, keep y_coord_right unchanged
      y_coord_right,
      ifelse(
        y_coord < 85/2,
        85/2 + (85/2 - y_coord),  # Condition when y_coord is less than 100 in periods 1 or 3
        85/2 - (85/2 - y_coord)   # Condition when y_coord is 100 or greater in periods 1 or 3
  )))

ggplot(game_3_shots, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()   


####### FIXING GAME 9 SHOTS ############

game_9_shots <- PWHL_shots_filtered %>%
  filter(game_id == 9, event == 'shot') %>%
  mutate(
    x_coord_right = ifelse(
      period_of_game == 2 | period_of_game == 3,  # If period is 2 or 4, keep x_coord_right unchanged
      x_coord_right,
      ifelse(
        x_coord < 100,
        100 + (100 - x_coord),  # Condition when x_coord is less than 100 in periods 1 or 3
        100 - (100 - x_coord)   # Condition when x_coord is 100 or greater in periods 1 or 3
      )
    ),
    y_coord_right = ifelse(
      period_of_game == 2 | period_of_game == 3,  # If period is 2 or 4, keep y_coord_right unchanged
      y_coord_right,
      ifelse(
        y_coord < 85/2,
        85/2 + (85/2 - y_coord),  # Condition when y_coord is less than 100 in periods 1 or 3
        85/2 - (85/2 - y_coord)   # Condition when y_coord is 100 or greater in periods 1 or 3
      )))

ggplot(game_9_shots, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()   




####### FIXING GAME 23 SHOTS ############

game_23_shots <- PWHL_shots_filtered %>%
  filter(game_id == 23, event == 'shot') %>%
  mutate(
    x_coord_right = ifelse(
      period_of_game == 2 | period_of_game == 3,  # If period is 2 or 4, keep x_coord_right unchanged
      x_coord_right,
      ifelse(
        x_coord < 100,
        100 + (100 - x_coord),  # Condition when x_coord is less than 100 in periods 1 or 3
        100 - (100 - x_coord)   # Condition when x_coord is 100 or greater in periods 1 or 3
      )
    ),
    y_coord_right = ifelse(
      period_of_game == 2 | period_of_game == 3,  # If period is 2 or 4, keep y_coord_right unchanged
      y_coord_right,
      ifelse(
        y_coord < 85/2,
        85/2 + (85/2 - y_coord),  # Condition when y_coord is less than 100 in periods 1 or 3
        85/2 - (85/2 - y_coord)   # Condition when y_coord is 100 or greater in periods 1 or 3
      )))

ggplot(game_23_shots, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()   




####### FIXING GAME 35 SHOTS ############

game_35_shots <- PWHL_shots_filtered %>%
  filter(game_id == 35, event == 'shot') %>%
  mutate(
    x_coord_right = ifelse(
      period_of_game != 4,  # If period is 2 or 4, keep x_coord_right unchanged
      x_coord_right,
      ifelse(
        x_coord < 100,
        100 + (100 - x_coord),  # Condition when x_coord is less than 100 in periods 1 or 3
        100 - (100 - x_coord)   # Condition when x_coord is 100 or greater in periods 1 or 3
      )
    ),
    y_coord_right = ifelse(
      period_of_game != 4,  # If period is 2 or 4, keep y_coord_right unchanged
      y_coord_right,
      ifelse(
        y_coord < 85/2,
        85/2 + (85/2 - y_coord),  # Condition when y_coord is less than 100 in periods 1 or 3
        85/2 - (85/2 - y_coord)   # Condition when y_coord is 100 or greater in periods 1 or 3
      )))

ggplot(game_35_shots, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()   


####### FIXING GAME 38 SHOTS ############



game_38_shots <- PWHL_shots_filtered %>%
  filter(game_id == 38, event == 'shot') %>%
  mutate(
    x_coord_right = ifelse(
      period_of_game != 2,  # If period is 2 or 4, keep x_coord_right unchanged
      x_coord_right,
      ifelse(
        x_coord < 100,
        100 + (100 - x_coord),  # Condition when x_coord is less than 100 in periods 1 or 3
        100 - (100 - x_coord)   # Condition when x_coord is 100 or greater in periods 1 or 3
      )
    ),
    y_coord_right = ifelse(
      period_of_game != 2,  # If period is 2 or 4, keep y_coord_right unchanged
      y_coord_right,
      ifelse(
        y_coord < 85/2,
        85/2 + (85/2 - y_coord),  # Condition when y_coord is less than 100 in periods 1 or 3
        85/2 - (85/2 - y_coord)   # Condition when y_coord is 100 or greater in periods 1 or 3
      )))

ggplot(game_38_shots, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()




####### FIXING GAME 40 SHOTS ############

game_40_shots <- PWHL_shots_filtered %>%
  filter(game_id == 40, event == 'shot') %>%
  mutate(
    x_coord_right = ifelse(
      period_of_game == 1 | period_of_game == 3,  # If period is 2 or 4, keep x_coord_right unchanged
      x_coord_right,
      ifelse(
        x_coord < 100,
        100 + (100 - x_coord),  # Condition when x_coord is less than 100 in periods 1 or 3
        100 - (100 - x_coord)   # Condition when x_coord is 100 or greater in periods 1 or 3
      )
    ),
    y_coord_right = ifelse(
      period_of_game == 1 | period_of_game == 3,  # If period is 2 or 4, keep y_coord_right unchanged
      y_coord_right,
      ifelse(
        y_coord < 85/2,
        85/2 + (85/2 - y_coord),  # Condition when y_coord is less than 100 in periods 1 or 3
        85/2 - (85/2 - y_coord)   # Condition when y_coord is 100 or greater in periods 1 or 3
      )))

ggplot(game_40_shots, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()  



####### FIXING GAME 43 SHOTS ############

game_43_shots <- PWHL_shots_filtered %>%
  filter(game_id == 43, event == 'shot') %>%
  mutate(
    x_coord_right = ifelse(
      period_of_game != 1,  # If period is 2 or 4, keep x_coord_right unchanged
      x_coord_right,
      ifelse(
        x_coord < 100,
        100 + (100 - x_coord),  # Condition when x_coord is less than 100 in periods 1 or 3
        100 - (100 - x_coord)   # Condition when x_coord is 100 or greater in periods 1 or 3
      )
    ),
    y_coord_right = ifelse(
      period_of_game != 1,  # If period is 2 or 4, keep y_coord_right unchanged
      y_coord_right,
      ifelse(
        y_coord < 85/2,
        85/2 + (85/2 - y_coord),  # Condition when y_coord is less than 100 in periods 1 or 3
        85/2 - (85/2 - y_coord)   # Condition when y_coord is 100 or greater in periods 1 or 3
      )))

ggplot(game_43_shots, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()  





####### FIXING GAME 71 SHOTS ############

game_71_shots <- PWHL_shots_filtered %>%
  filter(game_id == 71, event == 'shot') %>%
  mutate(
    x_coord_right = ifelse(
      period_of_game == 2,  # If period is 2 or 4, keep x_coord_right unchanged
      x_coord_right,
      ifelse(
        x_coord < 100,
        100 + (100 - x_coord),  # Condition when x_coord is less than 100 in periods 1 or 3
        100 - (100 - x_coord)   # Condition when x_coord is 100 or greater in periods 1 or 3
      )
    ),
    y_coord_right = ifelse(
      period_of_game == 2,  # If period is 2 or 4, keep y_coord_right unchanged
      y_coord_right,
      ifelse(
        y_coord < 85/2,
        85/2 + (85/2 - y_coord),  # Condition when y_coord is less than 100 in periods 1 or 3
        85/2 - (85/2 - y_coord)   # Condition when y_coord is 100 or greater in periods 1 or 3
      )))

ggplot(game_71_shots, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()  







all_fixed_shots <- bind_rows(game_3_shots, game_9_shots, game_23_shots, game_35_shots, game_38_shots, game_40_shots, game_43_shots, game_71_shots)



# already in PWHL_shots_filtered_good
# without_weird_shots <- game_plays %>%
#   filter(!game_id %in% c(3, 9, 23, 35, 40, 43, 71), event=='shot')


PWHL_shots_2024 <- bind_rows(PWHL_shots_filtered_good, all_fixed_shots) %>%
  arrange(game_date)

# 
# ggplot(PWHL_shots_2024, aes(x = x_coord_right, y = y_coord_right)) +
#   geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
#   theme_minimal() +          # Use a clean theme
#   labs(
#     title = "PWHL Shot Attempts",
#     x = "X Coordinate",
#     y = "Y Coordinate"
#   ) +
#   coord_fixed() 
# 
# 
# ggplot(game_plays %>% filter(event=='shot'), aes(x = x_coord_right, y = y_coord_right)) +
#   geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
#   theme_minimal() +          # Use a clean theme
#   labs(
#     title = "PWHL Shot Attempts",
#     x = "X Coordinate",
#     y = "Y Coordinate"
#   ) +
#   coord_fixed() 

write_csv(PWHL_shots_2024, here("data/PWHL_shots_2024"))