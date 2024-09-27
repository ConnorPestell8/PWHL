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

library(readr)

PWHL_shots_2024 <- read_csv(here("data/PWHL_shots_2024"))



# Filter for rows where the 'event' column contains 'shot'
PWHL_shots_2024 <- PWHL_shots_2024 %>%
  select(game_id, team_id, period_of_game, time_of_period, x_coord, x_coord_right, y_coord, y_coord_right, goal)

# Subtract 100 from all x_coord values
PWHL_shots_2024$x_coord_right <- PWHL_shots_2024$x_coord_right - 100
PWHL_shots_2024$y_coord_right <- PWHL_shots_2024$y_coord_right - 85/2

# #shots are now from 25 to 100 max
# PWHL_shots <- PWHL_shots %>%
#   filter(x_coord_right > 25)
# 
# 
# write_csv(PWHL_shots, here("data/PWHL_shots"))



ggplot(PWHL_shots_2024, aes(x = x_coord_right, y = y_coord_right)) +
  geom_point(alpha = 0.5) +  # Use alpha for transparency to deal with overlapping points
  theme_minimal() +          # Use a clean theme
  labs(
    title = "PWHL Shot Attempts",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed()      # Ensures that one unit on the x-axis is equal in length to one unit on the y-axis

# now to classify shots as different regions 
# 1 is behind net (x greater than x net)
# 2 is super close to crease (within distance of 5 feet and not behind not)
# 3-5 is low left slow, low middle slot, low right slot (distance of )
# 6 is far left middle
# 7-9 is left, middle, and right slot
# 10 is far right middle
# 11-13 is left, middle, and right point
# 14 is beyond blue line

# 89-25=64 is distance to blue line straight

# see how many points classify as each and readjust if need be

# start with region 1
# anything behind net means anything with x_coord_right greater than 89 (net is at 89)
y_net <- 0
x_net <- 89
x_region_345_circle <- 120
y_region_345_circle <- 0
x_region_678_circle <- 115
y_region_678_circle <- 0
  

# arctan(y/x-x_net) will give angle

PWHL_shots_region <- PWHL_shots_2024 %>%
  mutate(distance = sqrt((x_coord_right-x_net)^2+(y_coord_right)^2),
         distance_345 = sqrt((x_coord_right-x_region_345_circle)^2+(y_coord_right)^2),
         distance_789 = sqrt((x_coord_right-x_region_678_circle)^2+(y_coord_right)^2),
         angle = atan(y_coord_right/(x_coord_right-x_net))*180/pi,
         region_1 = ifelse(
            x_coord_right > x_net, TRUE, FALSE),
         region_2 = ifelse(
              distance < 8 & region_1 == FALSE, TRUE, FALSE),
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
         )


PWHL_shots_by_region <- PWHL_shots_region %>%
  mutate(region = case_when(
    region_1 == TRUE ~ "Region 01",
    region_2 == TRUE ~ "Region 02",
    region_3 == TRUE ~ "Region 03",
    region_4 == TRUE ~ "Region 04",
    region_5 == TRUE ~ "Region 05",
    region_6 == TRUE ~ "Region 06",
    region_7 == TRUE ~ "Region 07",
    region_8 == TRUE ~ "Region 08",
    region_9 == TRUE ~ "Region 09",
    region_10 == TRUE ~ "Region 10",
    region_11 == TRUE ~ "Region 11",
    region_12 == TRUE ~ "Region 12",
    region_13 == TRUE ~ "Region 13",
    region_14 == TRUE ~ "Region 14",
    TRUE ~ "Uncategorized"  # Catch-all for shots that don’t match any region
  ))


write_csv(PWHL_shots_by_region, here("data/PWHL_shots_by_region"))

PWHL_shots_by_region_rounded <- PWHL_shots_by_region %>% filter(x_coord_right >= 0) %>%
 mutate(x_coord_right = floor(x_coord_right), y_coord_right = floor(y_coord_right))



library(ggplot2)

img <- png::readPNG("data/halfrinklight.png")  # Adjust the path to your image file

p = ggplot(PWHL_shots_by_region_rounded, aes(x = x_coord_right, y = y_coord_right, color = factor(region))) +
  annotation_raster(
    img,
    xmin = -0, xmax = Inf,  # Set a defined range for the image, ensuring it starts at x = 0
    ymin = -85/2, ymax = 85/2
  )+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(alpha = 2, size = 2.5) +  # Adjust alpha for transparency and size of points
  scale_color_manual(values = c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00",
    "#FFDF33", "#A65628", "#F781BF", "#999999",
    "#FF00FF", "#00FFFF", "#FF0000", "#00FF00",
    "#0000FF", "#800080")) +
  labs(
    title = "PWHL Shots by Region",
    x = NULL,
    y = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(b = 20)),  # Larger, bold title
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t=20)),  # Size and style for x-axis label
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r=20)),  # Size and style for y-axis label
    axis.text.x = element_text(size = 12),  # Size of x-axis tick labels
    axis.text.y = element_text(size = 12),  # Size of y-axis tick labels
    # legend.title = element_text(size = 10),  # Size of legend title
    # legend.text = element_text(size = 10),  # Size of legend text
    # legend.key.size = unit(1, "cm"),  # Size of legend color swatches
    # legend.box.spacing = unit(1, "cm"),  # Spacing around the legend box
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(.25, .25, .25, .25), "cm")  # Add margin around the plot
    # panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border around the plot panel
  ) +
  theme(
    legend.position = "none",  # Remove the legend
    axis.text.x = element_blank(),  # Remove x-axis text (tick labels)
    axis.text.y = element_blank(),  # Remove y-axis text (tick labels)
    axis.ticks = element_blank()    # Remove axis ticks
  )+
  coord_fixed()  
p

ggsave("plots/pwhl_xg.png",plot = p, width = 12, height = 8, dpi = 600)










