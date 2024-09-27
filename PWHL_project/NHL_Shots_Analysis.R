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




NHL_shots <- read_csv(here("data/2014-2022.csv"))


NHL_shots_reduced <- NHL_shots %>%
  mutate(x_coord_right = xCordAdjusted, y_coord_right = yCordAdjusted) %>%
  select(shotID, event, goal, x_coord_right, y_coord_right) %>%
  filter(event != 'MISS')






y_net <- 0
x_net <- 89
x_region_345_circle <- 120
y_region_345_circle <- 0
x_region_678_circle <- 115
y_region_678_circle <- 0


# arctan(y/x-x_net) will give angle

NHL_shots_region <- NHL_shots_reduced %>%
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
             distance_345 <= 50 &
             y_coord_right >= -0.765*x_coord_right+73.085, TRUE, FALSE),
         region_4 = ifelse(
           !region_1 & !region_2 & !region_3 &
             distance_345 <= 50 &
             y_coord_right >= 0.765*x_coord_right-73.085, TRUE, FALSE),
         region_5 = ifelse(
           !region_1 & !region_2 & !region_3 & !region_4 &
             distance_345 <= 50, TRUE, FALSE),
         region_6 = ifelse(
           distance_345 > 50 & 
             y_coord_right >= -0.765*x_coord_right+73.085, TRUE, FALSE),
         region_7 = ifelse(
           !region_6 & distance_345 > 50 & distance_789 <= 70 &
             y_coord_right >= -0.332*x_coord_right+29.548, TRUE, FALSE),
         region_8 = ifelse(
           !region_6 & !region_7 & distance_345 > 50 & distance_789 <= 70 &
             y_coord_right >= 0.332*x_coord_right-29.548, TRUE, FALSE),
         region_9 = ifelse(
           !region_6 & !region_7 & !region_8 & distance_345 > 50 & distance_789 <= 70 &
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


NHL_shots_by_region <- NHL_shots_region %>%
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


write_csv(NHL_shots_by_region, here("data/NHL_shots_by_region"))



# ggplot(NHL_shots_by_region, aes(x = x_coord_right, y = y_coord_right, color = factor(region))) +
#   geom_point(alpha = 0.7) +  # Adjust alpha for transparency
#   scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
#                                 "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", 
#                                 "#bcbd22", "#17becf", "#bc80bd", "#fdb462", "#b3de69", "#1f77b4")) +  # 13 colors
#   theme_minimal() +          # Clean theme
#   labs(
#     title = "NHL Shot Attempts by Region",
#     x = "X Coordinate",
#     y = "Y Coordinate",
#     color = "Shot Region"    # Label for the color legend
#   ) +
#   coord_fixed()              # Maintain aspect ratio
# 







NHL_shots_by_region_rounded <- NHL_shots_by_region %>% filter(x_coord_right >= 0) %>%
  mutate(x_coord_right = floor(x_coord_right), y_coord_right = floor(y_coord_right))

library(ggplot2)

img <- png::readPNG("data/halfrinklight.png")  # Adjust the path to your image file

p = ggplot(NHL_shots_by_region_rounded, aes(x = x_coord_right, y = y_coord_right, color = factor(region))) +
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
    title = "NHL Shots by Region",
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

ggsave("plots/nhl_xg.png",plot = p, width = 12, height = 8, dpi = 600)




# Comparing PWHL xG against NHL xG
# NHL
# 0    region_1  0.098994. MORE
# 1    region_2  0.210185. MORE
# 2    region_3  0.069506 MORE
# 3    region_4  0.174085. MORE
# 4    region_5  0.072735. MORE
# 5    region_6  0.037440. MORE
# 6    region_7  0.079963. MORE
# 7    region_8  0.121250. MORE
# 8    region_9  0.083690. MORE
# 9   region_10  0.042728. MORE
# 10  region_11  0.028067. LESS
# 11  region_12  0.046303. MORE
# 12  region_13  0.028073. LESS
# 13  region_14  0.042053. MORE
# 
# PWHL
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


# Create the NHL xG tibble
NHL_xG <- tibble::tibble(
  region = paste0("region_", 1:14),
  xG = c(0.098994, 0.210185, 0.069506, 0.174085, 0.072735, 0.037440, 
         0.079963, 0.121250, 0.083690, 0.042728, 0.028067, 0.046303, 
         0.028073, 0.042053)
)

# Create the PWHL xG tibble
PWHL_xG <- tibble::tibble(
  region = paste0("region_", 1:14),
  xG = c(0.084842, 0.189633, 0.020739, 0.134533, 0.059088, 0.018506, 
         0.058387, 0.082456, 0.033699, 0.025245, 0.042244, 0.043209, 
         0.062576, 0.010337)
)

# View the tibbles
NHL_xG
PWHL_xG
