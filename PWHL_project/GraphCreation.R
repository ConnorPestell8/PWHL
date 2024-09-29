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



team_1 <- read_csv(here("data/processed_statistics_team_1.csv"))
team_1 <- team_1 %>% slice(3:n())
team_2 <- read_csv(here("data/processed_statistics_team_2.csv"))
team_3 <- read_csv(here("data/processed_statistics_team_3.csv"))
team_3 <- team_3 %>% slice(2:n())
team_4 <- read_csv(here("data/processed_statistics_team_4.csv"))
team_4 <- team_4 %>% slice(2:n())
team_5 <- read_csv(here("data/processed_statistics_team_5.csv"))
team_5 <- team_5 %>% slice(3:n())
team_6 <- read_csv(here("data/processed_statistics_team_6.csv"))


# Create a custom legend dataframe
legend_data <- data.frame(
  Team = c("Boston", "Minnesota", "Montreal", "New York", "Ottawa", "Toronto"),
  Color = c("forestgreen", "purple", "darkred", "turquoise", "red", "blue"),
  Label = c("Boston", "Minnesota", "Montreal", "New York", "Ottawa", "Toronto")
)





# Combining xG_EV together for plotting
combined_data_xG_EV <- bind_rows(
  statistics_team_1 %>% transmute(Row = row_number(), Value = .[[6]], Team = "Boston"),
  statistics_team_2 %>% transmute(Row = row_number(), Value = .[[6]], Team = "Minnesota"),
  statistics_team_3 %>% transmute(Row = row_number(), Value = .[[6]], Team = "Montreal"),
  statistics_team_4 %>% transmute(Row = row_number(), Value = .[[6]], Team = "New York"),
  statistics_team_5 %>% transmute(Row = row_number(), Value = .[[6]], Team = "Ottawa"),
  statistics_team_6 %>% transmute(Row = row_number(), Value = .[[6]], Team = "Toronto")
)

combined_data_xG_EV <- combined_data_xG_EV %>%
  mutate(Importance = ifelse(Team %in% c("Boston", "Minnesota", "Montreal", "Toronto"), "Made Playoffs", "Missed Playoffs"))

# Create a base plot with a custom legend
p <- ggplot() +
  geom_line(data = combined_data_xG_EV, aes(x = Row, y = Value, color = Team, linetype = Importance, size = Importance)) +
  scale_size_manual(values = c("Made Playoffs" = 1.5, "Missed Playoffs" = 1)) +  # Set line thickness for different importance levels
  scale_color_manual(values = c("Boston" = "forestgreen", "Minnesota" = "purple", "Montreal" = "darkred", "New York" = "turquoise",
                                "Ottawa" = "red", "Toronto" = "blue")) +  # Set colors for each team
  scale_linetype_manual(values = c("Made Playoffs" = "solid", "Missed Playoffs" = "dashed")) +  # Set line types
  guides(
    color = guide_legend(title = "PWHL Team", override.aes = list(size = 2)),  # Increase line thickness in the legend
    linetype = guide_legend(title = "Made Playoffs"),  # Hide the legend for linetype (importance)
    size = guide_none()  # Hide the legend for size (importance)
  ) +
  labs(title = "Even Strength Performance", 
       x = "Games Played", 
       y = "Expected Goals %") +
  scale_y_continuous(breaks = seq(min(0.35), max(0.65), by = 0.05)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(b = 20)),  # Larger, bold title
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t=20)),  # Size and style for x-axis label
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r=20)),  # Size and style for y-axis label
    axis.text.x = element_text(size = 12),  # Size of x-axis tick labels
    axis.text.y = element_text(size = 12),  # Size of y-axis tick labels
    legend.title = element_text(size = 0),  # Size and style for legend title
    legend.text = element_text(size = 20),  # Size of legend text
    legend.key.size = unit(1.5, "cm"),  # Size of legend color swatches
    legend.key.width = unit(2, "cm"),  # Increase width of legend key area
    legend.key.height = unit(1, "cm"),  # Increase height of legend key area
    legend.box.spacing = unit(1, "cm"),  # Spacing around the legend box
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(.25, .25, .25, .25), "cm")
  )

# Add a custom legend manually
p <- p + geom_segment(data = legend_data, aes(x = 0.5, xend = 0.5, y = 0.5, yend = 0.5, color = Team), size = 2) 
p

ggsave("plots/xg_ev.png", plot = p, width = 12, height = 8, dpi = 600)









# Combining shooting together for plotting
combined_data_shooting <- bind_rows(
  statistics_team_1 %>% transmute(Row = row_number(), Value = .[[4]], Team = "Boston"),
  statistics_team_2 %>% transmute(Row = row_number(), Value = .[[4]], Team = "Minnesota"),
  statistics_team_3 %>% transmute(Row = row_number(), Value = .[[4]], Team = "Montreal"),
  statistics_team_4 %>% transmute(Row = row_number(), Value = .[[4]], Team = "New York"),
  statistics_team_5 %>% transmute(Row = row_number(), Value = .[[4]], Team = "Ottawa"),
  statistics_team_6 %>% transmute(Row = row_number(), Value = .[[4]], Team = "Toronto")
)

combined_data_shooting <- combined_data_shooting %>%
  mutate(Importance = ifelse(Team %in% c("Boston", "Minnesota", "Montreal", "Toronto"), "Made Playoffs", "Missed Playoffs"))

# Create a base plot with a custom legend
p <- ggplot() +
  geom_line(data = combined_data_shooting, aes(x = Row, y = Value, color = Team, linetype = Importance, size = Importance)) +
  scale_size_manual(values = c("Made Playoffs" = 1.5, "Missed Playoffs" = 1)) +  # Set line thickness for different importance levels
  scale_color_manual(values = c("Boston" = "forestgreen", "Minnesota" = "purple", "Montreal" = "darkred", "New York" = "turquoise",
                                "Ottawa" = "red", "Toronto" = "blue")) +  # Set colors for each team
  scale_linetype_manual(values = c("Made Playoffs" = "solid", "Missed Playoffs" = "dashed")) +  # Set line types
  guides(
    color = guide_legend(title = "PWHL Team", override.aes = list(size = 2)),  # Increase line thickness in the legend
    linetype = guide_legend(title = "Made Playoffs"),  # Hide the legend for linetype (importance)
    size = guide_none()  # Hide the legend for size (importance)
  ) +
  labs(title = "Shooting Performance", 
       x = "Games Played", 
       y = "Shooting %") +
  scale_y_continuous(breaks = seq(min(0), max(0.2), by = 0.025)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(b = 20)),  # Larger, bold title
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t=20)),  # Size and style for x-axis label
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r=20)),  # Size and style for y-axis label
    axis.text.x = element_text(size = 12),  # Size of x-axis tick labels
    axis.text.y = element_text(size = 12),  # Size of y-axis tick labels
    legend.title = element_text(size = 0),  # Size and style for legend title
    legend.text = element_text(size = 20),  # Size of legend text
    legend.key.size = unit(1.5, "cm"),  # Size of legend color swatches
    legend.key.width = unit(2, "cm"),  # Increase width of legend key area
    legend.key.height = unit(1, "cm"),  # Increase height of legend key area
    legend.box.spacing = unit(1, "cm"),  # Spacing around the legend box
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(.25, .25, .25, .25), "cm")
  )

# Add a custom legend manually
p <- p + geom_segment(data = legend_data, aes(x = 0.5, xend = 0.5, y = 0.075, yend = 0.075, color = Team), size = 2) + coord_cartesian(ylim = c(0.025, 0.150))

p

ggsave("plots/shooting.png", plot = p, width = 12, height = 8, dpi = 600, bg = "transparent")















# Combining save together for plotting
combined_data_save<- bind_rows(
  statistics_team_1 %>% transmute(Row = row_number(), Value = .[[5]], Team = "Boston"),
  statistics_team_2 %>% transmute(Row = row_number(), Value = .[[5]], Team = "Minnesota"),
  statistics_team_3 %>% transmute(Row = row_number(), Value = .[[5]], Team = "Montreal"),
  statistics_team_4 %>% transmute(Row = row_number(), Value = .[[5]], Team = "New York"),
  statistics_team_5 %>% transmute(Row = row_number(), Value = .[[5]], Team = "Ottawa"),
  statistics_team_6 %>% transmute(Row = row_number(), Value = .[[5]], Team = "Toronto")
)

combined_data_save <- combined_data_save %>%
  mutate(Importance = ifelse(Team %in% c("Boston", "Minnesota", "Montreal", "Toronto"), "Made Playoffs", "Missed Playoffs"))

# Create a base plot with a custom legend
p <- ggplot() +
  geom_line(data = combined_data_save, aes(x = Row, y = Value, color = Team, linetype = Importance, size = Importance)) +
  scale_size_manual(values = c("Made Playoffs" = 1.5, "Missed Playoffs" = 1)) +  # Set line thickness for different importance levels
  scale_color_manual(values = c("Boston" = "forestgreen", "Minnesota" = "purple", "Montreal" = "darkred", "New York" = "turquoise",
                                "Ottawa" = "red", "Toronto" = "blue")) +  # Set colors for each team
  scale_linetype_manual(values = c("Made Playoffs" = "solid", "Missed Playoffs" = "dashed")) +  # Set line types
  guides(
    color = guide_legend(title = "PWHL Team", override.aes = list(size = 2)),  # Increase line thickness in the legend
    linetype = guide_legend(title = "Made Playoffs"),  # Hide the legend for linetype (importance)
    size = guide_none()  # Hide the legend for size (importance)
  ) +
  labs(title = "Goalie Performance", 
       x = "Games Played", 
       y = "Save %") +
  scale_y_continuous(breaks = seq(min(0.8), max(1), by = 0.025)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(b = 20)),  # Larger, bold title
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t=20)),  # Size and style for x-axis label
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r=20)),  # Size and style for y-axis label
    axis.text.x = element_text(size = 12),  # Size of x-axis tick labels
    axis.text.y = element_text(size = 12),  # Size of y-axis tick labels
    legend.title = element_text(size = 0),  # Size and style for legend title
    legend.text = element_text(size = 20),  # Size of legend text
    legend.key.size = unit(1.5, "cm"),  # Size of legend color swatches
    legend.key.width = unit(2, "cm"),  # Increase width of legend key area
    legend.key.height = unit(1, "cm"),  # Increase height of legend key area
    legend.box.spacing = unit(1, "cm"),  # Spacing around the legend box
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(.25, .25, .25, .25), "cm")
  )

# Add a custom legend manually
p <- p + geom_segment(data = legend_data, aes(x = 0.5, xend = 0.5, y = 0.9, yend = 0.9, color = Team), size = 2) + coord_cartesian(ylim = c(0.85, 0.975))
p
ggsave("plots/save.png", plot = p, width = 12, height = 8, dpi = 600)














# Combining shots for together for plotting
combined_data_shots_for <- bind_rows(
  statistics_team_1 %>% transmute(Row = row_number(), Value = .[[8]], Team = "Boston"),
  statistics_team_2 %>% transmute(Row = row_number(), Value = .[[8]], Team = "Minnesota"),
  statistics_team_3 %>% transmute(Row = row_number(), Value = .[[8]], Team = "Montreal"),
  statistics_team_4 %>% transmute(Row = row_number(), Value = .[[8]], Team = "New York"),
  statistics_team_5 %>% transmute(Row = row_number(), Value = .[[8]], Team = "Ottawa"),
  statistics_team_6 %>% transmute(Row = row_number(), Value = .[[8]], Team = "Toronto")
)

combined_data_shots_for <- combined_data_shots_for %>%
  mutate(Importance = ifelse(Team %in% c("Boston", "Minnesota", "Montreal", "Toronto"), "Made Playoffs", "Missed Playoffs"))

# Create a base plot with a custom legend
p <- ggplot() +
  geom_line(data = combined_data_shots_for, aes(x = Row, y = Value, color = Team, linetype = Importance, size = Importance)) +
  scale_size_manual(values = c("Made Playoffs" = 1.5, "Missed Playoffs" = 1)) +  # Set line thickness for different importance levels
  scale_color_manual(values = c("Boston" = "forestgreen", "Minnesota" = "purple", "Montreal" = "darkred", "New York" = "turquoise",
                                "Ottawa" = "red", "Toronto" = "blue")) +  # Set colors for each team
  scale_linetype_manual(values = c("Made Playoffs" = "solid", "Missed Playoffs" = "dashed")) +  # Set line types
  guides(
    color = guide_legend(title = "PWHL Team", override.aes = list(size = 2)),  # Increase line thickness in the legend
    linetype = guide_legend(title = "Made Playoffs"),  # Hide the legend for linetype (importance)
    size = guide_none()  # Hide the legend for size (importance)
  ) +
  labs(title = "Shots For", 
       x = "Games Played", 
       y = "Shots For %") +
  scale_y_continuous(breaks = seq(min(0.35), max(0.65), by = 0.05)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(b = 20)),  # Larger, bold title
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t=20)),  # Size and style for x-axis label
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r=20)),  # Size and style for y-axis label
    axis.text.x = element_text(size = 12),  # Size of x-axis tick labels
    axis.text.y = element_text(size = 12),  # Size of y-axis tick labels
    legend.title = element_text(size = 0),  # Size and style for legend title
    legend.text = element_text(size = 20),  # Size of legend text
    legend.key.size = unit(1.5, "cm"),  # Size of legend color swatches
    legend.key.width = unit(2, "cm"),  # Increase width of legend key area
    legend.key.height = unit(1, "cm"),  # Increase height of legend key area
    legend.box.spacing = unit(1, "cm"),  # Spacing around the legend box
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(.25, .25, .25, .25), "cm")
  )

# Add a custom legend manually
p <- p + geom_segment(data = legend_data, aes(x = 0.5, xend = 0.5, y = 0.5, yend = 0.5, color = Team), size = 2) 
p
ggsave("plots/shots_for.png", plot = p, width = 12, height = 8, dpi = 600)











# Combining PP_XG together for plotting
combined_data_PP_xG <- bind_rows(
  statistics_team_1 %>% transmute(Row = row_number(), Value = .[[7]], Team = "Boston"),
  statistics_team_2 %>% transmute(Row = row_number(), Value = .[[7]], Team = "Minnesota"),
  statistics_team_3 %>% transmute(Row = row_number(), Value = .[[7]], Team = "Montreal"),
  statistics_team_4 %>% transmute(Row = row_number(), Value = .[[7]], Team = "New York"),
  statistics_team_5 %>% transmute(Row = row_number(), Value = .[[7]], Team = "Ottawa"),
  statistics_team_6 %>% transmute(Row = row_number(), Value = .[[7]], Team = "Toronto")
)

combined_data_PP_xG <- combined_data_PP_xG %>%
  mutate(Importance = ifelse(Team %in% c("Boston", "Minnesota", "Montreal", "Toronto"), "Made Playoffs", "Missed Playoffs"))

# Create a base plot with a custom legend
p <- ggplot() +
  geom_line(data = combined_data_PP_xG, aes(x = Row, y = Value, color = Team, linetype = Importance, size = Importance)) +
  scale_size_manual(values = c("Made Playoffs" = 1.5, "Missed Playoffs" = 1)) +  # Set line thickness for different importance levels
  scale_color_manual(values = c("Boston" = "forestgreen", "Minnesota" = "purple", "Montreal" = "darkred", "New York" = "turquoise",
                                "Ottawa" = "red", "Toronto" = "blue")) +  # Set colors for each team
  scale_linetype_manual(values = c("Made Playoffs" = "solid", "Missed Playoffs" = "dashed")) +  # Set line types
  guides(
    color = guide_legend(title = "PWHL Team", override.aes = list(size = 2)),  # Increase line thickness in the legend
    linetype = guide_legend(title = "Made Playoffs"),  # Hide the legend for linetype (importance)
    size = guide_none()  # Hide the legend for size (importance)
  ) +
  labs(title = "Power Play Performance", 
       x = "Games Played", 
       y = "PP Expected Goals %") +
  scale_y_continuous(breaks = seq(min(0.25), max(0.75), by = 0.05)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(b = 20)),  # Larger, bold title
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t=20)),  # Size and style for x-axis label
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r=20)),  # Size and style for y-axis label
    axis.text.x = element_text(size = 12),  # Size of x-axis tick labels
    axis.text.y = element_text(size = 12),  # Size of y-axis tick labels
    legend.title = element_text(size = 0),  # Size and style for legend title
    legend.text = element_text(size = 20),  # Size of legend text
    legend.key.size = unit(1.5, "cm"),  # Size of legend color swatches
    legend.key.width = unit(2, "cm"),  # Increase width of legend key area
    legend.key.height = unit(1, "cm"),  # Increase height of legend key area
    legend.box.spacing = unit(1, "cm"),  # Spacing around the legend box
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(.25, .25, .25, .25), "cm")
  )

# Add a custom legend manually
p <- p + geom_segment(data = legend_data, aes(x = 0.5, xend = 0.5, y = 0.5, yend = 0.5, color = Team), size = 2) + coord_cartesian(ylim = c(0.25, 0.75))

p
ggsave("plots/pp_xg.png", plot = p, width = 12, height = 8, dpi = 600)






# Load cowplot library
library(cowplot)

# Extract the legend from one of the plots
legend <- get_legend(
  ggplot() +
    geom_line(data = combined_data_xG_EV, aes(x = Row, y = Value, color = Team, linetype = Importance, size = Importance)) +
    scale_size_manual(values = c("Made Playoffs" = 1.5, "Missed Playoffs" = 1)) +
    scale_color_manual(values = c("Boston" = "forestgreen", "Minnesota" = "purple", "Montreal" = "darkred", 
                                  "New York" = "turquoise", "Ottawa" = "red", "Toronto" = "blue")) +
    scale_linetype_manual(values = c("Made Playoffs" = "solid", "Missed Playoffs" = "dashed")) +
    guides(
      # color = guide_legend(title = "PWHL Team"),
      color = 'none',
      linetype = guide_legend(title = "Made Playoffs"),
      # linetype = 'none',
      size = 'none'
    )
)

legend

# Save the legend as a separate PNG file
ggsave("plots/legend_type.png", plot = as_ggplot(legend), width = 5, height = 5, dpi = 1200)



































# Read the CSV file in R
grouped_data <- read_csv(here("data/grouped_data.csv"))

# Print the DataFrame to check
print(grouped_data)



library(ggplot2)

# Read the CSV f
pdf("my_tester_plot.pdf", width = 10, height = 6)
# Create the bar plot with customizations
p <- ggplot(grouped_data, aes(x = bin, y = accuracy)) +
  geom_bar(stat = 'identity', fill = 'lightblue', color = 'black') +
  labs(
    title = "Prediction Accuracy by Win Probability Bin",
    x = "Win Probability Bin (%)",
    y = "Accuracy"
  ) +
  scale_x_discrete(breaks = grouped_data$bin) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(b = 20)),  # Larger, bold title
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),  # Size and style for x-axis label
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),  # Size and style for y-axis label
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis tick labels for readability
    axis.text.y = element_text(size = 12),  # Size of y-axis tick labels
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")
  )

# Print the plot
print(p)
# ggsave("plots/win_by_bin.pdf", plot = p, width = 12, height = 8, dpi = 1200)
dev.off()

library(svglite)

# Save the plot as SVG with transparent background
ggsave("plots/win_by_bin.png", plot = p, width = 12, height = 8, dpi = 600)













#### PLOTTING POWER RANKINGS AT END OF SEASON






# Function to create plots without legends
create_plot <- function(data, title, y_label) {
  ggplot() +
    geom_line(data = data, aes(x = Row, y = Value, color = Team, linetype = Importance, size = Importance)) +
    scale_size_manual(values = c("Made Playoffs" = 1.5, "Missed Playoffs" = 1)) +
    scale_color_manual(values = c("Boston" = "forestgreen", "Minnesota" = "purple", "Montreal" = "darkred", 
                                  "New York" = "turquoise", "Ottawa" = "red", "Toronto" = "blue")) +
    scale_linetype_manual(values = c("Made Playoffs" = "solid", "Missed Playoffs" = "dashed")) +
    labs(title = title, x = "Games Played", y = y_label) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(b = 20)),
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "none"  # Suppress the legend
    )
}

# Create the individual plots
p1 <- create_plot(combined_data_xG_EV, "Even Strength Performance", "Expected Goals %")
p1
p2 <- create_plot(combined_data_shooting, "Shooting Performance", "Shooting %")
p2
p3 <- create_plot(combined_data_save, "Goalie Performance", "Save %")
p3

# Save the individual plots
ggsave("plots/xg_ev_no_legend.png", plot = p1, width = 12, height = 8, dpi = 600)
ggsave("plots/shooting_no_legend.png", plot = p2, width = 12, height = 8, dpi = 600)
ggsave("plots/save_no_legend.png", plot = p3, width = 12, height = 8, dpi = 600)

