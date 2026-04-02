setwd("~/Desktop/Rice Classes/SMGT 435/Final Project")

#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)
library(ggplot2)
#install.packages("mgcv")
library(mgcv)
library(akima)
#install.packages("sjPlot")
#install.packages("robustlmm")

#Import and organize the Data
sprint_speed <- read.csv("sprint_speed.csv")
jump_data <- read.csv("jump.csv")
linear_weight <- read.csv("~/Desktop/Rice Classes/SMGT 435/linear_weight.csv")
batted_ball <- read.csv("~/Desktop/Rice Classes/SMGT 435/batted_ball.csv")
player <- read.csv("~/Desktop/Rice Classes/SMGT 435/player.csv")

full_data <- jump_data |> 
  left_join(sprint_speed, join_by(resp_fielder_id == player_id)) |> 
  dplyr::select(
    Name = "last_name..first_name.x", team, age, player_id = "resp_fielder_id", OAA = "outs_above_average",
    reaction = "rel_league_reaction_distance", burst = "rel_league_burst_distance", 
    route = "rel_league_routing_distance", jump = "rel_league_bootup_distance", jump_distance = "f_bootup_distance",
    opportunities = "n", outs = "n_outs")

# Filter batted ball data on fair, fly balls, calculate linear weight
pbp_data <- batted_ball |> 
  filter(first_fielder %in% c(7, 8, 9),
         launch_angle > 10,
         spray_angle > -45,
         spray_angle < 45) |> 
  mutate(first_fielder_id = case_when(first_fielder == 7 ~ fielder_7_id,
                                      first_fielder == 8 ~ fielder_8_id,
                                      first_fielder == 9 ~ fielder_9_id)) |> 
  left_join(linear_weight, by = 'event') |> 
  group_by(first_fielder_id) |> 
  summarise(runs_saved = -sum(linear_weight),
            batted_balls = n()) |> 
  mutate(runs_saved_per = runs_saved/batted_balls) |> 
  left_join(player, join_by("first_fielder_id" == 'player_id')) |> 
  select("player_id" = "first_fielder_id", name_full, team, 
         runs_saved, batted_balls, runs_saved_per) |> 
  arrange(-runs_saved) |> 
  inner_join(full_data, 'player_id')

#linear_model <- lm(runs_saved ~ reaction + burst + route + sprint_speed, data = pbp_data)
#summary(linear_model)



#plot(linear_model)

#linear_model$coefficients

#predictions <- predict(linear_model, data = pbp_data)

# ggplot(mapping = aes(pbp_data$runs_saved, predictions)) +
#   geom_point(aes(col = pbp_data$opportunities.x - pbp_data$opportunities.y)) +
#   geom_abline(slope = 1, col = 'red', lwd = 1) +
#   labs(x = "2024 Runs Saved", y = "Predicted Runs Saved") +
#   ggtitle("Predicted vs Actual Values for the Linear Model") +
#   scale_color_continuous(name = "Opportunity Difference") +
#   xlim(c(-40, 20)) +
#   ylim(c(-40, 20))


# Attemtpting to get specific play by play data from Statcast API by scraping JSONs
statcast_plays <- data.frame()

for(player in full_data$player_id){
  url <- paste0("https://baseballsavant.mlb.com/player-services/range?playerId=", player, 
                "&season=2024&playerType=fielder")
  response <- GET(url)
  json_data <- content(response, as = "text", encoding = "UTF-8")
  individual_data <- fromJSON(json_data, flatten = TRUE)
  statcast_plays <- bind_rows(statcast_plays, individual_data)
}


length(unique(statcast_plays$player_id)) == length(unique(full_data$player_id))

statcast_plays_good <- statcast_plays |> 
  select(-c(play_id, name_display_first_last)) |> 
  mutate(across(everything(), as.numeric)) |> 
  filter(stars >= 2, stars <= 5) |> 
  left_join(full_data, "player_id")

# Logistic Regression Approach
catch_prob_model <- glm(out ~ distance + hang_time, data = statcast_plays_good, family = "binomial")
summary(catch_prob_model)
player_model <- glm(out ~ distance + hang_time + sprint_speed + burst + reaction + route, 
                                        data = statcast_plays_good, family = "binomial")
summary(player_model)


# Random Player Effects Approach
lmer_data <- statcast_plays_good |> 
  select(out, distance, hang_time, sprint_speed, burst, reaction, route, player_id)
lmer_data$distance = scale(lmer_data$distance)
lmer_data$hang_time = scale(lmer_data$hang_time)
lmer_data$sprint_speed = scale(lmer_data$sprint_speed)
lmer_data$burst = scale(lmer_data$burst)
lmer_data$reaction = scale(lmer_data$reaction)
lmer_data$route = scale(lmer_data$route)

lmer_model <- lme4::glmer(out ~ distance + hang_time + sprint_speed + burst + reaction + route + (1 | player_id),
                               data = statcast_plays_good, family = "binomial",
                               control = lme4::glmerControl(optimizer ="bobyqa"))

sjPlot::plot_model(lmer_model, type = 'std', title = "Standardized Beta Values", axis.title = c("Odds Ratios", "Variables"), terms = c("sprint_speed", "burst", "reaction", "route"))
sjPlot::plot_model(lmer_model, type = 're', title = "Random Effect")

random_effects <- as.vector(robustlmm::getME(lmer_model, "b"))
names <- rownames(robustlmm::getME(lmer_model, "Zt"))
random_effects_data <- data.frame(names = as.numeric(names), random_effects)

random_effects_names <- random_effects_data |> 
  left_join(select(player, player_id, name_full, team), join_by("names" == "player_id"))

print(names)
ggplot() +
  geom_density(mapping = aes(random_effects), fill = '#9fddf9', alpha = .4)+
  labs(x = "Random Effect (on Catch Probability)", y = "Density") +
  ggtitle("Distribution of Random Effects for Outfielders") +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_linedraw()

var_component <- as.data.frame(lme4::VarCorr(lmer_model))
var_component

var_signal <- var_component |>
  dplyr::filter(grp == "player_id") |>
  with(vcov)

# Apply full batted ball set to all players
predictions_df <- data.frame()
for(player in unique(full_data$player_id)){
    # Create a copy of new_df with the current player_id
    new_df <- statcast_plays_good |> 
      select(distance, hang_time, sprint_speed) |> 
      mutate(player_id = player) 
    
    # Merge with additional player data if needed
    new_df <- new_df |> 
      left_join(full_data, by = "player_id") |> 
      mutate(sprint_speed = as.numeric(sprint_speed))
    
    # Predict catch probabilities for this player's plays
    #base_preds <- predict(catch_prob_model, newdata = new_df, type = "response")
    #jump_preds <- predict(player_model, newdata = new_df, type = "response")
    lmer_preds <- predict(lmer_model, newdata = new_df, type = "response")
    
    # Bind the player_id, play index, and predictions into a data frame
    player_predictions <- data.frame(
      player_id = rep(player, nrow(new_df)),
      play_index = seq_len(nrow(new_df)), # Index for plays
      #simple_catch_prob = base_preds,
      #metric_catch_prob = jump_preds,
      lmer_catch_prob = lmer_preds
    )
    predictions_df <- predictions_df |> 
      bind_rows(player_predictions)
}

results <- predictions_df |> 
  group_by(player_id) |> 
  summarise(outs = sum(lmer_catch_prob >= .5)) |> 
  mutate(out_rate = outs/5299) |> 
  left_join(select(read.csv("~/Desktop/Rice Classes/SMGT 435/player.csv"), player_id, name_full, team), "player_id") |> 
  arrange(-out_rate)
            


library(dplyr)
library(tidyr)
library(ggplot2)

jacob_id <- 691718
jesse_id <- 608385

lmer_data %>% filter(player_id == jacob_id) %>% slice(1)

# Extract Jacob's constant individual stats (e.g., sprint_speed, burst, etc.) from statcast_plays_good
jacob_stats <- statcast_plays_good %>%
  as.data.frame() %>%
  filter(player_id == jacob_id) %>%
  slice(1) %>%  # pick first row, since these are constant per player
  dplyr::select(`sprint_speed`, burst, reaction, route)

jesse_stats <- statcast_plays_good %>%
  as.data.frame() %>%
  filter(player_id == jesse_id) %>%
  slice(1) %>%  # pick first row, since these are constant per player
  dplyr::select(`sprint_speed`, burst, reaction, route)

# Calculate median hang_time for Jacob (or pick a fixed value)
jacob_hang_time <- statcast_plays_good %>%
  summarise(hang_time = median(hang_time, na.rm = TRUE)) %>%
  pull(hang_time)

# Build grid of run_x and run_y (directional displacement)
# Use the min/max observed run_x and run_y from his actual plays
jacob_plays <- predictions_df %>%
  filter(player_id == jacob_id) %>%
  bind_cols(landing_pos_x = statcast_plays_good$landing_pos_x, 
            landing_pos_y = statcast_plays_good$landing_pos_y,
            start_pos_x = statcast_plays_good$start_pos_x,
            start_pos_y = statcast_plays_good$start_pos_y) %>%
  mutate(
    run_x = landing_pos_x - start_pos_x,
    run_y = landing_pos_y - start_pos_y
  )

x_seq <- seq(min(jacob_plays$run_x, na.rm = TRUE),
             max(jacob_plays$run_x, na.rm = TRUE),
             length.out = 100)

y_seq <- seq(min(jacob_plays$run_y, na.rm = TRUE),
             max(jacob_plays$run_y, na.rm = TRUE),
             length.out = 100)

distmean <- mean(statcast_plays_good$distance)
distsd <- sd(statcast_plays_good$distance)
hangmean <- mean(statcast_plays_good$hang_time)
hangsd <- sd(statcast_plays_good$hang_time)

grid_df <- expand_grid(run_x = x_seq, run_y = y_seq) %>%
  # Calculate scalar distance from run_x and run_y
  mutate(
    distance = sqrt(run_x^2 + run_y^2),
    hang_time = jacob_hang_time,
    sprint_speed = jacob_stats$sprint_speed,
    burst = jacob_stats$burst,
    reaction = jacob_stats$reaction,
    route = jacob_stats$route,
    player_id = jacob_id
    # # Calculate landing positions back from run_x, run_y, and median start positions for plotting (optional)
    # landing_pos_x = run_x + median(jacob_plays$start_pos_x, na.rm = TRUE),
    # landing_pos_y = run_y + median(jacob_plays$start_pos_y, na.rm = TRUE)
  )

# Predict catch probability on this grid
grid_df <- grid_df %>%
  mutate(pred_catch_prob = predict(lmer_model, newdata = .) %>% as.numeric())

#Plot contour with run_x and run_y as axes
ggplot(grid_df, aes(x = run_x, y = run_y, z = pred_catch_prob)) +
  geom_contour_filled(bins = 50) +
  labs(
    title = "Pete Crow-Armstrong Catch Probability Map",
    x = "Run Displacement X (ft)",
    y = "Run Displacement Y (ft)"
  ) +
  theme_minimal() +
  guides(fill = "none") +
  xlim(c(-75, 75)) +
  ylim(-75, 75)

ggplot(grid_df, aes(x = run_x, y = run_y)) +
  geom_contour_filled(mapping = aes(z = pred_catch_prob), bins = 50, alpha = .7) +
  geom_point(data = filter(statcast_plays_good, player_id == jacob_id), 
             mapping = aes(landing_pos_x - start_pos_x, 
                           landing_pos_y - start_pos_y,
                           shape = ifelse(out == 0, "Not Caught", "Caught")
             ), size = 2.5) +
  scale_shape_manual(values = c(16, 1)) +
  labs(
    title = "Pete Crow-Armstrong Catch Probability Map",
    x = "Run Displacement X (ft)",
    y = "Run Displacement Y (ft)",
    shape = ""
  ) +
  theme_minimal() +
  guides(fill = "none", shape = "none") +
  xlim(c(-75, 75)) +
  ylim(-75, 75)

## NOW JESSE WINKER ##

jesse_hang_time <- statcast_plays_good %>%
  summarise(hang_time = median(hang_time, na.rm = TRUE)) %>%
  pull(hang_time)

# Build grid of run_x and run_y (directional displacement)
# Use the min/max observed run_x and run_y from his actual plays
jesse_plays <- predictions_df %>%
  filter(player_id == jesse_id) %>%
  bind_cols(landing_pos_x = statcast_plays_good$landing_pos_x, 
            landing_pos_y = statcast_plays_good$landing_pos_y,
            start_pos_x = statcast_plays_good$start_pos_x,
            start_pos_y = statcast_plays_good$start_pos_y) %>%
  mutate(
    run_x = landing_pos_x - start_pos_x,
    run_y = landing_pos_y - start_pos_y
  )

x_seq <- seq(min(jesse_plays$run_x, na.rm = TRUE),
             max(jesse_plays$run_x, na.rm = TRUE),
             length.out = 100)

y_seq <- seq(min(jesse_plays$run_y, na.rm = TRUE),
             max(jesse_plays$run_y, na.rm = TRUE),
             length.out = 100)

distmean <- mean(statcast_plays_good$distance)
distsd <- sd(statcast_plays_good$distance)
hangmean <- mean(statcast_plays_good$hang_time)
hangsd <- sd(statcast_plays_good$hang_time)

grid_df <- expand_grid(run_x = x_seq, run_y = y_seq) %>%
  # Calculate scalar distance from run_x and run_y
  mutate(
    distance = (sqrt(run_x^2 + run_y^2) - distmean)/distsd,
    hang_time = jacob_hang_time,
    sprint_speed = jesse_stats$sprint_speed,
    burst = jesse_stats$burst,
    reaction = jesse_stats$reaction,
    route = jesse_stats$route,
    player_id = jesse_id
    # # Calculate landing positions back from run_x, run_y, and median start positions for plotting (optional)
    # landing_pos_x = run_x + median(jacob_plays$start_pos_x, na.rm = TRUE),
    # landing_pos_y = run_y + median(jacob_plays$start_pos_y, na.rm = TRUE)
  )

# Predict catch probability on this grid
grid_df <- grid_df %>%
  mutate(pred_catch_prob = predict(lmer_model, newdata = .) %>% as.numeric())

#Plot contour with run_x and run_y as axes
ggplot(grid_df, aes(x = run_x, y = run_y, z = pred_catch_prob)) +
  geom_contour_filled(bins = 50) +
  labs(
    title = "Jesse Winker – Catch Probability by Directional Run",
    x = "Run Displacement X (ft)",
    y = "Run Displacement Y (ft)"
  ) +
  theme_minimal() +
  guides(fill = "none") +
  xlim(c(-75, 75)) +
  ylim(-75, 75)

ggplot(grid_df, aes(x = run_x, y = run_y)) +
  geom_contour_filled(mapping = aes(z = pred_catch_prob), bins = 50, alpha = .7) +
  geom_point(data = filter(statcast_plays_good, player_id == jesse_id), 
             mapping = aes(landing_pos_x - start_pos_x, 
                           landing_pos_y - start_pos_y,
                           shape = ifelse(out == 0, "Not Caught", "Caught")
             ), size = 2.5) +
  scale_shape_manual(values = c(16, 1)) +
  labs(
    title = "Jesse Winker – Catch Probability by Directional Run",
    x = "Run Displacement X (ft)",
    y = "Run Displacement Y (ft)",
    shape = ""
  ) +
  theme_minimal() +
  guides(fill = "none") +
  xlim(c(-75, 75)) +
  ylim(-75, 75)
# newjacob <- predictions_df |>
#   dplyr::filter(player_id == jacob_id) |>
#   bind_cols(run_x = statcast_plays_good$landing_pos_x - statcast_plays_good$start_pos_x,
#             run_y = statcast_plays_good$landing_pos_y - statcast_plays_good$start_pos_y,
#             select(statcast_plays_good, -player_id))

# interp_result <- interp(newjacob$run_x, newjacob$run_y, newjacob$lmer_catch_prob, duplicate = "median", linear = TRUE, nx = 200, ny = 200)
# 
# # 2. Convert interp output to a data frame suitable for ggplot
# interp_df <- with(interp_result, expand.grid(x = x, y = y))
# 
# interp_df$z <- as.vector(interp_result$z)

# 3. Plot contour map with ggplot
ggplot(grid_df, aes(x = run_x, y = run_y)) +
  stat_contour_filled(mapping = aes(z = pred_catch_prob), bins = 100, alpha = .6) +  # bins control number of discrete fill levels
  #scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  geom_point(data = filter(statcast_plays_good, player_id == jesse_id), 
             mapping = aes(landing_pos_x - start_pos_x, 
                           landing_pos_y - start_pos_y,
                           shape = ifelse(out == 0, "Not Caught", "Caught")
                           ), size = 2.5) +
  labs(title = "Jesse Winker Catch Probability by Ball Location",
       x = "Distance From Starting Point (ft)",
       y = "Distance From Starting Point (ft)",
       shape = "") +
  scale_shape_manual(values = c(16, 1)) +
  theme_minimal() +
  guides(fill = "none")

# ggplot(interp_df, aes(x = run_x, y = run_y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 10, alpha = 1) +  # bins control number of discrete fill levels
#   #scale_fill_brewer(palette = "YlGnBu", direction = -1) +
#   labs(title = "Jacob Young Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")

## NOW FOR JESSE WINKER ##

# newjesse <- predictions_df |>
#   dplyr::filter(player_id == 608385) |>
#   bind_cols(run_x = statcast_plays_good$landing_pos_x - statcast_plays_good$start_pos_x,
#             run_y = statcast_plays_good$landing_pos_y - statcast_plays_good$start_pos_y,
#             select(statcast_plays_good, -player_id))
# 
# interp_result_jesse <- interp(newjesse$run_x, newjesse$run_y, newjesse$lmer_catch_prob, duplicate = "mean", linear = FALSE, nx = 100, ny = 100)
# 
# # 2. Convert interp output to a data frame suitable for ggplot
# interp_df_jesse <- with(interp_result_jesse, expand.grid(x = x, y = y))
# 
# interp_df_jesse$z <- as.vector(interp_result_jesse$z)
# 
# # 3. Plot contour map with ggplot
# ggplot(interp_df_jesse, aes(x = x, y = y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 9, alpha = .6) +  # bins control number of discrete fill levels
#   scale_fill_brewer(palette = "RdYlBu", direction = 1) +
#   geom_point(data = filter(statcast_plays_good, player_id == 608385), 
#              mapping = aes(landing_pos_x - start_pos_x, 
#                            landing_pos_y - start_pos_y,
#                            color = ifelse(out == 0, "Not Caught", "Caught")
#              )) +
#   scale_color_manual(values = c("red", "black")) +
#   labs(title = "Jesse Winker Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")
# 
# ggplot(interp_df_jesse, aes(x = x, y = y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 9, alpha = 1) +  # bins control number of discrete fill levels
#   scale_fill_brewer(palette = "YlGnBu", direction = -1) +
#   labs(title = "Jesse Winker Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")
# 
# # ggplot(interp_df, aes(x = x, y = y, z = z)) +
# #   stat_contour(aes(fill = after_stat(level))) +
# #   scale_fill_gradientn(colours = rev(brewer.pal(8, "RdYlBu"))) +
# #   theme_minimal()
# 
# # Make distance/hang time heat maps
# jacob_plays <- predictions_df %>%
#   filter(player_id == jacob_id) %>%
#   bind_cols(landing_pos_x = statcast_plays_good$landing_pos_x,
#             landing_pos_y = statcast_plays_good$landing_pos_y,
#             start_pos_x = statcast_plays_good$start_pos_x,
#             start_pos_y = statcast_plays_good$start_pos_y) %>%
#   mutate(
#     run_x = landing_pos_x - start_pos_x,
#     run_y = landing_pos_y - start_pos_y
#   )
# 
# x_seq <- seq(min(jacob_plays$run_x, na.rm = TRUE),
#              max(jacob_plays$run_x, na.rm = TRUE),
#              length.out = 80)
# 
# y_seq <- seq(min(jacob_plays$run_y, na.rm = TRUE),
#              max(jacob_plays$run_y, na.rm = TRUE),
#              length.out = 80)
# 
# distmean <- mean(statcast_plays_good$distance)
# distsd <- sd(statcast_plays_good$distance)
# hangmean <- mean(statcast_plays_good$hang_time)
# hangsd <- sd(statcast_plays_good$hang_time)
# 
# grid_df <- expand_grid(run_x = x_seq, run_y = y_seq) %>%
#   # Calculate scalar distance from run_x and run_y
#   mutate(
#     distance = (sqrt(run_x^2 + run_y^2) - distmean)/distsd,
#     hang_time = 0,
#     sprint_speed = jacob_stats$sprint_speed,
#     burst = jacob_stats$burst,
#     reaction = jacob_stats$reaction,
#     route = jacob_stats$route,
#     player_id = jacob_id
#     # # Calculate landing positions back from run_x, run_y, and median start positions for plotting (optional)
#     # landing_pos_x = run_x + median(jacob_plays$start_pos_x, na.rm = TRUE),
#     # landing_pos_y = run_y + median(jacob_plays$start_pos_y, na.rm = TRUE)
#   )
# 
# # Predict catch probability on this grid
# grid_df <- grid_df %>%
#   mutate(pred_catch_prob = predict(lmer_model, newdata = ., type = "response") %>% as.numeric())
# 
# # Plot contour with run_x and run_y as axes
# ggplot(grid_df, aes(x = run_x, y = run_y, z = pred_catch_prob)) +
#   geom_contour_filled(bins = 12) +
#   scale_fill_viridis_d(labels = scales::percent_format(accuracy = 1),
#                        name = "Catch Probability") +
#   coord_equal() +
#   labs(
#     title = "Jacob Young – Catch Probability by Directional Run",
#     x = "Run Displacement X (ft)",
#     y = "Run Displacement Y (ft)"
#   ) +
#   theme_minimal()
# 
# newjacob <- predictions_df |>
#   dplyr::filter(player_id == jacob_id) |>
#   bind_cols(run_x = statcast_plays_good$landing_pos_x - statcast_plays_good$start_pos_x,
#             run_y = statcast_plays_good$landing_pos_y - statcast_plays_good$start_pos_y,
#             select(statcast_plays_good, -player_id))
# 
# interp_result <- interp(newjacob$run_x, newjacob$run_y, newjacob$lmer_catch_prob, duplicate = "error", nx = 100, ny = 100)
# 
# # 2. Convert interp output to a data frame suitable for ggplot
# interp_df <- with(interp_result, expand.grid(x = x, y = y))
# 
# interp_df$z <- as.vector(interp_result$z)
# 
# # 3. Plot contour map with ggplot
# ggplot(interp_df, aes(x = x, y = y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 9, alpha = .6) +  # bins control number of discrete fill levels
#   scale_fill_brewer(palette = "YlGnBu", direction = -1) +
#   geom_point(data = filter(statcast_plays_good, player_id == jacob_id),
#              mapping = aes(landing_pos_x - start_pos_x,
#                            landing_pos_y - start_pos_y,
#                            color = ifelse(out == 0, "Not Caught", "Caught")
#              )) +
#   scale_color_manual(values = c("red", "black")) +
#   labs(title = "Jacob Young Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")

# ggplot(interp_df, aes(x = x, y = y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 9, alpha = 1) +  # bins control number of discrete fill levels
#   scale_fill_brewer(palette = "YlGnBu", direction = -1) +
#   labs(title = "Jacob Young Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")
# 
# ## NOW FOR JESSE WINKER ##
# 
# newjesse <- predictions_df |>
#   dplyr::filter(player_id == 608385) |>
#   bind_cols(run_x = statcast_plays_good$landing_pos_x - statcast_plays_good$start_pos_x,
#             run_y = statcast_plays_good$landing_pos_y - statcast_plays_good$start_pos_y,
#             select(statcast_plays_good, -player_id))
# 
# interp_result_jesse <- interp(newjesse$run_x, newjesse$run_y, newjesse$lmer_catch_prob, duplicate = "error", nx = 100, ny = 100)
# 
# # 2. Convert interp output to a data frame suitable for ggplot
# interp_df_jesse <- with(interp_result_jesse, expand.grid(x = x, y = y))
# 
# interp_df_jesse$z <- as.vector(interp_result_jesse$z)
# 
# # 3. Plot contour map with ggplot
# ggplot(interp_df_jesse, aes(x = x, y = y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 9, alpha = .6) +  # bins control number of discrete fill levels
#   scale_fill_brewer(palette = "YlGnBu", direction = -1) +
#   geom_point(data = filter(statcast_plays_good, player_id == 608385), 
#              mapping = aes(landing_pos_x - start_pos_x, 
#                            landing_pos_y - start_pos_y,
#                            color = ifelse(out == 0, "Not Caught", "Caught")
#              )) +
#   scale_color_manual(values = c("red", "black")) +
#   labs(title = "Jacob Young Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")
# 
# ggplot(interp_df_jesse, aes(x = x, y = y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 9, alpha = 1) +  # bins control number of discrete fill levels
#   scale_fill_brewer(palette = "YlGnBu", direction = -1) +
#   labs(title = "Jesse Winker Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")
# 
# # ggplot(interp_df, aes(x = x, y = y, z = z)) +
# #   stat_contour(aes(fill = after_stat(level))) +
# #   scale_fill_gradientn(colours = rev(brewer.pal(8, "RdYlBu"))) +
# #   theme_minimal()
# 
# # Make distance/hang time heat maps
# jesse_id = 608385
# 
jacob_plays <- predictions_df %>%
  filter(player_id == jacob_id) %>%
  bind_cols(distance = statcast_plays_good$distance, hang_time = statcast_plays_good$hang_time) %>%
  mutate(distance = (distance - distmean)/distsd,
         hang_time = (hang_time - hangmean)/hangsd)


x_seq <- seq(min(jacob_plays$distance, na.rm = TRUE),
             max(jacob_plays$distance, na.rm = TRUE),
             length.out = 80)

y_seq <- seq(min(jacob_plays$hang_time, na.rm = TRUE),
             max(jacob_plays$hang_time, na.rm = TRUE),
             length.out = 80)

distmean <- mean(statcast_plays_good$distance)
distsd <- sd(statcast_plays_good$distance)
hangmean <- mean(statcast_plays_good$hang_time)
hangsd <- sd(statcast_plays_good$hang_time)

grid_df <- expand_grid(distance = x_seq, hang_time = y_seq) %>%
  # Calculate scalar distance from run_x and run_y
  mutate(
    sprint_speed = jacob_stats$sprint_speed,
    burst = jacob_stats$burst,
    reaction = jacob_stats$reaction,
    route = jacob_stats$route,
    player_id = jesse_id
    # # Calculate landing positions back from run_x, run_y, and median start positions for plotting (optional)
    # landing_pos_x = run_x + median(jacob_plays$start_pos_x, na.rm = TRUE),
    # landing_pos_y = run_y + median(jacob_plays$start_pos_y, na.rm = TRUE)
  )

# Predict catch probability on this grid
grid_df <- grid_df %>%
  mutate(pred_catch_prob = predict(lmer_model, newdata = ., type = "response") %>% as.numeric(),
         og_distance = distance*distsd + distmean,
         og_hang = hang_time * hangsd + hangmean)

interp_result <- interp(grid_df$og_distance, grid_df$og_hang, grid_df$pred_catch_prob, duplicate = "mean", linear = FALSE, nx = 100, ny = 100)

interp_df <- with(interp_result, expand.grid(x = x, y = y))

interp_df$z <- as.vector(interp_result$z)

# Plot contour with run_x and run_y as axes
ggplot(interp_df, aes(x = x, y = y)) +
  stat_contour_filled(mapping = aes(z = z), bins = 50, alpha = .6) +
  #scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  # geom_point(data = filter(statcast_plays_good, player_id == jesse_id),
  #            mapping = aes(x = distance, y = hang_time,
  #                          shape = ifelse(out == 0, "Not Caught", "Caught")
  #            )) +
  labs(title = "Jacob Young 2024 Fly Balls Over Catch Probability",
       x = "Distance",
       y = "Hang Time",
       shape = "") +
  theme_minimal() +
  guides(fill = "none")
# 

jesse_plays <- predictions_df %>%
  filter(player_id == jesse_id) %>%
  bind_cols(distance = statcast_plays_good$distance, hang_time = statcast_plays_good$hang_time) %>%
  mutate(distance = (distance - distmean)/distsd,
         hang_time = (hang_time - hangmean)/hangsd)


x_seq <- seq(min(jacob_plays$distance, na.rm = TRUE),
             max(jacob_plays$distance, na.rm = TRUE),
             length.out = 80)

y_seq <- seq(min(jacob_plays$hang_time, na.rm = TRUE),
             max(jacob_plays$hang_time, na.rm = TRUE),
             length.out = 80)

distmean <- mean(statcast_plays_good$distance)
distsd <- sd(statcast_plays_good$distance)
hangmean <- mean(statcast_plays_good$hang_time)
hangsd <- sd(statcast_plays_good$hang_time)

grid_df <- expand_grid(distance = x_seq, hang_time = y_seq) %>%
  # Calculate scalar distance from run_x and run_y
  mutate(
    sprint_speed = jacob_stats$sprint_speed,
    burst = jacob_stats$burst,
    reaction = jacob_stats$reaction,
    route = jacob_stats$route,
    player_id = jesse_id
    # # Calculate landing positions back from run_x, run_y, and median start positions for plotting (optional)
    # landing_pos_x = run_x + median(jacob_plays$start_pos_x, na.rm = TRUE),
    # landing_pos_y = run_y + median(jacob_plays$start_pos_y, na.rm = TRUE)
  )

# Predict catch probability on this grid
grid_df <- grid_df %>%
  mutate(pred_catch_prob = predict(lmer_model, newdata = ., type = "response") %>% as.numeric(),
         og_distance = distance*distsd + distmean,
         og_hang = hang_time * hangsd + hangmean)

interp_result <- interp(grid_df$og_distance, grid_df$og_hang, grid_df$pred_catch_prob, duplicate = "mean", linear = FALSE, nx = 100, ny = 100)

interp_df <- with(interp_result, expand.grid(x = x, y = y))

interp_df$z <- as.vector(interp_result$z)

# Plot contour with run_x and run_y as axes
ggplot(interp_df, aes(x = x, y = y)) +
  stat_contour_filled(mapping = aes(z = z), bins = 50, alpha = .6) +
  #scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  # geom_point(data = filter(statcast_plays_good, player_id == jesse_id),
  #            mapping = aes(x = distance, y = hang_time,
  #                          shape = ifelse(out == 0, "Not Caught", "Caught")
  #            )) +
  labs(title = "Jacob Young 2024 Fly Balls Over Catch Probability",
       x = "Distance",
       y = "Hang Time",
       shape = "") +
  theme_minimal() +
  guides(fill = "none")
# # 2. Convert interp output to a data frame suitable for ggplot
# interp_df <- with(interp_result, expand.grid(x = x, y = y))
# 
# interp_df$z <- as.vector(interp_result$z)
# 
# # 3. Plot contour map with ggplot
# ggplot(interp_df, aes(x = x, y = y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 9, alpha = .6) +  # bins control number of discrete fill levels
#   scale_fill_brewer(palette = "YlGnBu", direction = -1) +
#   geom_point(data = filter(statcast_plays_good, player_id == jacob_id), 
#              mapping = aes(landing_pos_x - start_pos_x, 
#                            landing_pos_y - start_pos_y,
#                            color = ifelse(out == 0, "Not Caught", "Caught")
#                            )) +
#   scale_color_manual(values = c("red", "black")) +
#   labs(title = "Jacob Young Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")
# 
# ggplot(interp_df, aes(x = x, y = y)) +
#   stat_contour_filled(mapping = aes(z = z), bins = 9, alpha = 1) +  # bins control number of discrete fill levels
#   scale_fill_brewer(palette = "YlGnBu", direction = -1) +
#   labs(title = "Jacob Young Catch Probability by Ball Location",
#        x = "Distance From Starting Point (ft)",
#        y = "Distance From Starting Point (ft)",
#        color = "") +
#   theme_minimal() +
#   guides(fill = "none")
# 

jacob_id <- 691718
jesse_id <- 608385

lmer_data %>% filter(player_id == jacob_id) %>% slice(1)

# Extract Jacob's constant individual stats (e.g., sprint_speed, burst, etc.) from statcast_plays_good
jacob_stats <- lmer_data %>%
  as.data.frame() %>%
  filter(player_id == jacob_id) %>%
  slice(1) %>%  # pick first row, since these are constant per player
  dplyr::select(`sprint_speed`, burst, reaction, route)

jesse_stats <- lmer_data %>%
  as.data.frame() %>%
  filter(player_id == jesse_id) %>%
  slice(1) %>%  # pick first row, since these are constant per player
  dplyr::select(`sprint_speed`, burst, reaction, route)

# Calculate median hang_time for Jacob (or pick a fixed value)
jacob_hang_time <- statcast_plays_good %>%
  summarise(hang_time = median(hang_time, na.rm = TRUE)) %>%
  pull(hang_time)

# Build grid of run_x and run_y (directional displacement)
# Use the min/max observed run_x and run_y from his actual plays
jacob_plays <- predictions_df %>%
  filter(player_id == jacob_id) %>%
  bind_cols(landing_pos_x = statcast_plays_good$landing_pos_x, 
            landing_pos_y = statcast_plays_good$landing_pos_y,
            start_pos_x = statcast_plays_good$start_pos_x,
            start_pos_y = statcast_plays_good$start_pos_y) %>%
  mutate(
    run_x = landing_pos_x - start_pos_x,
    run_y = landing_pos_y - start_pos_y
  )

x_seq <- seq(min(jacob_plays$distance, na.rm = TRUE),
             max(jacob_plays$distance, na.rm = TRUE),
             length.out = 100)

y_seq <- seq(min(jacob_plays$hang_time, na.rm = TRUE),
             max(jacob_plays$hang_time, na.rm = TRUE),
             length.out = 100)

distmean <- mean(statcast_plays_good$distance)
distsd <- sd(statcast_plays_good$distance)
hangmean <- mean(statcast_plays_good$hang_time)
hangsd <- sd(statcast_plays_good$hang_time)

grid_df <- expand_grid(run_x = x_seq, run_y = y_seq) %>%
  # Calculate scalar distance from run_x and run_y
  mutate(
    distance = (sqrt(run_x^2 + run_y^2) - distmean)/distsd,
    hang_time = 0,
    sprint_speed = jacob_stats$sprint_speed,
    burst = jacob_stats$burst,
    reaction = jacob_stats$reaction,
    route = jacob_stats$route,
    player_id = jacob_id
    # # Calculate landing positions back from run_x, run_y, and median start positions for plotting (optional)
    # landing_pos_x = run_x + median(jacob_plays$start_pos_x, na.rm = TRUE),
    # landing_pos_y = run_y + median(jacob_plays$start_pos_y, na.rm = TRUE)
  )

# Predict catch probability on this grid
grid_df <- grid_df %>%
  mutate(pred_catch_prob = predict(lmer_model, newdata = .) %>% as.numeric())

#Plot contour with run_x and run_y as axes
ggplot(grid_df, aes(x = run_x, y = run_y, z = pred_catch_prob)) +
  geom_contour_filled(bins = 50) +
  labs(
    title = "Jacob Young – Catch Probability by Directional Run",
    x = "Run Displacement X (ft)",
    y = "Run Displacement Y (ft)"
  ) +
  theme_minimal() +
  guides(fill = "none") +
  xlim(c(-75, 75)) +
  ylim(-75, 75)
