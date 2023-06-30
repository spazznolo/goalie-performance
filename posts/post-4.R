
# Required packages: dplyr, MASS, fitdistrplus, ggplot2; loaded with 'load/libraries.R'

# Required objects:
# - shots (tibble): Contains shot data; created with 'load/data.R'
# - single_color (string): Specifies the fill color for single color plots; created with 'load/functions.R'
# - multiple_colors (function): Specifies the fill color for gradients; created with 'load/functions.R'


# Load necessary libraries from the 'libraries.R' file
source('load/libraries.R')  

# Load custom functions from the 'functions.R' file
source('load/functions.R')  

# Load data from the 'data.R' file
source('load/data.R')  



exp_f_by_season <-
  shots %>%
  group_by(season) %>%
  summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    exp_f = 1 - (exp_g/shots)  # Calculate expected save percentage
  ) %>%
  mutate(exp_f = (exp_f + 2*mean(exp_f))/3) %>%
  select(season, season_exp_f = exp_f)

mean_exp_f <- mean(exp_f_by_season$season_exp_f)

# Calculate career statistics for goalies
career_statistics <- 
  shots %>%  # tibble containing shot data loaded by the 'data' script
  left_join(exp_f_by_season, by = 'season') %>%
  group_by(goalie_name, season) %>%  # Group shots by goalie name
  dplyr::summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    sv_pct_a_exp = sv_pct - exp_f,  # Calculate adjusted save percentage
    adj_sv_pct = mean(season_exp_f) + sv_pct_a_exp,  # Calculate adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    adj_goals = shots - adj_saves,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>%
  group_by(goalie_name) %>%
  summarize(
    shots = sum(shots),  # Count the number of shots
    goals = sum(goals),  # Sum the number of goals
    exp_g = sum(exp_g),
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    adj_saves = sum(adj_saves),
    adj_goals = sum(adj_goals),
    adj_sv_pct = adj_saves/(adj_saves + adj_goals),
    .groups = 'drop'
  ) %>%
  #dplyr::filter(exp_g > 10) %>%  # Filter rows where expected goals is greater than 10
  drop_na()  # Drop rows with missing values



# Remove goalies whose career spans outside of the range of interest
goalies_to_remove <-
  shots %>% 
  filter(season %in% c(2007, 2022)) %>% 
  group_by(goalie_name) %>%
  summarize(
    shots = n(),
    min_season = min(season),
    max_season = max(season)
  ) %>%
  filter(!(goalie_name %in% c('Carey Price', 'Brian Elliott', 'Jaroslav Halak', 'Jonathan Quick', 'Mike Smith',
                              'Semyon Varlamov', 'Tuukka Rask', 'Anton Khudobin', 'Jonathan Bernier',
                              'Pekka Rinne', 'Thomas Greiss', 'Corey Crawford', 'Cory Schneider',
                              'Curtis McElhinney', 'Josh Harding', 'Matt Keetley', 'Daniel Lacosta',
                              'Chris Beckford-Tseu', 'Daniel Taylor', 'Marek Schwarz', 'Niklas Backstrom',
                              'Peter Budaj', 'Alex Stalock', 'Fredrik Norrena', 'Erik Ersberg',
                              'Dimitri Patzold', 'Drew MacIntyre', 'Tyler Weiman', 'Tobias Stephan',
                              'Joey MacDonald'))) %>%
  select(goalie_name) %>%
  add_row(goalie_name = 'Manny Fernandez')


# Summary of goalie statistics in initial set
shots %>%
  group_by(goalie_name) %>%  # Group shots by goalie name
  summarize(
    seasons = n_distinct(season),
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = 0.939 + ((sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>% 
  summarize(
    count = n(),
    across(c(seasons, shots, adj_sv_pct), list(mean, ~sum(.*(exp_g/sum(exp_g)))))
  )

# Summary of goalie statistics in filtered set
shots %>%
  anti_join(goalies_to_remove, by = 'goalie_name') %>% # Filter goalies who haven't played the first or last season of available data
  group_by(goalie_name) %>%  # Group shots by goalie name
  summarize(
    seasons = n_distinct(season),
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = 0.939 + ((sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>% 
  summarize(
    count = n(),
    across(c(seasons, shots, adj_sv_pct), list(mean, ~sum(.*(exp_g/sum(exp_g)))))
  )



# Plot cumulative distribution function for total shots faced during career for goalies who haven't played the first or last season of available data
shots %>%
  anti_join(goalies_to_remove, by = 'goalie_name') %>% # Filter goalies who haven't played the first or last season of available data
  count(goalie_name, name = 'shots_faced') %>%  # Count the number of shots faced for each goalie_name
  count(shots_faced, name = 'occurrences') %>%
  mutate(pct_oc = occurrences/sum(occurrences), cml_goalies = cumsum(pct_oc)) %>%  # Calculate cumulative sum of normalized counts
  filter(shots_faced < 3000) %>% 
  ggplot() +  # Start a ggplot object
  geom_hline(yintercept = c(0.250, 0.500, 0.750), col = 'white', alpha = 0.5, linetype = 'dashed') +  # Add horizontal dashed lines for areas of interest
  geom_vline(xintercept = c(32, 202, 2606), col = 'white', alpha = 0.5, linetype = 'dashed') +  # Add vertical dashed lines for areas of interest
  geom_line(aes(shots_faced, cml_goalies), col = single_color, stat = 'identity', lwd = 1) +  # Add bar plot with counts and normalized counts
  dark_theme() +  # Apply custom dark theme to the plot
  scale_y_continuous(breaks = c(0.250, 0.500, 0.750), labels = scales::percent) +  # Add y-axis labels for areas of interest
  scale_x_continuous(breaks = c(33, 202, 2606)) +  # Add x-axis breaks for areas of interest
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  labs(x = 'Shots Faced') + 
  rremove("ylab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-one.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


seasons_played <-
  shots_with_cml %>%
  distinct(goalie_name, season) %>%
  count(goalie_name, name = 'seasons_played')

shots_with_cml %>%
  group_by(goalie_name) %>%
  slice(n()) %>%
  ungroup() %>%
  left_join(seasons_played, by = 'goalie_name') %>%
  group_by(seasons_played) %>%
  summarize(
    adj_sv_pct = mean(adj_sv_pct),
    .groups = 'drop'
  ) %>%
  ggplot() +
  geom_line(aes(seasons_played, adj_sv_pct), col = single_color, lwd = 1) +  # Create facet panels based on status variable
  scale_y_continuous(labels = scales::percent) +  # Customize y-axis labels as percentages
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA),  # Remove legend key border
    axis.text.x = element_blank(),
    strip.text = element_text(colour = 'white'),  # Customize facet strip text color
    strip.background = element_rect(colour = "black", fill = "black")  # Customize facet strip background
  ) +
  labs(x = 'Seasons Played') +
  rremove("ylab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-two.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)



cleaned_careers <- 
  career_statistics %>% 
  anti_join(goalies_to_remove, by = 'goalie_name') %>%  # Filter goalies who haven't played the first or last season of available data
  mutate(adj_goals = round(adj_goals, 0)) %>%
  filter(adj_goals > 0)

# Fit a beta distribution to the career save percentages using maximum likelihood estimation
beta_prior_under = fitdist(cleaned_careers %>% filter(shots > 50, shots < 1500, adj_sv_pct < 0.97, adj_sv_pct > 0.8) %>% pull(adj_sv_pct), "beta")

# Extract the estimated shape parameters from the fitted beta distribution
beta_under_1 = beta_prior_under$estimate[1]
beta_under_2 = beta_prior_under$estimate[2]

beta_under_1/(beta_under_1 + beta_under_2)

# Calculate the prior probability densities for the sequence of save percentage values
beta_prior_under_points = dbeta(sv_pct_fits, shape1 = beta_under_1, shape2 = beta_under_2)

# Normalize probabilities to sum to 1
beta_prior_under_points = beta_prior_under_points/sum(beta_prior_under_points)


# Fit a beta distribution to the career save percentages using maximum likelihood estimation
beta_prior_over = fitdist(cleaned_careers %>% filter(shots > 1500) %>% pull(adj_sv_pct), "beta")

# Extract the estimated shape parameters from the fitted beta distribution
beta_over_1 = beta_prior_over$estimate[1]
beta_over_2 = beta_prior_over$estimate[2]

beta_over_1/(beta_over_1 + beta_over_2)

# Calculate the prior probability densities for the sequence of save percentage values
beta_prior_over_points = dbeta(sv_pct_fits, shape1 = beta_over_1, shape2 = beta_over_2)

# Normalize probabilities to sum to 1
beta_prior_over_points = beta_prior_over_points/sum(beta_prior_over_points)

ggplot() +
  geom_density(data = cleaned_careers %>% filter(shots > 50) %>% mutate(status = ifelse(shots > 1500, '1500+', '-1500')), 
               aes(adj_sv_pct, fill = status), alpha = 0.75) +
  geom_line(aes(sv_pct_fits, beta_prior_under_points*1000), col = 'white', lwd = 1, alpha = 0.75, linetype = 'dashed') +
  geom_line(aes(sv_pct_fits, beta_prior_over_points*1000), col = 'white', lwd = 1, alpha = 0.75, linetype = 'dashed') +
  scale_fill_manual('Shots Faced', values = c(single_color, 'grey')) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.position="bottom",
    axis.text.y = element_blank()  # Remove y-axis text
  ) +
  scale_x_continuous(limits = c(0.85, 1), labels = scales::percent) +
  rremove("xlab") +
  rremove("ylab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-three.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)




shots_with_cml <-
  shots %>%
  left_join(exp_f_by_season, by = 'season') %>%
  anti_join(goalies_to_remove, by = 'goalie_name') %>% # Filter goalies who haven't played the first or last season of available data
  arrange(goalie_name, season, game_id) %>%
  group_by(goalie_name) %>%
  mutate(
    cml_shots = 1:n(),
    cml_xg = cumsum(x_goal),
    cml_g = cumsum(goal),
    cml_sv_pct = 1 - (cml_g/cml_shots),
    exp_f = 1 - (cml_xg/cml_shots),
    sv_pct_a_exp = cml_sv_pct - exp_f,  # Calculate adjusted save percentage
    adj_sv_pct = season_exp_f + sv_pct_a_exp,  # Calculate adjusted save percentage
    adj_saves = adj_sv_pct*cml_shots,  # Calculate adjusted saves
    adj_goals = cml_shots - adj_saves  # Calculate adjusted saves
    ) %>%
  ungroup() 

goalie_status <-
  shots_with_cml %>%
  group_by(goalie_name) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(status = ifelse(cml_shots > 1500, 1, 0)) %>%
  select(goalie_name, status)

model_set <-
  shots_with_cml %>%
  left_join(goalie_status, by = 'goalie_name') %>%
  filter(cml_shots < 1500) %>%
  select(goalie_name, cml_shots, cml_xg, cml_sv_pct, adj_sv_pct, status) #%>%
  #mutate(across(c(cml_shots, adj_sv_pct), ~(. - mean(.))/sd(.)))

glm_model <- glm(status ~ cml_shots + adj_sv_pct, model_set, family = 'binomial')
model_set$pred <- predict(glm_model, model_set, type = 'response')

model_set %>%
  arrange(pred) %>%
  mutate(
    pred_rank = 1:n(),
    pred_bin = pred_rank %% 100,
    pred_bin = ifelse(pred_bin == 1, 1, 0),
    pred_bin = cumsum(pred_bin)) %>%
  group_by(pred_bin) %>%
  summarize(pred = mean(pred), status = mean(status)) %>%
  ungroup() %>%
  ggplot() +
  geom_abline(slope = 1, intercept = 0, col = 'white', alpha = 0.75, linetype = 'dashed') +
  geom_line(aes(pred, status), col = single_color, lwd = 1) +
  scale_x_continuous(breaks = seq(0.2, 0.8, 0.2), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0.2, 0.8, 0.2), labels = scales::percent) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
  ) +
  labs(x = '\nPredicted probability of facing over 1500 shots', y = 'Percentage of time it occurred\n')

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-four.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)









shots_with_post <- 
  shots_with_cml %>%
  select(-cml_g, -goalie_id, -game_id, -season) %>%
  mutate(
    cml_adj_saves = adj_sv_pct*cml_shots,
    pred = predict(glm_model, ., type = 'response'),
    pred = ifelse(cml_shots > 1500, 1, pred),
    post_good = ((beta_good_1 + cml_adj_saves)/(beta_good_1 + beta_good_2 + cml_shots)),
    post_bad = ((beta_bad_1 + cml_adj_saves)/(beta_bad_1 + beta_bad_2 + cml_shots)),
    post_sv_pct = (pred*(beta_good_1 + cml_adj_saves)/(beta_good_1 + beta_good_2 + cml_shots)) + 
      ((1 - pred)*(beta_bad_1 + cml_adj_saves)/(beta_bad_1 + beta_bad_2 + cml_shots))
    )

shots_with_post %>%
  group_by(goalie_name) %>%
  slice(n()) %>%
  ungroup() %>% 
  filter(cml_shots > 10) %>% 
  ggplot() +
  geom_point(aes(cml_shots, adj_sv_pct), col = single_color, alpha = 0.75) +
  geom_smooth(aes(cml_shots, adj_sv_pct), col = single_color, alpha = 0.25) +
  scale_x_log10() +
  scale_y_continuous(labels = scales::percent) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
  ) +
  rremove("xlab") +
  rremove("ylab")

shots_with_post %>%
  group_by(goalie_name) %>%
  slice(n()) %>%
  ungroup() %>% 
  filter(cml_shots > 10) %>% 
  ggplot() +
  geom_hline(yintercept = (1806.619 - 112.9502)/(1806.619), col = 'white', alpha = 0.5, linetype = 'dashed')  + 
  geom_hline(yintercept = new_start, col = 'white', alpha = 0.5, linetype = 'dashed')  + 
  geom_point(aes(cml_shots, post_sv_pct), col = single_color, alpha = 0.75) +
  geom_smooth(aes(cml_shots, post_sv_pct), col = single_color, alpha = 0.25) +
  scale_x_log10() +
  scale_y_continuous(labels = scales::percent) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
  ) +
  rremove("xlab") +
  rremove("ylab")


shots_with_cml %>%
  filter(goalie_name == 'Braden Holtby') %>%
  slice(c(1, 1000, 5000, 10000, 15000, n())) %>%
  select(cml_shots, post_sv_pct) %>%
  data.frame()

shots_ex <-
  shots_with_cml %>% 
  group_by(status, cml_shots) %>%
  summarize(count = n(), sv_pct = median(post_sv_pct, na.rm = TRUE), .groups = 'drop') %>% 
  filter(count > 5) 

shots_ex_1 <-
  shots_ex %>%
  filter(cml_shots <= 1500) %>%
  ggplot() +
  geom_line(aes(cml_shots, sv_pct, col = status), alpha = 0.80) +
  geom_hline(yintercept = 0.941, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  scale_color_manual('', values = c("#FF0000", "#FFAA00", "#FFFF00", "white")) +
  labs(x = '\nCumulative Shots', y = 'pAdjSV%\n') +
  scale_y_continuous(breaks = c(0.936, 0.938, 0.940, 0.942), labels = scales::percent) +  # Customize y-axis labels as percentages
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA)
  ) + rremove("ylab") + rremove("xlab")

shots_ex_2 <-
  shots_ex %>%
  filter(cml_shots <= 10000) %>%
  ggplot() +
  geom_line(aes(cml_shots, sv_pct, col = status), alpha = 0.80) +
  geom_hline(yintercept = 0.941, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  scale_color_manual('', values = c("#FF0000", "#FFAA00", "#FFFF00", "white")) +
  labs(x = '\nCumulative Shots', y = '') +
  scale_y_continuous(breaks = c(0.936, 0.938, 0.940, 0.942), labels = scales::percent) +  # Customize y-axis labels as percentages
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA),
  ) + 
  rremove("ylab") + 
  rremove("xlab")


ggarrange(shots_ex_1, shots_ex_2, ncol=2, nrow=1, common.legend = TRUE, legend="top", labels = NULL) + bgcolor("black")


# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-four.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 7.5,  # Set the width of the plot
  height = 4.5,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


shots_with_cml %>% 
  group_by(status, cml_shots) %>%
  summarize(count = n(), age = mean(age, na.rm = TRUE), .groups = 'drop') %>% 
  filter(cml_shots < 5000, count > 3) %>% #filter(cml_shots == 1000)
  ggplot() +
  geom_line(aes(cml_shots, age, col = status)) +
  geom_hline(yintercept = 24.6, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  geom_hline(yintercept = 26.3, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  geom_hline(yintercept = 27.4, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  geom_vline(xintercept = 1000, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a vertical line
  scale_y_continuous(breaks = c(23, 24.6, 26.3, 27.4, 29)) +  # Customize y-axis labels as percentages
  scale_color_manual('', values = c("#FF0000", "#FFAA00", "#FFFF00", "white")) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA)
  ) + 
  rremove("ylab") + 
  rremove("xlab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-six.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)




group_size = 500
season_statistics <-
  shots %>%
  arrange(goalie_name, season, game_id) %>%
  group_by(goalie_name) %>%
  mutate(shot_count = 1:n()) %>%
  ungroup() %>%
  mutate(shot_group = as.integer((shot_count - 1)/group_size)) %>%
  group_by(goalie_name, shot_group) %>%  # Group shots by goalie name
  dplyr::summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = (0.9418437 + (sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    adj_goals = shots - adj_saves,  # Calculate adjusted saves
    .groups = 'drop'
  )

model_lm <-
  shots_with_ages %>%
  arrange(goalie_name, season, game_id) %>%
  group_by(goalie_name) %>%
  mutate(shot_count = 1:n()) %>%
  ungroup() %>%
  group_by(goalie_name) %>%
  mutate(
    cml_shots = 1:n(),
    cml_xg = cumsum(x_goal),
    cml_g = cumsum(goal),
    cml_sv_pct = 1 - (cml_g/cml_shots),
    exp_f = 1 - (cml_xg/cml_shots),
    adj_sv_pct = 0.9399938 + cml_sv_pct - exp_f,
    cml_adj_saves = adj_sv_pct*cml_shots,
    post_sv_pct = (beta_1 + cml_shots - cml_adj_saves)/(beta_1 + beta_2 + cml_shots),  # Calculate adjusted save percentage
    post_sv_pct = 1 - post_sv_pct
  ) %>%
  ungroup() %>%
  mutate(shot_group = as.integer((shot_count - 1)/group_size)) %>%
  group_by(goalie_name, shot_group) %>%  # Group shots by goalie name
  select(goalie_name, shot_group, age, cml_shots, cml_adj_saves, cml_sv_pct, post_sv_pct) %>%
  group_by(goalie_name, shot_group) %>%
  slice(n()) %>%
  ungroup() %>%
  left_join(season_statistics, by = c('goalie_name', 'shot_group')) %>%
  group_by(goalie_name) %>%
  mutate(
    post_sv_pct_1 = lag(post_sv_pct, 1),
    adj_sv_pct_1 = lag(adj_sv_pct, 1),
    cml_shots_1 = lag(cml_shots, 1)
  ) %>%
  ungroup() %>%
  drop_na()


cor_vec = vector()

for (i in 1:500) {
  
  boot_lm <- 
    model_lm %>%
    sample_n(n(), replace = TRUE) %>%
    mutate(pred_adj_sv_pct = predict(lm(adj_sv_pct ~ post_sv_pct_1 + age + cml_shots_1, .), .))
  
  boot_rsq <-
    boot_lm %>%
    summarize(
      corr = cor(pred_adj_sv_pct, adj_sv_pct),
      adj_r_sq = summary(lm(adj_sv_pct ~ post_sv_pct_1 + age + cml_shots_1 + adj_sv_pct_1, .))$adj.r.squared) %>%
    pull(adj_r_sq)
  
  cor_vec[i] = boot_rsq
  
}

mean(cor_vec)
hist(cor_vec)

plot(boot_lm$pred_adj_sv_pct, boot_lm$adj_sv_pct)
print(summary(lm(adj_sv_pct ~ post_sv_pct_1 + age + cml_shots_1 + adj_sv_pct_1, boot_lm)))


shots_with_ages %>%
  distinct(goalie_name, season, game_id) %>%
  group_by(goalie_name) %>%
  mutate(goalie_game = 1:n()) %>%
  ungroup()



exp_f_by_season <-
  shots %>%
  group_by(season) %>%
  summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    exp_f = 1 - (exp_g/shots)  # Calculate expected save percentage
  ) %>%
  select(season, season_exp_f = exp_f)

model <- loess(cml_delta ~ age, data = raw_delta_ages, span = 1)
xrange <- range(raw_delta_ages$age)
xseq <- seq(from=xrange[1], to=xrange[2], length=17)
pred <- predict(model, newdata = data.frame(age = xseq), se=TRUE)

y = unname(pred$fit)
y = y - max(y)
age_adjustment <- 
  tibble(age = xseq, age_adj = -y) %>%
  add_row(age = 20, age_adj = 0.00240) %>%
  add_row(age = 21, age_adj = 0.00180) %>%
  add_row(age = 39, age_adj = 0.00615) %>%
  add_row(age = 40, age_adj = 0.00715)



# Calculate career statistics for goalies
career_statistics <- 
  shots_with_ages %>%  # tibble containing shot data loaded by the 'data' script
  mutate(age = round(age, 0)) %>%
  drop_na() %>% 
  left_join(exp_f_by_season, by = 'season') %>%
  left_join(age_adjustment, by = 'age') %>% 
  group_by(goalie_name, season) %>%  # Group shots by goalie name
  dplyr::summarize(
    age = max(age),
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = (1 - (exp_g/shots)) - mean(age_adj),  # Calculate expected save percentage
    adj_exp_g = (1 - exp_f)*shots,
    sv_pct_a_exp = sv_pct - exp_f,  # Calculate adjusted save percentage
    adj_sv_pct = mean_exp_f + sv_pct_a_exp,  # Calculate adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    adj_goals = shots - adj_saves,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>% 
  group_by(goalie_name) %>%
  summarize(
    age = max(age),
    shots = sum(shots),  # Count the number of shots
    goals = sum(goals),  # Sum the number of goals
    ex_g = sum(adj_exp_g),
    exp_f = 1 - (ex_g/shots),
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    adj_saves = sum(adj_saves),
    adj_goals = sum(adj_goals),
    adj_sv_pct = adj_saves/(adj_saves + adj_goals),
    inv_adj_sv_pct = 1 - adj_sv_pct,
    .groups = 'drop'
  ) %>%
  #dplyr::filter(exp_g > 10) %>%  # Filter rows where expected goals is greater than 10
  drop_na()  # Drop rows with missing values


# Calculate adjusted save percentage and posterior alpha and beta values
career_posteriors <- 
  career_statistics %>%
  mutate(
    post_sv_pct = (beta_1 + shots - adj_saves)/(beta_1 + beta_2 + shots),  # Calculate adjusted save percentage
    post_sv_pct = 1 - post_sv_pct, 
    alpha_post = beta_1 + shots - adj_saves,  # Calculate posterior alpha values
    beta_post = beta_2 + adj_saves,  # Calculate posterior beta values
    better_avg = map2_dbl(alpha_post, beta_post, ~mean(rbeta(100000, shape1 = .x, shape2 = .y) < 1 - mean(exp_f_by_season$season_exp_f)))  # Calculate proportion of values greater than median_sv_pct
  )



career_posteriors %>%
  anti_join(goalies_to_remove, by = 'goalie_name') %>% # Filter goalies who haven't played the first or last season of available data
  ggplot(aes(log(shots), post_sv_pct)) +
  geom_point(col = single_color) +
  geom_smooth(col = 'white', span = 1.1, alpha = 0.20, fill = 'white') + # Add smoothed line to the plot
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA)
  ) + 
  rremove("ylab") + 
  rremove("xlab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-six.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)

# weighted average age fenwick adjustment (age 21, 300 shots; age 22, 600 shots -> 0.33*adj_21 + 0.67*adj_22)

shots_with_ages %>%
  filter(goalie_name == 'Adin Hill')
