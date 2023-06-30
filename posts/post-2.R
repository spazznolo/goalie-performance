
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
    adj_sv_pct = mean_exp_f + #(mean_exp_f - mean(season_exp_f)) + 
      sv_pct_a_exp,  # Calculate adjusted save percentage
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
    inv_adj_sv_pct = 1 - adj_sv_pct,
    .groups = 'drop'
  ) %>%
  dplyr::filter(shots > 200) %>%  # Filter rows where expected goals is greater than 10
  drop_na()  # Drop rows with missing values

# Calculate median career average save percentage
median_sv_pct <- median(career_statistics$adj_sv_pct)

# Generate sequence of save percentage values
sv_pct_fits = seq(min(career_statistics$adj_sv_pct), max(career_statistics$adj_sv_pct), length = 45)


# Fit prior distributions to the observed career save percentages

# Fit a weibull distribution to the career save percentages using maximum likelihood estimation
weibull_prior = fitdistr(career_statistics$adj_sv_pct, "weibull")

# Extract the estimated shape parameters from the fitted weibull distribution
weibull_1 = weibull_prior$estimate[1]
weibull_2 = weibull_prior$estimate[2]

# Calculate the prior probability densities for the sequence of save percentage values
weibull_prior_points = dweibull(sv_pct_fits, shape = weibull_1, scale = weibull_2)

# Normalize probabilities to sum to 1
weibull_prior_points = weibull_prior_points/sum(weibull_prior_points)


# Fit a gamma distribution to the career save percentages using maximum likelihood estimation
gamma_prior = fitdistr(career_statistics$adj_sv_pct, "gamma")

# Extract the estimated shape parameters from the fitted gamma distribution
gamma_1 = gamma_prior$estimate[1]
gamma_2 = gamma_prior$estimate[2]

# Calculate the prior probability densities for the sequence of save percentage values
gamma_prior_points = dgamma(sv_pct_fits, shape = gamma_1, rate = gamma_2)

# Normalize probabilities to sum to 1
gamma_prior_points = gamma_prior_points/sum(gamma_prior_points)


# Fit a beta distribution to the career save percentages using maximum likelihood estimation
beta_prior = fitdist(career_statistics$adj_sv_pct, "beta")

# Extract the estimated shape parameters from the fitted beta distribution
beta_1 = beta_prior$estimate[1]
beta_2 = beta_prior$estimate[2]

# Calculate the prior probability densities for the sequence of save percentage values
beta_prior_points = dbeta(sv_pct_fits, shape1 = beta_1, shape2 = beta_2)

# Normalize probabilities to sum to 1
beta_prior_points = beta_prior_points/sum(beta_prior_points)


# Create a histogram of career save percentages overlaid with the prior probability densities
ggplot() +
  geom_histogram(data = career_statistics, aes(adj_sv_pct), fill = single_color, col = 'black', alpha = 0.75, bins = 20) +
  geom_line(aes(sv_pct_fits, beta_prior_points*nrow(career_statistics)*45/20), col = 'white', lwd = 1, alpha = 0.75) +
  geom_line(aes(sv_pct_fits, weibull_prior_points*nrow(career_statistics)*45/20), col = 'red', lwd = 1, alpha = 0.75) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank()  # Remove y-axis ticks
  ) +
  labs(x = '', y = '')

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-performance-2-1.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)



# Calculate adjusted save percentage and posterior alpha and beta values
career_posteriors <- 
  career_statistics %>%
  mutate(
    post_sv_pct = (beta_1 + adj_saves)/(beta_1 + beta_2 + shots),  # Calculate adjusted save percentage
    alpha_post = beta_1 + adj_saves,  # Calculate posterior alpha values
    beta_post = beta_2 + shots - adj_saves,  # Calculate posterior beta values
    better_avg = map2_dbl(alpha_post, beta_post, ~mean(rbeta(100000, shape1 = .x, shape2 = .y) > median(career_statistics$adj_sv_pct)))  # Calculate proportion of values greater than median_sv_pct
  )


# Function to obtain random outcomes for a specific goalie from career_posteriors tibble
get_random_outcomes <- function(goalie_name_) {
  
  # Filter the tibble based on the goalie_name
  filtered_data <- career_posteriors %>% filter(goalie_name == goalie_name_)
  
  # Check if any rows match the condition
  if (nrow(filtered_data) > 0) {
    
    # Extract the alpha_post and beta_post values from the filtered tibble
    alpha_post <- filtered_data$alpha_post
    beta_post <- filtered_data$beta_post
    
    # Use map2_dbl to iterate over the values and compute the mean of the rbeta samples
    result <- rbeta(100000, shape1 = alpha_post, shape2 = beta_post)
    
    # Return the resulting vector of random outcomes
    return(result)
  } else {
    # Handle the case when no rows match the condition
    return("No data found for the specified goalie")
  }
}

# Function to compare two goalies based on their random outcomes
comp_h2h <- function(goalie_a, goalie_b) {
  
  # Obtain random outcomes for goalie_a
  outcomes_a <- get_random_outcomes(goalie_a)
  
  # Obtain random outcomes for goalie_b
  outcomes_b <- get_random_outcomes(goalie_b)
  
  # Check if any outcomes are available for both goalies
  if (length(outcomes_a) == 0 || length(outcomes_b) == 0) {
    stop("Not enough data available for comparison.")
  }
  
  # Compute the mean proportion of outcomes where goalie_a performs better than goalie_b
  return(paste("Probability that", goalie_a, "is better than", goalie_b, "is", 100*mean(outcomes_a > outcomes_b), "%."))
}

comp_h2h('Jeremy Swayman', 'Jake Oettinger')

# Function to plot comparison of two goalie posterior distributions
plot_h2h_dists <- function(goalie_a, goalie_b) {
  
  # Filter the career_posteriors tibble for the specified goalies and calculate the adjusted save distributions
  adj_sv_dist <-
    career_posteriors %>%
    filter(goalie_name %in% c(goalie_a, goalie_b)) %>%
    transmute(
      goalie_name, post_sv_pct,
      adj_sv_dist = pmap(list(alpha_post, beta_post), ~ dbeta(sv_pct_fits, shape1 = ..1, shape2 = ..2))
    )

  # Create a histogram of samples from the prior distribution, adjusted save distributions, and vertical lines for medians and adjusted save percentages
  ggplot() +
    geom_vline(aes(xintercept = median(career_statistics$adj_sv_pct)), lwd = 0.75, col = single_color, alpha = 0.35, linetype = 'dashed') +
    geom_vline(aes(xintercept = adj_sv_dist$post_sv_pct[1]), lwd = 0.75, col = 'white', alpha = 0.60, linetype = 'dashed') +
    geom_histogram(aes(sample(sv_pct_fits, 100000, prob = beta_prior_points, replace = TRUE)), fill = single_color, col = 'black', alpha = 0.5, bins = 45) +
    geom_vline(aes(xintercept = adj_sv_dist$post_sv_pct[2]), lwd = 0.75, col = 'grey', alpha = 0.60, linetype = 'dashed') +
    geom_line(aes(sv_pct_fits, 100000*adj_sv_dist$adj_sv_dist[[2]]/sum(adj_sv_dist$adj_sv_dist[[2]])), col = 'grey', lwd = 0.75, alpha = 0.75) +
    geom_line(aes(sv_pct_fits, 100000*adj_sv_dist$adj_sv_dist[[1]]/sum(adj_sv_dist$adj_sv_dist[[1]])), col = 'white', lwd = 0.75, alpha = 0.75) +
    annotate("text", x = 0.915, y = 4000, label = adj_sv_dist$goalie_name[1], col = 'white') +
    annotate("text", x = 0.915, y = 6000, label = adj_sv_dist$goalie_name[2], col = 'grey') +
    dark_theme() +
    theme(
      panel.grid.major = element_line(color = 'black'),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(x = '', y = '') +
    scale_y_continuous()
  
}

# Example of plot_h2h_dists function
plot_h2h_dists('Jeremy Swayman', 'Jake Oettinger')

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-performance-2-2.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)



cor_sv_adj = round(cor(career_posteriors$sv_pct, career_posteriors$adj_sv_pct), 3)

# Compare raw save percentage to adjusted save percentages
sv_adj <-
  career_posteriors %>%
  ggplot() +
  geom_point(aes(sv_pct, adj_sv_pct, col = exp_g), alpha = 0.75) +
  scale_colour_gradientn("xG Faced", colours = multiple_colors(10)) +
  dark_theme() +  # Apply dark theme to the plot
  annotate("text", x = 0.912, y = 0.95, label = paste0('cor = ', cor_sv_adj), col = 'white') +
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  labs(x = '\nSV%', y = 'AdjSV%\n')  # Set x and y axis labels to empty


cor_adj_post = round(cor(career_posteriors$adj_sv_pct, career_posteriors$post_sv_pct), 3)

# Compare raw save percentage to adjusted save percentages
adj_post <-
  career_posteriors %>%
  ggplot() +
  geom_point(aes(adj_sv_pct, post_sv_pct, col = exp_g), alpha = 0.75) +
  scale_colour_gradientn("xG Faced", colours = multiple_colors(10)) +
  dark_theme() +  # Apply dark theme to the plot
  annotate("text", x = 0.913, y = 0.95, label = paste0('cor = ', cor_adj_post), col = 'white') +
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  labs(x = '\nAdjSV%', y = 'Posterior AdjSV%\n')  # Set x and y axis labels to empty


cor_sv_post = round(cor(career_posteriors$sv_pct, career_posteriors$post_sv_pct), 3)

# Compare raw save percentage to adjusted save percentages
sv_post <-
  career_posteriors %>%
  ggplot() +
  geom_point(aes(sv_pct, post_sv_pct, col = exp_g), alpha = 0.75) +
  scale_colour_gradientn("xG Faced", colours = multiple_colors(10)) +
  dark_theme() +  # Apply dark theme to the plot
  annotate("text", x = 0.913, y = 0.95, label = paste0('cor = ', cor_sv_post), col = 'white') +
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  labs(x = '\nSV%', y = 'Posterior AdjSV%\n')  # Set x and y axis labels to empty


# Compare expected goals faced to posterior adjusted save percentages
xg_post <-
career_posteriors %>%
  ggplot() +
  geom_point(aes(exp_g, post_sv_pct, col = exp_g), alpha = 0.75) +
  scale_colour_gradientn("xG Faced", colours = multiple_colors(10)) +
  dark_theme() +  # Apply dark theme to the plot
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  labs(x = '\nxG Faced', y = 'Posterior AdjSV%\n')  # Set x and y axis labels to empty

ggarrange(sv_adj, adj_post, sv_post, xg_post, ncol=2, nrow=2, common.legend = TRUE, legend="right") + bgcolor("black")


# Save the plot as a PNG file
ggsave(
  filename = 'goalie-performance-2-3.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 10,  # Set the width of the plot
  height = 6,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


  