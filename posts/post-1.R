
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



# Calculate career statistics for goalies
career_statistics <- 
  shots %>%  # tibble containing shot data loaded by the 'data' script
  group_by(goalie_name) %>%  # Group shots by goalie name
  summarize(
    shots = n(),  # Count the number of shots for each goalie
    goals = sum(goal),  # Calculate the total number of goals allowed
    saves = shots - goals, # Calculate the total number of saves made by subtracting the total goals from the total shots
    mean_sv_pct = 1 - mean(goal),  # Calculate the mean save percentage by subtracting the mean of goals from 1
    .groups = 'drop') %>%  # Drop grouping information
  filter(shots > 600) %>%  # Keep goalies with more than 750 shots
  drop_na()  # Remove rows with missing values



# Create a histogram of mean save percentages
career_statistics %>%
  ggplot() +
  geom_histogram(aes(mean_sv_pct), fill = single_color, col = 'black', alpha = 0.75, bins = 15) +  # Plot histogram with specified aesthetics
  dark_theme() +  # Apply a dark theme to the plot
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank()  # Remove y-axis ticks
  ) +
  labs(x = '', y = '') +  # Set x and y axis labels as blank
  xlim(0.915, 0.957)

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-performance-1-one.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)



# Fit prior distribution to the observed career save percentages

# Generate sequence of save percentage values to assign density functions to
sv_pct_fits = seq(0.915, 0.957, length = 45)

# Fit a beta distribution to the career save percentages using maximum likelihood estimation
goalie_prior = fitdist(career_statistics$mean_sv_pct, "beta", method = 'mle')

# Extract the estimated shape parameters from the fitted beta distribution
alpha_0 = goalie_prior$estimate[1]
beta_0 = goalie_prior$estimate[2]

# Calculate the prior probability densities for the sequence of save percentage values
prior_points = dbeta(sv_pct_fits, shape1 = alpha_0, shape2 = beta_0)

# Normalize probabilities to sum to 1
prior_points = prior_points/sum(prior_points)



# Create a histogram of career save percentages overlaid with the prior probability densities
ggplot() +
  geom_histogram(data = career_statistics, aes(mean_sv_pct), fill = single_color, col = 'black', alpha = 0.75, bins = 15) +
  geom_line(aes(sv_pct_fits, prior_points*180*3), col = 'white', lwd = 1, alpha = 0.75) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank()  # Remove y-axis ticks
  ) +
  labs(x = '', y = '') +
  xlim(0.915, 0.957)

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-performance-1-two.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)



# Calculate median career save percentage 
median_sv_pct <- median(career_statistics$mean_sv_pct)

# Calculate adjusted save percentage and posterior alpha and beta values
career_posteriors <- 
  career_statistics %>%
  mutate(
    adj_sv_pct = (alpha_0 + saves)/(alpha_0 + beta_0 + shots),  # Calculate adjusted save percentage
    alpha_post = alpha_0 + saves,  # Calculate posterior alpha values
    beta_post = beta_0 + goals,  # Calculate posterior beta values
    better_avg = map2_dbl(alpha_post, beta_post, ~mean(rbeta(1000000, shape1 = .x, shape2 = .y) > median_sv_pct))  # Calculate proportion of values greater than median_sv_pct
  ) %>%
  arrange(desc(adj_sv_pct))  # Arrange in descending order of adj_sv_pct


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

# Example of comp_h2h function
comp_h2h('Jeremy Swayman', 'Jake Oettinger')


# Function to plot comparison of two goalie posterior distributions
plot_h2h_dists <- function(goalie_a, goalie_b) {
  
  # Filter the career_posteriors tibble for the specified goalies and calculate the adjusted save distributions
  adj_sv_dist <-
    career_posteriors %>%
    filter(goalie_name %in% c(goalie_a, goalie_b)) %>%
    transmute(
      goalie_name, adj_sv_pct,
      adj_sv_dist = pmap(list(alpha_post, beta_post), ~ dbeta(sv_pct_fits, shape1 = ..1, shape2 = ..2))
    )
  
  # Create a histogram of samples from the prior distribution, adjusted save distributions, and vertical lines for medians and adjusted save percentages
  ggplot() +
    geom_histogram(aes(sample(sv_pct_fits, 100000, prob = prior_points, replace = TRUE)), fill = single_color, col = 'black', alpha = 0.5, bins = 45) +
    geom_vline(aes(xintercept = median_sv_pct), lwd = 0.75, col = 'grey', alpha = 0.35, linetype = 'dashed') +
    geom_line(aes(sv_pct_fits, 100000*adj_sv_dist$adj_sv_dist[[1]]/sum(adj_sv_dist$adj_sv_dist[[1]])), col = 'white', lwd = 0.75, alpha = 0.75) +
    geom_vline(aes(xintercept = adj_sv_dist$adj_sv_pct[1]), lwd = 0.75, col = 'white', alpha = 0.60, linetype = 'dashed') +
    annotate("text", x = .925, y = 4000, label = adj_sv_dist$goalie_name[1], col = 'white') +
    geom_line(aes(sv_pct_fits, 100000*adj_sv_dist$adj_sv_dist[[2]]/sum(adj_sv_dist$adj_sv_dist[[2]])), col = 'grey', lwd = 0.75, alpha = 0.75) +
    geom_vline(aes(xintercept = adj_sv_dist$adj_sv_pct[2]), lwd = 0.75, col = 'grey', alpha = 0.60, linetype = 'dashed') +
    annotate("text", x = .925, y = 5000, label = adj_sv_dist$goalie_name[2], col = 'grey') +
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
plot_h2h_dists('Jake Oettinger', 'Jeremy Swayman')

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-performance-1-three.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)
