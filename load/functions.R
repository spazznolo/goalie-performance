

# Function to download and extract a zip file to the 'data' folder
download_and_extract <- function(url) {
  temp <- tempfile()  # Create a temporary file to store the downloaded zip file
  download.file(url, temp)  # Download the zip file
  unzip(temp, exdir = "data/")  # Unzip the downloaded file and extract its contents to the specified directory
}


# Function to calculate the normalized entropy of a series
norm_ent <- function(s) {
  
  s1 <- which(s %in% 1)  # Find the indices of 1s in the series
  
  iet <- c(s1[1], diff(s1), length(s) + 1 - tail(s1, 1))  # Calculate the inter-event times
  
  iet <- iet / (length(s) + 1)  # Normalize the inter-event times
  
  h <- 1 + ((sum(log(iet) * iet)) / log(sum(s) + 1))  # Calculate the normalized entropy
  
  return(h)  # Return the normalized entropy
  
}


# Function to perform entropy test on a series
ent_test <- function(s) {
  
  n <- 10000  # Number of simulations
  
  # Perform random permutations of the series and calculate normalized entropy for each permutation
  x <- replicate(n, sample(s, length(s), replace = FALSE))
  x <- lapply(1:ncol(x), function(i) x[, i])
  
  # Calculate normalized entropy for each permutation
  y <- map_dbl(x, norm_ent)
  
  # Calculate the proportion of permutations that have lower normalized entropy than the original series
  return(sum(y < norm_ent(s)) / n)
  
}


# Function to perform Poisson season simulations
poisson_season_simulations <- function(goalie_, goal_share_) {
  
  total_goals <- 5.5
  goal_share_opp <- total_goals - goal_share_
  
  # Generate 10,000 simulations of goal differentials for each game
  results_ <- replicate(10000, {
    c(
      rpois(41, total_goals/2) - rpois(41, goal_share_),
      rpois(41, total_goals/2) - rpois(41, goal_share_opp)
    )
  })
  
  # Calculate standing points for each simulation
  standing_points <- apply(results_, 2, function(x) {
    (sum(x == 0) * 1.5) + (sum(x > 0) * 2)
  })
  
  # Create a tibble with the goalie and corresponding standing points
  return(
    tibble(
      goalie = goalie_,
      standing_points
    )
  )
  
}


# Function to calculate normalized entropy of a series weighted by t
norm_ent_xg <- function(s, t) {
  
  s2 <- cumsum(t)  # Calculate cumulative sum of t
  s1 <- s2[which(s %in% 1)]  # Find the indices of elements in s equal to 1 in s2
  
  iet <- c(t[1], diff(s1), sum(t) + 1 - tail(t, 1))  # Calculate inter-event times
  
  iet <- iet / (sum(t) + 1)  # Normalize inter-event times
  
  h <- 1 + ((sum(log(iet) * iet)) / log(sum(t) + 1))  # Calculate normalized entropy
  
  return(h)
  
}


# Function to calculate the entropy test for xG
ent_test_xg <- function(goals_, x_g_, x_gsaa_xg_) {
  
  pb$tick()  # Progress bar update
  
  n <- 10000  # Number of simulations
  x_g_adj_ <- x_g_ - (x_g_ * x_gsaa_xg_)  # Adjusted xG
  
  ok <- replicate(n, x_g_adj_)  # Generate simulations using adjusted xG
  ok <- lapply(1:ncol(ok), function(i) ok[, i])  # Convert matrix to list of vectors
  
  x <- replicate(n, rbinom(length(x_g_adj_), 1, x_g_adj_))  # Generate simulations using binomial distribution
  x <- lapply(1:ncol(x), function(i) x[, i])  # Convert matrix to list of vectors
  
  z <- map2_dbl(x, ok, norm_ent_xg)  # Calculate normalized entropy for each simulation
  
  return(sum(z < norm_ent_xg(goals_, x_g_adj_)) / n)  # Calculate the percentage of simulations with lower entropy
}


# Function to calculate standing points
std_pts <- function(s) {
  
  s1 <- rbinom(82 * 30, 1, 0.1) - s  # Generate random binomial simulations
  s2 <- split(s1, ceiling(seq_along(s1) / 30))  # Split the simulations into groups of 30
  s3 <- unlist(lapply(s2, sum))  # Calculate the sum of each group
  
  pts <- (sum(s3 > 0) * 2) + (sum(s3 == 0) * 1.5)  # Calculate the total points
  
  return(pts)  # Return the total points
}


# Function to create a dark theme for plots
dark_theme <- function() {
  
  theme(
    panel.grid.major = element_line(color = '#99a1b3'),  # Customize major grid lines
    panel.grid.minor = element_line(color = 'black'),  # Customize minor grid lines
    panel.background = element_rect(fill = 'black'),  # Set panel background color
    panel.border = element_blank(),  # Remove panel borders
    plot.background = element_rect(fill = "black", color = "black"),  # Set plot background color
    axis.text.x = element_text(color = 'white'),  # Customize x-axis tick labels
    axis.text.y = element_text(color = 'white'),  # Customize y-axis tick labels
    plot.title = element_text(face = 'bold', color = 'white', hjust = 0.5),  # Customize plot title
    plot.subtitle = element_text(face = 'bold', color = 'white'),  # Customize plot subtitle
    axis.title.x = element_text(color = 'white'),  # Customize x-axis title
    axis.title.y = element_text(color = 'white'),  # Customize y-axis title
    legend.background = element_rect(fill = "black", color = NA),  # Set legend background color
    legend.key = element_rect(color = "gray", fill = "black"),  # Customize legend key
    legend.title = element_text(color = "white"),  # Customize legend title
    legend.text = element_text(color = "white")  # Customize legend text
  )
  
}


# Define colors for consistency
single_color = "#FFD500"  # Define a single color
multiple_colors <- colorRampPalette(c('white', "#FFD500"))  # Define a color palette
  




