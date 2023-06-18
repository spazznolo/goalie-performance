
# Function to fetch goalie data for a specific year
get_goalie_data <- function(year) {
  # Append URL wrap to create complete links
  url <- paste0('https://www.hockey-reference.com/leagues/NHL_', year, '_goalies.html')
  
  # Read the HTML page containing the list of goalies for the specified year
  goalies_in_season <- read_html(url)
  
  # Create a tibble containing player names and URLs for the specified year
  goalie_data <- 
    tibble(
      'player' = goalies_in_season %>% html_nodes("td[data-stat='player']") %>% html_text(),
      'url' = goalies_in_season %>% html_nodes("td[data-stat='player']") %>% html_nodes("a") %>% html_attr("href")
    )
  
  Sys.sleep(3)  # Pause execution for 3 seconds to be more respectful of the website's servers
  
  return(goalie_data)
}

# Function to retrieve the date of birth (DOB) from a goalie URL
get_dob <- function(goalie_url) {
  # Append base URL to create complete links
  goalie_url <- paste0('https://www.hockey-reference.com', goalie_url)
  
  # Read the HTML page containing goalie information
  goalie_info <- read_html(goalie_url)

  # Extract date of birth from goalie URL
  dob <- tryCatch({
    goalie_info %>% 
      html_nodes("p") %>% 
      html_nodes("span#necro-birth") %>% 
      html_attr('data-birth')
  }, error = function(e) {
    NA  # Return NA in case of an error
  })
  
  Sys.sleep(3)  # Pause execution for 3 seconds to be more respectful of the website's servers
  
  return(dob)
}

# Fetch goalie data and date of birth for each year from 2007 to 2023
goalie_data <- 
  map_dfr(2007:2023, get_goalie_data) %>% # Get goalie data for years 2007 to 2023
  distinct(url, .keep_all = TRUE) %>%  # Remove duplicate URLs
  mutate(dob = map_chr(url, get_dob))  # Retrieve DOB for each goalie URL

# Run garbage collection to close unused connections
gc(verbose = TRUE)

write_csv(goalie_data, 'data/goalie_data_2007_2023.csv')
