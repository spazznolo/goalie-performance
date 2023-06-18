
# Load shot data for years 2007-2008 to 2022-2023
shots <- 
  map_dfr(c('data/shots_2007-2021.csv', 'data/shots_2022.csv'), 
          ~read_csv(.) %>%
            clean_names() %>% # Clean column names and make them consistent
            filter(home_skaters_on_ice == 5, away_skaters_on_ice == 5) %>% # Filter data for even strength (5v5)
            select(season, original_game_id = game_id, goalie_id = goalie_id_for_shot, goalie_name = goalie_name_for_shot, goal, x_goal)) # Select relevant columns

goalie_data <-
  read_csv('data/goalie_data_2007_2023.csv') %>%
  mutate(
    goalie_name = stri_trans_general(str = player, id = "Latin-ASCII"),
    goalie_name = gsub('[*]', '', goalie_name),
    dob = ymd(dob)) %>%
  select(goalie_name, dob)

convert_game_id <-
  shots %>%
  arrange(season, original_game_id) %>%
  distinct(season, original_game_id) %>%
  group_by(season) %>%
  mutate(game_id = 1:n()) %>%
  ungroup()

shots <-
  shots %>%
  left_join(convert_game_id, by = c('season', 'original_game_id')) %>%
  select(-original_game_id) %>%
  arrange(season, game_id) %>%
  mutate(
    goalie_name = case_when(
      goalie_name == 'Cal Petersen' ~ 'Calvin Petersen',
      goalie_name == 'Michael DiPietro' ~ 'Michael Dipietro',
      grepl('Berube', goalie_name) ~ 'Jean-Francois Berube',
      goalie_name == 'Olie Kolzig' ~ 'Olaf Kolzig',
      goalie_name == 'Zach Fucale' ~ 'Zachary Fucale',
      goalie_name == 'Edward Pasquale' ~ 'Eddie Pasquale',
      goalie_name == 'Dan Vladar' ~ 'Daniel Vladar',
      goalie_name == 'Zach Sawchenko' ~ 'Zachary Sawchenko',
      goalie_name == 'Ken Appleby' ~ 'Kenneth Appleby',
      TRUE ~ goalie_name
    )
  )

shots %>% filter(goalie_name == 'Zachary Sawchenko')
