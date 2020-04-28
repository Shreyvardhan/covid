h <- read_html("https://en.wikipedia.org/wiki/User:Michael_J/County_table")

tab <- h %>% html_nodes("table")

tab <- tab[[1]] %>% html_table

coords <- tab %>%
  select(State, "County [2]", Latitude, Longitude, FIPS) %>%
  rename(state = State, 
         county = "County [2]",
         lat = Latitude, 
         long = Longitude, 
         fips = FIPS) 

nytc <- read_csv("raw_data/nyt-county.csv") %>%
  filter(date == "2020-04-26") %>%
  mutate(fips = as.numeric(fips))

comb <- left_join(nytc, coords, by=c("county", "fips")) %>%
  select(county, state.x, cases, deaths, lat, long) %>%
  rename(state = state.x) %>%
  mutate(lat = substr(lat, 1, nchar(lat)-1), 
         long = substr(long, 2, nchar(long)-1),
         lat = as.double(lat),
         long = as.double(long),
         long = -1 * long)

nyt_states <- read_csv("raw_data/nyt-states.csv")
medal <- read_csv("raw_data/state-medal-count.csv")

state_coords <- medal %>%
  select(location, lat, lon) %>%
  rename(state = "location")

state_comb <- left_join(nyt_states, state_coords, by="state") %>%
  rename(long = lon)