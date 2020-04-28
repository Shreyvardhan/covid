claims <- read_csv("raw_data/claims.csv", skip =2 ) %>%
  rename(date = "X1", sa = "S.A.") %>%
  select(date, sa) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

res <- read_csv("raw_data/res.csv") %>%
  gather("date", "value", -Type, -Name) %>%
  filter(Type == "country") %>%
  select(date, Name, value) %>%
  rename(country = "Name") %>%
  mutate(date = as.Date(date, format="%m/%d")) %>%
  rename(bookings = "value")

flights <- read_csv("raw_data/flights.csv") %>%
  rename(date = "DateTime", 
         flights = "Number of flights") %>%
  select(date, flights)

search <- read_csv("raw_data/search.csv") %>%
  rename(date = "Week", search = "coronavirus")