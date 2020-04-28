covid_approval <- read_csv("raw_data/covid_approval.csv") %>%
  rename(date = modeldate,
         approve = approve_estimate,
         disapprove = disapprove_estimate) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
  select(date, party, approve, disapprove)

covid_approval_all %>%
  filter(party = "all")

covid_concern <- read_csv("raw_data/covid_concern.csv") %>%
  rename(date = modeldate,
         very = very_estimate,
         somewhat = somewhat_estimate,
         not_very = not_very_estimate,
         not_at_all = not_at_all_estimate) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
  select(subject, date,very, somewhat, not_very, not_at_all)

economy <- covid_concern %>%
  filter(subject == "concern-economy")

infected <- covid_concern %>%
  filter(subject == "concern-infected")

approval <- read_csv("raw_data/approval.csv") %>%
  rename(date = modeldate,
         approve = approve_estimate,
         disapprove = disapprove_estimate) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y"))

approval_all <- approval %>%
  filter(subgroup == "All polls") %>%
  select(date, approve, disapprove)

nyt <- read_csv("raw_data/nyt.csv")

approval_lm <- left_join(nyt, approval, by="date")

covid_lm <- left_join(nyt, covid_approval, by="date")

economy_lm <- left_join(nyt, economy, by="date")

infected_lm <- left_join(nyt, infected, by="date")