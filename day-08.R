# Samantha Nauman
# Date: 2/23/2025
# Daily Exercise 08

# loading in the data
library(tidyverse)
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid <- read_csv(url)
head(covid)

# creating a new data frame
df = data.frame(region = state.region,
                state = state.name,
                state_abb = state.abb)
head(df)

# join to raw COVID data
covid <- covid %>%
  left_join(df, by = "state") %>%
              filter(!is.na(state))

head(covid)

# split apply the joined data to find daily, cumulative, cases and deaths for each region
covid_region <- covid %>%
  group_by(date, region) %>%
  summarize(daily_cases = sum(cases, na.rm = TRUE),
            daily_deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup()
covid_region <- covid_region %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(cumulative_cases = cumsum(daily_cases),
         cumulative_deaths = cumsum(daily_deaths)) %>%
  ungroup()

# pivot from wide to long format
covid_long <- covid_region %>%
  pivot_longer(cols = c(cumulative_cases, cumulative_deaths),
               names_to = "metric",
               values_to = "count")
covid_long <- covid_long %>%
  filter(!is.na(region))

# plot data (setup, layers, facets, themes)
ggplot(covid_long, aes(x = date, y = count, color = region)) +
  geom_line(size = 2) +
  facet_grid(metric~region, scales = "free_y") +
  theme_gdocs() + 
  labs(title = "Cumulative COVID-19 Cases & Deaths by USA Region",
       subtitle = "COVID-19 Data: NY-Times",
       x = "Date",
       y = "Count",
       color = "Region",
       caption = "Daily Exercise 08") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")

# save image to img
ggsave("img/cumulative_cases_region.png", width = 10, height = 6)
