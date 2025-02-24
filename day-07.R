# Samantha Nauman
# Date: 2/18/2025
# Daily Exercise 7

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)

# Question 1 - Line plot of the 6 states with the most cases

current_date <- max(covid$date)
current_date <- covid %>%
  filter(date == current_date)
state_cases <- current_date %>%
  group_by(state) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE)) %>%
  ungroup()
top_6_states <- state_cases %>%
  arrange(desc(total_cases)) %>%
  slice(1:6) %>%
  pull(state)
top_6_states
filter_6 <- covid %>%
  filter(state %in% top_6_states) %>%
  group_by(state, date) %>%
  summarize(cumulative_cases = sum(cases, na.rm = TRUE), .groups="drop")
ggplot(filter_6, aes(x=date,y=cumulative_cases,color=state,group=state))+
  geom_line(linewidth=1)+
  facet_wrap(~ state, scales="free_y")+
  labs(title="Cumulative Case Counts: COVID-19 Pandemic",x="Date",y="Cases")+
  theme_minimal()+
  theme(strip.text = element_text(size=12,face="bold"))
ggsave("img/top_6_states_graph.png",width=10,height=8)

# Question 2 - Making a column plot of daily total cases in the USA

daily_cases <- covid %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE), .groups="drop") 
ggplot(daily_cases, aes(x = date, y = total_cases)) +
  geom_col(fill = "darkred") +
  geom_smooth(se = FALSE, color = "black", method = "loess", span = 0.2, size = 4) +
  labs(title = "National Cumulative Case Counts: COVID-19 Pandemic",
       x = "Date", y = "Cases") +
  theme_bw()

ggsave("img/daily_total_cases.png", width = 10, height = 6)
