library(tidyverse)
library(tidymodels)

# Ingest COVID
covid_url <-  'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
pop_url   <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv'

data   <- readr::read_csv(covid_url)
census <- readr::read_csv(pop_url) 

census <- census_raw %>%
  filter(COUNTY == '000') %>%
  rename(fips = STATE) %>%
  select(fips, POPESTIMATE2021, DEATHS2021, BIRTHS2021)  

state_data <- data %>%
  group_by(fips) %>%
  mutate(new_cases = pmax(0, cases - lag(cases)),
         new_deaths = pmax(0, deaths - lag(deaths))) %>%
  ungroup() %>%
  left_join(census, by = "fips") %>%
  mutate(y = year(date), m = month(date), 
         season = case_when(
           m %in% c(12,1,2) ~ "Winter",
           m %in% 3:5 ~ "Spring",
           m %in% 6:8 ~ "Summer",
           m %in% 9:11 ~ "Fall"
         )) %>%
  group_by(state, y, season) %>%
  mutate(season_cases = sum(new_cases, na.rm = TRUE),
         season_deaths = sum(new_deaths, na.rm = TRUE)) %>%
  distinct(state, y, season, .keep_all = TRUE) %>%
  ungroup() %>%
  select(state, contains("season"), contains("2021")) %>%
  drop_na() %>%
  mutate(logC = log(season_cases + 1),
         logD = log(season_deaths + 1))

skimr::skim(state_data)

### ML Applications

set.seed(123)
split <- initial_split(state_data, prop = .8, strata = season)
training <- training(split)
testing <- testing(split) 
folds <- vfold_cv(training, v = 10)

rec <- recipe(season_deaths ~ ., data = training) %>%
  step_rm(state) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_center(all_numeric_predictors()) 

lm_mod <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") 

rf_mod <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression") 

rf_mod2 <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression") 

b_mod <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression") 

nn_mod <- mlp(hidden = 10) %>%
  set_engine("nnet") %>%
  set_mode("regression") 

wf <- workflow_set(list(rec), list(lm_mod,
                                   rf_mod,
                                   rf_mod2,
                                   b_mod, 
                                   nn_mod)) %>%
  workflow_map(resamples = folds)

results <- wf %>% collect_metrics()
results %>% filter(.metric == "rmse") %>% arrange(mean)
autoplot(wf)

best_id <- results %>%
  filter(.metric=="rmse") %>%
  arrange(mean) %>%
  slice_head(n = 1) %>%
  pull(wflow_id)

final_wf <- wf %>%
  extract_workflow(best_id) %>%
  fit(data = training)

preds <- predict(final_wf, testing) %>%
  bind_cols(testing)

preds %>% metrics(truth = logD, estimate = .pred)

scatter <- ggplot(preds, aes(x = .pred, y = logD)) +
  geom_abline(lty = "dashed", color = "gray50") +
  geom_point(alpha = 0.7) +
  labs(
    x = "Predicted Log+1 Deaths",
    y = "Observed Log+1 Deaths",
    title = "Truth vs Predicted: Seasonal COVID Deaths"
  ) +
  theme_minimal()

print(scatter)
ggsave("truthvpredicteddeaths.png", scatter, width = 6, height = 6, dpi = 300)

