# LEARNING LAB 29: MULTIVARIATE FORECASTING OF CRUDE OIL PRICES
# SHINY API SERIES: SHINY + QUANDL API
# MULTIVARIATE TIME SERIES FORECASTING ----

# LIBRARIES -----

# Time Series
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(slider)

# Modeling
library(recipes)
library(rsample)

# Core
library(tidyverse)
library(tidyquant)


# Setup
forecast_horizon <- 3

# Read Data
us_petroleum_joined_m_tsbl <- read_rds("01_data/us_petroleum_joined_m_tsbl.rds")

# Compute lags based on forecast horizon
lag_settings_tbl <- us_petroleum_joined_m_tsbl %>%
    map_dfr(.f = function(x) {
        x %>%
            tail(24) %>%
            is.na() %>%
            sum()
    }) %>%
    pivot_longer(cols = everything()) %>%
    mutate(value = value + forecast_horizon) %>%
    pivot_wider(names_from = "name", values_from = "value")


# Apply Recipe - Major change is removing normalization (fable handles internally)
# - Major change is removing normalization (fable handles internally)
preprocess_03_rec <- recipe(price_wti ~ ., data = us_petroleum_joined_m_tsbl) %>%
    step_date(year_month, features = c("decimal")) %>%
    step_lag(price_wti,
             lag = lag_settings_tbl$price_wti) %>%
    step_lag(stock_us_refineries,
             lag = lag_settings_tbl$stock_us_refineries) %>%
    step_lag(financial_arca,
             lag = lag_settings_tbl$financial_arca) %>%
    step_rm(stock_us_refineries, financial_arca) %>%
    step_naomit(all_predictors()) %>%
    prep()


us_petroleum_prep_03_m_tsbl <- bake(preprocess_03_rec, us_petroleum_joined_m_tsbl) %>%
    as_tsibble(index = "year_month")

us_petroleum_prep_03_m_tsbl %>% glimpse()


# 9.0 ARIMA MODEL ----
train_test_splits <- initial_time_split(us_petroleum_prep_03_m_tsbl, prop = 0.8)

# BENCHMARK
# What is the benchmark?
# - NAIVE: 3 Month Look-Ahead
benchmark_tbl <- train_test_splits %>% 
    testing() %>%
    as_tibble() %>%
    select(contains("price_wti")) %>%
    summarize(mae = mean(abs(price_wti - lag_3_price_wti))) %>%
    add_column(.model = "NAIVE", .before = 1)

# 9.1 UNIVARIATE FORECAST ----
model_01_fit <- training(train_test_splits) %>%
    model(
        ARIMA(price_wti)
    )

model_01_fit %>% forecast(h = forecast_horizon)

model_01_fit %>% 
    forecast(h = forecast_horizon) %>% 
    autoplot(us_petroleum_prep_03_m_tsbl)

# 9.2 MULTIVARIATE FORECAST ----
model_02_fit <- training(train_test_splits) %>%
    model(
        ARIMA(price_wti),
        ARIMA(price_wti ~ lag_3_financial_arca),
        ARIMA(price_wti ~ lag_3_price_wti + lag_3_financial_arca),
        ARIMA(price_wti ~ lag_3_price_wti + lag_3_financial_arca + lag_6_stock_us_refineries),
        ARIMA(price_wti ~ lag_3_price_wti + lag_3_financial_arca + lag_6_stock_us_refineries + year_month_decimal)
    )

predictions_tsbl <- forecast(model_02_fit, new_data = testing(train_test_splits)) 

predictions_tsbl %>%
    mutate(.model = as_factor(.model)) %>%
    autoplot(testing(train_test_splits)) +
    facet_wrap(~.model, ncol = 3) +
    scale_color_tq() +
    theme_tq(base_size = 16) +
    theme(legend.direction = "vertical")

# NOTE - Should implement rolling predictions for the test dataset

# Calculate test accuracy - Not Rolling
predictions_tsbl %>%
    
    # Filter time
    # filter_index("2016-04" ~ "2016-06") %>%
    
    as_tibble() %>%
    select(.model, year_month, price_wti) %>%
    rename(.pred = price_wti) %>%
    mutate(.model = as_factor(.model)) %>%
    
    # Calculate MAE
    left_join(
        us_petroleum_joined_m_tsbl %>%
            as_tibble() %>%
            select(year_month, price_wti)
    ) %>%
    group_by(.model) %>%
    summarize(
        mae = mean(abs(price_wti - .pred))
    ) %>%
    mutate(.model = as.character(.model)) %>%
    bind_rows(benchmark_tbl)

# Calculate test accuracy - Rolling
predictions_tsbl %>%
    
    as_tibble() %>%
    select(.model, year_month, price_wti) %>%
    rename(.pred = price_wti) %>%
    mutate(.model = as_factor(.model)) %>%
    
    # Calculate MAE
    left_join(
        us_petroleum_joined_m_tsbl %>%
            as_tibble() %>%
            select(year_month, price_wti)
    ) %>%
    
    # Grouped window calculation
    group_by(.model) %>%
    mutate(rolling_mae = slide2_dbl(.x = price_wti, 
                                    .y = .pred, 
                                    .f = function(x, y) mean(abs(x - y)), 
                                    .before   = forecast_horizon - 1, 
                                    .step     = 1, 
                                    .complete = TRUE)) %>%
    summarize(
        mae = mean(rolling_mae, na.rm = TRUE)
    ) %>%
    mutate(.model = as.character(.model)) %>%
    bind_rows(benchmark_tbl)

# 10.0 SPECIAL NOTES ----
# - Did not implement backtesting 
# - Out-Of-Sample Accuracy - Univariate ARIMA is artificially high

