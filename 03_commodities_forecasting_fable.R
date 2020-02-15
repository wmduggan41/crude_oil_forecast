# LEARNING LAB 29: MULTIVARIATE FORECASTING OF CRUDE OIL PRICES
# SHINY API SERIES: SHINY + QUANDL API
# MULTIVARIATE TIME SERIES FORECASTING ----

# LIBRARIES -----

# Time Series
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

# Modeling
library(recipes)
library(rsample)

# Core
library(tidyverse)
library(tidyquant)


# Read Data
us_petroleum_joined_m_tsbl <- read_rds("01_data/us_petroleum_joined_m_tsbl.rds")

# Apply Recipe - Major change is removing normalization (fable handles internally)
preprocess_03_rec <- recipe(price_wti ~ ., data = us_petroleum_joined_m_tsbl) %>%
    step_date(year_month, features = c("decimal")) %>%
    step_lag(price_wti, lag = 1) %>%
    step_lag(demand_population, lag = 2) %>%
    step_lag(stock_us_total, lag = 4) %>%
    step_lag(stock_us_refineries, lag = 4) %>%
    step_lag(financial_arca, lag = 1) %>%
    step_lag(financial_cpi, lag = 4) %>%
    step_rm(demand_imports, demand_population, demand_product_supplied,
            supply_exports, supply_field_production, supply_operating_capacity,
            stock_us_total, stock_us_refineries, stock_us_tank_farms,
            financial_arca, financial_sp500, financial_cpi) %>%
    step_naomit(all_predictors()) %>%
    prep()

us_petroleum_prep_03_m_tsbl <- bake(preprocess_03_rec, us_petroleum_joined_m_tsbl) %>%
    as_tsibble(index = "year_month")

us_petroleum_prep_02_m_tsbl %>% glimpse()


# 9.0 ARIMA MODEL ----
train_test_splits <- initial_time_split(us_petroleum_prep_03_m_tsbl, prop = 0.8)

# BENCHMARK
# What is the benchmark?
# - NAIVE: 1 Month Look-Ahead

benchmark_tbl <- train_test_splits %>% 
    testing() %>%
    as_tibble() %>%
    summarize(mae = mean(abs(price_wti - lag_1_price_wti))) %>%
    add_column(.model = "NAIVE", .before = 1)

# UNIVARIATE FORECAST ----
model_01_fit <- training(train_test_splits) %>%
    model(
        ARIMA(price_wti)
    )

testing(train_test_splits)

model_01_fit %>% forecast(h = 1)

model_01_fit %>% forecast(h = 12)

model_01_fit %>% forecast(h = 12) %>% autoplot(us_petroleum_prep_02_m_tsbl)

# MULTIVARIATE FORECAST ----
model_02_fit <- training(train_test_splits) %>%
    model(
        ARIMA(price_wti),
        ARIMA(price_wti ~ lag_1_financial_arca),
        ARIMA(price_wti ~ lag_1_price_wti + lag_1_financial_arca)
        ,
        ARIMA(price_wti ~ lag_1_price_wti + lag_1_financial_arca + lag_4_stock_us_refineries),
        ARIMA(price_wti ~ lag_1_price_wti + lag_1_financial_arca + lag_4_stock_us_refineries + year_month_decimal),
        ARIMA(price_wti ~ lag_1_price_wti + lag_1_financial_arca + lag_4_stock_us_refineries + year_month_decimal + lag_4_financial_cpi)
    )

predictions_tsbl <- forecast(model_02_fit, testing(train_test_splits)) 

predictions_tsbl %>%
    mutate(.model = as_factor(.model)) %>%
    autoplot(testing(train_test_splits)) +
    facet_wrap(~.model, ncol = 3) +
    scale_color_tq() +
    theme_tq(base_size = 16) +
    theme(legend.direction = "vertical")

# Untransform and calculate test accuracy

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
    group_by(.model) %>%
    summarize(
        mae = mean(abs(price_wti - .pred))
    ) %>%
    mutate(.model = as.character(.model)) %>%
    bind_rows(benchmark_tbl)


