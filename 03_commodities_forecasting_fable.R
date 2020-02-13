# LEARNING LAB 29: MULTIVARIATE FORECASTING OF CRUDE OIL PRICES
# SHINY API SERIES: SHINY + QUANDL API
# MULTIVARIATE TIME SERIES FORECASTING ----

# LIBRARIES -----

# Time Series
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

# EDA
library(DataExplorer)

# Modeling
library(recipes)
library(rsample)
library(yardstick)

# Core
library(tidyverse)
library(tidyquant)


# Read Data
us_petroleum_joined_m_tsbl <- read_rds("01_data/us_petroleum_joined_m_tsbl.rds")

# Apply Recipe
preprocess_02_rec <- read_rds("02_recipes/preprocess_02_rec.rds")

us_petroleum_prep_02_m_tsbl <- bake(preprocess_02_rec, us_petroleum_joined_m_tsbl) %>%
    as_tsibble(index = "year_month")

us_petroleum_prep_02_m_tsbl %>% glimpse()

# 9.0 ARIMA MODEL ----
train_test_splits <- initial_time_split(us_petroleum_prep_02_m_tsbl, prop = 0.8)

# UNIVARIATE FORECAST ----
model_01_fit <- training(train_test_splits) %>%
    model(
        ARIMA(price_wti)
    )

testing(train_test_splits)

model_01_fit %>% forecast(h = 1)

model_01_fit %>% forecast(h = 10)

# MULTIVARIATE FORECAST ----
model_02_fit <- training(train_test_splits) %>%
    model(
        ARIMA(price_wti),
        ARIMA(price_wti ~ lag_1_financial_arca),
        ARIMA(price_wti ~ lag_1_price_wti + lag_1_financial_arca)
        # ,
        # ARIMA(price_wti ~ lag_1_price_wti + lag_1_financial_arca + lag_4_stock_us_refineries),
        # ARIMA(price_wti ~ lag_1_price_wti + lag_1_financial_arca + lag_4_stock_us_refineries + year_month_decimal),
        # ARIMA(price_wti ~ lag_1_price_wti + lag_1_financial_arca + lag_4_stock_us_refineries + year_month_decimal + lag_4_financial_cpi)
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

preprocess_02_rec %>% tidy()

preprocess_02_rec %>% tidy(1) %>% filter(terms == "price_wti")

predictions_tsbl %>%
    as_tibble() %>%
    select(.model, year_month, price_wti) %>%
    rename(.pred = price_wti) %>%
    mutate(.model = as_factor(.model)) %>%
    
    # Reverse Normalize Transformation
    mutate(.pred = (.pred * 26.3) + 61.9) %>%
    
    # Calculate MAE
    left_join(
        us_petroleum_joined_m_tsbl %>%
            as_tibble() %>%
            select(year_month, price_wti)
    ) %>%
    group_by(.model) %>%
    summarize(
        mae = mean(abs(price_wti - .pred))
    )


