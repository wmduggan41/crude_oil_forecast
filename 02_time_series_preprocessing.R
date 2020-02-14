# LEARNING LAB 29: MULTIVARIATE FORECASTING OF CRUDE OIL PRICES
# SHINY API SERIES: SHINY + QUANDL API
# TIME SERIES PREPROCESSING ----

# LIBRARIES -----

# Preprocessing
library(recipes)

# Time Series
library(lubridate)
library(tsibble)

# EDA
library(DataExplorer)

# Visualization
library(plotly)
library(ggcorrplot)

# Core
library(tidyverse)
library(tidyquant)


# Read Data
us_petroleum_joined_m_tsbl <- read_rds("01_data/us_petroleum_joined_m_tsbl.rds")

us_petroleum_joined_m_tsbl %>% tail() %>% glimpse()


# 6.0 TIME SERIES PREPROCESSING ----

preprocess_01_rec <- recipe(price_wti ~ ., data = us_petroleum_joined_m_tsbl) %>%
    
    # Center & Scale
    step_normalize(all_numeric()) %>%
    
    # Engineered Features
    step_mutate(demand_net_imports = demand_imports - supply_exports) %>%
    step_date(year_month, features = c("decimal")) %>%
    
    # Add Lagged Features
    step_lag(price_wti, lag = 1:12) %>%
    
    step_lag(demand_imports, lag = 4:12) %>%
    step_lag(demand_population, lag = 2:12) %>%
    step_lag(demand_product_supplied, lag = 4:12) %>%
    
    step_lag(supply_exports, lag = 4:12) %>%
    step_lag(supply_field_production, lag = 4:12) %>%
    step_lag(supply_operating_capacity, lag = 4:12) %>%
    
    step_lag(stock_us_total, lag = 4:12) %>%
    step_lag(stock_us_refineries, lag = 4:12) %>%
    step_lag(stock_us_tank_farms, lag = 4:12) %>%
    
    step_lag(financial_arca, lag = 1:12) %>%
    step_lag(financial_sp500, lag = 1:12) %>%
    step_lag(financial_cpi, lag = 4:12) %>%
    
    # Remove Features
    step_rm(demand_imports, demand_population, demand_product_supplied,
            supply_exports, supply_field_production, supply_operating_capacity,
            stock_us_total, stock_us_refineries, stock_us_tank_farms,
            financial_arca, financial_sp500, financial_cpi, 
            demand_net_imports) %>%
    
    # Remove rows with NA's
    step_naomit(all_predictors()) %>%
    prep()


us_petroleum_prep_01_m_tsbl <- bake(preprocess_01_rec, us_petroleum_joined_m_tsbl) %>%
    as_tsibble(index = "year_month") %>%
    select(year_month, price_wti, everything())

us_petroleum_prep_01_m_tsbl

# 7.0 LAG PLOTS ----

col_match <- "price_wti"

lag_plot <- function(data, col_match) {
    
    us_petroleum_prep_01_m_tsbl %>%
        select(price_wti, contains(col_match)) %>%
        as_tibble() %>%
        select(-year_month) %>%
        pivot_longer(-price_wti, names_to = "lag", values_to = col_match, names_repair = "unique") %>%
        mutate(lag = as_factor(lag)) %>%
        
        ggplot(aes_string(names(.)[[3]], names(.)[[1]])) +
        geom_point(alpha = 0.5) +
        facet_wrap(~ lag) +
        labs(x = col_match, y = "price_wti")
}

# WTI Prices - Lag 1
us_petroleum_prep_01_m_tsbl %>% lag_plot("price_wti") + geom_smooth()

# Demand Featues - Possibly Demand Population?
us_petroleum_prep_01_m_tsbl %>% lag_plot("demand") + geom_smooth()

# Supply Features - supply operating capacity?
us_petroleum_prep_01_m_tsbl %>% lag_plot("supply") + geom_smooth()

# Stock Features - Stock US Refineries  
us_petroleum_prep_01_m_tsbl %>% lag_plot("stock") + geom_smooth()

# Financial - ARCA (Oil & Gas Index)
us_petroleum_prep_01_m_tsbl %>% lag_plot("financial") + geom_smooth()


# 8.0 FINAL RECIPE ----
preprocess_02_rec <- recipe(price_wti ~ ., data = us_petroleum_joined_m_tsbl) %>%
    
    # Center & Scale - Not Necessary for fable
    step_normalize(all_numeric()) %>%
    
    # Engineered Features
    step_date(year_month, features = c("decimal")) %>%
    
    # Add Lagged Features
    step_lag(price_wti, lag = 1) %>%
    
    step_lag(demand_population, lag = 2) %>%
    
    # step_lag(supply_operating_capacity, lag = 4) %>%
    
    step_lag(stock_us_total, lag = 4) %>%
    step_lag(stock_us_refineries, lag = 4) %>%
    
    step_lag(financial_arca, lag = 1) %>%
    # step_lag(financial_sp500, lag = 1) %>%
    step_lag(financial_cpi, lag = 4) %>%
    
    # Remove Features
    step_rm(demand_imports, demand_population, demand_product_supplied,
            supply_exports, supply_field_production, supply_operating_capacity,
            stock_us_total, stock_us_refineries, stock_us_tank_farms,
            financial_arca, financial_sp500, financial_cpi) %>%
    
    # Remove rows with NA's
    step_naomit(all_predictors()) %>%
    prep()


us_petroleum_prep_02_m_tsbl <- bake(preprocess_02_rec, us_petroleum_joined_m_tsbl) %>%
    as_tsibble(index = "year_month") %>%
    select(year_month, price_wti, everything())

us_petroleum_prep_02_m_tsbl

g <- us_petroleum_prep_02_m_tsbl %>%
    as_tibble() %>%
    select(-year_month) %>%
    cor() %>%
    ggcorrplot()

ggplotly(g)

# write_rds(preprocess_02_rec, "02_recipes/preprocess_02_rec.rds")
