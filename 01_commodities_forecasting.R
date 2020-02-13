# LEARNING LAB 29: MULTIVARIATE FORECASTING OF CRUDE OIL PRICES
# SHINY API SERIES: SHINY + QUANDL API
# - Paper Reference: https://arxiv.org/pdf/1811.08963.pdf

# 1.0 LIBRARIES -----

# API
library(Quandl)
library(config)

# Time Series
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

# EDA
library(DataExplorer)

# Visualization
library(plotly)
library(ggcorrplot)

# Modeling
library(recipes)
library(parsnip)
library(rsample)
library(tune)
library(dials)
library(yardstick)
library(keras)

# Core
library(tidyverse)
library(tidyquant)


# 2.0 QUANDL API SETUP ----
# - GET API KEY: https://www.quandl.com/

# - Set API Key
# Quandl.api_key("your_api_key")
config::get("quandl", file = "../config.yml") %>% Quandl.api_key()

# 3.0 QUANDL API USAGE ----
# - QUANDL API DOCUMENTATION: https://www.quandl.com/data/EIA-U-S-Energy-Information-Administration-Data/documentation
# - FORMAT: EIA/{COMPONENT}_{INDICATOR}_{FREQUENCY}

eia_code <- 'EIA/AEO_2016_REF_NO_CPP_PRCE_NA_COMM_NA_NG_NA_SATL_Y13DLRPMCF_A'

Quandl(
    code       = eia_code, 
    start_date = '2020-12-31', 
    end_date   = '2040-12-31', 
    type       = "raw", 
    order      = "asc",
    meta       = TRUE) 


# 4.0 US CRUDE OIL DATA ----

# Helper Functions ----
quandl_download <- function(code, 
                            value_name  = "Value", 
                            start_date  = "2000-01-01", 
                            end_date    = "2020-02-29",
                            order       = "asc",
                            summary_fun = median) {
    
    Quandl(code, start_date = start_date, end_date = end_date, order = "asc") %>%
        as_tsibble(index = "Date") %>%
        index_by(year_month = yearmonth(Date)) %>%
        summarise(!! value_name := summary_fun(Value)) 
    
}

plot_series <- function(data) {
    data %>%
        ggplot(aes_string(names(data)[[1]], names(data)[[2]])) +
        scale_y_continuous(labels = scales::comma_format()) +
        geom_line() 
}

# 4.1 CRUDE OIL PRICES (WTI) ----
wti_spot_prices_tsbl <- quandl_download("EIA/PET_RWTC_D", value_name = "price_wti", summary_fun = first)
wti_spot_prices_tsbl

wti_spot_prices_tsbl %>% plot_series() + labs(title = "WTI Spot Prices (USD)")


# 4.2 CRUDE DEMAND ----

# 4.2.1 US Crude Imports, Source: EIA PET.MTTIMUS1.M
demand_us_imports_tsbl <- quandl_download("EIA/PET_MTTIMUS1_M", value_name = "demand_imports")

demand_us_imports_tsbl %>% 
    plot_series() + 
    labs(title = "US Imports (Thousands of Barrels)")

# 4.2.2 US Population, Source: FRED CNP16OV
demand_us_population_tsbl <- quandl_download("FRED/CNP16OV", value_name = "demand_population")

demand_us_population_tsbl %>% plot_series() + labs(title = "US Population")

# 4.2.3 US Total Crude Oil and Petroleum Product Supplied, Source: EIA PET.MTTUPUS2.M
demand_us_product_supplied_tsbl <- quandl_download("EIA/PET_MTTUPUS2_M", value_name = "demand_product_supplied") 

demand_us_product_supplied_tsbl %>% plot_series() + labs(title = "US Product Supplied")


# 4.3 CRUDE SUPPLY ----

# 4.3.1 US Crude Field Production, Source: EIA PET.MCRFPUS2.M
supply_us_crude_production_tsbl <- quandl_download("EIA/PET_MCRFPUS2_M", value_name = "supply_field_production") 

supply_us_crude_production_tsbl %>% plot_series() + labs(title = "US Crude Production")

# 4.3.2 US Crude Exports, Source: EIA PET.MTTEXUS2.M
supply_us_crude_exports_tsbl <- quandl_download("EIA/PET_MTTEXUS2_M", value_name = "supply_exports") 
    
supply_us_crude_exports_tsbl %>% plot_series() + labs(title = "US Crude Exports")

# 4.3.3 Operable Capacity, Source: EIA PET.MOCGGUS2.M
supply_us_operating_capacity_tsbl <- quandl_download("EIA/PET_MOCGGUS2_M", value_name = "supply_operating_capacity") 

supply_us_operating_capacity_tsbl %>% plot_series() + labs(title = "US Refinery Operating Capacity")

# 4.4 CRUDE STOCK ----

# 4.4.1 US Total Crude Stocks, Source EIA PET.MTTSTUS1.M 
stock_us_crude_stocks_tsbl <- quandl_download("EIA/PET_MTTSTUS1_M", value_name = "stock_us_total") 
    
stock_us_crude_stocks_tsbl %>% plot_series() + labs(title = "US Total Crude Oil Stock (Thousands of Barrels)")

# 4.4.2 Stock at Refineries, Source EIA PET.MCRRSUS1.M
stock_us_refineries_tsbl <- quandl_download("EIA/PET_MCRRSUS1_M", value_name = "stock_us_refineries") 
    
stock_us_refineries_tsbl %>% plot_series() + labs(title = "US Refinery Oil Stock (Thousands of Barrels)")

# 4.4.3 Stock at Tank Farms, Source: EIA PET.MCRSFUS1.M
stock_us_tank_farms_tsbl <- quandl_download("EIA/PET_MCRSFUS1_M", value_name = "stock_us_tank_farms")

stock_us_tank_farms_tsbl %>% plot_series() + labs(title = "US Tank Farm Oil Stock  (Thousands of Barrels)")


# 4.5 FINANCIAL MARKETS -----

# 4.5.1 NYSE ARCA Oil & Gas Index, Source: Yahoo Finance ^XOI
financial_arca_tsbl <- tq_get("^XOI", from = "2000-01-01") %>%
    as_tsibble(index = "date") %>%
    select(date, adjusted) %>%
    index_by(year_month = yearmonth(date)) %>%
    summarise(Value = first(adjusted)) %>%
    rename(financial_arca = Value)

financial_arca_tsbl %>% plot_series() + labs(title = "ARCA Oil & Gas Index")

# 4.5.2 S&P 500 Index, Source: Yahoo Finance ^GSPC
financial_sp_500_tsbl <- tq_get("^GSPC", from = "2000-01-01") %>%
    as_tsibble(index = "date") %>%
    select(date, adjusted) %>%
    index_by(year_month = yearmonth(date)) %>%
    summarise(Value = first(adjusted)) %>%
    rename(financial_sp500 = Value)

financial_sp_500_tsbl %>% plot_series() + labs(title = "S&P500 Index")

# 4.5.3 CPI, Source: FRED CPIAUCSL
financial_cpi_tsbl <- quandl_download("FRED/CPIAUCSL", value_name = "financial_cpi")
    
financial_cpi_tsbl %>% plot_series() + labs(title = "CPI")

# 5.0 EDA ----

# 5.1 Join data ----

us_petroleum_joined_m_tsbl <- wti_spot_prices_tsbl %>%
    # Demand
    left_join(demand_us_imports_tsbl) %>%
    left_join(demand_us_population_tsbl) %>%
    left_join(demand_us_product_supplied_tsbl) %>%
    # Supply
    left_join(supply_us_crude_exports_tsbl) %>%
    left_join(supply_us_crude_production_tsbl) %>%
    left_join(supply_us_operating_capacity_tsbl) %>%
    # Stock
    left_join(stock_us_crude_stocks_tsbl) %>%
    left_join(stock_us_refineries_tsbl) %>%
    left_join(stock_us_tank_farms_tsbl) %>%
    # Financial
    left_join(financial_arca_tsbl) %>%
    left_join(financial_sp_500_tsbl) %>%
    left_join(financial_cpi_tsbl)

# Visualize
plot_facets <- function(data, scales = "free_y", ncol = 3, ...) {
    data %>%
        pivot_longer(cols = -year_month) %>%
        mutate(name = as_factor(name)) %>%
        
        ggplot(aes(year_month, value, color = name)) +
        geom_line() +
        facet_wrap(~ name, scales = scales, ncol = ncol, ...) +
        scale_color_tq() +
        theme_minimal()
}

us_petroleum_joined_m_tsbl %>% plot_facets()
    

# 5.2 Missing Data ----
us_petroleum_joined_m_tsbl %>% plot_missing()

us_petroleum_joined_m_tsbl %>% tail() %>% View()


# 6.0 TIME SERIES PREPROCESSING -----

us_petroleum_joined_m_tsbl %>% glimpse()

# 6.1 Correlation Analysis Part 1 ----

preprocess_01_rec <- recipe(price_wti ~ ., data = us_petroleum_joined_m_tsbl) %>%
    step_normalize(all_numeric()) %>%
    step_mutate(demand_net_imports = demand_imports - supply_exports) %>%
    step_date(year_month, features = c("month", "decimal")) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    prep()

us_petroleum_prep_01_m_tsbl <- bake(preprocess_01_rec, us_petroleum_joined_m_tsbl) %>%
    as_tsibble(index = "year_month") %>%
    select(year_month, price_wti, everything())

us_petroleum_prep_01_m_tsbl %>% glimpse()

us_petroleum_prep_01_m_tsbl %>% 
    select(-contains("year_month_month"), -year_month_decimal) %>% 
    plot_facets()

g <- us_petroleum_prep_01_m_tsbl %>%
    as_tibble() %>%
    select(-year_month) %>%
    cor(use = "pairwise.complete.obs") %>%
    ggcorrplot()

ggplotly(g)


# 6.2 Correlation Analysis, Part 2 - Add Lagged Features ----

us_petroleum_joined_m_tsbl %>% tail() %>% glimpse()

preprocess_02_rec <- recipe(price_wti ~ ., data = us_petroleum_joined_m_tsbl) %>%
    step_normalize(all_numeric()) %>%
    step_mutate(demand_net_imports = demand_imports - supply_exports) %>%
    step_date(year_month, features = c("decimal")) %>%
    
    # Add Lagged Features
    step_lag(price_wti, lag = 1:12) %>%
    
    # step_lag(demand_imports, lag = 4:12) %>%
    step_lag(demand_population, lag = 2:12) %>%
    step_lag(demand_product_supplied, lag = 4:12) %>%
    
    # step_lag(supply_exports, lag = 4:12) %>%
    # step_lag(supply_field_production, lag = 4:12) %>%
    # step_lag(supply_operating_capacity, lag = 4:12) %>%
    
    # step_lag(stock_us_total, lag = 4:12) %>%
    step_lag(stock_us_refineries, lag = 4:12) %>%
    # step_lag(stock_us_tank_farms, lag = 4:12) %>%
    
    step_lag(financial_arca, lag = 1:12) %>%
    # step_lag(financial_sp500, lag = 1:12) %>%
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

us_petroleum_prep_02_m_tsbl <- bake(preprocess_02_rec, us_petroleum_joined_m_tsbl) %>%
    as_tsibble(index = "year_month") %>%
    select(year_month, price_wti, everything())

us_petroleum_prep_02_m_tsbl %>% glimpse()

g <- us_petroleum_prep_02_m_tsbl %>%
    as_tibble() %>%
    select(-year_month) %>%
    cor() %>%
    ggcorrplot()

ggplotly(g)

# 7.0 Modeling ----

initial_split_spec <- initial_time_split(us_petroleum_prep_02_m_tsbl, prop = 0.80)


resamples_spec <- rsample::rolling_origin(
    data    = training(initial_split_spec), 
    initial = 12 * 5, 
    assess  = 12, 
    skip    = 12 * 2,
    cumulative = FALSE
)

# GLM ----

glmnet_model_spec <- linear_reg(
    mode = "regression",
    penalty = tune(),
    mixture = tune()
) %>%
    set_engine("glmnet")

glmnet_model_spec

glmnet_params <- parameters(penalty(), mixture())
glmnet_params

set.seed(123)
glmnet_grid_spec <- grid_max_entropy(glmnet_params, size = 20)
glmnet_grid_spec

glmnet_validation_results_tbl <- tune_grid(
    formula   = price_wti ~ . - year_month,
    model     = glmnet_model_spec,
    resamples = resamples_spec,
    grid      = glmnet_grid_spec,
    metrics   = metric_set(mae, rmse, rsq),
    control   = control_grid(verbose = TRUE)
)

glmnet_validation_results_tbl

glmnet_validation_results_tbl %>% 
    show_best("mae", n = 10, maximize = FALSE)

params_glmnet_best <- glmnet_validation_results_tbl %>% 
    select_best("mae", maximize = FALSE)

params_glmnet_best

glmnet_final_model_spec <- glmnet_model_spec %>% 
    finalize_model(parameters = params_glmnet_best)

glmnet_final_model_spec

glmnet_final_model_fit <- glmnet_final_model_spec %>%
    fit(price_wti ~ . - year_month, data = training(initial_split_spec))

preds_untransformed_glmnet_tbl <- predict(glmnet_final_model_fit, new_data = testing(initial_split_spec)) %>%
    bind_cols(testing(initial_split_spec)) %>%
    select(year_month, price_wti, .pred)


preprocess_02_rec %>% tidy()

preprocess_02_rec %>% tidy(1) %>% filter(terms == "price_wti")

preds_transformed_glmnet_tbl <- preds_untransformed_glmnet_tbl %>%
    mutate(
        price_wti = (price_wti * 26.3) + 61.9,
        .pred     = (.pred * 26.3) + 61.9
    )

preds_transformed_glmnet_tbl %>%
    pivot_longer(cols = -year_month) %>%
    ggplot(aes(year_month, value, color = name)) +
    geom_line()

preds_transformed_glmnet_tbl %>%
    mae(price_wti, .pred)


# SVM ----
svm_radial_model_spec <- svm_rbf(
    mode      = "regression", 
    cost      = tune(), 
    rbf_sigma = tune(), 
    margin    = tune()) %>%
    set_engine("kernlab")

svm_radial_model_spec

svm_radial_params <- parameters(cost(), rbf_sigma(), margin())
svm_radial_params

set.seed(123)
svm_radial_grid_spec <- grid_max_entropy(svm_radial_params, size = 20)
svm_radial_grid_spec

svm_radial_validation_results_tbl <- tune_grid(
    formula   = price_wti ~ . - year_month,
    model     = svm_radial_model_spec,
    resamples = resamples_spec,
    grid      = svm_radial_grid_spec,
    metrics   = metric_set(mae, rmse, rsq),
    control   = control_grid(verbose = TRUE)
)

svm_radial_validation_results_tbl %>% show_best("mae", maximize = FALSE)

params_svm_radial_best <- svm_radial_validation_results_tbl %>% 
    select_best("mae", maximize = FALSE)

params_svm_radial_best

svm_radial_final_model_spec <- svm_radial_model_spec %>% 
    finalize_model(parameters = params_svm_radial_best)

svm_radial_final_model_spec


svm_radial_final_model_fit <- svm_radial_final_model_spec %>%
    fit(price_wti ~ . - year_month, data = training(initial_split_spec))

preds_untransformed_svm_radial_tbl <- predict(svm_radial_final_model_fit, new_data = testing(initial_split_spec)) %>%
    bind_cols(testing(initial_split_spec)) %>%
    select(year_month, price_wti, .pred)


price_wti_normalization_tbl <- preprocess_02_rec %>% tidy(1) %>% filter(terms == "price_wti")
price_wti_normalization_vec <- price_wti_normalization_tbl %>% pull(value)
names(price_wti_normalization_vec) <- price_wti_normalization_tbl %>% pull(statistic)
price_wti_normalization_vec

preds_transformed_svm_radial_tbl <- preds_untransformed_svm_radial_tbl %>%
    mutate(
        price_wti = (price_wti * price_wti_normalization_vec[["sd"]]) + price_wti_normalization_vec[["mean"]],
        .pred     = (.pred * price_wti_normalization_vec[["sd"]]) + price_wti_normalization_vec[["mean"]]
    )

preds_transformed_svm_radial_tbl %>%
    pivot_longer(cols = -year_month) %>%
    ggplot(aes(year_month, value, color = name)) +
    geom_line()

preds_transformed_svm_radial_tbl %>%
    mae(price_wti, .pred)

preds_transformed_glmnet_tbl %>%
    left_join(preds_transformed_svm_radial_tbl, by = "year_month") %>%
    mutate(.pred = (.pred.x + .pred.y)/2) %>%
    rename(price_wti = price_wti.x) %>%
    select(year_month, price_wti, .pred) %>%
    pivot_longer(cols = -year_month) %>%
    ggplot(aes(year_month, value, color = name)) +
    geom_line()



    


# Neural Network ----
nnet_model_spec <- mlp(
    hidden_units = tune(), 
    dropout = tune(), 
    epochs  = 300, 
    activation = "relu") %>%
    set_mode("regression") %>%
    set_engine("keras", verbose = 0, validation_split = .20)

nnet_model_spec

nnet_params <- parameters(hidden_units(), dropout())
nnet_params

set.seed(123)
nnet_grid_spec <- grid_max_entropy(nnet_params, size = 20)
nnet_grid_spec

nnet_validation_results_tbl <- tune_grid(
    formula   = price_wti ~ . - year_month,
    model     = nnet_model_spec,
    resamples = resamples_spec,
    grid      = nnet_grid_spec,
    metrics   = metric_set(mae, rmse, rsq),
    control   = control_grid(verbose = TRUE)
)

nnet_validation_results_tbl %>% show_best(metric = "mae", maximize = FALSE)

params_nnet_best <- nnet_validation_results_tbl %>% 
    select_best("mae", maximize = FALSE)

params_nnet_best

nnet_final_model_spec <- nnet_model_spec %>% 
    finalize_model(parameters = params_nnet_best)

nnet_final_model_spec


nnet_final_model_fit <- nnet_final_model_spec %>%
    fit(price_wti ~ . - year_month, data = training(initial_split_spec))

preds_untransformed_nnet_tbl <- predict(nnet_final_model_fit, new_data = testing(initial_split_spec)) %>%
    bind_cols(testing(initial_split_spec)) %>%
    select(year_month, price_wti, .pred)


preprocess_02_rec %>% tidy()

preprocess_02_rec %>% tidy(1) %>% filter(terms == "price_wti")

preds_transformed_nnet_tbl <- preds_untransformed_nnet_tbl %>%
    mutate(
        price_wti = (price_wti * 26.3) + 61.9,
        .pred     = (.pred * 26.3) + 61.9
    )

preds_transformed_nnet_tbl %>%
    pivot_longer(cols = -year_month) %>%
    ggplot(aes(year_month, value, color = name)) +
    geom_line()

preds_transformed_nnet_tbl %>%
    mae(price_wti, .pred)


