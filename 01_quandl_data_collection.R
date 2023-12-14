# LEARNING LAB 29: MULTIVARIATE FORECASTING OF CRUDE OIL PRICES
# SHINY API SERIES: SHINY + QUANDL API
# DATA COLLECTION ----
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
# - Will handle these in time series preprocessing (next)
us_petroleum_joined_m_tsbl %>% plot_missing()

us_petroleum_joined_m_tsbl %>% tail() %>% View()

us_petroleum_joined_m_tsbl %>% glimpse()

# write_rds(us_petroleum_joined_m_tsbl, "data/us_petroleum_joined_m_tsbl.rds")
