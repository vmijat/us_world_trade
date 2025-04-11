# Trade Gainers

library(tidyverse)


# import data

export_from_us <- readxl::read_xlsx("./data_raw/us_trading_per_country.xlsx",
                                    sheet = "Table 4",
                                    skip = 4) 

import_into_us <- readxl::read_xlsx("./data_raw/us_trading_per_country.xlsx",
                                    sheet = "Table 5",
                                    skip = 4)



# Let's take only annual Exports and imports ------------------------------

export_from_us <- 
  export_from_us |> 
  slice(-1) |> # take out the first row
  filter(Period %in% c(1999:2024)) |>  # keep only annual
  mutate(Period = as.integer(Period)) 

import_into_us <- 
  import_into_us |> 
  slice(-1) |> # take out the first row
  filter(Period %in% c(1999:2024)) |>  # keep only annual
  mutate(Period = as.integer(Period)) 

# make long format

export_from_us_long <- 
  export_from_us |> 
  pivot_longer(cols = -Period,
               names_to = "Country",
               values_to = "Export_amount") 


import_into_us_long <- 
  import_into_us |> 
  pivot_longer(cols = -Period,
               names_to = "Country",
               values_to = "Import_amount") 

# make trade

trade_with_us <- 
  export_from_us_long |> 
  left_join(import_into_us_long,
            by = c("Period", "Country"))


