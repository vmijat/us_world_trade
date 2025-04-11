# Trade Gainers

library(tidyverse)
library(ggflags) 
library(countrycode) # to convert country names to country codes
library(tidytext) # for reorder_within
library(scales) # for application of common formats to scale labels (e.g., comma, percent, dollar)



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
            by = c("Period", "Country")) |> 
  mutate(deficit = Import_amount - Export_amount)



# Let's see only for 2024 -------------------------------------------------


trade_with_us_2024 <- 
  trade_with_us |> 
  filter(Period == 2024)


trade_with_us_2024 <- trade_with_us_2024 %>%
  mutate(country_code = tolower(countrycode(Country, 
                                            "country.name", 
                                            "iso2c", 
                                            warn = FALSE)))


trade_with_us_2024 <- 
  trade_with_us_2024 |> 
  mutate(country_code = case_when(
    country_code == "European Union" ~ "eu",
    TRUE ~ country_code
  ))




# Make a graph ------------------------------------------------------------


trade_with_us_2024 |> 
  arrange(desc(deficit)) |> 
  filter(str_length(country_code) == 2) |> 
  slice_head(n = 15) |> 
  
  ggplot(aes(
    x = fct_reorder(country_code, deficit),
    y = deficit,
    fill = deficit
  )) +
  geom_col() +
  geom_flag(
    aes(country = country_code),
    y = 0
  ) +
  labs(y = "", 
       x = "", 
       title = "US Goods Trade Deficit in 2024 Per Country", 
       subtitle = "top 15",
       caption = "Visualization: Vlad Mijatovic") +
  scale_x_discrete() +
  theme_minimal() +
  theme(
    legend.position = 'none'
  ) +
  coord_flip()

