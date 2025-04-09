# load libraries

library(tidyverse)
library(readxl)


# get data from US Bureau of Economic Analysis (BEA)

url = "https://www.bea.gov/sites/default/files/2025-03/trad-geo-time-series-0125.xlsx"
download.file(url = url, 
              "./data_raw/us_trading_per_country.xlsx",
              mode = "wb")



# get excel data

df <- readxl::read_xlsx("./data_raw/us_trading_per_country.xlsx")


# reading specific tabs
# "Table 4" is export of goods from US
# "Table 5" is import of goods to US

export_from_us <- readxl::read_xlsx("./data_raw/us_trading_per_country.xlsx",
                                    sheet = "Table 4",
                                    skip = 4)

import_into_us <- readxl::read_xlsx("./data_raw/us_trading_per_country.xlsx",
                                    sheet = "Table 5",
                                    skip = 4)




# Extract China -----------------------------------------------------------

china_export_from_us <- 
  export_from_us |> 
  select("Period","China") |> 
  rename("China_import" = "China")

china_import_to_us <- 
  import_into_us |> 
  select("Period","China") |> 
  rename("China_export" = "China")


# join dataframes

china_trade <- 
  china_export_from_us |> 
  left_join(china_import_to_us)



# Let's look at annual trade only -----------------------------------------


china_trade <- 
  china_trade |> 
  slice(-1)

china_trade <- 
  china_trade |> 
  filter(Period %in% c(1999:2024)) |> 
  mutate(Period = as.integer(Period))



# Visualization -----------------------------------------------------------

# convert to long format

china_trade_long <- 
  china_trade |> 
  pivot_longer(
    cols = c("China_import", "China_export"),
    names_to = "direction"
  )

china_trade_long |> 
  ggplot(aes(
    x = Period,
    y = value,
    color = direction
  )) +
  geom_line() +
  coord_cartesian(ylim = c(0, 600000)) +
  labs(
    title = "US Trade with China",
    subtitle = "Billions of USD",
    x = "",
    y = ""
  ) +
  theme_minimal()
