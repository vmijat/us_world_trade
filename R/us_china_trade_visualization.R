# load libraries

library(tidyverse)
library(readxl)
library(scales)
library(ggimage)


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
  rename("export to China" = "China")

china_import_to_us <- 
  import_into_us |> 
  select("Period","China") |> 
  rename("import from China" = "China")


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
  mutate(Period = as.integer(Period)) |> 
  # divide by 1000 to get to billions
  mutate(across(contains("china", ignore.case = TRUE), ~ . / 1000))



# Visualization -----------------------------------------------------------

# convert to long format

china_trade_long <- 
  china_trade |> 
  pivot_longer(
    cols = contains("china", ignore.case = TRUE),
    names_to = "direction"
  )

china_trade_long |> 
  ggplot(aes(
    x = Period,
    y = value,
    color = direction
  )) +
  geom_line(
    linewidth = 1.5
  ) +
  coord_cartesian(ylim = c(0, 600)) +
  labs(
    title = "US Trade with China",
    subtitle = "(billions of USD)",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  # improve theme
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  # add image
  geom_image(
    data = tibble(
      period = 2020, 
      value  = 200),
    aes(image = "https://substackcdn.com/image/fetch/f_auto,q_auto:good,fl_progressive:steep/https://bucketeer-e05bbc84-baa3-437e-9518-adb32be77984.s3.amazonaws.com/public/images/16628e17-6726-45e3-98c3-f4924d537c14_1000x567.jpeg"),
    size = 0.5
  )

