# load libraries

library(tidyverse)
library(readxl)
library(scales)
library(ggimage)
library(extrafont)


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

export_from_ch <- readxl::read_xlsx("./data_raw/us_trading_per_country.xlsx",
                                    sheet = "Table 4",
                                    skip = 4)

import_into_ch <- readxl::read_xlsx("./data_raw/us_trading_per_country.xlsx",
                                    sheet = "Table 5",
                                    skip = 4)



# Colors ------------------------------------------------------------------


colors_manual <- c(
  "#072ac8",
  "#c81d25"
)

# fonts

font_import()






# Extract China -----------------------------------------------------------

switzerland_export_from_us <- 
  export_from_ch |> 
  select("Period","Switzerland") |> 
  rename("export to Switzerland" = "Switzerland")

switzerland_import_to_us <- 
  import_into_ch |> 
  select("Period","Switzerland") |> 
  rename("import from Switzerland" = "Switzerland")


# join dataframes

switzerland_trade <- 
  switzerland_export_from_us |> 
  left_join(switzerland_import_to_us)



# Let's look at annual trade only -----------------------------------------


switzerland_trade <- 
  switzerland_trade |> 
  slice(-1)

switzerland_trade <- 
  switzerland_trade |> 
  filter(Period %in% c(1999:2024)) |> 
  mutate(Period = as.integer(Period)) |> 
  # divide by 1000 to get to billions
  mutate(across(contains("switzerland", ignore.case = TRUE), ~ round(. / 1000, 1)))



# Visualization -----------------------------------------------------------

# convert to long format

switzerland_trade_long <- 
  switzerland_trade |> 
  pivot_longer(
    cols = contains("switzerland", ignore.case = TRUE),
    names_to = "direction"
  )

p <- switzerland_trade_long |> 
  ggplot(aes(
    x = Period,
    y = value,
    color = direction
  )) +
  geom_line(
    linewidth = 1.5
  ) +
  geom_point(
    data = switzerland_trade_long |>
      filter(Period == max(switzerland_trade_long$Period)),
    aes(
      x = Period,
      y = value 
    ),
    size = 4 
  ) +
  geom_point(
    data = switzerland_trade_long |>
      filter(Period == max(switzerland_trade_long$Period)),
    aes(
      x = Period,
      y = value 
    ),
    size = 2.5,
    color = "white" 
  ) +
  geom_text(
    data = switzerland_trade_long |>
      filter(Period == max(switzerland_trade_long$Period)),
    aes(
      x = Period,
      y = value,
      label = value,
      color = direction
    ),
    hjust = 0.6,
    vjust = - 1.2,
    size = 4
  ) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(
    title = "US Goods Trade Deficit with Switzerland",
    subtitle = "(billions of USD, per year)",
    caption = "Data from bea.gov | Visual: Vlad Mijatović",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  # improve theme
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", size = 21, hjust = 0.5) ,
    plot.subtitle = element_text(face = "italic", size = 18, hjust = 0.5)
  ) +
  guides(color = guide_legend(override.aes = list(shape = NA))) +
  scale_color_manual(values = colors_manual) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) # +

# add image
# geom_image(
#   data = tibble(
#     Period = 2020,
#     value  = 400,
#     direction = "x"),
#   aes(image = "./images/chem2.bmp"),
#   size = 0.5
#   
# )

p


# Animate -----------------------------------------------------------------


library(gganimate)


# Animate with transition_reveal()
animated_plot_with_pause <- p +
  transition_reveal(Period) 



animate(animated_plot_with_pause, 
        duration = 14, 
        fps = 25, 
        width = 700, 
        height = 500, 
        end_pause = 70,
        renderer = gifski_renderer("multi_line_animation_cj.gif"))
