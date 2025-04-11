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


p <- trade_with_us_2024 |> 
  arrange(desc(deficit)) |> 
  filter(str_length(country_code) == 2) |> 
  slice_head(n = 15) |> 
  mutate(deficit = round(deficit / 1000, 0)) |> 
  
  ggplot(aes(
    x = fct_reorder(country_code, deficit),
    y = deficit,
    fill = deficit
  ),
  color = "white") +
  geom_col(
    width = .98
  ) +
  geom_flag(
    aes(country = country_code),
    y = 0, 
    size = 7
  ) +
  geom_label(
    aes(label = 
          paste(
            scales::dollar(deficit), 
            "B")),  # Format as currency
    hjust =  0.2  # Position labels above the bars
  ) +
  labs(y = "", 
       x = "", 
       title = "2024 US Goods Trade Deficit Per Country", 
       subtitle = "top 15",
       caption = "Data from bea.gov | Visualization: Vlad Mijatović") +
  theme_minimal() +
  scale_fill_gradient(high = "red", low = "darkorchid") + 
  theme(
    legend.position = 'none',
    plot.title.position = "plot",
    plot.title = element_text(size = 20),
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 350)) +
  coord_flip()

p

# Highlight Switzerland ---------------------------------------------------


trade_with_us_2024 |> 
  arrange(desc(deficit)) |> 
  filter(str_length(country_code) == 2) |> 
  slice_head(n = 15) |> 
  mutate(deficit = round(deficit / 1000, 0),
         highlight = country_code == "ch") |> 
  
  ggplot(aes(
    x = fct_reorder(country_code, deficit),
    y = deficit,
    fill = highlight
  ),
  color = "white") +
  geom_col(
    width = .98,
    alpha = 0.7
  ) +
  geom_flag(
    aes(country = country_code),
    y = 0, 
    size = 7
  ) +
  geom_label(
    aes(label = 
          paste(
            scales::dollar(deficit), 
            "B"),
        fill = highlight,
        alpha = if_else(highlight, 1, 0.2) ),  # Format as currency
    hjust =  0.2,
    fill = "white"# Position labels above the bars
  ) +
  labs(y = "", 
       x = "", 
       title = "2024 US Goods Trade Deficit Per Country", 
       subtitle = "Switzerland: 38 B",
       caption = "Data from bea.gov | Visualization: Vlad Mijatović") +
  theme_minimal() +
  scale_fill_manual(values = c(
    "grey70",
    "darkorchid")) + 
  theme(
    legend.position = 'none',
    plot.title.position = "plot",
    plot.title = element_text(size = 20),
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 350)) +
  coord_flip()




# Animate -----------------------------------------------------------------

library(gganimate)


# Animate with transition_reveal()
animated_plot_with_pause <- p +
  transition_time(deficit) 



animate(animated_plot_with_pause, 
        duration = 9, 
        fps = 20, 
        width = 700, 
        height = 500, 
        end_pause = 70,
        renderer = gifski_renderer("trade_deficit_animation.gif"))


