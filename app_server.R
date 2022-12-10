# SERVER
library(tidyverse)
library(shiny)
library(plotly)
library(scales)

carbon_emissions <- read_csv("data/owid-co2-data.csv")

# Create and select columns to be plotted
world_emissions <- carbon_emissions %>%
  filter(country == "World" & year >= 1950) %>%
  select(year,
         co2,
         cement_co2,
         coal_co2,
         oil_co2,
         gas_co2,
         flaring_co2,
         other_industry_co2) %>%
  mutate(Proportion.of.Cement.CO2 = cement_co2 / co2,
         Proportion.of.Coal.CO2 = coal_co2 / co2,
         Proportion.of.Oil.CO2 = oil_co2 / co2,
         Proportion.of.Gas.CO2 = gas_co2 / co2,
         Proportion.of.Flaring.CO2 = flaring_co2 / co2,
         Proportion.of.Other.CO2 = other_industry_co2 / co2) %>%
  select(year,
         CO2.Emissions=co2,
         Proportion.of.Cement.CO2,
         Proportion.of.Coal.CO2,
         Proportion.of.Oil.CO2,
         Proportion.of.Gas.CO2,
         Proportion.of.Flaring.CO2,
         Proportion.of.Other.CO2)

# Automatically generated values
avg_co2_2021 <- carbon_emissions %>% # use this
  filter(year == 2021) %>%
  summarise(avg = mean(co2, na.rm = T)) %>%
  pull()

# Find country with highest CO2 emissions
highest_co2 <- carbon_emissions %>%
  filter(!(country %in% c("World",
                          "Non-OECD (GCP)",
                          "Asia",
                          "Asia (GCP)",
                          "Upper-middle-income countries",
                          "High-income countries",
                          "OECD (GCP)"))) %>%
  filter(year == 2021) %>%
  filter(co2 == max(co2, na.rm = T))

highest_country <- highest_co2 %>% pull(country) # use this
highest_amount <- highest_co2 %>% pull(co2) # use this

change_df <- carbon_emissions %>%
  filter(country == "World" & year %in% c(2001, 2021))

change_co2 = change_df$co2[2] - change_df$co2[1] # use this

# Introduction
intro_md <- 
paste0(
" 
### Introduction
> Carbon emissions are at an all time high

Global warming is caused by an increase in greenhouse gases in the Earth's 
atmosphere. These gases, primarily carbon dioxide, trap heat from the sun and 
prevent it from escaping back into space, causing the Earth's temperature to rise.
Through this application, I track the history and sources of carbon dioxide emissions in our world to 
determine how our carbon footprint has changed over time.

### The Data
I use the CO2 and Greenhouse Gas Emissions dataset from _Our World in Data_ and
analyze the following variables:

- `co2`: Annual total production-based emissions of carbon dioxide, excluding 
land-use change, measured in million tonnes.
- `Proportion.of.Cement.CO2`: Annual production-based emissions of carbon 
dioxide from cement, measured as a proportion of global production-based emissions 
of carbon dioxide. Created using the original variables `co2` and `cement_co2`.
- `Proportion.of.Coal.CO2`: Annual production-based emissions of carbon 
dioxide from coal, measured as a proportion of global production-based emissions 
of carbon dioxide. Created using the original variables `co2` and `coal_co2`.
- `Proportion.of.Oil.CO2`: Annual production-based emissions of carbon 
dioxide from oil, measured as a proportion of global production-based emissions 
of carbon dioxide. Created using the original variables `co2` and `oil_co2`.
- `Proportion.of.Gas.CO2`: Annual production-based emissions of carbon 
dioxide from gas, measured as a proportion of global production-based emissions 
of carbon dioxide. Created using the original variables `co2` and `gas_co2`.
- `Proportion.of.Flaring.CO2`: Annual production-based emissions of carbon 
dioxide from flaring, measured as a proportion of global production-based emissions 
of carbon dioxide. Created using the original variables `co2` and `flaring_co2`.
- `Proportion.of.Other.CO2`: Annual production-based emissions of carbon 
dioxide from other industry sources, measured as a proportion of global production-based emissions 
of carbon dioxide. Created using the original variables `co2` and `other_industry_co2`.

### Values
To provide some perspective, the average carbon emission across all countries in
2021 was about ", as.character(round(avg_co2_2021, 0))," million tonnes. The country with the greatest amount
of carbon emissions in 2021 was ", highest_country," with about ", as.character(prettyNum(round(highest_amount, 0), big.mark=",")),
" million tonnes worth of emissions. For comparison, during the 20 years between 2001 and 
2021, the world's carbon emissions increased by about ", as.character(prettyNum(round(change_co2, 0), big.mark=","))," million tonnes.
These values convey the dire state of the world in terms of carbon emissions.")

server <- function(input, output) {
  # create interactive plot
  output$chart <- renderPlotly({
    
    plot_df <- world_emissions %>%
      filter(year >= input$plot_range[1],
             year <= input$plot_range[2])
    
    p <- ggplot(data = plot_df, mapping = aes(x = year, 
                                              y = !!as.symbol(input$plot_var))) +
      geom_point() +
      geom_line() +
      #scale_y_continuous(labels = "comma") +
      labs(title = paste0("World ", 
                          str_replace_all(input$plot_var, "\\."," "), 
                          " (", 
                          as.character(input$plot_range[1]),
                          "-", 
                          as.character(input$plot_range[2]),
                          ")"),
           x = "Year",
           y = case_when(input$plot_var == "CO2.Emissions" ~ paste0(str_replace_all(input$plot_var, "\\."," "), 
                                                                  " (million tonnes)"),
                         T ~ str_replace_all(input$plot_var, "\\."," ")))
    
    pp <- ggplotly(p)
    return(pp)
  })
  
  # create introduction page
  output$intro <- renderUI({
    HTML(markdown::markdownToHTML(text = intro_md, fragment.only = TRUE))
  })
}