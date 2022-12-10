# UI
library(shiny)
library(shinythemes)
library(plotly)

### HOME PAGE
home_page <- tabPanel(
  title = "Introduction",
  titlePanel("World Carbon Emissions"),
  p("by: David Li"),
  fluidPage(
    img("", src="https://climatekids.nasa.gov/resources/icons/greenhouse-effect.png"),
    p(uiOutput("intro"))
  )
)

### INTERACTIVE PAGE
interactive_sidebar_content <- sidebarPanel(selectInput(inputId = "plot_var",
                                                        label = "Choose a quantity",
                                                        choices = list("Total emissions" = "CO2.Emissions",
                                                                       "Proportion of cement emissions" = "Proportion.of.Cement.CO2",
                                                                       "Proportion of coal emissions" = "Proportion.of.Coal.CO2",
                                                                       "Proportion of oil emissions" = "Proportion.of.Oil.CO2",
                                                                       "Proportion of gas emissions" = "Proportion.of.Gas.CO2",
                                                                       "Proportion of flaring emissions" = "Proportion.of.Flaring.CO2",
                                                                       "Proportion of other emissions" = "Proportion.of.Other.CO2")),
                                            sliderInput(inputId = "plot_range",
                                                        label = "Choose a timeframe",
                                                        min = 1950,
                                                        max = 2021,
                                                        value = c(2001,2021)))

interactive_main_content <- mainPanel(plotlyOutput("chart"))

interactive_page <- tabPanel(
  title = "Interactive",
  titlePanel("Visualization of World CO2 Data"),
  sidebarLayout(
    interactive_sidebar_content,
    interactive_main_content
  ),
  p("There are several things that can be learned from this visualization: the
    first thing being that the total CO2 emission of the world has steadily increased
    between the years 1950 and 2021, with very few decreases in emissions between
    subsequent years. Another significant observation is that, from 1950, coal was
    responsible for over half of the world's carbon emissions, until 1963. This occured
    as part of an overall decrease in the use of coal, as the proportion of carbon emissions
    from coal hit an all-time low of about 0.342 in 1978. However, since then, it has 
    increased, reflecting the growth of the world population, and in effect, our energy 
    needs. Also, it is interesting to see that, as the proportion of carbon emissions from
    coal decreased every year between 1950 and 1973, the proportion of emissions from oil
    increased, peaking at about 0.481 in 1973. To speculate, this may reflect an increase
    in the demand for oil as an alternative to coal for supplying energy to our automobiles, homes,
    and businesses."),
  p("Today, coal, oil, and natural gas account for over 90% of the world's carbon emissions, with
    coal being the biggest contributor. While the proportion of emissions due to coal has significantly
    decreased since 1950, that of oil and natural gas have increased. Thus, to remedy the greenhouse effect
    caused by carbon emissions from fossil fuels, we must look towards renewable/clean energy sources—a
    promising option being solar energy.")
)

ui <- navbarPage("CO2 Emissions",
                 home_page,
                 interactive_page,
                 theme = shinytheme("flatly"))