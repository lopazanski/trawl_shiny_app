##################################
# Cori Lopazanski
# Shiny App - NC CRFL Trawl Data
##################################

# 1. Attach packages and read in the data
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(lubridate)


# test

trawl <- read_csv("trawl_tidy.csv") %>% 
  mutate(date = ymd(date),
         month = month(date, label = TRUE),
         year = year(date)) %>% 
  filter(habitat %in% c("sea grass", "salt marsh", "mud flat", "creek"))

trawl$field_id[trawl$field_id == "Eucinostomus"] <- "mojarra"
trawl <- read_csv("trawl_tidy.csv") %>% 
  mutate(date = ymd(date))

# 2. User Interface
ui <- navbarPage("Seasonality in NC Estuarine Communities",
                 theme = shinytheme("spacelab"),
                 tabPanel("About",
                          h1("About this App"),
                          p("Some information about the app here...")),
                 tabPanel("Explore the Catch",
                          sidebarLayout(
                            sidebarPanel("Choose your options:",
                                         # dropdown menu for species
                                         selectInput(inputId = "species",
                                                     label = "Select species:",
                                                     choices = c(unique(trawl$field_id)),
                                                     selected = "pinfish"),
                                                     choices = c(unique(trawl$species_name))),
                                         checkboxGroupInput("habitat",
                                                            label = "Choose habitat(s):",
                                                            choices = c(unique(trawl$habitat)),
                                                            selected = "sea grass"),
                                         sliderInput("date",
                                                     label = "Date Range:",
                                                     min = as.Date("2010-07-01"), 
                                                     max = as.Date("2017-11-17"),
                                                     value = c(as.Date("2010-07-01"), as.Date("2017-11-17")),
                                                     timeFormat = "%F")),
                            mainPanel("Catch per unit effort:",
                                      plotOutput(outputId = "cpue_plot"),
                                      "Monthly",
                                      plotOutput(outputId = "cpue_monthly_plot")),
                            mainPanel("Graph Title Here, Probably?",
                                      plotOutput(outputId = "cpue_plot"))),
                 tabPanel("Model",
                          sidebarLayout(
                            sidebarPanel("Choose your options:",
                                         # more options here for modeling portion
                                         selectInput(inputId = "species_modeling",
                                                     label = "Select species:",
                                                     choices = c(unique(trawl$field_id)))),
                                                     choices = c(unique(trawl$species_name))),
                            mainPanel("Main Panel Title Here",
                                      # Have outputs for model page here
                            )))

# 3. Server

server <- function(input, output) {
  # Selection 1: species dropdown
  cpue_select <- reactive({
    trawl %>% 
      filter(field_id == input$species) %>% 
      filter(species_name == input$species) %>%
      filter(habitat == input$habitat) %>% 
      filter(between(date, input$date[1], input$date[2]))
  })
  
  
  
  # Output 1: cpue_plot
  output$cpue_plot <- renderPlot({
    ggplot(data = cpue_select(), aes(x = date, y = num)) +
      geom_point(aes(color = year), size = 3)
  })

  
  # Output 2: cpue_monthly_plot
  output$cpue_monthly_plot <- renderPlot({
    ggplot(data = cpue_select(), aes(x = month, y = num)) +
      geom_point(aes(color = year))
  })
  
}


# 4. Create App
shinyApp(ui = ui, server = server)
