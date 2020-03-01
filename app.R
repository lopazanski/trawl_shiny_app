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
library(DT)
library(kableExtra)


# Read in the Data

trawl <- read_csv("trawl_subset.csv") %>% 
  mutate(date = ymd(date),
         month = month(date, label = TRUE),
         year = year(date)) %>% 
  filter(habitat %in% c("sea grass", "salt marsh", "mud flat", "creek"))

trawl$field_id[trawl$field_id == "Eucinostomus"] <- "mojarra"


# 2. User Interface
ui <- navbarPage("Seasonality in NC Estuarine Communities",
                 theme = shinytheme("spacelab"),
                 tabPanel("About",
                          h1("About this App"),
                          p("Some information about the app here...")),
                 tabPanel("Explore the Catch",
                          sidebarLayout(
                            sidebarPanel(# dropdown menu for species
                                         selectInput(inputId = "species",
                                                     label = "Species:",
                                                     choices = c(unique(trawl$field_id)),
                                                     selected = "pinfish"),
                                         #biomass/catch selection
                                         radioButtons("data_type",
                                                      label = "Data Type:",
                                                      choiceNames = c("Abundance", "Biomass"),
                                                      choiceValues = c("catch_100m", "biomass_100m")),
                                         # habitat checkbox
                                         checkboxGroupInput("habitat",
                                                            label = "Habitat(s):",
                                                            choices = c(unique(trawl$habitat)),
                                                            selected = "sea grass"),
                                         # date slider
                                         sliderInput("date",
                                                     label = "Date Range:",
                                                     min = as.Date("2010-07-01"), 
                                                     max = as.Date("2017-11-17"),
                                                     value = c(as.Date("2010-07-01"), as.Date("2017-11-17")),
                                                     timeFormat = "%F")),
                            mainPanel("Catch per unit effort:",
                                      plotOutput(outputId = "cpue_plot"),
                                      "Monthly",
                                      plotOutput(outputId = "cpue_monthly_plot"),
                                      "Data Table",
                                      DT::dataTableOutput(outputId = "table")))),
                 tabPanel("Model",
                          sidebarLayout(
                            sidebarPanel("Choose your options:",
                                         # more options here for modeling portion
                                         selectInput(inputId = "species_modeling",
                                                     label = "Select species:",
                                                     choices = c(unique(trawl$field_id)))),
                            mainPanel("Main Panel Title Here",
                                      # Have outputs for model page here
                            )))
                          )

# 3. Server

server <- function(input, output) {
  # Selection 1: species dropdown
  cpue_select <- reactive({
    trawl %>% 
      filter(field_id == input$species) %>% 
      filter(habitat == input$habitat) %>% 
      filter(between(date, input$date[1], input$date[2])) 
      #select(field_id, habitat, date, input$data_type)
  })
  
  
  # Output 1: cpue_plot
  output$cpue_plot <- renderPlot({
    ggplot(data = cpue_select(), aes(x = date, y = !!as.name(input$data_type))) +
      geom_point(aes(color = year), size = 3)
  })

  
  # Output 2: cpue_monthly_plot
  output$cpue_monthly_plot <- renderPlot({
    ggplot(data = cpue_select(), aes(x = month, y = !!as.name(input$data_type))) +
      geom_point(aes(color = year))
  })
  
  # Output 3: table of values to see what's happening
  output$table <- DT::renderDataTable({data.frame(cpue_select())})
  
}


# 4. Create App
shinyApp(ui = ui, server = server)
