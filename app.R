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
         year = year(date),
         doy = yday(date)) %>% 
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
                            sidebarPanel(# more options here for modeling portion
                                         selectInput(inputId = "species_modeling",
                                                     label = "Select species:",
                                                     choices = c(unique(trawl$field_id)),
                                                     selected = "pinfish")),
                            mainPanel("Abundance Graph",
                                      plotOutput(outputId = "abundance_plot"),
                                      "Presence/Absence Graph",
                                      plotOutput(outputId = "pres_abs_plot")
                            )))
                          )

# 3. Server

server <- function(input, output) {
  
  ######################################################
  # CATCH EXPLORATION PAGE
  ######################################################
  
  # Selection 1: 
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
  
  ######################################################
  # MODEL PAGE
  ######################################################
  
  model_plot <- reactive({
    
    trawl_model <- trawl %>%
      filter(field_id == input$species_modeling) %>% 
      filter(habitat == "sea grass") # Maybe change later if needed
  
    # Fit the models to the data:
    # Abundance Model
    abun_mod_all <- lm(log(catch_100m + 1) ~ doy + I(doy^2) + I(doy^3), data = trawl_model)
  
    # Abundance Model for only the positive catches
    abun_mod_pos <- lm(log(catch_100m + 1) ~ doy + I(doy^2) + I(doy^3), data = trawl_model %>% 
                        filter(pres_abs == 1))
    
    # Binomial Presence/Absence Model
     pres_abs_blr <- glm(pres_abs ~ doy + I(doy^2), family = "binomial", data = trawl_model)
  
    # Create vector to predict values
    predict_doy <- data.frame(doy = c(min(trawl_model$doy):max(trawl_model$doy)))
  
    # Make & save predictions
    #model_predict <- data.frame() # just to make sure data frame is empty when testing code
      model_predict <- data.frame(cbind(
      predict_doy,
      abun_mod_all_predict = predict(abun_mod_all, predict_doy, se.fit = TRUE),
      abun_mod_pos_predict = predict(abun_mod_pos, predict_doy, se.fit = TRUE),
      pres_abs_blr_predict = predict(pres_abs_blr, newdata = predict_doy, type = "response", se.fit = TRUE)))
  
    # Rename and remove some crazy columns
    model_predict <- model_predict %>% 
      select(-abun_mod_all_predict.residual.scale, 
            -abun_mod_pos_predict.residual.scale, 
            -pres_abs_blr_predict.residual.scale,
            -abun_mod_all_predict.df,
            -abun_mod_pos_predict.df) %>% 
      rename(abun_mod_all_fit = abun_mod_all_predict.fit,
            abun_mod_all_se = abun_mod_all_predict.se.fit,
            abun_mod_pos_fit = abun_mod_pos_predict.fit,
            abun_mod_pos_se = abun_mod_pos_predict.se.fit,
            pres_abs_blr_probability = pres_abs_blr_predict.fit,
             pres_abs_blr_se = pres_abs_blr_predict.se.fit)
  
  # Adust range of predictions for when the species is present for the present data - convert others to NA - we don't care what the model predicts outside of the range that went into making the model - we might need to do this in the actual app because of how the model is calculated, but maybe not... don't need for pinfish because is almost always present
    present <- trawl_model %>% 
      filter(pres_abs == 1)
  
    range_adj <- c(min(present$doy),max(present$doy))
  
    model_predict$abun_mod_pos_fit[model_predict$doy < range_adj[1]] <- NA
    model_predict$abun_mod_pos_fit[model_predict$doy > range_adj[2]] <- NA
    model_predict$abun_mod_pos_se[model_predict$doy < range_adj[1]] <- NA
    model_predict$abun_mod_pos_se[model_predict$doy > range_adj[2]] <- NA
  
    ggplot(data = model_predict) +
      geom_point(data = trawl_model, aes(x = doy,y = log(catch_100m + 1))) +
      geom_line(aes(x = doy,y = abun_mod_all_fit)) +
      geom_ribbon(aes(x = doy, 
                     ymin = abun_mod_all_fit - abun_mod_all_se,
                      ymax = abun_mod_all_fit + abun_mod_all_se), alpha = 0.3) +
     geom_line(aes(x = doy, y = abun_mod_pos_fit), color = "red") +
      geom_ribbon(aes(x = doy, 
                     ymin = abun_mod_pos_fit - abun_mod_pos_se, 
                      ymax = abun_mod_pos_fit + abun_mod_pos_se), 
                  alpha = 0.3, fill = "red")
  })
  
  pres_plot <- reactive({
    
    trawl_model <- trawl %>%
      filter(field_id == input$species_modeling) %>% 
      filter(habitat == "sea grass") # Maybe change later if needed
    
    # Fit the models to the data:
    # Abundance Model
    abun_mod_all <- lm(log(catch_100m + 1) ~ doy + I(doy^2) + I(doy^3), data = trawl_model)
    
    # Abundance Model for only the positive catches
    abun_mod_pos <- lm(log(catch_100m + 1) ~ doy + I(doy^2) + I(doy^3), data = trawl_model %>% 
                         filter(pres_abs == 1))
    
    # Binomial Presence/Absence Model
    pres_abs_blr <- glm(pres_abs ~ doy + I(doy^2), family = "binomial", data = trawl_model)
    
    # Create vector to predict values
    predict_doy <- data.frame(doy = c(min(trawl_model$doy):max(trawl_model$doy)))
    
    # Make & save predictions
    #model_predict <- data.frame() # just to make sure data frame is empty when testing code
    model_predict <- data.frame(cbind(
      predict_doy,
      abun_mod_all_predict = predict(abun_mod_all, predict_doy, se.fit = TRUE),
      abun_mod_pos_predict = predict(abun_mod_pos, predict_doy, se.fit = TRUE),
      pres_abs_blr_predict = predict(pres_abs_blr, newdata = predict_doy, type = "response", se.fit = TRUE)))
    
    # Rename and remove some crazy columns
    model_predict <- model_predict %>% 
      select(-abun_mod_all_predict.residual.scale, 
             -abun_mod_pos_predict.residual.scale, 
             -pres_abs_blr_predict.residual.scale,
             -abun_mod_all_predict.df,
             -abun_mod_pos_predict.df) %>% 
      rename(abun_mod_all_fit = abun_mod_all_predict.fit,
             abun_mod_all_se = abun_mod_all_predict.se.fit,
             abun_mod_pos_fit = abun_mod_pos_predict.fit,
             abun_mod_pos_se = abun_mod_pos_predict.se.fit,
             pres_abs_blr_probability = pres_abs_blr_predict.fit,
             pres_abs_blr_se = pres_abs_blr_predict.se.fit)
    
    # Adust range of predictions for when the species is present for the present data - convert others to NA - we don't care what the model predicts outside of the range that went into making the model - we might need to do this in the actual app because of how the model is calculated, but maybe not... don't need for pinfish because is almost always present
    present <- trawl_model %>% 
      filter(pres_abs == 1)
    
    range_adj <- c(min(present$doy),max(present$doy))
    
    model_predict$abun_mod_pos_fit[model_predict$doy < range_adj[1]] <- NA
    model_predict$abun_mod_pos_fit[model_predict$doy > range_adj[2]] <- NA
    model_predict$abun_mod_pos_se[model_predict$doy < range_adj[1]] <- NA
    model_predict$abun_mod_pos_se[model_predict$doy > range_adj[2]] <- NA
    
    ggplot(data = model_predict) +
      geom_point(data = trawl_model, aes(x = doy, y = pres_abs)) +
      geom_line(aes(x = doy,y = pres_abs_blr_probability)) +
      geom_ribbon(aes(x = doy, 
                      ymin = pres_abs_blr_probability - pres_abs_blr_se,
                      ymax = pres_abs_blr_probability + pres_abs_blr_se), alpha = 0.3)
    
  })
  
  # Make plots with the normal and predicted curves
  # Abundance plot
  output$abundance_plot <- renderPlot({
    model_plot()
  })
  
  # presence absence plot
  output$pres_abs_plot <- renderPlot({
    pres_plot()
  })
  
  
  
  
  
  
}


# 4. Create App
shinyApp(ui = ui, server = server)
