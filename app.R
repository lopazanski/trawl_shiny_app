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
ui <- navbarPage(title=div(img(src="lab_logo.png", width = 80, height = 80),"Seasonality in NC Estuarine Communities"),
                 tags$head(
                   tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:5px !important; 
                            padding-bottom:10px !important;
                            height: 60px;
                            }
                           .navbar {min-height:60px !important;}')),
                 ),
                 theme = shinytheme("spacelab"),
                 tabPanel("About",
                          tags$head(tags$style( HTML(' .nav {margin-top:30px;}'))),
                          sidebarLayout(
                            sidebarPanel(h4("How to use:"),
                                         br(),
                                         h5("1. Read about the project"),
                                         p("This page contains important information about how and when data was collected."),
                                         br(),
                                         h5("2. Explore the catch"),
                                         p("In this section, explore the data by selecting various characteristics. Choose the species, habitat, and date range for the data, as well as whether you would like the information displayed as biomass (g) or abundance (number of individuals)."),
                                         br(),
                                         h5("3. Model"),
                                         p("Use this section to explore the trend for a given species across certain times in the year.")),
                            mainPanel(img(src = "unc_ims_logo.jpg", height = 70, width = 230),
                                      h3("Summary"),
                                      p("This app allows the user to explore the composition and seasonality of North Carolina estuarine fish and invertebrate communities. It uses graphs, tables, and predictive modeling to illustrate trends in abundance, biomass, and species diversity in different habitats over time."),
                                      br(),
                                      h3("Data"),
                                      p("The data used for this app was collected by the Fisheries Ecology and Oceanography Lab at the University of North Carolina at Chapel Hill’s Institute of Marine Sciences (UNC-IMS) between 2010 and 2018. Funding for the project was provided by the North Carolina Marine Resources Fund (CRFL). Estuarine communities were surveyed in Back and Core Sounds in North Carolina. The area was selected because it represents a range of environmental conditions (wave exposure and fetch direction, salinity, vegetation patchiness, etc.) and contained target sea grass meadows."),
                                      br(),
                                      p("Monthly surveys were conducted during day light hours between May - November annually from 2010-2019. This corresponded with the periods when most winter and spring spawned fishes inhabit shallow water estuarine habitats. Organisms were collected using a 5-m otter trawl with a 4-seam balloon design, with floating and lead lines but without a tickler chain. Trawls were conducted for 2 min at 3-4 km/h, and organisms were counted and each species weighed to the nearest gram. During each trawl, measures of salinity, temperature, habitat type, depth, and lat/long were also recorded.")))),
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
                            mainPanel(
                              h4("Exploring catch per unit effort in estuarine habitats"),
                              p("These graphs illustrate trends in catch per unit effort for the variables selected, where effort is standardized by each 100 meters towed."),
                              plotOutput(outputId = "cpue_plot"),
                              p(strong("Figure 1."), "Catch per unit effort (CPUE) for the selected species, habitat, and date range. Color transition from dark to light indicates earlier to later years."),
                              br(),
                              plotOutput(outputId = "cpue_monthly_plot"),
                              p(strong("Figure 2."), "Monthly changes in catch per unit effort (CPUE) for selected species, habitat, and date range. Darker shades indicate earlier years, and lighter shades indicate more recent years."),
                              plotOutput(outputId = "monthly_averages_plot")))),
                 tabPanel("Model",
                          sidebarLayout(
                            sidebarPanel(# more options here for modeling portion
                                         selectInput(inputId = "species_modeling",
                                                     label = "Select species:",
                                                     choices = c(unique(trawl$field_id)),
                                                     selected = "pinfish")),
                            mainPanel(
                              h4("Modeling seasonal abundance of estuarine organisms in seagrass habitats "),
                              p("These graphs show..."),
                              plotOutput(outputId = "abundance_plot"),
                              p(strong("Figure 1."), "Catch per unit effort of selected species in number of individuals per 100m towed for data collected 2010-2018. Explain models..."),
                              br(),
                              "Presence/Absence Graph",
                              p("This graph shows..."),
                              plotOutput(outputId = "pres_abs_plot"),
                              p(strong("Figure 2."), "Probability of occcurence of selected species during the year for data collected 2010-2018. Presence or absence of selected species is shown by black points. Black line indicates...")
                            ))),
                 tabPanel("Data",
                          "Data Tables",
                          DT::dataTableOutput(outputId = "monthly_table"),
                          DT::dataTableOutput(outputId = "table"))
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
  })
  
  # Selection 2:
  average <- reactive ({
    trawl %>% 
      filter(field_id == input$species) %>% 
      filter(habitat == input$habitat) %>% 
      filter(between(date, input$date[1], input$date[2])) %>% 
      group_by(month) %>% 
      summarize(
        mean = mean(!!as.name(input$data_type), na.rm = TRUE)
      )
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
  
  # Output 3: monthly_averages_plot
  output$monthly_averages_plot <- renderPlot({
    ggplot(data = average(), group = year) +
      geom_point(aes(x = month, y = mean))
  })
  
  # Output 4: other table for testing
  output$monthly_table <- DT::renderDataTable({data.frame(average())})
  
  # Output 5: table of values to see what's happening
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
                  alpha = 0.3, fill = "red") +
      theme_minimal()
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
                      ymax = pres_abs_blr_probability + pres_abs_blr_se), alpha = 0.3) +
      theme_minimal()
    
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
