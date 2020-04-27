# shiny/app.R
# The purpose of this app is to facilitate the data exploration process
# There are a lot of panels that need to be looked at and it's easier to
# do so via a reactive environment

# Libraries ---------------------------------------------------------------

# library(tidyverse) # Change this to only the required dependencies for easier publishing
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(correlation)
library(dplyr)
library(ggplot2)
library(ggraph)
# setwd("shiny")


# Data --------------------------------------------------------------------

ALL_cor <- readRDS("ALL_cor.Rda")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # dashboardHeader(title = "Correlation between SST and other variable anomalies during MHWs"),
    dashboardHeader(title = "MHW flux"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Figures", tabName = "figures", icon = icon("chart-line")),
            menuItem("About", tabName = "about", icon = icon("question"))
    ),
    selectInput(inputId = "vars", label = "Variables:",
                choices = unique(ALL_cor$Parameter2), multiple = TRUE,
                selected = c("msnlwrf_mld", "mslhf_mld", 
                             "msshf_mld", "msnswrf_mld", "qnet_mld")),
    selectInput(inputId = "regions", label = "Regions:",
                choices = unique(ALL_cor$region), multiple = TRUE,
                selected = unique(ALL_cor$region)),
    selectInput(inputId = "seasons", label = "Seasons:",
                choices = levels(ALL_cor$season), multiple = TRUE,
                selected = levels(ALL_cor$season)),
    selectInput(inputId = "ts", label = "Time series portion:",
                choices = levels(ALL_cor$ts), multiple = TRUE,
                selected = levels(ALL_cor$ts)),
    # radioButtons(inputId = "figure", label = "Figure:", 
    #              choices = c("histogram", "box_plot", "linear_model"),
                 # selected = "histogram", inline = T),
    radioButtons(inputId = "fill", label = "Grouping:", 
                 choices = c("region", "season", "none"),
                 selected = "none", inline = T),
    sliderInput(inputId = "duration", label = "Min Duraion:",
                min = 1, max = max(ALL_cor$n_Obs),
                value = c(1, max(ALL_cor$n_Obs))),
    sliderInput(inputId = "p_val", label = "Max p:",
                min = 0, max = 1, value = 1)),
    dashboardBody(
        tabItems(
            tabItem(tabName = "figures", 
                    fluidRow(
                        box(width = 6, title = "Histogram", status = "primary", 
                            solidHeader = TRUE, collapsible = TRUE,
                            dropdownButton(
                                h4("Histogram controls:"),
                                radioButtons(inputId = "position", label = "Position:", 
                                             choices = c("stack", "dodge"),
                                             selected = "stack", inline = T),
                                sliderInput(inputId = "bins", label = "Number of bins:",
                                            min = 1, max = 20, value = 10),
                                circle = TRUE, status = "danger", icon = icon("gear")),
                            plotOutput("histPlot")),
                        box(plotOutput("boxPlot"), width = 6, title = "Boxplot", status = "primary", 
                            solidHeader = TRUE, collapsible = TRUE)),
                    fluidRow(box(plotOutput("linePlot"), width = 12, title = "Lineplot", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE))),
            tabItem(tabName = "about", 
                    h2("This is where the functioning of the app will be explained..."))
        )
    )
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Filter data
    filtered_data <- reactive({
        req(input$vars)
        ALL_cor_sub <- ALL_cor %>% 
            filter(Parameter1 == "temp",
                   Parameter2 %in% input$vars,
                   region %in% input$regions,
                   ts %in% input$ts,
                   season %in% input$seasons,
                   p <= input$p_val,
                   n_Obs >= input$duration[1],
                   n_Obs <= input$duration[2]) %>%
            na.omit()
        return(ALL_cor_sub)
    })
    
    # Histogram
    output$histPlot <- renderPlot({
        if(input$fill != "none"){
            ggplot(filtered_data(), aes(x = r)) +
                geom_histogram(aes_string(fill = input$fill), bins = input$bins, position = input$position) +
                facet_grid(ts ~ Parameter2)
            } else {
                ggplot(filtered_data(), aes(x = r)) +
                    geom_histogram(bins = input$bins, position = input$position) +
                    facet_grid(ts ~ Parameter2)
            }
        })
    
    # Histogram
    output$boxPlot <- renderPlot({
        if(input$fill != "none"){
            ggplot(filtered_data(), aes(x = ts, y = r)) +
                geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
                geom_boxplot(aes_string(fill = input$fill)) +
                facet_wrap(~Parameter2)
            } else {
                ggplot(filtered_data(), aes(x = ts, y = r)) +
                    geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
                    geom_boxplot() +
                    facet_wrap(~Parameter2)
            }
        })

    # Histogram
    output$linePlot <- renderPlot({
        if(input$fill != "none"){
            ggplot(filtered_data(), aes(x = n_Obs, y = r)) +
                geom_point(aes_string(colour = input$fill)) +
                geom_smooth(aes_string(colour = input$fill, linetype = "ts"), method = "lm", se = F) +
                facet_wrap(~Parameter2)
        } else {
            ggplot(filtered_data(), aes(x = n_Obs, y = r)) +
                geom_point() +
                geom_smooth(aes(linetype = ts), method = "lm", se = F) +
                facet_wrap(~Parameter2)
        }
    })
    session$onSessionEnded(stopApp)
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

