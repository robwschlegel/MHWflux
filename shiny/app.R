# shiny/app.R
# The purpose of this app is to facilitate the data exploration process
# There are a lot of panels that need to be looked at and it's easier to
# do so via a reactive environment

# Libraries ---------------------------------------------------------------

# .libPaths(c("~/R-packages", .libPaths()))
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(correlation)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggraph)
# setwd("shiny")


# Data --------------------------------------------------------------------

# The GLOYS MHW events by region
# Created in 'analysis/data-prep.Rmd'
GLORYS_region_MHW <- readRDS("GLORYS_region_MHW.Rda")

# MHW Clims
GLORYS_MHW_clim <- GLORYS_region_MHW %>%
    select(-cats) %>%
    unnest(events) %>%
    filter(row_number() %% 2 == 1) %>%
    unnest(events) %>% 
    ungroup()

# MHW Events
GLORYS_MHW_event <- GLORYS_region_MHW %>%
    select(-cats) %>%
    unnest(events) %>%
    filter(row_number() %% 2 == 0) %>%
    unnest(events) %>% 
    mutate(month_peak = lubridate::month(date_peak, label = T),
           season = case_when(month_peak %in% c("Jan", "Feb", "Mar") ~ "Winter",
                              month_peak %in% c("Apr", "May", "Jun") ~ "Spring",
                              month_peak %in% c("Jul", "Aug", "Sep") ~ "Summer",
                              month_peak %in% c("Oct", "Nov", "Dec") ~ "Autumn"),
           season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))) %>%
    select(-month_peak) %>% 
    ungroup()

# MHW Categories
GLORYS_MHW_cats <- GLORYS_region_MHW %>%
    select(-events) %>%
    unnest(cats) %>% 
    ungroup()

# Physical variable anomalies
ALL_anom <- readRDS("ALL_anom.Rda")
ALL_anom_cum <- readRDS("ALL_anom_cum.Rda")
ALL_anom_mld <- readRDS("ALL_anom_mld.Rda")

# Combine the anomaly dataframes into one
ALL_anom_full <- rbind(ALL_anom[,c("region", "var", "t", "anom")], 
                       ALL_anom_cum[,c("region", "var", "t", "anom")],
                       ALL_anom_mld[,c("region", "var", "t", "anom")])
ALL_anom_full_wide <- ALL_anom_full %>% 
    pivot_wider(values_from = anom, names_from = var)

# The correlations
ALL_cor <- readRDS("ALL_cor.Rda")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # The app title
    # dashboardHeader(title = "Correlation between SST and other variable anomalies during MHWs"),
    dashboardHeader(title = "MHW flux"),
    
    # The primary options
    dashboardSidebar(
        sidebarMenu(id = "mainMenu",
                    menuItem("Summary", tabName = "summary", icon = icon("chart-bar"), selected = TRUE),
                    menuItem("Event", tabName = "event", icon = icon("chart-pie")),
                    menuItem("About", tabName = "about", icon = icon("question")),
                    # The reactive controls based on the primary option chosen
                    uiOutput(outputId = "sidebar_controls"))
    ),
    
    # The dashboard
    dashboardBody(
        tabItems(

    # Summary figures ---------------------------------------------------------
            
            tabItem(tabName = "summary", 
                    fluidRow(
                        # Histogram box
                        box(width = 6, title = "Histogram", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                            dropdownButton(
                                h4("Histogram controls:"),
                                radioButtons(inputId = "position", label = "Position:", 
                                             choices = c("stack", "dodge"),
                                             selected = "stack", inline = T),
                                sliderInput(inputId = "bins", label = "Number of bins:",
                                            min = 1, max = 20, value = 10),
                                circle = TRUE, status = "danger", icon = icon("gear")),
                            plotOutput("histPlot")),
                        # Boxplot box
                        box(width = 6, title = "Boxplot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                            dropdownButton(
                                h4("Boxplot controls:"),
                                radioButtons(inputId = "notch", label = "Notches:", 
                                             choices = c(TRUE, FALSE),
                                             selected = TRUE, inline = T),
                                circle = TRUE, status = "danger", icon = icon("gear")),
                            plotOutput("boxPlot"))),
                    # Lineplot box
                    fluidRow(box(plotOutput("linePlot"), width = 12, title = "Lineplot", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE))),
            

    # Event figures -----------------------------------------------------------

            tabItem(tabName = "event",
                    # The event metric table
                    fluidRow(box(dataTableOutput("eventTable"), width = 12, title = "Event metrics", 
                                 status = "primary", solidHeader = TRUE, collapsible = TRUE)),
                    # The correlation plot
                    fluidRow(box(width = 6, title = "Correlation plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                 dropdownButton(
                                     h4("Correlation controls:"),
                                     selectInput(inputId = "vars2", label = "Variables:",
                                                 choices = levels(ALL_cor$Parameter2), multiple = TRUE, 
                                                 selected = c("temp", "msnlwrf_mld", "mslhf_mld", 
                                                              "msshf_mld", "msnswrf_mld", "qnet_mld")),
                                     circle = TRUE, status = "danger", icon = icon("gear")),
                                 plotOutput("correlationPlot")),
                             # The scatterplot
                             box(width = 6, title = "Scatterplot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                 dropdownButton(
                                     h4("Scatter controls:"),
                                     selectInput(inputId = "scat_x", label = "X axis:",
                                                 choices = unique(ALL_anom_full$var),
                                                 selected = "qnet_mld"),
                                     selectInput(inputId = "scat_y", label = "Y axis:",
                                                 choices = unique(ALL_anom_full$var),
                                                 selected = "temp"),
                                     circle = TRUE, status = "danger", icon = icon("gear")),
                                 plotOutput("scatterPlot")))),
                    # Test box
                    # fluidRow(box(verbatimTextOutput("devel")))),
            

    # App explanation ---------------------------------------------------------

            tabItem(tabName = "about", 
                    h2("This is where the functioning of the app will be explained..."))
            )
        )
    )


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
    # Render UI ---------------------------------------------------------------

    output$sidebar_controls <- renderUI({
        if(input$mainMenu == "summary"){
            sidebarMenu(
                pickerInput(inputId = "vars", label = "Variables:",
                            choices = levels(ALL_cor$Parameter2), multiple = TRUE,
                            options = list(size = 6),
                            selected = c("msnlwrf_mld", "mslhf_mld", 
                                         "msshf_mld", "msnswrf_mld", "qnet_mld")),
                pickerInput(inputId = "regions", label = "Regions:",
                            choices = unique(ALL_cor$region), multiple = TRUE,
                            options = list(`actions-box` = TRUE, size = 6),
                            selected = unique(ALL_cor$region)),
                pickerInput(inputId = "seasons", label = "Seasons:",
                            choices = levels(ALL_cor$season), multiple = TRUE,
                            selected = levels(ALL_cor$season)),
                pickerInput(inputId = "ts", label = "MHW sections:",
                            choices = levels(ALL_cor$ts), multiple = TRUE,
                            selected = c("onset", "decline")),
                prettyRadioButtons(inputId = "fill", label = "Grouping:", 
                                   choices = c("region", "season", "none"),
                                   selected = "none", inline = T,
                                   status = "primary", fill = TRUE),
                sliderInput(inputId = "duration", label = "Min Duraion:",
                            min = 1, max = max(ALL_cor$n_Obs),
                            value = c(1, max(ALL_cor$n_Obs))),
                sliderInput(inputId = "p_val", label = "Max p:",
                            min = 0, max = 1, value = 1)
            )
        } else if(input$mainMenu == "event"){
            sidebarMenu(
                pickerInput(inputId = "regions", label = "Regions:",
                            choices = unique(ALL_cor$region), multiple = TRUE,
                            options = list(`actions-box` = TRUE, size = 6),
                            selected = unique(ALL_cor$region)),
                pickerInput(inputId = "seasons", label = "Seasons:",
                            choices = levels(ALL_cor$season), multiple = TRUE,
                            selected = levels(ALL_cor$season)),
                pickerInput(inputId = "ts", label = "MHW section:",
                            choices = levels(ALL_cor$ts), multiple = FALSE,
                            selected = "full"),
                sliderInput(inputId = "duration", label = "Max Duraion:",
                            min = 1, max = max(GLORYS_MHW_event$duration),
                            value = max(GLORYS_MHW_event$duration))
            )
        } else{
            # Intentionally empty
        }
    })

    
    # Filter data -------------------------------------------------------------

    # The correlations results
    cor_data <- reactive({
        req(input$vars); req(input$p_val)
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
    
    # The GLORYS MHW metrics
    MHW_data <- reactive({
        req(input$regions)
        GLORYS_MHW_event_sub <- GLORYS_MHW_event %>% 
            dplyr::rename(i_mean = intensity_mean, i_max = intensity_max, i_cum = intensity_cumulative,
                          start = date_start, peak = date_peak, end = date_end, event = event_no) %>% 
            dplyr::select(region, season, event, start, peak, end, duration,
                          i_mean, i_max, i_cum, rate_onset, rate_decline) %>% 
            filter(region %in% input$regions,
                   season %in% input$seasons,
                   duration <= input$duration) %>% 
            mutate_if(is.numeric, round, 2)
        return(GLORYS_MHW_event_sub)
    })
    
    # Data for the selected MHW
    MHW_single <- reactive({
        req(length(input$eventTable_cell_clicked) > 0)
        
        # Find selected event
        MHW_data <- MHW_data()
        event_sub <- MHW_data[input$eventTable_cell_clicked$row,]

        # Filter accordingly
        if(input$ts == "full"){
            ts_wide <- ALL_anom_full_wide %>%
                filter(t >= event_sub$start,
                       t <= event_sub$end,
                       region == event_sub$region)
        } else if(input$ts == "onset"){
            ts_wide <- ALL_anom_full_wide %>%
                filter(t >= event_sub$start,
                       t <= event_sub$peak,
                       region == event_sub$region)
        } else if(input$ts == "decline"){
            ts_wide <- ALL_anom_full_wide %>%
                filter(t >= event_sub$peak,
                       t <= event_sub$end,
                       region == event_sub$region)
        }
        return(ts_wide)
    })
    

    # Summary figures ---------------------------------------------------------
    
    # Histogram
    output$histPlot <- renderPlot({
        req(input$vars); req(input$p_val)
        if(input$fill != "none"){
            ggplot(cor_data(), aes(x = r)) +
                geom_vline(aes(xintercept = 0), colour = "red", size = 1) +
                geom_histogram(aes_string(fill = input$fill), bins = input$bins, position = input$position) +
                facet_grid(ts ~ Parameter2)
            } else {
                ggplot(cor_data(), aes(x = r)) +
                    geom_vline(aes(xintercept = 0), colour = "red", size = 1) +
                    geom_histogram(bins = input$bins, position = input$position) +
                    facet_grid(ts ~ Parameter2)
            }
        })
    
    # Boxplot
    output$boxPlot <- renderPlot({
        req(input$vars); req(input$p_val)
        if(input$fill != "none"){
            ggplot(cor_data(), aes(x = ts, y = r)) +
                geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
                geom_boxplot(aes_string(fill = input$fill), notch = input$notch) +
                facet_wrap(~Parameter2)
            } else {
                ggplot(cor_data(), aes(x = ts, y = r)) +
                    geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
                    geom_boxplot(notch = input$notch) +
                    facet_wrap(~Parameter2)
            }
        })

    # Lineplot
    output$linePlot <- renderPlot({
        req(input$vars)
        if(input$fill != "none"){
            ggplot(cor_data(), aes(x = n_Obs, y = r)) +
                geom_point(aes_string(colour = input$fill)) +
                geom_smooth(aes_string(colour = input$fill, linetype = "ts"), method = "lm", se = F) +
                facet_wrap(~Parameter2)
        } else {
            ggplot(cor_data(), aes(x = n_Obs, y = r)) +
                geom_point() +
                geom_smooth(aes(linetype = ts), method = "lm", se = F) +
                facet_wrap(~Parameter2)
        }
    })
    

    # Event figures -----------------------------------------------------------

    # Table showing filtered events from sidebar controls
    output$eventTable = renderDataTable({
        MHW_data()
    }, selection = 'single')
    
    # Correlation plot for single figure
    output$correlationPlot <- renderPlot({
        req(input$vars2)
        # TO DO: Add functionaility to chose variables
        MHW_single() %>%
        # ALL_anom_full_wide %>% 
            pivot_longer(cols = msnlwrf:qnet_mld) %>% 
            filter(name %in% input$vars2) %>%
            # filter(name %in% vars) %>%
            pivot_wider(names_from = name, values_from = value) %>% 
            correlation() %>%
            plot()
    })
    
    # Scatterplot of two variable during onset, full, or decline of a single event
    output$scatterPlot <- renderPlot({
        ggplot(data = MHW_single(), aes_string(x = input$scat_x, y = input$scat_y)) +
            geom_smooth(method = "lm") +
            geom_point(aes(colour = t))
    })
    
    # Test text output for table interaction
    output$devel <- renderPrint({
        req(length(input$eventTable_cell_clicked) > 0)
        # input$eventTable_cell_clicked
        MHW_single()
    })
    
    # This automatically ends the session when the app is closed
    session$onSessionEnded(stopApp)
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

