# shiny/app.R
# The purpose of this app is to facilitate the data exploration process
# There are a lot of panels that need to be looked at and it's easier to
# do so via a reactive environment

# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(correlation)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(plotly)
library(see)
# setwd("shiny")


# Data --------------------------------------------------------------------

# The GLORYS MHW events by region
# Created in 'analysis/data-prep.Rmd'
# region_MHW <- readRDS("GLORYS_region_MHW.Rda")
region_MHW <- readRDS("OISST_region_MHW.Rda")

# MHW Clims
MHW_clim <- region_MHW %>%
    select(-cats) %>%
    unnest(events) %>%
    filter(row_number() %% 2 == 1) %>%
    unnest(events) %>% 
    ungroup()

# MHW Events
MHW_event <- region_MHW %>%
    select(-cats) %>%
    unnest(events) %>%
    filter(row_number() %% 2 == 0) %>%
    unnest(events) %>% 
    ungroup() %>% 
    mutate(month_peak = lubridate::month(date_peak, label = T),
           season = case_when(month_peak %in% c("Jan", "Feb", "Mar") ~ "Winter",
                              month_peak %in% c("Apr", "May", "Jun") ~ "Spring",
                              month_peak %in% c("Jul", "Aug", "Sep") ~ "Summer",
                              month_peak %in% c("Oct", "Nov", "Dec") ~ "Autumn"),
           season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
           region = factor(region, levels = c("mab", "gm", "ss", "cbs", "gsl", "nfs"))) %>%
    select(-month_peak) %>% 
    mutate_if(is.numeric, round, 2)

# MHW Categories
MHW_cats <- region_MHW %>%
  select(-events) %>%
  unnest(cats) %>% 
  ungroup()

# Event count by region 
region_count <- MHW_event %>% 
    group_by(region) %>% 
    summarise(count = n())

# Event count by region 
season_count <- MHW_event %>% 
    group_by(season) %>% 
    summarise(count = n())

# Event count by region 
region_season_count <- MHW_event %>% 
    group_by(region, season) %>% 
    summarise(count = n())

# List of desired variables
choose_vars <- c("sst", "bottomT", "sss", "mld_cum", "mld_1_cum", "t2m", "tcc_cum", "p_e_cum", "mslp_cum",
                 "lwr_mld_cum", "swr_mld_cum", "lhf_mld_cum", "shf_mld_cum", "qnet_mld_cum", "sst_thresh")

# Physical variable anomalies
ALL_anom <- readRDS("ALL_anom.Rda")
ALL_anom_cum <- readRDS("ALL_anom_cum.Rda")

# Combine the anomaly dataframes into one
ALL_anom_full <- rbind(ALL_anom[,c("region", "var", "t", "anom")], 
                       ALL_anom_cum[,c("region", "var", "t", "anom")]) %>%
  filter(var %in% choose_vars) %>% 
  mutate(region = factor(region, levels = c("mab", "gm", "ss", "cbs", "gsl", "nfs")),
         var = case_when(var == "sst" ~ "SST",
                         var == "bottomT" ~ "SBT",
                         var == "sss" ~ "SSS",
                         var == "mld_cum" ~ "MLD_c",
                         var == "mld_1_cum" ~ "MLD_1_c",
                         var == "t2m" ~ "Air_temp",
                         var == "tcc_cum" ~ "Cloud_cover_c",
                         var == "p_e_cum" ~ "Precip_Evap_c",
                         var == "mslp_cum" ~ "MSLP_c",
                         var == "lwr_mld_cum" ~ "Qlw",
                         var == "swr_mld_cum" ~ "Qsw",
                         var == "lhf_mld_cum" ~ "Qlh",
                         var == "shf_mld_cum" ~ "Qsh",
                         var == "qnet_mld_cum" ~ "Qnet",
                         TRUE ~ var))
ALL_anom_full_wide <- ALL_anom_full %>% 
    pivot_wider(values_from = anom, names_from = var, values_fn = mean)

# The correlations
ALL_cor <- readRDS("ALL_cor.Rda") %>% 
  mutate(region = factor(region, levels = c("mab", "gm", "ss", "cbs", "gsl", "nfs")),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
         Parameter2 = as.character(Parameter2)) %>% 
  filter(Parameter1 %in% choose_vars,
         Parameter2 %in% choose_vars) %>% 
  mutate(Parameter1 = case_when(Parameter1 == "sst" ~ "SST",
                                Parameter1 == "bottomT" ~ "SBT",
                                Parameter1 == "sss" ~ "SSS",
                                Parameter1 == "mld_cum" ~ "MLD_c",
                                Parameter1 == "mld_1_cum" ~ "MLD_1_c",
                                Parameter1 == "t2m" ~ "Air_temp",
                                Parameter1 == "tcc_cum" ~ "Cloud_cover_c",
                                Parameter1 == "p_e_cum" ~ "Precip_Evap_c",
                                Parameter1 == "mslp_cum" ~ "MSLP_c",
                                Parameter1 == "lwr_mld_cum" ~ "Qlw",
                                Parameter1 == "swr_mld_cum" ~ "Qsw",
                                Parameter1 == "lhf_mld_cum" ~ "Qlh",
                                Parameter1 == "shf_mld_cum" ~ "Qsh",
                                Parameter1 == "qnet_mld_cum" ~ "Qnet",
                                TRUE ~ Parameter1),
         Parameter2 = case_when(Parameter2 == "sst" ~ "SST",
                                Parameter2 == "bottomT" ~ "SBT",
                                Parameter2 == "sss" ~ "SSS",
                                Parameter2 == "mld_cum" ~ "MLD_c",
                                Parameter2 == "mld_1_cum" ~ "MLD_1_c",
                                Parameter2 == "t2m" ~ "Air_temp",
                                Parameter2 == "tcc_cum" ~ "Cloud_cover_c",
                                Parameter2 == "p_e_cum" ~ "Precip_Evap_c",
                                Parameter2 == "mslp_cum" ~ "MSLP_c",
                                Parameter2 == "lwr_mld_cum" ~ "Qlw",
                                Parameter2 == "swr_mld_cum" ~ "Qsw",
                                Parameter2 == "lhf_mld_cum" ~ "Qlh",
                                Parameter2 == "shf_mld_cum" ~ "Qsh",
                                Parameter2 == "qnet_mld_cum" ~ "Qnet",
                                TRUE ~ Parameter2))

# Corners of the study area
    # Created in 'MHWNWA/analysis/polygon-prep.Rmd'
NWA_corners <- readRDS("NWA_corners.Rda")

# Individual regions
    # Created in 'MHWNWA/analysis/polygon-prep.Rmd'
NWA_coords <- readRDS("NWA_coords.Rda") %>% 
    mutate(region = factor(region, levels = c("mab", "gm", "ss", "cbs", "gsl", "nfs")))

# The base land polygon
# Created in 'MHWNWA/analysis/polygon-prep.Rmd'
map_base <- readRDS("map_base.Rda")

# The base map frame used for all figures
frame_base <- ggplot(map_base, aes(x = lon, y = lat)) +
    scale_x_continuous(breaks = seq(-70, -50, 10),
                       labels = c("70°W", "60°W", "50°W"),
                       position = "top") +
    scale_y_continuous(breaks = c(40, 50),
                       labels = scales::unit_format(suffix = "°N", sep = "")) +
    coord_cartesian(xlim = c(NWA_corners[1:2]), ylim = c(NWA_corners[3:4]), expand = F) +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
          axis.text = element_text(size = 12, colour = "black"),
          axis.ticks = element_line(colour = "black"))

# The results text table
# res_table <- read_csv("res_table.csv")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # The app title
    # dashboardHeader(title = "Correlation between SST and other variable anomalies during MHWs"),
    dashboardHeader(title = "MHW flux"),
    
    # The primary options
    dashboardSidebar(
        sidebarMenu(id = "mainMenu",
                    menuItem("Map", tabName = "map", icon = icon("map"), selected = TRUE),
                    menuItem("Event", tabName = "event", icon = icon("chart-pie")),
                    menuItem("Summary", tabName = "summary", icon = icon("chart-bar")),
                    menuItem("Flavours", tabName = "flavours", icon = icon("adjust")),
                    # menuItem("Tables", tabname = "tables", icon = icon("table")),
                    menuItem("About", tabName = "about", icon = icon("question")),
                    # The reactive controls based on the primary option chosen
                    uiOutput(outputId = "sidebar_controls"))
    ),
    
    # The dashboard
    dashboardBody(
        tabItems(


    # Map figures -------------------------------------------------------------

            tabItem(tabName = "map",
                    fluidRow(box(plotOutput("mapRegions"), width = 6, title = "Region map",
                                 status = "primary", solidHeader = TRUE, collapsible = TRUE),
                             box(plotlyOutput("eventLolli"), width = 6, title = "MHW Lollis",
                                 status = "primary", solidHeader = TRUE, collapsible = TRUE))),
            
            
    # Event figures -----------------------------------------------------------
            
            tabItem(tabName = "event",
                    # The event metric table
                    fluidRow(box(dataTableOutput("eventTable"), width = 12, title = "Event metrics", 
                                 status = "primary", solidHeader = TRUE, collapsible = TRUE)),
                    # The correlation plot
                    fluidRow(box(width = 4, title = "Correlation plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                 dropdownButton(
                                     h4("Correlation controls:"),
                                     selectInput(inputId = "vars2", label = "Variables:",
                                                 # choices = list(
                                                 #   Flux = c("Qnet", "Qlw", "Qsw", "Qlh", "Qsh"),
                                                 #   Air = c("Air temp", "Cloud cover (c)", "Precip-Evap (c)", "MSLP (c)"),
                                                 #   Sea = c("SST", "SSS", "MLD", "MLD_1", "SBT")
                                                 # ), 
                                                 choices = unique(ALL_cor$Parameter2), 
                                                 multiple = TRUE, 
                                                 selected = c("SST", "Qlw", "Qsw", "Qlh", "Qsh", "Qnet")),
                                     circle = TRUE, status = "danger", icon = icon("gear")),
                                 plotOutput("correlationPlot")),
                             # The scatterplot
                             box(width = 4, title = "Scatterplot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                 dropdownButton(
                                     h4("Scatter controls:"),
                                     # These inputs need to be ordered by category
                                     selectInput(inputId = "scat_x", label = "X axis:",
                                                 choices = unique(ALL_anom_full$var),
                                                 selected = "Qnet"),
                                     selectInput(inputId = "scat_y", label = "Y axis:",
                                                 choices = unique(ALL_anom_full$var),
                                                 selected = "SST"),
                                     circle = TRUE, status = "danger", icon = icon("gear")),
                                 plotOutput("scatterPlot")),
                             box(width = 4, title = "RMSE", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                 dropdownButton(
                                   h4("RMSE controls:"),
                                   selectInput(inputId = "rmse_var", label = "Qx:",
                                               choices = c("Qnet", "Qlw", "Qsw", "Qlh", "Qsh"),
                                               selected = "Qnet"),
                                   circle = TRUE, status = "danger", icon = icon("gear")),
                                 plotOutput("rmsePlot")))),
            # Test box
            # fluidRow(box(verbatimTextOutput("devel")))),
            
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
            

    # Flavour figures ---------------------------------------------------------

    # Show ow the proportion of the mix of seasons and regions changes when selecting for
    # the most dominant Q variables, and then down the list
    # These would be visualised by growing or shrinking balls of colour.
    # Each representing a region or season. Within each ball would be the count of events.
    # These would be arranged to look like an ice cream cone.
    
            tabItem(tabName = "flavours",
                    fluidRow(box(width = 6, title = "Flavourtown", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                 uiOutput("flux", inline = T), uiOutput("air", inline = T), uiOutput("sea", inline = T),
                                 plotlyOutput("flavourPlot")))),  
    

    # Tables ------------------------------------------------------------------

            # tabItem(tabname = "tables",
            #         # tableOutput("resultsKable"),
            #         h2("test")),
                

    # App explanation ---------------------------------------------------------

            tabItem(tabName = "about", 
                    fluidPage(
                      column(12,
                             h2(tags$b("About")),
                             p("The purpose of this app is to provide interactive access to the results from the",
                               a(target = '_blank', rel = 'noopener noreferrer',
                                 href = "https://robwschlegel.github.io/MHWflux/", "MHW Flux project"), ". There are many ways to visualise
                               the results and I have chosen three primary ways of doing so. These are outlined in the following sub-headers.
                               For more information about this project and the data used in it please follow the link in this paragraph.
                               The column on the left of the screen contains the names of the tabs that the user may click on to bring up
                               different dashboards. As the tabs are clicked on the other controls in this column will change accordingly.
                               The controls generated for each tab are explained in the specific headers below."),
                             h2(tags$b("Map")),
                             p("This tab shows the study area and the region polygons therein in the left panel. Underneath the colourful polygons
                               are labels that show the count of MHWs per season against the total count of MHWs in that season. This number will change
                               if the duration slider in the control column is moved. Note that this is a dual-ended slider and so one may chose to filter
                               out MHWs from both short and long durations simultaneously. One may also choose to de-select any of the regions or
                               seasons as desired. The panel to the right shows the MHWs detected in each region. The colours between the panels correspond
                               to the regions. The MHW panel is a plotly figure so one may hover over the lolliplots for more information."),
                             h2(tags$b("Event")),
                             p("This tab allows users to generate visualisations for individual events. One must first click on an event in the table
                               before the bottom three panels will show anything. Note that one may filter and search in the table to more quickly find
                               a MHW of interest. The filtering options in the control column also work on the table. Once an event has been clicked it is
                               recommended to minimise the table by clicking in the top right corner. Also note that the portion of the event may also be
                               selected. This means one may visualise the stats for just the onset or decline portion of the MHWs, in addition to the full event.
                               With an event selected one may now see in the left panel a correlation plot showing the relationship of SST with the other
                               Qx terms. The colour of the lines shows the strength of the correlations, and the thickness shows the significance.
                               With thicker lines having smaller p values. The variables being shown may be changed by clicking the red gear icon
                               and then clicking in the variables selection box. The centre panel shows a scatterplot of SST against Qnet. Layered on top of this
                               is a linear model that helps to demonstrate the strength of a correlation seen in the right panel. The red gear icon here allows
                               user to change the variables seen in the X and Y axes. The right panel shows the RMSE between SST and Qnet. The red gear icon allows
                               one to select a different Qx term. Note that the SST anomalies shown here are SST - threshold (90th percentile), and not SST - seasonal
                               climatology, as seen throughout the rest of the project."),
                             h2(tags$b("Summary")),
                             p("The summary tab shows a higher level report on the correlations between variables and SST. The left panel shows the correlation results
                               via histograms. The X-axes in the histogram show the strength of the correlations between SST and the given variable. These will range
                               from -1 to 1. Each column of histograms belongs to one variable, as seen in the top header. Each row of histograms belongs to one
                               portion of the time series. By default these are the onset and decline portions of the events. This can be changed in the control column
                               to the left. Different variables may be chosen in the control column, and regions and seasons may be filtered out. Scrolling down, one
                               will see that events may be filtered by duration, as well as the p-value of the correlation. It is also possible to chose to group the
                               histograms by regions or seasons. The vertical red lines in the centre of the histograms serve as a visual aid/reminder that a correlation
                               of 0 means no relationship. This red line relates directly to the horizontal red line in the right panel, which is showing the same data as
                               the left panel but grouped into boxplots instead. The same groupings and filtering may be applied to these boxplots as to the histograms.
                               Lastly, there is a third panel on the bottom that is minimised by default. This is a series of scatterplots with linear models fitted to them
                               that show the relationship between the duration of a MHW (X-axis) and the correlation strength (Y-axis) of variables. It basically shows
                               that there isn't much of a pattern. That longer MHWs tend to be more unique in their drivers so any relationship with the duration of an
                               event and physical drivers tends to break down after 30 days or so."),
                             h2(tags$b("Flavours")),
                             p("Welcome to flavourtown! This was my failed attempt at creating some sort of visualisation that looked like ice-cream cones. The
                               idea was that there appear to be different 'flavours' of MHWs. For example, events whose onset coincides with strong latent heatflux
                               and low high cloud cover. What I was left with here is a sort of game in which the user may click on either of the three icons at the top
                               of the panel to move sliders for Qx, atmosphere, or ocean variables to see if one can find a strong fingerprint of events the all share
                               something in common. The balls themselves show the total number of events per region and season that meet the criteria in the sliders. The
                               sliders go from -1 to 1 for each variable and show the acceptable range of correlations that the user will take. So if one wanted to filter
                               out events that had a correlation of at least 0.7 with shortwave radiation,click on the red icon and slide the Qsw slider accordingly. To filter
                               out events with mixed layer depth correlation less greater than -0.7, click on the dark blus icon and slide that little guy over to -0.7.
                               It's actually quite a lot of fun to play around with. If you're into that sort of thing.")
                             )
                      )
                    )
            )
        )
    )


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
    # Render UI ---------------------------------------------------------------

    # Select variables from a dropdown
    picker_vars <- pickerInput(inputId = "vars", label = "Variables:",
                               choices = list(
                                 Flux = c("Qnet", "Qlw", "Qsw", "Qlh", "Qsh"),
                                 Air = c("Air_temp", "Cloud_cover_c", "Precip_Evap_c", "MSLP_c"),
                                 Sea = c("SST", "SSS", "MLD_c", "MLD_1_c", "SBT")
                                 ),
                               multiple = TRUE,
                               options = list(size = 6),
                               selected = c("Qnet", "Qlw", "Qsw", "Qlh", "Qsh"))
    # picker_vars <- selectInput(inputId = "vars", label = "Variables:",
    #                            multiple = TRUE, 
    #                            selected = c("Qnet", "Qlw", "Qsw", "Qlh", "Qsh"))
    
    # Select regions from a dropdown
    picker_regions <- pickerInput(inputId = "regions", label = "Regions:",
                                  choices = levels(MHW_event$region), multiple = TRUE,
                                  options = list(`actions-box` = TRUE, size = 6),
                                  selected = levels(MHW_event$region))
    
    # Select seasons from a dropdown
    picker_seasons <- pickerInput(inputId = "seasons", label = "Seasons:",
                                  choices = levels(ALL_cor$season), multiple = TRUE,
                                  selected = levels(ALL_cor$season))
    
    # Select parts of a time series
    picker_ts_multiple <- pickerInput(inputId = "ts_multiple", label = "MHW sections:",
                                      choices = levels(ALL_cor$ts), multiple = TRUE,
                                      selected = c("onset", "decline"))
    picker_ts_single <- pickerInput(inputId = "ts_single", label = "MHW section:",
                                    choices = levels(ALL_cor$ts), multiple = FALSE,
                                    selected = "onset")
    
    # Radio buttons to choose fill of histograms and boxplots
    radio_fill <- prettyRadioButtons(inputId = "fill", label = "Grouping:", 
                                     choices = c("region", "season", "none"),
                                     selected = "none", inline = T,
                                     status = "primary", fill = TRUE)
    
    # Filter events by their duration
    slider_duration_min <- sliderInput(inputId = "duration_min", label = "Min Duraion:",
                                       min = 1, max = max(ALL_cor$n_Obs),
                                       value = c(1, max(ALL_cor$n_Obs)))
    slider_duration <- sliderInput(inputId = "duration", label = "Duration:",
                                   min = 1, max = max(MHW_event$duration),
                                   value = c(1, max(MHW_event$duration)))
    
    # Filter correlations by p-value
    slider_p_val <- sliderInput(inputId = "p_val", label = "Max p:",
                                min = 0, max = 1, value = 1)
    
    # Filter events by heat flux terms
    slider_Qlh <- sliderInput(inputId = "Qlh", label = "Qlh:",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_Qsh <- sliderInput(inputId = "Qsh", label = "Qsh:",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_Qlw <- sliderInput(inputId = "Qlw", label = "Qlw:",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_Qsw <- sliderInput(inputId = "Qsw", label = "Qsw:",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_Qnet <- sliderInput(inputId = "Qnet", label = "Qnet:",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    
    # Filter events by sea variables
    slider_SBT <- sliderInput(inputId = "SBT", label = "SBT:",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_SSS <- sliderInput(inputId = "SSS", label = "SSS:",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_MLD <- sliderInput(inputId = "MLD", label = "MLD (c):",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_MLD_1 <- sliderInput(inputId = "MLD_1", label = "1/MLD (c):",
                                min = -1, max = 1, value = c(-1, 1), step = 0.1)
    
    # Filter events by air variables
    slider_air <- sliderInput(inputId = "air", label = "Air temp:",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_cloud <- sliderInput(inputId = "cloud", label = "Cloud cover (c):",
                                min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_p_e <- sliderInput(inputId = "p_e", label = "Precip-Evap (c):",
                              min = -1, max = 1, value = c(-1, 1), step = 0.1)
    slider_MSLP <- sliderInput(inputId = "MSLP", label = "MSLP (c):",
                               min = -1, max = 1, value = c(-1, 1), step = 0.1)
    
    # Render dropdown buttons
    output$flux <- renderUI({
      dropdownButton(status = "danger", icon = icon("fire"), circle = TRUE, 
                     tooltip = TRUE, label = "Flux", inline = T,
                     slider_Qnet, slider_Qlw, slider_Qsw, slider_Qlh, slider_Qsh)
    })
    output$air <- renderUI({
      dropdownButton(status = "info", icon = icon("cloud"), circle = TRUE, 
                     tooltip = TRUE, label = "Air", inline = T,
                     slider_air, slider_cloud, slider_p_e, slider_MSLP)
    })
    output$sea <- renderUI({
      dropdownButton(status = "primary", icon = icon("tint"), circle = TRUE, 
                     tooltip = TRUE, label = "Sea", inline = T,
                     slider_SBT, slider_SSS, slider_MLD, slider_MLD_1)
    })
    
    # The chosen controls per tab
    output$sidebar_controls <- renderUI({
        if(input$mainMenu == "summary"){
            sidebarMenu(picker_vars, picker_regions, picker_seasons, picker_ts_multiple, 
                        radio_fill, slider_duration_min, slider_p_val)
        } else if(input$mainMenu == "event"){
            sidebarMenu(picker_regions, picker_seasons, picker_ts_single, slider_duration)
        } else if(input$mainMenu == "map"){
            sidebarMenu(picker_regions, picker_seasons, slider_duration)
        } else if(input$mainMenu == "flavours"){
          sidebarMenu(picker_ts_single, slider_duration)
        } else {
          # Intentionally empty
        }
    })

    
    # Filter data -------------------------------------------------------------

    # The correlations results
    cor_data <- reactive({
        req(input$vars); req(input$p_val)
        ALL_cor_sub <- ALL_cor %>% 
            filter(Parameter1 == "SST",
                   Parameter2 %in% input$vars,
                   region %in% input$regions,
                   ts %in% input$ts_multiple,
                   season %in% input$seasons,
                   p <= input$p_val,
                   n_Obs >= input$duration[1],
                   n_Obs <= input$duration[2])
        return(ALL_cor_sub)
    })
    
    # The OISST MHW metrics
    MHW_data <- reactive({
        req(input$regions)
        MHW_event_sub <- MHW_event %>% 
            dplyr::rename(i_mean = intensity_mean, i_max = intensity_max, i_cum = intensity_cumulative,
                          start = date_start, peak = date_peak, end = date_end, event = event_no) %>% 
            dplyr::select(region, season, event, start, peak, end, duration,
                          i_mean, i_max, i_cum, rate_onset, rate_decline) %>% 
            filter(region %in% input$regions,
                   season %in% input$seasons,
                   duration >= input$duration[1],
                   duration <= input$duration[2]) %>% 
            mutate_if(is.numeric, round, 2)
        return(MHW_event_sub)
    })
    
    # Data for the selected MHW
    MHW_single <- reactive({
        req(length(input$eventTable_cell_clicked) > 0)
        
        # Find selected event
        MHW_data <- MHW_data()
        event_sub <- MHW_data[input$eventTable_cell_clicked$row,]

        # Filter accordingly
        if(input$ts_single == "full"){
            ts_wide <- ALL_anom_full_wide %>%
                filter(t >= event_sub$start,
                       t <= event_sub$end,
                       region == event_sub$region)
        } else if(input$ts_single == "onset"){
            ts_wide <- ALL_anom_full_wide %>%
                filter(t >= event_sub$start,
                       t <= event_sub$peak,
                       region == event_sub$region)
        } else if(input$ts_single == "decline"){
            ts_wide <- ALL_anom_full_wide %>%
                filter(t >= event_sub$peak,
                       t <= event_sub$end,
                       region == event_sub$region)
        }
        return(ts_wide)
    })
    
    # Data for the flavour figure
    flavour_data <- reactive({
      req(input$ts_single)#; req(input$Qlh)
      flavour_data <- ALL_cor %>% 
        filter(Parameter1 == "SST",
               ts %in% input$ts_single,
               n_Obs >= input$duration[1],
               n_Obs <= input$duration[2]) %>%
        dplyr::select(region:ts, Parameter2, r, n_Obs) %>% 
        pivot_wider(names_from = Parameter2, values_from = r) %>% 
               # Heat flux filters
        filter(Qnet >= input$Qnet[1], Qnet <= input$Qnet[2],
               Qlh >= input$Qlh[1], Qlh <= input$Qlh[2],
               Qsh >= input$Qsh[1], Qsh <= input$Qsh[2],
               Qlw >= input$Qlw[1], Qlw <= input$Qlw[2],
               Qsw >= input$Qsw[1], Qsw <= input$Qsw[2],
               # Air filters
               Air_temp >= input$air[1], Air_temp <= input$air[2],
               Cloud_cover_c >= input$cloud[1], Cloud_cover_c <= input$cloud[2],
               Precip_Evap_c >= input$p_e[1], Precip_Evap_c <= input$p_e[2],
               MSLP_c >= input$MSLP[1], MSLP_c <= input$MSLP[2],
               # Sea filters
               SBT >= input$SBT[1], SBT <= input$SBT[2],
               SSS >= input$SSS[1], SSS <= input$SSS[2],
               MLD_c >= input$MLD[1], MLD_c <= input$MLD[2],
               MLD_1_c >= input$MLD_1[1], MLD_1_c <= input$MLD_1[2])
      return(flavour_data)
    })
    
    # Map figures -------------------------------------------------------------

    # The map
    output$mapRegions <- renderPlot({
        
        MHW_data <- MHW_data()
        
        # Add other MHW metric filters
        
        # Proportion of MHWs in each region in each node
        region_info <- MHW_data %>%
            left_join(region_count, by = "region") %>% 
            group_by(region) %>%
            mutate(prop = round(n()/count, 2)) %>%
            select(region, count, prop) %>%
            unique() %>%
            ungroup()
        
        # Create labels for number of MHWs per region
        region_prop_label <- NWA_coords %>%
            left_join(region_info, by = "region") %>%
            group_by(region) %>%
            mutate(lon_center = mean(lon), lat_center = mean(lat)) %>%
            na.omit() %>% 
            mutate(lon_center = case_when(region == "gsl" ~ lon_center+2,
                                          region == "ss" ~ lon_center+1,
                                          region == "gm" ~ lon_center-1,
                                          region == "mab" ~ lon_center+1.8,
                                          TRUE ~ lon_center),
                   lat_center = case_when(region == "gm" ~ lat_center-1.5,
                                          region == "mab" ~ lat_center+0.8,
                                          TRUE ~ lat_center)) %>%
            ungroup()
        
        # Count of seasons
        season_info <- MHW_data %>%
          left_join(season_count, by = "season") %>% 
          group_by(season) %>%
          mutate(prop = round(n()/count, 2)) %>%
          select(season, count, prop) %>%
          unique() %>%
          ungroup()
        
        # The map
        mr <- frame_base +
          geom_polygon(data = region_prop_label, size = 2, alpha = 0.7,
                       aes(fill = region, colour = region, #alpha = prop,
                           text = paste0("Region: ",region)), show.legend = F) +
          geom_polygon(data = map_base, aes(group = group), show.legend = F) +
          # Count per region
          geom_label(data = region_prop_label,
                     aes(x = lon_center, y = lat_center, 
                         label = paste0(round(prop*count),"/",count))) +
          # Count per seasons
          geom_label(data = filter(season_info, season == "Spring"), 
                     aes(x = -55, y = 39, label = paste0(season,": ",round(prop*count),"/",count))) +
          geom_label(data = filter(season_info, season == "Summer"), 
                     aes(x = -55, y = 38, label = paste0(season,": ",round(prop*count),"/",count))) +
          geom_label(data = filter(season_info, season == "Autumn"), 
                     aes(x = -55, y = 37, label = paste0(season,": ",round(prop*count),"/",count))) +
          geom_label(data = filter(season_info, season == "Winter"), 
                     aes(x = -55, y = 36, label = paste0(season,": ",round(prop*count),"/",count)))
        mr
        # ggplotly(mr,  tooltip = "text") %>% 
        #     layout(showlegend = FALSE)
    })
    
    # The lolliplot
    output$eventLolli <- renderPlotly({
        
        MHW_data <- MHW_data()
        
        # Lolliplot
        el <- ggplot(data = MHW_data, aes(x = peak, y = i_max)) +
            geom_segment(aes(xend = peak, yend = 0)) +
            geom_point(shape = 21, size = 2, show.legend = F,
                       aes(fill = region,
                           text = paste0("Event: ",event,
                                         "<br>Duration: ",duration," days",
                                         "<br>Start Date: ", start,
                                         "<br>Peak Date: ", peak,
                                         "<br>End Date: ", end,
                                         "<br>Mean Intensity: ",i_mean,"°C",
                                         "<br>Max. Intensity: ",i_max,"°C",
                                         "<br>Cum. Intensity: ",i_cum,"°C"))) +
            scale_x_date(expand = c(0, 0), date_labels = "%b %Y", 
                         limits = c(min(MHW_data$start-61), max(MHW_data$end+61))) +
            scale_y_continuous(expand = c(0,0), limits = c(0, max(MHW_data$i_max)*1.1)) +
            labs(x = "", y = "Max. Intensity (°C)") +
            facet_wrap(~region, ncol = 1)
        ggplotly(el, tooltip = "text", dynamicTicks = F) %>% 
            layout(showlegend = FALSE)
    })

    # Event figures -----------------------------------------------------------
    
    # Table showing filtered events from sidebar controls
    # Add a title explicitly stating what this shows
    output$eventTable = renderDataTable({
        MHW_data()
    }, selection = 'single', caption = "test")
    
    # Correlation plot for single figure
    output$correlationPlot <- renderPlot({
        req(input$vars2)
        MHW_single() %>%
            pivot_longer(cols = c(-region, -t)) %>% 
            filter(name %in% input$vars2) %>%
            pivot_wider(names_from = name, values_from = value) %>% 
            correlation() %>%
            plot()
    })
    
    # This needs to be fixed
    
    # Scatterplot of two variable during onset, full, or decline of a single event
    output$scatterPlot <- renderPlot({
      MHW_single <- MHW_single()
      sp <- ggplot(data = MHW_single, aes_string(x = input$scat_x, y = input$scat_y)) +
        geom_smooth(method = "lm", formula = 'y ~ x') +
        geom_point(aes(colour = t)) +
        scale_colour_date(low = "red", high = "blue") +
        labs(colour = "Date")
      sp
      # ggplotly(sp, tooltip = "text", dynamicTicks = F) %>% 
      #   layout(showlegend = FALSE)
    })
    
    # RMSE scatterplot of SST and a Qx variable during onset, full, or decline of a single event
    output$rmsePlot <- renderPlot({
      # Prep ts data
      MHW_single <- MHW_single()
      MHW_single <- left_join(MHW_single, MHW_clim[,c("region", "t", "temp", "thresh")],
                              by = c("region", "t")) %>% 
        mutate(SST_thresh = temp-thresh)
      # Find RMSE value
      MHW_data <- MHW_data()
      event_sub <- MHW_data[input$eventTable_cell_clicked$row,]
      MHW_rmse <- ALL_cor %>% 
        filter(region == event_sub$region,
               event_no == event_sub$event,
               ts == input$ts_single,
               Parameter2 == input$rmse_var) %>% 
        na.omit()
      # Find y for Qx
      Qx_y <- as.numeric(MHW_single[1,input$rmse_var])
      # The plot
      rp <- ggplot(data = MHW_single, aes(x = t)) +
        geom_point(aes(y = SST_thresh), colour = "red") +
        geom_line(aes(y = SST_thresh), colour = "red") +
        geom_point(aes_string(y = input$rmse_var), colour = "blue") +
        geom_line(aes_string(y = input$rmse_var), colour = "blue") +
        geom_label(aes(x = t[1], y = SST_thresh[1],
                       label = "SST"), colour = "red") +
        geom_label(aes(x = t[1], y = Qx_y,
                       label = input$rmse_var), colour = "blue") +
        geom_label(aes(x = mean(t), y = quantile(SST_thresh, 0.1),
                       label = paste0("RMSE = ",MHW_rmse$rmse[1]))) +
        labs(x = NULL, y = "SST-thresh (°C) | Qx/MLD (°C)")
      rp
    })
    
    # Test text output for table interaction
    output$devel <- renderPrint({
        req(length(input$eventTable_cell_clicked) > 0)
        # input$eventTable_cell_clicked
        MHW_single()
    })
    

    # Summary figures ---------------------------------------------------------
    
    # Histogram
    output$histPlot <- renderPlot({
      req(input$vars); req(input$p_val); req(input$ts_multiple)
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
      req(input$vars); req(input$p_val); req(input$ts_multiple)
      
      cor_data <- cor_data()
      
      if(input$fill != "none"){
        ggplot(data = cor_data, aes(x = ts, y = r)) +
          geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
          geom_boxplot(aes_string(fill = input$fill), notch = input$notch) +
          facet_wrap(~Parameter2)
        } else {
          ggplot(data = cor_data, aes(x = ts, y = r)) +
            geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
            geom_boxplot(notch = input$notch) +
            facet_wrap(~Parameter2)
          }
      })
    
    # Lineplot
    output$linePlot <- renderPlot({
        req(input$vars); req(input$p_val); req(input$ts_multiple)
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
    

    # Flavour figures ---------------------------------------------------------

    # Flavourplot
    output$flavourPlot <- renderPlotly({
      req(input$ts_single); req(input$Qnet)
      
      flavour_data <- flavour_data()
      
      flavour_count <- flavour_data %>%
        dplyr::select(region, season, event_no) %>% 
        unique() %>% 
        group_by(region, season) %>% 
        summarise(sub_count = n()) %>% 
        ungroup() %>% 
        left_join(region_season_count, by = c("region", "season"))
      
      fp <- ggplot(data = flavour_count, aes(x = region, y = season)) +
        geom_point(aes(colour = region, size = sub_count,
                       text = paste0(sub_count,"/",count))) +
        # geom_label(aes(label = paste0(sub_count,"/",count))) +
        scale_size(range = c(1, 30), limits = c(1, 19)) +
        # scale_y_reverse() +
        # ylim(breaks = c("Spring", "Summer", "Autumn", "Winter")) +
        ylim(breaks = c("Winter", "Spring", "Summer", "Autumn")) +
        labs(x = NULL, y = NULL)
      # fp
      ggplotly(fp, tooltip = "text", dynamicTicks = F) %>%
        layout(showlegend = FALSE)
    })

    # Tables ------------------------------------------------------------------

    # output$resultsKable <- function() {
    #     res_table %>% 
    #         knitr::kable(format = "html", caption = "Most of the variables that have been 
    #         correlated against the temperature anomalies during the onset, 
    #                                     decline, and full duration of MHWs. The cumulative heat flux terms 
    #                                     were corrected for by the daily MLD (Q/(rho x Cp x hmld)) before 
    #                                     the correlations were calculated. Correlations were also run on the 
    #                                     cumulative flux terms without correcting for MLD, but there was little 
    #                                     difference so the results are not itemised here. This table shows the 
    #                                     full names of the variables, as well as the abbreviations used in the code. 
    #                                     The 'onset' column describes (in shorthand) what the tendency of correlations 
    #                                     for the MHWs is during the onset of events. This is repeated for the 'full' 
    #                                     and 'decline' columns respectively. The 'season' column briefly states the 
    #                                     most clear/notewrothy pattern(s) when looking at how the correlations are 
    #                                     divided up by season. The same is done in the 'region' column. The last column, 
    #                                     'story', gives a TRUE/FALSE if I think the variable has a story to tell. 
    #                                     Something worth pursuing further. Particularly to see if the variables realte 
    #                                     strongly to other variables, not just temperature. This then could provide a 
    #                                     framework for determining 'types' of MHWs (e.g. strong SSS change with 
    #                                     strong latent heat flux).") #%>% 
    #         # kable_styling("striped", full_width = T) #%>%
    #         # add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
    # }

    
    # This automatically ends the session when the app is closed
    session$onSessionEnded(stopApp)
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

