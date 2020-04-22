# shiny/app.R
# The purpose of this app is to facilitate the data exploration process
# There are a lot of panels that need to be looked at and it's easier to
# do so via a reactive environment

# Libraries ---------------------------------------------------------------

library(tidyverse) # Change this to only the required dependencies for easier publishing
library(shiny)
# setwd("shiny")


# Data --------------------------------------------------------------------

ALL_cor <- readRDS("ALL_cor.Rda")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Correlation between SST and other variable anomalies during MHWs"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
            selectInput(inputId = "vars",
                        label = "Variables:",
                        choices = unique(ALL_cor$var), 
                        multiple = TRUE,
                        selected = c("msnlwrf_mld", "mslhf_mld", 
                                     "msshf_mld", "msnswrf_mld", "qnet_mld")),
            selectInput(inputId = "regions",
                        label = "Regions:",
                        choices = unique(ALL_cor$region), 
                        multiple = TRUE,
                        selected = unique(ALL_cor$region)),
            selectInput(inputId = "seasons",
                        label = "Seasons:",
                        choices = unique(ALL_cor$season), 
                        multiple = TRUE,
                        selected = unique(ALL_cor$season)),
            selectInput(inputId = "ts",
                        label = "Time series portion:",
                        choices = unique(ALL_cor$ts), 
                        multiple = TRUE,
                        selected = unique(ALL_cor$ts)),
            radioButtons(inputId = "figure",
                         label = "Figure:", 
                         choices = c("histogram", "box_plot", "linear_model"),
                         selected = "histogram",
                         inline = T),
            radioButtons(inputId = "fill",
                         label = "Grouping:", 
                         choices = c("region", "season", "none"),
                         selected = "none",
                         inline = T),
            sliderInput(inputId = "duration",
                        label = "Min Duraion:",
                        min = 1,
                        max = max(ALL_cor$n),
                        value = c(1, max(ALL_cor$n))),
            h4("Histogram controls:"),
            radioButtons(inputId = "position",
                         label = "Position:", 
                         choices = c("stack", "dodge"),
                         selected = "stack",
                         inline = T),
            sliderInput(inputId = "bins",
                        label = "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 9,
           plotOutput("distPlot")
        )
    )
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        req(input$fill)
        req(input$ts)
        
        # Filter data
        ALL_cor_sub <- ALL_cor %>% 
            filter(var %in% input$vars,
                   region %in% input$regions,
                   ts %in% input$ts,
                   season %in% input$seasons,
                   n >= input$duration[1],
                   n <= input$duration[2]) %>% 
            na.omit()
        
        # Create histogram
        if(input$figure == "histogram"){
            if(input$fill != "none"){
                ggplot(ALL_cor_sub, aes(x = r)) +
                    geom_histogram(aes_string(fill = input$fill), bins = input$bins, position = input$position) +
                    facet_grid(ts ~ var)
            } else {
                ggplot(ALL_cor_sub, aes(x = r)) +
                    geom_histogram(bins = input$bins, position = input$position) +
                    facet_grid(ts ~ var)
            }
        } else if(input$figure == "box_plot"){
            if(input$fill != "none"){
                ggplot(ALL_cor_sub, aes(x = ts, y = r)) +
                    geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
                    geom_boxplot(aes_string(fill = input$fill)) +
                    facet_wrap(~var)
            } else {
                ggplot(ALL_cor_sub, aes(x = ts, y = r)) +
                    geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
                    geom_boxplot() +
                    facet_wrap(~var)
            } 
        } else if(input$figure == "linear_model"){
            if(input$fill != "none"){
                ggplot(ALL_cor_sub, aes(x = n, y = r)) +
                    geom_point(aes_string(colour = input$fill)) +
                    geom_smooth(aes_string(colour = input$fill, linetype = "ts"), method = "lm", se = F) +
                    facet_wrap(~var)
            } else {
                ggplot(ALL_cor_sub, aes(x = n, y = r)) +
                    geom_point() +
                    geom_smooth(aes(linetype = ts), method = "lm", se = F) +
                    facet_wrap(~var)
            }
        }
    })
    session$onSessionEnded(stopApp)
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

