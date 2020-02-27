# shiny/app.R
# The purpose of this app is to facilitate the data exploration process
# There are a lot of panels that need to be looked at and it's easier to
# do so via a reactive environment

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(shiny)


# Data --------------------------------------------------------------------

ALL_cor <- readRDS("../data/ALL_cor.Rda")


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Correlation between SST and other variable anomalies during MHWs"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "vars",
                        label = "Variables:",
                        choices = unique(ALL_cor$var), 
                        multiple = TRUE,
                        selected = unique(ALL_cor$var)),
            selectInput(inputId = "regions",
                        label = "Regions:",
                        choices = unique(ALL_cor$region), 
                        multiple = TRUE,
                        selected = unique(ALL_cor$region)),
            selectInput(inputId = "ts",
                        label = "Time series portion:",
                        choices = unique(ALL_cor$ts), 
                        multiple = TRUE,
                        selected = unique(ALL_cor$ts)),
            sliderInput(inputId = "bins",
                        label = "Number of bins:",
                        min = 1,
                        max = 20,
                        value = 10),
            radioButtons(inputId = "position",
                         label = "Position:", 
                         choices = c("stack", "dodge"),
                         selected = "stack"),
            radioButtons(inputId = "fill",
                         label = "grouping:", 
                         choices = c("region", "none"),
                         selected = "none"),
            sliderInput(inputId = "duration",
                        label = "Min Duraion:",
                        min = 1,
                        max = max(ALL_cor$n),
                        value = c(1, max(ALL_cor$n))),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        # Filter data
        ALL_cor_sub <- ALL_cor %>% 
            filter(var %in% input$vars,
                   region %in% input$regions,
                   ts %in% input$ts,
                   n >= input$duration[1],
                   n <= input$duration[2]) %>% 
            na.omit()
        
        # Create histogram
        if(input$fill == "region"){
            ggplot(ALL_cor_sub, aes(x = r)) +
                geom_histogram(aes(fill = region), bins = input$bins, position = input$position) +
                facet_grid(ts ~ var)  
        } else{
            ggplot(ALL_cor_sub, aes(x = r)) +
                geom_histogram(bins = input$bins, position = input$position) +
                facet_grid(ts ~ var)
        }

    })
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

