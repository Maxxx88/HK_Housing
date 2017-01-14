library(plotly)

shinyUI(fluidPage(

  # Application title
  titlePanel("Is it the right time to buy in HK or not?"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        width = 3,
        checkboxGroupInput("size","Size",
                           c("Less than 400 m²"="40m",
                             "40-69.9 m²"="40-69.9m",
                             "70-99.9 m²"="70-99.9m",
                             "100-159.9 m²"="100-159.9m",
                             "160+ m²"="160"
                           ),
                           selected=c("40m","40-69.9m","70-99.9m","100-159.9m","160")
        ),
        checkboxGroupInput("district","District",
                           c("Hong-Kong Island"="HK",
                             "Kowloon"="KWL",
                             "New Territories"="NT"
                             ),selected=c("HK","KWL","NT")
                           ),
        actionButton("action", label = "Next")
        ),
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Sales Avg transaction", br(), plotlyOutput("plot1"), br("Please note that neither the Size or teh District will have an effect on this graph.")),
                    tabPanel("Sales details", br(), plotlyOutput("plot2"), br("Please not that a Size and a District need to be at least selected to see the graph.")),
                    tabPanel("Stocks", br(), plotlyOutput("plot3"), br("PLease not that at least one Size need to be selected but District will have no effect.")),
                    tabPanel("Rent", br(), plotlyOutput("plot4"), br("Please note that neither the Size or teh District will have an effect on this graph.")),
                    # tabPanel("Rent vs Sales Indice", br(), plotlyOutput("plot6")),
                    tabPanel("Forecast on Stock (temp)", br("Let s analyse the potential forecast for the stock with different model and see which one is the most efficient:"),
                             plotOutput("plotforecast1"),
                             plotOutput("plotforecast2"),
                             plotOutput("plotforecast3"),
                             plotOutput("plotforecast4"),
                             plotOutput("plotforecast5"),
                             plotOutput("plotforecast6"),
                             plotOutput("plotforecast7"),
                             plotOutput("plotforecastResult"),
                             br("Now, we can see than Arima model is a little better so we will keep this one.")),
                    tabPanel("Forecast on sales indice by size", br("Less than 40 m²"), plotOutput("plotforecastsalesSize1"),
                             br("Between 40 and 70 m²"), plotOutput("plotforecastsalesSize2"),
                             br("Between 70 and 100 m²"), plotOutput("plotforecastsalesSize3"),
                             br("Between 100 and 160 m²"), plotOutput("plotforecastsalesSize4"),
                             br("More than 160 m²"), plotOutput("plotforecastsalesSize5")),
                    tabPanel("Forecast for rent", br(), plotOutput("plotforecastrent"))
                    )
    )
  )
)
)
