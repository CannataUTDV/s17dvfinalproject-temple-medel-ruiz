#ui.R
require(shiny)
require(shinydashboard)
require(DT)
require(leaflet)
require(plotly)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Box Plots", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("Histograms", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Scatter Plots", tabName = "scatter", icon = icon("dashboard")),
      menuItem("Crosstabs, KPIs, Parameters", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabItems(
      # Begin Box Plots tab content.
      tabItem(tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb5", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("boxplotRegions"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Simple Box Plot", 
                         sliderInput("boxSalesRange1", "Sales Range:", # See https://shiny.rstudio.com/articles/sliders.html
                                     min = min(globals$Sales), max = max(globals$Sales), 
                                     value = c(min(globals$Sales), max(globals$Sales))),
                         sliderInput("range5a", "Loop through Quarters:", 
                                     min(globals$Order_Date), 
                                     max(globals$Order_Date) + .75, 
                                     max(globals$Order_Date), 
                                     step = 0.25,
                                     animate=animationOptions(interval=2000, loop=T)),
                         plotlyOutput("boxplotPlot1", height=500))
              )
      ),
      # End Box Plots tab content.
      # Begin Histogram tab content.
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",  
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("Histdata")
                ),
                tabPanel("Histogram", plotlyOutput("Histplot", height=1000))
              )
      ),
      # End Histograms tab content.
      # Begin Scatter Plots tab content.
      tabItem(tabName = "scatter",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("scatterStates"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("scatterData1")
                ),
                tabPanel("Political Change over Turbines built: 2008 - 2014", plotlyOutput("scatterPlot1", height=400),plotlyOutput("scatterPlot2", height=400),plotlyOutput("scatterPlot3", height=400))
                
              )
      ),
      # End Scatter Plots tab content.
      
      tabItem(tabName = "barchart",
              tabsetPanel(
                tabPanel("Data",
                         uiOutput("1"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("Bardata")
                ),
                tabPanel("Barchart","Black = Value, Red = Mean, Blue = Calculated Difference from Average", plotOutput("Barplot", height=4000))
              )
      )
      # Begin Crosstab tab content.
      
    )
  )
)
