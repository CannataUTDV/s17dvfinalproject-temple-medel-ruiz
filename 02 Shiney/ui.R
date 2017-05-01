#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Barchart tab content.
      tabItem(tabName = "barchart",
        tabsetPanel(
          tabPanel("Data",
                   uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                   actionButton(inputId = "click2",  label = "To get data, click here"),
                   hr(), # Add space after button.
                   DT::dataTableOutput("data2")
          ),
          tabPanel("Barchart", "Black = Sum of Sales per Region, Red = Average Sum of Sales per Category, and  Blue = (Sum of Sales per Region - Average Sum of Sales per Category)", plotOutput("plot2", height=1500))
        )
      )
      # End Barchart tab content.
    )
  )
)

