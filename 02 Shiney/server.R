require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)

online0 = TRUE

# Server.R structure:
#   Queries that don’t need to be redone
#   shinyServer
#   widgets
#   tab specific queries and plotting

  

############################### Start shinyServer Function ####################

shinyServer(function(input, output) {   
  
  
  
  # Begin Scatter Plots Tab ------------------------------------------------------------------
  dfsc1 <- eventReactive(input$click3, {
   
      print("Getting from data.world")
      tdf = query(
        data.world(token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmphY29iYnRlbXBsZSIsImlzcyI6ImFnZW50OmphY29iYnRlbXBsZTo6NTlkNDhjOTktNGVhMy00OTNlLTk0OGQtZWNjMDlhODhmMGY1IiwiaWF0IjoxNDkyNDgyMjA1LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.ZbvoSVOGE5N4fj-ANbG7cNoLUKydk1_01IuCIUJtyj_t5nuGPqUYGq_gM7jPnOG6MWV0lpeG2-lSCWgOKHjOVw"),
        dataset="jacobbtemple/finalprojectdata", type="sql",
        query="select `County Name` as County, increase, Density,ElectionWind.`Clinton % B(W) Obama`/ ElectionWind.`% Obama` as Gain, `Non-Hydro Renewable Percent Total Generation`,
        case
        when `% Obama` > `% Romney` and `% Clinton` > `% Trump` then 'Dem hold'
        when `% Obama` > `% Romney` and `% Clinton` < `% Trump` then 'Rep gain'
        when `% Obama` < `% Romney` and `% Clinton` > `% Trump` then 'Dem gain'
        when `% Obama` < `% Romney` and `% Clinton` < `% Trump` then 'Rep hold'
        end as PartyChange
        from ElectionWind where increase > 1 and increase <1000"
      ) # %>% View()
    }
    )
  output$scatterData1 <- renderDataTable({DT::datatable(dfsc1(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  output$scatterPlot1 <- renderPlotly({p <- ggplot(dfsc1()) + 
    theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_point(aes(x=increase, y=Gain, colour=PartyChange), size=2)  
  ggplotly(p)
  })
  output$scatterPlot2 <- renderPlotly({p <- ggplot(dfsc1()) + 
    theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_point(aes(x=increase, y=Density, colour=PartyChange), size=2) 
  ggplotly(p)
  })
  output$scatterPlot3 <- renderPlotly({p <- ggplot(dfsc1()) + 
    theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_point(aes(x=increase, y=`Non-Hydro Renewable Percent Total Generation`, colour=PartyChange), size=2)
  ggplotly(p)
  })
  # End Scatter Plots Tab ___________________________________________________________
  
  # End Barchart Tab ___________________________________________________________
  
  })
