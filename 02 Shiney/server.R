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
  # Begin Box Plot Alv
  dfbox <- eventReactive(input$click5,{
    print("Getting from data.world")
    tdfbox = query(
      data.world(token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmphY29iYnRlbXBsZSIsImlzcyI6ImFnZW50OmphY29iYnRlbXBsZTo6NTlkNDhjOTktNGVhMy00OTNlLTk0OGQtZWNjMDlhODhmMGY1IiwiaWF0IjoxNDkyNDgyMjA1LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.ZbvoSVOGE5N4fj-ANbG7cNoLUKydk1_01IuCIUJtyj_t5nuGPqUYGq_gM7jPnOG6MWV0lpeG2-lSCWgOKHjOVw"),
      dataset="jacobbtemple/finalprojectdata", type="sql",
      query = "select * from energyByStateClean e join electricityJoined j on e.Abbreviation = j.State"
    )
  }
  )
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbox(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
  # Begin Scatter Plots Tab ------------------------------------------------------------------
   dfsc1 <- eventReactive(input$click3, {
   
      print("Getting from data.world")
      tdf = query(
        data.world(token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmphY29iYnRlbXBsZSIsImlzcyI6ImFnZW50OmphY29iYnRlbXBsZTo6NTlkNDhjOTktNGVhMy00OTNlLTk0OGQtZWNjMDlhODhmMGY1IiwiaWF0IjoxNDkyNDgyMjA1LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.ZbvoSVOGE5N4fj-ANbG7cNoLUKydk1_01IuCIUJtyj_t5nuGPqUYGq_gM7jPnOG6MWV0lpeG2-lSCWgOKHjOVw"),
        dataset="jacobbtemple/finalprojectdata", type="sql",
        query="select e.`County Name` as County, c.increase, e.Density,e.`Clinton % B(W) Obama`/ e.`% Obama` as Gain, s.`Non-Hydro Renewable Percent Total Generation`,
        case
        when e.`% Obama` > e.`% Romney` and e.`% Clinton` > e.`% Trump` then 'Dem hold'
        when e.`% Obama` > e.`% Romney` and e.`% Clinton` < e.`% Trump` then 'Rep gain'
        when e.`% Obama` < e.`% Romney` and e.`% Clinton` > e.`% Trump` then 'Dem gain'
        when e.`% Obama` < e.`% Romney` and e.`% Clinton` < e.`% Trump` then 'Rep hold'
        end as PartyChange,
        case
        when e.`Clinton % B(W) Obama`/ e.`% Obama` > -.193 then 'Above Avergae'
        else 'Below Average'
        end as AvgGain,

        case
        when e.Density > 272.38 then 'Above Average'
        else 'Below Average'
        end as AvgDens,

        case
        when s.`Non-Hydro Renewable Percent Total Generation`>.04 then 'Above Average'
        else 'Below Average'
        end as AvgRen
        from CleanedElection e 
        LEFT OUTER JOIN CleanedCounty c
        ON e.`County Name` = c.county and e.`State Code` = c.state
        LEFT OUTER JOIN CleanedEnergy s 
        ON s.Abbreviation = e.`State Code` 
        where increase > 1"
      ) 
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
    geom_point(aes(x=increase, y=Gain, colour=PartyChange, shape = AvgGain), size=2)  
  ggplotly(p)
  })
  output$scatterPlot2 <- renderPlotly({p <- ggplot(dfsc1()) + 
    theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_point(aes(x=increase, y=Density, colour=PartyChange, shape = AvgDens), size=2) 
  ggplotly(p)
  })
  output$scatterPlot3 <- renderPlotly({p <- ggplot(dfsc1()) + 
    theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_point(aes(x=increase, y=`Non-Hydro Renewable Percent Total Generation`, colour=PartyChange, shape = AvgRen), size=2)
  ggplotly(p)
  })
  output$plot2 <- renderPlot({
    brush = brushOpts(id="plot_brush", delayType = "throttle", delay = 30)
    bdf=brushedPoints(df, input$plot_brush)
    View(bdf)
    if( !is.null(input$plot_brush) ) {
      df %>% dplyr::filter(x %in% bdf[, "x"]) %>% ggplot() + geom_point(aes(x=x, y=y, colour=z, size=4)) + guides(size=FALSE)
    } 
  })
  # End Scatter Plots Tab ___________________________________________________________
  
  #Barchart 
  dfBar <- eventReactive(input$click1, {
    print("Getting from data.world")
    tdf = query(
      data.world(propsfile = "www/.data.world"),
      dataset="pavilionall/s-17-dv-project-6", type="sql",
      query="select `energyByStateClean.csv/energyByStateClean`.Region as Region,`censusMedianHousingExpenses.csv/censusMedianHousingExpenses`.State as State, sum(`energyByStateClean.csv/energyByStateClean`.`Total RE Generation (MWh)` / `censusMedianHousingExpenses.csv/censusMedianHousingExpenses`.medianMonthlyHousingCost) as `Total RE Generation per Housing Expense Dollar`
      from `energyByStateClean.csv/energyByStateClean` e left join `censusMedianHousingExpenses.csv/censusMedianHousingExpenses` h on e.Abbreviation = h.State where `censusMedianHousingExpenses.csv/censusMedianHousingExpenses`.medianMonthlyHousingCost >=750 group by Region, e.State order by Region, `censusMedianHousingExpenses.csv/censusMedianHousingExpenses`.State
      
      "
    ) 
    
    
    tdf2 = tdf %>% group_by(Region) %>% summarize(avgREperDollar = mean(`Total RE Generation per Housing Expense Dollar`))
    dplyr::inner_join(tdf, tdf2, by = "Region")
    
  })
  output$Bardata <- renderDataTable({DT::datatable(dfBar(), rownames = FALSE,
                                                   extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$Barplot <- renderPlot({ggplot(dfBar(), aes(x=State, y=`Total RE Generation per Housing Expense Dollar`)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=8, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~Region, ncol=1) + 
      coord_flip() +
      geom_text(mapping=aes(x=State, y=`Total RE Generation per Housing Expense Dollar`, label=`Total RE Generation per Housing Expense Dollar`),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=State, y=`Total RE Generation per Housing Expense Dollar`, label=`Total RE Generation per Housing Expense Dollar` - avgREperDollar),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = avgREperDollar), color="red") +
      geom_text(aes( -1, avgREperDollar, label = avgREperDollar, vjust = -.5, hjust = -.25), color="red")
  })
  
  # End Barchart Tab ___________________________________________________________
  
  })
