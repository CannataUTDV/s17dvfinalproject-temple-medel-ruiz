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
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
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
  # crosstab
  dfct1 <- eventReactive(input$click1, {
      
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="jacobbtemple/finalprojectdata", type="sql",
        query="select e.`County Name` as County, r.State, r.WindRatio, r.NucRatio, r.BioRatio, r.SolRatio, r.GeoRatio, r.HydroRatio, s.`Renewable Percent Total Generation`,
      
        case
        when CleanedElection.`% Trump` < .95 and CleanedElection.`% Trump` >= .90 then '.90'
        when CleanedElection.`% Trump` < .90 and CleanedElection.`% Trump` >= .85 then '.85'
        when CleanedElection.`% Trump` < .85 and CleanedElection.`% Trump` >= .80 then '.80'
        when CleanedElection.`% Trump` < .80 and CleanedElection.`% Trump` >= .75 then '.75'
        when CleanedElection.`% Trump` < .75 and CleanedElection.`% Trump` >= .70 then '.70'
        when CleanedElection.`% Trump` < .70 and CleanedElection.`% Trump` >= .65 then '.65'
        when CleanedElection.`% Trump` < .65 and CleanedElection.`% Trump` >= .60 then '.60'
        when CleanedElection.`% Trump` < .60 and CleanedElection.`% Trump` >= .55 then '.55'
        when CleanedElection.`% Trump` < .55 and CleanedElection.`% Trump` >= .50 then '.50'
        when CleanedElection.`% Trump` < .5  and CleanedElection.`% Trump` >= .45 then '.45'
        when CleanedElection.`% Trump` < .45 and CleanedElection.`% Trump` >= .40 then '.40'
        when CleanedElection.`% Trump` < .40 and CleanedElection.`% Trump` >= .35 then '.35'
        when CleanedElection.`% Trump` < .35 and CleanedElection.`% Trump` >= .30 then '.30'
        when CleanedElection.`% Trump` < .30 and CleanedElection.`% Trump` >= .25 then '.25'
        when CleanedElection.`% Trump` < .25 and CleanedElection.`% Trump` >= .20 then '.20'
        when CleanedElection.`% Trump` < .20 and CleanedElection.`% Trump` >= .15 then '.15'
        when CleanedElection.`% Trump` < .15 and CleanedElection.`% Trump` >= .10 then '.10'
        when CleanedElection.`% Trump` < .10 and CleanedElection.`% Trump` >= .05 then '.05'
        when CleanedElection.`% Trump` < .05 then '.00'
        end as TrumpPercentage,
        
        case when CleanedElection.`% Trump` >= 50 then 'Trump'
        when CleanedElection.`% Trump` < 50 then 'Clinton' 
        end as winner,
        case
        When r.WindRatio > r.BioRatio and r.WindRatio >  r.SolRatio and  r.WindRatio > r.GeoRatio and  r.WindRatio > r.HydroRatio  and  r.WindRatio > r.NucRatio then 'Wind'
        When  r.BioRatio  >  r.WindRatio  and  r.BioRatio > r.SolRatio  and  r.BioRatio > r.GeoRatio  and  r.BioRatio  >  r.HydroRatio  and  r.BioRatio  >  r.NucRatio  then 'Biomass'
        When  r.SolRatio  >  r.WindRatio  and  r.SolRatio > r.BioRatio  and  r.SolRatio > r.GeoRatio  and  r.SolRatio  >  r.HydroRatio  and  r.SolRatio  >  r.NucRatio  then 'Solar'
        When  r.GeoRatio  >  r.WindRatio  and  r.GeoRatio > r.SolRatio  and  r.GeoRatio > r.BioRatio  and  r.GeoRatio  >  r.HydroRatio  and  r.GeoRatio  >  r.NucRatio  then 'Geothermal'
        When  r.HydroRatio  >  r.WindRatio  and  r.HydroRatio > r.SolRatio  and  r.HydroRatio > r.GeoRatio  and  r.HydroRatio  >  r.BioRatio  and  r.HydroRatio  >  r.NucRatio  then 'Hydro'
        else 'Nuclear'
        end as RelativelargestRenewable,
        case
        when CleanedEnergy.`Renewable Percent Total Generation` < ? then '03 Low'
        when CleanedEnergy.`Renewable Percent Total Generation` < ? then '02 Medium'
        else '01 High'
        end as kpi
        
        from CleanedElection e
        join CleanedEnergy s 
        ON s.Abbreviation = e.`State Code`
        join CleanedRatios r
        ON r.State = s.State
        ",
        queryParameters = list(KPI_Low(), KPI_Medium())
      ) # %>% View()
    
  })
  
  output$data1 <- renderDataTable({DT::datatable(dfct1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(dfct1()) + 
      theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_tile(aes(x=RelativelargestRenewable, y=TrumpPercentage, fill=kpi), alpha=0.50)
   
      
  })
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
  
  #Histogram
  dfHist <- eventReactive(input$click4, {
    print("Getting from data.world")
    tdf = query(
      data.world(propsfile = "www/.data.world"),
      dataset="pavilionall/s-17-dv-project-6", type="sql",
      query="select h.State as State, (`energyByStateClean.csv/energyByStateClean`.`Total Generation (MWh)` / `censusMedianHousingExpenses.csv/censusMedianHousingExpenses`.medianMonthlyHousingCost) as `Total Generation per Housing Expense Dollar`, `censusMedianHousingExpenses.csv/censusMedianHousingExpenses`.medianMonthlyHousingCost as `Median Monthly Housing Cost`
from `energyByStateClean.csv/energyByStateClean` e left join `censusMedianHousingExpenses.csv/censusMedianHousingExpenses` h on e.Abbreviation = h.State order by h.State
  "
    ) 
    
    
    
    
  })
  output$Histdata <- renderDataTable({DT::datatable(dfHist(), rownames = FALSE,
                                                   extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$Histplot <- renderPlotly({p <- ggplot(dfHist()) +
    geom_histogram(aes(x=`Total Generation per Housing Expense Dollar`)) +
    theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
  ggplotly(p)
  })
  
  #End Histogram
  
  })
