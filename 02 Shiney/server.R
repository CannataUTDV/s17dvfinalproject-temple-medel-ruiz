# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)

# The following query is for the select list in the Barcharts tab.


shinyServer(function(input, output) { 
  
  
# Begin Barchart Tab ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="pavilionall/s-17-dv-project-4", type="sql",
        query="select state , county, poptotal
                from ETLOutMidwest
                group by state, county"
      ) # %>% View()
    
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(state) %>% summarize(avgpoptotal = mean(poptotal))
    dplyr::inner_join(tdf, tdf2, by = "state")
    # Analytic SQL would look something like this:
      # select Category, Region, sum_sales, avg(sum_sales) 
      # OVER (PARTITION BY Category ) as window_avg_sales
      # from (select Category, Region, sum(Sales) sum_sales
      #       from SuperStoreOrders
      #      group by Category, Region)
  })
  output$data2 <- renderDataTable({DT::datatable(df2(), rownames = FALSE,
                        extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot2 <- renderPlot({ggplot(df2(), aes(x=county, y=poptotal)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=1, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=1, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~state, ncol=1) +
      coord_flip() + 
      geom_hline(aes(yintercept = round(avgpoptotal)), color="red") 
      #geom_text(aes( -1, window_avg_sales, label = window_avg_sales, vjust = -.5, hjust = -.25), color="red")
  })
  # End Barchart Tab ___________________________________________________________
  
})
