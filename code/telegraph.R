


telData <- reactive({  
  if (is.null(input$DT_text)) return()
  if (input$DT_text=="") return()
 print(input$DT_text)
  
  df <-  DT_data %>% 
    group_by(date) %>% 
    summarize(count = sum(str_count(headlines,input$DT_text)))
  
  info=list(df=df)
  return(info)
  
})
  
  output$headlineChart <- renderPlotly({
    if (is.null(input$DT_text)) return()
    if (input$DT_text=="") return()
    
    
theTitle <- paste0("DT articles with '",input$DT_text,"' in the Headline")

df <- telData()$df  

plot_ly(df,
               x = date,
               y = count,
               
               type = "bar"
               
  ) %>% 
    layout(hovermode = "closest",
           title=theTitle,
           xaxis=list(title=""),
           yaxis=list(title="Article Count")
    )
  
})

## do a plotly for crosstalk

## crosstalk to get to individual chart
cv <- crosstalk::ClientValue$new("plotly_click", group = "A")


output$headlineTable <-DT::renderDataTable({
  s <- cv$get()
  if (length(s)==0) return()
  
  theDate=s[["x"]]
  
 print(theDate)
  
  
 sel <- DT_data %>% 
    filter(date==theDate&str_detect(headlines,input$DT_text)==TRUE) %>% 
   mutate(url = paste0(
     "<a href=\"","http://www.telegraph.co.uk",link,"\" target=\"_blank\">", headlines,"</a>"
   )) %>%
   select(headline=url) %>% 
   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,escape = FALSE,
                 options= list(paging = FALSE, searching = FALSE,info=FALSE))

})


output$selection <- renderPrint({
  s <- cv$get()
  print(s)
  if (length(s) == 0) {
    "Click on a cell in the heatmap to display a scatterplot"
  } else {
    cat("You selected: \n\n")
    as.list(s) # get back x and y values and point numvber which looks like rownumber
  }
})