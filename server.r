# Try using reactive values
values <- reactiveValues()



## function which takes value clisked and the gets guardian data
getDate = function(data,location,session){
  print("enter getDate")
  if(is.null(data)) return(NULL)
  
  # need to adjust from milliseconds from origin
 values$trueDate<- as.Date(as.POSIXct((data$date)/1000, origin='1970-01-01 00:10:00 GMT'))
#   updateDateInput(session, "gDate", label = NULL, value = trueDate, min = NULL,
#                   max = NULL)
  print(values$trueDate)
}
  

  
 # Unhandled error in observer: object 'output' not found if output blah
  #observe({

#}

shinyServer(function(input, output,session) {
  
  ## obtain wiki search results
  data <- eventReactive(input$goButton,{
    if (is.null(input$name)){ 
      return()
    }  else {
      theName <- str_replace_all(input$name," ","_")
      wikiURL <- paste0("http://en.wikipedia.org/wiki/",theName)
    }
    
    startDate <- as.character(input$daterange[1])
    endDate <- as.character(input$daterange[2])
    
    print(endDate)
    df <- wp_trend(page = theName, 
                   from = startDate, 
                   to   = endDate,
                   
                   lang = c("en"))
    
    df$id <- 1:nrow(df)
    
    info=list(df=df,wikiURL=wikiURL)
    return(info)
    
  })
  
  
  # produce chart  
    
  observeEvent(input$goButton,{
    
    
    if(is.null(input$name)) return()
    
    df <- data()$df
    all_values <- function(x) {
      if(is.null(x)) return(NULL)
      row <- df[df$id == x$id,c("date","count") ]
      paste0( format(row), collapse = "<br />")
    }
    
    
    data()$df %>% ggvis(~date,~count,key := ~id) %>%
      layer_points() %>% 
      add_tooltip(all_values, "click") %>% 
      add_axis("x", title="") %>% 
      add_axis("y", title="") %>% 
      handle_click(getDate) %>% 
      bind_shiny("ggChart")
    
  })
  
  
  
  
  
  

#   
  
  # Print the Wikipedia Entry sidebar
  
  output$testvcard <- renderUI({
    
    
    
    url <- data()$wikiURL
 #   print("the url")
    print(url)
    
    test <-http_status(GET(url))
 
    if (test$category=="client error") return()
    
  
    
    vcard <-html(url) %>% 
      html_nodes(".vcard")
    
 
    if(length(vcard)==0) return()
    
    vcardInfo <- vcard[[1]]
    
    HTML(as(vcardInfo,"character"))
  } )

  # Guardian Headlines
  
  output$headlinesDT <- DT::renderDataTable({
    print(" enter headlinesT")
    if(is.null(values$trueDate)) return()
    print(values$trueDate)
    
    theName <- str_replace_all(isolate(input$name)," ","+") # this does mean no change until new name is entered but leaves old table up
    #  encapsulate for exact name
    theName <- paste0("%22",theName,"%22")
    
    results <- get_guardian(theName, 
                            from.date=values$trueDate, 
                            to.date=values$trueDate, 
                            
                            api.key="3xzg2fk53jcdgaj5tbwqqhcz")
    
    #print(glimpse(results))
    # try reassingning values to null 
   # values$trueDate <- NULL
    
    if (nrow(results) == 1&is.na(results$id[1])){
      #return()
      DT::datatable(blankdf,rownames=FALSE,escape=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
    } else {
      
      results %>% 
        mutate(link=paste0("<a href=\"",webUrl,"\" target=\"_blank\">", webTitle,"</a>")) %>% 
        select(link) %>% 
        DT::datatable(rownames=FALSE,escape=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
    }
    
  #  print(glimpse(results))
    
  })
  
})
