

## function which updates Guardian search day input
  getDate = function(data,location,session){

    if(is.null(data)) return(NULL)
    
    # need to adjust from milliseconds from origin
    trueDate<- as.Date(as.POSIXct((data$date)/1000, origin='1970-01-01 00:10:00 GMT'))
    updateDateInput(session, "gDate", label = NULL, value = trueDate, min = NULL,
                    max = NULL)
  }

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
  

  

  
 
    Gdata <- reactive({
   
    if (is.null(input$name)){ 

      return()
    }  else {
      theName <- str_replace_all(input$name," ","+")
      #  encapsulate for exact name
      theName <- paste0("%22",theName,"%22")
    }

      # currently look at articles for just one day
    selectedDate <- input$gDate

    results <- get_guardian(theName, 
                            from.date=selectedDate, 
                            to.date=selectedDate, 

                            api.key="3xzg2fk53jcdgaj5tbwqqhcz") # different on github

    info=list(results=results)
  })
  

  # output links to Guardian Articles
    
  output$headlinesDT <- DT::renderDataTable({
  
    if (input$name=="")  return() 
   
    
    if(is.null(Gdata()$results)) return()
    gRes <- Gdata()$results
  
    if (nrow(gRes) == 1&is.na(gRes$id[1])){
      #return()
      DT::datatable(blankdf,rownames=FALSE,escape=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
    } else {
    
    gRes %>% 
      mutate(link=paste0("<a href=\"",webUrl,"\" target=\"_blank\">", webTitle,"</a>")) %>% 
      select(link) %>% 
      DT::datatable(rownames=FALSE,escape=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
    }
      })
  

 # Print the Wikipedia Entry sidebar
  
  output$testvcard <- renderUI({
    
 
    
    url <- data()$wikiURL
    
    vcard <-html(url) %>% 
       html_nodes(".vcard")


    vcardInfo <- vcard[[1]]
  
    HTML(as(vcardInfo,"character"))
    } )
  

})
