# Set up reactive values initially as null

values <- reactiveValues()



## function which takes point clicked and then gets guardian data
getDate = function(data,location,session) {
  if (is.null(data))
    return(NULL)
  
  # need to adjust from milliseconds from origin
  values$trueDate <-
    as.Date(as.POSIXct((data$date) / 1000, origin = '1970-01-01 00:10:00 GMT'))
  values$name <- data$name
  ## these seem to work
  vals <- unlist(str_split(input$name,","))
  values$name <- vals[1]
  #print(values$trueDate)
  #print(values$name)
}







## obtain wiki search results
data <- eventReactive(input$goButton1,{
  
  print("guardian is go")
  print(input$name)
  
  if (is.null(input$name)) {
    return()
  }  else {
    vals <- unlist(str_split(input$name,","))
    
    theName <- str_replace_all(vals[1]," ","_")
    wikiURL <- paste0("http://en.wikipedia.org/wiki/",theName)
  }
  
  startDate <- as.character(input$daterange1[1])
  endDate <- as.character(input$daterange1[2])
  
  print(startDate)
  
  
  df <- wp_trend(
    page = theName,
    from = startDate,
    to   = endDate,
    
    lang = c("en")
  )
  
  df$id <- 1:nrow(df)
  
  print(glimpse(df))
  
  info = list(df = df,wikiURL = wikiURL)
  return(info)
  
})


# produce chart

# observeEvent(input$goButton1,{
#   print("enterchart")
#   if (is.null(input$name))
#     return()
#   print("enterchart and should be working")
#   df <- data()$df
#   all_values <- function(x) {
#     if (is.null(x))
#       return(NULL)
#     row <- df[df$id == x$id,c("date","count")]
#     paste0(format(row), collapse = "<br />")
#   }
#   
#   
#   data()$df %>% ggvis( ~ date, ~ count,key:= ~ id) %>%
#     layer_points() %>%
#     add_tooltip(all_values, "click") %>%
#     add_axis("x", title = "") %>%
#     add_axis("y", title = "") %>%
#     handle_click(getDate) %>%
#     bind_shiny("ggChart")
#   
# })


## plotly version



output$wikiChart <- renderPlotly({
  df <- data()$df
  
  
  theTitle<- input$name
  
  ## specific annotations eg Bowie
#   m <- df[which.max(df$count), ]
#   n <- df[df$date=="2016-01-08", ]
#   
#   a <- list()
#   
#   a[[1]] <- list(
#     x = m$date,
#     y = m$count,
#     text = "RIP",
#     xref = "x",
#     yref = "y",
#     showarrow = T,
#     arrowhead = 5,
#     ax = 30,
#     ay = -30
#   )
#   
#   a[[2]] <- list(
#     x = n$date,
#     y = n$count,
#     text = "Blackstar Released",
#     xref = "x",
#     yref = "y",
#     showarrow = T,
#     arrowhead = 5,
#     ax = -30,
#     ay = -30
#   )
#   
#   print(m)
#   print(a)
#   
  
    p <- plot_ly(df,
               x = date,
               y = count,
               
               mode = "markers",
               hoverinfo = "text",
              
               text = paste(
                 date,
                 "<br>Searches: ",count
               
               ))  %>% 
               
  
    layout(hovermode = "closest",
           title=theTitle,
           xaxis=list(title=""),
           yaxis=list(title="Article Count")#,
                     # annotations = a
           )
    
  p
})


## plotly led wiki guardian table

## crosstalk to get to individual chart
ct <- crosstalk::ClientValue$new("plotly_click", group = "A") ##NB have a group="A' in telegraph


output$headlinesDT <- DT::renderDataTable({
  
  print ("enter guardian heads")
  
  s <- ct$get()
  if (length(s)==0) return()
  
  theDate=s[["x"]]
  
  print(theDate) # "2016-01-11" #theDate <- "2016-01-11"
  
  
#   trueDate <-
#     as.Date(as.POSIXct((theDate) / 1000, origin = '1970-01-01 00:10:00 GMT'))
#   print(trueDate)
  
  #   vals <- unlist(str_split(input$name,","))
  #   values$name <- vals[1]
  
   vals <- unlist(str_split("david bowie",","))
 #  values$name <- vals[1]
  
  #   theName <-
  #     str_replace_all(isolate(values$name)," ","+") # this does mean no change until new name is entered but leaves old table up
  #   #  encapsulate for exact name
  #   theName <- paste0("%22",theName,"%22")
   
   
     theName <-
       str_replace_all(vals[1]," ","+") # this does mean no change until new name is entered but leaves old table up
     #  encapsulate for exact name
     theName <- paste0("%22",theName,"%22") # "%22david+bowie%22"
  
 
  
    results <- get_guardian(
      theName,
      from.date = theDate,
      to.date = theDate,
      
      api.key="3xzg2fk53jcdgaj5tbwqqhcz")
    
    print(glimpse(results))
    
    if (nrow(results) == 1 & is.na(results$id[1])) {
      DT::datatable(
        blankdf,rownames = FALSE,escape = FALSE,options = list(
          paging = FALSE, searching = FALSE,info = FALSE
        )
      )
    } else {
      results %>%
        mutate(link = paste0(
          "<a href=\"",webUrl,"\" target=\"_blank\">", webTitle,"</a>"
        )) %>%
        select(link) %>%
        DT::datatable(
          rownames = FALSE,escape = FALSE,options = list(
            paging = FALSE, searching = FALSE,info = FALSE
          )
        )
    }
    
  
  # blankdf is set up in global
#   DT::datatable(
#           blankdf,rownames = FALSE,escape = FALSE,options = list(
#             paging = FALSE, searching = FALSE,info = FALSE
#           )
#   )
  
#   vals <- unlist(str_split(input$name,","))
#   values$name <- vals[1]
#   
#   print("truedate")
#   print(values$trueDate)
#   if (is.null(values$trueDate))
#     return()
#   
#   
#   theName <-
#     str_replace_all(isolate(values$name)," ","+") # this does mean no change until new name is entered but leaves old table up
#   #  encapsulate for exact name
#   theName <- paste0("%22",theName,"%22")
#   
#   results <- get_guardian(
#     theName,
#     from.date = values$trueDate,
#     to.date = values$trueDate,
#     
#     api.key = "enteryourshere"
#   )
#   
#   print(glimpse(results))
#   
#   if (nrow(results) == 1 & is.na(results$id[1])) {
#     DT::datatable(
#       blankdf,rownames = FALSE,escape = FALSE,options = list(
#         paging = FALSE, searching = FALSE,info = FALSE
#       )
#     )
#   } else {
#     results %>%
#       mutate(link = paste0(
#         "<a href=\"",webUrl,"\" target=\"_blank\">", webTitle,"</a>"
#       )) %>%
#       select(link) %>%
#       DT::datatable(
#         rownames = FALSE,escape = FALSE,options = list(
#           paging = FALSE, searching = FALSE,info = FALSE
#         )
#       )
#   }
#   
  
  
})


output$headlineTable <-DT::renderDataTable({
  s <- cv$get()
  if (length(s)==0) return()
  
  theDate=s[["x"]]
  
  #  print(theDate)
  
  
  sel <- DT_data %>% 
    filter(date==theDate&str_detect(headlines,input$DT_text)==TRUE) %>% 
    mutate(url = paste0(
      "<a href=\"","http://www.telegraph.co.uk",link,"\" target=\"_blank\">", headlines,"</a>"
    )) %>%
    select(headline=url) %>% 
    DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,escape = FALSE,
                  options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
})



# Print the Wikipedia Entry sidebar

output$testvcard <- renderUI({
  url <- data()$wikiURL
  
  
  test <- http_status(GET(url))
  
  if (test$category == "client error")
    return()
  
  
  
  vcard <- read_html(url) %>%
    html_nodes(".vcard")
  
  
  if (length(vcard) == 0)
    return()
  
  vcardInfo <- vcard[[1]]
  
  HTML(as(vcardInfo,"character"))
})

# Guardian Headlines old version with ggvis

# output$headlinesDT <- DT::renderDataTable({
#   print("truedate")
#   print(values$trueDate)
#   if (is.null(values$trueDate))
#     return()
#   
#   
#   theName <-
#     str_replace_all(isolate(values$name)," ","+") # this does mean no change until new name is entered but leaves old table up
#   #  encapsulate for exact name
#   theName <- paste0("%22",theName,"%22")
#   
#   results <- get_guardian(
#     theName,
#     from.date = values$trueDate,
#     to.date = values$trueDate,
#     
#     api.key = "enteryourshere"
#   )
#   
#   print(glimpse(results))
#   
#   if (nrow(results) == 1 & is.na(results$id[1])) {
#     DT::datatable(
#       blankdf,rownames = FALSE,escape = FALSE,options = list(
#         paging = FALSE, searching = FALSE,info = FALSE
#       )
#     )
#   } else {
#     results %>%
#       mutate(link = paste0(
#         "<a href=\"",webUrl,"\" target=\"_blank\">", webTitle,"</a>"
#       )) %>%
#       select(link) %>%
#       DT::datatable(
#         rownames = FALSE,escape = FALSE,options = list(
#           paging = FALSE, searching = FALSE,info = FALSE
#         )
#       )
#   }
#   
#   
#   
# })
