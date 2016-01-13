## comparisons of more than one individual
## replicate http://fivethirtyeight.com/datalab/how-media-interest-in-the-gop-candidates-compares-to-public-interest/
## for wiki searches



# Set up reactive values initially as null

# compValues <- reactiveValues()
# 
# 
# 
# ## function which takes point clicked and then gets guardian data
# getCompDate = function(data,location,session) {
#   if (is.null(data))
#     return(NULL)
#  
#   # need to adjust from milliseconds from origin
#   compValues$trueDate <-
#     as.Date(as.POSIXct((data$date) / 1000, origin = '1970-01-01 00:10:00 GMT'))
#   compValues$name <- data$name
#  
# }




compData <- eventReactive(input$goButton2,{
  startDate <- as.character(input$daterange2[1])
  endDate <- as.character(input$daterange2[2])
  
  vals <- unlist(str_split(input$comps,","))
  
  for (i in 1:length(vals)) {
    origName <- str_trim(vals[i])
    theName <- str_replace_all(origName," ","_")
    wikiURL <- paste0("http://en.wikipedia.org/wiki/",theName)
    tempdf <- wp_trend(
      page = theName,
      from = startDate,
      to   = endDate,
      
      lang = c("en")
    )
    tempdf$name <- origName
   
    
    if (i != 1) {
      df <- rbind(df,tempdf)
    } else {
      df <- tempdf
    }
    
  }
  # prob just needed for gggplot which have dispensed with
  df$id <- 1:nrow(df)
  
  print("compdf")
  print(glimpse(df))
  
  info = list(df = df,vals = vals,wikiURL = wikiURL) # prob wont use latter as it gets wiki onfo (unless could just get photo)
  return(info)
  
})
output$compChart <- renderPlotly({
  df <- compData()$df
  
  df <- df %>% 
    ungroup() %>% 
    arrange(date) %>% 
    mutate(logCount=log10(count))
  
  #print(input$comps)
  
  
  theTitle<- input$comps
  
#   print(unique(df$title))
#  print("compdf")
#  print(glimpse(df))
 
 ## issue with lines not showing sequentially - so only offer point initially
#  if (input$chartType == "Point") {
#  theMode <- "markers"
#  } else {
#  theMode <- "lines"
#  }
 
 print(names(df))
 
 if (input$chartScale=="Count") {
  p <- plot_ly(df,
               x = date,
               y = count,
               
               mode = "lines+markers",
               color=name,
               hoverinfo = "text",
               
               text = paste(name,
                 date,
                 "<br>Searches: ",count
                 
               ))  %>% 
    
    
    layout(hovermode = "closest",
           title=theTitle,
           xaxis=list(title=""),
           yaxis=list(title="Article Count")
    )
 } else {
   p <- plot_ly(df,
                x = date,
                y = logCount,
                
                mode = "lines+markers",
                color=name,
                hoverinfo = "text",
                
                text = paste(name,
                             date,
                             "<br>Searches: ",count
                             
                ))  %>% 
     
     
     layout(hovermode = "closest",
            title=theTitle,
            xaxis=list(title=""),
            yaxis=list(title="Article Count (log10)")
     )
 }
  
  p
})



ctComp <- crosstalk::ClientValue$new("plotly_click", group = "A") ##NB have a group="A' in telegraph


output$headlinesComp <- DT::renderDataTable({
  
   print ("enter guardian comps")
  
  s <- ctComp$get()
  
  print(s)
#   curveNumber        pointNumber                  x                  y 
#   "0"               "42"       "2016-01-12" "3.93338604190305" 
  
  if (length(s)==0) return()
  
  theDate=s[["x"]]
  
 
  
  vals <- unlist(str_split(input$comps,","))
  
  print(vals)
 
  i <- as.integer(s[["curveNumber"]])+1
  
  
  theName <-
    str_replace_all(vals[i]," ","+") # this does mean no change until new name is entered but leaves old table up
  #  encapsulate for exact name
  theName <- paste0("%22",theName,"%22") # "%22david+bowie%22"
  
  
  
  results <- get_guardian(
    theName,
    from.date = theDate,
    to.date = theDate,
    
    api.key="3xzg2fk53jcdgaj5tbwqqhcz")
  
  # print(glimpse(results))
  
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


# output$headlineTable <-DT::renderDataTable({
#   s <- cv$get()
#   if (length(s)==0) return()
#   
#   theDate=s[["x"]]
#   
#   #  print(theDate)
#   
#   
#   sel <- DT_data %>% 
#     filter(date==theDate&str_detect(headlines,input$DT_text)==TRUE) %>% 
#     mutate(url = paste0(
#       "<a href=\"","http://www.telegraph.co.uk",link,"\" target=\"_blank\">", headlines,"</a>"
#     )) %>%
#     select(headline=url) %>% 
#     DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,escape = FALSE,
#                   options= list(paging = FALSE, searching = FALSE,info=FALSE))
#   
# })



# produce chart

# #observeEvent(input$goButton2,{ # works but prob is when just want switching between scales
# observe({
#   print("enterchart")
#   if (is.null(input$comps))
#     return()
#   if (is.null(compData()$df))
#     return()
#   
#   df <- data.frame(compData()$df) # trying to get round Warning: Unhandled error in observer: on_click is not a function
#   
#   
#   
#   all_values <- function(x) {
#     if (is.null(x))
#       return(NULL)
#     row <- df[df$id == x$id,c("name","date","count")]
#     paste0(format(row), collapse = "<br />")
#   }
#   
#  
#   if (input$chartType == "Point" & input$chartScale == "Count") {
#     df %>% 
#       ggvis(~date,~count,key:= ~id) %>%
#       layer_points(fill =  ~ name) %>%
#       add_tooltip(all_values, "click") %>%
#       add_axis("x", title = "") %>%
#       add_axis("y", title = "") %>% 
#        add_legend("fill",title="") %>%
#       handle_click(getCompDate) %>%
#       bind_shiny("ggCompChart")
#   } else if (input$chartType == "Point" & input$chartScale == "Log") {
#     df %>% ggvis( ~ date, ~ log10(count),key:= ~ id) %>%
#       layer_points(fill =  ~ name) %>%
#       add_tooltip(all_values, "click") %>%
#       add_axis("x", title = "") %>%
#       add_axis("y", title = "Log10 Count") %>%
#        add_legend("fill",title="") %>%
#     handle_click(getCompDate) %>%
#       bind_shiny("ggCompChart")
#   }else if (input$chartType == "Line" & input$chartScale == "Count") {
#     df %>%
#       arrange(date) %>%
#       ggvis( ~ date, ~ count,stroke =  ~ name) %>%
#       group_by(name) %>% # if not there then all together one line
#       layer_lines() %>%  # back to issue of disappearing
#      
#       add_axis("x", title = "") %>%
#       add_axis("y", title = "") %>%
#        add_legend("stroke",title="") %>%
#      
#       bind_shiny("ggCompChart")
#   } else if (input$chartType == "Line" & input$chartScale == "Log") {
#     df %>%
#       arrange(date) %>%
#       ggvis( ~ date, ~ log10(count),stroke =  ~ name) %>%
#       group_by(name) %>% # if not there then all together one line
#       layer_lines() %>%  # back to issue of disappearing
#       #add_tooltip(all_values, "click") %>%
#       add_axis("x", title = "") %>%
#       add_axis("y", title = "Log10 Count") %>%
#        add_legend("line",title="") %>%
#      
#       bind_shiny("ggCompChart")
#   }
#   
#   
# })

output$compTable <- DT::renderDataTable({
  df <- compData()$df %>%
    filter(count != 0) %>%
    group_by(name) %>%
    summarize(
      max = max(count,na.rm = T),min = min(count,na.rm = T),mean = round(mean(count,na.rm =
                                                                                T),0)
    ) %>%
    ungroup() %>%
    arrange(desc(mean)) %>%
    DT::datatable(options = list(
      paging = FALSE, searching = FALSE,info = FALSE
    ))
})




# output$headlinesComp <- DT::renderDataTable({
#  
#   if (is.null(compValues$trueDate))
#     return()
#   
#   
#   print(compValues$name) # issue when is something like Andrew Clark (priest)
#  
#   betterName <- str_trim(str_split(compValues$name,"[(]")[[1]][1])
#   
#   theName <- str_replace_all(isolate(betterName)," ","+")
#   #   #  encapsulate for exact name
#   theName <- paste0("%22",theName,"%22")
# 
#   results <- get_guardian(
#     theName,
#     from.date = compValues$trueDate,
#     to.date = compValues$trueDate,
#     
#     api.key = "enteryourshere"
#   )
#   
#   
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