


shinyServer(function(input, output,session) {
  output$sb <- renderUI({
    if (input$sbMenu == "guardian") {
      inputPanel(
        id = "ip1",
        textInput("name","Input Subject",""), # do not enter value as otherwise titles come up before chart
        
        dateRangeInput(
          "daterange1", "Date range:",
          start  = Sys.Date() - 60,
          end    = Sys.Date(),
          min    = "2007-12-01",
          max    = Sys.Date(),
          format = "yyyy-mm-dd",
          startview = "year",
          separator = " - "
        ),
        actionButton("goButton1", "Go!")
      )
    } else if (input$sbMenu == "comparisons") {
      inputPanel(
        id = "ip2",
        textInput("comps","Enter selection(s) separated by comma"),# do not enter value as otherwise titles come up before chart
        radioButtons("chartType","Choose Chart",c("Point","Line"), inline =
                       TRUE),
        radioButtons("chartScale","Choose Scale",c("Count","Log"),inline =
                       TRUE),
        
        dateRangeInput(
          "daterange2", "Date range:",
          start  = Sys.Date() - 60,
          end    = Sys.Date(),
          min    = "2007-12-01",
          max    = Sys.Date(),
          format = "yyyy-mm-dd",
          startview = "year",
          separator = " - "
        ),
        actionButton("goButton2", "Go!")
      )
    }
    
  })
  
  
  source("code/comparisons.R", local = TRUE)
  
  source("code/guardian.R", local = TRUE)
  source("code/republican.R", local = TRUE)
  
})
