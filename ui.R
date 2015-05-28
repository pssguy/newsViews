

shinyUI(fluidPage(

  # Application title
  titlePanel("WikiGuardian",windowTitle="wikiPiki"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      includeMarkdown("about.md"),
      textInput("name","Input data",""), # do not enter value as otherwise titles come up before chart

dateRangeInput("daterange", "Date range:",
               start  = Sys.Date()-60,
               end    = Sys.Date(),
               min    = "2007-12-01",
               max    = Sys.Date(),
               format = "yyyy-mm-dd",
               startview = "year",
               separator = " - "),
      actionButton("goButton", "Go!"),
      uiOutput("testvcard")
    ),

    # Show a plot of the generated distribution
    mainPanel(

      helpText(h4("Daily English Wikipedia Searches - click on point for Guardian coverage")),
      ggvisOutput("ggChart"),
      dateInput("gDate","Guardian Articles", value=Sys.Date(),min = "2007-12-01", max=Sys.Date()),
      DT::dataTableOutput("headlinesDT")
 #     DT::dataTableOutput("temp")

    )
  )
))
