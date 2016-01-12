dashboardPage(
  skin = "blue",
  dashboardHeader(title = "News Views"),
  
  dashboardSidebar(
    includeCSS("custom.css"),
    uiOutput("sb"),
    
    
    
    
    
    sidebarMenu(
      id = "sbMenu",  
      
      menuItem("Wiki Guardian", tabName = "wikiGuardian",
      menuSubItem(
        "Single Entry with Wikipedia", tabName = "guardian",icon = icon("line-chart")
      ,selected=T),
      menuSubItem(
        "Comparison Chart", tabName = "comparisons",icon = icon("line-chart")
      ),
      menuSubItem(
        "Republican Candidates", tabName = "republican",icon = icon("user")
      ),
      menuSubItem("Info", tabName = "info", icon = icon("info"))
      ),
      menuItem("DT Headlines", tabName = "dtHeadlines"),
      
      
      tags$hr(),
      menuItem(text="",href="https://mytinyshinys.shinyapps.io/dashboard",badgeLabel = "All Dashboards and Trelliscopes (14)"),
      tags$hr(),
      
      tags$body(
        a(class="addpad",href="https://twitter.com/pssGuy", target="_blank",img(src="images/twitterImage25pc.jpg")),
        a(class="addpad2",href="mailto:agcur@rogers.com", img(src="images/email25pc.jpg")),
        a(class="addpad2",href="https://github.com/pssguy",target="_blank",img(src="images/GitHub-Mark30px.png")),
        a(href="https://rpubs.com/pssguy",target="_blank",img(src="images/RPubs25px.png"))
      )
#       menuItem(
#         "Other Dashboards",
#         
#         
#         menuSubItem("Climate",href = "https://mytinyshinys.shinyapps.io/climate"),
#         menuSubItem("Cricket",href = "https://mytinyshinys.shinyapps.io/cricket"),
#         menuSubItem("Mainly Maps",href = "https://mytinyshinys.shinyapps.io/mainlyMaps"),
#         menuSubItem("MLB",href = "https://mytinyshinys.shinyapps.io/mlbCharts"),
#         
#         menuSubItem("World Soccer",href = "https://mytinyshinys.shinyapps.io/worldSoccer")
#         
#       ),
#       menuItem("", icon = icon("twitter-square"),
#                href = "https://twitter.com/pssGuy"),
#       menuItem("", icon = icon("envelope"),
#                href = "mailto:agcur@rogers.com")
      
    )
  ),
  dashboardBody(tabItems(
    tabItem("guardian",
            fluidRow(
              column(
                width = 4,
                box(
                  width = 12, collapsible = TRUE,
                  status = "success", solidHeader = TRUE,
                  title = "Guardian Links",
                  DT::dataTableOutput("headlinesDT")
                  
                ),
                box(
                  width = 12, collapsible = TRUE,
                  status = "success", solidHeader = TRUE,
                  title = "Wikipedia Summary",
                  uiOutput("testvcard")
                )
              ),
              column(
                width = 8,
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  title = "Daily English Wikipedia Searches - click on point for Guardian coverage",
                  #includeMarkdown("about.md")
                  ggvisOutput("ggChart"),
                 plotlyOutput("wikiChart")
                )
              )
            )),
    tabItem("republican",
            fluidRow(
              column(
                width = 6,
                helpText(p(
                  h4(
                    "FiveThirtEight.com recently ran an article comparing ",
                    tags$a(
                      "media and public interest in GOP candidates",
                      href = "http://fivethirtyeight.com/datalab/how-media-interest-in-the-gop-candidates-compares-to-public-interest/",
                      target = "_blank"
                    ),"using Google data"
                  )
                ),
                p(
                  h4(
                    "Here are equivalent results for Wikipedia searches showing actual numbers"
                  )
                )),
                imageOutput("wikiImage"),
                helpText(
                  h4(
                    "The results are not dissimilar, although Cruz tops Trump for most interest and there are subtle variatons elsewhere"
                  )
                ),
                helpText(h4("Summary data shown below")),
                DT::dataTableOutput("repTable")
              ),
              column(width = 6,
                     
                     imageOutput("googleImage"))
            )),
    
    tabItem("comparisons",
            fluidRow(
              column(
                width = 8,
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  title = "Daily English Wikipedia Searches - click on point for Guardian coverage",
                  #includeMarkdown("about.md")
                  #ggvisOutput("ggCompChart"),
                  DT::dataTableOutput("compTable")
                )
                
                
              ),
              column(
                width = 4,box(
                  width = 12, collapsible = TRUE,collapsed = FALSE,
                  status = "success", solidHeader = TRUE,
                  title = "Guardian Links",
                  DT::dataTableOutput("headlinesComp")
                  
                )
                
                
              )
            )),  
    tabItem("dtHeadlines",
            box(
      width = 6, collapsible = TRUE,collapsed = FALSE,
      status = "success", solidHeader = FALSE,
      #title = "Daily Telegraph",
      textInput("DT_text",label = "Enter subject of interest (case is important) for Daily Telegraph 2015 coverage"),
      footer="Pan and Zoom. Click bar for headlines",
      plotlyOutput("headlineChart")
    #  DT::dataTableOutput("headlinesComp")
      
    ),
    box(
      width = 6, collapsible = TRUE,collapsed = FALSE,
      status = "success", solidHeader = FALSE,
      title = "Headlines - click link to view article in separate tab",
      
      DT::dataTableOutput("headlineTable")
     
    )
    ),
    
    tabItem("info", includeMarkdown("info.md"))
    
    
    
  ) # tabItems
  ) # body
  ) # page
  