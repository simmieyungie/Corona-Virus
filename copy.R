#value boxes
frow1 <- fluidRow(
  shinydashboard::valueBoxOutput("value1"),
  shinydashboard::valueBoxOutput("value2"),
  shinydashboard::valueBoxOutput("value3"))

frow3 <- fluidRow(  
  box(
    title = "WordCloud of Words"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("wordcloud", height = "300px")
  ),
  box(
    title = "Top 10 words"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("top10", height = "300px")
  )
)


frow2 <- fluidRow(
  box(
    title = "Top Positive and Negative Words",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("bing", height = "300px")
  ),
  box(
    title = "Sentiment Polarity",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("Polarity", height = "300px")
  )
)

#body of dashboard
body <- dashboardBody(frow1, frow2, frow3)


sidebar <- dashboardSidebar(
  sidebarPanel(style = "background-color: #00005c",
               numericInput("num_tweets_to_download",
                            "Number of tweets to download:",
                            min = 100,
                            max = 18000,
                            value = 200,
                            step = 100), # <- Don't forget comma here
               textInput("hashtag_to_search",
                         "Hashtag to search:",
                         value = "#covid")
               , width = 0.3))


ui <- dashboardPage(header, sidebar, tabbody, skin = "blue",
                    tags$head(
                      tags$style(HTML("
          .main-sidebar {
            background-color: #00005c !important;
          }
        "))))
