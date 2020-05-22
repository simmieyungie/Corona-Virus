#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tm)
library(rsconnect)   
library(base64enc)
library(shiny)
library(rtweet)
library(reactable)
library(scales)
library(reshape2)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(shinydashboard)
library(twitteR)
library(ROAuth)
library(plotly)
library(glue)
library(twitteR)
library(rvest)
library(wordcloud2)
library(textdata)

#value boxes


ui <- dashboardPage(
    dashboardHeader(title = h4(HTML("Twitter Covid'19 <br/>Sentiment Analysis")), titleWidth = 230,
                    disable = FALSE),
    dashboardSidebar(
        sidebarPanel(
                     h5(style="color:#cc4c02", sliderInput("Tweets_to_Download",
                                  "No of Tweets to Download:",
                                  min = 500,
                                  max = 18000,
                                  value = 500,
                                  step = 500)),
                    h6(style = "color:#006d2c", selectInput("Input_Hashtag", "Hashtag to search:", c("#corona",
                                                                          "#covid",
                                                                          "#covid19",
                                                                          "#coronavirus",
                                                                          "#CovidKE")))
                     , width = 0.3)
      ),

    
    dashboardBody(
      # Also add some custom CSS to make the title background area the same
      # color as the rest of the header.
      tags$head(tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
        .main-sidebar {
            background-color: skinblue !important;
          }
  
          
        "))),
        tabsetPanel(
            tabPanel(title = "Sentiment Analysis",
        fluidRow(
                valueBoxOutput("value1"),
                valueBoxOutput("value2"),
                valueBoxOutput("value3")),            
        fluidRow(  
                box(
                    title = "WordCloud"
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE
                    ,wordcloud2Output("wordcloud", height = "300px")
                ),
                box(
                    title = "Top 10 words"
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE 
                    ,plotOutput("top10", height = "300px")
                )),
            fluidRow(
                box(
                    title = "Top Positive and Negative Words",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("bing", height = "300px")
                ),
                box(
                    title = "NRC Sentiments",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("NRC", height = "300px")
                )),
          fluidRow(
            box(
              title = "Sentiment Polarity",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("Polarity", height = "300px")
              
            )),
            ),
           tabPanel(title = "Tweet Table",
                     fluidRow(
                         valueBoxOutput("value4"),
                         valueBoxOutput("value5"),
                         valueBoxOutput("value6")),
                     fluidRow(
                       valueBoxOutput("value7"),
                       valueBoxOutput("value8"),
                       valueBoxOutput("value9")),
                     fluidRow(reactableOutput("tweet_table"))),
           tabPanel(title = "About App",
                    tags$div( id = 'ci_intel_by_hs_hstable' ,
                              fluidRow( h3( style="color:#cc4c02",HTML("<b>Twitter Sentiment Analysis Shiny App</b>")),
                                        h5(HTML("Using Twittter API, this Dashboard collects recent tweets.
                                          The Number of Tweets and preferred hashtag can be used to retrieve tweets with a range between
                                          500 & 18000 tweets at a time. There are also a range of preferred hashtags that can be used to
                                          guide the search. The tweet table presented using reactable, contains options to search either 
                                                a specific column or the entire table. </br>
                                            The Sentiment Polarity Graph, shows the extreme positivity or negativity of all tweets collected. 
                                                An interactive plot, that presents the guiding statement, tweet author and link to the page of twitter itself.
                                                The <b>Tweet table</b> contains the details
                                                of each tweet and <b>>></b> is the tweet link")))
                    ))
    )))






# Define server logic 
server <- function(input, output) {
    #Save app credentials for access to tweets
    consumerKey <- "TLwcYDEblVFgtDIqeqGXY05zH"
    
    consumerSecret <- "Wpy8VEH6eGliqisMCNonqC7q9Z3YrxpmjxkiIyMdSj8qsocdSw"
    
    accessToken <- "1097552545411678208-zIYNJm6l1aOwxUwCtLbgbpmjGciJtp"
    
    accessTokenSecret <-  "d3V7AQ2BSkcj5qCZIjuSeuOPpsfUfM3eRb1YyMmlLdoPG"
   
    #set up 
    setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
    
    dataInput <- reactive({
       data <- searchTwitter(input$Input_Hashtag, n = input$Tweets_to_Download, 
                       resultType = "recent", lang = "en")
        
         twListToDF(data)
        })
    
    #Create reactive word cloud
    output$wordcloud <- renderWordcloud2({
        ##Word clouds for all tweets
        table_1 <- dataInput() %>% 
            mutate(rowmumber = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = removeNumbers(text)) %>% 
            mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word, sort = T) 
        
        wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                                 fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                                 minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                                 rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                                 widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
        {
          if ("table" %in% class(data)) {
            dataOut = data.frame(name = names(data), freq = as.vector(data))
          }
          else {
            data = as.data.frame(data)
            dataOut = data[, 1:2]
            names(dataOut) = c("name", "freq")
          }
          if (!is.null(figPath)) {
            if (!file.exists(figPath)) {
              stop("cannot find fig in the figPath")
            }
            spPath = strsplit(figPath, "\\.")[[1]]
            len = length(spPath)
            figClass = spPath[len]
            if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
              stop("file should be a jpeg, jpg, png, bmp or gif file!")
            }
            base64 = base64enc::base64encode(figPath)
            base64 = paste0("data:image/", figClass, ";base64,", 
                            base64)
          }
          else {
            base64 = NULL
          }
          weightFactor = size * 180/max(dataOut$freq)
          settings <- list(word = dataOut$name, freq = dataOut$freq, 
                           fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                           minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                           gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                           shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                           ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
          chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                            width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                                    browser.padding = 0, browser.fill = TRUE))
          chart
        }
        
        wordcloud2a(table_1, size = 0.75, shape = "circle", ellipticity = 0.65)
    })

    #Build value box
    output$value1 <- renderValueBox({
        n <- dataInput() %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            group_by(word, sentiment) %>% 
            count(word, sentiment, sort = T) %>% 
            ungroup() %>% 
            group_by(sentiment) %>% 
            summarise(n = sum(n)) %>% 
            mutate(n = round(n/sum(n), 2)) %>% 
            filter(sentiment == "positive")
        
        n <- n[,2]
        
        
        valueBox(paste(n, "%"), subtitle = "Positive Tweets", 
                                 icon = icon("smile", lib ="font-awesome" ), color = "aqua")
    })
    
    output$value2 <- renderValueBox({
        n <- dataInput()[,1:16] %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            group_by(word, sentiment) %>% 
            count(word, sentiment, sort = T) %>% 
            ungroup() %>% 
            group_by(sentiment) %>% 
            summarise(n = sum(n)) %>% 
            mutate(n = round(n/sum(n), 2)) %>% 
            filter(sentiment == "negative")
        
        n <- n[,2]
        
        
        valueBox(paste(n, "%"), subtitle = "Negative Tweets", 
                                 icon = icon("angry", lib ="font-awesome" ), color = "green")
    })
    output$top10 <- renderPlot({
        topwords <-  dataInput()[,1:16] %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = gsub("covid", "", text)) %>% 
            mutate(text = removeNumbers(text)) %>% 
            mutate(text = gsub("19", "", text)) %>% 
            mutate(text = gsub("ppl", "people", text)) %>% 
            mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
            mutate(text = gsub("en", "", text)) %>% 
            mutate(rowmumber = row_number()) %>%#mutate row numbers
            mutate(text = str_remove(text, "rt")) %>% 
            unnest_tokens(word, text) %>%  #unnest words
            anti_join(stop_words) %>% #removes stop words
            count(word, sort = T) %>%#count most occuring words
            top_n(10) #select top 10
        
        ggplot(topwords, aes(reorder(word, n), n, fill = word)) + #piped into ggplot
            geom_bar(stat = "identity", show.legend = F) + coord_flip() +
            labs(x = "Word", y = "count") + theme_minimal() +
          theme(axis.title.x = element_text(face ="bold", size = 15),
                axis.title.y = element_text(face = "bold", size = 15),
                axis.text = element_text(face = "bold"))
    })
    
    
    output$value3 <- renderValueBox({
        
        tweets_count <- dataInput() %>% 
            nrow()
        
        
        valueBox(tweets_count, subtitle = "Total Tweets", 
                                 icon = icon("chart-bar", lib ="font-awesome" ), color = "orange")
    })
    
    output$bing <- renderPlot({
        pos_vs_neg <- dataInput()[,1:16] %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("fidelity", " ", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            group_by(word, sentiment) %>% 
            count(word, sentiment, sort = T) %>% 
            ungroup() %>% 
            group_by(sentiment) %>% 
            top_n(10)
            
        ggplot(pos_vs_neg, aes(reorder(word, n), n, fill = word)) +
            geom_col(show.legend = F) +
            facet_wrap(~sentiment, scales = "free_y") +
            coord_flip() + 
            labs(y = "Count", x = "Words") +
            theme_bw()
        
        
    })
    
    output$Polarity <- renderPlotly({
        data_format <- dataInput() %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(row_id = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("brt", "", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            count(sentiment, row_id, screenName) %>% 
            spread(sentiment, n, fill = 0) %>%  # made data wide rather than narrow
            mutate(sentiment = positive - negative)
        
        All_banks <- dataInput() %>%
            mutate(row_id = row_number())
        
        data_format <- data_format %>% 
            left_join(All_banks, by = "row_id")
        
        label_wrap <- label_wrap_gen(width = 60)
        
        data_format2 <- data_format %>% 
            rename(screenName = screenName.x) %>% 
            select(-screenName.y) %>% 
            mutate(text_formatted = str_glue("Row ID: {row_id}
                                   Screen Name: {screenName}
                                   Text: {label_wrap(text)} "))
        
        data_format3<- data_format2 %>% 
            select(1:5, "text_formatted")
        
        
        ggplotly(data_format3 %>% 
                      ggplot(aes(row_id, sentiment)) +
                      geom_line(color= "black", alpha = 0.5) +
                      geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
                      geom_hline(aes(yintercept = mean(sentiment), color = "blue")) +
                      geom_point(aes(text = text_formatted), color = "orange", shape = 5) +
                      geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "orange") +
                      geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "orange") +
                      theme_bw() +
                      labs(y = "sentiment score", x = "Twitter User"),
                  tooltip = "text") %>% 
            layout(
                xaxis = list(
                    rangeslider = list(type = "date")
                )
            )
    })
    
    #Build value box for total global infections
    output$value4 <- renderValueBox({
        #Get the death rates
        x <- "https://www.worldometers.info/coronavirus/"
        
        #total global infections
      global <-  read_html(x) %>% 
            html_node("#maincounter-wrap:nth-child(7) span") %>% 
            html_text()
        
       valueBox(paste(global), subtitle = "Total Number of Global Cases", 
                                 icon = icon("chart-line", lib ="font-awesome" ), color = "red")
    })
    
    #build value box for number of deaths
    #total deaths
    output$value5 <- renderValueBox({
        x <- "https://www.worldometers.info/coronavirus/"
        
    deaths <- read_html(x) %>% 
        html_node("#maincounter-wrap:nth-child(9) span") %>% 
        html_text()
        valueBox(deaths, subtitle = "Total Number of Deaths",
                                 icon = icon("chart-bar", lib = "font-awesome"),
                                 color = "olive")
    })
    
    #Value box for total recoveries world wide
    output$value6 <- renderValueBox({
        
        x <- "https://www.worldometers.info/coronavirus/"
        
        #total recoveries
        recovery <- read_html(x) %>% 
            html_node("#maincounter-wrap+ #maincounter-wrap span") %>% 
            html_text()
        
        valueBox(recovery, subtitle = "Total Number of Recoveries",
                                 icon = icon("stats", lib = "glyphicon"),
                                 color = "aqua")
    })
    
    make_url_html <- function(url) {
        if(length(url) < 2) {
            if(!is.na(url)) {
                as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
            } else {
                ""
            }
        } else {
            paste0(map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
        }
    }
    
    tweet_table_data <- reactive({
        req(dataInput())
        dataInput() %>% 
            rename(user_id = id,
                   created_at = created,
                   screen_name= screenName,
                   favorite_count = favoriteCount,
                   retweet_count = retweetCount,
                   urls_expanded_url = statusSource) %>% 
                    mutate(status_id = user_id) %>% 
            select(user_id, created_at, status_id, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
            mutate(
                Tweet = glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
                URLs = map_chr(urls_expanded_url, make_url_html)
            )%>%
            select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
    })
    
    output$tweet_table <- renderReactable({
          reactable(tweet_table_data(), 
                             filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                             showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10, 50, 75, 100, 200), 
                             columns = list(
                                 DateTime = colDef(defaultSortOrder = "asc"),
                                 User = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                 Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                 RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                                 URLs = colDef(html = TRUE)
                             )
        )
    })
    #Build the NRC sentiment for tweets
    output$NRC <- renderPlot({
          
        nr <- read.csv("nrc.csv")  
        #nrc tweet analysis
        nrc <- dataInput() %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(nr) %>% 
            group_by(sentiment) %>% 
            count(sentiment, sort = T)

        ggplot(nrc, aes(reorder(sentiment, n), n, fill = sentiment)) + 
            geom_bar(stat = "identity", show.legend = F) + coord_flip() +
            theme_minimal() + labs(x = "Sentiments", y = "n") +
            theme(axis.title.x = element_text(face ="bold", size = 15),
                axis.title.y = element_text(face = "bold", size = 15),
                axis.text = element_text(face = "bold"))
    })
    
    output$value7 <- renderValueBox({
      
      y <- "https://www.worldometers.info/coronavirus/coronavirus-death-toll/"
      
      total_change <- read_html(y) %>% 
        html_node(".table-responsive:nth-child(2) .table-list") %>% 
        html_table()
      
     
      valueBox(total_change[1,3], subtitle = "Total Deaths Today",
               icon = icon("stats", lib = "glyphicon"),
               color = "purple")
    })
    output$value8 <- renderValueBox({
      
      y <- "https://www.worldometers.info/coronavirus/coronavirus-death-toll/"
      
      total_change <- read_html(y) %>% 
        html_node(".table-responsive:nth-child(2) .table-list") %>% 
        html_table()
      
      
      valueBox(total_change[1,4], subtitle = "Change in Total Deaths",
               icon = icon("stats", lib = "glyphicon"),
               color = "maroon")
    
      
    })
    output$value9 <- renderValueBox({
      
      #Number of critical Cases
      z <-  "https://www.worldometers.info/coronavirus/coronavirus-cases/#daily-cases"
      
      critical <-read_html(z) %>% 
        html_node("tr:nth-child(6) .number-table") %>% 
        html_text()
      
      valueBox(critical, subtitle = "Critical Cases",
               icon = icon("stats", lib = "glyphicon"),
               color = "black")
    })
}



# Run the application 
shinyApp(ui = ui, server = server)





