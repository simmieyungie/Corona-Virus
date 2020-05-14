#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(base64enc)
library(shiny)
library(shiny)
library(rtweet)
library(dplyr)
library(glue)
library(reactable)
library(purrr)
library(shiny)
library(scales)
library(reshape2)
library(tidyverse)
library(wordcloud)
library(tidytext)
library(RColorBrewer)
library(shinydashboard)
library(twitteR)
library(ROAuth)
library(plotly)
library(glue)
library(twitteR)
#value boxes

ui <- dashboardPage(
    dashboardHeader(title = "Corona"),
    dashboardSidebar(
        sidebarPanel(style = "background-color: #1b2327",
                     numericInput("num_tweets_to_download",
                                  "Number of tweets to download:",
                                  min = 100,
                                  max = 18000,
                                  value = 500,
                                  step = 100), # <- Don't forget comma here
                     textInput("hashtag_to_search",
                               "Hashtag to search:",
                               value = "#covid")
                     , width = 0.3)),
    
    
    dashboardBody(
        tags$head(
            tags$style(HTML("
          .main-sidebar {
            background-color: skinblue !important;
          }
        "))),
        tabsetPanel(
            tabPanel(title = "First",
        fluidRow(
                shinydashboard::valueBoxOutput("value1"),
                shinydashboard::valueBoxOutput("value2"),
                shinydashboard::valueBoxOutput("value3")),            
        fluidRow(  
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
                    title = "Sentiment Polarity",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotlyOutput("Polarity", height = "300px")
                )),
            ),
           tabPanel(title = "New",
                     fluidRow(
                         shinydashboard::valueBoxOutput("value4"),
                         shinydashboard::valueBoxOutput("value5"),
                         shinydashboard::valueBoxOutput("value6")),
                     fluidRow(reactableOutput("tweet_table")))))
    )
    






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
       data <- searchTwitter(input$hashtag_to_search, n = input$num_tweets_to_download, 
                       resultType = "recent", lang = "en")
        
         twListToDF(data)
        })
    
    #Create reactive word cloud
    output$wordcloud <- renderPlot({
        ##Word clouds for all tweets
        table_1 <- dataInput() %>% 
            mutate(rowmumber = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word, sort = T) 
        wordcloud(table_1$word,freq = table_1$n, max.words = 100,
                  min.freq=1,scale=c(3,.5), 
                  random.order = FALSE,rot.per=.5,
                  colors = brewer.pal(8, "Dark2"))
    })
    #Build value box
    output$value1 <- shinydashboard::renderValueBox({
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
            filter(sentiment == "positive")
        
        n <- n[,2]
        
        
        shinydashboard::valueBox(paste(n, "%"), subtitle = "Positive Tweets", 
                                 icon = icon("stats", lib ="glyphicon" ), color = "light-blue")
    })
    
    output$value2 <- shinydashboard::renderValueBox({
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
        
        
        shinydashboard::valueBox(paste(n, "%"), subtitle = "Negative Tweets", 
                                 icon = icon("stats", lib ="glyphicon" ), color = "green")
    })
    output$top10 <- renderPlot({
        topwords <-  dataInput()[,1:16] %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
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
            labs(x = "Word", y = "count", title = "Most words used (Top 10)")
    })
    
    
    output$value3 <- shinydashboard::renderValueBox({
        
        tweets_count <- dataInput()[,1:16] %>% 
            nrow()
        
        
        shinydashboard::valueBox(tweets_count, subtitle = "Total Tweets", 
                                 icon = icon("stats", lib ="glyphicon" ), color = "orange")
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
            labs(y = "Count", x = "Words", main = "Positive vs Negative words")
        
        
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
                      geom_point(aes(text = text_formatted), color = "red") +
                      geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
                      geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") +
                      theme_bw() +
                      labs(title = "Sentiment Polarity", y = "sentiment score", x = "Twitter User"),
                  tooltip = "text") %>% 
            layout(
                xaxis = list(
                    rangeslider = list(type = "date")
                )
            )
    })
    
    #Build value box for total global infections
    output$value4 <- shinydashboard::renderValueBox({
        #Get the death rates
        x <- "https://www.worldometers.info/coronavirus/"
        
        #total global infections
      global <-  read_html(x) %>% 
            html_node("#maincounter-wrap:nth-child(7) span") %>% 
            html_text()
        
        shinydashboard::valueBox(paste(global), subtitle = "Global Cases", 
                                 icon = icon("stats", lib ="glyphicon" ), color = "light-blue")
    })
    
    #build value box for number of deaths
    #total deaths
    output$value5 <- shinydashboard::renderValueBox({
        x <- "https://www.worldometers.info/coronavirus/"
        
    deaths <- read_html(x) %>% 
        html_node("#maincounter-wrap:nth-child(9) span") %>% 
        html_text()
        shinydashboard::valueBox(deaths, subtitle = "Global Deaths",
                                 icon = icon("stats", lib = "glyphicon"),
                                 color = "orange")
    })
    
    #Value box for total recoveries world wide
    output$value6 <- shinydashboard::renderValueBox({
        
        x <- "https://www.worldometers.info/coronavirus/"
        
        #total recoveries
        recovery <- read_html(x) %>% 
            html_node("#maincounter-wrap+ #maincounter-wrap span") %>% 
            html_text()
        
        shinydashboard::valueBox(recovery, subtitle = "Global Deaths",
                                 icon = icon("stats", lib = "glyphicon"),
                                 color = "blue")
    })
    
    make_url_html <- function(url) {
        if(length(url) < 2) {
            if(!is.na(url)) {
                as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
            } else {
                ""
            }
        } else {
            paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
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
                Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
                URLs = purrr::map_chr(urls_expanded_url, make_url_html)
            )%>%
            select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
    })
    
    output$tweet_table <- renderReactable({
        reactable::reactable(tweet_table_data(), 
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
        #nrc tweet analysis
        nrc <- dataInput() %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("nrc")) %>% 
            group_by(sentiment) %>% 
            count(sentiment, sort = T)#
        
        ggplot(nrc, aes(reorder(sentiment, n), n, fill = sentiment)) + 
            geom_bar(stat = "identity", show.legend = F) + coord_flip()
    })
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
