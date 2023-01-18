# This is a Shiny web application.
# Author: Joshua Oseni
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tidyverse)
library(shiny)
# library(reactable)
library(rtweet)
library(lubridate)
library(textdata)
library(stopwords)
library(tidytext)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(ggwordcloud)

# api_key = '**'
# api_secret = '**'
# access_token = '**'
# access_secret = '**'
# 
# create_token(app="RTweetBot2",api_key, api_secret, access_token, access_secret)

auth_as(readRDS("create_token.rds"))

# THERE ARE TWO PARTS TO EVERY SHINY WEB APPLICATION.
# THE UI & THE SERVER

# THIS IS THE UI SECTION WHERE THE LAYOUT, APPEARANCE AND INTERACTIVITY OF THE WEB APP IS DESIGNED
# FOR EVERY UI INPUT, THERE IS USUALLY A CORRESPONDING SERVER OUTPUT.
ui <- fluidPage(theme = shinytheme("superhero"),
                sidebarPanel(tags$h3("Search"),
                             textInput("search", "Enter a Twitter hashtag or topic for your analysis"),
                             numericInput("no_of_tweets", "Enter the number of tweets you want to fetch",
                                         min=0,max=1000,value=0,step=100),
                             actionButton("submit","Fetch", class="btn-primary"),
                             br(),
                             br(),
                             br(),
                             p("Do note that you may only fetch",strong(span("a maximum of 1000 tweets", style = "color:red")), "at a time."),
                             p("Tweets can only be fetched as far as",strong("6 - 9 days ago.")),
                             # img(src = "rstudio.png", height = 70, width = 200),
                ),
                mainPanel(
                    h1("Sentiment Analysis Visualization"),
                    # verbatimTextOutput("search"),
                    # reactableOutput("tweet_table"),
                    fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"),
                    plotOutput("tweets"),
                    plotOutput("word_emotion"))),
                    fluidRow(
                        splitLayout(cellWidths = c("50%","50%"),
                    plotOutput("pos_wordcloud"),
                    plotOutput("neg_wordcloud")
                    )),
                    fluidRow(plotOutput("sentiment_bar")
                    )
                ))

# THIS IS THE SERVER SECTION.
# THIS IS WHERE THE PROCESSING ENGINE AND MACHINE LEARNING MODELS SIT.
# THEY CONTROL THE RESULTS OF INPUT MADE BY THE USER.
server <- function(input, output) {
    output$search <- renderText({
        input$search}, quoted=FALSE
         )
    
    t1 <- eventReactive(input$submit, {search_tweets(input$search, n=input$no_of_tweets, include_rts = FALSE, lang="en") %>%
            mutate(day.name = weekdays(as.POSIXlt(created_at, format="%Y-%m-%d %H:%M:%S"))) %>%
            mutate(day.num = wday(as.POSIXlt(created_at, format="%Y-%m-%d %H:%M:%S"), label = FALSE))
                    })

    # t2 <- reactive({
    #                 req(t1())
    #                 t1() %>%
    #                 mutate(day.name = weekdays(as.POSIXlt(created_at, format="%Y-%m-%d %H:%M:%S"))) %>%
    #                 mutate(day.num = wday(as.POSIXlt(created_at, format="%Y-%m-%d %H:%M:%S"), label = FALSE))
    #         })
    
    # sentiment_tbl <- get_sentiments("bing") 
    # sentiment_tbl <- read.csv("bing_sentiment.csv") %>%
    #                             select(word,sentiment) # WORD-SENTIMENTS
    
    # nrc_sentiment <- lexicon_nrc()
    nrc_sentiment <- read.csv("nrc_sentiment.csv") %>%
                                select(word,sentiment) # WORD-EMOTIONS
    
    sentiment_tbl <- read.csv("nrc_sentiment.csv") %>%
        select(word,sentiment)%>%
        filter(sentiment %in% c('positive','negative')) # WORD-SENTIMENTS
    
    pos_sent <- sentiment_tbl %>%
        filter(sentiment=="positive")
    
    neg_sent <- sentiment_tbl %>%
        filter(sentiment=="negative")
    
    generic_words <- stopwords::stopwords("en", source="stopwords-iso")
    
    # output$tweet_table <- renderReactable({
    #                     reactable::reactable(t2(), 
    #                     filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
    #                     showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
    #                     columns = list(
    #                         day.name = colDef(defaultSortOrder = "asc"),
    #                         id = colDef(defaultSortOrder = "asc"),
    #                         full_text = colDef(defaultSortOrder = "asc"),
    #                         favorite_count = colDef(html = TRUE, minWidth = 190, resizable = TRUE)
    #                      )
    #                     )
    # })
    
    # GENERAL WORDCLOUD OF TOP  50 WORDS
    output$tweets <- renderPlot({
        t1() %>%
            select(full_text) %>%
            unnest_tokens(input=full_text, output=full_text) %>%
            filter(!full_text %in% c(generic_words,"https","t.co")) %>%
            count(full_text) %>%
            arrange(-n) %>%
            slice(2:51) %>% # I AM SLICING FROM ROW 2 BECAUSE ROW 1 WOULD ALWAYS BE THE SEARCH TERM AND WE DON'T NEED THAT TO BE IN THE GENERAL WORDCLOUD
            ggplot(aes(label=full_text, size=n, col=(n))) +
            geom_text_wordcloud_area(rm_outside = TRUE, max_steps = 1,
                                     grid_size = 1, eccentricity = 1) +
            scale_size_area(max_size = 35) +
            ggtitle("Most Tweeted Words") +
            scale_color_gradient(low = "#52be80",
                                 high = "#145a32",
                                 space = "Lab",
                                 na.value = "grey50",
                                 guide = "colourbar",
                                 aesthetics = "colour")+
            # scale_color_brewer(palette = "Paired", direction = -1)+
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })

    # WORD EMOTION DISTRIBUTION
    output$word_emotion <- renderPlot({
        t1() %>%
            select(full_text) %>%
            unnest_tokens(input=full_text, output=full_text) %>% # unnest_tokens separates a chunk of strings like a sentence...
                                                                 # ...into different sub-strings i.e. words
            inner_join(nrc_sentiment, by=c("full_text"="word")) %>%
            count(sentiment) %>%
            ggplot(aes(y=reorder(sentiment, n), x=n, fill=sentiment)) +
            geom_bar(stat="identity", show.legend = TRUE) +
            geom_text(aes(label=n),color="black") +
            xlab("Frequency") + ylab("Sentiment") +
            ggtitle("Word Emotion Analysis Using Tweets") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
    # POSITIVE SENTIMENT WORDCLOUD
    output$pos_wordcloud <- renderPlot({
        t1() %>%
            select(full_text) %>%
            unnest_tokens(input=full_text, output=full_text) %>%
            inner_join(pos_sent, by=c("full_text"="word")) %>%
            filter(!full_text %in% c(generic_words)) %>%
            count(full_text) %>%
            arrange(-n) %>%
            slice(1:50) %>%
            ggplot(aes(label=full_text, size=n, col=n)) +
            geom_text_wordcloud_area(rm_outside = TRUE, max_steps = 1,
                                     grid_size = 1, shape = 'diamond') +
            scale_size_area(max_size = 30) +
            ggtitle("Positive Sentiments") +
            scale_color_gradient(low = "#4fc3f7",
                                 high = "#00008b",
                                 space = "Lab",
                                 na.value = "grey50",
                                 guide = "colourbar",
                                 aesthetics = "colour")+
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
    # NEGATIVE SENTIMENT WORDCLOUD
    output$neg_wordcloud <- renderPlot({
        t1() %>%
            select(full_text) %>%
            unnest_tokens(input=full_text, output=full_text) %>%
            inner_join(neg_sent, by=c("full_text"="word")) %>%
            filter(!full_text %in% c(generic_words)) %>%
            count(full_text) %>%
            arrange(-n) %>%
            slice(1:50) %>%
            ggplot(aes(label=full_text, size=n, col=n)) +
            geom_text_wordcloud_area(rm_outside = TRUE, max_steps = 1,
                                     grid_size = 1, eccentricity = .9) +
            scale_size_area(max_size = 30) +
            ggtitle("Negative Sentiments") +
            scale_color_gradient(low = "#FC7676",
                                 high = "#420C09",
                                 space = "Lab",
                                 na.value = "grey50",
                                 guide = "colourbar",
                                 aesthetics = "colour")+
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
    # POSITIVE/NEGATIVE DISTRIBUTION BAR
    output$sentiment_bar <- renderPlot({
        t1() %>%
            select(created_at, day.name, day.num, full_text) %>%
            unnest_tokens(input=full_text, output=full_text) %>%
            inner_join(sentiment_tbl, by=c("full_text"="word")) %>%
            filter(!full_text %in% c(generic_words)) %>%
            ggplot(aes(y=reorder(day.name, -day.num), fill=sentiment)) +
            geom_bar(stat = "count", width=0.5) +
            scale_fill_manual(values = brewer.pal(4, "RdYlGn")[c(1,4)]) +
            xlab("Tweets") + ylab("") +
            ggtitle("Positive/Negative Distribution") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
}

# # THIS IS THE FINAL PART OF THE SHINY WEB APPLICATON
# ALTHOUGH, IT IS NOT A CORE PORTION OF THE WEB APP. IT IS ALSO IMPORTANT AS IT CALLS THE UI & SERVER SECTIONS TOGETHER TO BUILD THE APP.
shinyApp(ui, server)
