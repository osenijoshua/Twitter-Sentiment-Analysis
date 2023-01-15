# TO CHECK PACKAGES RUNNING IN AN R SESSION
(.packages())

# INSTALL THIS PACKAGE BEFORE GOING FURTHER IN THE SCRIPT
install.packages('rtweet','tidyverse','getPass','lubridate')  # WE'LL BE NEEDING THESE

library(tidyverse) # THIS PACKAGE REALLY HELPS IN DATA WRANGLING
library(rtweet) # THIS WOULD HELP IN CREATING A CONNECTION TO TWITTER
library(getPass) # FOR ENCRYPTION REASONS WHEN ENTERING YOUR TOKENS
library(lubridate) # THIS IS USED FOR DATE-TIME TRANSFORMATION

# YOU MAY NEED INTERNET CONNECTION TO RUN A NUMBER OF THESE SCRIPTS
# AUTHORIZE R-STUDIO TO CONNECT TO TWITTER USING THE UNDERLISTED FUNCTIONS
# NOTE THAT YOU NEED TO HAVE A TWITTER DEVELOPER ACCOUNT

api_key = getPass(msg="Enter API Key")
api_secret = getPass(msg="Enter API Secret")
access_token = getPass(msg="Enter Access Token")
access_secret = getPass(msg="Enter Access Token Secret")

create_token(app="RTweetBot2",api_key, api_secret, access_token, access_secret)

# ALTERNATE CONNECTION METHOD
auth_as(readRDS("create_token.rds")) # USE THIS AUTHENTICATION METHOD IF YOU DON'T WANT TO MAKE YOUR TOKENS VISIBLE
# THE create_token.rds FILE IS AUOTOMATICALLY CREATED IN THE WORKING DIRECTORY OF YOUR R SCRIPT AFTER INITIALLY ENTERING THE FOUR REQUIRED TOKENS i.e;
# api_key
# api_secret
# access_token
# access_secret

# TWEETS THAT CONTAIN A SEARCH TERM
tweets <- search_tweets("#FIFAWorldCup", n=100, include_rts = FALSE, lang ="en") %>%
  mutate(day.name = weekdays(as.POSIXlt(created_at, format="%Y-%m-%d %H:%M:%S"))) %>%
  mutate(day.num = wday(as.POSIXlt(created_at, format="%Y-%m-%d %H:%M:%S"), label = FALSE))

View(users_data(tweets))

# LIVE TWEET STREAMING
# tweets <- stream_tweets(timeout=5) NOT WOKRING.

View(tweets)
head(tweets)
tweets %>%
  count(day.name) %>%
  group_by(day.name)

# TWEETS FROM A USER (WORKS ON PRIVATE TWITTER ACCOUNTS AS WELL)
musk_tweets <- get_timeline("elonmusk",n=100)
View(musk_tweets)

# TWEETS FROM MY TIMELINE
my_tl_tweets <- get_my_timeline(n=100)
View(my_tl_tweets)

# WRITING TO TWITTER
post_tweet(status = "tweeted via bot")

# TWEETS TAGGING MY USERNAME
get_mentions(n=15)

# SENTIMENT ANALYSIS FROM TWEETS
install.packages("textdata","tidytext","RColorBrewer","tm")

library(textdata) # THIS PACKAGE CONTAINS DIFFERENT VOCABULARIES CRITICAL TO SENTIMENT ANALYSIS
library(stopwords) # CONTAINS A SET OF GENERIC WORDS THAT MAY NEED TO BE EXCLUDED IN TEXT MINING 
library(tidytext) # TEXT DATA TRANSFORMATION
# library(RColorBrewer)
library(ggplot) # DATA VISUALIZATION
library(ggwordcloud) # DATA VISUALIZATION

sentiment_tbl <- get_sentiments("bing") # WORD-SENTIMENTS

x <- lexicon_nrc() %>%
  filter(sentiment %in% c('positive','negative'))

nrc_sentiment <- lexicon_nrc() # WORD-EMOTIONS
View(nrc_sentiment)
View(sentiment_tbl)

pos_sent <- sentiment_tbl %>%
  filter(sentiment=="positive")

neg_sent <- sentiment_tbl %>%
  filter(sentiment=="negative")

generic_words <- stopwords::stopwords("en", source="stopwords-iso")

# WORD-EMOTION TABLE
tb1 <- tweets %>%
  select(full_text) %>%
  unnest_tokens(input=full_text, output=full_text) %>% # unnest_tokens separates a chunk of strings like a sentence into different sub-strings i.e. words
  inner_join(nrc_sentiment, by=c("full_text"="word")) %>%
  count(sentiment)

# WORD SENTIMENT TABLE
tb2 <- tweets %>%
  select(created_at, day.name, day.num, full_text) %>%
  unnest_tokens(input=full_text, output=full_text) %>%
  inner_join(sentiment_tbl, by=c("full_text"="word")) %>%
  filter(!full_text %in% c(generic_words))

# POSITIVE SENTIMENT TABLE
tb4 <- tweets %>%
  select(full_text) %>%
  unnest_tokens(input=full_text, output=full_text) %>%
  inner_join(pos_sent, by=c("full_text"="word")) %>%
  filter(!full_text %in% c(generic_words)) %>%
  count(full_text)

# NEGATIVE SENTIMENT TABLE
tb5 <- tweets %>%
  select(full_text) %>%
  unnest_tokens(input=full_text, output=full_text) %>%
  inner_join(neg_sent, by=c("full_text"="word")) %>%
  filter(!full_text %in% c(generic_words)) %>%
  count(full_text)

# GENERAL WORD CLOUD
tweets %>%
  select(full_text) %>%
  unnest_tokens(input=full_text, output=full_text) %>%
  filter(!full_text %in% c(generic_words,"https","t.co")) %>%
  count(full_text) %>%
  arrange(-n) %>%
  slice(1:100) %>%
  ggplot(aes(label=full_text, size=n, col=as.character(n))) +
  geom_text_wordcloud_area(rm_outside = TRUE, max_steps = 1,
                           grid_size = 1, eccentricity = .9) +
  scale_size_area(max_size = 35) +
  labs(title = "Most Tweeted Words") +
  # scale_color_brewer(palette = "Paired", direction = -1)+
  theme_void()

# POSITIVE & NEGATIVE WORDCLOUD
tweets %>%
  select(full_text) %>%
  unnest_tokens(input=full_text, output=full_text) %>%
  filter(!full_text %in% c(generic_words)) %>%
  count(full_text) %>%
  arrange(-n) %>%
  inner_join(sentiment_tbl, c("full_text"="word")) %>%
  ggplot(aes(label=full_text, color=sentiment, size=n)) +
  geom_text_wordcloud_area() +
  facet_wrap(~ sentiment, ncol=2) +
  scale_size_area(max_size = 30)

# VISUALIZATION

# EMOTION BAR CHARTS
tb1 %>%
  ggplot(aes(y=reorder(sentiment, n), x=n, fill=sentiment)) +
  geom_bar(stat="identity", show.legend = TRUE) +
  geom_text(aes(label=n),color="black") +
  xlab("Frequency") + ylab("Sentiment") +
  ggtitle("Word Emotion Analysis Using Tweets") +
  theme_minimal()

# SENTIMENT BAR CHARTS
tb2 %>%
  ggplot(aes(y=reorder(day.name, -day.num), fill=sentiment)) +
  geom_bar(stat = "count", width=0.5) +
  scale_fill_manual(values = brewer.pal(4, "RdYlGn")[c(1,4)]) +
  xlab("Tweets") + ylab("") +
  ggtitle("Sentiment Analysis Using Tweets") +
  theme_minimal()

# # GENERAL WORDCLOUD
# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5,y=0.5,"Most Tweeted Words",col="purple")
# wordcloud(words=tb3$full_text, freq=tb3$n, max.words = 500,
#           random.order=FALSE, rot.per=0.35)

# POSITIVE WORDCLOUD
# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5,y=0.5,"Positive Sentiments",col="blue")
# wordcloud(words=tb4$full_text, freq=tb4$n, max.words = 50, scale = c(4,1), min.freq = 1,
#            random.order=FALSE, rot.per=0, colors = brewer.pal(9,"Blues"))

# # NEGATIVE WORDCLOUD
# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5,y=0.5,"Negative Sentiments",col="red")
# wordcloud(words=tb5$full_text, freq=tb5$n, max.words = 50, scale = c(4,1),
#           min.freq = 1,
#            random.order=TRUE, rot.per=0, colors = brewer.pal(9,"Reds"))
