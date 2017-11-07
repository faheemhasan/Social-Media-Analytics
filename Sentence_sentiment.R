library(twitteR)
library(ROAuth)
library(plyr)
library(lubridate)
library(dplyr)
library(plotly)
library(stringr)
library(NLP)
library(tm)
library(devtools)
library(tidytext)
library(data.table)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)
library(SentimentAnalysis)
library(syuzhet)
library(pander)
library(httr)
consumer_key<-"xxxx"
consumer_secret<-"xxxx"
access_token<-"xxxx"
access_secret<-"xxxx"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
### Search Tweets ###
Tweet<- searchTwitter("#ITMovie OR @ITMovie OR #itmovie OR   #itMovie OR #itMovie2017 OR #ItMovie OR #ITmovie"
                      , n=200 , lang = 'en',since = "2017-09-08", until = "2017-09-12")
df1<- do.call(rbind,lapply(Tweet, as.data.frame))
df1$text

### Text cleaning ####
text = df1$text
text = gsub('[[:punct:]]', '', text)
text = gsub('[[:cntrl:]]', '', text)
text = gsub('\\d+', '', text)
text = gsub("@\\w+ *", "", text)
text = tolower(text)
text = gsub("[^0-9A-Za-z///' ]", "", text)
## Removing stopwords###
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
text = stringr::str_replace_all(text, stopwords_regex, '')
text = str_trim(text, side = c("both", "left", "right"))
text = gsub("http", "", text)
text = gsub("rt", "", text)
#### Sentiment Analysis ####
text = str_replace_all(text, stopwords_regex, '')
senti_value <- get_sentiment(text)
senti_value <- sign(senti_value)
sentiment1<-cbind(text,senti_value)

str(sentiment1)
class(sentiment1)
sentiment1<-as.data.frame(sentiment1)
sentiment1$senti_value = as.numeric(as.character(sentiment1$senti_value))
sentiment1$Sentiment_type <- ifelse(sentiment1$senti_value >0,"Positve",
                                    ifelse(sentiment1$senti_value <0, "Negative","Neutral"))
p <- plot_ly(sentiment1 %>% group_by(Sentiment_type) %>% 
               summarise(n=n()) %>% mutate(percent=n/sum(n)), 
             labels = ~Sentiment_type, values = ~percent, type = 'pie') %>%
  layout(title = 'Sentiment Analysis',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p
get_sentiment("Public want security from government")

#####################################
