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
library(base64enc)
consumer_key<-"7HzXe0RedgnIQSpPWP8vTmizF"
consumer_secret<-"snzwo1Fb4WdbvGy6VCgbbYbpkiy4Oq2XefKOSuuHf48MY7DVRK"
access_token<-"905673084237111296-YGNP6yilPzOFRco9rKBdQ25Df1ZZfmP"
access_secret<-"4ar6pImdsVOuZhESkTrTe6vWxkwPzDf5e180BmWJjP6Sm"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

# Fetch user timeline data
df<-userTimeline("twitterid",n=200)
df<- do.call(rbind,lapply(df, as.data.frame))
df$text
user<- getUser("twitterid")
## Average number of favorite count on profile ##
count = sum(df$favoriteCount)/nrow(df)
## Average number of favorite count on profile ##
Retweet = sum(df$retweetCount)/nrow(df)
### Page activity by week##
test = df
test$date = test$created
test$month = month(test$created)
test$year = year(test$created)
test$week = week(test$created)
test1<-dplyr::select(test, favoriteCount,year,week)
test2<-ddply(test1,c("week","year"),summarise,sumfc = sum(favoriteCount),
             n=length(week))
test2$favoritecount<-round(test2$sumfc/test2$n,2)
newdata<-test2[order(test2$year),]
newdata$timeperiod <- paste(newdata$week,"week","of",newdata$year)
a = as.data.frame(paste(newdata$week,"of", newdata$year, sep = "-"))
a
m<-list(l=40,r=40,b=50,t=50,pad=4)
p <- plot_ly(newdata, x = ~timeperiod, y = ~sumfc,
     type = 'scatter', mode = 'lines')%>%
  layout(m,title="Page activity by week")
p

### Text cleaning ####
text = df$text
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
df<-data.frame(v1=text, stringsAsFactors = FALSE)
df
mycorpus<-Corpus(DataframeSource(df))
tdm <- TermDocumentMatrix(mycorpus,
                          control = list(removePunctuation = TRUE,stopwords=TRUE))
tdm              
freqterm<-findFreqTerms(tdm, lowfreq = 2)                                            
m<-as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

###### Wordcloud ########

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#### Sentiment Analysis ####
text.text = str_replace_all(d$word, stopwords_regex, '')
nrc_data <- get_nrc_sentiment(text.text)
sentiment1<-cbind(text.text,nrc_data)
sentimentTotal<-data.frame(colSums(sentiment1[,c(2:11)]))
names(sentimentTotal)<-"count"
sentimentTotal<-cbind("sentiment"=rownames(sentimentTotal), sentimentTotal)
sentimentTotal$pct<-(sentimentTotal$count/sum(sentimentTotal$count))*100
polarity = sentimentTotal[-c(1:8),]
emotion = sentimentTotal[-c(9:10),]
#colnames(sentimentTotal) <- c("Sentiment", "count")
p2 <- plot_ly(emotion, x = ~pct, y = ~as.character(sentiment), type = 'bar',
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(10,48,107)',
                                        width = 1.5))) %>%
  layout(title = "Emotional Analysis",
         xaxis = list(title = "Count"),
         yaxis = list(title = "Sentiment"))
p2


p2 <- plot_ly(emotion, x = ~pct, y = ~as.character(sentiment), type = 'bar',
              marker = list(color = 'rgb(168,202,225)',
                            line = list(color = 'rgb(10,48,107)',
                                        width = 1.5))) %>%
  layout(title = "Emotion Analysis",
         xaxis = list(title = "Count"),
         yaxis = list(tickangle=38,title = "Sentiment"))
p2


### Polarity Analysis ###

p3 <- plot_ly(polarity, x = ~pct, y = ~as.character(sentiment), type = 'bar',
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(10,48,107)',
                                        width = 1.5))) %>%
  layout(title = "Polarity Analysis",
         xaxis = list(title = "Count"),
         yaxis = list(title = "Sentiment"))
p3

#Fetch Search tweet 
Tweet<- searchTwitter("movie IT", n=50, lang="en", since=NULL, until=NULL,
                      locale=NULL, geocode=NULL, sinceID=NULL,resultType=NULL,
                      retryOnRateLimit=120)
df1<- do.call(rbind,lapply(Tweet, as.data.frame))
df1$text

devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")

