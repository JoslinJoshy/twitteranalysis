install.packages("twitteR")
install.packages("ROAuth")
library("twitteR")
library("ROAuth")
# Download "cacert.pem" file)
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions

api_key = "952yCvBuW80Ai4499OTSup4py" # your api_key
api_secret = "vpaEc1Y2MpNRYIHPbhLopz4naIFB8lKsN0A60GleBKuO0PawVJ" # your api_secret 
access_token = " 872112219235926016-GWI2M5Jd5IMKBgZsbSoV8JAKsHSiHLV" # your access_token 
access_token_secret = "MsN1GVJNtom26k8bVrF4ky9tGPWTx2SUc7F5utKtxtEqF" # your access_token_sceret 
credential<-OAuthFactory$new(consumerKey=api_key,
                             consumerSecret=api_secret,
                             requestURL="https://api.twitter.com/oauth/request_token",
                             accessURL="https://api.twitter.com/oauth/access_token",
                             authURL="https://api.twitter.com/oauth/authorize")

credential$handshake()
4126234

setup_twitter_oauth(consumer_key = "952yCvBuW80Ai4499OTSup4py",
                    consumer_secret = "vpaEc1Y2MpNRYIHPbhLopz4naIFB8lKsN0A60GleBKuO0PawVJ",
                    access_token = "872112219235926016-GWI2M5Jd5IMKBgZsbSoV8JAKsHSiHLV",
                    access_secret = "MsN1GVJNtom26k8bVrF4ky9tGPWTx2SUc7F5utKtxtEqF")



tweets = searchTwitter("StayWrogn", 2500, lang="en")
 tweets.text <- sapply(tweets, function(x) x$getText())
 #tweets.text <- tolower(tweets.text)
 tweets.text <- tolower(tweets.text)
# Remove blank spaces at the beginning
 tweets.text <- gsub("^ ", "", tweets.text)
 
   # Remove blank spaces at the end
   tweets.text <- gsub(" $", "", tweets.text)
 # Remove links
   tweets.text <- gsub("http\\w+", "", tweets.text)
 # Remove punctuation
   tweets.text <- gsub("[[:punct:]]", "", tweets.text)
 # Replace blank space ("rt")
   tweets.text <- gsub("rt", "", tweets.text)

   # Replace @UserName
   tweets.text <- gsub("@\\w+", "", tweets.text)
 library("tm")
 
   #create corpus
   tweets.text.corpus <- Corpus(VectorSource(tweets.text))

   #clean up by removing stop words
  tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))
 library("wordcloud")


#generate wordcloud
 wordcloud(tweets.text.corpus,min.freq = 10, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
 
 
 library(twitteR)
 library(ROAuth)
 library(plyr)
 library(dplyr)
 library(stringr)
 library(ggplot2)
 
 
 search <- function(searchterm)
 {
   #access tweets and create cumulative file
   
   list <- searchTwitter(searchterm,n=5000, lang="en", since=NULL, until=NULL, retryOnRateLimit=10)
   df <- twListToDF(list)
   df <- df[, order(names(df))]
   df$created <- strftime(df$created, '%Y-%m-%d')
   if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
   
   #merge last access with cumulative file and remove duplicates
   stack <- read.csv(file=paste(searchterm, '_stack.csv'))
   stack <- rbind(stack, df)
   stack <- subset(stack, !duplicated(stack$text))
   write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
   
   #evaluation tweets function
   score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
   {
     require(plyr)
     require(stringr)
     scores <- laply(sentences, function(sentence, pos.words, neg.words){
       sentence <- gsub('[[:punct:]]', "", sentence)
       sentence <- gsub('[[:cntrl:]]', "", sentence)
       sentence <- gsub('\\d+', "", sentence)
       #sentence <- tolower(sentence)
       word.list <- str_split(sentence, '\\s+')
       words <- unlist(word.list)
       pos.matches <- match(words, pos.words)
       neg.matches <- match(words, neg.words)
       pos.matches <- !is.na(pos.matches)
       neg.matches <- !is.na(neg.matches)
       score <- sum(pos.matches) - sum(neg.matches)
       return(score)
     }, pos.words, neg.words, .progress=.progress)
     scores.df <- data.frame(score=scores, text=sentences)
     return(scores.df)
   }
   
   pos <- scan('C:/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
   neg <- scan('C:/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
   pos.words <- c(pos, 'upgrade')
   neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
   
   Dataset <- stack
   Dataset$text <- as.factor(Dataset$text)
   scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
   write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results into the file
   
   #total evaluation: positive / negative / neutral
   stat <- scores
   stat$created <- stack$created
   stat$created <- as.Date(stat$created)
   stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
   by.tweet <- group_by(stat, tweet, created)
   by.tweet <- summarise(by.tweet, number=n())
   write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)
   
   #create chart
   ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
     geom_point(aes(group=tweet, color=tweet), size=4) +
     theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
     #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
     ggtitle(searchterm)
   
   ggsave(file=paste(searchterm, '_plot.jpeg'))
   
   
   ggplot(stat, aes(created, score)) + geom_point(size=1) +
     stat_summary(fun.data = 'mean_cl_normal', mult = 1, geom = 'smooth') +
     ggtitle(searchterm)
   
   ggsave(file=paste(searchterm, '_plot_val.jpeg'))
   
   
   
 }
 
 search("StayWrogn") #enter keyword
 
 
 calculate_total_presence_sentiment(tweets.text)
 
 if (!require('pacman')) install.packages('pacman')
 pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)
 
 # harvest some tweets
 some_tweets = searchTwitter('StayWrogn', n=4000, lang='en')
 
 # get the text
 some_txt = sapply(some_tweets, function(x) x$getText())
 
 
 some_txt = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', some_txt)
 # remove at people
 some_txt = gsub('@\\w+', '', some_txt)
 # remove punctuation
 some_txt = gsub('[[:punct:]]', '', some_txt)
 # remove numbers
 some_txt = gsub('[[:digit:]]', '', some_txt)
 # remove html links
 some_txt = gsub('http\\w+', '', some_txt)
 # remove unnecessary spaces
 some_txt = gsub('[ \t]{2,}', '', some_txt)
 some_txt = gsub('^\\s+|\\s+$', '', some_txt)
 
 # define 'tolower error handling' function
 try.error = function(x)
 {
   # create missing value
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error=function(e) e)
   # if not an error
   if (!inherits(try_error, 'error'))
     y = tolower(x)
   # result
   return(y)
 }
 # lower case using try.error with sapply
 some_txt = sapply(some_txt, try.error)
 
 # remove NAs in some_txt
 some_txt = some_txt[!is.na(some_txt)]
 names(some_txt) = NULL
 
 
 
 # Perform Sentiment Analysis
 # classify emotion
 class_emo = classify_emotion(some_txt, algorithm='bayes', prior=1.0)
 # get emotion best fit
 emotion = class_emo[,7]
 # substitute NA's by 'unknown'
 emotion[is.na(emotion)] = 'unknown'
 
 # classify polarity
 class_pol = classify_polarity(some_txt, algorithm='bayes')
 # get polarity best fit
 polarity = class_pol[,4]
 # Create data frame with the results and obtain some general statistics
 # data frame with results
 sent_df = data.frame(text=some_txt, emotion=emotion,
                      polarity=polarity, stringsAsFactors=FALSE)
 
 # sort data frame
 sent_df = within(sent_df,
                  emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
 
 
 # Let's do some plots of the obtained results
 # plot distribution of emotions
 ggplot(sent_df, aes(x=emotion)) +
   geom_bar(aes(y=..count.., fill=emotion)) +
   scale_fill_brewer(palette='Dark2') +
   labs(x='emotion categories', y='number of tweets') +
   ggtitle('Sentiment Analysis of Tweets StayWrogn\n(classification by emotion)') +
   theme(plot.title = element_text(size=12, face='bold'))
 
 
 # plot distribution of polarity
 ggplot(sent_df, aes(x=polarity)) +
   geom_bar(aes(y=..count.., fill=polarity)) +
   scale_fill_brewer(palette='RdGy') +
   labs(x='polarity categories', y='number of tweets') +
   ggtitle('Sentiment Analysis of StayWrogn\n(classification by polarity)') +
   theme(plot.title = element_text(size=12, face='bold'))
 
 
 # Separate the text by emotions and visualize the words with a comparison cloud
 # separating text by emotion
 emos = levels(factor(sent_df$emotion))
 nemo = length(emos)
 emo.docs = rep('', nemo)
 for (i in 1:nemo)
 {
   tmp = some_txt[emotion == emos[i]]
   emo.docs[i] = paste(tmp, collapse=' ')
 }
 
 # remove stopwords
 emo.docs = removeWords(emo.docs, stopwords('english'))
 # create corpus
 corpus = Corpus(VectorSource(emo.docs))
 tdm = TermDocumentMatrix(corpus)
 tdm = as.matrix(tdm)
 colnames(tdm) = emos
 
 # comparison word cloud
 comparison.cloud(tdm, colors = brewer.pal(nemo, 'Dark2'),
                  scale = c(3,.5), random.order = FALSE, title.size = 1.5)
