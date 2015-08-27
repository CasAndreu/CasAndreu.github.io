#######################################
## TWITTER DATA FOR SOCIAL RESEARCH  ##
## APSA'15 Text as Data Workshop     ##
## Sep. 3, 2015 (9am - 5pm)          ##
## Andreu Casas and John Wilkerson   ##
#######################################

# In this script we will see different ways of scraping and using
#     Twitter messages for social science research.

# 1) OBTAINING A TWITTER DEVELOPER KEY
# To use the Twitter API you first need to have a Twitter account
#     and a developer Key. Go to https://apps.twitter.com/, sign
#     in using your Twitter username and password, click on 'Create
#     New App", fill the form (e.g. Name: Andreu; Description: Social
#     Researcher; Website: http://google.com). Access your app and
#     go to the tab 'Keys and Acces Tokens' where you will find your
#     Consumer Key and Consumer Secret. 


# 2) INSTALLING/LOADING REQUIRED PACKAGES

install.packages('devtools')
install.packages("streamR")
install.packages("twitteR")
install.packages('RCurl')
library(streamR)
library(twitteR)
library(devtools)
library(ROAuth)
library(RCurl)

# 3) CONNECTING TO TWITTER'S API

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "Fm106u13gICaJN7R0w"
consumerSecret <- "ovKL3CBFTyF01n549tqCsR4WsXq28RuTExjN4qBPVM"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# Copy paste the link printed at the console to a web browser, and then paste
#     the resulting code back to the R console 

save(my_oauth, file = "my_oauth.Rdata")
registerTwitterOAuth(my_oauth)


# 4) USING THE 'REST API'
# We use the REST API to gather information of specific users. We can capture
#     the last 3,200 tweets sent by any user. Here we will collect the last
#     200 tweets of 3 House Republican leaders.

members <- c('SpeakerBoehner','GOPWhip','GOPLeader')
data <- NULL

for (i in members){
  tweets <- try(userTimeline(i,n=200, includeRts=T))
  if(inherits(tweets, "try-error")){
    print(i)
    next
  } 
  df <- do.call("rbind", lapply(tweets, as.data.frame))
  data <- rbind(data,df)
  # write.csv(df, file=paste(i, ".csv", sep = ""), row.names = F) # This saves
  #       a csv file with the messages of each user
}

View(data)


# 5) USING THE 'SEARCH API'
# We use the SEARCH API to look for past tweets by keyword/s. We can only access
#     tweets that have been sent in the last 6-9 days. 


# Searching for the last 1,000 tweets mentioning "climate change":

tweets <- searchTwitter(c('"Jeff Bush"'), n=1000)
# For windows machines (?):
# searchTwitter("#PoliSciNSF", cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
tweets <-  do.call("rbind", lapply(tweets, as.data.frame))




# 6) USING THE 'STREAMING API'
# We use the STREAMING API to capture tweets while they are being sent. We can only 
#     collect up to 1% of the messages that are being sent at any moment. When reaching
#     that limit, Twitter returns a sample of what is being said (random? we don't know)
#     Here we will capture all tweets mentioning the bigram 'climate change' in a minute.

filterStream(file.name = "tweets_keyword.json", 
             track = c('Obama'),
             timeout = 60, oauth = my_oauth)
# ... now you should see in the console: "Capturing tweets..." (for 60 sec.)
streamtweets <- parseTweets("tweets_keyword.json", verbose = T)


# 7) SENTIMENT ANALYSIS
# Here we collect the last 1,000 tweets mentioning Clinton and we classify
#     them into POSTIVIE and NEGATIVE messages using a Supervised 
#     Machine Learning Algorithm.

# Gathering the data
clinton <- searchTwitter(c('obama'), n=1000)
clinton <-  do.call("rbind", lapply(clinton, as.data.frame))

# Getting examples of POSITIVE and NEGATIVE statements (Movie
#     Review Dataset, originally created by Bo Pang and Lillian
#     Lee).
library(RCurl) # install.packages('RCurl') # If you don't have it
url = "http://students.washington.edu/acasas2/reviews_sentiment.csv"
movies = read.csv(file=url)

names(movies)
table(movies$label)

# Some preprocessing: cleaning the text of the reviews and also the
#       twitter messages
library(tm)  # install.packages('tm')
txt <- Corpus(VectorSource(movies$text), readerControl = list(language = "en"))
txt <- tm_map(txt, stripWhitespace) 
txt <- tm_map(txt, removePunctuation)
txt <- tm_map(txt, tolower)
txt <- tm_map(txt, removeWords, stopwords("english")) 
txt <- tm_map(txt, removeNumbers)
txt <- tm_map(txt, stemDocument, language= "english")
txt <- tm_map(txt, PlainTextDocument)
movies$cleantext<- as.character(txt)

# Removing non-ASCII characters from the tweets
Encoding(clinton$text) <- 'UTF-8'
clinton$text <- iconv(clinton$text, "UTF-8", "ASCII", sub="")

txt2 <- Corpus(VectorSource(clinton$text), readerControl = list(language = "en"))
txt2 <- tm_map(txt2, stripWhitespace) 
txt2 <- tm_map(txt2, removePunctuation)
txt2 <- tm_map(txt2, tolower)
txt2 <- tm_map(txt2, removeWords, stopwords("english")) 
txt2 <- tm_map(txt2, removeNumbers)
txt2 <- tm_map(txt2, stemDocument, language= "english")
txt2 <- tm_map(txt2, PlainTextDocument)
clinton$cleantext<- as.character(txt2)

# Mixing the observation in the 'movies' dataset so that we don't
#     have first all the positive cases and then all the negatives
movies <- movies[sample(x=1:2000,size=2000,replace=F),]

# Using an SVM algorithm from RTextTools to classify the tweets
library(RTextTools)  # install.packages('RTextTools')
doc_matrix <- create_matrix(movies$cleantext, language="english",
                            removeNumbers=F,stemWords=F,
                            removeSparseTerms=.998)
container <- create_container(doc_matrix, movies$label, 
                            trainSize=1:500,testSize=501:1000,
                            virgin=FALSE)
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)
analytics <- create_analytics(container,SVM_CLASSIFY)
performance <- analytics@algorithm_summary

# This gives us information about the classifier performance
print(performance) # ... not bad.

# Now we create a new data set merging the movie-reviews dataset
#     and the Twitter messages. We need to share some variable names.
#     Then we will train the SVM algorithm using all the movie
#     reviews and we will use it to classify the tweets.

movies2 <- as.data.frame(cbind(as.character(movies$text),
                 movies$cleantext,
                 movies$label))
clinton2 <- as.data.frame(cbind(as.character(clinton$text),
                clinton$cleantext))
clinton2$label <- NA
colnames(movies2) <- c('text','cleantext','label')
colnames(clinton2) <- colnames(movies2)
maindb <- rbind(movies2,clinton2)

    # Train and Classify
doc_matrix2 <- create_matrix(maindb$cleantext, language="english",
                            removeNumbers=F,stemWords=F,
                            removeSparseTerms=.998)
container2 <- create_container(doc_matrix2, maindb$label, 
                              trainSize=1:2000,
                              testSize=2001:length(maindb[,1]),
                              virgin=TRUE)
SVM2 <- train_model(container2,"MAXENT")
SVM_CLASSIFY2 <- classify_model(container2, SVM2)

View(SVM_CLASSIFY2)

