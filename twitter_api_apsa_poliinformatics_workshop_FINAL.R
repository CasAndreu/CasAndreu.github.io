############################################
## TWITTER DATA FOR SOCIAL RESEARCH  	    
## APSA'15 Text as Data Workshop      	  
## Sep. 3, 2015 (9am - 5pm)          	    
## Andreu Casas					                  
## cappp.org					                    
## University of Washington 			        
############################################


# 1) OBTAINING A TWITTER DEVELOPER KEY
# 	To use the Twitter API you need to have a Twitter account
#     and a developer Key. First you need to create a Twitter account.
#	Then go to https://apps.twitter.com/, sign
#     in using your Twitter username and password, click on 'Create
#     New App", fill out the form (e.g. Name: Andreu; Description:
#	Social Science Researcher; Website: http://google.com). 
#	Access your app and go to the tab 'Keys and Acces Tokens,' 
#	where you will find your Consumer Key and Consumer Secret. 


# 2) INSTALLING/LOADING REQUIRED PACKAGES

install.packages('devtools')
install.packages("streamR")
install.packages("twitteR")
install.packages('RCurl')
install.packages('ROAuth')
install.packages('tm')
install.packages('SnowballC')
install.packages('RTextTools')
library(streamR)
library(twitteR)
library(devtools)
library(ROAuth)
library(RCurl)
library(SnowballC)

# 3) CONNECTING TO TWITTER'S API

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey ='Fm106u13gICaJN7R0w'
consumerSecret = 'ovKL3CBFTyF01n549tqCsR4WsXq28RuTExjN4qBPVM'
accessToken = '485044109-MRLnLeKCfdxg3cpzVL7khsENwBFuCam4D2eXMoxn'
accessSecret ='yiSzjkPISW6V0qS7AJREVFSS1QVdt1QGew4tZ1kJw'

my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, requestURL = requestURL, accessURL = accessURL, authURL = authURL) 
my_oauth$handshake(cainfo = system.file("CurlSSL",  "cacert.pem", package = "RCurl"))

# Copy paste the link printed at the console to a web browser, and 
# paste the resulting code back to the R console 

save(my_oauth, file = "my_oauth.Rdata")
registerTwitterOAuth(my_oauth)
# For PC USERS: (choose/type 1)
# setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret) 

# 4) USING THE 'REST API'

# We use the REST API to gather information from specific users. 
# We can capture the last 3,200 tweets sent by any user. 
# Here we collect the last 200 tweets of 3 House Republican leaders.

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

# We use the SEARCH API to look for past tweets by keyword/s. 
# We can only access tweets that have been sent in the last 6-9 days. 
# We will search for the last 1,000 tweets mentioning "Jeb Bush":

tweets <- searchTwitter(c('"Jeff Bush"'), n=1000)

# For windows machines (?):
# searchTwitter('"Jeff Bush"', cainfo = system.file("CurlSSL", 
# "cacert.pem", package = "RCurl"))

tweets <-  do.call("rbind", lapply(tweets, as.data.frame))


# 6) USING THE 'STREAMING API'

# We can only collect up to 1% of the tweets that are being sent
# at any moment. 
# When reaching that limit, Twitter returns a sample (random?) 
# Here we capture all tweets mentioning the bigram 'climate change' 
# during a one minute time period.

filterStream(file.name = "tweets_keyword.json", 
             track = c('Obama'),
             timeout = 60, oauth = my_oauth)

# ...  you should see in the console: "Capturing tweets..." 

streamtweets <- parseTweets("tweets_keyword.json", verbose = T)


# 7) SENTIMENT ANALYSIS

# Here we collect the last 1,000 tweets mentioning Clinton and 
# classify them into POSTIVE and NEGATIVE using a 
# Supervised Machine Learning Algorithm.

# Collecting the tweets

clinton <- searchTwitter(c('clinton'), n=1000)
clinton <-  do.call("rbind", lapply(clinton, as.data.frame))

# Obtaining labeled examples of POSITIVE and NEGATIVE statements

# We use a subset from a database of labeled movie reviews created 
# by a research group at Stanford.
# http://help.sentiment140.com/for-students/)

url = "http://andreucasas.com/labeled_tweets_sent.csv"
labeled = read.csv(file=url)

names(labeled)
table(labeled$label)

library(tm)

# Cleaning (pre-processing) the texts of the reviews and tweets
labeled$text <- as.character(labeled$text)
Encoding(labeled$text) <- 'UTF-8'
labeled$text <- iconv(labeled$text, "UTF-8", "ASCII", sub="")

txt <- gsub('@\\S+', '', labeled$text) # removing @usernames
txt <- gsub('#\\S+', '', txt) # removing #hashtags
txt <- Corpus(VectorSource(txt), readerControl = list(language = "en"))
txt <- tm_map(txt, stripWhitespace) 
txt <- tm_map(txt, removePunctuation)
txt <- tm_map(txt, tolower)
txt <- tm_map(txt, removeWords, stopwords("english")) 
txt <- tm_map(txt, removeNumbers)
txt <- tm_map(txt, stemDocument, language= "english")
txt <- tm_map(txt, PlainTextDocument)
labeled$cleantext<- as.character(txt)
# For PC users:
#labeled$cleantext <- unlist(sapply(txt,'content'))

# Removing non-ASCII characters from the tweets

clinton$text <- as.character(clinton$text)
Encoding(clinton$text) <- 'UTF-8'
clinton$text <- iconv(clinton$text, "UTF-8", "ASCII", sub="")

txt2 <- gsub('@\\S+', '', clinton$text) # removing @usernames
txt2 <- gsub('#\\S+', '', txt) # removing #hashtags
txt2 <- Corpus(VectorSource(clinton$text), readerControl = list(language = "en"))
txt2 <- tm_map(txt2, stripWhitespace) 
txt2 <- tm_map(txt2, removePunctuation)
txt2 <- tm_map(txt2, tolower)
txt2 <- tm_map(txt2, removeWords, stopwords("english")) 
txt2 <- tm_map(txt2, removeNumbers)
txt2 <- tm_map(txt2, stemDocument, language= "english")
txt2 <- tm_map(txt2, PlainTextDocument)
clinton$cleantext<- as.character(txt2)
# For PC users: !!!
# clinton$cleantext <- unlist(sapply(txt2,'content'))

# Using RTextTools (SVM algorithm) to test how well a sample of 1000 labeled reviews can predict 1000 other reviews

library(RTextTools)  # install.packages('RTextTools')
doc_matrix <- create_matrix(labeled$cleantext, language="english",
                            removeNumbers=F,stemWords=F,
                            removeSparseTerms=.998)
container <- create_container(doc_matrix, labeled$label, 
                            trainSize=1:1000,testSize=1001:2000,
                            virgin=FALSE)
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)
analytics <- create_analytics(container,SVM_CLASSIFY)
performance <- analytics@algorithm_summary

print(performance) 

# ... not amazing performance ...

# Now we create a new dataset merging the labeled movie-reviews 
# and the unlabeled tweets. 
# Then we will train the SVM algorithm on the movie reviews 
# and use it to classify the tweets.

clinton2 <- as.data.frame(cbind(as.character(clinton$text),
                clinton$cleantext))
clinton2$label <- NA
colnames(clinton2) <- c('text','cleantext','label')
maindb <- rbind(labeled,clinton2)

# Train and Classify

doc_matrix2 <- create_matrix(maindb$cleantext, language="english",
                            removeNumbers=F,stemWords=F,
                            removeSparseTerms=.998)
container2 <- create_container(doc_matrix2, maindb$label, 
                              trainSize=1:2000,
                              testSize=2001:length(maindb[,1]),
                              virgin=TRUE)
SVM2 <- train_model(container2,"SVM")
SVM_CLASSIFY2 <- classify_model(container2, SVM2)

View(SVM_CLASSIFY2)
result <- as.data.frame(t(as.numeric(table(SVM_CLASSIFY2$SVM_LABEL))/1000))
colnames(result) <- c('neg','pos')

print(result) # Lets look at the output!
