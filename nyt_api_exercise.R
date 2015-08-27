#######################################
## SCRAPPING NEW YORK TIMES ARTICLES ##
## APSA'15 Text as Data Workshop     ##
## Sep. 3, 2015 (9am - 5pm)          ##
## Andreu Casas and John Wilkerson   ##
#######################################

# 1) OBTAINING NYT-API KEY

# To use the New York Times API you need first to register to the NYT
#     website and then get an API Key. Go to the following web-address
#     (http://developer.nytimes.com/apps/register), click to 'Register'
#     (upper-right corner), enter your email and create a password.
#     Then go back to the initial web address and click to Log In.
#     Finally, go to the APIs tab and click to '1.Request an API key'.
#     Enter a 'fake'/temporary name for your application (e.g. Test),
#     select the box 'Issue a new key for Article Search API', scroll 
#     down to the bottom, agree to the terms of service, and click to 
#     'Register Application'. You have already created your key! You
#     can always see your keys in the 'Keys' site of your NYT developer
#     profile. 


# 2) INSTALLING THE 'rtimes' PACKAGE

install.packages("devtools")
devtools::install_github("ropengov/rtimes")
library("rtimes")


# 3) SCRAPPING SOME ARTICLES: Mentioning 'Donald Trump' and 'Ted Cruz' in Aug. 2015

# Enter your key here"
options(nytimes_as_key = "847566eb0d04582e1c182aa0944910b5:5:72765732") 

trump <- NULL
cruz <- NULL

    # Searching articles on D.Trump:

search <- as_search(q='"Donald Trump"', begin_date = "20150801",
                    end_date = '20150901',page=p)$data
p <- 1
while (length(search)!=0) {
  search <- as_search(q='"Donald Trump"', begin_date = "20150801",
                      end_date = '20150901',page=p)$data
  for (i in 1:length(search)) {
    article <- search[[i]]
    headline <- article$headline$main
    subtitle <- article$headline$sub
    abstract <- article$snippet
    date <- article$pub_date
    url <- article$web_url
    word_count <- article$word_count
    info <- NULL
    listInfo <- list(headline,subtitle,abstract,date,url,word_count)
    for (j in 1:length(listInfo)) {
      if (is.null(listInfo[[j]])) {
        info <- c(info,'')
      } else{
        info <- c(info,listInfo[[j]])
      }
    }
    trump <- rbind(trump,info)
  }
  p <- p + 1
}


    # Searching articles on J.Bush:

p <- 1
search <- as_search(q='"Ted Cruz"', begin_date = "20150801",
                    end_date = '20150901',page=p)$data
while (length(search)!=0) {
  search <- as_search(q='"Ted Cruz"', begin_date = "20150801",
                      end_date = '20150901',page=p)$data
  for (i in 1:length(search)) {
    article <- search[[i]]
    headline <- article$headline$main
    subtitle <- article$headline$sub
    abstract <- article$snippet
    date <- article$pub_date
    url <- article$web_url
    word_count <- article$word_count
    info <- NULL
    listInfo <- list(headline,subtitle,abstract,date,url,word_count)
    for (j in 1:length(listInfo)) {
      if (is.null(listInfo[[j]])) {
        info <- c(info,'')
      } else{
        info <- c(info,listInfo[[j]])
      }
    }
    cruz <- rbind(cruz,info)
  }
  p <- p + 1
}

colnames(trump) <- c('headline','subtitle','abstract','date','url','word_count')
colnames(cruz) <- colnames(trump)
trump <- as.data.frame(trump)
cruz <- as.data.frame(cruz)

# 3) PLOTTING THE NUMBER OF NEWS STORIES ON EACH CANDIDATE BY DAY

trump$date2 <- as.Date(strptime(trump$date, '%Y-%m-%dT%H:%M:%S'))
cruz$date2 <- as.Date(strptime(cruz$date, '%Y-%m-%dT%H:%M:%S'))

plot(table(trump$date2),type='l',ylab='Num. Stories (NYT)',
     xlab='September 2015',axes=F,lwd=1.5,
     xlim=c(0,max(table(trump$date2))),col='blue3')
lines(as.numeric(table(cruz$date2)),lwd=1.5,col='green4')
axis(2,las=2)
axis(1)
legend('topright',legend=c('Trump','Cruz'),col=c('blue3','green4'),bty='n',
       lty=1,lwd=2)


# 4) WHAT ARE THE MOST USED ADJECTIVES IN TRUMP'S AND CRUZ'S NEWS STORIES?
#     (Using Part of Speech Tags)

trumpAdj <- NULL
cruzAdj <- NULL

# Installing/Loading the packages 'NLP' and 'openNLP'
install.packages('NLP')
install.packages('openNLP')
library(NLP)
library(openNLP)

# Gathering all the adjectives of each abstract:

    # Trump

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
adjTags <- c('JJ','JJR','JJS')

for (art in trump$abstract) {
  if (art=='') {
    next
  }
  art <- as.String(art)
  art2 <- annotate(art, list(sent_token_annotator, word_token_annotator))
  art3 <- annotate(art, pos_tag_annotator, art2)
  for (i in 1:length(art3)) {
    if (art3[i]$type=='sentence') {
      next
    }
    word <- substring(art,art3[i]$start,art3[i]$end)
    tag <- art3[i]$features[[1]][[1]]
    if (tag %in% adjTags){
      trumpAdj <- c(trumpAdj,word)
    }
  }
}

    # Cruz

for (art in cruz$abstract) {
  if (art=='') {
    next
  }
  art <- as.String(art)
  art2 <- annotate(art, list(sent_token_annotator, word_token_annotator))
  art3 <- annotate(art, pos_tag_annotator, art2)
  for (i in 1:length(art3)) {
    if (art3[i]$type=='sentence') {
      next
    }
    word <- substring(art,art3[i]$start,art3[i]$end)
    tag <- art3[i]$features[[1]][[1]]
    if (tag %in% adjTags){
      cruzAdj <- c(cruzAdj,word)
    }
  }
}

# 5) ORDERING THE RESULTS AND GETTING RELATIVE FREQUENCIES

trumpAdj <- as.data.frame(sort(table(trumpAdj),decreasing=T))
cruzAdj <- as.data.frame(sort(table(cruzAdj),decreasing=T))
colnames(trumpAdj) <- c('abs')
colnames(cruzAdj) <- c('abs')
trumpAdj$rel <- trumpAdj$abs/sum(trumpAdj$abs)
cruzAdj$rel <- cruzAdj$abs/sum(cruzAdj$abs)

# Print the Top-10 adjectives for the stories related to each Rep. candidate
print(head(trumpAdj,10))
print(head(cruzAdj,10))

