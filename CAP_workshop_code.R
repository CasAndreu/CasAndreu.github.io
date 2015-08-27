########################################
## BASIC TEXT PROCESSING AND ANALYSIS ##
## CAP'15 Text as Data Workshop       ##
## June 22, 2015 (Lisbon)             ##
## John Wilkerson and Andreu Casas    ##
########################################

#'tm' is the primary package for pre-processing text. Other R packages offer analysis tools (such as RTextTools for supervised machine learning). 

#tm reference manual offers example scripts for each function: http://cran.r-project.org/web/packages/tm/tm.pdf

#This example brings in .htm files stored in a local folder, strips unwanted formatting, creates the features, and converts the text to data for analysis.

#The example data are meeting transcripts from the Federal Reserve Board  http://www.federalreserve.gov/monetarypolicy/fomc_historical.htm

#My advice is to run each block of code below in sequence. .

######################################
#Skip this. Downloading pages

#DownThemAll is a free Firefox add on that makes it easy to download url linked webpages or documents
#https://addons.mozilla.org/en-US/firefox/addon/downthemall/

######################################
EXAMPLE STARTS HERE

#download (or copying and pasting) the 3 html files at http://faculty.washington.edu/jwilker/DGo2015/FOMC/ to a folder on your machine

# Install needed packages (only need to do this once)

install.packages("RCurl")
#install.packages("XML")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")

# Load packages (need to do this each time you open R)

library(RCurl)
library(XML)
library(tm)
library(SnowballC)
library(wordcloud)


#######################################
#SET YOUR WORKING DIRECTORY Tell R where to look for these files

setwd('path_to_the_folder_with_html_files')
# setwd("C:/Users/John/Desktop/DGO") #requires forward / slashes

getwd() #tells you what directory you are curently in

#######################################
# Step 1. Converting HTML to plain text

# Skip this, but if you wanted to read and parse an ONLINE HTML file you would do this:

fomc1 <- htmlTreeParse('http://faculty.washington.edu/jwilker/DGo2015/FOMC/FOMC20081216meeting.htm', useInternal = TRUE)

# Instead, we are going to process the three .html files in your folder

# First create an 'object' (called 'fomc') that includes the list of filenames from your folder that end in .htm or .html 

fomc <- list.files(pattern="\\.(htm|html)$") # get just .htm and .html files from folder

# Loop through that list of names, reading in the file content and storing each as an html object in a vector or 'list' called 'docs). 

docs  <- NULL
for (file in fomc) {
  html <- htmlTreeParse(file, useInternal=T)
  docs <- c(docs,html)
}

# Create a new 'list' (pars) that contains a list of  paragraphs for each of the three files in docs (using the '/p' delimiter in the html encoding, starting at the root of the document). 

pars <- NULL 
for (i in 1:length(docs)){
  p <- unlist(xpathApply(docs[[i]], '//p', xmlValue))
  p <- list(p)
  pars <- c(pars,p)
} # 'pars' is a list of 3 objects. 
# Each object is a list of the paragraphs in each doc


# Concatenate the lists of paragraphs into 3 strings (txt)

txt <- NULL
for (p in pars) { # for each list of paragraphs
  tx <- NULL
  for (i in 1:length(p)) { # for each paragraph in each list
    tx <- paste(tx, as.character(p[i]), sep= ' ')
  }
  tx <- list(tx)
  txt <- c(txt,tx)
}


# Remove and replace all \n, \u html formatting with spaces, save as a new (3 document) list called txt2
# could do lots more cleaning here if desired)

txt2 <- NULL # txt2 will be a list of 3 "clean" strings
for (tx in txt) {
  tx <- gsub('\\n', ' ', tx) # substitute space for \n
  list(tx)
  txt2 <- c(txt2,tx)
}


# Ignore this: Write out (export) if desired
#	write(txt2, file = ("C:/Users/John/Desktop/DGO", txt2.txt)) 

# 
# Ignore this: Load a file
# newtext = read.table("C:/Users/John/Desktop/Programs/R/fomccorpus/txt2.txt")


#########################
# STEP 2: CREATE A CORPUS
  
# Create a 'corpus' from txt2. A corpus is a special kind of vector/list that is helpful for applying text processing commands to lots of texts at once. The tm_map command below does this and only works for files that are part of a corpus. 

corp <-Corpus(VectorSource(txt2), readerControl = list(language = "en"))
corp <- tm_map(corp, PlainTextDocument)


# Ignore this: Write out a Corpus, if desired

#writeCorpus(corp, path="C:/Users/John/Desktop/DGO")

# Ignore this: Load a Corpus, if desired

#setwd('C:/Users/John/Desktop/Programs/R/fomccorpus') 
#fomcimport  <-Corpus(DirSource(C:/Users/John/Desktop/DGO") , readerControl = list(language="en")) 
#summary(fomcimport)

-----------------------------------------------------
######################################  
# STEP 3: FEATURE SELECTION

# The goal here is to refine document content in ways that make it more useful for analysis. For example, we probably don't want to be counting common 'stopwords' like 'the;' and we probably want to treat words like happy and happiness as instances of the same thing (by stemming to 'happ' in each case). There's a lot you can do here depending on your goals.


#     Eliminating Extra Whitespace

corp <- tm_map(corp, stripWhitespace) 

#     Eliminating Punctuation

corp <- tm_map(corp, removePunctuation)


#     Eliminating Numbers

corp <- tm_map(corp, removeNumbers)

#     Convert to Lower Case

corp <- tm_map(corp, tolower)

#     Remove Stopwords

corp <- tm_map(corp, removeWords, stopwords(kind="en"))

#     Stem words in a text document using Porter's stemming algorithm (must install SnowballC package).

corp <- tm_map(corp, stemDocument, language= "english")

#	Another option is to eliminate rare words. In this case, all but the 3% of least common words will be retained.

corp <-removeSparseTerms(corp, sparse=.97)


###############################################

# STEP 4: NOW WE CONVERT THE TEXT TO DATA
# by creating a document-term matrix where each row is a document and each column is a word or feature found in at least one of the documents. The cells are simply counts of the number of times that word appears in the document. 

corp <- tm_map(corp, PlainTextDocument)

tm <-DocumentTermMatrix(corp)
tm <-removeSparseTerms(tm, sparse=.97)

# Take a look at the values of columns 20-30 for all three docs 

inspect(tm[1:3, 20:30])  # rows 1-3, terms 20-30


# Take a look a terms that show up at least twice

findFreqTerms(tm,2)


# Sometimes a Term Document Matrix is required (document is column rather than row)

dtm <- TermDocumentMatrix(corp, control = list(minWordLength = 3))

inspect(dtm[20:30, 1:3])

-------------------------------------------------------
###############################################
#STEP 5: INITIAL DATA EXPLORATION

# Which 'features' or words are fairly common?

	findFreqTerms(dtm, lowfreq=10) # 10 or more
	findFreqTerms(dtm, lowfreq=80)

# Which words are most common?

	sort(rowSums(as.matrix(dtm)), decreasing=TRUE)
	
# As I understand it, this asks for the list of words that co-occur with chairman (across the three documents) at least 67% of the time. Probably a long list.

	findAssocs(dtm, "chairman",.67)

#Lots of other options in the tm package

################################################
# STEP 6: CREATING A WORD CLOUD
# Which words show up most often across the documents?

dtm.matrix <- as.matrix(dtm) #don't ask why, just do it!
dtm.sort <- sort(rowSums(dtm.matrix),decreasing=TRUE)
dtm.last <- data.frame(word = names(dtm.sort),freq=dtm.sort)
	table(dtm.last$freq)
	pal2 <- brewer.pal(7,"BrBG")
	png(file = "fomc.png", width=1280, height=800)
	wordcloud(dtm.last$word,dtm.last$freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
	dev.off()

#wordcloud is just one of many R packages for analyzing text! 

###################
# STEP 7: WRITE OUT THE DATA FILE
#This should convert to a .csv file and export it to your current working directory

write.csv(dtm.last, file = "tadah.csv",row.names=F)

#And if you wanted to bring that file back into R

NewData <- read.csv(file="tadah.csv", sep=",") #have to be in the right directory obviously




