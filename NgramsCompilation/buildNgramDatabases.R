options(stringsAsFactors = F)
library(tm)
library(RWeka)
library(R.utils)
library(stringr)
library(stringi)
library(qdap)
library(Matrix)
library(dplyr)
library(data.table)

########## FUNCTIONS ##############

#Function to create clean corpus:
# (2) clean data: remove twitter handle, emails, URLs, punctuation, numbers, extra white spaces, stop words & transform all words to lower case
# (3) remove profanity
getCleanCorpus <- function(docsText,profanityWords){
  
  #Remove non-English characters
  docsText <- iconv(docsText,"latin1","ASCII",sub="")
  
  #Remove specific words found highly frequent: www, dot, com
  docsText <- str_replace_all(docsText,"www","")
  docsText <- str_replace_all(docsText,"dot","")
  docsText <- str_replace_all(docsText,"com","")
  
  #Remove repetition of words
  docsText <- str_replace_all(docsText,"(\\w+[:space:]){1,}\\1+","")
  
  #Remove text within brackets
  docsText <- bracketX(docsText)
  
  #Replace abbreviations
  docsText <- replace_abbreviation(docsText)
  
  #Replace contractions
  docsText <- replace_contraction(docsText)
  
  #Create corpus object
  corpusObj <- VCorpus(VectorSource(docsText))
  
  #Regex function
  cleanUsingRegex <- content_transformer(function(w,regexExp) gsub(regexExp," ",w))
  
  #Remove Twitter #
  corpusObj <- tm_map(corpusObj,cleanUsingRegex,"@[^\\s]{1,15}")
  
  #Remove emails
  corpusObj <- tm_map(corpusObj,cleanUsingRegex,"^[[:alnum:].-_]+@[[:alnum:].-]+$")
  
  #Remove URLs
  corpusObj <- tm_map(corpusObj,cleanUsingRegex,"(f|ht)tp[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
  
  #Remove punctuation
  corpusObj <- tm_map(corpusObj,removePunctuation)
  
  #Remove numbers
  corpusObj <- tm_map(corpusObj,removeNumbers)
  
  #Remove extra white spaces
  corpusObj <- tm_map(corpusObj,stripWhitespace)
  
  #Transform into lower case
  corpusObj <- tm_map(corpusObj,content_transformer(tolower))

  #Remove profanity words
  corpusObj <- tm_map(corpusObj, removeWords, profanityWords)
  
  corpusObj <- tm_map(corpusObj, PlainTextDocument)
  
  #Return
  return(corpusObj)
}

#Functions to tokenize into n-gram
N2gramToken <- function(w){NGramTokenizer(w,Weka_control(min=2,max=2))}
N3gramToken <- function(w){NGramTokenizer(w,Weka_control(min=3,max=3))}
N4gramToken <- function(w){NGramTokenizer(w,Weka_control(min=4,max=4))}

#Function to get frequency table of N-grams
getTokensFreq <- function(corpusText,ngram){
  
  # Term document obj depending on N-gram
  if(ngram==2){
    tdm <- TermDocumentMatrix(corpusText, control = list(tokenize = N2gramToken))
  }else if(ngram == 3){
    tdm <- TermDocumentMatrix(corpusText, control = list(tokenize = N3gramToken))
  }else{
    tdm <- TermDocumentMatrix(corpusText, control = list(tokenize = N4gramToken))
  }
  
  #Convert to sparseMatrix
  mtx <-  Matrix::sparseMatrix(i=tdm$i,j=tdm$j,x=tdm$v,dims=c(tdm$nrow, tdm$ncol),dimnames = tdm$dimnames)
  rm(tdm)
  
  # Frequency
  freq <- sort(Matrix::rowSums(mtx), decreasing = TRUE)
  # Freq fataframe
  freqdf <- data.table("Token"=names(freq),"Freq"=freq)
  # Add first ngram - 1 word in one column and add following word in another
  if(ngram==2){
    #Unique first word
    freqdf$firstW <- sapply(freqdf$Token,function(x)strsplit(x,split=" ")[[1]][1])
  }else if(ngram == 3){
    #Unique first & second word combination
    freqdf$firstW <- sapply(freqdf$Token,function(x)paste(strsplit(x,split=" ")[[1]][1:2],collapse=" "))
  }else{
    #Unique first, second and third word combination
    freqdf$firstW <- sapply(freqdf$Token,function(x)paste(strsplit(x,split=" ")[[1]][1:3],collapse=" "))
  }
  
  #Order by firstW and frequency
  freqdf <- freqdf[order(freqdf$firstW, -freqdf$Freq)] 
  
  #Select for each first word the three most frequent
  freqdftop3 <- freqdf %>% group_by(firstW) %>% slice(1:3) 
  freqdftop3 <- data.table("Token"=freqdftop3$Token,"Freq"=freqdftop3$Freq,"firstW"=freqdftop3$firstW)
  
  #Remove frequencies = 1
  freqdftop3 <- freqdftop3[which(freqdftop3$Freq > 1),]
  
  #Clean
  rm(mtx)
  rm(freq)
  rm(freqdf)
  gc()
  
  #Return
  return(freqdftop3)
}

#####################################

#Path to the data folder
pathData <- "~/COURSERA/final/en_US/"
pathBadWords <- "~/COURSERA/"

#Load profanity words
#Downloaded from https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en
conBadWords <- file(paste0(pathBadWords,"profanityWords.txt"), "r")
profanityWords <- VectorSource(readLines(conBadWords))
close(conBadWords)
rm(conBadWords)
saveRDS(profanityWords,"profanityWords.rds")

#Load the data 

#Twitter
conTwits <- file(paste0(pathData,"en_US.twitter.txt"), "r") 
twits <- readLines(conTwits,skipNul = TRUE)
close(conTwits)
rm(conTwits)

#Blogs
conBlogs <- file(paste0(pathData,"en_US.blogs.txt"), "r") 
blogs <- readLines(conBlogs,skipNul = TRUE)
close(conBlogs)
rm(conBlogs)

#News
conNews <- file(paste0(pathData,"en_US.news.txt"), "r") 
news <- readLines(conNews,skipNul = TRUE)
close(conNews)
rm(conNews)

#Combine all data
allData <- c(twits,blogs,news)

#Remove non-English characters from the data
allData <- iconv(allData, "latin1", "ASCII", sub = "")

#Free space
rm(twits)
rm(blogs)
rm(news)
gc()

#Sample 20k of the data 10 times to compute the databases

#For reproducibility
seedVec <- c(1234,2345,3456,4567,5678,6789,7890,4321,5674,3421)
#List that will contain the sampled data
datalist <- list()
#Sampling
for(ti in seq(1,10)){
  #Set seed
  set.seed(seedVec[ti])
  #Sample data
  datalist[[ti]] <- sample(allData,size=20000,replace=F)
}
#Clean
rm(allData)
gc()

#For each element of datalist - build N2 grams
#Init
wcorpus <- getCleanCorpus(datalist[[1]],profanityWords)
n2g <- getTokensFreq(wcorpus,2)
rm(wcorpus)
gc()
for(i in seq(2,length(datalist))){
  print(i)
  wcorpus <- getCleanCorpus(datalist[[i]],profanityWords)
  n2gi <- getTokensFreq(wcorpus,2)
  n2g <- rbind(n2g,n2gi)
  n2g <- aggregate(n2g$Freq,by=list("Token"=n2g$Token,"firstW"=n2g$firstW),sum)
  colnames(n2g) <- c("Token","firstW","Freq")
  n2gdf <- n2g[order(n2g$firstW, -n2g$Freq),] 
  #Select for each first word the three most frequent
  n2gdf <- n2gdf %>% group_by(firstW) %>% slice(1:3) 
  n2g <- data.table("Token"=n2gdf$Token,"Freq"=n2gdf$Freq,"firstW"=n2gdf$firstW)
  rm(n2gdf)
  rm(n2gi)
  gc()
}
#Save
saveRDS(n2g,"N2gramDb.rds")
#free memory 
rm(n2g)
gc()

#For each chunk - N3 grams
wcorpus <- getCleanCorpus(datalist[[1]],profanityWords)
n3g <- getTokensFreq(wcorpus,3)
rm(wcorpus)
gc()
for(j in seq(2,length(datalist))){
  print(j)
  wcorpus <- getCleanCorpus(datalist[[j]],profanityWords)
  n3gi <- getTokensFreq(wcorpus,3)
  n3g <- rbind(n3g,n3gi)
  n3g <- aggregate(n3g$Freq,by=list("Token"=n3g$Token,"firstW"=n3g$firstW),sum)
  colnames(n3g) <- c("Token","firstW","Freq")
  n3gdf <- n3g[order(n3g$firstW, -n3g$Freq),] 
  #Select for each first word the three most frequent
  n3gdf <- n3gdf %>% group_by(firstW) %>% slice(1:3) 
  n3g <- data.table("Token"=n3gdf$Token,"Freq"=n3gdf$Freq,"firstW"=n3gdf$firstW)
  rm(n3gdf)
  rm(n3gi)
  gc()
}
#Save
saveRDS(n3g,"N3gramDb.rds")
#free memory 
rm(n3g)
gc()

#For each chunk - N4 grams
wcorpus <- getCleanCorpus(datalist[[1]],profanityWords)
n4g <- getTokensFreq(wcorpus,4)
rm(wcorpus)
gc()
for(z in seq(2,length(datalist))){
  print(z)
  wcorpus <- getCleanCorpus(datalist[[z]],profanityWords)
  n4gi <- getTokensFreq(wcorpus,4)
  n4g <- rbind(n4g,n4gi)
  n4g <- aggregate(n4g$Freq,by=list("Token"=n4g$Token,"firstW"=n4g$firstW),sum)
  colnames(n4g) <- c("Token","firstW","Freq")
  n4gdf <- n4g[order(n4g$firstW, -n4g$Freq),] 
  #Select for each first word the three most frequent
  n4gdf <- n4gdf %>% group_by(firstW) %>% slice(1:3) 
  n4g <- data.table("Token"=n4gdf$Token,"Freq"=n4gdf$Freq,"firstW"=n4gdf$firstW)
  rm(n4gdf)
  rm(n4gi)
  gc()
}
#Save
saveRDS(n4g,"N4gramDb.rds")
#free memory 
rm(n4g)
gc()
####################################################
