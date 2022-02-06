library(shiny)
library(shinythemes)
library(markdown)
library(stringr)
library(stringi)
library(qdap)
library(tm)

######### Graphical User Interface ###############
ui <- fluidPage(
          theme = shinytheme("sandstone"),
          navbarPage("NWP",
                 tabPanel("App",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Instructions"), 
                              h5("1. Enter word(s) in the text box"),
                              h5("2. The top three most probable words are displayed"),
                              h6("The predictions are automatic, no need to submit your entry"),
                              h6("The next words predicted are ordered:"),
                              h6("Top 1 in back"),
                              h6("Top 2 in dark grey"),
                              h6("Top 3 in ligh grey"),
                              h6("A question mark indicates there is no prediction for your entry")
                            ),
                            mainPanel(
                              textInput("words", h3("Input:"), 
                                        value = "All you"),
                              h3("Predicted next word(s):"),
                              h4(em(span(textOutput("pred1"), style="color:black"))),
                              h4(em(span(textOutput("pred2"), style="color:darkgrey"))),
                              h4(em(span(textOutput("pred3"), style="color:lightgrey")))
                            )
                          )
                        ),
                 tabPanel("About",
                          # Application title
                          titlePanel("Next Word Prediction (NWP)"),
                          p("NWP takes as an input multiple words and provides the top three most probable words."),
                          p("This app has been created for the Data Science Capstone project proposed by Coursera:"),
                          uiOutput("linkProject")
                 )
              )
          )

##############################################

########## Sever logic ######################

#Load N-gram frequency databases
db2Ngram <- readRDS("./data/N2gramDb.rds")
db3Ngram <- readRDS("./data/N3gramDb.rds")
db4Ngram <- readRDS("./data/N4gramDb.rds")

server <- function(input, output) {
  
  urlCoursera <- a("https://www.coursera.org/learn/data-science-project", href="https://www.coursera.org/learn/data-science-project")
  output$linkProject <- renderUI({
    tagList("Capstone Project Coursera:", urlCoursera)
  })
  
  w1 <- reactive(predNextWord(input$words,1))
  w2 <- reactive(predNextWord(input$words,2))
  w3 <- reactive(predNextWord(input$words,3))

  output$pred1 <- renderText({w1()})
  output$pred2 <- renderText({w2()})
  output$pred3 <- renderText({w3()})

}

#' Function to predict next word based on input w
#' 
#' @param w user input word(s)
#' @param topn top n predicted word to return
#' 
#' @return the predicted word based on the user input
predNextWord <- function(w,topn) {
  
  #Clean user input
  w <- cleanEntry(w)
  
  #Empty entry
  if (w == "") {
    res <- "?"
  }else if(length(strsplit(w," ")[[1]]) == 1) {
    #User entered 1 word, looking for the next one using N2-grams
    res <- runModel(w,2)
  }else if(length(strsplit(w," ")[[1]]) == 2){
    #User entered 2 words, looking for the next one using N3-grams
    res <- runModel(w,3)
  }else if (length(strsplit(w," ")[[1]]) > 2) {
    #If the user entered more than 2 words, select the last one ones and proceed with N4-grams for the prediction
    ws <- strsplit(w," ")[[1]]
    n <- length(ws)
    res <- runModel(paste(ws[(n-2):n],collapse=" "),4)
    if(length(res)==1){
      #If res empty > try 3 grams
      if(res==""){
        res <- runModel(paste(ws[(n-1):n],collapse=" "),3)
        if(length(res)==1){
          #If res empty > try 2 grams
          if(res==""){
            res <- runModel(ws[n],2)
          }
        }
      }
    }
  }
  
  if(length(res)==0){
    resrender <- "?"  
  }else if(length(res)==3){
    resrender <- res[topn]
  }else if(length(res)==2){
    if(topn<=2){
      resrender <- res[topn]
    }else{
      resrender <- "?"
    }
  }else{
    if(topn==1){
      resrender <- res[topn]
    }else{
      resrender <- "?"
    }
  }
  
  #Return
  return(resrender)
}

#' Function to clean the user entry as the grams database were cleaned
#' 
#' @param w user input word(s)
#' @param badwords profanity words
#' 
#' @return cleaned entry
cleanEntry <- function(w){
  
  #Remove non-English characters
  wclean <- iconv(w,"latin1","ASCII",sub="")
  
  #Remove repetition of words
  wclean <- str_replace_all(wclean,"(\\w+[:space:]){1,}\\1+","")
  
  #Remove text within brackets
  wclean <- bracketX(wclean)
  
  #Replace abbreviations
  wclean <- replace_abbreviation(wclean)
  
  #Replace contractions
  wclean <- replace_contraction(wclean)
  
  #Regex function
  cleanUsingRegex <- content_transformer(function(w,regexExp) gsub(regexExp," ",w))
  
  #Remove Twitter #
  wclean <- gsub("@[^\\s]{1,15}","",wclean)
  
  #Remove emails
  wclean <- gsub("^[[:alnum:].-_]+@[[:alnum:].-]+$","",wclean)
  
  #Remove URLs
  wclean <- gsub("(f|ht)tp[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+","",wclean)
  
  #Remove punctuation
  wclean <- gsub("[,.';:!?]","",wclean)
  
  #Remove numbers
  wclean <- gsub("[0123456789]{1,}","",wclean)
  
  #Remove extra white spaces (up to two)
  wclean <- gsub("  "," ",wclean)
  
  #Transform into lower case
  wclean <- tolower(wclean)
  
  return(wclean)
}

#' Function to run the model
#' 
#' @param w clean user input word(s)
#' @param n N-gram database to use
#' 
#' @return top three predicted next words
runModel <- function(w,n){
  
  #If N-gram = 2
  if(n == 2){
    matchTokens <- db2Ngram[str_detect(db2Ngram$Token,paste0("^(",w,")[:space:]")),]
    nw <- 2
  }else if(n == 3){
    #If N-gram = 3
    matchTokens <- db3Ngram[str_detect(db3Ngram$Token,paste0("^(",w,")[:space:]")),]
    nw <- 3
  }else{
    #If N-gram = 4 (and more)
    matchTokens <- db4Ngram[str_detect(db4Ngram$Token,paste0("^(",w,")[:space:]")),]
    nw <- 4
  }
  
  if(nrow(matchTokens)>0){
    #Order by frequency
    matchTokens <- matchTokens[order(matchTokens$Freq,decreasing = T),]
    #Return the three top predicted words
    predWord <- sapply(matchTokens$Token,function(x)strsplit(x," ")[[1]][nw])
    if(length(predWord)>3){
      predWord <- predWord[1:3]
    }
  }else{
    predWord <- ""
  }
  return(predWord)
}

##############################################

shinyApp(ui, server)