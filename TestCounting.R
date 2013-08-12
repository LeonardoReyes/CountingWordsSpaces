
oldwd<-getwd()

setwd("C:\\Users\\Leonardo.Reyes\\Documents\\Projects\\ForeverMark\\Conversation_Analysis\\Twitter")

library(tm)
library(igraph)
library(reshape2)
library(rJava)
library(Snowball)

## loading Sentence
 str1 <- c("Coke is great for lunch it makes me happy","Coke is great makes me happy at lunch")
 Twitter<-Rawdata<-read.csv("sysomos-content-2013-08-06.csv",  header = TRUE,sep = ",") 

##CreatingCorpus


dataSys.corpus <- Corpus(VectorSource(Twitter$contents))
summary(dataSys.corpus)

## Cleaning extra spaces
removeExtraSpaces <- function(x) gsub(' {2,}',' ',x)
#str2 <- gsub(' {2,}',' ',str1)
dataSys.corpus <- tm_map(dataSys.corpus, tolower)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
makeCombination <- function(x) {
  y <- gsub('diamonds','diamond', x)
  y <- gsub('rings','ring', x)
  return(y)
}
dataSys.corpus <- tm_map(dataSys.corpus, removeURL)
dataSys.corpus <- tm_map(dataSys.corpus, stripWhitespace)
dataSys.corpus <- tm_map(dataSys.corpus, removePunctuation)
dataSys.corpus <- tm_map(dataSys.corpus, makeCombination)
## Stemming



dataSys.corpus.copy <- dataSys.corpus
dataSys.corpus <- tm_map(dataSys.corpus, stemDocument,language='english')

CleanDataFrame<-as.data.frame(dataSys.corpus)
CleanDataFrame.T <- t(CleanDataFrame[,1:ncol(CleanDataFrame)])


 #strsplit(str2,' ')[[1]]
 #length(strsplit(str2,' ')[[1]])-1
 
 #Searching for specific word and counting Spaces
 CountingSpaces <- function(Sentence,Adjective,Brand){
   for (i in 1:length(Sentence)){
   RelativePositionAdjective<-which(strsplit(Sentence[i],' ')[[1]] == Adjective)
   RelativePositionBrand<-which(strsplit(Sentence[i],' ')[[1]] == Brand)
   SpacesBetween<-RelativePositionAdjective-RelativePositionBrand
   if (i==1){Output<-as.matrix(SpacesBetween)}else{Output<-as.matrix(rbind(Output,SpacesBetween))}
   
   #rownames(Output)<-c(1:length(Sentence))
   
   }
   return(Output)
 }

 
 CountingWords <- function(Sentence,Adjective,Brand){
   for (i in 1:length(Sentence)){
     RelativePositionAdjective<-which(strsplit(Sentence[i],' ')[[1]] == Adjective)
     RelativePositionBrand<-which(strsplit(Sentence[i],' ')[[1]] == Brand)
     if (length(RelativePositionAdjective)>0 & length(RelativePositionBrand)>0){
        WordsBetween<-if(RelativePositionAdjective-RelativePositionBrand>0)
          {RelativePositionAdjective-RelativePositionBrand-1} 
        else{RelativePositionAdjective-RelativePositionBrand+1}
        }
     else{WordsBetween<-as.integer()
       }
     if (i==1){Output<-as.matrix(WordsBetween)}else{Output<-as.matrix(rbind(Output,WordsBetween))}
   }
   return(Output)
 }
 #Test<-CountingSpaces(str2,"great","Coke")
 #CountingWords(str2,"great","Coke")
 
TestTwitterSpaces<-CountingSpaces(CleanDataFrame.T,"ring","diamond") 
TestTwitterCounting<-CountingWords(CleanDataFrame.T,"ring","diamond") 


