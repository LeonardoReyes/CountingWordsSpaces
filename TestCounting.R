
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
 str2 <- gsub(' {2,}',' ',Twitter$contents)
 #str2 <- gsub(' {2,}',' ',str1)

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
   return(Output)
   }
   
 }

 
 CountingWords <- function(Sentence,Adjective,Brand){
   RelativePositionAdjective<-which(strsplit(Sentence,' ')[[1]] == Adjective)
   RelativePositionBrand<-which(strsplit(Sentence,' ')[[1]] == Brand)
   WordsBetween<-if(RelativePositionAdjective-RelativePositionBrand>0){RelativePositionAdjective-RelativePositionBrand-1} 
   else{RelativePositionAdjective-RelativePositionBrand+1}
   return(WordsBetween)
 }
 
 #Test<-CountingSpaces(str2,"great","Coke")
 #CountingWords(str2,"great","Coke")
 TestTwitter<-CountingSpaces(str2,"beautiful","American") 

