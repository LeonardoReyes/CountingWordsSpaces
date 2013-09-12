# Scripted by: J.L. Reyes Acosta
# e-mail: leonardo.reyes@mindshareworld.com
# MindShare WW Business planning
# SCRIPT TO COUNT DISTANCES BETWEEN ADJETIVES AND NOUNS


oldwd<-getwd()

#setwd("C:\\Users\\Leonardo.Reyes\\Documents\\Projects\\ForeverMark\\Conversation_Analysis\\Twitter")

library(tm)
library(igraph)
library(rJava)
library(Snowball)
if (!require("xlsx")) {
  install.packages("xlsx")
  library(xlsx)
}
if (!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}
if (!require("reshape2")) {
  install.packages("reshape2")
  library(reshape2)
}
source("CountingFunctions.R")

## loading Sentence
 #str1 <- c("Coke is great for lunch it makes me happy","Coke is great makes me happy at lunch")
 #Twitter<-read.csv("sysomos-content-2013-08-06.csv",  header = TRUE,sep = ",") 



Adjectives<-read.xlsx("Polish_Adjectives.xlsx",1) 

PolishBrands<-read.xlsx("PolishBrands.xlsx",1) 

BrandsCount<-list()

for (j in 1:length(PolishBrands$Brand)){

res <- read.xlsx("TwitterConversations.xlsx", j,encoding="UTF-8")  # read the first sheet

##Selecting and cleaning conversations

Content<-res$contents

Content<-removeURL(Content)
Content<-removepunctuationLeo(Content)
Content<-removeExtraSpaces(Content)
Content<-tolowerLeo(Content)

##Counting Spaces

for(i in 1:length(Adjectives$Polish)){
  TestTwitterSpaces<-CountingSpaces(Content,as.character(Adjectives$Polish[i]),as.character(PolishBrands$Brand[j])) 
  TestTwitterCounting<-CountingWords(Content,as.character(Adjectives$Polish[i]),as.character(PolishBrands$Brand[j]))

  if (i==1){PolandSpaces<-list(TestTwitterSpaces)
            PolandWords<-list(TestTwitterCounting)}
  else{PolandSpaces<-append(PolandSpaces,list(TestTwitterSpaces))
       PolandWords<-append(PolandWords,list(TestTwitterCounting))}
}


PolandSpacesTest<-data.frame(cbind(melt(PolandSpaces)[,3],as.character(Adjectives$English[melt(PolandSpaces)[,4]])))
colnames(PolandSpacesTest)<-c("Spaces","Adjectives")

eval(parse(text=paste("BrandsCount<-c(BrandsCount,\"",as.character(PolishBrands$Brand[j]),"\"=PolandSpacesTest)",sep ="")))
eval(parse(text=paste("write.csv(PolandSpacesTest,\"",as.character(PolishBrands$Brand[j]),"Adjectives_Analysis.csv\")",sep ="")))
}

