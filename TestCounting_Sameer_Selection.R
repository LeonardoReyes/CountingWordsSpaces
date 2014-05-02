# Scripted by: J.L. Reyes Acosta
# e-mail: leonardo.reyes@mindshareworld.com
# MindShare WW Business planning
# SCRIPT TO COUNT DISTANCES BETWEEN ADJETIVES AND NOUNS


oldwd<-getwd()
setwd("C:\\Users\\Leonardo.Reyes\\Documents\\Projects\\CountingWords\\CountingWordsSpaces")


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
if (!require("FactoMineR")) {
  install.packages("FactoMineR")
  library(FactoMineR)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
source("CountingFunctions.R")

## loading Sentence
#str1 <- c("Coke is great for lunch it makes me happy","Coke is great makes me happy at lunch")
#Twitter<-read.csv("sysomos-content-2013-08-06.csv",  header = TRUE,sep = ",") 

setwd("C:\\Users\\Leonardo.Reyes\\Documents\\Projects\\CountingWords\\CountingWordsSpaces\\MS_USA")

Adjectives<-read.xlsx("Adjectives.xlsx",1) 

Brands<-read.xlsx("Brands.xlsx",1) 

BrandsCount<-list()
MasterTable<-matrix(,nrow=length(Brands$Brand),ncol=length(Adjectives$English))
rownames(MasterTable)<- as.character(Brands$Brand)
colnames(MasterTable)<- as.character(Adjectives$English)
MasterTable <- as.table(MasterTable)

for (j in 1:length(Brands$Brand)){
  setwd("C:\\Users\\Leonardo.Reyes\\Documents\\Projects\\CountingWords\\CountingWordsSpaces\\Carlsberg")
  res <- read.csv(paste("Twitter_Conversations_",j,".csv",sep=""))  # read the first sheet
  ##Selecting and cleaning conversations
  
  Content<-as.character(res$contents)
  
  Content<-removeURL(Content)
  #Content<-removepunctuationLeo(Content)
  Content<-removeExtraSpaces(Content)
  Content<-tolowerLeo(Content)
  
  if(j==3){Content<-gsub("jose cuervo","jose_cuervo",Content)
           Content<-gsub("jose-cuervo","jose_cuervo",Content)}
  
  if(j==4){Content<-gsub("jack daniels","jack_daniels",Content)
           Content<-gsub("jack daniels","jack_daniels",Content)}
  if(j==6){Content<-gsub("sailor jerry","sailor_jerry",Content)
           Content<-gsub("sailor jerry","sailor_jerry",Content)}
  
  ##Counting Spaces
  
  for(i in 1:length(Adjectives$English)){
    TestTwitterSpaces<-CountingSpaces(Content,as.character(Adjectives$English[i]),as.character(Brands$Brand[j])) 
    TestTwitterCounting<-CountingWords(Content,as.character(Adjectives$English[i]),as.character(Brands$Brand[j]))
    
    if (i==1){Spaces<-list(TestTwitterSpaces)
              Words<-list(TestTwitterCounting)}
    else{Spaces<-append(Spaces,list(TestTwitterSpaces))
         Words<-append(Words,list(TestTwitterCounting))}
    # Making Tables for Correspindance analysis
    MasterTable[j,i]<-length(TestTwitterSpaces)
  }
  
  
  SpacesTest<-data.frame(cbind(melt(Spaces)[,3],as.character(Adjectives$English[melt(Spaces)[,4]])))
  colnames(SpacesTest)<-c("Spaces","Adjectives")
  
  eval(parse(text=paste("BrandsCount<-c(BrandsCount,\"",as.character(Brands$Brand[j]),"\"=SpacesTest)",sep ="")))
  #eval(parse(text=paste("write.csv(SpacesTest,\"",as.character(Brands$Brand[j]),"Adjectives_Analysis.csv\")",sep ="")))
}


## Correspondence Analysis
#write.csv(MasterTable, file = "Soft_drinks_Adjectives_RAW.csv",row.names=TRUE) # Test for Sameer
MasterDistill <- read.csv("Soft_drinks_Adjectives_Distilled.csv",row.names=1)  # Test for Sameer

res<-CA(MasterDistill, ncp=5, row.sup=NULL, col.sup=NULL, graph = TRUE)
#res<-CA(MasterTable, ncp=5, row.sup=NULL, col.sup=NULL, graph = TRUE)
# plot.CA(res, axes=c(1, 2), col.row="red", col.col="blue", label=c("col","col.sup", "row", "row.sup"),cex=.7)

df.gg <- data.frame(dim1 = c(res$col$coord[,1],res$row$coord[,1]), 
                    dim2 = c(res$col$coord[,2],res$row$coord[,2]),
                    type=c(colnames(mytable),rownames(mytable)))

#setwd(oldwd)
write.csv(df.gg, file = "Map_Adjectives_Distilled_1.csv",row.names=FALSE)


df.gg <- data.frame(dim1 = c(res$col$coord[,1],res$row$coord[,1]), 
                    dim2 = c(res$col$coord[,2],res$row$coord[,2]),
                    type=c(rep(1,length(res$col$coord[,1])),rep(2,length(res$row$coord[,1]))))

theme_set(theme_grey(25))
m <- ggplot(df.gg,aes(x=dim1,y=dim2,group=factor(type)))
m + geom_point(aes(colour=factor(type)),size=1) +
  geom_text(aes(label=rownames(df.gg),size=factor(type),
                colour=factor(type)))+
  scale_x_continuous("",breaks=0,labels="",expand=c(0.1,0.2))+
  scale_y_continuous("",breaks=0,labels="",expand=c(0.1,0.2))+
  scale_size_manual(values=c(2.8, 6.5))+
  scale_colour_manual(values=c("red","purple"))+
  opts(legend.position="none")

#ggsave("Drinks_Adjectives.pdf",width=15,height=7)


#Rank Top adjectives
sort(colSums(MasterTable),decreasing=TRUE)[1:10]

SumsAdjec<-rowSums(MasterTable)
for(k in 1:length(SumsAdjec)){
  MasterTable[k,]<-MasterTable[k,]/SumsAdjec[k]
}
