# Scripted by: J.L. Reyes Acosta
# e-mail: leonardo.reyes@mindshareworld.com
# MindShare WW Business planning

#MAIN FUCTION FOR COUNTING, MODIFY AT YOUR OWN RISK :P


#dataSys.corpus <- Corpus(VectorSource(res$contents))
#summary(dataSys.corpus)

## Cleaning extra spaces
removeExtraSpaces <- function(x) gsub(' {2,}',' ',x)
removepunctuationLeo <- function(x)str_replace_all(x, "[(),.:;@#!?/\\*-_]", "")                

#str2 <- gsub(' {2,}',' ',str1)
#dataSys.corpus <- tm_map(dataSys.corpus, tolower)
removeURL <- function(x) gsub("http://[[:alnum:]]*", "", x)
# makeCombination <- function(x) {
#   y <- gsub('diamonds','diamond', x)
#   y <- gsub('rings','ring', x)
#   return(y)
# }
tolowerLeo<-function(x){y<-x
                        for (i in 1:length(LETTERS)){
                          y<-chartr(LETTERS[i],letters[i],y)
                        }                    
                        return(y)
}


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