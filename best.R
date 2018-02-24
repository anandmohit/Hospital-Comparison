best <- function(state_code, outcome){
  setwd("F:\\iStudy\\Data World\\Coursera_DS\\Programming Assigment 3")
  oocm<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
checkState <- unique(oocm[,7])
if(!(state_code %in% checkState)) stop ("invalid state")

oocm<-subset(oocm,oocm[,7]==state_code)

l_sym<- if(outcome == "heart attack"){oocm[, c(2,11)]}
       else if( outcome == "heart failure") {oocm[, c(2,17)]}
       else if( outcome == "pneumonia") {oocm[,c(2,23)]}
       else stop("invalid outcome")

suppressMessages(l_sym[,2]<- as.numeric(l_sym[,2]))
l_sym<-subset(l_sym,!is.na(l_sym[,2]))
top_sym <- head(l_sym[order(l_sym[,2],l_sym[,1]),1],1)
return (top_sym)

}


best("SC", "heart attack")