rankhospital<-function(state_code, outcome, num = "best"){
  setwd("F:\\iStudy\\Data World\\Coursera_DS\\Programming Assigment 3")
  oocm<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  checkState<- unique(oocm[,7])
  
  if(!(state_code %in% checkState)) stop("invalid state")
  
  if(!(num=="best"||num=="worst"||is.numeric(num))) stop("invalid number")
  
  oocm <- subset(oocm,oocm[,7]==state_code)
  
  l_sym<- if(outcome=="heart attack"){oocm[,c(2,11)]}
  else if(outcome =="heart failure"){oocm[,c(2,17)]}
  else if(outcome =="pneumonia"){oocm[,c(2,23)]}
  else stop("invalid outcome")
  
  suppressWarnings(l_sym[,2]<-as.numeric(l_sym[,2]))
  l_sym <- subset(l_sym,!is.na(l_sym[,2]))
  l_sym <- l_sym[order(l_sym[,2],l_sym[,1]),1]
  
  fin_out<- if(num=="best"){head(l_sym,1)}
  else if (num =="worst"){tail(l_sym,1)}
  else l_sym[num]
  
  return (fin_out)
}