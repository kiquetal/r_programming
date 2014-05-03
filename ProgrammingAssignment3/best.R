best <- function(state,outcome)
{
  
  file=read.csv("outcome-of-care-measures.csv",TRUE)
  if (outcome != "heart attack" && outcome != "heart failure" && outcome !="pneumonia")
    stop("invalid outcome")
 
  row_states=subset(file,select=State,subset=(State==state))
  if (length(row.names(row_states))<1)
     stop("invalid state")
  caseUpper <- sapply(outcome,function(x){ 
                              s <-strsplit(x," ")[[1]] 
                                  paste(toupper(substring(s,1,1)),substring(s,2),sep="",collapse=" ")})

  consta <- paste("Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from",caseUpper)  
  consta <-gsub("\\(|\\)|\\s|\\-","\\.",consta)
#  consta <-gsub("\\(|\\)","\\.",consta)
  colNumber <-which(colnames(file)==consta)
  dt <- subset(file,select=c(colNumber,2),subset=(State==state))
 dt <-na.omit(dt)
 dt <-dt[dt[,1]!="Not Available",]
 dt <-dt[order(as.numeric(as.character(dt[,1])),dt[,2]),]
 name <-dt[1,]
 realName <-as.character(name[[2]])
 realName
  #order(as.numeric(as.character(esto[,1]))),]
}

