rankhospital <-function(state,outcome,num="best")
{
  
  file=read.csv("outcome-of-care-measures.csv",TRUE)
  if (outcome != "heart attack" && outcome != "heart failure" && outcome !="pneumonia")
    stop("invalid outcome")
  if (num !="best" && num!="worst" && !is.numeric(num))
  {
    stop("invalid num")
  } 
  row_states=subset(file,select=State,subset=(State==state))
  if (length(row.names(row_states))<1)
    stop("invalid state")
  row=nrow(row_states)
  if (is.numeric(num) && num>row)
       "NA"
  else
  {
  caseUpper <- sapply(outcome,function(x){
    s <-strsplit(x," ")[[1]]
    paste(toupper(substring(s,1,1)),substring(s,2),sep="",collapse=" ")})
  consta <- paste("Hospital 30-Day Death (Mortality) Rates from",caseUpper)
  consta <-gsub("\\(|\\)|\\s|\\-","\\.",consta)
  
  low_morta=which(colnames(file)==consta)
  dt <-subset(file,select=c(low_morta,2),subset=(State==state))
  dt<-dt[dt[,1]!="Not Available",]
  dt[,1]<-as.numeric(as.character(dt[,1]))
  dt<-dt[order(dt[,1],dt[,2]),]  
  
  if (num=="worst")
  {
    as.character(tail(dt,1)[[2]])
    
  }
  else
  {
    if (num=="best")
    {
      as.character(dt[1,][[2]])
    }
    else
    {
      as.character(dt[num,][[2]])
    }
  }
  
  
}
}
