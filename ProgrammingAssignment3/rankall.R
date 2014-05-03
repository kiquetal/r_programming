rankall <-function(outcome,num="best")
{
   table=read.csv("outcome-of-care-measures.csv",TRUE)
   
   if (outcome != "heart attack" && outcome != "heart failure" && outcome !="pneumonia")
      stop("invalid outcome")
   if (num!="best" && num!="worst" && !is.numeric(num))
    stop("invalid state")
   all_states=subset(table,select=c(7))
    vector_states=as.vector(levels(all_states[,1]))
   caseUpper <- sapply(outcome,function(x){
     s <-strsplit(x," ")[[1]]
     paste(toupper(substring(s,1,1)),substring(s,2),sep="",collapse=" ")})
   consta <- paste("Hospital 30-Day Death (Mortality) Rates from",caseUpper)
   consta <-gsub("\\(|\\)|\\s|\\-","\\.",consta)
   
   low_morta=which(colnames(table)==consta)
   print(low_morta)
   real_data_frame <-data.frame(hospital=character(50),state=character(2))[0,]
   for (i in 1:length(vector_states))
    {
      
      state=vector_states[i]
      print(state)
      dt <-subset(table,select=c(low_morta,2),subset=(State==state))
      dt<-dt[dt[,1]!= "Not Available",]
      print("que sucede")
      if (nrow(dt)<1)
        {
        print("VACIO")
        new_row<-data.frame(hospital=NA,state=state)
      }
      else
      {
        print(paste("iNGREO STATO ",state))
        dt[,1]<-as.numeric(as.character(dt[,1]))
        orderNado <-dt[order(dt[,1],dt[,2]),]
        if (num=="worst")
          {
          h <-(tail(orderNado,1))[[2]]
        }
        else
        {
          if (num=="best")
          {
            h<-(head(orderNado,1))[[2]]
            
          }
          else
          {
            
            h <-orderNado[num,2]
            
          }
        }
        s <-state
        new_row <-data.frame(hospital=h,state=s)
      }
      
      real_data_frame <-rbind(real_data_frame,new_row)
    }
  real_data_frame
}
