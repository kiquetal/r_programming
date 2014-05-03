pollutantmean <- function(directory, pollutant, id = 1:332)
  {
  dataFrame <-data.frame(ids=NA,sum=NA,rows=NA,mean=NA)[0,]
  for (i in seq_along(id))
  {
    pos <-sprintf("%03d",id[i]) 
#    print(pos)
    dir <-paste(directory,"/",pos,".csv",sep="")
#    print(dir)
    myFile <-read.csv(dir,TRUE)
    t<-nrow(myFile)
 #   print (pollutant)
    if (pollutant=="nitrate"){
    myFile <-subset(myFile,!is.na(nitrate),select=c(nitrate))
    sumF <-sum(myFile$nitrate)
    rows <-nrow(myFile)
    }
    else
    {
      if (pollutant == "sulfate")
      {
      myFile <-subset(myFile,!is.na(myFile$sulfate))
      
      sumF <-sum(myFile$sulfate)
      rows <-nrow(myFile)
       }
    }

    dataFrame <-rbind(dataFrame,data.frame(ids=id[i],sum=sumF,rows=rows,mean=sumF/rows))
    
  }
  d<-round((sum(dataFrame$sum)/sum(dataFrame$rows)),3)
  print(d)
}
