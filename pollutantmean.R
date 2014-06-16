pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
 m<-mean(bind(directory,pollutant,id))
 #debug("mean:",m)
 return(round(m,3))
}

bind<-function(directory,pollutant,id){
  files<-c()
  for (x in 1:length(id)){
    df=getdf(directory,id[x])
    df<-df[pollutant]
    df<-df[!is.na(df)]
    files<-c(files,df)
  }
  file<-cbind(files)
  return(files)
}



getdf<-function(directory,id){
  name<-createName(directory,id)
  df<-read.csv(name)
  return (df)
}

createName<- function(directory,id) {
  directory<-paste(directory,"/",sep="")
  name<-paste(directory,idStr(id),sep="")
  name<-paste(name,".csv",sep="")
  #debug("FNAME",name)
  return(name)
}

idStr <- function(id){
  if (id < 10 ) return(paste("00",toString(id),sep=""))
  if (id > 9 & id < 100 )  return(paste("0",toString(id),sep=""))
  return(id)
}

debug<-function(lable,value){
  cat(lable,value)
}