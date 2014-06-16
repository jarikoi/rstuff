setwd("/Users/jkoister/miscdev/rclass")
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  #files<-getfiles(directory)
  #printList(files)
  dirs<-c()
  result<-data.frame()

  for (i in 1:length(id)){
 # debug("i=",i)
  file<-getdf(directory,id[i])
  n<-completeObs(file,id[i])
  dirs<-rbind(dirs,c(i,n))
  #rresrow<-c(createName(directory,id[i]),n)
  result<-rbind(result,n)
  #print(n)
  }

  #print(dirs, row.names=FALSE)
 #prettyprint(dirs)
 colnames(result)<-c("id","nobs")
 return(result)

}

prettyprint<-function(dirs){
  cat("##     id nobs \n")
  for (i in 1:nrow(dirs)){
    #print(dirs[i,], row.names=FALSE)
    cat("##  ",dirs[i,],"\n")
   #print( format(dirs[i,],justify=c("right")))
    #l<-dirs[i,]
    #print(paste("##",l))
}
}

prettyprint2<-function(dirs){
  cat("##     id nobs \n")
  cat("##  ",dirs[i,],"\n")
}

completeObs<-function(df,no){
  df<-df[complete.cases(df),]
  cc<-nrow(df)
  return (c(no,cc))
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