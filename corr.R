corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  options(digits=4)
  files<-getfiles(directory)
  corrs<-vector('numeric')
  for (i in 1:length(files)){
    df<-getdf2(directory,files[i])
    cdf<-df[complete.cases(df),]
    if(nrow(cdf)>threshold){
      #debug("nrow",nrow(cdf),TRUE)
      corrs<-c(corrs,getcorr(cdf))
    }
   # cat(nrow(df),":",completeObs(df,files[i]),"\n")
  }
  return(corrs)
}

getcorr<-function(cdf){
  digits=3
  ccdf<-cor(cdf["nitrate"],cdf["sulfate"],use="pairwise.complete.ob")
  #print(ccdf)
  return(ccdf)
}

completeObs<-function(df,no){
  df<-df[complete.cases(df),]
  cc<-nrow(df)
  return (c(no,cc))
}

getdf2<-function(directory,name){
  df<-read.csv(createName2(directory,name))
  return (df)
}

createName2<- function(directory,id) {
  directory<-paste(directory,"/",sep="")
  name<-paste(directory,id,sep="")
  return(name)
}

idStr <- function(id){
  if (id < 10 ) return(paste("00",toString(id),sep=""))
  if (id > 9 & id < 100 )  return(paste("0",toString(id),sep=""))
  return(id)
}

debug<-function(lable,value,nl=FALSE){
  if(nl){
    cat(lable,value,"\n")
  }else{
    cat(lable,value)
  }
}