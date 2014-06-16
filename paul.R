corrAnon <- function(directory,threshold=0){
  
  filelist <- dir(directory, full.names=TRUE)
  data <- data.frame()
  cor2 <- vector()
  for (i in 1:332) {
    debug("i",i)
   # CHANGE 1 data <- rbind(data, read.csv(filelist[i]),header=TRUE)   
   data <- read.csv(filelist[i])
    data2 <- na.omit(data)
    nobs <- sum(complete.cases(data2))  
    if (nobs > threshold) {
    
      #CHANGE 2 cor2 <- c(cor2,cor(data2$sulfate, data2$nitrate)) 
      cor2 <- c(cor2,cor(data2["sulfate"], data2["nitrate"])) 
    }
  }

  cor2
}



corr <- function(directory = "specdata", threshold = 150) {
  
  files_list <- list.files(directory, pattern = ".csv", full.names = TRUE)
  n <- length(files_list)
  debug("n",n)
  dat <- data.frame()
  dat1<-vector("numeric")
  for (i in 1:n) 
  {
    dat <- read.csv(files_list[i])
    complete <- dat[complete.cases(dat),]
    sumComplete <- nrow(complete)
    if (sumComplete > threshold) 
    {
      dat2  <- cor(complete["sulfate"], complete["nitrate"])
      dat1 <- c(dat1, dat2)      
    }               
  }
  return(dat1)
}

debug<-function(lable,value,nl=FALSE){
  if(nl){
    cat(lable,value,"\n")
  }else{
    cat(lable,value)
  }
}