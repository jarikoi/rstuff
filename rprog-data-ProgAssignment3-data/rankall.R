


# Set wd, remove when submitting
setwd("/Users/jkoister/miscdev/rclass/rprog-data-ProgAssignment3-data")


#cStatus ... jamfor numerid med string nar jag skall plocka ur hospital
val_outcomes<-c("heart attack","heart failure","pneumonia")
#val_states<-c("AK","AL","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","MD","MA","MI","MN","MS","MO","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
val_states<-c("AK","AL","AZ","AR","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")

rankall <- function(outcome, num = "best") {
  allrank<-NULL
  for (is in val_states){
    tmp<-rankhospital(is,outcome,num)
    allrank<-rbind(allrank,tmp)
  }
  df<-as.data.frame(allrank)
  row.names(df)<-val_states
  colnames(df)<-c("hospital","state")
  return(df)
}


rankhospital <- function(state, outcome, numi = "best") {
  ## Read outcome data
  ##read outcome data
  full_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings=c("Not Available"))
  ## chect state and outcome are valid
  if(!valid_state(state)){
    stop("invalid state")
  }
  if(!valid_outcome(outcome)){
    stop("invalid outcome")
  }
  # Get column name
  col_n<-col_outcome(outcome)
  # Remove NA, could ne done after column selection for better performance

  
  # Get outcome data
  outcome_df<-full_df[,c('Hospital.Name','State',col_n)]
  clean_df<-na.omit(outcome_df)
  state_outcome_df<-clean_df[clean_df$State==state,]
  # Add numeric column with values, called num
  state_outcome_dfn<-get_outcome_wnum(state_outcome_df,outcome)
  #return(state_outcome_dfn)
  state_outcome_dfo<-state_outcome_dfn[order(state_outcome_dfn$num,state_outcome_dfn$Hospital.Name),]
  #state_outcome_dfo<-state_outcome_dfn[order(state_outcome_dfn$num),]
  
  # If num is 0, return all
  if(numi == "all") {
    # Return all
    return(state_outcome_dfo)
  }
  if(numi == "best") {
    # Return first
    return(c(state_outcome_dfo[1,]$Hospital.Name,state))
    stop("Wrong outcome string")
  }
  if(numi == "worst") {
    # Return last
    return(c(state_outcome_dfo[nrow(state_outcome_dfo),]$Hospital.Name,state))
  }
  # Return number numi
  theone<-state_outcome_dfo[numi,]
  return(c(theone$Hospital.Name,theone$State)) 

}


get_best<-function(df){
  mvalue<-min(df$num)
  min.df <- df[df$num==mvalue,]
  sort.df <- with(min.df,  min.df[order(min.df$Hospital.Name), ])
  return(sort.df[1,])
  stop("Wrong outcome string")
}


get_worst<-function(df){
  mvalue<-max(df$num)
  max.df <- df[df$num==mvalue,]
  sort.df <- with(max.df,  max.df[order(max.df$Hospital.Name), ])
  return(sort.df[1,])
  stop("Wrong outcome string")
}


best<-function(state,outcome){
  
  mvalue<-max(df$num)
  mvalue<-min(state_outcome_dfn$num)
  best_outcomes<-get_best(state_outcome_dfn,mvalue)
  #best_outcomes<-get_best2(state_outcome_df,col_n,mvalue)
  return(best_outcomes$Hospital.Name)
}

# Map outcome to column name
col_outcome<-function(o){
  ha<-c("heart attack")
  if(o=='heart attack') { return('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')}
  if(o=='heart failure') { return('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')}
  if(o=='pneumonia'){return('Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')}
  stop("Wrong outcome string")
}

# Adds a numeriv column that can be used to find min and then select appropriate rows
get_outcome_wnum<-function(df,outcome){
  if(outcome=='heart attack') { 
    values<-df["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]
    valuesn<-data.matrix(values)
    colnames(valuesn)<-c("num")
    return(cbind(df,valuesn))
  }
  if(outcome=='heart failure') { 
    values<-df["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
    valuesn<-data.matrix(values)
    mvalue<-min(valuesn)
    colnames(valuesn)<-c("num")
    return(cbind(df,valuesn))
  }
  if(outcome=='pneumonia'){
    values<-df["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]
    valuesn<-data.matrix(values)
    mvalue<-min(valuesn)
    colnames(valuesn)<-c("num")
    return(cbind(df,valuesn))
  }
  stop("problem in get_outcome_wnum")
}


valid_state<-function(state){
  return(state %in% val_states)
}

valid_outcome<-function(o){
  return(o %in% val_outcomes)
}

### Just a simple debug function
debug<-function(lable,value){
  cat(lable,value)
}


rankhospital2 <- function(state, outcome, numi = "best") {
  ## Read outcome data
  ##read outcome data
  full_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings=c("Not Available"))
  ## chect state and outcome are valid
  if(!valid_state(state)){
    stop("invalid state")
  }
  if(!valid_outcome(outcome)){
    stop("invalid outcome")
  }
  # Get column name
  col_n<-col_outcome(outcome)
  # Remove NA, could ne done after column selection for better performance
  clean_df<-na.omit(full_df)
  
  # Get outcome data
  outcome_df<-clean_df[,c('Hospital.Name','State',col_n)]
  state_outcome_df<-outcome_df[outcome_df$State==state,]
  # Add numeric column with values, called num
  state_outcome_dfn<-get_outcome_wnum(state_outcome_df,outcome)
  #return(state_outcome_dfn)
  state_outcome_dfo<-state_outcome_dfn[order(state_outcome_dfn$num,state_outcome_dfn$Hospital.Name),]
  #state_outcome_dfo<-state_outcome_dfn[order(state_outcome_dfn$num),]
  
  # If num is 0, return all
  if(numi == "all") {
    # Return all
    return(state_outcome_dfo)
  }
  if(numi == "best") {
    # Return first
    #return(c(state_outcome_dfo[1,]$Hospital.Name,state))
    best=get_best(state_outcome_dfn)
    return(c(best$Hospital.Name,state))
    stop("Wrong outcome string")
  }
  if(numi == "worst") {
    # Return last
    wor=get_worst(state_outcome_dfn)
    return(c(wor$Hospital.Name,state))
  }
  # Return number numi
  theone<-state_outcome_dfo[numi,]
  return(c(theone$Hospital.Name,theone$State)) 
  
}

