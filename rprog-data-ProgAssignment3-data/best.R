# Set wd, remove when submitting
setwd("/Users/jkoister/miscdev/rclass/rprog-data-ProgAssignment3-data")

best<-function(state,outcome){
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
 # mvalue<-min(state_outcome_df$col_n)
 #mvalue<-get_min(state_outcome_df,outcome)
 
 if(outcome=='heart attack') { 
  values<-state_outcome_df["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]
  valuesn<-data.matrix(values)
   mvalue<-min(valuesn )
   debug("ha",mvalue)
 }
 if(outcome=='heart failure') { 
   values<-state_outcome_df["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
   valuesn<-data.matrix(values)
   mvalue<-min(valuesn)
   debug("hf",mvalue)
 }
 if(outcome=='pneumonia'){
   values<-state_outcome_df["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]
   valuesn<-data.matrix(values)
   mvalue<-min(valuesn)
   debug("p",mvalue)
 }
  #best_outcomes<-state_outcome_df[state_outcome_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==mvalue,]
 #best_outcomes<-state_outcome_df[state_outcome_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==mvalue,]
 ## Return hospital name in hat that state with lowest 30-day death rate
 
best_outcomes<-get_best(state_outcome_df,outcome,mvalue)
#best_outcomes<-get_best2(state_outcome_df,col_n,mvalue)
  return(best_outcomes)
}

# returns true or false
val_states<-c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","MD","MA","MI","MN","MS","MO","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
valid_state<-function(state){
  return(state %in% val_states)
}

val_outcomes<-c("heart attack","heart failure","pneumonia")
valid_outcome<-function(o){
  return(o %in% val_outcomes)
}

col_outcome<-function(o){
  ha<-c("heart attack")
  if(o=='heart attack') { return('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')}
  if(o=='heart failure') { return('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')}
  if(o=='pneumonia'){return('Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')}
  stop("Wrong outcome string")
}

get_best<-function(df,outcome,mvalue){
  if(outcome=='heart attack') { 
    return(df[df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==mvalue,])
  }
  if(outcome=='heart failure') { 
    return(df[df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==mvalue,])
  }
  if(outcome=='pneumonia'){
    return(df[df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==mvalue,])
  }
  stop("Wrong outcome string")
}

get_best2<-function(df,outcome,mvalue){
    return(df[df$outcome==mvalue,])
}


get_min<-function(df,outcome){
  if(outcome=='heart attack') { 
    return(min(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ))
  }
  if(outcome=='heart failure') { 
    return(min(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  }
  if(outcome=='pneumonia'){
    return(min(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  }
  stop("Wrong outcome string")
}


debug<-function(lable,value){
  cat(lable,value)
}