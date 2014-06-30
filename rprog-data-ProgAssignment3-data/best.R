# Set wd, remove when submitting
setwd("/Users/jkoister/miscdev/rclass/rprog-data-ProgAssignment3-data")


#cStatus ... jamfor numerid med string nar jag skall plocka ur hospital
val_outcomes<-c("heart attack","heart failure","pneumonia")
val_states<-c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","MD","MA","MI","MN","MS","MO","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
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
  # Add numeric column with values, called num
  state_outcome_dfn<-get_outcome_wnum(state_outcome_df,outcome)
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

get_best<-function(df,mvalue){

  min.df <- df[df$num==mvalue,]
  sort.df <- with(min.df,  min.df[order(min.df$Hospital.Name), ])
  return(sort.df)
  stop("Wrong outcome string")
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



########### OLD can be removed
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

######## just a help function, used it to find  suitable test data with multiple results
get_all<-function(outcome){
  full_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings=c("Not Available"))
  clean_df<-na.omit(full_df)
  # Get outcome data
  col_n<-col_outcome(outcome)
  outcome_df<-clean_df[,c('Hospital.Name','State',col_n)]
  for (i in val_states) {
    cat("************STATE STATE STATE", i)
    state_outcome_df<-outcome_df[outcome_df$State==i,]
    state_outcome_dfn<-get_outcome_wnum(state_outcome_df,outcome)
    b<-get_best(state_outcome_dfn,min(state_outcome_dfn$num))
    print(b)
  }
}
