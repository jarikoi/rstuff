# Set wd, remove when submitting
setwd("/Users/jkoister/miscdev/rclass/rprog-data-ProgAssignment3-data")

best<-function(state,outcome){
  ##read outcome data
  if(!valid_state(state)){
    stop("invalid state")
  }
  
  if(!valid_outcome(outcome)){
    stop("invalid outcome")
  }
  
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## chect state and outcome are valid
  
  ## Return hospital name in hat that state with lowest 30-day death rate
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