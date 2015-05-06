#best.R

#best that take two arguments: the 2-character abbreviated name of a state and an outcome name. 
# The function reads the outcome-of-care-measures.csv file (from http://hospitalcompare.hhs.gov/) 
# and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state. 
# The hospital name is the name provided in the Hospital.Name variable. 
# The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. 
# Hospitals that do not have data on a particular outcome were be excluded from the set of hospitals when deciding the rankings.
# In the case of a tie, the hospital that comes first alphabetically is returned.

best <- function(state, outcome) {
  
  #  Read data for the outcome of care measures
  outcomedata <- read.csv("outcome-of-care-measures.csv")  
  
  # Check that state abbreviation is valid
  if (!is.element(state,unique(outcomedata$State))){
    stop("Invalid State")
  }
  
  # Pick out only data concerning the state of interest
  statedatapre <-subset(outcomedata, outcomedata[,7] == state)
  
  # Initialize the character vector to be returned
  name <- character()
  
  # Find name of the hospital in given state with lowest 30-day mortality for the specified outcome
  if (outcome=='heart attack'){
    statedata <- statedatapre[statedatapre$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available", ]
    name <- subset(statedata,as.numeric(as.character(statedata[,11]))==min(as.numeric(as.character(statedata[,11]))),select=Hospital.Name)
  } else if (outcome == 'heart failure'){
    statedata <- statedatapre[statedatapre$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available", ]
    name <- subset(statedata,as.numeric(as.character(statedata[,17]))==min(as.numeric(as.character(statedata[,17]))),select=Hospital.Name)
  } else if (outcome == 'pneumonia'){
    statedata <- statedatapre[statedatapre$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available", ]
    name <- subset(statedata,as.numeric(as.character(statedata[,23]))==min(as.numeric(as.character(statedata[,23]))),select=Hospital.Name)
  } else {
    # Throw stop if outcome is not one of "heart attack","heart failure", or "pneumonia"
    stop("invalid outcome")
  }
  
  if (length(name) >1){
    # In case of a tie, return the one that comes first when sorted into alphabetical order.
    return(sort(as.character(name))[[1]])
  } else {return(as.character(name[[1]]))}
}