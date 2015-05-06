# rankhospital.R

# rankhospital that takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num). 
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the ranking specified by the num argument.
# The num argument can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are better). 
# If the number given by num is larger than the number of hospitals in that state, then the function should return NA. 
# Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
# Ties are handled in the same way as best.R
# For example, the call rankhospital("MD", "heart failure", 5) returns a character vector containing the name of the hospital with the 5th lowest 30-day death rate for heart failure. 


rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv") 
  
  # Check that state abbreviation is valid
  if (!is.element(state,unique(outcomedata$State))){
    stop("Invalid State")
  }
  
  # Initialize character vector to be returned
  name <- character()
  
  # Find name of the hospital in given state with given rank for the specified outcome
  if (outcome=='heart attack'){
    statedata <-subset(outcomedata, outcomedata[,7] == state & outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
    if (identical(num,"best")){
      num <- 1
    } else if (identical(num,"worst")){
      num <- nrow(statedata)
    }
    if (num > nrow(statedata)){stop("NA")}
    sorted <-statedata[order(as.numeric(as.character(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),statedata$Hospital.Name),]
    name <- as.character(sorted$Hospital.Name[num])
    
  } else if (outcome == 'heart failure'){
    statedata <-subset(outcomedata, outcomedata[,7] == state & outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
    if (identical(num,"best")){
      num <- 1
    } else if (identical(num,"worst")){
      num <- nrow(statedata)
    }
    if (num > nrow(statedata)){stop("NA")}
    sorted <-statedata[order(as.numeric(as.character(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),statedata$Hospital.Name),]
    name <- as.character(sorted$Hospital.Name[num])
  } else if (outcome == 'pneumonia'){
    statedata <-subset(outcomedata, outcomedata[,7] == state & outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available")
    if (identical(num,"best")){
      num <- 1
    } else if (identical(num,"worst")){
      num <- nrow(statedata)
    }
    if (num > nrow(statedata)){stop("NA")}
    sorted <-statedata[order(as.numeric(as.character(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),statedata$Hospital.Name),]
    name <- as.character(sorted$Hospital.Name[num])
  } else {
    # Throw stop if outcome is not one of "heart attack","heart failure", or "pneumonia"
    stop("invalid outcome")
  }
  return(name)
}