# rankall.R

# rankall takes two arguments: an outcome name (outcome) and a hospital rank- ing (num). 
# The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame containing the hospital in each state that has the ranking specified in num. 
# The function should return a value for every state (some may be NA). 
# The first column in the data frame is named hospital, which contains the hospital name, and the second column is named state, which contains the 2-character abbreviation for the state name. 
# Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
# Ties are handled in the same way as best.R
# For example the function call rankall("heart attack", "best") would return a data frame containing the names of the hospitals that are the best in their respective states for 30-day heart attack death rates. 

rankall <- function(outcome, num = "best") {
  # Read outcome data
  
  outcomedata <- read.csv("outcome-of-care-measures.csv") 
  
  # Create data frame
  ranked <- data.frame(hospital=NULL,state=NULL)
  
  # For each state, find the ranking of the given outcome specified by num and attach to the data frame "ranked".
  if (outcome=='heart attack'){
    newnum <- num
    for (i in sort(unique(outcomedata$State))){
      statedata <-subset(outcomedata, outcomedata[,7] == i & outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
      # Assign numeric values to "best" and "worst"
      if (identical(num,"best")){
        newnum <- 1
      } else if (identical(num,"worst")){
        newnum <- nrow(statedata)
        print(newnum)
      }
      # Check that num is not larger than the number of hospitals in the state
      if (newnum > nrow(statedata)){
        ranked <- rbind(ranked,data.frame(hospital="NA",state=i))
      }else {
        sorted <-statedata[order(as.numeric(as.character(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),statedata$Hospital.Name),]
        name <- as.character(sorted$Hospital.Name[newnum])
        ranked <- rbind(ranked,data.frame(hospital=name,state=i))
      }
      
    }
  } else if (outcome == 'heart failure'){
    newnum <- num
    for (i in sort(unique(outcomedata$State))){
      statedata <-subset(outcomedata, outcomedata[,7] == i & outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
      # Assign numeric values to "best" and "worst"
      if (identical(num,"best")){
        newnum <- 1
      } else if (identical(num,"worst")){
        newnum <- nrow(statedata)
        print(newnum)
      }
      # Check that num is not larger than the number of hospitals in the state
      if (newnum > nrow(statedata)){
        ranked <- rbind(ranked,data.frame(hospital="NA",state=i))
      }else {
        sorted <-statedata[order(as.numeric(as.character(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),statedata$Hospital.Name),]
        name <- as.character(sorted$Hospital.Name[newnum])
        ranked <- rbind(ranked,data.frame(hospital=name,state=i))
      }
      
    }
  } else if (outcome == 'pneumonia'){
    newnum <- num
    for (i in sort(unique(outcomedata$State))){
      statedata <-subset(outcomedata, outcomedata[,7] == i & outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available")
      # Assign numeric values to "best" and "worst"
      if (identical(num,"best")){
        newnum <- 1
      } else if (identical(num,"worst")){
        newnum <- nrow(statedata)
      }
      # Check that num is not larger than the number of hospitals in the state
      if (newnum > nrow(statedata)){
        ranked <- rbind(ranked,data.frame(hospital="NA",state=i))
      }else {
        sorted <-statedata[order(as.numeric(as.character(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),statedata$Hospital.Name),]
        name <- as.character(sorted$Hospital.Name[newnum])
        ranked <- rbind(ranked,data.frame(hospital=name,state=i))
      }
      
    }
  } else {
    # Throw stop if outcome is not one of "heart attack","heart failure", or "pneumonia"
    stop("invalid outcome")
  }
  return(ranked)
}
