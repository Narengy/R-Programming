best <- function(state, outcome) {
  # Read Data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
    #Check invalid outcome 
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  #Get index for given outcome
  outcomeclo <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  
  #Suppressing warnings
  data[,outcomeclo] <- suppressWarnings(as.numeric(data[,outcomeclo]))
  #Remove Nas
  data <- na.omit(data)
  
  #Check invalid state 
  states <- table(data$State)
  if (!state %in% names(states)) { 
    stop("invalid state")
  }
  
  #subdata
  subdata <- subset(data, State==state)
  subdata <- subdata[order(subdata[,outcomeclo], na.last=TRUE),2]
  subdata <- na.omit(subdata)
  
  #Get hospital name with the lowest 30-day mortality rate.
  subdata[1]
}