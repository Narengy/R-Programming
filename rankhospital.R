rankhospital <- function (state,outcome,num="best"){
  
  # Read Data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #Check invalid outcome 
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  #Get outcomeclo for given outcome
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
  # Order based on rate
  subdata <- subdata[order(subdata[,outcomeclo], subdata[,2], na.last=TRUE),2]
  subdata <- na.omit(subdata)
  
  num <- ifelse(num == "best", 1, ifelse(num == "worst", length(subdata), as.numeric(num)))
  
  #
  subdata[num]
}
  
 
