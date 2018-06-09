rankall <- function(outcome, num = "best") 
  {
  
 
  setwd("/Users/parthajmera/Documents/GitHub/Comparing-Hospitals")
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  betterData <- as.data.frame(cbind(data[,2], data[, 7], data[, 11], data[, 17], data[, 23]), stringsAsFactors = FALSE)
  colnames(betterData) <- c("Hospital Name","State", "heart attack", "heart failure", "pneumonia")
  
  
  if(!(outcome=="heart attack" || outcome=="heart failure" || outcome=="pneumonia")){stop("invalid outcome")}
  
  hospname <- character()
  statename <- character()
 
  listOfStates <- unique(betterData$State)
  arrangedListofStates <- listOfStates[order(listOfStates)]
  
 
  for (i in arrangedListofStates) {
    
    rowsInWhichStateNeeded <- which(betterData[,"State"]==i)
    
    betterStateData <- betterData[rowsInWhichStateNeeded, ]
    
    removingTextAndIntroducing_NA <- as.numeric(betterStateData[, outcome])  
    
    betterStateData[,outcome]<-removingTextAndIntroducing_NA
    
    arrangedData <- betterStateData[order(betterStateData[,outcome],betterStateData$`Hospital Name`), ] 
    
    if(num == "best"){num <- 1}
    if(num == "worst"){
      num <- nrow(arrangedData[!is.na(arrangedData[,outcome]),])
    
    statename <- c(statename, i)
    hospname <- c(hospname, arrangedData[num,"Hospital Name"])
    num <- "worst"
    }
    
    
   if(num!="worst"){ 
    statename <- c(statename, i)
    hospname <- c(hospname, arrangedData[num,"Hospital Name"])}
  
    
    
    }
    
    
  
  
  resultData <- as.data.frame(cbind(statename,hospname),stringsAsFactors = FALSE)
  colnames(resultData) <- c("state","hospital")
  
  return(resultData)
  
  
                              
}