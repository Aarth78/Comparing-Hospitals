rankhospital <- function(state, outcome, num="best")
{
  
  setwd("/Users/parthajmera/Documents/GitHub/Comparing-Hospitals")
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  betterData <- as.data.frame(cbind(data[,2], data[, 7], data[, 11], data[, 17], data[, 23]), stringsAsFactors = FALSE)
  colnames(betterData) <- c("Hospital Name","State", "heart attack", "heart failure", "pneumonia")
  
  
  if(!(outcome=="heart attack" || outcome=="heart failure" || outcome=="pneumonia")){stop("invalid outcome")}
  if(!(state %in% unique(betterData$State))){stop("invalid state")}
  

  
  rowsInWhichStateNeeded <- which(betterData[,"State"]==state)
  
  betterStateData <- betterData[rowsInWhichStateNeeded, ]
  
  removingTextAndIntroducing_NA <- as.numeric(betterStateData[, outcome])  
  
  betterStateData[,outcome]<-removingTextAndIntroducing_NA
  
  arrangedData <- betterStateData[order(betterStateData[,outcome],betterStateData$`Hospital Name`), ] 
  
  if(num == "best"){num <- 1}
  if(num == "worst"){num <- nrow(arrangedData[!is.na(arrangedData[,outcome]),])}
  
  return(arrangedData[num,"Hospital Name"])
  
  
}