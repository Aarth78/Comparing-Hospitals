best <- function(state, outcome)
{
  setwd("/Users/parthajmera/Documents/GitHub/Comparing-Hospitals/")
  #state <- "TX"
  #outcome <- "heart attack"
  
  ##Read Data
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Organise Data
  
  betterData   <-  as.data.frame(cbind(data[, 2],   # hospital
                                  data[, 7],   # state
                                  data[, 11],  # heart attack
                                  data[, 17],  # heart failure
                                  data[, 23]), # pneumonia
                                  stringsAsFactors = FALSE)
  
  colnames(betterData) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  
  ##Verify Data
  
  if(!((outcome == "heart attack")|| (outcome ==  "heart failure") || (outcome == "pneumonia"))){stop("invalid outcome")}
  if(!(state %in% unique(betterData$state))){stop("invalid state")}
  
  
  
    which_Rows_Have_State_Needed <- which(betterData[ , "state"] == state)
    
  
    betterStateData <- betterData[which_Rows_Have_State_Needed, ]    # extracting data for the called state
    
    
    #Since data has text "Not Available", it will coerce Numerical values to text as well. So we convert it into numerics
    
    
    removingTextAndIntroducing_NA <- as.numeric(betterStateData[, outcome])                 #
    
    min_val <- min(removingTextAndIntroducing_NA, na.rm = TRUE)
    
    result  <- betterStateData[, "hospital"][which(removingTextAndIntroducing_NA == min_val)]
    
    output  <- result[order(result)]
    
    return(output)
  
}