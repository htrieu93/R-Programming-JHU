setwd("C:/Users/Hieu Trieu/Desktop/rprog_2Fdata%2FProgAssignment3-data")

find_min <- function(df, col, state){
  
        df[,col] <- as.numeric(levels(df[,col])[df[,col]])
        hospByState <- df[df[,'State'] == state,]
        min_rate <- min(hospByState[,col], na.rm = TRUE)
        hosp <- sort(hospByState[which(hospByState[,col] == min_rate),2])
        return(hosp[1])
}

best <- function(state, outcome){
  
        options(warn = -1)
  
  ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        data[,'State'] <- as.character(data[,'State'])
        data[,'Hospital.Name'] <- as.character(data[,'Hospital.Name'])
  ## Check that state and outcome and valid
        

      
        
        if(any(grepl(state, data$State)) == TRUE){
          
                if(outcome %in% "heart attack"){
                        find_min(data,11,state)
                }
                else if(outcome == "heart failure"){
                        find_min(data,17,state)
                }
                else if(outcome %in% "pneumonia"){
                        find_min(data,23,state)
                }
                else{
                        stop("invalid outcome")
                }
        }
        else
                stop("invalid state")
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}