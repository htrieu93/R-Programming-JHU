setwd("C:/Users/Hieu Trieu/Desktop/rprog_2Fdata%2FProgAssignment3-data")

find_rank <- function(df, col, state, rank){
        
        df[,col] <- as.numeric(levels(df[,col])[df[,col]])
        hospByState <- df[df[,'State'] == state,c(2,col)]
        noNA <- hospByState[!is.na(hospByState[,2]),]
        
        if(rank %in% "best"){
                o <- order(noNA[,2])
                hosp <- noNA[o,]
  
  ## Break ties
                if(sum(noNA[o,2] == hosp[1,2]) > 1){
                        hosp_name <- hosp[hosp[,2] == hosp[1,2],1]
                        hosp_rank <- hosp_name[order(hosp_name)]
                        print(hosp_rank[1]) 
                }
                else{
                        print(hosp[1,1])                 
                }
                 
        }
        else if(rank %in% "worst"){
                o <- order(noNA[,2], decreasing = TRUE)
                hosp <- noNA[o,]
                
                if(sum(noNA[o,2] == hosp[1,2]) > 1){
                        hosp_name <- hosp[hosp[,2] == hosp[1,2],1]
                        hosp_rank <- hosp_name[order(hosp_name, decreasing = TRUE)]
                        print(hosp_rank[1]) 
                }
                else{
                        print(hosp[1,1])                 
                }
        }
        else if(rank > nrow(noNA)){
                return(NA)
        }
        else{
                o <- order(noNA[,2])
                hosp <- noNA[o,]
                
                ## Break ties
                if(sum(noNA[o,2] == hosp[rank,2]) > 1){
                        rankings <- which(hosp[,2] == hosp[rank,2])
                        tb <- cbind(hosp[hosp[,2] == hosp[rank,2],],rankings)
                        tb[,1] <- tb[order(tb[,1]),1]
                        print(tb[tb[,'rankings'] == rank,1]) 
                }
                else{
                        print(hosp[1,1])                 
                }
        }
}

rankhospital <- function(state, outcome, num = "best"){
  
        options(warn = -1)
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        data[,'State'] <- as.character(data[,'State'])
        data[,'Hospital.Name'] <- as.character(data[,'Hospital.Name'])
        ## Check that state and outcome and valid
        
        if(any(grepl(state, data$State)) == TRUE){
          
                if(outcome %in% "heart attack"){
                        find_rank(data, 11, state, num)
                }
                else if(outcome %in% "heart failure"){
                        find_rank(data, 17, state, num)
                }
                else if(outcome %in% "pneumonia"){
                        find_rank(data, 23, state, num)
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