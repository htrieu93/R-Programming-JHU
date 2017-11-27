setwd("C:/Users/Hieu Trieu/Desktop/rprog_2Fdata%2FProgAssignment3-data")

find_rank <- function(df, state, rank){
  
  if(rank %in% "best"){
    o <- order(noNA[,3])
    noNA <- noNA[o,]
    table <- c()
    
    ## Break ties
    if(sum(noNA[o,3] == noNA[1,3]) > 1){
      hosp_rank <- noNA[noNA[,3] == noNA[1,3],1][order(noNA[noNA[,3] == noNA[1,3],1])]
      print(hosp_rank[1]) 
    }
    else{
      print(noNA[1,1])                 
    }
    
  }
  else if(rank %in% "worst"){
    o <- order(noNA[,3], decreasing = TRUE)
    hosp <- noNA[o,]
    
    if(sum(noNA[o,2] == noNA[1,3]) > 1){
      hosp_rank <- noNA[noNA[,3] == noNA[1,3],1][order(noNA[noNA[,3] == noNA[1,3],1], decreasing = TRUE)]
      print(hosp_rank[1]) 
    }
    else{
      print(noNA[1,1])                 
    }
  }
  else if(rank > nrow(noNA)){
    print(NA)
  }
  else{
    o <- order(noNA[,3])
    noNA <- noNA[o,]
    
    ## Break ties
    if(sum(noNA[o,3] == noNA[rank,3]) > 1){
      rankings <- which(noNA[,3] == noNA[rank,3])
      tb <- cbind(noNA[noNA[,3] == noNA[rank,3],], rankings)
      tb[,1] <- tb[order(tb[,1]),1]
      print(tb[tb[,'rankings'] == rank,1])
    }
    else{
      print(noNA[1,1])
    }
  }
}
  
rankall <- function(outcome, num = "best"){
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
    
        options(warn = -1)
        
    
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
    
        ## Check that state and outcome and valid
        
        stat <- sort(unique(data$State))
        table <- c()
        
        
        
        if(outcome %in% "heart attack"){
                data <- data[,c(2,7,11)]
        }
        else if(outcome == "heart failure"){
                data <- data[,c(2,7,17)]
        }
        else if(outcome %in% "pneumonia"){
                data <- data[,c(2,7,23)]
        }
        else{
                stop("invalid outcome")
        }
        
        names(data)[3] = "Deaths"
        data[, 3] = suppressWarnings( as.numeric(data[, 3]) )
        
        # Remove rows with NA
        data = data[!is.na(data$Deaths),]
        
        splited = split(data, data$State)
        ans = lapply(splited, function(x, num) {
          # Order by Deaths and then HospitalName
          x = x[order(x$Deaths, x$Hospital.Name),]
          
          # Return
          if(class(num) == "character") {
            if(num == "best") {
              return (x$Hospital.Name[1])
            }
            else if(num == "worst") {
              return (x$Hospital.Name[nrow(x)])
            }
          }
          else {
            return (x$Hospital.Name[num])
          }
        }, num)
        
        #Return data.frame with format
        return ( data.frame(hospital=unlist(ans), state=names(ans)) )
}