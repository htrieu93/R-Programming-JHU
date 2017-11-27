setwd("D:/Hieu Trieu/Documents/rprog_2Fdata%2Fspecdata")
complete <- function(directory, id = 1:332){
  list <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  id_v <- c()
  nobs_v <- c()
  
  for (i in id){
    data <- read.csv(list[i])
    nobs_v <- c(nobs_v,length(data[complete.cases(data),1]))
    id_v <- c(id_v,i)
  }
  
  data.frame("id" = id_v, "nobs" = nobs_v)
}