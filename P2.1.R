pollutantmean <- function(directory, pollutant, id = 1:332){
  files <- list.files(directory, pattern = "*.csv", full.names=TRUE)
  length(id)
  mean_v <- c()
  
  for (i in id){
      data <- read.csv(files[i])
        remove_na <- data[!is.na(data[,pollutant]), pollutant]
        mean_v <- c(mean_v,remove_na)
  }
  mean(mean_v)
}

