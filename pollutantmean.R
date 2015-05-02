pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  if (class(directory) == "character" & class(pollutant) == "character") {
    comb <- vector(mode="numeric", length=0)
    fils <- list.files(directory)
    for (f in fils) {
      d1 <- read.csv(paste(directory, f, sep = "/"))
      if (length(comb) > 0) {
        comb <- rbind(comb,d1)
      } else {
        comb <- d1
      }
    }
    poll <- subset(comb, ID %in% id)
    if (pollutant == "sulfate") {
      retval <- mean(poll$sulfate, na.rm=TRUE)
    } else if(pollutant == "nitrate"){
      retval <- mean(poll$nitrate, na.rm=TRUE)
    } else {
      retval <- NA
    }
  } else {
    retval <- NA
  }
  retval
}