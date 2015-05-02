complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
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
  ok <- complete.cases(poll$sulfate, poll$nitrate)
  n <- table(poll[ok,"ID"])
  data.frame(ID=names(n), nobs=as.vector(n))
}