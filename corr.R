corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  retval <- vector(mode="numeric", length=0)
  if (threshold > 0) {
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
    #poll <- subset(comb, ID %in% id)
    ok <- complete.cases(comb$sulfate, comb$nitrate)
    n <- table(comb[ok,"ID"])
    mon <- data.frame(ID=names(n), nobs=as.vector(n))
    valid_mon <- mon$nobs > threshold
    for (id in mon[valid_mon, "ID"]) {
      poll <- subset(comb, ID == id)
      retval <- c(retval, cor(poll$nitrate, poll$sulfate, use = "complete.obs"))
    }  
  }
  retval
}