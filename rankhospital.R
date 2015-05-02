rankhospital <- function(state, outcome, num = "best") {
  library(dplyr)
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that num is valid
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- -1
  } else if (as.numeric(num) > 0) {
    num <- as.numeric(num)
  } else stop("invalid num")
  
  ## Check that state and outcome are valid
  states <- data$State[data$State == state]
  if (length(states) == 0) stop("invalid state")
  
  mydf <- tbl_df(data)
    
  if (outcome == "heart attack") {
    ret <- select(mydf, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, State, Hospital.Name) %>%   
      filter(State == state) %>%
      arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), Hospital.Name) %>%  
      mutate(ranking = row_number(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),
             rankDesc = row_number(desc(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))) %>% 
      filter((ranking == num & num > 0) | (rankDesc == 1 & num == -1))
    
  } else if (outcome == "heart failure") {
    ret <-  select(mydf, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, State, Hospital.Name) %>%   
      filter(State == state) %>%
      arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), Hospital.Name) %>%  
      mutate(ranking = row_number(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
             rankDesc = row_number(desc(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))) %>% 
      filter((ranking == num & num > 0) | (rankDesc == 1 & num == -1))
    
  } else if (outcome == "pneumonia") {
    ret <-  select(mydf, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, State, Hospital.Name) %>%   
      filter(State == state) %>%
      arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), Hospital.Name) %>%  
      mutate(ranking = row_number(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),
             rankDesc = row_number(desc(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))) %>% 
      filter((ranking == num & num > 0) | (rankDesc == 1 & num == -1))
    
  } else stop("invalid outcome")
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  as.character(ret[1,3])
}