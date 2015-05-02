best <- function(state, outcome) {
  library(dplyr)
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states <- data$State[data$State == state]
  if (length(states) == 0) stop("invalid state")
  
  mydf <- tbl_df(data)
  
  if (outcome == "heart attack") {
    ret <-  select(mydf, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, State, Hospital.Name) %>% 
            filter(State == state) %>% 
            arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), Hospital.Name)
  } else if (outcome == "heart failure") {
    ret <-  select(mydf, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, State, Hospital.Name) %>% 
            filter(State == state) %>% 
            arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), Hospital.Name)
  } else if (outcome == "pneumonia") {
    ret <-  select(mydf, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, State, Hospital.Name) %>% 
            filter(State == state) %>% 
            arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), Hospital.Name)
  } else stop("invalid outcome")
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  as.character(ret[1, 3])
}