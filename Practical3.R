## Week 4
## Assignment 3

## 3: Ranking hospitals by outcome in a state


source("rankhospital.R")

rankhospital("TX", "heart failure", 4)
#"DETAR HOSPITAL NAVARRO"


rankhospital("MD", "heart attack", "worst")
#3
#[1] "HARFORD MEMORIAL HOSPITAL"

rankhospital("MN", "heart attack", 5000)
#NA
