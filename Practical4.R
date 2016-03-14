## Week 4
## Assignment 3

## 4: Ranking hospitals in all states


source("rankall.R")
head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)