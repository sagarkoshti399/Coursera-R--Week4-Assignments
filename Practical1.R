## Week 4
## Assignment 3

## 1: Plot the 30-day mortality rates for heart attack

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


## Print the no. of rows and column
nrow(outcome)
ncol(outcome)

## Print details of individual columns
names(outcome)
str(outcome)

## Converting character to numeric
outcome[,11] <- as.numeric(outcome[,11])

## Plot the Histogram
hist(outcome[,11])
