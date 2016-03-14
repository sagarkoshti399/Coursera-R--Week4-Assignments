## Function to rank all hospital in each state

fetch_rank <- function(vStateSubset,vColumn,num){
        result <- vStateSubset[, 2][order(vColumn, vStateSubset[, 2])[num]]
        return(result)
}        


rank_adjust <- function(vStateSubset,vCol,num) {
        
        vOutcome <- as.numeric(vStateSubset[, vCol])
        vLen <- dim(vStateSubset[!is.na(vOutcome), ])[1]        
        
        if(num == "best") {
                vRank <- fetch_rank(vStateSubset,vOutcome,1)
        }else if(num == "worst") {
                vRank <- fetch_rank(vStateSubset,vOutcome,vLen)
        }else if(num > vLen) {
                vRank <- NA
        }else {
                vRank <- fetch_rank(vStateSubset,vOutcome,num)        
        }
        
        return(vRank)
}


rankall <- function(outcome, num = "best") {

        #outcome <- "heart attack"
        #num <- "best"
        
        ## Read the csv file
        infile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Initialize the outcome and store it in a vector
        vOutcome <- c("heart attack","heart failure","pneumonia")
        
        ## Validate the Outcome and set the correct checkpoint
        vcheck1 <- 0
        if(outcome %in% vOutcome){
                vcheck1 <- 1
        }else {
                stop("Invalid outcome. Please provide correct outcome.")
        }
        
        ## Convert required columns to Numeric
        infile[,11] <- suppressWarnings(as.numeric(infile[,11]))
        infile[,17] <- suppressWarnings(as.numeric(infile[,17]))
        infile[,23] <- suppressWarnings(as.numeric(infile[,23]))

        
        vUniqueState <- sort(unique(infile$State))
        vUniqueState
        
        vStateLength <- length(vUniqueState)
        vStateLength
        
        vHospital <- rep("", vStateLength)
        vHospital
        
        ## Validate the checkpoint and initialize the checkpoint vcheck3
        ## Initialize dummy column name
        if(vcheck1 == 1) {
                
                for(i in 1:vStateLength) {        
                        
                        vStateSubset <- infile[infile[,7] == vUniqueState[i],]
                        vStateSubset
                        
                        if(outcome == "heart attack") {
                                vHospital[i] <- rank_adjust(vStateSubset,11,num)
                        }else if(outcome == "heart failure") {
                                vHospital[i] <- rank_adjust(vStateSubset,17,num)
                        }else if(outcome == "pneumonia") {
                                vHospital[i] <- rank_adjust(vStateSubset,23,num)
                        }
                }
        }
        

        vDataFrame <- data.frame(Hospital=vHospital, state=vUniqueState)
        vOut <- vDataFrame
        print(vOut)
        return(vOut)
        
}

