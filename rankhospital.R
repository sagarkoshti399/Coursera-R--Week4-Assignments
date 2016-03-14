## Function to rank hospital by outcome in a state


fetch_rank <- function(vStateSubset,vColumn,num){
        result <- vStateSubset[, 2][order(vColumn, vStateSubset[, 2])[num]]
        return(result)
}        


rankhospital <- function(state, outcome,num = "best"){
        
        ## Read the csv file
        infile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check the State name. Prompt invalid State with correct message.
        if(state %in% infile$State) {
                vcheck1 <- 1
        }else {
                stop("Invalid State. Please provide correct State code.")
        }
        
        ## Initialize the outcome and store it in a vector
        vOutcome <- c("heart attack","heart failure","pneumonia")
        
        ## Validate the Outcome and set the correct checkpoint
        if(outcome %in% vOutcome){
                vcheck2 <- 1
        }else {
                stop("Invalid cause/outcome. Please provide correct outcome.")
        }
        
        ## Convert required columns to Numeric
        infile[,11] <- suppressWarnings(as.numeric(infile[,11]))
        infile[,17] <- suppressWarnings(as.numeric(infile[,17]))
        infile[,23] <- suppressWarnings(as.numeric(infile[,23]))
        
        
        ## Validate the checkpoint and initialize the checkpoint vcheck3
        ## Initialize dummy column name
        if(vcheck1 == 1 && vcheck2 == 1) {
                if(outcome == "heart attack") {
                        vcol <- 11
                }else if(outcome == "heart failure") {
                        vcol <- 17
                }else if(outcome == "pneumonia") {
                        vcol <- 23
                }
        }
        
        vStateSubset <- infile[infile[,7] == state, ]
        #str(vStateSubset)
        vColumn <- vStateSubset[,vcol]
        #str(vStateSubset)
        vLength <- dim(vStateSubset[!is.na(vColumn), ])[1]
        
        #num
        #vLength
        
        if(num == "best") {
                rank <- fetch_rank(vStateSubset,vColumn,1)
        }else if(num == "worst") {
                rank <- fetch_rank(vStateSubset,vColumn,vLength)
        }else if(num > vLength) {
                rank <- NA
        }else {
                rank <- fetch_rank(vStateSubset,vColumn,num)        
        }
        rank
}
        
