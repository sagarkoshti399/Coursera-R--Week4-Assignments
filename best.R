## Function to find best hospital in a state

best <- function(state, outcome){

        #state <- "MD"
        #outcome <- "pneumonia"
        
        #print(state)
        #print(outcome)
        
        ## Defining the required SQL library        
        library("sqldf")
        library("RSQLite")
        library("tcltk2")
        
                
        ## Read the csv file
        infile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Initialize checkpoint
        vcheck1 <- 0
        vcheck2 <- 0
        vcheck3 <- 0
        
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
        
        
        ## Validate the checkpoint and initialize the checkpoint vcheck3
        ## Initialize dummy column name
        ## Convert required columns to Numeric
        
        if(vcheck1 == 1 && vcheck2 == 1) {
                if(outcome == "heart attack") {
                        vcheck3 <- 1
                        colnames(infile)[11] <- c("col11")
                        vcol <- "col11"
                        
                        ## Use suppressWarnings to overcome "Warning: NAs introduced by coercion"
                        infile[,11] <- suppressWarnings(as.numeric(infile[,11]))
                }else if(outcome == "heart failure") {
                        vcheck3 <- 1
                        colnames(infile)[17] <- c("col17")
                        vcol <- "col17"
                        
                        ## Use suppressWarnings to overcome "Warning: NAs introduced by coercion"
                        infile[,17] <- suppressWarnings(as.numeric(infile[,17]))
                }else if(outcome == "pneumonia") {
                        vcheck3 <- 1
                        colnames(infile)[23] <- c("col23")
                        vcol <- "col23"
                        
                        ## Use suppressWarnings to overcome "Warning: NAs introduced by coercion"
                        infile[,23] <- suppressWarnings(as.numeric(infile[,23]))
                }
                
        }
        
        ## Validate checkpoint and fetch the required result
        if(vcheck3 == 1) {
                
                ## Restore column name without spaces
                colnames(infile)[2] <- c("HospitalName")
                
                ## Query to fetch required result
                vsql<-sprintf("Select HospitalName, min(%s) 
                              from infile
                              where State = '%s'
                              order by HospitalName",vcol,state,vcol)
                
                vout <- sqldf(vsql)
                vreturn <- vout[1]
                vreturn
        }        
}