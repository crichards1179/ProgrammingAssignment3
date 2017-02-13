# This function finds the hospital in all states that has the score of the requested rank
# (i.e. "best", "worst", ranking) score for a given outcome.
# Input arguments:  
#       1) Outcome name
#       2) Rank

rankall <- function(outcome, num = "best") {
    # Validate the outcome argument
    validoutcome = c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% validoutcome == FALSE ) stop("invalid outcome")
    
    
    # Read in data file
    d <- read.csv("outcome-of-care-measures.csv", na.strings = 'Not Available', stringsAsFactors = FALSE)
    
    # Get relevant columns and name them
    if (outcome == "heart attack") {
        d <- d[c(2, 7, 11)]
        names(d) <- c("hospital", "state", "heart attack")
    } else if (outcome == "heart failure") { 
        d <- d[c(2, 7,  17)]
        names(d) <- c("hospital", "state", "heart failure")
    } else if (outcome == "pneumonia") {
        d <- d[c(2, 7, 23)]
        names(d) <- c("hospital", "state", "pneumonia")
    }
    
    
    # Remove NAs
    d <- d[complete.cases(d),]
    
    # Get rows orderd by 1) state, 2) outcome score and 3) hospital name.
    sortedrows <<-d[order(d[2], d[3], d[1]),]
    
    #Split by state
    splitrows <<- split(sortedrows, sortedrows$state)
    
    #Get given rank and return
    if (num == "best"){
        final <<- sapply(splitrows, "[", 1, ) 
        final <- unlist(final)
        final <- as.data.frame(final, row.names = FALSE, col.names = names(d))
        #final <- final[, 1]
    } else if (num == "worst"){
        # Get rows orderd by 1) state, 2) outcome score and 3) hospital name, in decreasing order.
        sortedrows <<-d[order(d[2], d[3], d[1], decreasing = TRUE),]
        #Split by state
        splitrows <<- split(sortedrows, sortedrows$state)
        final <<- lapply(splitrows, "[", 1, )
    } else {
        final <<- lapply(splitrows, "[", num, )   
    }
    
    return(final)
    
    
    
    
    
}