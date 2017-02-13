# This function finds the hospital in a given state that has the score of the requested rank
# (i.e. "best", "worst", ranking) score for a given outcome.
# Input arguments:  
#       1) State name (two character abbreviation)
#       2) Outcome name
#       3) Rank

rankhospital <- function(state, outcome, num = "best") {
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
    
    # Validate the state argument
    validstate <- unique(d[,2])
    if( state %in% validstate == FALSE ) stop("invalid state")
    
    # Remove NAs
    d <- d[complete.cases(d),]
    
    # Get rows with state argument and order by outcome first and then hospital name
    rows <- d[d$state == state & d[outcome] != 'Not Available',]
    sortedrows <-rows[order(rows[3], rows[1]),]
    
    if (num == "best"){
        rankedhosp <- sortedrows[1,1]
    } else if (num == "worst"){
        rankedhosp <- sortedrows[nrow(sortedrows),1]
    } else {
        rankedhosp <- sortedrows[num,1]
    }
    return(rankedhosp)
    
}