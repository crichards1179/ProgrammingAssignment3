# This function finds the hospital in a given state that has the best (i.e. lowest)
# score for a given outcome.
# Input arguments:  
#       1) State name (two character abbreviation)
#       2) Outcome name

best <- function(state, outcome) {
    # Validate the outcome argument
    validoutcome = c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% validoutcome == FALSE ) stop("invalid outcome")
    
    
    # Read in data file
    d <- read.csv("outcome-of-care-measures.csv", na.strings = 'Not Available', stringsAsFactors = FALSE)
    
    # Get relevant columns
    d <- d[c(2, 7, 11, 17, 23)]
    
    # Name the columns
    names(d) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    # Validate the state argument
    validstate <- unique(d[,2])
    if( state %in% validstate == FALSE ) stop("invalid state")
    
    # Get rows with state argument and sort by hospital name
    rows <- d[d$state == state & d[outcome] != 'Not Available',]
    sortedrows <- rows[order(rows$hospital),]
    
    # Get column corresponding to outcome argument
    outcomevals <- sortedrows[,outcome]
    
    # Get row number for minimum value in the outcome column
    rownumofminval <- which.min(outcomevals)
    
    # Get the hospital name where the row numer is the rownum.minval and return it
    sortedrows[rownumofminval,]$hospital
    

    
}