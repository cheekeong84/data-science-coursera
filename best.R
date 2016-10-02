best <- function(state, outcome) {
    
    ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        
    ## Check that state and outcome are valid
        uniqueState <- unique(outcomedata$State)
        isStateValid <- match(state, uniqueState)
        if (is.na(isStateValid)) {
            stop("invalid state")
        }
        
        validoutcome <- c("heart attack","heart failure","pneumonia")
        isOutcomeValid <- match(outcome, validoutcome)
        if (is.na(isOutcomeValid)) {
            stop("invalid outcome")
        }
        
    ## Return hospital name in that state with lowest 30-day death
        state.data <- outcomedata[which(outcomedata$State == state), ]
        if (outcome == 'heart failure') {
            lowest <- state.data[which(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(as.numeric(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) , na.rm = TRUE)), ]
        }
        else if (outcome == 'heart attack') {
            lowest <- state.data[which(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(as.numeric(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE)), ]
        }
        else if (outcome == 'pneumonia') {
            lowest <- state.data[which(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(as.numeric(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE)), ]
        }
        
    ## Return hospital name
        lowest$Hospital.Name
}