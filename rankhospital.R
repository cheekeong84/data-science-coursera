rankhospital <- function(state, outcome, num = "best") {
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
        
    ## Return hospital name in that state with the given rank
        state.data <- outcomedata[which(outcomedata$State == state), ]
        last.row <- 0
        if (outcome == 'heart failure') {
            hospital.rank <- state.data[order(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, state.data$Hospital.Name),]
            last.row.data <- hospital.rank[!is.na(hospital.rank$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
            last.row <- NROW(last.row.data)
        }
        else if (outcome == 'heart attack') {
            hospital.rank <- state.data[order(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, state.data$Hospital.Name),]
            last.row.data <- hospital.rank[!is.na(hospital.rank$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
            last.row <- NROW(last.row.data)
        }
        else if (outcome == 'pneumonia') {
            hospital.rank <- state.data[order(state.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, state.data$Hospital.Name),]
            last.row.data <- hospital.rank[!is.na(hospital.rank$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
            last.row <- NROW(last.row.data)
        }
        
    ## 30-day death rate
        if (num == "best") {
            result.row <- hospital.rank[1,]
        }
        else if (num == "worst"){
            result.row <- hospital.rank[last.row,]
        }
        else{
            result.row <- hospital.rank[num,]
        }
            
    ##return hospital name
        result.row$Hospital.Name      
}