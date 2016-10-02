rankall <- function(outcome, num = "best") {
    ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        outcomedata <- outcomedata[,c(2,7,11,17,23)]
    ## Check that outcome is valid
        validoutcome <- c("heart attack","heart failure","pneumonia")
        isOutcomeValid <- match(outcome, validoutcome)
        if (is.na(isOutcomeValid)) {
            stop("invalid outcome")
        }
    ## For each state, find the hospital of the given rank
        split.data <- split(outcomedata, outcomedata$State)
        final.data <- lapply(split.data, hospitalNameFunction, outcome, num)
        
        unlisted.values <- unlist(final.data)
        list.names <- names(final.data)
        
        data.frame(hospital=unlisted.values, state=list.names, row.names=list.names)
}

hospitalNameFunction <- function(hospital.data, outcome, num="best"){
    last.row <- 0
    if (outcome == 'heart failure') {
        hospital.rank <- hospital.data[order(hospital.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, hospital.data$Hospital.Name),]
        last.row.data <- hospital.rank[!is.na(hospital.rank$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
        last.row <- NROW(last.row.data)
    }
    else if (outcome == 'heart attack') {
        hospital.rank <- hospital.data[order(hospital.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, hospital.data$Hospital.Name),]
        last.row.data <- hospital.rank[!is.na(hospital.rank$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
        last.row <- NROW(last.row.data)
    }
    else if (outcome == 'pneumonia') {
        hospital.rank <- hospital.data[order(hospital.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, hospital.data$Hospital.Name),]
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
    
    #return hospital name 
    result.row$Hospital.Name
}
    