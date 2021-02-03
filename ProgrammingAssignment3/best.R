# outcome <- read.csv("DataFiles/outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# ncol(outcome)
# names(outcome)
# outcome[, 11] <- as.numeric(outcome[, 11])
# hist(outcome[, 11])

# The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best 30-day mortality for the specified outcome
# in a given state.
best <- function(state, outcome) {
        # Read data
        data <- read.csv("DataFiles/outcome-of-care-measures.csv", colClasses = "character")
        
        # State and Outcome Validation
        if (!state %in% unique(data$State)) {
                stop('invalid state')
        } else if (!outcome %in% c("heart attack","heart failure","pneumonia")) {
                stop('invalid outcome')
        }
        
        # Filters by state
        data <- data[data$State == state,]
        
        # Removes not needed columns
        data <- data[c(2,11,17,23)]
        
        # Renames columns
        names(data) <- c("hospital","heart attack","heart failure","pneumonia")
        
        # Convert outcome columns to numeric
        data[,2:4] <- apply(data[,2:4],2,function(x) suppressWarnings(as.numeric(x)))
        
        # Remove NA values
        data <- data[complete.cases(data),]
        
        # Sort data in ascending order
        data <- data[order(data[outcome],data$hospital),]
        
        # Return hospital name from top 1 data
        head(data$hospital,1)
}