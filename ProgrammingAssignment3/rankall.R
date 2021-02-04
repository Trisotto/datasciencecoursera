# The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num.
rankall <- function(outcome, num = 'best') {
        # Read data
        data <- read.csv("DataFiles/outcome-of-care-measures.csv", colClasses = "character")
        
        # Outcome Validation
        if (!outcome %in% c("heart attack","heart failure","pneumonia")) {
                stop('invalid outcome')
        }
        
        # Removes not needed columns
        data <- data[c(2,7,11,17,23)]
        
        # Renames columns
        names(data) <- c("hospital","state","heart attack","heart failure","pneumonia")
        
        # Convert outcome columns to numeric
        data[,3:5] <- apply(data[,3:5],2,function(x) suppressWarnings(as.numeric(x)))
        
        # Remove NA values
        data <- data[complete.cases(data),]
        
        # Sort data by state, outcome and hospital
        data <- data[order(data$state,data[outcome],data$hospital),]
        
        data <- split(data[,"hospital"],data$state)
        
        # Return hospital name in that state with the given rank 30-day death rate
        getHospital <- function (data, num) {
                if (num == "best"){
                        head(data,1)
                } else if (num == "worst"){
                        tail(data,1)
                } else {
                        data[num]
                }
        }
        
        # Return a data frame with the hospital names and the (abbreviated) state name
        data <- lapply(data,getHospital,num)
        data.frame(hospital = unlist(data), state = names(data), row.names = names(data))
}