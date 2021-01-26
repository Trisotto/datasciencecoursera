pollutantmean <- function(directory,pollutant,id = 1:332) {
        data <- data.frame()
        for (i in seq_along(id)) {
                path <- paste(directory, "/", formatC(id[i], width = 3, flag = 0), ".csv", sep = "")
                raw_data <- read.csv(path)
                data <- rbind(data,raw_data)
        }
        mean(data[,pollutant],na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
        data <- data.frame()
        for (i in seq_along(id)) {
                path <- paste(directory, "/", formatC(id[i], width = 3, flag = 0), ".csv", sep = "")
                raw_data <- read.csv(path)
                cmp_cases <- c(id[i], sum(complete.cases(raw_data) == TRUE))
                data <- rbind(data,cmp_cases)
        }
        names(data) <- c("id","nobs")
        data
}

corr <- function(directory, threshold = 0) {
        file_names <- list.files(path = paste(directory, "/", sep = ""), pattern = "*.csv", full.names = TRUE)
        list_corr <- c()
        for (i in seq_along(file_names)) {
                raw_data <- read.csv(file_names[i])
                if (sum(complete.cases(raw_data) == TRUE) > threshold) {
                        cmp_cases <- raw_data[complete.cases(raw_data),]
                        cr <- cor(cmp_cases$sulfate, cmp_cases$nitrate)
                        list_corr <- c(list_corr,cr)
                }
        }
        if (length(list_corr) == 0) {
                return(0)
        }else{
                list_corr
        }
}