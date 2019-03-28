#1. Final

pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    data = numeric()
    for (i in id) {
        
        ReadFile <-  read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"),
                                   sep = "", ".csv"))
        
        data <-  c(data, ReadFile[[pollutant]])
    }
    
    ## Return the mean of the pollutant across all monitor ID numbers to be used
    return(mean(data, na.rm = TRUE))
}
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)
