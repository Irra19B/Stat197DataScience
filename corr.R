#3. Final
corr <- function(directory, threshold = 0) {
    ## 'threshold' is a numeric vector of length 1 indicating the number of
    ## completely observed observations (on all variables) required to compute
    ## the correlation between nitrate and sulfate; the default is 0
    

    df <-  complete(directory) 
    ids <-  df[df["nobs"] > threshold, ]$id
    corrr <- numeric()
    for (i in ids) {
     
        ReadFile <-  read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                    ".csv", sep = ""))
        dff <-  ReadFile[complete.cases(ReadFile), ]
        corrr <-  c(corrr, cor(dff$sulfate, dff$nitrate))
    }
    
    ## Return a numeric vector of correlations
    return(corrr)
}
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)

source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)

