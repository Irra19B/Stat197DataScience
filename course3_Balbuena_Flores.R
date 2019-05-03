getwd()
setwd("C:/Users/Irra Balbuena/Documents/balbuena R/Hospital")


# (1) plot the 30-day mortality rates for heart attacks
outcome <- read.csv("C:/Users/Irra Balbuena/Documents/balbuena R/Hospital/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])   


# (2) Finding the best hospital in a state
best <- function(state, outcome) {
    
    ## Read the outcome data
    out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
   
    if(state %in% out[,7] == FALSE){
        stop("invalid state")
    }
        
    switch(outcome, "heart attack" = {y <- 11}, "heart failure" = {y <- 17}, "pneumonia" = {y <- 23},
        stop("invalid outcome"))
    
    ## Return hospital name in that state with lowest 30-day death rate
    df = out[out$State == state, c(2, y)]
    df[which.min(df[, 2]), 1] #finds the min
}
best("TX", "heart attack")
best("BB", "heart attack")



# (3) Ranking Hospitals by outcome in a state
rankhospital <- function(state, outcome, num) {
    
    ## Read the outcome data
    out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(state %in% out[,7] == FALSE){
        stop("invalid state")
    }
    
    switch(outcome, "heart attack" = {y <- 11}, "heart failure" = {y <- 17}, "pneumonia" = {y <- 23},
           stop("invalid outcome"))
    
    out[, y] <-  as.numeric(out[, y])
    df <-  out[out[, 7] == state, c(2, y)]
    df <-  na.omit(df)
    hos <-  nrow(df)
    switch(num, "best" = {num <- 1}, "worst" = {num <- hos})
    if (num > hos) {
        return(NA)
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    o = order(df[, 2], df[, 1])
    df[o, ][num, 1]
}
rankhospital("TX", "heart failure", 4)
rankhospital("MN", "heart attack", 5000)


    

