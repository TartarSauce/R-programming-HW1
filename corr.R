getfilename <- function (i) {
    pad <- function(x) { 
        if (x < 10) {
            x <-str_pad(x, 3, pad="0")
            x
        }
        else if (x >= 10 && x < 100) {
            x <- str_pad(x, 3, pad="0")
            x
        }
        else {
            x
        }
    }
    fname <- paste(pad(i), ".csv", sep="")
    fname
}

corr <- function(directory, threshold = 0) {
    setwd(directory) 
    numFiles <- length(list.files())
    
    # calculate how many files you have to process
    id <- 1:numFiles   
    
    nobs <- numeric(numFiles)
    corSulfateNitrate <- numeric(numFiles)
    corVecIndex <- 1
    
    # iterate through each file in the folder 
    for (i in 1:numFiles) {
        filename <- getfilename(id[i])
        tabData <- read.table(filename, sep=",", header=TRUE)
        completeCases <- tabData[complete.cases(tabData), ]
        nobs[i] <- nrow(completeCases)
        if (threshold > 0) {
            if (nobs[i] > threshold) {
                corSulfateNitrate[corVecIndex] <- cor(completeCases$sulfate, completeCases$nitrate)
                corVecIndex <- corVecIndex + 1
            } 
        }
        else if (threshold == 0) {
            corSulfateNitrate[i] <- cor(completeCases$sulfate, completeCases$nitrate)
        }
    }
    setwd("../")
    corSulfateNitrate[corSulfateNitrate != 0]
}