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

complete <- function(directory, id = 1:332) {
    setwd(directory)
    
    # calculate how many files you have to process
    numFiles <- length(id)   
    
    nobs <- numeric(numFiles)
    
    # iterate through each file in the folder 
    for (i in 1:numFiles) {
        filename <- getfilename(id[i])
        tabData <- read.table(filename, sep=",", header=TRUE)
        nobs[i] <- nrow(tabData[complete.cases(tabData), ])
    }
  
    completeData <- data.frame (id=id, nobs=nobs)
    completeData
}