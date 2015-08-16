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

pollutantmean <- function(directory, pollutant, id = 1:332) {
    setwd(directory)
    
    # calculate how many files you have to process
    numFiles <- length(id)   
    
    # create two vectors to hold the sum and locations from each file
    sumPollutants <- numeric(numFiles)
    numLocations <- numeric(numFiles)
    
    # switch columns based on the type of pollutant
    if (pollutant == "sulfate") {
        column <- 2
    }
    else if (pollutant == "nitrate") {
        column <- 3
    }
    
    # iterate through each file in the folder and calculate
    # sum of each column in each file, also track how many 
    # locations this corresponds to
    for (i in 1:numFiles) {
        filename <- getfilename(id[i])
        tabData <- read.table(filename, sep=",", header=TRUE)
        v <- complete.cases(tabData[column])
        completeVec <- tabData[,column][v]
        sumPollutants[i] <- sum(completeVec)
        numLocations[i] <- length(completeVec)
    }
    
    meanPollutants <- sum(sumPollutants)/sum(numLocations)
    meanPollutants
}