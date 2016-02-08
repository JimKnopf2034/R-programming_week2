# This the solution to the R-Programming task in Week 2, written R

# Part 1
# --------
#
# The task is to read in the data in the csv files and calculate the mean of the
# quantity sulfate or nitrate. Basics steps are:
# 1. Read data
# 2. Remove NAs
# 3. Compute mean()
# Function returns the mean of the pollutant in question.
pollutantmean <- function(directory, pollutant, id=1:332){
    result <- c()
    for (i in id) {
        inputData <- read.csv(paste(directory, sprintf("/%03d.csv", i), sep=''))
        result <- c(result, inputData[pollutant][!is.na(inputData[pollutant])])
    }
mean_res <- mean(result)
}


# Part 2
# --------
#
# The Task is to  read all the files and count the number of complete cases in
# each file. Basic steps  are:
# 1. loop over files
# 2. Read the file
# 3. count the number of complete cases
complete <- function(directory, id=1:332){
    # starting with two empty objects that get coerced to vectors
    idx <- NULL
    nobs <- NULL
    for(i in id){
        inputData <- read.csv(paste(directory, sprintf("/%03d.csv", i), sep=''))
        # creating list with indices
        idx <- c(idx, i)
        cdat <- inputData[complete.cases(inputData),]
        # extracting the first dim of the complete cases
        nobs <- c(nobs, dim(cdat)[1])
    }
    result <- data.frame(idx, nobs)
    print(head(result))
    invisible(result)
}


# Part 3
# --------
#
# The Task is to compute the correlation of nitrate and sulfate of data files
# where the number of complete cases is above certain threshold. Basic steps are:
# 1. Read data
# 2. compute complete cases
# 3.  if length above threshold append correlation to result vector
# 4. Postprocess result to return ether numeric(0) or the result vector
corr <- function(directory, threshold=0){
    result <- NULL
    for (i in list.files(directory)) {
        iData <- read.csv(paste(directory,'/',i, sep=''))
        cdat <- iData[complete.cases(iData), ]
        if (dim(cdat)[1] >= threshold){
            # append correlation to result vector
            result <- c(result, cor(cdat$sulfate, cdat$nitrate))
        }
    }
    if (length(result) == 0){
        result <- numeric(0)
    } else{
        result
    }
}


# run the functions to test them
#print(pollutantmean('./data/specdata', 'sulfate'))
#print(pollutantmean('./data/specdata', 'nitrate'))
nobs <- complete('./data/specdata')
nums <- corr('./data/specdata', 10000)
print(nums)
