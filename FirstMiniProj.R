## FUNCTION 1: pollutantmean 

##takes in arguments for which folder to access, the pollutant to be calculated, and the range
pollutantmean <- function(directory,pollutant, id = 1:332) {
  
  #create an empty numeric vector
  dataCollection <- numeric()
  
  #iterate through the range specified in the function parameter above
  for (i in id) {
    #Get the path with the filename appended and store in variable fileName
    fileName <- paste(getwd(),"/",directory,"/",formatC(i, width = 3, flag = "0"),".csv", sep="")
    
    #Pass in fileName variable to read.csv() function
    pollutantDatafile <- read.csv(fileName)
    
    #Extract from pollutantDatafile elements in the pollutant column 
    #create a vector for the extracted data and bind to dataCollection
    dataCollection <- c(dataCollection, pollutantDatafile[[pollutant]])
  }
  #computes the mean for dataCollection while ignoring all fields with NA 
  mean(dataCollection, na.rm=TRUE) 
}

#Sample Input: 
#pollutantmean("specdata", "sulfate", 1:30)

#Sample Output:
#[1] 2.409731


#FUNCTION 2: Complete Function 
complete <- function(directory, id = 1:332) {
  
  #Create an empty data frame for storing all observations
  observations <- data.frame() 
  
  #Iterate through the range specified in the function parameter above
  for (i in id) {
    
    #Get the path with the filename appended and store in variable fileName
    fileName <- paste(getwd(),"/",directory,"/",formatC(i,width=3,flag="0"),".csv", sep="")
    
    #Pass in fileName variable to read.csv() function and store in pollutantDatafile variable
    pollutantDatafile <- read.csv(fileName)
    
    ## complete.cases returns a logical vector indicating which cases are complete
    ##i.e., have no missing values.
    ##check for complete cases in the pollutantDatafile and pass into a sum function
    ##the resulting sum will be stored in a nonEmpty variable
    nonEmpty <-sum(complete.cases(pollutantDatafile))
    
    #combine or bind the data frame that we will create
    #with nonEmpty data in a data frame along with their index.
    #this will be further combined to the data frame that we created earlier named "observations"
    observations <- rbind(observations, data.frame(i,nonEmpty))
  }
  #this specifies the names of the columns 
  #which will be "id" and "nobs" respectively
  colnames(observations) <- c("id","nobs")
  
  #return the resulting data frame
  return(observations)
}
  
#Sample Input:
#complete("specdata", 1:50)
#Sample Output:
#   id nobs
#1   1  117
#2   2 1041
#3   3  243
#4   4  474
#5   5  402
#6   6  228
#7   7  442
#8   8  192
#9   9  275



#FUNCTION 3: Correlation function 

#this function takes in the directory or filename of the containing folder, 
#and a numeric threshold with a default value of 0.

corr <- function(directory, threshold =0){
  
  #modification to the above code using list.files and full.names to
  #create a list of all files in the directory with the directory
  #path prepended to the file names to give a relative file path.
  complete_files <- list.files(directory, full.names= TRUE) 
  
  #this takes the length of the the list of files and stores in variable max
  #this allows us to set the max threshold for iterating through files in the for loop
  max <- length(complete_files)
  
  #create empty vector for results after iterating through files
  results <- vector(mode = "numeric", length = 0)
  
  
  for(i in 1:max){
    #read each file
    aux <- read.csv(complete_files[i])
    
    #calculate the sum of non empty elements for both sulfate and nitrate
    #so that it can be compared to the threshold
    
    total_sum <- sum(!is.na(aux$sulfate) & !is.na(aux$nitrate))
    
    #Conditional to check if the sum is greater than the threshold
    if (total_sum > threshold){
      
      #if greater than threshold, extract sulfate and nitrate data
      sulfate_data <- aux[which(!is.na(aux$sulfate)), ]
      nitrate_data <- sulfate_data[which(!is.na(sulfate_data$nitrate)), ]
      
      #apply cor function on nitrate and sulfate data, store in results variable
      results <- c(results, cor(nitrate_data$nitrate, nitrate_data$sulfate))
    }
  }
  results
}


#Histogram Application on Hospital Data

#read outcome-of-care-measures.csv
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

#coerce outcomes column 11 elements as numeric
outcome[, 11] <- as.numeric(outcome[, 11])

#pass into hist function that plots a histogram while adjusting title, and x-axis label
hist(outcome[, 11], xlab = "Deaths", 
     main = paste("Histogram of" , "30-Day Death (Mortality) Rates from Heart Attack"),
)

