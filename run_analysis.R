#read data from provided directory and dataset file name
readData <- function(directory, filename) {
  # read the column names
  filepath <- paste("./", directory, "/", "features.txt", sep="")
  data_cols <- read.table(filepath, header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  
  # read y data
  filepath <- paste("./", directory, "/", filename, "/y_", filename, ".txt", sep="")
  y_data <- read.table(filepath, header=F, col.names=c("ActivityID"))
  
  # read x data
  filepath <- paste("./", directory, "/", filename, "/X_", filename, ".txt", sep="")
  x_data <- read.table(filepath, header=F, col.names=data_cols$MeasureName)
  
  # read subject data
  filepath <- paste("./", directory, "/", filename, "/subject_", filename, ".txt", sep="")
  subject_data <- read.table(filepath, header=F, col.names=c("SubjectID"))  
  
  ## extract the data from the merged data where the column names are mean OR std
  mean_columns <- grep(".*mean\\(\\)", data_cols$MeasureName)
  std_columns <- grep(".*std\\(\\)", data_cols$MeasureName)
  
  ## put both mean and std columns into single vector
  mean_std_data_cols <- c(mean_columns, std_columns)
  
  ## sort the vector 
  mean_std_data_cols <- sort(mean_std_data_cols)
  
  # subset the data
  x_data <- x_data[,mean_std_data_cols]
  
  # append the activity id and subject id columns
  x_data$ActivityID <- y_data$ActivityID
  x_data$SubjectID <- subject_data$SubjectID
  
  # return the data
  x_data
}

trainData <- readData("UCI HAR Dataset","train")
testData <- readData("UCI HAR Dataset","test")


#Merge data from test and train sets
mergeData <- {

  data <- rbind(testData, trainData)
  col_names <- colnames(data)
  col_names <- gsub("\\.+mean\\.+", col_names, replacement="Mean")
  col_names <- gsub("\\.+std\\.+",  col_names, replacement="Std")
  colnames(data) <- col_names
  
  labels_filename <- paste("./", directory, "/", "activity_labels.txt", sep="")
  activity_labels <- read.table(labels_filename, col.names=c("ActivityID", "ActivityName"), header=FALSE, as.is=TRUE)
  activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
  data <- merge(data, activity_labels)
  
  data
}

#create tidy data set
createDataset <- function(data) {
  library(reshape2)
  
  # melt the dataset
  id_vars = c("ActivityID", "ActivityName", "SubjectID")
  mvars = setdiff(colnames(data), id_vars)
  melted_data <- melt(data, id=id_vars, measure.vars=mvars)
  
  # result
  dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}



#create tidy data set
data <- createDataset(mergeData)
#write it to the file
write.table(data, "tidyset.txt")

print("Saved data to tidyset.txt.")
