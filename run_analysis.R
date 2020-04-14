
bonjour<-function(){
  library(dplyr)
  library(reshape2)
  
  ## Read Data
  activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
  features <- read.table("UCI HAR Dataset/features.txt")  
  
  #Subject who performed the activity
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",col.names="subject")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",col.names="subject")
  merge_sub<-rbind(subject_train,subject_test)
  
  #Accelerations values
  X_train <- read.table("UCI HAR Dataset/train/X_train.txt",col.names = features[,2])
  #Activities labels
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt",col.names="activites")

  
  #Accelerations values
  X_test <- read.table("UCI HAR Dataset/test/X_test.txt",col.names = features[,2])
  #Activities labels
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt",col.names="activites")


## Analysis
  # 1. Merges the training and the test sets to create one data set.
  mergesX <- rbind(X_train,X_test)

  # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  mergesX <- select(mergesX,contains("mean")|contains("std"))
  
  # 3. Uses descriptive activity names to name the activities in the data set
  # 3.1Merge with activites column
  mergesY<-rbind(y_train,y_test)
  
  mergestt<-cbind(mergesY,merge_sub,mergesX)
  
  #3.2 Replace the value activity in the first column by the name  
  #use the data: activity_labels
  
  # 4. Appropriately labels the data set with descriptive activity names.
  mergestt[["activites"]] <- factor(mergestt[, "activites"]
                                   , levels = activity_labels[[1]]
                                   , labels = activity_labels[[2]])


# 5. Creates a second, independent tidy data set with the average of each variable for 
#  each activity and each subject. 

  melted <- melt(mergestt, id=c("subject","activites"))
  #creation of the new database
  tidy <- dcast(melted, subject+activites ~ variable, mean)

  data.table::fwrite(x = tidy, file = "tidyData.txt", row.names = FALSE)
  
}
