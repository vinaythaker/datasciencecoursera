run_analysis <- function() {

        # read the test set
        test <- read_set("./UCI HAR Dataset/test/x_test.txt", "~/data/UCI HAR Dataset/test/subject_test.txt")
        
        # read the training set
        train <- read_set("./UCI HAR Dataset/train/x_train.txt", "~/data/UCI HAR Dataset/train/subject_train.txt")
        
        # 1. Merge the test and training set        
        tt <- merge(test, train, all=TRUE)
        
        # 2. Extracts only the measurements on the mean and standard deviation for each measurement
        cols <- c(562,1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,345,346,347,348,349,350,424,425,426,427,428,429,503,504,516,517,529,530,542,543)
        tt.1 <- tt[cols]
        
        # 3. Uses descriptive activity names to name the activities in the data set
        col.names <- c("subject", "tBodyAccMeanX", "tBodyAccMeanY", "tBodyAccMeanZ","tBodyAccStdX", "tBodyAccStdY", "tBodyAccStdZ", "tGravityAccMeanX", "tGravityAccMeanY", "tGravityAccMeanZ","tGravityAccStdX", "tGravityAccStdY", "tGravityAccStdZ", "tBodyAccJerkMeanX", "tBodyAccJerkMeanY", "tBodyAccJerkMeanZ", "tBodyAccJerkStdX", "tBodyAccJerkStdY", "tBodyAccJerkStdZ", "tBodyGyroMeanX", "tBodyGyroMeanY", "tBodyGyroMeanZ", "tBodyGyroStdX", "tBodyGyroStdY", "tBodyGyroStdZ", "tBodyGyroJerkMeanX", "tBodyGyroJerkMeanY", "tBodyGyroJerkMeanZ", "tBodyGyroJerkStdX", "tBodyGyroJerkStdY", "tBodyGyroJerkStdZ", "tBodyAccMagMean", "tBodyAccMagStd", "tGravityAccMagMean", "tBodyAccMagStd", "tBodyAccJerkMagMean", "tBodyAccJerkMagStd", "tBodyGyroMagMean", "tBodyGyroMagStd", "tBodyGyroJerkMagMean", "tBodyGyroJerkMagStd", "fBodyAccMeanX", "fBodyAccMeanY", "fBodyAccMeanZ", "fBodyAccStdX", "fBodyAccStdY", "fBodyAccStdZ", "fBodyAccJerkMeanX", "fBodyAccJerkMeanY", "fBodyAccJerkMeanZ", "fBodyAccJerkStdX", "fBodyAccJerkStdY", "fBodyAccJerkStdZ", "fBodyGyroMeanX", "fBodyGyroMeanY", "fBodyGyroMeanZ", "fBodyGyroStdX", "fBodyGyroStdY", "fBodyGyroStdZ", "fBodyAccMagnMean", "fBodyAccMagnStd", "fBodyAccJerkMagMean", "fBodyAccJerkMagStd", "fBodyGyroMagMean", "fBodyGyroMagStd", "fBodyGyroJerkMagMean", "fBodyGyroJerkMagStd")
        
        # 4. Appropriately labels the data set with descriptive activity names.
        colnames(tt.1) <- col.names
        
        # 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        ll <- split(tt.1, tt.1$subject)
        df <- data.frame()
        
        for(name in names(ll)) {
                d <- ll[[name]]
                df <- rbind(df, sapply(d,mean))
        }
        
        colnames(df) <- col.names
        
        # save the tidy set to a file called output.txt
        write.table(df, "./UCI HAR Dataset/output.txt")
}

read_set <- function(f1, f2) {
        # read the set
        s1 <- read.table(f1)
        
        # read the subject set
        subject <- read.table(f2)
        names(subject) <- c("subject")
        
        # add subject as a column to the  set
        s1$subject <- subject$subject
        
        s1
}