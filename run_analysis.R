#Step1:Merge the two dataset
trainData <- read.table("./train/X_train.txt")
dim(trainData) #7352  561
head(trainData)
trainLabel <- read.table("./train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./train/subject_train.txt")
testData <- read.table("./test/X_test.txt")
dim(testData) #2947  561
testLabel <- read.table("./test/y_test.txt")
table(testLabel)
testSubject <- read.table("./test/subject_test.txt")
mergeData <- rbind(trainData, testData)
dim(mergeData) # 10299  561
mergeLabel <- rbind(trainLabel, testLabel)
dim(mergeLabel) # 10299  1
mergeSubject <- rbind(trainSubject, testSubject)
dim(mergeSubject) # 10299  1

#Step2:Extracts only the measurements on the mean and 
#standard deviation for each measurement. 
features <- read.table("./features.txt")
dim(features)  # 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
mergeData <- mergeData[, meanStdIndices]
dim(mergeData) # 10299*66
names(mergeData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
names(mergeData) <- gsub("mean", "Mean", names(mergeData)) 
names(mergeData) <- gsub("std", "Std", names(mergeData)) 
names(mergeData) <- gsub("-", "", names(mergeData)) 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[mergeLabel[, 1], 2]
mergeLabel[, 1] <- activityLabel
names(mergeLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(mergeSubject) <- "subject"
cleanedData <- cbind(mergeSubject, mergeLabel, mergeData)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(mergeSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
        for(j in 1:activityLen) {
                result[row, 1] <- sort(unique(mergeSubject)[, 1])[i]
                result[row, 2] <- activity[j, 2]
                bool1 <- i == cleanedData$subject
                bool2 <- activity[j, 2] == cleanedData$activity
                result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
                row <- row + 1
        }
}
head(result)
write.table(result, "finalresult.txt")






