Getting-Cleaning-Data-Course-Project
====================================

Producing a Tidy Data Set
# First I download data. Then place data into the working directory which is a sub-directory
# of the main wd I use for the Data Science Specialization courses from Coursera. The sub-directory for
# this project is called 'humanActivityRecog'
# I set the working directory to the new sub-directory:

setwd("./humanActivityRecog")

# view contents of new wd

dir()
"""
[1] "activity_labels.txt" "features.txt"        "features_info.txt"  
[4] "README.txt"          "test"                "train" 
"""

# Then I begin reading the different data sets in the folder file by file, labelling as I go
activity_set <- read.table("./activity_labels.txt")

# Have a look:
dim(activity_set)

# This is a small set, so I will name the variables then print it out
activity_set <- read.table("./activity_labels.txt",col.names=c("activity_number","activity_name"))
# Then print it out.
activity_set.
#Next is "features.txt"
features_set <- read.table("./features.txt")
dim(features_set)
# This is a larger set with multiple variables in a single column, and poorly named columns

# For now let's rename the columns:
features_set <- read.table("./features.txt", col.names=c("id", "detection_feature"))
# Note: naming the second column, anything in particular ends up being silly, since
# I will just poach the column's contents using [,2]

# This is going to be the names of our activities, so we can do some quick clean up here to help later.
# Tried using rename and sub, at first, but at last gsub

# gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
# fixed = FALSE, useBytes = FALSE)
features_set[,2] <- gsub("^f", "frequency", features_set[,2])
features_set[,2] <- gsub("^t", "time", features_set[,2])
features_set[,2] <- gsub("-mean", " mean", features_set[,2])
features_set[,2] <- gsub("-std", " std_deviation", features_set[,2])
features_set[,2] <- gsub("Acc", " acceleration", features_set[,2])
features_set[,2] <- gsub("Mag", " magnitude", features_set[,2])
features_set[,2] <- gsub("Body", " body", features_set[,2])
features_set[,2] <- gsub("angle.t", "angle-", features_set[,2])
features_set[,2] <- gsub("Gyro", " gyroscopic", features_set[,2])
features_set[,2] <- gsub("Jerk", " jerk", features_set[,2])

# Onto the next file
features_info <- read.table("./features_info.txt")

# But it is a readme type of file on the features, so I skip it and laugh at myself
# Test and train are folders, so we'll look into them. 
# "test" contains a sub-folder "Inertial Signals" as well as three files "subject_test"
# "X_test" and "y_test" 
# Likewise, "train" contains a sub-folder "Inertial Signals" as well as three files "subject_train"
# "X_train" and "y_train"

# So I need to combine the X and y tests (according to instruciton #1. "Merges the training and the test 
# sets to create one data set."
X_test_data <- read.table("./test/X_test.txt", col.names=features_set[,2]) 

#Note: after previous failures where I tried to add colnames in later, I decided to scrap the project and start over, this time
#putting in the names as I went.
X_train_data <- read.table("./train/X_train.txt", col.names=features_set[,2])
# now the y bits the same way:
y_test_data <- read.table("./test/y_test.txt", col.names="activity_number")
y_train_data <- read.table("./train/y_train.txt", col.names="activity_number")

# Here I messed up, maybe, the first time because I used rbind to put everything together before 
# putting in the subject id's, so this time I do it first here:
test_subjects <- read.table("./test/subject_test.txt", col.names="subject_number")
train_subjects <- read.table("./train/subject_train.txt", col.names="subject_number") 

# Here I am actually merging the training, and separately the testing in columns, not rows. First time I used rows
# and got everything jacked.
test_data_set <- cbind(test_subjects, X_test_data, y_test_data)
train_data_set <- cbind(train_subjects, X_train_data, y_train_data)

# Now I want to merge the testing and training into a single frame. Also, where I made the error of using rbind
# for everything above the first time, I see now it's at this step that the rbind comes in.
test_train_combined <- rbind(test_data_set, train_data_set)

# I'd like to make rows 1:6 match the description in the activity_set
rownames(test_train_combined) <- activity_set[,2]
# That didn't work, so I try:
test_train_combined$activity_number <- c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing", "Laying")
# So next I try:
testTrain_all <- merge(activity_set[,2], test_train_combined, all=TRUE)

# That did it, but now I have no label on my first column, so:
colnames(testTrain_all)[1] <- "activity"

# Now I need to figure out how to get everything mean and sd - (which is labeled "std" in this data set)
# going to try with "dplyr" package
library(dplyr)
testTrain_all <- tbl_df(test_train_combined)
get_mean <- filter(testTrain_all, "mean")
# But I forgot I can't use quotes here. Supposed to use equivalences
# But I did remember I can use mutate to join the activities list I tried above

# grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
     #fixed = FALSE, useBytes = FALSE, invert = FALSE)
get_mean <- grep("mean", names(testTrain_all), ignore.case =TRUE, fixed = FALSE)
get_std <- grep("std", names(testTrain_all), ignore.case =TRUE, fixed = FALSE)
desired_tidy_data1 <- testTrain_all[, c(1:2, get_mean, get_std)]

## Note - I just realized these can be combined, so:
get_meanStd <- testTrain_all[, grep("mean|std|subject_number|activity", names(testTrain_all))]

#So now I finally print names, and even though it's detailed, it's too long for columns. 
#So I need to shorten it. 
##Note: I will replace the initial renaming with this one on the run_analysis.R file
features_set[,2] <- gsub("^f", "freq", features_set[,2])
features_set[,2] <- gsub("^t", "time", features_set[,2])
features_set[,2] <- gsub("-mean", " mean", features_set[,2])
features_set[,2] <- gsub("-std", " std_Dev", features_set[,2])
features_set[,2] <- gsub("Acc", " accel", features_set[,2])
features_set[,2] <- gsub("Mag", " mag", features_set[,2])
features_set[,2] <- gsub("Body", " body", features_set[,2])
features_set[,2] <- gsub("angle.t", "angle-", features_set[,2])
features_set[,2] <- gsub("Gyro", " gyro", features_set[,2])
features_set[,2] <- gsub("Jerk", " jerk", features_set[,2])

#Now I need to sort the data (or thought I did. I abandoned this as you will see
order_data <- testTrain_all[order(testTrain_all$subject_number,testTrain_all$activity),]
##Note: I abandoned sort, as it alphabetized everything and led me on a wild dplyr goose-chase
##I was trying to use "arrange" and "select" in dplyr to rearrange everything and pull out columns, it was no good.
## I am sparing you all my excess codeage :)

# Now I can get the mean of the variables for each activity and each subject

# aggregate(x = testDF, by = list(fby1, fby2), FUN = "mean")
tidy_final <- aggregate(get_meanStd, by=list(activity = get_meanStd$activity, subject_number = get_meanStd$subject_number), mean)

# This threw many a warning and is adding in duplicate columns. 
# I tried using "exclude = "" but it only through an error.
# Finally I realized I could tell aggregate but leave 
# out the activity and subject_number by using a list and != 
# So:
tidy_final <- aggregate(get_meanStd[,names(get_meanStd) != c("activity", "subject_number")],by=list(activity=get_meanStd$activity,subject_number=get_meanStd$subject_number),mean)

#Finally I write to a file using write.csv 

# write.csv(file = "foo.csv", x=my_data)
write.csv(file="tidy_data.csv", x=get_meanStd)
write.csv(file="tidy_final.csv", x=tidy_final)

## Haha - this too turned out to be incorrect, as per the instructions we were to use "write.table" and 
## save to text file with row.name=FALSE
# So:
write.table(tidy_final, file="tidy_data.txt", row.name=FALSE)



