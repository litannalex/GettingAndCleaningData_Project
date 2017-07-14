# cleaning the workspace
remove(list = ls())

# loading the libraries needed
library(reshape2)
library(dplyr)

# downloading and unzipping files
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "UCI HAR Dataset.zip")
download.file(URL, f)
unzip("UCI HAR Dataset.zip", exdir = getwd())

# opening files
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

# naming columns
names(x_test) <- features$V2
names(x_train) <- features$V2
names(y_test) <- "activity"
names(subject_test) <- "subject"
names(y_train) <- "activity"
names(subject_train) <- "subject"

# merging initial data into two sets, "test" and "train"
test <- bind_cols(subject_test, y_test, x_test)
train <- bind_cols(subject_train, y_train, x_train)

# merging test and train into one set, preserving the original set name in the
# variable "group"
merged <- bind_rows("test" = test, "train" = train, .id = "group")

# assigning activity names instead of numbers
oldvalues <- activity_labels[, 1]
newvalues <- activity_labels[, 2]
merged$activity <- newvalues[match(merged$activity, oldvalues)]

# cleaning workspace
remove(x_test, y_test, x_train, y_train, subject_test, subject_train, features,
       test, train, activity_labels, oldvalues, newvalues, URL, f)

# exctracting the columns containing mean and st deviation ("mean()", "std()"),
# not taking columns containing "gravityMean", "meanFreq", etc.
names_needed <- grepl("^group|^activity|^subject|[Mm]ean[:(:]|[Ss]td[:(:]",
                names(merged))
fin <- merged[names_needed == TRUE]

# cleaning the variable names
names(fin) <- gsub("[Ss]td", "StDev", names(fin))
names(fin) <- gsub("[Mm]ean", "Mean", names(fin))
names(fin) <- gsub("[:():]", "", names(fin))
names(fin) <- gsub("-", "", names(fin))
names(fin) <- gsub("^t", "time", names(fin))
names(fin) <- gsub("^f", "freq", names(fin))
names(fin) <- gsub("BodyBody", "Body", names(fin))

# creating a new tidy data set containing average of each variable for each
# activity and subject, also using "group" variable left after merging
fin_melted <- melt(fin, id = c("subject", "activity", "group"))
tidy <- dcast(fin_melted, subject + activity + group ~ variable, mean)

# saving file
write.table(tidy, "tidy.txt", row.names = FALSE, quote = FALSE)