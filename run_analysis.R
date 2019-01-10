
# Section 0 ---------------------------------------------------------------
# Setting environment and loading packages
setwd("UCI HAR Dataset")
library(dplyr)
library(stringr)

# Section 1 ---------------------------------------------------------------
# Merging the training and the test sets to create one data set.

# 1.1 test set
test.set <- read.table("test/X_test.txt")
label <- read.table("test/Y_test.txt")
test.set$label <- label$V1
subject <- read.table("test/subject_test.txt")
test.set$subject <- subject$V1

# 1.2 train set
train.set <- read.table("train/X_train.txt")
label <- read.table("train/Y_train.txt")
train.set$label <- label$V1
subject <- read.table("train/subject_train.txt")
train.set$subject <- subject$V1

# 1.3 merge sets
set <- rbind(test.set, train.set)

# Section 2 ---------------------------------------------------------------
# Extracting only the measurements on the mean and standard deviation for each
# measurement.

feature <- read.table("features.txt", stringsAsFactors = FALSE)
colnames(set) <- c(feature$V2, "label", "subject")
set.sub <- set[,grepl("mean\\(\\)|std\\(\\)|label|subject", colnames(set))]
head(set.sub)

# Section 3 ---------------------------------------------------------------
# Using descriptive activity names to name the activities in the data set

activity <- read.table("activity_labels.txt")
set.sub$label <- factor(set.sub$label,
                        levels = activity$V1,
                        labels = activity$V2 )

# Section 4 ---------------------------------------------------------------
# Appropriately labeling the data set with descriptive variable names.

new.col <- colnames(set.sub) %>%
  str_replace("^t", "TimeDomain") %>%
  str_replace("^f", "FrequencyDomain") %>%
  str_replace("Mag", "Magnitude") %>%
  str_replace("Acc", "Accelormeter") %>%
  str_replace("Gyro", "Gyroscope") %>%
  str_replace("-mean\\(\\)", "MeanValue") %>%
  str_replace("-std\\(\\)", "StandardDeviation") %>%
  str_replace("-X$", "X-axis") %>%
  str_replace("-Y$", "Y-axis") %>%
  str_replace("-Z$", "Z-axis") %>%
  str_replace("(Body){2,}", "Body")
new.col
colnames(set.sub) <- new.col

# Section 5 ---------------------------------------------------------------
# Creating a tidy data set with the average of each variable for each activity
# and each subject.

tidy.data <- set.sub %>%
  select(subject, activity = label, everything()) %>%
  group_by(subject, activity) %>%
  summarise_all(mean)

# Output the data to "tidy_data.txt" file
write.table(tidy.data, "../tidy_data.txt", row.names = FALSE)
