library(tidyverse)

########################
### Download dataset ###
########################


if(!file.exists("./HARUSDaataset")) {
  dir.create("./HARUSDataset")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./HARUSDataset/dataset.zip")

unzip(zipfile = "./HARUSDataset/dataset.zip", exdir = "./HARUSDataset/")


###################
### Data import ###
###################


train_data <- read.table("./HARUSDataset/UCI HAR Dataset/train/X_train.txt")
train_labs <- read.table("./HARUSDataset/UCI HAR Dataset/train/y_train.txt")
train_subject_id <- read.table("./HARUSDataset/UCI HAR Dataset/train/subject_train.txt")

test_data <- read.table("./HARUSDataset/UCI HAR Dataset/test/X_test.txt")
test_labs <- read.table("./HARUSDataset/UCI HAR Dataset/test/y_test.txt")
test_subject_id <- read.table("./HARUSDataset/UCI HAR Dataset/test/subject_test.txt")

var_names <- read.table("./HARUSDataset/UCI HAR Dataset/features.txt")


# 1. Merge the training and test sets into one data set

# merge data and add column names
data <- rbind(train_data, test_data)                 
names(data) <- var_names[,2]

# merge activity labels
activity_labels <- rbind(train_labs, test_labs)   

# merge subject IDs
subject_id <- rbind(train_subject_id, test_subject_id)

# column-bind all three to create the complete dataset
data <- cbind(activity_labels, subject_id, data)

# fix the first to column names
colnames(data)[1:2] <- c("Activity", "SubjectID")


# 2. Extract the measurements on the mean and standard deviation for each measurement

data <- data %>% 
  select(Activity,
         SubjectID,
         contains(match = c("mean()", "std")))


# 3. Descriptive activity names

data <- data %>% 
  mutate(ActivityName = case_when(Activity == 1 ~ "WALKING",
                                  Activity == 2 ~ "WALKING UPSTAIRS",
                                  Activity == 3 ~ "WALKING DOWNSTAIRS",
                                  Activity == 4 ~ "SITTING",
                                  Activity == 5 ~ "STANDING",
                                  Activity == 6 ~ "LAYING")) %>% 
  select(Activity, ActivityName, everything())


# 4. Descriptive variable names

names(data) <- gsub(pattern = "-mean\\(\\)-", replacement = "Mean\\-", x = names(data))
names(data) <- gsub(pattern = "-std\\(\\)-", replacement = "STD\\-", x = names(data))
names(data) <- gsub(pattern = "^t", replacement = "Time", x = names(data))
names(data) <- gsub(pattern = "^f", replacement = "Fourier", x = names(data))
names(data) <- gsub(pattern = "Acc", replacement = "Acceleration", x = names(data))

                    
# 5. Summarise by activity and subject

data_tidy <- data %>% 
  group_by(SubjectID, ActivityName) %>% 
  arrange(subject_id, ActivityName)
  summarise_all(.tbl=.,
                .funs = mean) %>% 

    
##############
### Export ###
##############
  
write.table(data_tidy, "./HARUSDataset/dataTidy.txt")