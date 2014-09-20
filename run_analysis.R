## Course Project ##

# Part 1: Creation of Dataset1

# Step 1. Reading data from files and conversion to clean data types

train_id <- as.integer(readLines("./UCI HAR Dataset/train/subject_train.txt"))
test_id <- as.integer(readLines("./UCI HAR Dataset/test/subject_test.txt"))

factornames <- readLines("./UCI HAR Dataset/activity_labels.txt")
factornames <- sapply(
    factornames,
    function(x) {
        idx <- grepRaw( " ", x)
        substr(x, (idx+1), nchar(x)) },
    USE.NAMES=FALSE )
factornames <- factor(
    factornames,
    levels=factornames )

train_type <- as.integer(readLines("./UCI HAR Dataset/train/y_train.txt"))
test_type <- as.integer(readLines("./UCI HAR Dataset/test/y_test.txt"))

feature_names <- readLines("./UCI HAR Dataset/features.txt")
feature_names <- sapply(
    feature_names,
    function(x) {
        idx <- grepRaw( " ", x)
        substr(x, (idx+1), nchar(x)) },
    USE.NAMES=FALSE )
train_feature <- read.csv(
    file="./UCI HAR Dataset/train/X_train.txt",
    sep="",
    header=FALSE )
colnames(train_feature) <- feature_names

test_feature <- read.csv(
    file="./UCI HAR Dataset/test/X_test.txt",
    sep="",
    header=FALSE )
colnames(test_feature) <- feature_names

# Step 2. Merge the train and test data sets

feature <- rbind( train_feature, test_feature)
type <- c( train_type, test_type)
id <- c( train_id, test_id)

type_str <- character(length(type))
for (i in 1:length(factornames)) {
    type_str[type==i] <- levels(factornames)[i]
}
type <- factor(
    type_str,
    levels=levels(factornames) )

# Step 3. Select correct columns from feature

feature_names <- colnames(feature)

idx <- sapply(feature_names,
              function(x) {
                  grepl("mean",x) | grepl("std",x)
              },
              USE.NAMES=FALSE )

feature <- feature[,idx]

# Step 4. Create the data frame for DataSet1

dataset1 <- data.frame(
    subject=id,
    activity=type,
    feature)

colnames(dataset1) <- c( "subject", "activity", colnames(feature))

# Step 5. Save the DataSet1 data frame to a csv file with txt extension

write.csv(
    dataset1,
    file="~/R/dataset1.txt",
    row.names = FALSE )

# Step 6. Example code to read DataSet1 properly (with proper data types)

dataset1_read <- read.csv(
    file="~/R/dataset1.txt",
    header=TRUE,
    check.names=FALSE,
    stringsAsFactors=FALSE)

dataset1_read[["activity"]] <- factor(
    dataset1_read[["activity"]],
    levels=c("WALKING",
             "WALKING_UPSTAIRS",
             "WALKING_DOWNSTAIRS",
             "SITTING",
             "STANDING",
             "LAYING") )

# Part 2: Creation of dataset 2

# Step 1. read Dataset1 from file
dataset1 <- read.csv(
    file="~/R/dataset1.txt",
    header=TRUE,
    check.names=FALSE,
    stringsAsFactors=FALSE)

dataset1[["activity"]] <- factor(
    dataset1[["activity"]],
    levels=c("WALKING",
             "WALKING_UPSTAIRS",
             "WALKING_DOWNSTAIRS",
             "SITTING",
             "STANDING",
             "LAYING") )

# Step 2. Process Dataset1 to fill the Dataset2 data frame

feature_mean <- grepl("mean", colnames(dataset1))
subjects <- as.numeric( rownames( table(dataset1[["subject"]]) ) )
activities <- levels(dataset1[["activity"]])

dataset2 <- data.frame(
    subject =integer (length(subjects)*length(activities)),
    activity=character(length(subjects)*length(activities)),
    matrix(
        data=numeric(sum(feature_mean) *
                         length(subjects)*
                         length(activities)),
        ncol=sum(feature_mean),
        nrow=length(subjects)*length(activities) ),
    stringsAsFactors=FALSE
)
colnames(dataset2) <- c("subject",
                        "activity",
                        colnames(dataset1)[feature_mean])
count <- 1
for (subject in subjects) {
    for (activity in activities) {
        idx_subject <- ( dataset1[["subject"]] == subject )
        idx_activity <- ( dataset1[["activity"]] == activity )
        subset_df <- dataset1[ idx_subject & idx_activity,
                               feature_mean ]
        dataset2[count,"subject"] <- subject
        dataset2[count,"activity"] <- activity
        dataset2[count,3:ncol(dataset2)] <- sapply( subset_df, mean)
        count <- count + 1
    }
}

dataset2[["activity"]] <- factor(
    dataset2[["activity"]],
    levels=c("WALKING",
             "WALKING_UPSTAIRS",
             "WALKING_DOWNSTAIRS",
             "SITTING",
             "STANDING",
             "LAYING") )

# Step 3. Save the DataSet2 data frame to a csv file with txt extension

write.csv(
    dataset2,
    file="~/R/dataset2.txt",
    row.names = FALSE )

# Step 4. Example code to read DataSet2 properly (with proper data types)

dataset2_read <- read.csv(
    file="~/R/dataset2.txt",
    header=TRUE,
    check.names=FALSE,
    stringsAsFactors=FALSE)

dataset2_read[["activity"]] <- factor(
    dataset2_read[["activity"]],
    levels=c("WALKING",
             "WALKING_UPSTAIRS",
             "WALKING_DOWNSTAIRS",
             "SITTING",
             "STANDING",
             "LAYING") )

## The End ##