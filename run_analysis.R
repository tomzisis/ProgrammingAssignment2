# The code in run_analysis.R script,performs an analysis of the data from accelerometers from 
# the Samsung Galaxy S smartphones.First of all , We recommend to install the 'plyr','dplyr' 
# and 'reshape2' packages in R , in order to run the code successfully.
# Kindly find below,the comments for each part of the procedure:

# Lines 9-18: The compressed(.zip) file is downloaded from the correspodent internet link and 
# the basic folder 'UCI HAR Dataset' is extracted , in order to read the appropriate files.

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="UCI HAR Dataset.zip",mode="wb")
unzip("UCI HAR Dataset.zip",exdir=".")
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
subtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
subtest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
feats <- read.table("./UCI HAR Dataset/features.txt")
actlbs <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Lines 23-24: The 'actlbs' file contains the 6 activity names(check Codebook.md for further details)
# and we transform them to lowercase without underscores. 

actlbs = gsub("_","",actlbs$V2)
activ  = tolower(actlbs)

# Lines 31-33: Replacement of the column V1 in 'ytrain' and 'ytest' files with 
# the column 'activity' which contains the full descriptive 6 activity names and
# binding of the 2 files in one (yall).


ytrain <- mutate(ytrain,activity=factor(ytrain$V1,labels=activ))%>%select(-V1)
ytest <- mutate(ytest,activity=factor(ytest$V1,labels=activ))%>%select(-V1)
yall <- rbind(ytrain,ytest)

# Lines 39-41: We rename the column V1 in 'subtrain' and 'subtest' files with the name
# 'volunteer', which contains the number of each participant in the test (total 30) and
# bind the 2 files in one (suball).

subtrain <- rename(subtrain,volunteer=V1)
subtest <- rename(subtest,volunteer=V1)
suball <- rbind(subtrain,subtest)

# Lines 47-53: Transformation of the feature names(check Codebook.md for further details)
# without dashes,parentheses,double words and renaming the name columns in 'xtrain' and 'xtest' files 
# respectively with the transformed names of the features.Finally, we bind the 2 files in one (datall).  

feats <- select(feats,-V1)
feats$V2 <- gsub("-","",feats$V2)
feats$V2 <- gsub("[()]","",feats$V2) 
feats$V2 <- gsub("BodyBody","Body",feats$V2)
names(xtrain) <- feats$V2
names(xtest) <- feats$V2
datall <- rbind(xtrain,xtest)

# Lines 60-63: Combination of the 3 files 'datall','yall' and 'suball' in one (finalset1), 
# removing the duplicated columns and keeping only those which refer to mean value (mean)
# and standard deviation (std) measures of the analogous features along with 'volunteer'
# and 'activity' columns.At the end, sorting by 'volunteer' and 'activity' (finalset-data frame).

finalset1 <- cbind(datall,yall,suball)
finalset1 <- finalset1[,!duplicated(colnames(finalset1))]
g1 <- grep("(mean([a-zA-Z])?$|std)",colnames(finalset1))
finalset <- select(finalset1,volunteer,activity,g1)%>%arrange(volunteer,activity)

# Lines 69-71: Transformation of the gathered data (meltset) so that we can calculate  
# the average of the mean and std features for each volunteer and for each activity.
# Extraction in R console of the table with the final tidy data (finaldata-data frame).

meltset <- melt(finalset,id=c("volunteer","activity"),measure.vars=colnames(finalset1)[g1])
finaldata <- dcast(meltset,volunteer+activity~variable,mean)
View(finaldata)
    