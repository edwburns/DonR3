## run_analysis script used to do followng:
## 1) Merge training & test sets to create 1 data set
## 2) extracts only measurements on mean & standard deviation for each measurement
## NOTE: descriptive activity names used for activities in merged data set
## NOTE: appropriately label merged data set with descriptive variable names
## 3) from merged data set, creates 2nd independent tidy data set 
##    with the average of each variable for each activity & each subject
run_analysis<-function(){
## load library needed in run_analysis script
  library(dplyr)
  
  
  ## set up parameters for getting file for run_analysis script
  ###GCDFILE <- "C:/Users/misc/Documents/COURSERA/R PROGRAMMING/gcdfile.zip"
  
  ###if (!file.exists(GCDFILE))
  ###{
  ###   GCDURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  ## download data set run_analysis script (GCD -> getting clean data)
  ###   download.file(GCDURL, GCDFILE, method = 'curl')
  ###}   
  
  ###if (!file.exists("UCI HAR Dataset"))
  ###{  
  ###    unzip(GCDFILE)
  ###}  
  
  ## assign names to each data frame
  ## following scheme used for dataset names (XXXYYYY(y)_Z):
  ## 1) XXX = UHD -> know that UCI HAR Dataset used
  ## 2) YYYY(Y) = type of data involved:
  ##    A) FEATS = features
  ##    B) ACTS = activities
  ##    C) TEST = testing
  ##    D) TRAIN = training
  ## 3) _Z = group experiencing testing or training:s
  ##    A) _S -> subjects
  ##    B) _X -> subjects
  ##    C) _Y -> subjects
                             
  UHDFEATS  <- read.table("UCI HAR Dataset/features.txt", col.names = c("N","FUNCTIONS"))
  FEATNAMES <- as.list(UHDFEATS$FUNCTIONS)
  UHDACTS   <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("CODE","ACTIVITY"))
  UHDTEST_S  <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = c("SUBJECT"))
  UHDTEST_X  <- read.table("UCI HAR Dataset/test/x_test.txt", col.names = FEATNAMES)
  UHDTEST_Y  <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "CODE")
  UHDTRAIN_S <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "SUBJECT")
  UHDTRAIN_X <- read.table("UCI HAR Dataset/train/x_train.txt", col.names = FEATNAMES)
  UHDTRAIN_Y <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "CODE")
  
  ## merge test & training data frames to create a singular data set
  ## steps are: 1) create CTTX data set by row binding _X sets for test & training
  ##            2) create CTTY data set by row binding _Y sets for test & training
  ##            3) create CTTS data set by row binding _S sets for test & training
  ##            4) create MSYX data set by column binding CTTS, CTTY & CTTX
  ## assign names to each data frame
  ## following scheme used for dataset names with row & column binding
  ## 1) leading character: C -> COMBINE; M -> MERGE
  ## 2) rest of characters: A) TTX -> test & training for x set
  ##                        B) TTY -> test & training for y set 
  ##                        c) TTS -> test & training for s set
  ##                        NOTE: row binding done in each of CTTSX, CTTY & CTTS             
  ##                        D) SYX -> column binding of CTTX, CTTY & CTTS
  
  CTTX <- rbind(UHDTEST_X, UHDTRAIN_X)
  CTTY <- rbind(UHDTEST_Y, UHDTRAIN_Y)
  CTTS <- rbind(UHDTEST_S, UHDTRAIN_S)
  
  MSYX <- cbind(CTTS, CTTY, CTTX)
  
  ## only want mean & standard deviation (std) from merged data set
  
  ONLYNEED <- MSYX %>% select(SUBJECT, CODE, contains("mean"), contains("std"))
  
  ## give descriptive names to activities in data set 'ONLYNEED'
  
  names(ONLYNEED) [2] = "ACTIVITY"
  names(ONLYNEED) <- gsub("Acc", "ACCELEROMETER", names(ONLYNEED))
  names(ONLYNEED) <- gsub("Gyro", "GYROSCOPE", names(ONLYNEED))
  names(ONLYNEED) <- gsub("BodyBody", "BODY", names(ONLYNEED))
  names(ONLYNEED) <- gsub("Mag", "MAGNITUDE", names(ONLYNEED))
  names(ONLYNEED) <- gsub("^t", "TIME", names(ONLYNEED))
  names(ONLYNEED) <- gsub("^f", "FREQUENCY", names(ONLYNEED))
  names(ONLYNEED) <- gsub("tBody", "TIME BODY", names(ONLYNEED))
  names(ONLYNEED) <- gsub("-mean()", "MEAN", names(ONLYNEED))
  names(ONLYNEED) <- gsub("-std()", "STD", names(ONLYNEED))
  names(ONLYNEED) <- gsub("-freq()", "FREQUENCY", names(ONLYNEED))
  names(ONLYNEED) <- gsub("angle", "ANGLE", names(ONLYNEED))
  names(ONLYNEED) <- gsub("gravity", "GRAVITY", names(ONLYNEED))
  
  ## start:ESB code
  ## Let use assign the activity descriptions to the activity number
  actvlbl <- read.delim2("./UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE, colClasses = c("integer","character") )
 
  actvlblTbl <- as_tibble(actvlbl)
  j <- inner_join(ONLYNEED, actvlblTbl, by = c("ACTIVITY" = "V1"))
  j <- mutate(j, ACTIVITY = V2)
  j <- select(j, -V2)
  ## create data set with average of each variable for each activity & subject
  
  LASTNEED <- j %>%
  ## LASTNEED <- ONLYNEED %>%    
  ## end:ESB code  
              group_by(SUBJECT, ACTIVITY) %>%
  ## start: ESB code
  ##          summarise_all(funs(mean))  * ddfuns deprecated
              summarise_all(mean)
  ## end:ESB code
  
  write.table(LASTNEED, "finalData.txt")
  
  ## check steps:
  ## 1) variable names of LASTNEED
  str(LASTNEED)
  
  ## 2) review of LASTNEED table
  print(LASTNEED)
}