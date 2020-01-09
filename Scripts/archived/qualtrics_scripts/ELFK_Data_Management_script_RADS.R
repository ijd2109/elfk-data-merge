# last extraction on 7/20/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# Reactive Attachment Disorder Symptoms Checklist QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
RADS_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_RADS_Questionnaire.csv")

#view
head(RADS_Q)
names(RADS_Q)
nrow(RADS_Q)
ncol(RADS_Q)

# get rid of extra columns with random qualtrics info
RADS_Q<-RADS_Q[,8:44]
RADS_Q.1 <- RADS_Q[,-c(2,3,4,5,6,7,8,9,10)]
# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(RADS_Q.1) #this step is to update number of rows based on new data#
RADS_Q.2<-RADS_Q.1[3:row_num,]

#view
nrow(RADS_Q.2)
ncol(RADS_Q.2)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- RADS_Q.2$Q1
# removing characters /letters and x is only numbers
x <- str_extract_all(subject,"\\(?[0-9,.]+\\)?") # [[1]]
subject_num <- unlist(x)
first_num <- substr(subject_num, 1,1)

# removing zero if first number
subject_num_fixed <- as.numeric(ifelse(first_num == "0", substring(subject_num, 2), subject_num))

# adding EL and zeros as needed
IDENT_SUBID <- ifelse(subject_num_fixed < 10, paste0("EL00", subject_num_fixed), ifelse(subject_num_fixed <100, 
                                                                                        paste0("EL0", subject_num_fixed), 
                                                                                        ifelse(subject_num_fixed >= 100, paste0("EL", subject_num_fixed), NA)))
# replace subject with your new subjectid 
RADS_Q.2$Q1 <- IDENT_SUBID

# start relabeling columns 
names(RADS_Q.2)
ncol(RADS_Q.2)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("RADS_DATE_COMPLETE","IDENT_SUBID", "RADS1", "RADS2", "RADS3", "RADS4", "RADS5", "RADS6", "RADS7", "RADS8", "RADS9", "RADS10",
                   "RADS11", "RADS12", "RADS13", "RADS14", "RADS15", "RADS16", "RADS17", "RADS18", "RADS19", "RADS20",
                   "RADS21", "RADS22", "RADS23", "RADS24", "RADS25", "RADS26")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(RADS_Q.2) <- var_names

#make another copy to compare change in coding
RADS_Q.3 <- RADS_Q.2

#Convert data to Data Manual Codes. 
RADS_Q.3$RADS18<- ifelse(grepl(2, RADS_Q.3$RADS18), 0, ifelse(grepl(1, RADS_Q.3$RADS18), 1, ""))
RADS_Q.3$RADS19<- ifelse(grepl(2, RADS_Q.3$RADS19), 0, ifelse(grepl(1, RADS_Q.3$RADS19), 1, ""))
RADS_Q.3$RADS20<- ifelse(grepl(2, RADS_Q.3$RADS20), 0, ifelse(grepl(1, RADS_Q.3$RADS20), 1, ""))
RADS_Q.3$RADS21<- ifelse(grepl(2, RADS_Q.3$RADS21), 0, ifelse(grepl(1, RADS_Q.3$RADS21), 1, ""))
RADS_Q.3$RADS22<- ifelse(grepl(2, RADS_Q.3$RADS22), 0, ifelse(grepl(1, RADS_Q.3$RADS22), 1, ""))
RADS_Q.3$RADS23<- ifelse(grepl(2, RADS_Q.3$RADS23), 0, ifelse(grepl(1, RADS_Q.3$RADS23), 1, ""))
RADS_Q.3$RADS24<- ifelse(grepl(2, RADS_Q.3$RADS24), 0, ifelse(grepl(1, RADS_Q.3$RADS24), 1, ""))
RADS_Q.3$RADS25<- ifelse(grepl(2, RADS_Q.3$RADS25), 0, ifelse(grepl(1, RADS_Q.3$RADS25), 1, ""))
RADS_Q.3$RADS26<- ifelse(grepl(2, RADS_Q.3$RADS26), 0, ifelse(grepl(1, RADS_Q.3$RADS26), 1, ""))

#Parent made mistake with IDs for siblings. Last completed is 146, first one is 147. edit ID
RADS_Q.3[41,2] <- "EL146"
# conver to numeric 

RADS_Q.3$RADS1 <- as.numeric(as.character(RADS_Q.3$RADS1))
RADS_Q.3$RADS2 <- as.numeric(as.character(RADS_Q.3$RADS2))
RADS_Q.3$RADS3 <- as.nueric(as.character(RADS_Q.3$RADS3))
RADS_Q.3$RADS4 <- as.numeric(as.character(RADS_Q.3$RADS4))
RADS_Q.3$RADS5 <- as.numeric(as.character(RADS_Q.3$RADS5))
RADS_Q.3$RADS6 <- as.numeric(as.character(RADS_Q.3$RADS6))
RADS_Q.3$RADS7 <- as.numeric(as.character(RADS_Q.3$RADS7))
RADS_Q.3$RADS8 <- as.numeric(as.character(RADS_Q.3$RADS8))
RADS_Q.3$RADS9 <- as.numeric(as.character(RADS_Q.3$RADS9))
RADS_Q.3$RADS10 <- as.numeric(as.character(RADS_Q.3$RADS10))
RADS_Q.3$RADS11 <- as.numeric(as.character(RADS_Q.3$RADS11))
RADS_Q.3$RADS12 <- as.numeric(as.character(RADS_Q.3$RADS12))
RADS_Q.3$RADS13 <- as.numeric(as.character(RADS_Q.3$RADS13))
RADS_Q.3$RADS14 <- as.numeric(as.character(RADS_Q.3$RADS14))
RADS_Q.3$RADS15 <- as.numeric(as.character(RADS_Q.3$RADS15))
RADS_Q.3$RADS16 <- as.numeric(as.character(RADS_Q.3$RADS16))
RADS_Q.3$RADS17 <- as.numeric(as.character(RADS_Q.3$RADS17))
RADS_Q.3$RADS18 <- as.numeric(as.character(RADS_Q.3$RADS18))
RADS_Q.3$RADS19 <- as.numeric(as.character(RADS_Q.3$RADS19))
RADS_Q.3$RADS20 <- as.numeric(as.character(RADS_Q.3$RADS20))
RADS_Q.3$RADS21 <- as.numeric(as.character(RADS_Q.3$RADS21))
RADS_Q.3$RADS22 <- as.numeric(as.character(RADS_Q.3$RADS22))
RADS_Q.3$RADS23 <- as.numeric(as.character(RADS_Q.3$RADS23))
RADS_Q.3$RADS24 <- as.numeric(as.character(RADS_Q.3$RADS24))
RADS_Q.3$RADS25 <- as.numeric(as.character(RADS_Q.3$RADS25))
RADS_Q.3$RADS26 <- as.numeric(as.character(RADS_Q.3$RADS26))

# write to csv file on elvis
write.csv(RADS_Q.3, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_RADS_Questionnaire.csv", row.names = FALSE)
