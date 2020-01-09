#last extraction on 7/20/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
#Friendship Questionnaire
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
FQ <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Friendship_Questionnaire.csv")
#view
head(FQ)
names(FQ)
nrow(FQ)
ncol(FQ)

# [rows, columns]
# get rid of extra columns with random qualtrics info
FQ<- FQ[,8:40]
FQ.1 <- FQ[,-c(2,3,4,5,6,7,8,9)]

# get rid of extra rows with extra labels
row_num <-nrow(FQ.1) #this step is to update number of rows based on new data#
FQ.2<- FQ.1[3:row_num,]

#view
nrow(FQ.2)
ncol(FQ.2)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- FQ.2$Q26
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
FQ.2$Q26 <- IDENT_SUBID

# start relabeling columns 
names(FQ.2)
ncol(FQ.2)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("FQ_DATE_COMPLETE","IDENT_SUBID", "FQ_1", "FQ_2", "FQ_3", "FQ_4","FQ_5","FQ_5_OTHER","FQ_6","FQ_7", "FQ_8A", "FQ_8B",
                   "FQ_9", "FQ_10", "FQ_11", "FQ_12", "FQ_13", "FQ_14", "FQ_15", "FQ_16", "FQ_17", "FQ_18", "FQ_19", 
                   "FQ_20", "FQ_21")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(FQ.2) <- var_names

#Convert data to Data Manual Codes. 
FQ.2$FQ_8A<- ifelse(grepl(1, FQ.2$FQ_8A), 1, ifelse(grepl(2, FQ.2$FQ_8A), 0, ""))
FQ.2$FQ_11<- ifelse(grepl(1, FQ.2$FQ_11), 1, ifelse(grepl(2, FQ.2$FQ_11), 0, ""))
FQ.2$FQ_12<- ifelse(grepl(1, FQ.2$FQ_12), 1, ifelse(grepl(2, FQ.2$FQ_12), 0, ""))
#If there is an answer for 8B, 8A should be 1. If not, add here
FQ.2[18,11] = 1 #EL119
FQ.2[25,11] = 1 #EL122
FQ.2[32,11] = 1 #EL141
FQ.2[37,11] = 1 #EL104
FQ.2[40,11] = 1 #EL139
FQ.2[41,11] = 1 #EL041
FQ.2[42,11] = 1 #EL039
FQ.2[43,11] = 1 #EL040

# removing characters /letters and x is only numbers
#this will convert blanks to character(0)
library(stringr)
friendage <- FQ.2$FQ_18
fage <- str_extract_all(friendage,"\\(?[0-9,.]+\\)?") # [[1]]
FQ.2$FQ_18 <- fage
#edit answers for those who had characters/letters
FQ.2[5,22] = 14 #EL097
FQ.2[6,22] = 14 #EL099
FQ.2[22,22] = 6 #EL127

#convert age to months
FQ.2$FQ_18<- as.numeric(as.character(FQ.2$FQ_18))
fqage <-FQ.2$FQ_18* 12
FQ.2$FQ_18 <- fqage

#create last column. Read what was written in FQ_21. If you agree that there is something noteworthy mentioned here about the 
#child’s closest friend - code as 1. Code as 0- even if the parent thought there was something noteworthy, but it’s not really adding to the story. 
FQ.2$FQ_21_UNUSUAL <- NA
FQ.2[3,26] = 1
FQ.2[9,26] = 1
FQ.2[10,26] = 1
FQ.2[15,26] = 1
FQ.2[18,26] = 1
FQ.2[22,26] = 1
FQ.2[26,26] = 1
FQ.2[30,26] = 1
FQ.2[34,26] = 1
FQ.2[36,26] = 1
FQ.2[37,26] = 1
FQ.2[41,26] = 1
FQ.2[44,26] = 1
FQ.2[46,26] = 1
FQ.2[48,26] = 1
FQ.2[52,26] = 1
FQ.2[53,26] = 1
FQ.2[54,26] = 1
FQ.2[55,26] = 1
FQ.2[57,26] = 1
#NAs should be converted to 0 in FQ_21_UNUSUAL
FQ.2$FQ_21_UNUSUAL[is.na(FQ.2$FQ_21_UNUSUAL)] <- 0

#delete duplicates. Keep original for 104. 
FQ.2<- FQ.2[-c(50),]
#Parent made mistake with IDs for siblings. Last completed is 144, first one is 145. edit ID
FQ.2[54,2]<- "EL145"

#convert factors to numeric
FQ.2$FQ_1 <- as.numeric(as.character(FQ.2$FQ_1))
FQ.2$FQ_2 <- as.numeric(as.character(FQ.2$FQ_2))
FQ.2$FQ_3 <- as.numeric(as.character(FQ.2$FQ_3))
FQ.2$FQ_4 <- as.numeric(as.character(FQ.2$FQ_4))
FQ.2$FQ_5 <- as.numeric(as.character(FQ.2$FQ_5))
FQ.2$FQ_6 <- as.numeric(as.character(FQ.2$FQ_6))
FQ.2$FQ_7 <- as.numeric(as.character(FQ.2$FQ_7))
FQ.2$FQ_8A <- as.numeric(as.character(FQ.2$FQ_8A))
FQ.2$FQ_8B <- as.numeric(as.character(FQ.2$FQ_8B))
FQ.2$FQ_9 <- as.numeric(as.character(FQ.2$FQ_9))
FQ.2$FQ_10 <- as.numeric(as.character(FQ.2$FQ_10))
FQ.2$FQ_11 <- as.numeric(as.character(FQ.2$FQ_11))
FQ.2$FQ_12 <- as.numeric(as.character(FQ.2$FQ_12))
FQ.2$FQ_13 <- as.numeric(as.character(FQ.2$FQ_13))
FQ.2$FQ_14 <- as.numeric(as.character(FQ.2$FQ_14))
FQ.2$FQ_17 <- as.numeric(as.character(FQ.2$FQ_17))
FQ.2$FQ_18 <- as.numeric(as.character(FQ.2$FQ_18))
FQ.2$FQ_19 <- as.numeric(as.character(FQ.2$FQ_19))
# write to csv file on elvis
write.csv(FQ.2, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Friendship_Questionnaire.csv", row.names = FALSE)
