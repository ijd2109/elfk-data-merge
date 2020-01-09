# last extraction 7/13/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# Alphabetical Index of Occupations and Industry Questionnaire 
################################################################################################

#row_num <- nrow(Caregiving_Q)

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
AIOI_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_AIOI_Questionnaire.csv")
#view
head(AIOI_Q)
names(AIOI_Q)
nrow(AIOI_Q)
ncol(AIOI_Q)

# get rid of extra columns with random qualtrics info
# [ rows, columns]
AIOI_Q<-AIOI_Q[,8:33]
AIOI_Q.1 <- AIOI_Q[,-c(2,3,4,5,6,7,8,9,10)]
#if you have already assigned new values for "other" columns, delete them. instructions in r-script manual
AIOI_Q.2<- AIOI_Q.1 [,-c(11,12,13)]

# get rid of extra rows with extra labels
row_num <-nrow(AIOI_Q) #this step is to update number of rows based on new data#
AIOI_Q.3<-AIOI_Q.2 [10:row_num,]
#to delete additional row (with fake data) in the data
AIOI_Q.4<- AIOI_Q.3[-c(3,4,5,67,29,63,46,58,34,87,96,98), ] 
#fix ID number - uses I instead of 1 
AIOI_Q.4$Q4 <-as.character(AIOI_Q.4$Q4)
AIOI_Q.4[36,2] = "EL061P"
AIOI_Q.4 [84,2] = "EL124P"

#view
nrow(AIOI_Q.4)
ncol(AIOI_Q.4)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- AIOI_Q.4$Q4
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
#add P since this is a Parent questionnaire! 
IDENT_SUBID_P <- paste(IDENT_SUBID, "P", sep="")

# replace subject with your new subjectid 
AIOI_Q.4$Q4 <- IDENT_SUBID_P

#create columns for parent education
AIOI_Q.4$AIOI_P1_EDU <- NA
AIOI_Q.4$AIOI_P2_EDU <- NA

#make a copy in order to compare original data to transfered and coded data
AIOI_Q.5 <- AIOI_Q.4


#if they circle more than one education level (Q1_1 - Q1_8) for same parent - put the highest number only. Delete other entries.
# this section edits data from 7/20/2018 extraction. Adds and removes.
# Needs to be checked for new data - if they circle more than one education level for same parent
AIOI_Q.5$Q1_1 <-as.character(AIOI_Q.5$Q1_1)
AIOI_Q.5$Q1_2 <-as.character(AIOI_Q.5$Q1_2)
AIOI_Q.5$Q1_3 <-as.character(AIOI_Q.5$Q1_3)
AIOI_Q.5$Q1_4 <-as.character(AIOI_Q.5$Q1_4)
AIOI_Q.5$Q1_5 <-as.character(AIOI_Q.5$Q1_5)
AIOI_Q.5$Q1_6 <-as.character(AIOI_Q.5$Q1_6)
AIOI_Q.5$Q1_7 <-as.character(AIOI_Q.5$Q1_7)
AIOI_Q.5$Q1_8 <-as.character(AIOI_Q.5$Q1_8)

AIOI_Q.5[1, 9] = ""
AIOI_Q.5[2, 3] = ""
AIOI_Q.5[9, 7] = ""
AIOI_Q.5[14, 3] = ""
AIOI_Q.5[14, 4] = ""
AIOI_Q.5[14, 7] = ""
AIOI_Q.5[15,3] = ""
AIOI_Q.5[15,4] = ""
AIOI_Q.5[15, 7] = "2"
AIOI_Q.5[15, 8] = ""
AIOI_Q.5[16, 3] = ""
AIOI_Q.5[16, 4] = ""
AIOI_Q.5[20, 3] = ""
AIOI_Q.5[20, 4] = ""
AIOI_Q.5[38, 3] = ""
AIOI_Q.5[38, 4] = ""
AIOI_Q.5[38, 7] = ""
AIOI_Q.5[41, 5] = ""
AIOI_Q.5[42, 3] = ""
AIOI_Q.5[42, 4] = ""
AIOI_Q.5[43, 3] = ""
AIOI_Q.5[43, 4] = ""
AIOI_Q.5[43, 5] = ""
AIOI_Q.5[43, 6] = ""
AIOI_Q.5[46, 3] = ""
AIOI_Q.5[46, 4] = ""
AIOI_Q.5[46, 7] = ""
AIOI_Q.5[46, 8] = ""
AIOI_Q.5[48, 3] = ""
AIOI_Q.5[48, 4] = ""
AIOI_Q.5[49, 3] = ""
AIOI_Q.5[49, 4] = ""
AIOI_Q.5[49, 5] = ""
AIOI_Q.5[49, 6] = ""
AIOI_Q.5[54, 3] = ""
AIOI_Q.5[54, 4] = ""
AIOI_Q.5[54, 6] = "2"
AIOI_Q.5[59, 3] = ""
AIOI_Q.5[59, 4] = ""
AIOI_Q.5[59, 5] = ""
AIOI_Q.5[67, 7] = ""
AIOI_Q.5[75, 3] = ""
AIOI_Q.5[75, 4] = ""
AIOI_Q.5[75, 7] = ""
AIOI_Q.5[75, 8] = "2"
AIOI_Q.5[81, 7] = ""
AIOI_Q.5[90, 3] = ""
AIOI_Q.5[90, 4] = ""
AIOI_Q.5[90, 7] = ""
AIOI_Q.5[90, 8] = "1"
AIOI_Q.5[92, 3] = ""
AIOI_Q.5[92, 4] = "2"

#Delete double entries for parents who have multiple siblings and answered this Q more than once. identical answers. 
AIOI_Q.5<-AIOI_Q.5[-c(94,97),]

#transfer info from Q1_1-Q1_8 to AIOI_P1_EDU and AIOI_P2_EDU. apply codes. 
AIOI_Q.5$AIOI_P1_EDU <- ifelse(grepl("1", AIOI_Q.5$Q1_1) , 1, 
                                ifelse(grepl("1", AIOI_Q.5$Q1_2), 2, 
                                ifelse(grepl("1", AIOI_Q.5$Q1_3), 4,
                                ifelse(grepl("1", AIOI_Q.5$Q1_4), 3,
                                ifelse(grepl("1", AIOI_Q.5$Q1_5), 5,
                                ifelse(grepl("1", AIOI_Q.5$Q1_6), 7,
                                ifelse(grepl("1", AIOI_Q.5$Q1_7), 8,
                                ifelse(grepl("1", AIOI_Q.5$Q1_8), 9, ""))))))))

AIOI_Q.5$AIOI_P2_EDU <- ifelse(grepl("2", AIOI_Q.5$Q1_1) , 1, 
                         ifelse(grepl("2", AIOI_Q.5$Q1_2), 2, 
                         ifelse(grepl("2", AIOI_Q.5$Q1_3), 4,
                         ifelse(grepl("2", AIOI_Q.5$Q1_4), 3,
                         ifelse(grepl("2", AIOI_Q.5$Q1_5), 5,
                         ifelse(grepl("2", AIOI_Q.5$Q1_6), 7,
                         ifelse(grepl("2", AIOI_Q.5$Q1_7), 8,
                         ifelse(grepl("2", AIOI_Q.5$Q1_8), 9, ""))))))))

#Now I can remove  columns Q1_1 - Q1_8
AIOI_Q.6 <- AIOI_Q.5 [,-c(3,4,5,6,7,8,9,10)]

# start relabeling columns 
names(AIOI_Q.6)
ncol(AIOI_Q.6)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("PARENT_AIOI_DATE_COMPLETE","PARENT_IDENT_SUBID", "PARENT_AIOI_P1_Occupation", "PARENT_AIOI_P1_Income", "PARENT_AIOI_P2_Occupation", "PARENT_AIOI_P2_Income", "PARENT_AIOI_P1_EDU", "PARENT_AIOI_P2_EDU" )

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(AIOI_Q.6) <- var_names

#reorder columns to match Manual
AIOI_Q.6 <- AIOI_Q.6[c("PARENT_AIOI_DATE_COMPLETE","PARENT_IDENT_SUBID","PARENT_AIOI_P1_EDU","PARENT_AIOI_P2_EDU", "PARENT_AIOI_P1_Occupation", "PARENT_AIOI_P1_Income", "PARENT_AIOI_P2_Occupation", "PARENT_AIOI_P2_Income")]

#Make copy in order to compare coded income with raw entry
AIOI_Q.7 <- AIOI_Q.6

#Remove letters and/or spaces in income entries (e.g. 140k will be converted to 140)
# Script edits data from 7/20/2018 extraction
# Needs to be checked for new data. If new data has letters or spaces, add edit to script and save. 
AIOI_Q.7$PARENT_AIOI_P1_Income <-as.character(AIOI_Q.7$AIOI_P1_Income)
AIOI_Q.7$PARENT_AIOI_P2_Income <-as.character(AIOI_Q.7$AIOI_P2_Income)
AIOI_Q.7[5,6] = "140,000"
AIOI_Q.7[5,8] = "120,000"
AIOI_Q.7[7,6] = "100,000"
AIOI_Q.7[7,8] = "100,000"
AIOI_Q.7[10,6] = "95,000"
AIOI_Q.7[10,8] = "58,000"
AIOI_Q.7[23,6] = "70,000"
AIOI_Q.7[23,8] = "95,000"
AIOI_Q.7[29,6] = "135,000"
AIOI_Q.7[35,6] = "120,000"
AIOI_Q.7[35,8] = "30,000"
AIOI_Q.7[56,8] = "180,000"
AIOI_Q.7[58,6] = "96,000,000"
AIOI_Q.7[63,6] = "70,000"
AIOI_Q.7[63,8] = "110,000"
AIOI_Q.7[66,6] = "400,000"
AIOI_Q.7[66,8] = "200,000"
AIOI_Q.7[84,6] = "125,000"
AIOI_Q.7[84,8] = "200,000"
AIOI_Q.7[97,6] = "400,000"

#make sure R reads income as numeric. remove any chararcters,letters,commas
#Any inputs that did not have numerical data (e.g. ???, idk) or left blank will be converted to character(0) and then NA. 
library(stringr)
income1<- AIOI_Q.7$PARENT_AIOI_P1_Income
income2<- AIOI_Q.7$PARENT_AIOI_P2_Income
i1<- str_extract_all(income1,"\\(?[0-9,.]+\\)?") # [[1]]
i2<- str_extract_all(income2,"\\(?[0-9,.]+\\)?") # [[1]]
AIOI_Q.7$PARENT_AIOI_P1_Income <-i1
AIOI_Q.7$PARENT_AIOI_P2_Income <- i2
#remove commas
AIOI_Q.7$PARENT_AIOI_P1_Income<- gsub(",", "", AIOI_Q.7$PARENT_AIOI_P1_Income, fixed = TRUE) 
AIOI_Q.7$PARENT_AIOI_P2_Income<- gsub(",", "", AIOI_Q.7$PARENT_AIOI_P2_Income, fixed = TRUE) 
#read as numeric values
AIOI_Q.7$PARENT_AIOI_P1_Income <- as.numeric(as.character(AIOI_Q.7$PARENT_AIOI_P1_Income))
AIOI_Q.7$PARENT_AIOI_P2_Income <- as.numeric(as.character(AIOI_Q.7$PARENT_AIOI_P2_Income))

#convert income to codes
AIOI_Q.7$PARENT_AIOI_P1_Income <- ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 10000, 1, ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 25000, 2,
ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 40000, 3, ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 55000, 4, ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 70000, 5,
ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 85000, 6, ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 100000, 7, ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 150000, 8,
ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 200000, 9, ifelse(AIOI_Q.7$PARENT_AIOI_P1_Income <= 1000000000, 10, 0))))))))))

AIOI_Q.7$PARENT_AIOI_P2_Income <- ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 10000, 1, ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 25000, 2,
ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 40000, 3, ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 55000, 4, ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 70000, 5,
ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 85000, 6, ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 100000, 7, ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 150000, 8,
ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 200000, 9, ifelse(AIOI_Q.7$PARENT_AIOI_P2_Income <= 1000000000, 10, 0))))))))))


# write to csv file on elvis
write.csv(AIOI_Q.7,  "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Parent_AIOI_Questionnaire.csv", row.names =  FALSE)
