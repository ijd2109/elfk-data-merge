#last qualtrics extraction on 7/11/2018 - one new ELFK subject and PACCT/ELFK overlapping subject data
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# Peterson Development Scale: Male Questionnaire
################################################################################################

#michelle vantieghem edited Sept 18, 2018 
# to fix EL042 puberty - incorrectly labeled EL041 on qualtrics
# confirmed by checking date with SUBID key for behavioral visit date. 

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
Peterson_male_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Peterson_Male_Questionnaire_7.11.2018.csv")
#view
head(Peterson_male_Q)
names(Peterson_male_Q)
nrow(Peterson_male_Q)
ncol(Peterson_male_Q)

# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(Peterson_male_Q) #this step is to update number of rows based on new data#
Peterson_male_Q.1<-Peterson_male_Q[3:row_num,]
#delete rows with extra info
Peterson_male_Q.2<- Peterson_male_Q.1[-c(53,62,60,59,10, 20, 24, 25, 26, 31, 34),]

# get rid of extra columns with random qualtrics info
Peterson_male_Q.2<- Peterson_male_Q.2[,8:28]
Peterson_male_Q.3 <- Peterson_male_Q.2[,-c(2,3,4,5,6,7,8,9)]
#to delete additional columns in the data
Peterson_male_Q.4<- Peterson_male_Q.3[,-c(3) ] 
Peterson_male_Q.5<- Peterson_male_Q.4[,-c(3) ]

#view
nrow(Peterson_male_Q.5)
ncol(Peterson_male_Q.5)

# switch ID for EL041P duplicate on 12/6/15 -- actually EL042
Peterson_male_Q.5$Q1_1[13] <- "EL042"

#Fix wrong Ids for EL061P 
Peterson_male_Q.5$Q1_1 <- as.character(Peterson_male_Q.5$Q1_1)
Peterson_male_Q.5 [29,2] = "EL061P"

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- Peterson_male_Q.5$Q1_1
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
Peterson_male_Q.5$Q1_1 <- IDENT_SUBID

library(stringr)
height <- Peterson_male_Q.5$Q10_1
height_2 <-Peterson_male_Q.5$Q10_2
# removing characters /letters and x is only numbers
#this will convert blanks to character(0)
h <- str_extract_all(height,"\\(?[0-9,.]+\\)?") # [[1]]
Peterson_male_Q.5$Q10_1 <- h
h_2 <- str_extract_all(height_2,"\\(?[0-9,.]+\\)?") # [[1]]
Peterson_male_Q.5$Q10_2 <- h_2


#R needs to read the data as numeric or else it thinks it is a factor.
#You will be replacing blanks with NAs
Peterson_male_Q.5$Q10_1 <- as.numeric(as.character(Peterson_male_Q.5$Q10_1))
Peterson_male_Q.5$Q10_2 <- as.numeric(as.character(Peterson_male_Q.5$Q10_2))

#For height - convert feet to inches
Peterson_height <-Peterson_male_Q.5$Q10_1 * 12
Peterson_male_Q.5$Q10_1 <- Peterson_height

#Add data from Q_10_1 and Q_10_2
#This formula will replace NAs into 0s. This formula allows R not to convert additions with NA to NA. Keeps numbers.
#Replace all 0s into blanks on output excel file (PPDS_M_7_HEIGHT)
sum_height <- rowSums(cbind(Peterson_male_Q.5$Q10_1, Peterson_male_Q.5$Q10_2), na.rm = TRUE)
Peterson_male_Q.5$Q10_2 <-sum_height
#Remove column Q10_1
Peterson_male_Q.6<- Peterson_male_Q.5[,-c(9) ] 

#FIX WRONG DATA ENTRIES
Peterson_male_Q.6 [15,9] = 44
Peterson_male_Q.6 [16,9] = 45
Peterson_male_Q.6 [20,9] = 61.5
Peterson_male_Q.6 [41,9] = 44

# start relabeling columns 
names(Peterson_male_Q.6)
ncol(Peterson_male_Q.6)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("PPDS_M_DATE_COMPLETE","IDENT_SUBID", "PPDS_M_1", "PPDS_M_2", "PPDS_M_3", "PPDS_M_4","PPDS_M_5","PPDS_M_6",
                   "PPDS_M_7_HEIGHT","PPDS_M_8_WEIGHT")
# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(Peterson_male_Q.6) <- var_names

#Remove any characters/letters for PPDS_M_8_WEIGHT
#Any inputs that did not have numerical data (e.g. ???, idk) or left blank will be converted to character(0). 
#library(stringr)
weight <- Peterson_male_Q.6$PPDS_M_8_WEIGHT
# removing characters /letters 
w <- str_extract_all(weight,"\\(?[0-9,.]+\\)?") # [[1]]
Peterson_male_Q.6$PPDS_M_8_WEIGHT <-w
#R needs to read the data as numeric or else it thinks it is a factor.
#You will replacing any character(0) with NAs
Peterson_male_Q.6$PPDS_M_8_WEIGHT <- as.numeric(as.character(Peterson_male_Q.6$PPDS_M_8_WEIGHT))
#Replace NAs to blanks in output excel sheet (PPDS_F_9_WEIGHT)

#replace data from child version.Decided with Michelle VT and Tricia in April. Child data added here.
Peterson_male_Q.6 [1,3] = 2
Peterson_male_Q.6 [1,4] = 3
Peterson_male_Q.6 [1,5] = 3
Peterson_male_Q.6 [1,6] = 4 
Peterson_male_Q.6 [1,7] = 2
Peterson_male_Q.6 [1,8] = 4
Peterson_male_Q.6 [1,9] = 71
Peterson_male_Q.6 [1,10] = 150
Peterson_male_Q.6 [2,3] = 2
Peterson_male_Q.6 [2,4] = 1
Peterson_male_Q.6 [2,5] = 2
Peterson_male_Q.6 [2,6] = 2 
Peterson_male_Q.6 [2,7] = 3
Peterson_male_Q.6 [2,8] = 3
Peterson_male_Q.6 [2,9] = 61.5
Peterson_male_Q.6 [2,10] = 126
Peterson_male_Q.6 [3,3] = 3
Peterson_male_Q.6 [3,4] = 4
Peterson_male_Q.6 [3,5] = 3
Peterson_male_Q.6 [3,6] = 2 
Peterson_male_Q.6 [3,7] = 1
Peterson_male_Q.6 [3,8] = 3
Peterson_male_Q.6 [3,9] = 66
Peterson_male_Q.6 [3,10] = 140
Peterson_male_Q.6 [8,3] = 3
Peterson_male_Q.6 [8,4] = 3
Peterson_male_Q.6 [8,5] = 3
Peterson_male_Q.6 [8,6] = 4
Peterson_male_Q.6 [8,7] = 3
Peterson_male_Q.6 [8,8] = 3
Peterson_male_Q.6 [8,9] = 67
Peterson_male_Q.6 [8,10] = 115
Peterson_male_Q.6 [9,3] = 3
Peterson_male_Q.6 [9,4] = 3
Peterson_male_Q.6 [9,5] = 3
Peterson_male_Q.6 [9,6] = 3
Peterson_male_Q.6 [9,7] = 2
Peterson_male_Q.6 [9,8] = 3
Peterson_male_Q.6 [9,9] = 64
Peterson_male_Q.6 [9,10] = 115

############################################
#add overlapping PACCT/ELFK subject data
# these are subjects who did PACCT behavioral visit, and ELFK scan visit 
# so their puberty data is stored differently. 
PPDS_M_Q <- read.csv ("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/PAACT_ELFK_Overlapping_Qs/Peterson_male_PACCT_ELFK_subjects.csv")
#view
head(PPDS_M_Q)
names(PPDS_M_Q)
nrow(PPDS_M_Q)
ncol(PPDS_M_Q)

##### reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject_pacct <- PPDS_M_Q$IDENT_ID
# removing characters /letters and x is only numbers
x_pacct <- str_extract_all(subject_pacct,"\\(?[0-9,.]+\\)?") # [[1]]
subject_num_pacct <- unlist(x_pacct)
first_num_pacct <- substr(subject_num_pacct, 1,1)

# removing zero if first number
subject_num_fixed_pacct <- as.numeric(ifelse(first_num_pacct == "0", substring(subject_num_pacct, 2), subject_num_pacct))

# adding EL and zeros as needed
IDENT_SUBID_pacct <- ifelse(subject_num_fixed_pacct < 10, paste0("EL00", subject_num_fixed_pacct), 
                            ifelse(subject_num_fixed_pacct <100, paste0("EL0", subject_num_fixed_pacct), 
                                   ifelse(subject_num_fixed_pacct >= 100, paste0("EL", subject_num_fixed_pacct), NA)))
# replace subject with your new subjectid 
PPDS_M_Q$IDENT_ID <- IDENT_SUBID_pacct

#Edit PACCT subject IDs into ELFK subject IDs
PPDS_M_Q[1,1] = "EL144"
PPDS_M_Q[2,1] = "EL145"
PPDS_M_Q[3,1] = "EL148"

#ADD missing column
PPDS_M_Q$PPDS_M_DATE_COMPLETE<- NA

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names_pacct <- cbind("IDENT_SUBID", "PPDS_M_1", "PPDS_M_2", "PPDS_M_3", "PPDS_M_4","PPDS_M_5","PPDS_M_6",
                   "PPDS_M_7_HEIGHT","PPDS_M_8_WEIGHT", "PPDS_M_DATE_COMPLETE")
# make this to check column length
ncolvars_pacct<-ncol(var_names_pacct)
# now change the variable names of our dataframe to match.
names(PPDS_M_Q) <- var_names_pacct

#put columns in right order 
PPDS_M_Q <- PPDS_M_Q[c("PPDS_M_DATE_COMPLETE","IDENT_SUBID", "PPDS_M_1", "PPDS_M_2", "PPDS_M_3", "PPDS_M_4","PPDS_M_5","PPDS_M_6",
                       "PPDS_M_7_HEIGHT","PPDS_M_8_WEIGHT")]
# get subjectIDS for each dataframe.
PACCT_list <- PPDS_M_Q$IDENT_SUBID
ELFK_list <- Peterson_male_Q.6$IDENT_SUBID

# check that col names are identical before merging!! 
identical(names(PPDS_M_Q), names(Peterson_male_Q.6))

# add pacct data into SecurityScale_Q.4
PPDS_M_merged <- rbind(PPDS_M_Q,Peterson_male_Q.6)

# make sure all subjects were merged together successfully!! 
identical(nrow(PPDS_M_merged), nrow(PPDS_M_Q)+nrow(Peterson_male_Q.6))
Peterson_male_Q.6 <- PPDS_M_merged

# write to csv file on elvis
write.csv(Peterson_male_Q.6, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Peterson_Male_Questionnaire_new.csv", row.names = FALSE)

