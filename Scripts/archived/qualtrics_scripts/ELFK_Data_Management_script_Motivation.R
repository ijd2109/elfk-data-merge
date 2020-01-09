#last extraction on 5/18/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# MOTIVATION QUESTIONNAIRE  
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
motiv_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Motivation_Questionnaire.csv")
#view
head(motiv_Q)
names(motiv_Q)
nrow(motiv_Q)
ncol(motiv_Q)

# get rid of extra columns with random qualtrics info
motiv_Q <- motiv_Q[, 8:22]
motiv_Q.1 <- motiv_Q[, -c(2,3,4,5,6,7,8,9)]

#get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(motiv_Q.1) #this step is to update number of rows based on new data
#remove extra rows
motiv_Q.2<-motiv_Q.1[4:row_num,]

#changeIDforrepeatedentry
motiv_Q.2$Q1 <-as.character(motiv_Q.2$Q1)
motiv_Q.2[29,2] = "EL024"
motiv_Q.2[115,2] = "EL132"

#view
nrow(motiv_Q.2)
ncol(motiv_Q.2)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- motiv_Q.2$Q1
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
motiv_Q.2$Q1 <- IDENT_SUBID

# start relabeling columns 
names(motiv_Q.2)
ncol(motiv_Q.2)

# naming variables to match ELFK data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("MOTIV_DATE_COMPLETE", "IDENT_SUBID", "MOTIVATION_1", "MOTIVATION_2", "MOTIVATION_3", "MOTIVATION_4", 
                   "MOTIVATION_5")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(motiv_Q.2) <- var_names

#change factors to nuemric
motiv_Q.2$MOTIVATION_1 <- as.numeric(as.character(motiv_Q.2$MOTIVATION_1))
motiv_Q.2$MOTIVATION_2 <- as.numeric(as.character(motiv_Q.2$MOTIVATION_2))
motiv_Q.2$MOTIVATION_3 <- as.numeric(as.character(motiv_Q.2$MOTIVATION_3))
motiv_Q.2$MOTIVATION_4 <- as.numeric(as.character(motiv_Q.2$MOTIVATION_4))
motiv_Q.2$MOTIVATION_5 <- as.numeric(as.character(motiv_Q.2$MOTIVATION_5))


# write to csv file on lux
write.csv(motiv_Q.2, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_HigginsMotivation_Questionnaire.csv", row.names = FALSE)
