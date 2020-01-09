# Made by Sean Minns
# Dec 2018

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# JustSTAT QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
P_STAI_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Adult_Stai_Questionnaire_STATE1-20.csv", stringsAsFactors =FALSE)


#remove extra rows and test data
P_STAI_Q.1<-P_STAI_Q[-c(1:8),]

# get rid of extra columns with random qualtrics info, gender, age and the extra date  
P_STAI_Q.3 <- P_STAI_Q.1[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21)]

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
P_STAI_Q.4 <- P_STAI_Q.3

library(stringr)
subject <- P_STAI_Q.4$Q1
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
# add parent to name
IDENT_SUBID2 <- paste(IDENT_SUBID,"P", sep="")

# replace subject with your new subjectid 
P_STAI_Q.4$Q1 <- IDENT_SUBID2

# start relabeling columns 
# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("PARENT_STAI_STATE_DATE_COMPLETE", "IDENT_SUBID", "PARENT_STAI_STATE_1", "PARENT_STAI_STATE_2", "PARENT_STAI_STATE_3", "PARENT_STAI_STATE_4", 
                   "PARENT_STAI_STATE_5", "PARENT_STAI_STATE_6", "PARENT_STAI_STATE_7", "PARENT_STAI_STATE_8", "PARENT_STAI_STATE_9", "PARENT_STAI_STATE_10", "PARENT_STAI_STATE_11", "PARENT_STAI_STATE_12", "PARENT_STAI_STATE_13", "PARENT_STAI_STATE_14", "PARENT_STAI_STATE_15", "PARENT_STAI_STATE_16", "PARENT_STAI_STATE_17", "PARENT_STAI_STATE_18", "PARENT_STAI_STATE_19","PARENT_STAI_STATE_20")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(P_STAI_Q.4) <- var_names
(P_STAI_Q.4)
#Convert data to Data Manual Codes. 
# Only need to code STAI_1 STAI_3 STAI_6 STAI_7 STAI_10 STAI_13 STAI_14 STAI_16 STAI_19

P_STAI_Q.4$PARENT_STAI_STATE_1R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_1), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_1), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_1), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_1), 1, ""))))
P_STAI_Q.4$PARENT_STAI_STATE_2R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_2), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_2), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_2), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_2), 1, ""))))
P_STAI_Q.4$PARENT_STAI_STATE_5R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_5), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_5), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_5), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_5), 1,  ""))))
P_STAI_Q.4$PARENT_STAI_STATE_8R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_8), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_8), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_8), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_8), 1, ""))))
P_STAI_Q.4$PARENT_STAI_STATE_11R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_11), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_11), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_11), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_11), 1, ""))))
P_STAI_Q.4$PARENT_STAI_STATE_15R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_15), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_15), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_15), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_15), 1, ""))))
P_STAI_Q.4$PARENT_STAI_STATE_16R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_16), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_16), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_16), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_16), 1, ""))))
P_STAI_Q.4$PARENT_STAI_STATE_19R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_19), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_19), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_19), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_19), 1,  ""))))
P_STAI_Q.4$PARENT_STAI_STATE_20R<- ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_STATE_20), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_STATE_20), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_STATE_20), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_STATE_20), 1, ""))))


#remove trait questions
P_STAI_Q.5 <- P_STAI_Q.4[,-c(23:42)]


# write to csv file on elvis
write.csv(P_STAI_Q.5, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_PARENT_STAI_STATE_Questionnaire.csv" , row.names = F, quote = F)

