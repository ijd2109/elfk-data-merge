#last extraction on 6/28/2018
# Made by Michelle Leon
#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# 8-item GRIT QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
GRIT_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_GRIT_Questionnaire.csv")
#view
head(GRIT_Q)
names(GRIT_Q)
nrow(GRIT_Q)
ncol(GRIT_Q)

#get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(GRIT_Q) #this step is to update number of rows based on new data
#remove extra rows
GRIT_Q.1<-GRIT_Q[6:row_num,]
#to delete additional rows (with fake data) in the data
GRIT_Q.2<- GRIT_Q.1[-c(96,97,123), ]

# get rid of extra columns with random qualtrics info
GRIT_Q.2<- GRIT_Q.2[,8:27] 
GRIT_Q.3 <- GRIT_Q.2[,-c(2,3,4,5,6,7,8,9,10)]
#to delete additional date column
GRIT_Q.4 <- GRIT_Q.3[ , -c(3)]
#view
nrow(GRIT_Q.4)
ncol(GRIT_Q.4)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- GRIT_Q.4$Q12
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
GRIT_Q.4$Q12 <- IDENT_SUBID

# start relabeling columns 
names(GRIT_Q.4)
ncol(GRIT_Q.4)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("GRIT_DATE_COMPLETE", "IDENT_SUBID", "GRIT_1", "GRIT_2", "GRIT_3", "GRIT_4", 
                   "GRIT_5", "GRIT_6", "GRIT_7", "GRIT_8")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(GRIT_Q.4) <- var_names

#Convert data to Data Manual Codes. 
# Only need to code GRIT_2,4,7,8 b/c they are reverse scored.  GRIT_1,3,5,6 are fine. 

GRIT_Q.4$GRIT_2R<- ifelse(grepl(1, GRIT_Q.4$GRIT_2), 5, ifelse(grepl(2, GRIT_Q.4$GRIT_2), 4, ifelse(grepl(3, GRIT_Q.4$GRIT_2), 3, ifelse(grepl(4, GRIT_Q.4$GRIT_2), 2, ifelse(grepl(5, GRIT_Q.4$GRIT_2), 1, "")))))
GRIT_Q.4$GRIT_4R<- ifelse(grepl(1, GRIT_Q.4$GRIT_4), 5, ifelse(grepl(2, GRIT_Q.4$GRIT_4), 4, ifelse(grepl(3, GRIT_Q.4$GRIT_4), 3, ifelse(grepl(4, GRIT_Q.4$GRIT_4), 2, ifelse(grepl(5, GRIT_Q.4$GRIT_4), 1, "")))))
GRIT_Q.4$GRIT_7R<- ifelse(grepl(1, GRIT_Q.4$GRIT_7), 5, ifelse(grepl(2, GRIT_Q.4$GRIT_7), 4, ifelse(grepl(3, GRIT_Q.4$GRIT_7), 3, ifelse(grepl(4, GRIT_Q.4$GRIT_7), 2, ifelse(grepl(5, GRIT_Q.4$GRIT_7), 1, "")))))
GRIT_Q.4$GRIT_8R<- ifelse(grepl(1, GRIT_Q.4$GRIT_8), 5, ifelse(grepl(2, GRIT_Q.4$GRIT_8), 4, ifelse(grepl(3, GRIT_Q.4$GRIT_8), 3, ifelse(grepl(4, GRIT_Q.4$GRIT_8), 2, ifelse(grepl(5, GRIT_Q.4$GRIT_8), 1, "")))))

# convert factor to numeric
GRIT_Q.4$GRIT_1 <- as.numeric(as.character(GRIT_Q.4$GRIT_1))
GRIT_Q.4$GRIT_2 <- as.numeric(as.character(GRIT_Q.4$GRIT_2))
GRIT_Q.4$GRIT_3 <- as.numeric(as.character(GRIT_Q.4$GRIT_3))
GRIT_Q.4$GRIT_4 <- as.numeric(as.character(GRIT_Q.4$GRIT_4))
GRIT_Q.4$GRIT_5 <- as.numeric(as.character(GRIT_Q.4$GRIT_5))
GRIT_Q.4$GRIT_6 <- as.numeric(as.character(GRIT_Q.4$GRIT_6))
GRIT_Q.4$GRIT_7 <- as.numeric(as.character(GRIT_Q.4$GRIT_7))
GRIT_Q.4$GRIT_8 <- as.numeric(as.character(GRIT_Q.4$GRIT_8))
GRIT_Q.4$GRIT_2R <- as.numeric(as.character(GRIT_Q.4$GRIT_2R))
GRIT_Q.4$GRIT_4R <- as.numeric(as.character(GRIT_Q.4$GRIT_4R))
GRIT_Q.4$GRIT_7R <- as.numeric(as.character(GRIT_Q.4$GRIT_7R))
GRIT_Q.4$GRIT_8R <- as.numeric(as.character(GRIT_Q.4$GRIT_8R))

# write to csv file on elvis
write.csv(GRIT_Q.4, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_GRIT_Questionnaire.csv" , row.names = F)

                                                                                                                                                                                                                   