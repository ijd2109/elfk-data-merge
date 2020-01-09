# Made by Sean Minns

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# 12-item GRIT QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
P_GRIT_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Adult_Grit_Questionnaire.csv")

#view
head(P_GRIT_Q)
names(P_GRIT_Q)
nrow(P_GRIT_Q)
ncol(P_GRIT_Q)

#remove extra rows
P_GRIT_Q.1<-P_GRIT_Q[-c(1:3),]

#to delete additional rows (with fake data) in the data
P_GRIT_Q.2<-P_GRIT_Q.1[-c(1:6),]

# get rid of extra columns with random qualtrics info and the extra date  
P_GRIT_Q.3 <- P_GRIT_Q.2[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19)]

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe

P_GRIT_Q.4 <- P_GRIT_Q.3
library(stringr)
subject <- P_GRIT_Q.4$Q17
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
P_GRIT_Q.4$Q17 <- IDENT_SUBID2

# start relabeling columns 
# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("PARENT_GRIT_DATE_COMPLETE", "IDENT_SUBID", "PARENT_GRIT_1", "PARENT_GRIT_2", "PARENT_GRIT_3", "PARENT_GRIT_4", 
                   "PARENT_GRIT_5", "PARENT_GRIT_6", "PARENT_GRIT_7", "PARENT_GRIT_8", "PARENT_GRIT_9", "PARENT_GRIT_10", "PARENT_GRIT_11", "PARENT_GRIT_12")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(P_GRIT_Q.4) <- var_names

# convert factors to numeric
P_GRIT_Q.4$PARENT_GRIT_1 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_1))
P_GRIT_Q.4$PARENT_GRIT_2 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_2))
P_GRIT_Q.4$PARENT_GRIT_3 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_3))
P_GRIT_Q.4$PARENT_GRIT_4 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_4))
P_GRIT_Q.4$PARENT_GRIT_5 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_5))
P_GRIT_Q.4$PARENT_GRIT_6 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_6))
P_GRIT_Q.4$PARENT_GRIT_7 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_7))
P_GRIT_Q.4$PARENT_GRIT_8 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_8))
P_GRIT_Q.4$PARENT_GRIT_9 <- as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_9))
P_GRIT_Q.4$PARENT_GRIT_10 <-as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_10))
P_GRIT_Q.4$PARENT_GRIT_11 <-as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_11))
P_GRIT_Q.4$PARENT_GRIT_12 <-as.numeric(as.character(P_GRIT_Q.4$PARENT_GRIT_12))



#Convert data to Data Manual Codes. 
# Only need to code PARENT_GRIT_1,4,6,9,10 and 12 b/c they are reverse scored.  GRIT_1,3,5,6 are fine. 

P_GRIT_Q.4$PARENT_GRIT_1R<- ifelse(grepl(1, P_GRIT_Q.4$PARENT_GRIT_1), 5, ifelse(grepl(2, P_GRIT_Q.4$PARENT_GRIT_1), 4, ifelse(grepl(3, P_GRIT_Q.4$PARENT_GRIT_1), 3, ifelse(grepl(4, P_GRIT_Q.4$PARENT_GRIT_1), 2, ifelse(grepl(5, P_GRIT_Q.4$PARENT_GRIT_1), 1, "")))))
P_GRIT_Q.4$PARENT_GRIT_4R<- ifelse(grepl(1, P_GRIT_Q.4$PARENT_GRIT_4), 5, ifelse(grepl(2, P_GRIT_Q.4$PARENT_GRIT_4), 4, ifelse(grepl(3, P_GRIT_Q.4$PARENT_GRIT_4), 3, ifelse(grepl(4, P_GRIT_Q.4$PARENT_GRIT_4), 2, ifelse(grepl(5, P_GRIT_Q.4$PARENT_GRIT_4), 1, "")))))
P_GRIT_Q.4$PARENT_GRIT_6R<- ifelse(grepl(1, P_GRIT_Q.4$PARENT_GRIT_6), 5, ifelse(grepl(2, P_GRIT_Q.4$PARENT_GRIT_6), 4, ifelse(grepl(3, P_GRIT_Q.4$PARENT_GRIT_6), 3, ifelse(grepl(4, P_GRIT_Q.4$PARENT_GRIT_6), 2, ifelse(grepl(5, P_GRIT_Q.4$PARENT_GRIT_6), 1, "")))))
P_GRIT_Q.4$PARENT_GRIT_9R<- ifelse(grepl(1, P_GRIT_Q.4$PARENT_GRIT_9), 5, ifelse(grepl(2, P_GRIT_Q.4$PARENT_GRIT_9), 4, ifelse(grepl(3, P_GRIT_Q.4$PARENT_GRIT_9), 3, ifelse(grepl(4, P_GRIT_Q.4$PARENT_GRIT_9), 2, ifelse(grepl(5, P_GRIT_Q.4$PARENT_GRIT_9), 1, "")))))
P_GRIT_Q.4$PARENT_GRIT_10R<- ifelse(grepl(1, P_GRIT_Q.4$PARENT_GRIT_10), 5, ifelse(grepl(2, P_GRIT_Q.4$PARENT_GRIT_10), 4, ifelse(grepl(3, P_GRIT_Q.4$PARENT_GRIT_10), 3, ifelse(grepl(4, P_GRIT_Q.4$PARENT_GRIT_10), 2, ifelse(grepl(5, P_GRIT_Q.4$PARENT_GRIT_10), 1, "")))))
P_GRIT_Q.4$PARENT_GRIT_12R<- ifelse(grepl(1, P_GRIT_Q.4$PARENT_GRIT_12), 5, ifelse(grepl(2, P_GRIT_Q.4$PARENT_GRIT_12), 4, ifelse(grepl(3, P_GRIT_Q.4$PARENT_GRIT_12), 3, ifelse(grepl(4, P_GRIT_Q.4$PARENT_GRIT_12), 2, ifelse(grepl(5, P_GRIT_Q.4$PARENT_GRIT_12), 1, "")))))

P_GRIT_Q.4$PARENT_GRIT_1R <- as.numeric(P_GRIT_Q.4$PARENT_GRIT_1R) 
P_GRIT_Q.4$PARENT_GRIT_4R <- as.numeric(P_GRIT_Q.4$PARENT_GRIT_4R)
P_GRIT_Q.4$PARENT_GRIT_6R <- as.numeric(P_GRIT_Q.4$PARENT_GRIT_4R)
P_GRIT_Q.4$PARENT_GRIT_9R <- as.numeric(P_GRIT_Q.4$PARENT_GRIT_9R)
P_GRIT_Q.4$PARENT_GRIT_10R <- as.numeric(P_GRIT_Q.4$PARENT_GRIT_10R)
P_GRIT_Q.4$PARENT_GRIT_12R <- as.numeric(P_GRIT_Q.4$PARENT_GRIT_12R)

# write to csv file on elvis
write.csv(P_GRIT_Q.4, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Parent_GRIT_Questionnaire.csv" , row.names = F)

