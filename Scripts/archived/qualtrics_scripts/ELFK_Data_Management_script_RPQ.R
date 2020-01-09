# last extraction on 7/20/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# Relationship Problems QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
RPQ_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Relationship_Problems_Questionnaire.csv")

#view
head(RPQ_Q)
names(RPQ_Q)
nrow(RPQ_Q)
ncol(RPQ_Q)

# get rid of extra columns with random qualtrics info
RPQ_Q<-RPQ_Q[,8:26]
RPQ_Q.1 <- RPQ_Q[,-c(2,3,4,5,6,7,8,9)]
# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(RPQ_Q.1) #this step is to update number of rows based on new data#
RPQ_Q.2<-RPQ_Q.1[3:row_num,]

#view
nrow(RPQ_Q.2)
ncol(RPQ_Q.2)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- RPQ_Q.2$Q1
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
RPQ_Q.2$Q1 <- IDENT_SUBID

# start relabeling columns 
names(RPQ_Q.2)
ncol(RPQ_Q.2)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("RPQ_DATE_COMPLETE","IDENT_SUBID", "RPQ1", "RPQ2","RPQ3","RPQ4","RPQ5","RPQ6","RPQ7","RPQ8","RPQ9")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(RPQ_Q.2) <- var_names

#make another copy to compare change in coding
RPQ_Q.3 <- RPQ_Q.2
#Convert data to Data Manual Codes. 
RPQ_Q.3$RPQ1<- ifelse(grepl(4, RPQ_Q.3$RPQ1), 0, ifelse(grepl(3, RPQ_Q.3$RPQ1), 1, ifelse(grepl(2, RPQ_Q.3$RPQ1), 2, ifelse(grepl(1, RPQ_Q.3$RPQ1), 3, ""))))
RPQ_Q.3$RPQ2<- ifelse(grepl(4, RPQ_Q.3$RPQ2), 0, ifelse(grepl(3, RPQ_Q.3$RPQ2), 1, ifelse(grepl(2, RPQ_Q.3$RPQ2), 2, ifelse(grepl(1, RPQ_Q.3$RPQ2), 3, ""))))
RPQ_Q.3$RPQ3<- ifelse(grepl(4, RPQ_Q.3$RPQ3), 0, ifelse(grepl(3, RPQ_Q.3$RPQ3), 1, ifelse(grepl(2, RPQ_Q.3$RPQ3), 2, ifelse(grepl(1, RPQ_Q.3$RPQ3), 3, ""))))
RPQ_Q.3$RPQ4<- ifelse(grepl(4, RPQ_Q.3$RPQ4), 0, ifelse(grepl(3, RPQ_Q.3$RPQ4), 1, ifelse(grepl(2, RPQ_Q.3$RPQ4), 2, ifelse(grepl(1, RPQ_Q.3$RPQ4), 3, ""))))
RPQ_Q.3$RPQ5<- ifelse(grepl(4, RPQ_Q.3$RPQ5), 0, ifelse(grepl(3, RPQ_Q.3$RPQ5), 1, ifelse(grepl(2, RPQ_Q.3$RPQ5), 2, ifelse(grepl(1, RPQ_Q.3$RPQ5), 3, ""))))
RPQ_Q.3$RPQ6<- ifelse(grepl(4, RPQ_Q.3$RPQ6), 0, ifelse(grepl(3, RPQ_Q.3$RPQ6), 1, ifelse(grepl(2, RPQ_Q.3$RPQ6), 2, ifelse(grepl(1, RPQ_Q.3$RPQ6), 3, ""))))
RPQ_Q.3$RPQ7<- ifelse(grepl(4, RPQ_Q.3$RPQ7), 0, ifelse(grepl(3, RPQ_Q.3$RPQ7), 1, ifelse(grepl(2, RPQ_Q.3$RPQ7), 2, ifelse(grepl(1, RPQ_Q.3$RPQ7), 3, ""))))
RPQ_Q.3$RPQ8<- ifelse(grepl(4, RPQ_Q.3$RPQ8), 0, ifelse(grepl(3, RPQ_Q.3$RPQ8), 1, ifelse(grepl(2, RPQ_Q.3$RPQ8), 2, ifelse(grepl(1, RPQ_Q.3$RPQ8), 3, ""))))
RPQ_Q.3$RPQ9<- ifelse(grepl(4, RPQ_Q.3$RPQ9), 0, ifelse(grepl(3, RPQ_Q.3$RPQ9), 1, ifelse(grepl(2, RPQ_Q.3$RPQ9), 2, ifelse(grepl(1, RPQ_Q.3$RPQ9), 3, ""))))

#parent filled out the Q 3x for one sibling, keep the one done right after the other sibling (EL144). Delete the other two entries
RPQ_Q.3<- RPQ_Q.3[-c(42,48),]


# write to csv file on elvis
write.csv(RPQ_Q.3, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Relationship_Problems_Questionnaire.csv", row.names = FALSE)
