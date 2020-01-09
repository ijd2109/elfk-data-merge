#last extraction 7/20/2018
# Made by Michelle Leon
# checked by MVT Jan 2019 
#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# KSADS-DSM PI-PARENT REPORTED QUESTIONNAIRE
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
DSMParent_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_DSM_PIParents_Questionnaire.csv", stringsAsFactors = FALSE)
#view
head(DSMParent_Q)
names(DSMParent_Q)
nrow(DSMParent_Q)
ncol(DSMParent_Q)

# get rid of extra columns with random qualtrics info
# [ rows, columns]
DSMParent_Q<-DSMParent_Q[,8:44]
DSMParent_Q.1 <- DSMParent_Q[,-c(2,3,4,5,6,7,8,9,10)]
#remove other columns not required based on data entry manual
DSMParent_Q.2<- DSMParent_Q.1[,-c(3,4,5)] 

# get rid of extra rows with extra labels
row_num <-nrow(DSMParent_Q.2) #this step is to update number of rows based on new data#
DSMParent_Q.3<-DSMParent_Q.2[3:row_num,]

#view
nrow(DSMParent_Q.3)
ncol(DSMParent_Q.3)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- DSMParent_Q.3$Q1
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
DSMParent_Q.3$Q1 <- IDENT_SUBID

# start relabeling columns 
names(DSMParent_Q.3)
ncol(DSMParent_Q.3)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("KSADS_PARENT_REPORT_DATE_COMPLETE","IDENT_SUBID", "KSADS_DSM1_parent_report", "KSADS_DSM2_parent_report", "KSADS_DSM3_parent_report",
                   "KSADS_DSM4_parent_report","KSADS_DSM5_parent_report","KSADS_DSM6_parent_report","KSADS_DSM7_parent_report",
                   "KSADS_DSM8_parent_report","KSADS_DSM9_parent_report", "KSADS_DSM10_parent_report",
                   "KSADS_DSM11_parent_report", "KSADS_DSM12_parent_report", "KSADS_DSM13_parent_report","KSADS_DSM14_parent_report",
                   "KSADS_DSM15_parent_report", "KSADS_DSM16_parent_report", "KSADS_DSM17_parent_report", "KSADS_DSM18_parent_report",
                   "KSADS_DSM19_parent_report", "KSADS_DSM20_parent_report", "KSADS_DSM21_parent_report", "KSADS_DSM22_parent_report",
                   "KSADS_DSM23_parent_report")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(DSMParent_Q.3) <- var_names

#Convert data to Data Manual Codes. 
#makecopy to compare converted codes to original data

DSMParent_Q.4 <- DSMParent_Q.3
DSMParent_Q.4$KSADS_DSM1_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM1_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM1_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM1_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM1_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM2_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM2_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM2_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM2_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM2_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM3_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM3_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM3_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM3_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM3_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM4_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM4_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM4_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM4_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM4_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM5_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM5_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM5_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM5_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM5_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM6_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM6_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM6_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM6_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM6_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM7_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM7_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM7_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM7_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM7_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM8_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM8_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM8_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM8_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM8_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM9_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM9_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM9_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM9_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM9_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM10_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM10_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM10_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM10_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM10_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM11_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM11_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM11_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM11_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM11_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM12_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM12_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM12_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM12_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM12_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM13_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM13_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM13_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM13_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM13_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM14_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM14_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM14_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM14_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM14_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM15_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM15_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM15_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM15_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM15_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM16_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM16_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM16_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM16_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM16_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM17_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM17_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM17_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM17_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM17_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM18_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM18_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM18_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM18_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM18_parent_report), 3, ""))))
DSMParent_Q.4$KSADS_DSM19_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM19_parent_report), 0, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM19_parent_report), 1, ifelse(grepl(3, DSMParent_Q.4$KSADS_DSM19_parent_report), 2, ifelse(grepl(4, DSMParent_Q.4$KSADS_DSM19_parent_report), 3, ""))))

DSMParent_Q.4$KSADS_DSM20_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM20_parent_report), 1, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM20_parent_report), 0, ""))
DSMParent_Q.4$KSADS_DSM21_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM21_parent_report), 1, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM21_parent_report), 0, ""))
DSMParent_Q.4$KSADS_DSM22_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM22_parent_report), 1, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM22_parent_report), 0, ""))
DSMParent_Q.4$KSADS_DSM23_parent_report<- ifelse(grepl(1, DSMParent_Q.4$KSADS_DSM23_parent_report), 1, ifelse(grepl(2, DSMParent_Q.4$KSADS_DSM23_parent_report), 0, ""))

#remove a double-entry. Keep most recent one since they were told to fill it again. 
DSMParent_Q.4 <- DSMParent_Q.4[-c(1),]

# write to csv file on elvis
write.csv(DSMParent_Q.4, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_DSMPIParents_Questionnaire.csv", row.names = FALSE)
