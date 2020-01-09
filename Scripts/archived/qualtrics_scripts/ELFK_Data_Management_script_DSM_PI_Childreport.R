# last extraction on 5/18/2018
# Made by Michelle Leon
# checked by MVT Jan 2019
#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# KSADS-DSM QUESTIONNAIRE
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
DSM_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_DSM_Questionnaire.csv", stringsAsFactors = F)
#view
head(DSM_Q)
names(DSM_Q)
nrow(DSM_Q)
ncol(DSM_Q)

# get rid of extra columns with random qualtrics info
# [ rows, columns]
DSM_Q<- DSM_Q[,8:44]
DSM_Q.1<- DSM_Q[,-c(2,3,4,5,6,7,8,9,10)]
#remove other columns not required based on data entry manual
DSM_Q.2 <- DSM_Q.1[,-c(3,4,5)] 

# get rid of extra rows with extra labels
row_num <-nrow(DSM_Q.2) #this step is to update number of rows based on new data#
DSM_Q.3<-DSM_Q.2[3:row_num,]

#view
nrow(DSM_Q.3)
ncol(DSM_Q.3)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- DSM_Q.3$Q1
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
DSM_Q.3$Q1 <- IDENT_SUBID

# start relabeling columns 
names(DSM_Q.3)
ncol(DSM_Q.3)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("DSM_child_report_DATE_COMPLETE","IDENT_SUBID", "KSADS_DSM1_child_report", "KSADS_DSM2_child_report", "KSADS_DSM3_child_report", 
                   "KSADS_DSM4_child_report","KSADS_DSM5_child_report","KSADS_DSM6_child_report","KSADS_DSM7_child_report",
                   "KSADS_DSM8_child_report","KSADS_DSM9_child_report", "KSADS_DSM10_child_report", "KSADS_DSM11_child_report",
                   "KSADS_DSM12_child_report", "KSADS_DSM13_child_report", "KSADS_DSM14_child_report", "KSADS_DSM15_child_report",
                   "KSADS_DSM16_child_report", "KSADS_DSM17_child_report", "KSADS_DSM18_child_report", "KSADS_DSM19_child_report",
                   "KSADS_DSM20_child_report", "KSADS_DSM21_child_report", "KSADS_DSM22_child_report", "KSADS_DSM23_child_report")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(DSM_Q.3) <- var_names

#Convert data to Data Manual Codes. 
#makecopy to compare converted codes to original data

DSM_Q.4 <- DSM_Q.3
DSM_Q.4$KSADS_DSM1_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM1_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM1_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM1_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM1_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM2_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM2_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM2_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM2_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM2_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM3_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM3_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM3_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM3_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM3_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM4_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM4_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM4_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM4_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM4_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM5_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM5_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM5_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM5_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM5_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM6_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM6_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM6_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM6_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM6_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM7_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM7_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM7_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM7_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM7_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM8_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM8_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM8_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM8_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM8_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM9_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM9_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM9_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM9_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM9_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM10_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM10_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM10_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM10_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM10_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM11_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM11_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM11_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM11_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM11_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM12_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM12_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM12_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM12_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM12_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM13_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM13_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM13_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM13_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM13_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM14_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM14_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM14_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM14_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM14_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM15_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM15_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM15_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM15_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM15_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM16_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM16_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM16_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM16_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM16_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM17_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM17_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM17_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM17_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM17_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM18_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM18_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM18_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM18_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM18_child_report), 3, ""))))
DSM_Q.4$KSADS_DSM19_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM19_child_report), 0, ifelse(grepl(2, DSM_Q.4$KSADS_DSM19_child_report), 1, ifelse(grepl(3, DSM_Q.4$KSADS_DSM19_child_report), 2, ifelse(grepl(4, DSM_Q.4$KSADS_DSM19_child_report), 3, ""))))

DSM_Q.4$KSADS_DSM20_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM20_child_report), 1, ifelse(grepl(2, DSM_Q.4$KSADS_DSM20_child_report), 0, ""))
DSM_Q.4$KSADS_DSM21_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM21_child_report), 1, ifelse(grepl(2, DSM_Q.4$KSADS_DSM21_child_report), 0, ""))
DSM_Q.4$KSADS_DSM22_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM22_child_report), 1, ifelse(grepl(2, DSM_Q.4$KSADS_DSM22_child_report), 0, ""))
DSM_Q.4$KSADS_DSM23_child_report<- ifelse(grepl(1, DSM_Q.4$KSADS_DSM23_child_report), 1, ifelse(grepl(2, DSM_Q.4$KSADS_DSM23_child_report), 0, ""))

# write to csv file on elvis
write.csv(DSM_Q.4, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_DSM_Questionnaire.csv", row.names = FALSE, quote = F)
