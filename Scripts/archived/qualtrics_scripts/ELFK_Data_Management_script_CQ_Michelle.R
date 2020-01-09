# last extraction on 7/18/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# CAREGIVING QUESTIONNAIRE 
################################################################################################

#row_num <- nrow(Caregiving_Q)

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
Caregiving_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Caregiving_Questionnaire.csv")
#view
head(Caregiving_Q)
names(Caregiving_Q)
nrow(Caregiving_Q)
ncol(Caregiving_Q)

# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(Caregiving_Q) #this step is to update number of rows based on new data#
Caregiving_Q.1<-Caregiving_Q[6:row_num,]
Caregiving_Q.2<- Caregiving_Q.1[-c(2, 35,66), ] #to delete additional row (with fake data) in the data
# get rid of extra columns with random qualtrics info
Caregiving_Q.2<- Caregiving_Q.2[,8:21]
Caregiving_Q.3 <- Caregiving_Q.2[-c(2,3,4,5,6,7,8,9,10)]
#view
nrow(Caregiving_Q.3)
ncol(Caregiving_Q.3)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- Caregiving_Q.3$Q4
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
# only add P for Parent questionnaire! 
#IDENT_SUBID <- ifelse(grepl("P", subject) | grepl('p', subject), paste0(IDENT_SUBID, "P"), IDENT_SUBID)

# replace subject with your new subjectid 
Caregiving_Q.3$Q4 <- IDENT_SUBID

# start relabeling columns 
names(Caregiving_Q.3)
ncol(Caregiving_Q.3)


# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("CQ_DATE_COMPLETE", "IDENT_SUBID", "CQ_1", "CQ_2", "CQ_3")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(Caregiving_Q.3) <- var_names
#Make sure R reads characters are characters 
class(Caregiving_Q.3$CQ_1)
Caregiving_Q.3$CQ_1 <-as.character(Caregiving_Q.3$CQ_1)
Caregiving_Q.3$CQ_2 <- as.character(Caregiving_Q.3$CQ_2)
Caregiving_Q.3$CQ_3 <- as.character(Caregiving_Q.3$CQ_3)

#fix answers that need to be edited before perfoming coding or else codes will enter different answers (wrong answers)
#this is for old data... new data is coded correctly on Qualtrics
#add all old data that was removed from qualtrics when CQ questionnaire was edited. 
#New data will already be in but old data (until EL115) needs to be added

Caregiving_Q.3[1,3] = "1"
Caregiving_Q.3[1,4] = "1"
Caregiving_Q.3[1,5] = "1"
Caregiving_Q.3[2,3] = "0"
Caregiving_Q.3[2,4] = "1"
Caregiving_Q.3[2,5] = "1"
Caregiving_Q.3[3,3] = "0.5"
Caregiving_Q.3[3,4] = "Not sure"
Caregiving_Q.3[3,5] = "0"
Caregiving_Q.3[4,3] = "0.5"
Caregiving_Q.3[4,4] = "1"
Caregiving_Q.3[4,5] = "0"
Caregiving_Q.3[5,3] = "0.5"
Caregiving_Q.3[5,4] = "1"
Caregiving_Q.3[5,5] = "1"
Caregiving_Q.3[6,3] = "1"
Caregiving_Q.3[6,4] = "1"
Caregiving_Q.3[6,5] = "1"
Caregiving_Q.3[7,3] = "1"
Caregiving_Q.3[7,4] = "1"
Caregiving_Q.3[7,5] = "1"
Caregiving_Q.3[8,3] = "0.5"
Caregiving_Q.3[8,4] = "1"
Caregiving_Q.3[8,5] = "0"
Caregiving_Q.3[9,3] = "1"
Caregiving_Q.3[9,4] = "1"
Caregiving_Q.3[9,5] = "1"
Caregiving_Q.3[10,3] = "1"
Caregiving_Q.3[10,4] = "1"
Caregiving_Q.3[10,5] = "1"
Caregiving_Q.3[11,3] = "0.5"
Caregiving_Q.3[11,4] = "0"
Caregiving_Q.3[11,5] = "0"
Caregiving_Q.3[12,3] = "0.5"
Caregiving_Q.3[12,4] = "1"
Caregiving_Q.3[12,5] = "1"
Caregiving_Q.3[13,3] = "1"
Caregiving_Q.3[13,4] = "1"
Caregiving_Q.3[13,5] = "1"
Caregiving_Q.3[14,3] = "1"
Caregiving_Q.3[14,4] = "1"
Caregiving_Q.3[14,5] = "1"
Caregiving_Q.3[15,3] = "1"
Caregiving_Q.3[15,4] = "1"
Caregiving_Q.3[15,5] = "1"
Caregiving_Q.3[16,3] = "1"
Caregiving_Q.3[16,4] = "1"
Caregiving_Q.3[16,5] = "1"
Caregiving_Q.3[17,3] = "1"
Caregiving_Q.3[17,4] = "1"
Caregiving_Q.3[17,5] = "1"
Caregiving_Q.3[18,3] = "1"
Caregiving_Q.3[18,4] = "0.5"
Caregiving_Q.3[18,5] = "1"
Caregiving_Q.3[19,3] = "1" #No, this participant has data later on
Caregiving_Q.3[19,4] = "1" #No, this participant has data later on
Caregiving_Q.3[19,5] = "1" #No, this participant has data later on
Caregiving_Q.3[20,3] = "1"
Caregiving_Q.3[20,4] = "1"
Caregiving_Q.3[20,5] = "1"
Caregiving_Q.3[21,3] = "1"
Caregiving_Q.3[21,4] = "1"
Caregiving_Q.3[21,5] = "1"
Caregiving_Q.3[22,3] = "0.5"
Caregiving_Q.3[22,4] = "0.5"
Caregiving_Q.3[22,5] = "0.5"
Caregiving_Q.3[23,3] = "1"
Caregiving_Q.3[23,4] = "0.5"
Caregiving_Q.3[23,5] = "0.5"
Caregiving_Q.3[24,3] = "0.5"
Caregiving_Q.3[24,4] = "1"
Caregiving_Q.3[24,5] = "1"
Caregiving_Q.3[25,3] = "0.5"
Caregiving_Q.3[25,4] = "1"
Caregiving_Q.3[25,5] = "1"
Caregiving_Q.3[26,3] = "0.5"
Caregiving_Q.3[26,4] = "0"
Caregiving_Q.3[26,5] = "0"
Caregiving_Q.3[27,3] = "0.5"
Caregiving_Q.3[27,4] = "1"
Caregiving_Q.3[27,5] = "0"
Caregiving_Q.3[28,3] = "1"
Caregiving_Q.3[28,4] = "1"
Caregiving_Q.3[28,5] = "1"
Caregiving_Q.3[29,3] = "1"
Caregiving_Q.3[29,4] = "1"
Caregiving_Q.3[29,5] = "1"
Caregiving_Q.3[30,3] = ""
Caregiving_Q.3[30,4] = ""
Caregiving_Q.3[30,5] = ""
Caregiving_Q.3[31,3] = ""
Caregiving_Q.3[31,4] = ""
Caregiving_Q.3[31,5] = ""
Caregiving_Q.3[32,3] = ""
Caregiving_Q.3[32,4] = ""
Caregiving_Q.3[32,5] = ""

#Add same sex couple entry
Caregiving_Q.3 [86,2] = "EL143"
Caregiving_Q.3 [86,3] = 5
Caregiving_Q.3 [86,4] = 0.5
Caregiving_Q.3 [86,5] = 0.5

#convert the values to 

# write to csv file on lux
write.csv(Caregiving_Q.3, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Caregiving_Questionnaire.csv", row.names = FALSE)

