# Last data extraction from Qualtrics - 7/13/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# Beck Depression Inventory QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
BDI1A_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_BDI_Questionnaire.csv")
#view
head(BDI1A_Q)
names(BDI1A_Q)
nrow(BDI1A_Q)
ncol(BDI1A_Q)

# get rid of extra columns with random qualtrics info
BDI1A_Q<- BDI1A_Q[,8:39]
BDI1A_Q.2<- BDI1A_Q[, -c(2,3,4,5,6,7,8,9)]
# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(BDI1A_Q.2) #this step is to update number of rows based on new data#
BDI1A_Q.3<-BDI1A_Q.2[9:row_num,]
BDI1A_Q.4<-BDI1A_Q.3[-c(37,59, 51,83), ] #to delete additional row (with fake data) in the data

#view
nrow(BDI1A_Q.4)
ncol(BDI1A_Q.4)

#Change labels El06IP and EL08IP so that they read as EL061P and EL081P
BDI1A_Q.4$Q24 <- as.character(BDI1A_Q.4$Q24)
BDI1A_Q.4[34,2] <- "EL061p"
BDI1A_Q.4[41,2] <- "EL081p"

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- BDI1A_Q.4$Q24
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
BDI1A_Q.4$Q24 <- IDENT_SUBID_P

# start relabeling columns 
names(BDI1A_Q.4)
ncol(BDI1A_Q.4)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("BDI1A_DATE_COMPLETE", "IDENT_SUBID", "BDI1A_1","BDI1A_2","BDI1A_3","BDI1A_4","BDI1A_5","BDI1A_6","BDI1A_7","BDI1A_8",
                   "BDI1A_9", "BDI1A_10", "BDI1A_11", "BDI1A_12", "BDI1A_13", "BDI1A_14", "BDI1A_15", "BDI1A_16", "BDI1A_17",
                   "BDI1A_18", "BDI1A_19", "BDI1A_20", "BDI1A_21", "BDI1A_22")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(BDI1A_Q.4) <- var_names

#Convert data to Data Manual Codes.
BDI1A_Q.4$BDI1A_1<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_1), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_1), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_1), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_1), 3, ""))))
BDI1A_Q.4$BDI1A_2<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_2), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_2), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_2), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_2), 3, ""))))
BDI1A_Q.4$BDI1A_3<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_3), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_3), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_3), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_3), 3, ""))))
BDI1A_Q.4$BDI1A_4<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_4), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_4), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_4), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_4), 3, ""))))
BDI1A_Q.4$BDI1A_5<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_5), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_5), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_5), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_5), 3, ""))))
BDI1A_Q.4$BDI1A_6<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_6), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_6), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_6), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_6), 3, ""))))
BDI1A_Q.4$BDI1A_7<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_7), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_7), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_7), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_7), 3, ""))))
BDI1A_Q.4$BDI1A_8<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_8), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_8), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_8), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_8), 3, ""))))
BDI1A_Q.4$BDI1A_9<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_9), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_9), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_9), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_9), 3, ""))))
BDI1A_Q.4$BDI1A_10<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_10), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_10), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_10), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_10), 3, ""))))
BDI1A_Q.4$BDI1A_11<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_11), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_11), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_11), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_11), 3, ""))))
BDI1A_Q.4$BDI1A_12<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_12), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_12), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_12), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_12), 3, ""))))
BDI1A_Q.4$BDI1A_13<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_13), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_13), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_13), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_13), 3, ""))))
BDI1A_Q.4$BDI1A_14<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_14), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_14), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_14), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_14), 3, ""))))
BDI1A_Q.4$BDI1A_15<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_15), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_15), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_15), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_15), 3, ""))))
BDI1A_Q.4$BDI1A_16<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_16), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_16), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_16), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_16), 3, ""))))
BDI1A_Q.4$BDI1A_17<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_17), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_17), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_17), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_17), 3, ""))))
BDI1A_Q.4$BDI1A_18<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_18), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_18), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_18), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_18), 3, ""))))
BDI1A_Q.4$BDI1A_19<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_19), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_19), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_19), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_19), 3, ""))))
BDI1A_Q.4$BDI1A_20<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_20), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_20), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_20), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_20), 3, ""))))
BDI1A_Q.4$BDI1A_21<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_21), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_21), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_21), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_21), 3, ""))))
BDI1A_Q.4$BDI1A_22<-ifelse(grepl(1, BDI1A_Q.4$BDI1A_22), 0, ifelse(grepl(2, BDI1A_Q.4$BDI1A_22), 1, ifelse(grepl(3, BDI1A_Q.4$BDI1A_22), 2, ifelse(grepl(4, BDI1A_Q.4$BDI1A_22), 3, ""))))

BDI1A_Q.5 <- BDI1A_Q.4
#Caculate subscores and total


write.csv(BDI1A_Q.5, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_BDI1A_Questionnaire.csv", row.names = FALSE)
