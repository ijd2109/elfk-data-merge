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
BDI_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_BDI_Questionnaire.csv")
#view
head(BDI_Q)
names(BDI_Q)
nrow(BDI_Q)
ncol(BDI_Q)

# get rid of extra columns with random qualtrics info
BDI_Q<- BDI_Q[,8:39]
BDI_Q.2<- BDI_Q[, -c(2,3,4,5,6,7,8,9)]
# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(BDI_Q.2) #this step is to update number of rows based on new data#
BDI_Q.3<-BDI_Q.2[9:row_num,]
BDI_Q.4<-BDI_Q.3[-c(37,59, 51,83), ] #to delete additional row (with fake data) in the data

#view
nrow(BDI_Q.4)
ncol(BDI_Q.4)

#Change labels El06IP and EL08IP so that they read as EL061P and EL081P
BDI_Q.4$Q24 <- as.character(BDI_Q.4$Q24)
BDI_Q.4[34,2] <- "EL061p"
BDI_Q.4[41,2] <- "EL081p"

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- BDI_Q.4$Q24
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
BDI_Q.4$Q24 <- IDENT_SUBID_P

# start relabeling columns 
names(BDI_Q.4)
ncol(BDI_Q.4)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("BDI_DATE_COMPLETE", "IDENT_SUBID", "BDI_1","BDI_2","BDI_3","BDI_4","BDI_5","BDI_6","BDI_7","BDI_8",
                   "BDI_9", "BDI_10", "BDI_11", "BDI_12", "BDI_13", "BDI_14", "BDI_15", "BDI_16", "BDI_17",
                   "BDI_18", "BDI_19", "BDI_20", "BDI_21", "BDI_22")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(BDI_Q.4) <- var_names

#Convert data to Data Manual Codes.
BDI_Q.4$BDI_1<-ifelse(grepl(1, BDI_Q.4$BDI_1), 0, ifelse(grepl(2, BDI_Q.4$BDI_1), 1, ifelse(grepl(3, BDI_Q.4$BDI_1), 2, ifelse(grepl(4, BDI_Q.4$BDI_1), 3, ""))))
BDI_Q.4$BDI_2<-ifelse(grepl(1, BDI_Q.4$BDI_2), 0, ifelse(grepl(2, BDI_Q.4$BDI_2), 1, ifelse(grepl(3, BDI_Q.4$BDI_2), 2, ifelse(grepl(4, BDI_Q.4$BDI_2), 3, ""))))
BDI_Q.4$BDI_3<-ifelse(grepl(1, BDI_Q.4$BDI_3), 0, ifelse(grepl(2, BDI_Q.4$BDI_3), 1, ifelse(grepl(3, BDI_Q.4$BDI_3), 2, ifelse(grepl(4, BDI_Q.4$BDI_3), 3, ""))))
BDI_Q.4$BDI_4<-ifelse(grepl(1, BDI_Q.4$BDI_4), 0, ifelse(grepl(2, BDI_Q.4$BDI_4), 1, ifelse(grepl(3, BDI_Q.4$BDI_4), 2, ifelse(grepl(4, BDI_Q.4$BDI_4), 3, ""))))
BDI_Q.4$BDI_5<-ifelse(grepl(1, BDI_Q.4$BDI_5), 0, ifelse(grepl(2, BDI_Q.4$BDI_5), 1, ifelse(grepl(3, BDI_Q.4$BDI_5), 2, ifelse(grepl(4, BDI_Q.4$BDI_5), 3, ""))))
BDI_Q.4$BDI_6<-ifelse(grepl(1, BDI_Q.4$BDI_6), 0, ifelse(grepl(2, BDI_Q.4$BDI_6), 1, ifelse(grepl(3, BDI_Q.4$BDI_6), 2, ifelse(grepl(4, BDI_Q.4$BDI_6), 3, ""))))
BDI_Q.4$BDI_7<-ifelse(grepl(1, BDI_Q.4$BDI_7), 0, ifelse(grepl(2, BDI_Q.4$BDI_7), 1, ifelse(grepl(3, BDI_Q.4$BDI_7), 2, ifelse(grepl(4, BDI_Q.4$BDI_7), 3, ""))))
BDI_Q.4$BDI_8<-ifelse(grepl(1, BDI_Q.4$BDI_8), 0, ifelse(grepl(2, BDI_Q.4$BDI_8), 1, ifelse(grepl(3, BDI_Q.4$BDI_8), 2, ifelse(grepl(4, BDI_Q.4$BDI_8), 3, ""))))
BDI_Q.4$BDI_9<-ifelse(grepl(1, BDI_Q.4$BDI_9), 0, ifelse(grepl(2, BDI_Q.4$BDI_9), 1, ifelse(grepl(3, BDI_Q.4$BDI_9), 2, ifelse(grepl(4, BDI_Q.4$BDI_9), 3, ""))))
BDI_Q.4$BDI_10<-ifelse(grepl(1, BDI_Q.4$BDI_10), 0, ifelse(grepl(2, BDI_Q.4$BDI_10), 1, ifelse(grepl(3, BDI_Q.4$BDI_10), 2, ifelse(grepl(4, BDI_Q.4$BDI_10), 3, ""))))
BDI_Q.4$BDI_11<-ifelse(grepl(1, BDI_Q.4$BDI_11), 0, ifelse(grepl(2, BDI_Q.4$BDI_11), 1, ifelse(grepl(3, BDI_Q.4$BDI_11), 2, ifelse(grepl(4, BDI_Q.4$BDI_11), 3, ""))))
BDI_Q.4$BDI_12<-ifelse(grepl(1, BDI_Q.4$BDI_12), 0, ifelse(grepl(2, BDI_Q.4$BDI_12), 1, ifelse(grepl(3, BDI_Q.4$BDI_12), 2, ifelse(grepl(4, BDI_Q.4$BDI_12), 3, ""))))
BDI_Q.4$BDI_13<-ifelse(grepl(1, BDI_Q.4$BDI_13), 0, ifelse(grepl(2, BDI_Q.4$BDI_13), 1, ifelse(grepl(3, BDI_Q.4$BDI_13), 2, ifelse(grepl(4, BDI_Q.4$BDI_13), 3, ""))))
BDI_Q.4$BDI_14<-ifelse(grepl(1, BDI_Q.4$BDI_14), 0, ifelse(grepl(2, BDI_Q.4$BDI_14), 1, ifelse(grepl(3, BDI_Q.4$BDI_14), 2, ifelse(grepl(4, BDI_Q.4$BDI_14), 3, ""))))
BDI_Q.4$BDI_15<-ifelse(grepl(1, BDI_Q.4$BDI_15), 0, ifelse(grepl(2, BDI_Q.4$BDI_15), 1, ifelse(grepl(3, BDI_Q.4$BDI_15), 2, ifelse(grepl(4, BDI_Q.4$BDI_15), 3, ""))))
BDI_Q.4$BDI_16<-ifelse(grepl(1, BDI_Q.4$BDI_16), 0, ifelse(grepl(2, BDI_Q.4$BDI_16), 1, ifelse(grepl(3, BDI_Q.4$BDI_16), 2, ifelse(grepl(4, BDI_Q.4$BDI_16), 3, ""))))
BDI_Q.4$BDI_17<-ifelse(grepl(1, BDI_Q.4$BDI_17), 0, ifelse(grepl(2, BDI_Q.4$BDI_17), 1, ifelse(grepl(3, BDI_Q.4$BDI_17), 2, ifelse(grepl(4, BDI_Q.4$BDI_17), 3, ""))))
BDI_Q.4$BDI_18<-ifelse(grepl(1, BDI_Q.4$BDI_18), 0, ifelse(grepl(2, BDI_Q.4$BDI_18), 1, ifelse(grepl(3, BDI_Q.4$BDI_18), 2, ifelse(grepl(4, BDI_Q.4$BDI_18), 3, ""))))
BDI_Q.4$BDI_19<-ifelse(grepl(1, BDI_Q.4$BDI_19), 0, ifelse(grepl(2, BDI_Q.4$BDI_19), 1, ifelse(grepl(3, BDI_Q.4$BDI_19), 2, ifelse(grepl(4, BDI_Q.4$BDI_19), 3, ""))))
BDI_Q.4$BDI_20<-ifelse(grepl(1, BDI_Q.4$BDI_20), 0, ifelse(grepl(2, BDI_Q.4$BDI_20), 1, ifelse(grepl(3, BDI_Q.4$BDI_20), 2, ifelse(grepl(4, BDI_Q.4$BDI_20), 3, ""))))
BDI_Q.4$BDI_21<-ifelse(grepl(1, BDI_Q.4$BDI_21), 0, ifelse(grepl(2, BDI_Q.4$BDI_21), 1, ifelse(grepl(3, BDI_Q.4$BDI_21), 2, ifelse(grepl(4, BDI_Q.4$BDI_21), 3, ""))))
BDI_Q.4$BDI_22<-ifelse(grepl(1, BDI_Q.4$BDI_22), 0, ifelse(grepl(2, BDI_Q.4$BDI_22), 1, ifelse(grepl(3, BDI_Q.4$BDI_22), 2, ifelse(grepl(4, BDI_Q.4$BDI_22), 3, ""))))

#add overlapping PACCT/ELFK subject data 
BDI_PACCT<- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/PAACT_ELFK_Overlapping_Qs/BDI_PACCT_ELFK_SUBJECTS.csv")
#view
head(BDI_PACCT)
names(BDI_PACCT)
nrow(BDI_PACCT)
ncol(BDI_PACCT)

##### reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject_pacct <- BDI_PACCT$IDENT_ID
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
BDI_PACCT$IDENT_ID <- IDENT_SUBID_pacct

#Edit PACCT subject IDs into ELFK subject IDs
BDI_PACCT[1,1] = "EL144P"
BDI_PACCT[2,1] = "EL149P"
#ADD missing column
BDI_PACCT$BDI_DATE_COMPLETE<- NA
BDI_PACCT$BDI_9<- NA
BDI_PACCT$BDI_22<- NA

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names_pacct <- cbind("IDENT_SUBID", "BDI_1","BDI_2","BDI_3","BDI_4","BDI_5","BDI_6","BDI_7","BDI_8",
                         "BDI_10", "BDI_11", "BDI_12", "BDI_13", "BDI_14", "BDI_15", "BDI_16", "BDI_17",
                   "BDI_18", "BDI_19", "BDI_20", "BDI_21","BDI_DATE_COMPLETE","BDI_9", "BDI_22")
# make this to check column length
ncolvars_pacct<-ncol(var_names_pacct)
# now change the variable names of our dataframe to match.
names(BDI_PACCT) <- var_names_pacct

#put columns in right order 
BDI_PACCT <- BDI_PACCT[c("BDI_DATE_COMPLETE","IDENT_SUBID","BDI_1","BDI_2","BDI_3","BDI_4","BDI_5","BDI_6","BDI_7","BDI_8",
                         "BDI_9","BDI_10","BDI_11","BDI_12","BDI_13","BDI_14","BDI_15","BDI_16","BDI_17","BDI_18","BDI_19",
                         "BDI_20","BDI_21","BDI_22")]

# get subjectIDS for each dataframe.
PACCT_list <- BDI_PACCT$IDENT_SUBID
ELFK_list <- BDI_Q.4$IDENT_SUBID

# check that col names are identical before merging!! 
identical(names(BDI_PACCT), names(BDI_Q.4))

# add pacct data into BDI_Q.4
BDI_merged <- rbind(BDI_PACCT,BDI_Q.4)

# make sure all subjects were merged together successfully!! 
identical(nrow(BDI_merged), nrow(BDI_PACCT)+nrow(BDI_Q.4))
BDI_Q.4 <- BDI_merged

#PACCT BDI does not match exactly to the ELFK BDI.
#Tricia $ Bridget decided that it is best to match the BDI questions that overlap.
# What does not overlap will be left blank 

#change Pacct subject answers to right columns for ELFK-BDI version
BDI_Q.4 [1,11] = "0"
BDI_Q.4 [1,12] = "0"
BDI_Q.4 [1,19] = "1"
BDI_Q.4 [2,11] = "0"
BDI_Q.4 [2,12] = "0"
BDI_Q.4 [2,19] = "0"

#change PACCT subject answers to blanks
BDI_Q.4[1,9] = ""
BDI_Q.4[2,9] = ""
BDI_Q.4[1,13] = ""
BDI_Q.4[2,13] = ""
BDI_Q.4[1,16] = ""
BDI_Q.4[2,16] = ""
BDI_Q.4[1,17] = ""
BDI_Q.4[2,17] = ""
BDI_Q.4[1,18] = ""
BDI_Q.4[2,18] = ""
BDI_Q.4[1,18] = ""
BDI_Q.4[2,18] = ""
BDI_Q.4[1,21] = ""
BDI_Q.4[2,21] = ""
BDI_Q.4[1,22] = ""
BDI_Q.4[2,22] = ""
BDI_Q.4[1,24] = ""
BDI_Q.4[2,24] = ""
BDI_Q.5<- BDI_Q.4
names(BDI_Q.5)
setnames(BDI_Q.5, old=c("BDI_DATE_COMPLETE","BDI_1","BDI_2","BDI_3" ,"BDI_4", "BDI_5", "BDI_6","BDI_7","BDI_8","BDI_9","BDI_10","BDI_11","BDI_12","BDI_13","BDI_14" , "BDI_15" , "BDI_16","BDI_17","BDI_18", "BDI_19" , "BDI_20"  , "BDI_21" , "BDI_22"), new=c("PARENT_BDI_DATE_COMPLETE","PARENT_BDI_1","PARENT_BDI_2","PARENT_BDI_3" ,"PARENT_BDI_4", "PARENT_BDI_5", "PARENT_BDI_6",   "PARENT_BDI_7","PARENT_BDI_8","PARENT_BDI_9","PARENT_BDI_10","PARENT_BDI_11","PARENT_BDI_12","PARENT_BDI_13","PARENT_BDI_14" , "PARENT_BDI_15" , "PARENT_BDI_16","PARENT_BDI_17","PARENT_BDI_18", "PARENT_BDI_19" , "PARENT_BDI_20"  , "PARENT_BDI_21" , "PARENT_BDI_22"))
names(BDI_Q.5)
# write to csv file on lux
write.csv(BDI_Q.5, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_BDI_Questionnaire.csv", row.names = FALSE)
