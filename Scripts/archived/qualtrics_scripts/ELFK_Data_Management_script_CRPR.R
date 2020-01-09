# last time data extracted - 7/18/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# CHILD REARING PRACTICES REPORT QUESTIONNAIRE 
################################################################################################


# load in qualtrics data
setwd("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/")
CRPR_Q <- read.csv("QUALTRICS_RAW_FOR_R/Qualtrics_RAW_CRPR_Questionnaire.csv")
#view
head(CRPR_Q)
names(CRPR_Q)
nrow(CRPR_Q)
ncol(CRPR_Q)

# get rid of extra columns with random qualtrics info
CRPR_Q<- df[,8:59]
CRPR_Q.1<- CRPR_Q[,-c(2,3,4,5,6,7,8,9,10)]

#to delete additional date column
CRPR_Q.2 <- CRPR_Q.1[ , -c(3)]

# get rid of extra rows with extra labels
# [ rows, columns]
#remove extra rows
row_num <-nrow(CRPR_Q.2) #this step is to update number of rows based on new data#
CRPR_Q.3<-CRPR_Q.2[9:row_num,]

#Change label El06IP so that it reads as EL061P 
CRPR_Q.3$Q1_1 <- as.character(CRPR_Q.3$Q1_1)
CRPR_Q.3[53,2] <- "EL061"
#Make EL086P – first entry  – EL087. Leave second entry as EL086.
CRPR_Q.3[72,2] <- "EL087"

#to delete additional rows (with fake data) in the data
CRPR_Q.4 <- CRPR_Q.3[-c(4,14,26,35,68,83,105),]

#view
nrow(CRPR_Q.4)
ncol(CRPR_Q.4)

#split data for family who two siblings in same row
CRPR_Q.4[124,2] = "EL105"
CRPR_Q.4[117,2] = "EL104"
#COPY 104 DATA INTO 105
CRPR_Q.4[124,3:42] = CRPR_Q.4[117,3:42]

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- CRPR_Q.4$Q1_1
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
CRPR_Q.4$Q1_1 <- IDENT_SUBID

# start relabeling columns 
names(CRPR_Q.4)
ncol(CRPR_Q.4)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("CRPR_DATE_COMPLETE","IDENT_SUBID", "CRPR_1", "CRPR_2", "CRPR_3", "CRPR_4", "CRPR_5", "CRPR_6", "CRPR_7", "CRPR_8",
                   "CRPR_9", "CRPR_10", "CRPR_11", "CRPR_12", "CRPR_13", "CRPR_14", "CRPR_15", "CRPR_16", "CRPR_17",
                   "CRPR_18", "CRPR_19", "CRPR_20", "CRPR_21", "CRPR_22", "CRPR_23", "CRPR_24", "CRPR_25", "CRPR_26",
                   "CRPR_27", "CRPR_28", "CRPR_29", "CRPR_30", "CRPR_31", "CRPR_32", "CRPR_33", "CRPR_34", "CRPR_35",
                   "CRPR_36", "CRPR_37", "CRPR_38", "CRPR_39", "CRPR_40")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(CRPR_Q.4) <- var_names

#change data to the right format
CRPR_Q.4$CRPR_1 <- as.numeric(as.character(CRPR_Q.4$CRPR_1))
CRPR_Q.4$CRPR_2 <- as.numeric(as.character(CRPR_Q.4$CRPR_2))
CRPR_Q.4$CRPR_3 <- as.numeric(as.character(CRPR_Q.4$CRPR_3))
CRPR_Q.4$CRPR_4 <- as.numeric(as.character(CRPR_Q.4$CRPR_4))
CRPR_Q.4$CRPR_5 <- as.numeric(as.character(CRPR_Q.4$CRPR_5))
CRPR_Q.4$CRPR_6 <- as.numeric(as.character(CRPR_Q.4$CRPR_6))
CRPR_Q.4$CRPR_7 <- as.numeric(as.character(CRPR_Q.4$CRPR_7))
CRPR_Q.4$CRPR_8 <- as.numeric(as.character(CRPR_Q.4$CRPR_8))
CRPR_Q.4$CRPR_9 <- as.numeric(as.character(CRPR_Q.4$CRPR_9))
CRPR_Q.4$CRPR_10 <- as.numeric(as.character(CRPR_Q.4$CRPR_10))
CRPR_Q.4$CRPR_11 <- as.numeric(as.character(CRPR_Q.4$CRPR_11))
CRPR_Q.4$CRPR_12 <- as.numeric(as.character(CRPR_Q.4$CRPR_12))
CRPR_Q.4$CRPR_13 <- as.numeric(as.character(CRPR_Q.4$CRPR_13))
CRPR_Q.4$CRPR_14 <- as.numeric(as.character(CRPR_Q.4$CRPR_14))
CRPR_Q.4$CRPR_15 <- as.numeric(as.character(CRPR_Q.4$CRPR_15))
CRPR_Q.4$CRPR_16 <- as.numeric(as.character(CRPR_Q.4$CRPR_16))
CRPR_Q.4$CRPR_17 <- as.numeric(as.character(CRPR_Q.4$CRPR_17))
CRPR_Q.4$CRPR_18 <- as.numeric(as.character(CRPR_Q.4$CRPR_18))
CRPR_Q.4$CRPR_19 <- as.numeric(as.character(CRPR_Q.4$CRPR_19))
CRPR_Q.4$CRPR_20 <- as.numeric(as.character(CRPR_Q.4$CRPR_20))
CRPR_Q.4$CRPR_21 <- as.numeric(as.character(CRPR_Q.4$CRPR_21))
CRPR_Q.4$CRPR_22 <- as.numeric(as.character(CRPR_Q.4$CRPR_22))
CRPR_Q.4$CRPR_23 <- as.numeric(as.character(CRPR_Q.4$CRPR_23))
CRPR_Q.4$CRPR_24 <- as.numeric(as.character(CRPR_Q.4$CRPR_24))
CRPR_Q.4$CRPR_25 <- as.numeric(as.character(CRPR_Q.4$CRPR_25))
CRPR_Q.4$CRPR_26 <- as.numeric(as.character(CRPR_Q.4$CRPR_26))
CRPR_Q.4$CRPR_27 <- as.numeric(as.character(CRPR_Q.4$CRPR_27))
CRPR_Q.4$CRPR_28 <- as.numeric(as.character(CRPR_Q.4$CRPR_28))
CRPR_Q.4$CRPR_29<- as.numeric(as.character(CRPR_Q.4$CRPR_29))
CRPR_Q.4$CRPR_30<- as.numeric(as.character(CRPR_Q.4$CRPR_30))
CRPR_Q.4$CRPR_31<- as.numeric(as.character(CRPR_Q.4$CRPR_31))
CRPR_Q.4$CRPR_32<- as.numeric(as.character(CRPR_Q.4$CRPR_32))
CRPR_Q.4$CRPR_33<- as.numeric(as.character(CRPR_Q.4$CRPR_33))
CRPR_Q.4$CRPR_34<- as.numeric(as.character(CRPR_Q.4$CRPR_34))
CRPR_Q.4$CRPR_35<- as.numeric(as.character(CRPR_Q.4$CRPR_35))
CRPR_Q.4$CRPR_36<- as.numeric(as.character(CRPR_Q.4$CRPR_36))
CRPR_Q.4$CRPR_37<- as.numeric(as.character(CRPR_Q.4$CRPR_37))
CRPR_Q.4$CRPR_38<- as.numeric(as.character(CRPR_Q.4$CRPR_38))
CRPR_Q.4$CRPR_39<- as.numeric(as.character(CRPR_Q.4$CRPR_39))
CRPR_Q.4$CRPR_40<- as.numeric(as.character(CRPR_Q.4$CRPR_40))


#no coding necessary

# write to csv file on lux
write.csv(CRPR_Q.4, "QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_CRPR_Questionnaire.csv", row.names = FALSE)
