# last extraction on 7/18/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# SCARED FOR CHILDHOOD ANXIETY DISORDERS QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
setwd("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/")
SCARED_Q <- read.csv("QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Scared_for_childhood_anxiety_related_disorders_Questionnaire.csv")
#view
head(SCARED_Q)
names(SCARED_Q)
nrow(SCARED_Q)
ncol(SCARED_Q)

# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(SCARED_Q) #this step is to update number of rows based on new data#
SCARED_Q.1<-SCARED_Q[9:row_num,]
#to delete additional rows (with fake data) in the data
SCARED_Q.2<- SCARED_Q.1[-c(64, 35, 77, 22,100), ] 

# get rid of extra columns with random qualtrics info
SCARED_Q.2<- SCARED_Q.2[,8:59]
SCARED_Q.3<- SCARED_Q.2[, -c(2,3,4,5,6,7,8,9,10)]

#view
nrow(SCARED_Q.3)
ncol(SCARED_Q.3)

#change el06l entry to el061 (letter l to 1) and EL016_2 to EL016 (avoiding split when editing in next steps)
SCARED_Q.3$Q3 <-as.character(SCARED_Q.3$Q3)
SCARED_Q.3[48,2] <- "EL061"
SCARED_Q.3[34,2] <- "EL016"

#remove double entry - decided with Tricia
SCARED_Q.3<- SCARED_Q.3[-c(113),]

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- SCARED_Q.3$Q3
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
SCARED_Q.3$Q3 <- IDENT_SUBID

# start relabeling columns 
names(SCARED_Q.3)
ncol(SCARED_Q.3)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("SCARED_DATE_COMPLETE","IDENT_SUBID", "SCARED_1", "SCARED_2", "SCARED_3","SCARED_4","SCARED_5","SCARED_6","SCARED_7","SCARED_8",
                   "SCARED_9","SCARED_10","SCARED_11","SCARED_12","SCARED_13","SCARED_14","SCARED_15","SCARED_16","SCARED_17",
                   "SCARED_18", "SCARED_19","SCARED_20","SCARED_21","SCARED_22","SCARED_23","SCARED_24","SCARED_25","SCARED_26",
                   "SCARED_27","SCARED_28","SCARED_29","SCARED_30","SCARED_31","SCARED_32","SCARED_33","SCARED_34","SCARED_35",
                   "SCARED_36","SCARED_37", "SCARED_38", "SCARED_39", "SCARED_40", "SCARED_41")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(SCARED_Q.3) <- var_names

#Convert data to Data Manual Codes. 
SCARED_Q.3$SCARED_1<- ifelse(grepl(1, SCARED_Q.3$SCARED_1), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_1), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_1), 2, "")))
SCARED_Q.3$SCARED_2<- ifelse(grepl(1, SCARED_Q.3$SCARED_2), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_2), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_2), 2, "")))
SCARED_Q.3$SCARED_3<- ifelse(grepl(1, SCARED_Q.3$SCARED_3), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_3), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_3), 2, "")))
SCARED_Q.3$SCARED_4<- ifelse(grepl(1, SCARED_Q.3$SCARED_4), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_4), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_4), 2, "")))
SCARED_Q.3$SCARED_5<- ifelse(grepl(1, SCARED_Q.3$SCARED_5), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_5), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_5), 2, "")))
SCARED_Q.3$SCARED_6<- ifelse(grepl(1, SCARED_Q.3$SCARED_6), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_6), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_6), 2, "")))
SCARED_Q.3$SCARED_7<- ifelse(grepl(1, SCARED_Q.3$SCARED_7), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_7), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_7), 2, "")))
SCARED_Q.3$SCARED_8<- ifelse(grepl(1, SCARED_Q.3$SCARED_8), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_8), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_8), 2, "")))
SCARED_Q.3$SCARED_9<- ifelse(grepl(1, SCARED_Q.3$SCARED_9), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_9), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_9), 2, "")))
SCARED_Q.3$SCARED_10<- ifelse(grepl(1, SCARED_Q.3$SCARED_10), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_10), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_10), 2, "")))
SCARED_Q.3$SCARED_11<- ifelse(grepl(1, SCARED_Q.3$SCARED_11), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_11), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_11), 2, "")))
SCARED_Q.3$SCARED_12<- ifelse(grepl(1, SCARED_Q.3$SCARED_12), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_12), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_12), 2, "")))
SCARED_Q.3$SCARED_13<- ifelse(grepl(1, SCARED_Q.3$SCARED_13), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_13), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_13), 2, "")))
SCARED_Q.3$SCARED_14<- ifelse(grepl(1, SCARED_Q.3$SCARED_14), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_14), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_14), 2, "")))
SCARED_Q.3$SCARED_15<- ifelse(grepl(1, SCARED_Q.3$SCARED_15), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_15), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_15), 2, "")))
SCARED_Q.3$SCARED_16<- ifelse(grepl(1, SCARED_Q.3$SCARED_16), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_16), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_16), 2, "")))
SCARED_Q.3$SCARED_17<- ifelse(grepl(1, SCARED_Q.3$SCARED_17), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_17), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_17), 2, "")))
SCARED_Q.3$SCARED_18<- ifelse(grepl(1, SCARED_Q.3$SCARED_18), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_18), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_18), 2, "")))
SCARED_Q.3$SCARED_19<- ifelse(grepl(1, SCARED_Q.3$SCARED_19), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_19), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_19), 2, "")))
SCARED_Q.3$SCARED_20<- ifelse(grepl(1, SCARED_Q.3$SCARED_20), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_20), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_20), 2, "")))
SCARED_Q.3$SCARED_21<- ifelse(grepl(1, SCARED_Q.3$SCARED_21), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_21), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_21), 2, "")))
SCARED_Q.3$SCARED_22<- ifelse(grepl(1, SCARED_Q.3$SCARED_22), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_22), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_22), 2, "")))
SCARED_Q.3$SCARED_23<- ifelse(grepl(1, SCARED_Q.3$SCARED_23), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_23), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_23), 2, "")))
SCARED_Q.3$SCARED_24<- ifelse(grepl(1, SCARED_Q.3$SCARED_24), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_24), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_24), 2, "")))
SCARED_Q.3$SCARED_25<- ifelse(grepl(1, SCARED_Q.3$SCARED_25), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_25), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_25), 2, "")))
SCARED_Q.3$SCARED_26<- ifelse(grepl(1, SCARED_Q.3$SCARED_26), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_26), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_26), 2, "")))
SCARED_Q.3$SCARED_27<- ifelse(grepl(1, SCARED_Q.3$SCARED_27), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_27), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_27), 2, "")))
SCARED_Q.3$SCARED_28<- ifelse(grepl(1, SCARED_Q.3$SCARED_28), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_28), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_28), 2, "")))
SCARED_Q.3$SCARED_29<- ifelse(grepl(1, SCARED_Q.3$SCARED_29), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_29), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_29), 2, "")))
SCARED_Q.3$SCARED_30<- ifelse(grepl(1, SCARED_Q.3$SCARED_30), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_30), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_30), 2, "")))
SCARED_Q.3$SCARED_31<- ifelse(grepl(1, SCARED_Q.3$SCARED_31), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_31), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_31), 2, "")))
SCARED_Q.3$SCARED_32<- ifelse(grepl(1, SCARED_Q.3$SCARED_32), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_32), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_32), 2, "")))
SCARED_Q.3$SCARED_33<- ifelse(grepl(1, SCARED_Q.3$SCARED_33), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_33), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_33), 2, "")))
SCARED_Q.3$SCARED_34<- ifelse(grepl(1, SCARED_Q.3$SCARED_34), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_34), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_34), 2, "")))
SCARED_Q.3$SCARED_35<- ifelse(grepl(1, SCARED_Q.3$SCARED_35), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_35), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_35), 2, "")))
SCARED_Q.3$SCARED_36<- ifelse(grepl(1, SCARED_Q.3$SCARED_36), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_36), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_36), 2, "")))
SCARED_Q.3$SCARED_37<- ifelse(grepl(1, SCARED_Q.3$SCARED_37), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_37), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_37), 2, "")))
SCARED_Q.3$SCARED_38<- ifelse(grepl(1, SCARED_Q.3$SCARED_38), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_38), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_38), 2, "")))
SCARED_Q.3$SCARED_39<- ifelse(grepl(1, SCARED_Q.3$SCARED_39), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_39), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_39), 2, "")))
SCARED_Q.3$SCARED_40<- ifelse(grepl(1, SCARED_Q.3$SCARED_40), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_40), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_40), 2, "")))
SCARED_Q.3$SCARED_41<- ifelse(grepl(1, SCARED_Q.3$SCARED_41), 0, ifelse(grepl(2, SCARED_Q.3$SCARED_41), 1, ifelse(grepl(3, SCARED_Q.3$SCARED_41), 2, "")))

# write to csv file on elvis
write.csv(SCARED_Q.3, "QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_SCARED_Questionnaire.csv", row.names = FALSE)

