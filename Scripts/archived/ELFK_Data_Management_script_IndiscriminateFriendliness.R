#last extraction on 6/28/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# Indiscriminate Friendliness QUESTIONNAIRE
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
IF_Q <- read.csv("QUALTRICS_RAW_FOR_R/Qualtrics_RAW_Indiscriminate_Friendliness_Questionnaire.csv")
#view
head(IF_Q)
names(IF_Q)
nrow(IF_Q)
ncol(IF_Q)

# get rid of extra columns with random qualtrics info
# [ rows, columns]
IF_Q<- IF_Q[,8:23]
IF_Q.1 <- IF_Q[,-c(2,3,4,5,6,7,8,9)]
# get rid of extra rows with extra labels
row_num <-nrow(IF_Q.1) #this step is to update number of rows based on new data#
IF_Q.2 <- IF_Q.1[10:row_num,]
#Edit EL0883 to EL083. It was typo.
IF_Q.2$Q1<-as.character(IF_Q.2$Q1)
IF_Q.2[33,2] = "EL083"
#Remove double entries, one that will be repeated later on when paper data is added to script. 
IF_Q.3 <- IF_Q.2[-c(6,47,21,6,24,25,26,23,22,17,88,89),]

#view
nrow(IF_Q.3)
ncol(IF_Q.3)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- IF_Q.3$Q1
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
IF_Q.3$Q1 <- IDENT_SUBID

# start relabeling columns 
names(IF_Q.3)
ncol(IF_Q.3)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("IF_DATE_COMPLETE","IDENT_SUBID", "IF_1", "IF_2", "IF_3", "IF_4", "IF_5", "IF_6")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(IF_Q.3) <- var_names

#add data from paper forms
#df[nrow(df) + 1,] = list("v1","v2")
IF_Q.3$IF_5 <- as.character(IF_Q.3$IF_5)
IF_Q.3$IF_6 <- as.character(IF_Q.3$IF_6)

IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL060", 1,1,5,8,4,5)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL056", 1,5,5,5, 5,5)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL053", 9,1,10,1,10,8)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL052", 3,6,7,3,7,6)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL051", 1,2,5,6,5,5)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL050", 3,4,7,3,6,5)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL049", 2,6,7,7,1,9) 
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL048", 2,7,5,5,3,6)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL047", 2,8,4,8,1,4)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL046", 1,3,4,7,1,5)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL045", 1,1,3,8,1,3)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL044", 1,1,3,8,2,2)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL043", 1,10,7,4,1,1)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL042", 1,1,5,5,1,2)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL041", 2,2,5,4,4,4)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL040", 1,1,1,9,2,2)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL039", 7,5,9,1,9,8) 
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL037", 1,10,5,2,8,8)  
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL036", 1,10,5,3,5,7)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL035", 1,2,5,4,4,8)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL034", 2,6,6,4,4,8)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL025", 1,1,4,5,4,5)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL024", 2,2,7,2,4,6)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL023", 1,1,3,7,4,3)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL005", 1,5,1,9,1,1)
IF_Q.3[nrow(IF_Q.3) +1, ] = list('',"EL004", 1,5,5,3,3,1)

# write to csv file on lux
write.csv(IF_Q.3, "QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_IF_Questionnaire.csv", row.names = FALSE)
