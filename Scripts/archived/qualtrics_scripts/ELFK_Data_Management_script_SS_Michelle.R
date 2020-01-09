# last extraction on 7/11/2018 - merged with SS from PACCT participants
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# SECURITY SCALE QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
SecurityScale_Q <- read.csv("QUALTRICS_RAW_FOR_R/Qualtrics_Raw_SecurityScale_Questionnaire.csv")

#view
head(SecurityScale_Q)
names(SecurityScale_Q)
nrow(SecurityScale_Q)
ncol(SecurityScale_Q)

# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(SecurityScale_Q) #this step is to update number of rows based on new data#
SecurityScale_Q.1<-SecurityScale_Q[7:row_num,]
SecurityScale_Q.2<- SecurityScale_Q.1[-c(105, 110,109), ] #to delete additional row (with fake data) in the data
# get rid of extra columns with random qualtrics info
SecurityScale_Q.2 <- SecurityScale_Q.2 [,8:48]
SecurityScale_Q.3 <- SecurityScale_Q.2[,-c(2,3,4,5,6,7,8,9)]
SecurityScale_Q.4<- SecurityScale_Q.3[, -c(3) ] #to delete additional column (with fake data) in the data
#view
nrow(SecurityScale_Q.4)
ncol(SecurityScale_Q.4)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- SecurityScale_Q.4$Q1_1
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
SecurityScale_Q.4$Q1_1 <- IDENT_SUBID

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names1 <- cbind("SS_DATE_COMPLETE", "IDENT_SUBID", "SS_NEW_1A", "SS_NEW_1B","SS_NEW_2A","SS_NEW_2B","SS_NEW_3A","SS_NEW_3B","SS_NEW_4A","SS_NEW_4B","SS_NEW_5A","SS_NEW_5B","SS_NEW_6A","SS_NEW_6B","SS_NEW_7A","SS_NEW_7B","SS_NEW_8A","SS_NEW_8B","SS_NEW_9A","SS_NEW_9B","SS_NEW_10A","SS_NEW_10B","SS_NEW_11A","SS_NEW_11B","SS_NEW_12A","SS_NEW_12B","SS_NEW_13A","SS_NEW_13B","SS_NEW_14A","SS_NEW_14B","SS_NEW_15A", "SS_NEW_15B")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(SecurityScale_Q.4) <- var_names

#add overlapping PACCT/ELFK subject data
Pacct_SecurityScale<- read.csv("QUALTRICS_RAW_FOR_R/PAACT_ELFK_Overlapping_Qs/Qualtrics_Raw_SecurityScale_Questionnaire_PACCT_ELFK.csv")
#view
head(Pacct_SecurityScale)
names(Pacct_SecurityScale)
nrow(Pacct_SecurityScale)
ncol(Pacct_SecurityScale)
# get rid of extra rows with extra labels
# [ rows, columns]
row_num2 <-nrow(Pacct_SecurityScale) #this step is to update number of rows based on new data#
Pacct_SecurityScale<-Pacct_SecurityScale[3:row_num2,]
# get rid of extra columns with random qualtrics info
Pacct_SecurityScale_Q.1 <- Pacct_SecurityScale [,8:48]
Pacct_SecurityScale_Q.2 <- Pacct_SecurityScale_Q.1[,-c(2,3,4,5,6,7,8,9,10)]
#view
nrow(Pacct_SecurityScale_Q.2)
ncol(Pacct_SecurityScale_Q.2)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject2 <- Pacct_SecurityScale_Q.2$Q33
# removing characters /letters and x is only numbers
x2 <- str_extract_all(subject2,"\\(?[0-9,.]+\\)?") # [[1]]
subject_num2 <- unlist(x2)
first_num2 <- substr(subject_num2, 1,1)

# removing zero if first number
subject_num_fixed2 <- as.numeric(ifelse(first_num2 == "0", substring(subject_num2, 2), subject_num2))

# adding EL and zeros as needed
IDENT_SUBID2 <- ifelse(subject_num_fixed2 < 10, paste0("EL00", subject_num_fixed2), ifelse(subject_num_fixed2 <100, 
                                                                                        paste0("EL0", subject_num_fixed2), 
                                                                                        ifelse(subject_num_fixed2 >= 100, paste0("EL", subject_num_fixed2), NA)))
# replace subject with your new subjectid 
Pacct_SecurityScale_Q.2$Q33 <- IDENT_SUBID2

#Edit PACCT subject IDs into ELFK subject IDs
Pacct_SecurityScale_Q.2[1,2] = "EL144"
Pacct_SecurityScale_Q.2[2,2] = "EL145"
Pacct_SecurityScale_Q.2[3,2] = "EL146"
Pacct_SecurityScale_Q.2[4,2] = "EL147"
Pacct_SecurityScale_Q.2[5,2] = "EL148"
Pacct_SecurityScale_Q.2[6,2] = "EL149"

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names2 <- cbind("SS_DATE_COMPLETE", "IDENT_SUBID", "SS_NEW_1A", "SS_NEW_1B","SS_NEW_2A","SS_NEW_2B","SS_NEW_3A","SS_NEW_3B","SS_NEW_4A","SS_NEW_4B","SS_NEW_5A","SS_NEW_5B","SS_NEW_6A","SS_NEW_6B","SS_NEW_7A","SS_NEW_7B","SS_NEW_8A","SS_NEW_8B","SS_NEW_9A","SS_NEW_9B","SS_NEW_10A","SS_NEW_10B","SS_NEW_11A","SS_NEW_11B","SS_NEW_12A","SS_NEW_12B","SS_NEW_13A","SS_NEW_13B","SS_NEW_14A","SS_NEW_14B","SS_NEW_15A", "SS_NEW_15B")

# make this to check column length
ncolvars2<-ncol(var_names2)
# now change the variable names of our dataframe to match.
names(Pacct_SecurityScale_Q.2) <- var_names2

# get subjectIDS for each dataframe.
PACCT_list <- Pacct_SecurityScale_Q.2$IDENT_SUBID
ELFK_list <- SecurityScale_Q.4$IDENT_SUBID

# check that col names are identical before merging!! 
identical(names(SecurityScale_Q.4), names(Pacct_SecurityScale_Q.2))

# add pacct data into SecurityScale_Q.4
SecurityScale_merged <- rbind(SecurityScale_Q.4,Pacct_SecurityScale_Q.2)

# make sure all subjects were merged together successfully!! 
identical(nrow(SecurityScale_merged), nrow(SecurityScale_Q.4)+nrow(Pacct_SecurityScale_Q.2))
SecurityScale_Q.5 <- SecurityScale_merged

#Convert data to Data Manual Codes. 
SecurityScale_Q.5$SS_NEW_1A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_1A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_1A), 1, ""))
SecurityScale_Q.5$SS_NEW_2A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_2A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_2A), 1, ""))
SecurityScale_Q.5$SS_NEW_3A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_3A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_3A), 1, ""))
SecurityScale_Q.5$SS_NEW_4A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_4A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_4A), 1, ""))
SecurityScale_Q.5$SS_NEW_5A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_5A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_5A), 1, ""))
SecurityScale_Q.5$SS_NEW_6A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_6A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_6A), 1, ""))
SecurityScale_Q.5$SS_NEW_7A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_7A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_7A), 1, ""))
SecurityScale_Q.5$SS_NEW_8A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_8A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_8A), 1, ""))
SecurityScale_Q.5$SS_NEW_9A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_9A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_9A), 1, ""))
SecurityScale_Q.5$SS_NEW_10A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_10A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_10A), 1, ""))
SecurityScale_Q.5$SS_NEW_11A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_11A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_11A), 1, ""))
SecurityScale_Q.5$SS_NEW_12A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_12A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_12A), 1, ""))
SecurityScale_Q.5$SS_NEW_13A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_13A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_13A), 1, ""))
SecurityScale_Q.5$SS_NEW_14A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_14A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_14A), 1, ""))
SecurityScale_Q.5$SS_NEW_15A<- ifelse(grepl(1, SecurityScale_Q.5$SS_NEW_15A), 0, ifelse(grepl(2, SecurityScale_Q.5$SS_NEW_15A), 1, ""))


SecurityScale_Q.5$SS_NEW_1A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_1A))
SecurityScale_Q.5$SS_NEW_2A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_2A))
SecurityScale_Q.5$SS_NEW_3A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_3A))
SecurityScale_Q.5$SS_NEW_4A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_4A))
SecurityScale_Q.5$SS_NEW_5A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_5A))
SecurityScale_Q.5$SS_NEW_6A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_6A))
SecurityScale_Q.5$SS_NEW_7A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_7A))
SecurityScale_Q.5$SS_NEW_8A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_8A))
SecurityScale_Q.5$SS_NEW_9A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_9A))
SecurityScale_Q.5$SS_NEW_10A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_10A))
SecurityScale_Q.5$SS_NEW_11A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_11A))
SecurityScale_Q.5$SS_NEW_12A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_12A))
SecurityScale_Q.5$SS_NEW_13A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_13A))
SecurityScale_Q.5$SS_NEW_14A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_14A))
SecurityScale_Q.5$SS_NEW_15A <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_15A))
SecurityScale_Q.5$SS_NEW_1B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_1B))
SecurityScale_Q.5$SS_NEW_2B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_2B))
SecurityScale_Q.5$SS_NEW_3B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_3B))
SecurityScale_Q.5$SS_NEW_4B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_4B))
SecurityScale_Q.5$SS_NEW_5B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_5B))
SecurityScale_Q.5$SS_NEW_6B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_6B))
SecurityScale_Q.5$SS_NEW_7B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_7B))
SecurityScale_Q.5$SS_NEW_8B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_8B))
SecurityScale_Q.5$SS_NEW_9B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_9B))
SecurityScale_Q.5$SS_NEW_10B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_10B))
SecurityScale_Q.5$SS_NEW_11B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_11B))
SecurityScale_Q.5$SS_NEW_12B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_12B))
SecurityScale_Q.5$SS_NEW_13B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_13B))
SecurityScale_Q.5$SS_NEW_14B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_14B))
SecurityScale_Q.5$SS_NEW_15B <- as.numeric(as.character(SecurityScale_Q.5$SS_NEW_15B))


# write to csv file on elvis
write.csv(SecurityScale_Q.5, "QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_SecurityScale_Questionnaire.csv", row.names = FALSE)

