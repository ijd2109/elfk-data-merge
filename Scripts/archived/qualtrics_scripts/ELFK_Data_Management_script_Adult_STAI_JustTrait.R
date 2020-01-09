# Made by Sean Minns
# Dec 2018 

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# STAT QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
P_STAI_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Adult_Stai_Questionnaire_TRAIT.csv", stringsAsFactors=FALSE)
head(P_STAI_Q)[1:2]

## FIX EL081 ID because it is EL08L and then got coded as EL008 incorrectly.
P_STAI_Q$Q1[31] <- "EL081"

#remove extra rows
P_STAI_Q.1<-P_STAI_Q[-c(1:2),]

# get rid of extra columns with random qualtrics info, gender, age and the extra date  
P_STAI_Q.3 <- P_STAI_Q.1[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21)]


  
# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
P_STAI_Q.4 <- P_STAI_Q.3

library(stringr)
subject <- P_STAI_Q.4$Q1
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
# add parent to name
IDENT_SUBID2 <- paste(IDENT_SUBID,"P", sep="")

# replace subject with your new subjectid 
P_STAI_Q.4$Q1 <- IDENT_SUBID2

# start relabeling columns 
# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("PARENT_STAI_TRAIT_DATE_COMPLETE", "IDENT_SUBID", "PARENT_STAI_TRAIT_1", "PARENT_STAI_TRAIT_2", "PARENT_STAI_TRAIT_3", "PARENT_STAI_TRAIT_4", 
                   "PARENT_STAI_TRAIT_5", "PARENT_STAI_TRAIT_6", "PARENT_STAI_TRAIT_7", "PARENT_STAI_TRAIT_8", "PARENT_STAI_TRAIT_9", "PARENT_STAI_TRAIT_10", "PARENT_STAI_TRAIT_11", "PARENT_STAI_TRAIT_12", "PARENT_STAI_TRAIT_13", "PARENT_STAI_TRAIT_14", "PARENT_STAI_TRAIT_15", "PARENT_STAI_TRAIT_16", "PARENT_STAI_TRAIT_17", "PARENT_STAI_TRAIT_18", "PARENT_STAI_TRAIT_19","PARENT_STAI_TRAIT_20")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(P_STAI_Q.4) <- var_names

#Convert data to Data Manual Codes. 
# Only need to reverse-code STAI_1 STAI_3 STAI_6 STAI_7 STAI_10 STAI_13 STAI_14 STAI_16 STAI_19

P_STAI_Q.4$PARENT_STAI_TRAIT_1R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_1), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_1), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_1), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_1), 1, "")))))
P_STAI_Q.4$PARENT_STAI_TRAIT_3R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_3), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_3), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_3), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_3), 1, "")))))
P_STAI_Q.4$PARENT_STAI_TRAIT_6R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_6), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_6), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_6), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_6), 1,  "")))))
P_STAI_Q.4$PARENT_STAI_TRAIT_7R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_7), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_7), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_7), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_7), 1, "")))))
P_STAI_Q.4$PARENT_STAI_TRAIT_10R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_10), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_10), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_10), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_10), 1, "")))))
P_STAI_Q.4$PARENT_STAI_TRAIT_13R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_13), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_13), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_13), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_13), 1, "")))))
P_STAI_Q.4$PARENT_STAI_TRAIT_14R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_14), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_14), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_14), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_14), 1, "")))))
P_STAI_Q.4$PARENT_STAI_TRAIT_16R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_16), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_16), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_16), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_16), 1,  "")))))
P_STAI_Q.4$PARENT_STAI_TRAIT_19R<- as.numeric(ifelse(grepl(1, P_STAI_Q.4$PARENT_STAI_TRAIT_19), 4, ifelse(grepl(2, P_STAI_Q.4$PARENT_STAI_TRAIT_19), 3, ifelse(grepl(3, P_STAI_Q.4$PARENT_STAI_TRAIT_19), 2, ifelse(grepl(4, P_STAI_Q.4$PARENT_STAI_TRAIT_19), 1, "")))))

# CHECK FOR DUPLICATES
nrow(P_STAI_Q.4) 
length(unique(P_STAI_Q.4$IDENT_SUBID)) # 7 subjects with duplicates


library(tidyverse)
# find duplicate subject list
sub_fix <- P_STAI_Q.4 %>%
  group_by(IDENT_SUBID) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>% select(IDENT_SUBID)
sub_fix$IDENT_SUBID

# now get their dates and find earliest dated survey
earliest_STAI_data <- P_STAI_Q.4 %>% 
 # filter(IDENT_SUBID %in% sub_fix$IDENT_SUBID) %>% 
  group_by(IDENT_SUBID) %>%
  # take the first date they filled this out!
  summarize(earliest_date = sort(PARENT_STAI_TRAIT_DATE_COMPLETE)[1])

# merge this new thing into existing data
P_STAI_Q.5 <- merge(P_STAI_Q.4, earliest_STAI_data, by = "IDENT_SUBID")

# filter existing data to only include first date completed by each subject
P_STAI_Q.6 <- P_STAI_Q.5 %>%
  filter(PARENT_STAI_TRAIT_DATE_COMPLETE == earliest_date)

nrow(P_STAI_Q.6)

# write to csv file on elvis
write.csv(P_STAI_Q.6, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_PARENT_STAI_TRAIT_Questionnaire.csv" , row.names = F, quote = F)


