#last qualtrics extraction on 7/11/2018 - merged with SS from PACCT participants
# Made by Michelle Leon
# edited by MVT on April 16, 2018 to merge child-reported data using the script.
#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# Peterson Development Scale: Female Questionnaire
################################################################################################

# load in qualtrics data

Peterson_female_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Peterson_Female_Questionnaire_5.23.2018.csv")
#view
head(Peterson_female_Q)
names(Peterson_female_Q)
nrow(Peterson_female_Q)
ncol(Peterson_female_Q)

# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(Peterson_female_Q) #this step is to update number of rows based on new data#
Peterson_female_Q.1 <-Peterson_female_Q[3:row_num,]
#to delete additional rows (with fake data and duplicates) in the data
## don't do this by row number, because that might change pending addition of new data
# exclude based on subID & time_stamp. 
#Peterson_female_Q.2<- Peterson_female_Q.1[-c(33,64,55,53,25,22,23,
#16,14, 19, 29,78), ] 12 total should be excluded! 

Peterson_female_exclude <- with(Peterson_female_Q.1, ifelse(
                              # these are junk tests
                              Q1_1 == "TEST" | Q1_1 == "fgs" |
# these are parent-reported, keeping child-reported                                
Q1_1 == "EL102P " | (Q1_1 == "EL099" & Q1_3 == "4/29/63") | (Q1_1 == "EL016" & Q1_3 == "1/7/05") |
  (Q1_1 == "EL036" & Q1_2 == "1/2/15")  |  (Q1_1 == "EL037") | Q1_1 =="el038" | Q1_1 =="el041" |
(Q1_1 == "EL045" & ResponseId == "R_1mIA040pVABB2Gq")|
(Q1_1 == "EL057" & ResponseId == "R_2YbUZVcQigu5Swo") | Q1_1 =="el140", 1, 0)) 


sum(Peterson_female_exclude) # should be 12. 

# now only include those who arre not marked for exclusion!! 
Peterson_female_Q.2 <- subset(Peterson_female_Q.1, Peterson_female_exclude == 0)
# cecking that 12 rows were successfully removed! 
nrow(Peterson_female_Q.2) - nrow(Peterson_female_Q.1)

# get rid of extra columns with random qualtrics info
Peterson_female_Q.2<- Peterson_female_Q.2[,8:30]
Peterson_female_Q.3 <- Peterson_female_Q.2[,-c(2,3,4,5,6,7,8,9)]
#to delete additional columns (with fake data) in the data
Peterson_female_Q.4<- Peterson_female_Q.3[,-c(3) ] 
Peterson_female_Q.5<- Peterson_female_Q.4[,-c(3) ]

#view
nrow(Peterson_female_Q.5)
ncol(Peterson_female_Q.5)

##### reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- Peterson_female_Q.5$Q1_1
# removing characters /letters and x is only numbers
x <- str_extract_all(subject,"\\(?[0-9,.]+\\)?") # [[1]]
subject_num <- unlist(x)
first_num <- substr(subject_num, 1,1)

# removing zero if first number
subject_num_fixed <- as.numeric(ifelse(first_num == "0", substring(subject_num, 2), subject_num))

# adding EL and zeros as needed
IDENT_SUBID <- ifelse(subject_num_fixed < 10, paste0("EL00", subject_num_fixed), 
                      ifelse(subject_num_fixed <100, paste0("EL0", subject_num_fixed), 
                   ifelse(subject_num_fixed >= 100, paste0("EL", subject_num_fixed), NA)))
# replace subject with your new subjectid 
Peterson_female_Q.5$Q1_1 <- IDENT_SUBID

#############################
#adding child-reported data 
child_peterson <- read.csv("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Peterson_Female_Questionnaire_CHILD version.csv")
child_peterson <-child_peterson[10:nrow(child_peterson),]
nrow(child_peterson)
### get subnum
subject <- child_peterson$Q1_1
# removing characters /letters and x is only numbers
x <- str_extract_all(subject,"\\(?[0-9,.]+\\)?") # [[1]]
subject_num <- unlist(x)
first_num <- substr(subject_num, 1,1)

# removing zero if first number
subject_num_fixed <- as.numeric(ifelse(first_num == "0", substring(subject_num, 2), subject_num))

# adding EL and zeros as needed
IDENT_SUBID <- ifelse(subject_num_fixed < 10, paste0("EL00", subject_num_fixed), 
                      ifelse(subject_num_fixed <100, paste0("EL0", subject_num_fixed), 
                             ifelse(subject_num_fixed >= 100, paste0("EL", subject_num_fixed), NA)))
# replace subject with your new subjectid 
child_peterson$Q1_1<- IDENT_SUBID
child_peterson<-child_peterson[,8:30]
child_peterson2 <- child_peterson[,-c(2,3,4,5,6,7,8,9,11,12)]

# get subjectIDS for each dataframe.
child_list <- child_peterson2$Q1_1
other_list <- Peterson_female_Q.5$Q1_1

# everyone except for EL007 are already in this dataframe.
child_list %in% other_list
# so let's replace them! 
# BUT not replacing EL009 - becuase they are only 8. 
# should keep parent-reported here!

child_peterson3 <- subset(child_peterson2, IDENT_SUBID != "EL009") 
nrow(child_peterson3)
ncol(child_peterson3)

# check that col names are identical before merging!! 
identical(names(child_peterson3), names(Peterson_female_Q.5))

# remove the subjects who you're replacing in PEterson_female_Q.5 
child_peterson3$Q1_1 # EL002, EL005 and EL022 need to be replaced
# EL007 just needs to be added.

Peterson_female_Q.5 <- subset(Peterson_female_Q.5, 
                              (Q1_1 != "EL002" & Q1_1 != "EL005" & Q1_1 != "EL022"))
nrow(Peterson_female_Q.5)
nrow(child_peterson3)
###############
# replace child data into Peterson_female_Q5 
Peterson_merged <- rbind(Peterson_female_Q.5, child_peterson3)
# make sure all subjects were merged together successfully!! 
identical(nrow(Peterson_merged), nrow(child_peterson3)+nrow(Peterson_female_Q.5))
Peterson_female_Q.5 <- Peterson_merged

#R needs to read the data as numeric or else it thinks it is a factor.
#You will be replacing blanks with NAs
Peterson_female_Q.5$Q13_1 <- as.numeric(as.character(Peterson_female_Q.5$Q13_1))
Peterson_female_Q.5$Q13_2 <- as.numeric(as.character(Peterson_female_Q.5$Q13_2))

#For menstrual age - convert data in years into months 
Peterson_age <-Peterson_female_Q.5$Q13_1 * 12
Peterson_female_Q.5$Q13_1 <- Peterson_age

#Add data from Q_13_1 and Q_13_2
#This formula will replace NAs into 0s. This formula tells R not to convert additions with NA to NA. Keeps numbers.
#Replace all 0s into blanks on output excel file (PPDS_F_7_MENSTRUAGE)
sum_age <- rowSums(cbind(Peterson_female_Q.5$Q13_1, Peterson_female_Q.5$Q13_2), na.rm = TRUE)
Peterson_female_Q.5$Q13_2 <-ifelse(sum_age == 0, "", as.numeric(as.character(sum_age)))

#Remove column Q13_1
Peterson_female_Q.6<- Peterson_female_Q.5[,-c(9) ] 

#Make another copy to compare Q10_1 and Q10_2 data after next step
Peterson_female_Q.7 <- Peterson_female_Q.6

#Remove any characters/letters for Q10_1 and Q10_2
#Any inputs that did not have numerical data (e.g. ???, idk) or left blank will be converted to character(0). 
#library(stringr)
height <- Peterson_female_Q.7$Q10_1
height_2 <- Peterson_female_Q.7$Q10_2
# removing characters /letters 
b <- str_extract_all(height,"\\(?[0-9,.]+\\)?") # [[1]]
Peterson_female_Q.7$Q10_1 <- b
c <- str_extract_all(height_2,"\\(?[0-9,.]+\\)?") # [[1]]
Peterson_female_Q.7$Q10_2 <- c

#R needs to read the data as numeric or else it thinks it is a factor.
#You will replacing any character(0) with NAs
Peterson_female_Q.7$Q10_1 <- as.numeric(as.character(Peterson_female_Q.7$Q10_1))
Peterson_female_Q.7$Q10_2 <- as.numeric(as.character(Peterson_female_Q.7$Q10_2))

#For height - convert feet to inches
Peterson_height <-Peterson_female_Q.7$Q10_1 * 12
Peterson_female_Q.7$Q10_1 <- Peterson_height

#Add data from Q_10_1 and Q_10_2
#This formula will replace NAs into 0s. This formula allows R not to convert additions with NA to NA. Keeps numbers.
#Replace all 0s into blanks on output excel file (PPDS_F_8_HEIGHT)
sum_height <- rowSums(cbind(Peterson_female_Q.7$Q10_1, Peterson_female_Q.7$Q10_2), na.rm = TRUE)
Peterson_female_Q.7$Q10_2 <-sum_height
#Remove column Q10_1
Peterson_female_Q.8<- Peterson_female_Q.7[,-c(10) ] 

#fix entry removed
Peterson_female_Q.8 [20,10] = 46.50
Peterson_female_Q.8 [69,10] = 44

# start relabeling columns 
names(Peterson_female_Q.8)
ncol(Peterson_female_Q.8)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("PPDS_F_DATE_COMPLETE","IDENT_SUBID", "PPDS_F_1", "PPDS_F_2", "PPDS_F_3", "PPDS_F_4","PPDS_F_5","PPDS_F_6_MENTSTRU",
                   "PPDS_F_7_MENSTRUAGE","PPDS_F_8_HEIGHT","PPDS_F_9_WEIGHT")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(Peterson_female_Q.8) <- var_names

#Remove any characters/letters for PPDS_F_9_WEIGHT
#Any inputs that did not have numerical data (e.g. ???, idk) or left blank will be converted to character(0). 
#library(stringr)
weight <- Peterson_female_Q.8$PPDS_F_9_WEIGHT
# removing characters /letters 
w <- str_extract_all(weight,"\\(?[0-9,.]+\\)?") # [[1]]
Peterson_female_Q.8$PPDS_F_9_WEIGHT <-w
#R needs to read the data as numeric or else it thinks it is a factor.
#You will replacing any character(0) with NAs
Peterson_female_Q.8$PPDS_F_9_WEIGHT <- as.numeric(as.character(Peterson_female_Q.8$PPDS_F_9_WEIGHT))
#Replace NAs to blanks in output excel sheet (PPDS_F_9_WEIGHT)
Peterson_female_Q.8$PPDS_F_9_WEIGHT<- ifelse(is.na(Peterson_female_Q.8$PPDS_F_9_WEIGHT), "", Peterson_female_Q.8$PPDS_F_9_WEIGHT)

#Convert data to Data Manual Codes. 
Peterson_female_Q.8$PPDS_F_6_MENTSTRU <- ifelse(grepl(1, Peterson_female_Q.8$PPDS_F_6_MENTSTRU), 1, ifelse(grepl(2, Peterson_female_Q.8$PPDS_F_6_MENTSTRU), 4, ""))


#add overlapping PACCT/ELFK subject data 
PPDS_F_Q<- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/PAACT_ELFK_Overlapping_Qs/Peterson_female_PACCT_ELFK_subjects.csv")
#view
head(PPDS_F_Q)
names(PPDS_F_Q)
nrow(PPDS_F_Q)
ncol(PPDS_F_Q)

##### reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject_pacct <- PPDS_F_Q$IDENT_ID
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
PPDS_F_Q$IDENT_ID <- IDENT_SUBID_pacct

#Edit PACCT subject IDs into ELFK subject IDs
PPDS_F_Q[1,1] = "EL149"
#ADD missing column
PPDS_F_Q$PPDS_F_DATE_COMPLETE<- NA

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names_pacct <- cbind("IDENT_SUBID", "PPDS_F_1", "PPDS_F_2", "PPDS_F_3", "PPDS_F_4","PPDS_F_5","PPDS_F_6_MENTSTRU",
                   "PPDS_F_7_MENSTRUAGE","PPDS_F_8_HEIGHT","PPDS_F_9_WEIGHT", "PPDS_F_DATE_COMPLETE")
# make this to check column length
ncolvars_pacct<-ncol(var_names_pacct)
# now change the variable names of our dataframe to match.
names(PPDS_F_Q) <- var_names_pacct

#put columns in right order 
PPDS_F_Q <- PPDS_F_Q[c("PPDS_F_DATE_COMPLETE","IDENT_SUBID", "PPDS_F_1", "PPDS_F_2", "PPDS_F_3", "PPDS_F_4","PPDS_F_5","PPDS_F_6_MENTSTRU",
                                          "PPDS_F_7_MENSTRUAGE","PPDS_F_8_HEIGHT","PPDS_F_9_WEIGHT")]
# get subjectIDS for each dataframe.
PACCT_list <- PPDS_F_Q$IDENT_SUBID
ELFK_list <- Peterson_female_Q.8$IDENT_SUBID

# check that col names are identical before merging!! 
identical(names(PPDS_F_Q), names(Peterson_female_Q.8))

# add pacct data into SecurityScale_Q.4
PPDS_F_merged <- rbind(PPDS_F_Q,Peterson_female_Q.8)

# make sure all subjects were merged together successfully!! 
identical(nrow(PPDS_F_merged), nrow(PPDS_F_Q)+nrow(Peterson_female_Q.8))
Peterson_female_Q.9 <- PPDS_F_merged



# write to csv file on elvis
write.csv(Peterson_female_Q.9, "/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Peterson_Female_Questionnaire_new.csv", row.names = FALSE)
