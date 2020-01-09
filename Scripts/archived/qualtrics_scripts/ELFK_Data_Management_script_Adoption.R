#last extraction on 7/20/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
#International Adoption Inventory
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
IAI <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_International_Adoption_Inventory.csv")
#view
head(IAI)
names(IAI)
nrow(IAI)
ncol(IAI)

# [rows, columns]
# get rid of extra columns with random qualtrics info
IAI<-IAI[,8:47]
IAI.1 <- IAI[,-c(2,3,4,5,6,7,8,9,10)]

# get rid of extra rows with extra labels
row_num <-nrow(IAI.1) #this step is to update number of rows based on new data#
IAI.2<- IAI.1[4:row_num,]

#view
nrow(IAI.2)
ncol(IAI.2)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- IAI.2$Q1
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
IAI.2$Q1 <- IDENT_SUBID

# start relabeling columns 
names(IAI.2)
ncol(IAI.2)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("IAI_DATE_COMPLETE","IDENT_SUBID", "IAI_1A_COUNTRYADOPT", "IAI_1B_CITYBORN", "IAI_2_CITYSIZE", "IAI_3_AGEORPH","IAI_4_HOWORPH","IAI_5A_SEEORPH","IAI_5B_QUALBUILD","IAI_5C_CLEANFAC", "IAI_5D_QUANTCAREG", "IAI_5E_QUALCAREG",
                   "IAI_6A_SPECBOND", "IAI_6B_EXPLAN", "IAI_7_AGEADOPT", "IAI_8_HEALTHADOPT", "IAI_9A_HEALTHCONDADOPT", "IAI_9B_EXPLAN", "IAI_10A_HEALTHCONDLATER", "IAI_10B_EXPLAN", "IAI_11_REACTMEET", "IAI_12_MONTHADJUST", "IAI_13_CHALLADOPTINT", 
                   "IAI_14A_DEVADVANCE", "IAI_14B_EXPLAN", "IAI_15A_DEVSTRUG", "IAI_15B_EXPLAN", "IAI_16_REWADOPT", "IAI_17_AREACHILDEXCEL", "IAI_18_ AREACHILDSTRUG", "IAI_19B_EXPLAN")

# make this to check column length
ncolvars<-ncol(var_names)
# now change the variable names of our dataframe to match.
names(IAI.2) <- var_names

#add additional column at the end. Rearrange next. 
IAI.2$IAI_19A_OTHERINFO <- NA
IAI.2 <- IAI.2[c("IAI_DATE_COMPLETE", "IDENT_SUBID", "IAI_1A_COUNTRYADOPT", "IAI_1B_CITYBORN", "IAI_2_CITYSIZE", "IAI_3_AGEORPH","IAI_4_HOWORPH","IAI_5A_SEEORPH","IAI_5B_QUALBUILD","IAI_5C_CLEANFAC", "IAI_5D_QUANTCAREG", "IAI_5E_QUALCAREG",
                 "IAI_6A_SPECBOND", "IAI_6B_EXPLAN", "IAI_7_AGEADOPT", "IAI_8_HEALTHADOPT", "IAI_9A_HEALTHCONDADOPT", "IAI_9B_EXPLAN", "IAI_10A_HEALTHCONDLATER", "IAI_10B_EXPLAN", "IAI_11_REACTMEET", "IAI_12_MONTHADJUST", "IAI_13_CHALLADOPTINT", 
                 "IAI_14A_DEVADVANCE", "IAI_14B_EXPLAN", "IAI_15A_DEVSTRUG", "IAI_15B_EXPLAN", "IAI_16_REWADOPT", "IAI_17_AREACHILDEXCEL", "IAI_18_ AREACHILDSTRUG","IAI_19A_OTHERINFO", "IAI_19B_EXPLAN")]

#Delete Domestic adoption families who answered this questionnaire
IAI.2<-IAI.2[-c(39,40,41,42,43),]

#Convert data to Data Manual Codes now

#Convert age placed in orphanage to months IAI_3_AGEORPH to Data Entry manual codes. Updated on 7/5/18
IAI.2$IAI_3_AGEORPH <- as.numeric(as.character(IAI.2$IAI_3_AGEORPH))
IAI.2 [1,6] = 0.5
IAI.2 [2,6] = NA
IAI.2 [3,6] = 0
IAI.2 [4,6] = 0
IAI.2 [5,6] = 0
IAI.2 [6,6] = 3
IAI.2 [7,6] = 0
IAI.2 [8,6] = 1.5
IAI.2 [9,6] = 0
IAI.2 [10,6] = 0
IAI.2 [11,6] = 0
IAI.2 [12,6] = 1
IAI.2 [13,6] = 0
IAI.2 [14,6] = 0
IAI.2 [15,6] = 2
IAI.2 [16,6] = 0
IAI.2 [17,6] = 9
IAI.2 [18,6] = 6
IAI.2 [19,6] = 0.3
IAI.2 [20,6] = 6
IAI.2 [21,6] = 9
IAI.2 [22,6] = 5
IAI.2 [24,6] = 0
IAI.2 [25,6] = 1.5
IAI.2 [26,6] = NA
IAI.2 [27,6] = 2
IAI.2 [28,6] = 0.25
IAI.2[29, 6]  = NA                                                                                      
IAI.2[30, 6] = 4                                                                                          
IAI.2[31, 6] =1                                                                                   
IAI.2 [32,6] = 6                               
IAI.2[33,6] = 1                                                                                    
IAI.2[34,6] = 2                                                                                   
IAI.2[35,6] = 2
IAI.2[36,6] = 1
IAI.2 [37,6] = NA
IAI.2[38,6] = 0
IAI.2[39,6] = 0
IAI.2[40,6] = 1
IAI.2[41,6] = 1
IAI.2[42,6] = 6

#Convert data to Data Manual Codes. 
IAI.2$IAI_5A_SEEORPH<- ifelse(grepl(1, IAI.2$IAI_5A_SEEORPH), 1, ifelse(grepl(2, IAI.2$IAI_5A_SEEORPH), 0, ""))
IAI.2$IAI_6A_SPECBOND<- ifelse(grepl(1, IAI.2$IAI_6A_SPECBOND), 1, ifelse(grepl(2, IAI.2$IAI_6A_SPECBOND), 0, ""))

#Convert age for IAI_7_AGEADOPT to months
# remove words 
# Double check answers are in months, not years, no fractions, or word answers. Add edits in this section.
IAI.2$IAI_7_AGEADOPT <- as.character(IAI.2$IAI_7_AGEADOPT)
ageadopt <- IAI.2$IAI_7_AGEADOPT
age <- str_extract_all(ageadopt,"\\(?[0-9]+.\\)?") # [[1]]
IAI.2$IAI_7_AGEADOPT <- age
IAI.2$IAI_7_AGEADOPT <- as.numeric(as.character(IAI.2$IAI_7_AGEADOPT))
IAI.2[2,15] = 24
IAI.2[3,15] = 10.5
IAI.2[6,15] = 8.5
IAI.2[9,15] = 10
IAI.2[10,15] = 10.5
IAI.2[11,15] = 10.5
IAI.2[20,15] = 42
IAI.2[21,15] = 72
IAI.2[22,15] = 90
IAI.2[25,15] = 30
IAI.2[26,15] = 36
IAI.2[28,15] = 48
IAI.2[29,15] = 0.5
IAI.2[30,15] =9
IAI.2[31,15] = 5
IAI.2[32,15] =18.5
IAI.2[33,15] =24
IAI.2[34,15] =22 
IAI.2[35,15] = 7.25
IAI.2[36,15] = 11

IAI.2$IAI_9A_HEALTHCONDADOPT<- ifelse(grepl(1, IAI.2$IAI_9A_HEALTHCONDADOPT), 1, ifelse(grepl(2, IAI.2$IAI_9A_HEALTHCONDADOPT), 0, ""))
IAI.2$IAI_10A_HEALTHCONDLATER<- ifelse(grepl(1, IAI.2$IAI_10A_HEALTHCONDLATER), 1, ifelse(grepl(2, IAI.2$IAI_10A_HEALTHCONDLATER), 0, ""))

#Convert entries IAI_12_MONTHADJUST to Data Entry manual codes
IAI.2$IAI_12_MONTHADJUST <- as.numeric(as.character(IAI.2$IAI_12_MONTHADJUST))

IAI.2 [1,22] = 9
IAI.2 [2,22] = 1
IAI.2 [3,22] = 1
IAI.2 [4,22] = 0
IAI.2 [5,22] = 2
IAI.2 [6,22] = 1
IAI.2 [7,22] = 2
IAI.2 [8,22] = 10
IAI.2 [9,22] = 0
IAI.2 [10,22] = 1
IAI.2 [11,22] = 1
IAI.2 [12,22] = 2
IAI.2 [13,22] = 1
IAI.2 [14,22] = 3
IAI.2 [15,22] = 0
IAI.2 [16,22] = 5
IAI.2 [17,22] = 3
IAI.2 [18,22] = 0
IAI.2 [19,22] = 3
IAI.2 [20,22] = 3
IAI.2 [21,22] = 5
IAI.2 [22,22] = 9
IAI.2 [23,22] = 0
IAI.2 [24,22] = 6
IAI.2 [25,22] = 3
IAI.2 [26,22] = 0
IAI.2 [27,22] = 1
IAI.2 [28,22] = 9
IAI.2 [29,22] = 0
IAI.2 [30,22] = 1
IAI.2 [31,22] = 9
IAI.2 [32,22] = 1
IAI.2 [33,22] = 1
IAI.2 [34,22] = 4
IAI.2 [35,22] = 10
IAI.2 [36,22] = 5
IAI.2 [37,22] = 3                             
IAI.2[38,22] = 0                                                                                  
IAI.2[39,22] = 0                                                                                
IAI.2[40,22] = 0
IAI.2[41,22] = 0
IAI.2[42,22] = 0

IAI.2$IAI_14A_DEVADVANCE<- ifelse(grepl(1, IAI.2$IAI_14A_DEVADVANCE), 1, ifelse(grepl(2, IAI.2$IAI_14A_DEVADVANCE), 0, ""))
IAI.2$IAI_15A_DEVSTRUG<- ifelse(grepl(1, IAI.2$IAI_15A_DEVSTRUG), 1, ifelse(grepl(2, IAI.2$IAI_15A_DEVSTRUG), 0, ""))

#Add Data Entry Manual codes to IAI_19A_OTHERINFO
# This question was never on qualtrics so you have to manually enter this based on answers to IAI_19B_EXPLAN

IAI.2 [1,31] = 1
IAI.2 [2,31] = 0
IAI.2 [3,31] = 1
IAI.2 [4,31] = 1
IAI.2 [5,31] = 0
IAI.2 [6,31] = 0
IAI.2 [7,31] = 0
IAI.2 [8,31] = 0
IAI.2 [9,31] = 1
IAI.2 [10,31] = 0
IAI.2 [11,31] = 0
IAI.2 [12,31] = 0
IAI.2 [13,31] = 0
IAI.2 [14,31] = 0
IAI.2 [15,31] = 0
IAI.2 [16,31] = 0
IAI.2 [17,31] = 1
IAI.2 [18,31] = 0
IAI.2 [19,31] = 1
IAI.2 [20,31] = 0
IAI.2 [21,31] = 0
IAI.2 [22,31] = 1
IAI.2 [23,31] = 0
IAI.2 [24,31] = 0
IAI.2 [25,31] = 0
IAI.2 [26,31] = 1
IAI.2 [27,31] = 1
IAI.2 [28,31] = 1
IAI.2 [29,31] = 0
IAI.2 [30,31] = 0
IAI.2 [31,31] = 0
IAI.2 [32,31] = 0
IAI.2 [33,31] = 1
IAI.2 [34,31] = 1
IAI.2 [35,31] = 0
IAI.2 [36,31] = 0
IAI.2 [37,31] = 0
IAI.2 [38,31] = 1
IAI.2 [39,31] = 0
IAI.2 [40,31] = 0
IAI.2 [41,31] = 0
IAI.2 [42,31] = 1

#Delete double entry for El104 and EL105 - Tricia said male - Korean, female - Chinese
IAI.2<-IAI.2[-c(37,38),]

# write to csv file on elvis
write.csv(IAI.2, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Adoption_Questionnaire.csv", row.names = FALSE)
