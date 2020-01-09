#last extraction on 6/28/2018
# Made by Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# LIFE EVENTS QUESTIONNAIRE
################################################################################################

# load in qualtrics data
setwd("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
LEQ <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_LEQ_Questionnaire.csv")
#view
head(LEQ)
names(LEQ)
nrow(LEQ)
ncol(LEQ)

# [rows, columns]
# get rid of extra columns with random qualtrics info
LEQ<- LEQ[,8:102]

LEQ.1 <- LEQ[,-c(2,3,4,5,6,7,8,9,10)]

# get rid of extra rows with extra labels
row_num <-nrow(LEQ.1) #this step is to update number of rows based on new data#
LEQ.2<- LEQ.1[3:row_num,]

#view
nrow(LEQ.2)
ncol(LEQ.2)

#Delete double entries - discussed with TC. Only for old data, new data does not have this issue 
LEQ.3 <- LEQ.2 [-c(81,80,79,78,77,72,76,74,73,71,69,67,65,63,61,59,57,55,54,35,33,64,68,100,66,125,62,127,60,37,83,84,126,
                   12,36,58,85,38,40,34,42,41,123,39,52),]
#view
nrow(LEQ.3)
ncol(LEQ.3)

#Add missing ID number
LEQ.3$Q1 <- as.character(LEQ.3$Q1)
LEQ.3 [77,2] = "EL122"
LEQ.3 [88,2] = "EL141" #FIX THIS ONE ONCE I HAVE AN ANSWER FROM GIRLS on WHO IS T HE MISSING PERSON ON QUALTRICS


# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- LEQ.3$Q1
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
LEQ.3$Q1 <- IDENT_SUBID

#Create new columns to match Data Entry Master format, and code
LEQ.3$LEQ_1A_3MOS <- ifelse(LEQ.3$Q3.1_1 == 2, 1, 0)
LEQ.3$LEQ_1A_12MOS <- ifelse(LEQ.3$Q3.1_1 ==3, 1, 0)
LEQ.3$LEQ_1B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_1),0, ifelse(grepl(2, LEQ.3$Q3.2_1),1, ifelse(grepl(3, LEQ.3$Q3.2_1),2, ifelse(grepl(4, LEQ.3$Q3.2_1), 3,""))))
LEQ.3$LEQ_2A_3MOS <- ifelse(LEQ.3$Q3.1_2 == 2, 1, 0)
LEQ.3$LEQ_2A_12MOS <- ifelse(LEQ.3$Q3.1_2 ==3, 1, 0)
LEQ.3$LEQ_2B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_2),0, ifelse(grepl(2, LEQ.3$Q3.2_2),1, ifelse(grepl(3, LEQ.3$Q3.2_2),2, ifelse(grepl(4, LEQ.3$Q3.2_2), 3,""))))
LEQ.3$LEQ_3A_3MOS <- ifelse(LEQ.3$Q3.1_3 == 2, 1, 0)
LEQ.3$LEQ_3A_12MOS <- ifelse(LEQ.3$Q3.1_3 ==3, 1, 0)
LEQ.3$LEQ_3B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_3),0, ifelse(grepl(2, LEQ.3$Q3.2_3),1, ifelse(grepl(3, LEQ.3$Q3.2_3),2, ifelse(grepl(4, LEQ.3$Q3.2_3), 3,""))))
LEQ.3$LEQ_4A_3MOS <- ifelse(LEQ.3$Q3.1_4 == 2, 1, 0)
LEQ.3$LEQ_4A_12MOS <- ifelse(LEQ.3$Q3.1_4 ==3, 1, 0)
LEQ.3$LEQ_4B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_4),0, ifelse(grepl(2, LEQ.3$Q3.2_4),1, ifelse(grepl(3, LEQ.3$Q3.2_4),2, ifelse(grepl(4, LEQ.3$Q3.2_4), 3,""))))
LEQ.3$LEQ_5A_3MOS <- ifelse(LEQ.3$Q3.1_5 == 2, 1, 0)
LEQ.3$LEQ_5A_12MOS <- ifelse(LEQ.3$Q3.1_5 ==3, 1, 0)
LEQ.3$LEQ_5B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_5),0, ifelse(grepl(2, LEQ.3$Q3.2_5),1, ifelse(grepl(3, LEQ.3$Q3.2_5),2, ifelse(grepl(4, LEQ.3$Q3.2_5), 3,""))))
LEQ.3$LEQ_6A_3MOS <- ifelse(LEQ.3$Q3.1_6 == 2, 1, 0)
LEQ.3$LEQ_6A_12MOS <- ifelse(LEQ.3$Q3.1_6 ==3, 1, 0)
LEQ.3$LEQ_6B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_6),0, ifelse(grepl(2, LEQ.3$Q3.2_6),1, ifelse(grepl(3, LEQ.3$Q3.2_6),2, ifelse(grepl(4, LEQ.3$Q3.2_6), 3,""))))
LEQ.3$LEQ_7A_3MOS <- ifelse(LEQ.3$Q3.1_7 == 2, 1, 0)
LEQ.3$LEQ_7A_12MOS <- ifelse(LEQ.3$Q3.1_7 ==3, 1, 0)
LEQ.3$LEQ_7B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_7),0, ifelse(grepl(2, LEQ.3$Q3.2_7),1, ifelse(grepl(3, LEQ.3$Q3.2_7),2, ifelse(grepl(4, LEQ.3$Q3.2_7), 3,""))))
LEQ.3$LEQ_8A_3MOS <- ifelse(LEQ.3$Q3.1_8 == 2, 1, 0)
LEQ.3$LEQ_8A_12MOS <- ifelse(LEQ.3$Q3.1_8 ==3, 1, 0)
LEQ.3$LEQ_8B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_8),0, ifelse(grepl(2, LEQ.3$Q3.2_8),1, ifelse(grepl(3, LEQ.3$Q3.2_8),2, ifelse(grepl(4, LEQ.3$Q3.2_8), 3,""))))
LEQ.3$LEQ_9A_3MOS <- ifelse(LEQ.3$Q3.1_9 == 2, 1, 0)
LEQ.3$LEQ_9A_12MOS <- ifelse(LEQ.3$Q3.1_9 ==3, 1, 0)
LEQ.3$LEQ_9B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_9),0, ifelse(grepl(2, LEQ.3$Q3.2_9),1, ifelse(grepl(3, LEQ.3$Q3.2_9),2, ifelse(grepl(4, LEQ.3$Q3.2_9), 3,""))))
LEQ.3$LEQ_10A_3MOS <- ifelse(LEQ.3$Q3.1_10 == 2, 1, 0)
LEQ.3$LEQ_10A_12MOS <- ifelse(LEQ.3$Q3.1_10 ==3, 1, 0)
LEQ.3$LEQ_10B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_10),0, ifelse(grepl(2, LEQ.3$Q3.2_10),1, ifelse(grepl(3, LEQ.3$Q3.2_10),2, ifelse(grepl(4, LEQ.3$Q3.2_10), 3,""))))
LEQ.3$LEQ_11A_3MOS <- ifelse(LEQ.3$Q3.1_11 == 2, 1, 0)
LEQ.3$LEQ_11A_12MOS <- ifelse(LEQ.3$Q3.1_11 ==3, 1, 0)
LEQ.3$LEQ_11B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_11),0, ifelse(grepl(2, LEQ.3$Q3.2_11),1, ifelse(grepl(3, LEQ.3$Q3.2_11),2, ifelse(grepl(4, LEQ.3$Q3.2_11), 3,""))))
LEQ.3$LEQ_12A_3MOS <- ifelse(LEQ.3$Q3.1_12 == 2, 1, 0)
LEQ.3$LEQ_12A_12MOS <- ifelse(LEQ.3$Q3.1_12 ==3, 1, 0)
LEQ.3$LEQ_12B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_12),0, ifelse(grepl(2, LEQ.3$Q3.2_12),1, ifelse(grepl(3, LEQ.3$Q3.2_12),2, ifelse(grepl(4, LEQ.3$Q3.2_12), 3,""))))
LEQ.3$LEQ_13A_3MOS <- ifelse(LEQ.3$Q3.1_13 == 2, 1, 0)
LEQ.3$LEQ_13A_12MOS <- ifelse(LEQ.3$Q3.1_13 ==3, 1, 0)
LEQ.3$LEQ_13B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_13),0, ifelse(grepl(2, LEQ.3$Q3.2_13),1, ifelse(grepl(3, LEQ.3$Q3.2_13),2, ifelse(grepl(4, LEQ.3$Q3.2_13), 3,""))))
LEQ.3$LEQ_14A_3MOS <- ifelse(LEQ.3$Q3.1_14 == 2, 1, 0)
LEQ.3$LEQ_14A_12MOS <- ifelse(LEQ.3$Q3.1_14 ==3, 1, 0)
LEQ.3$LEQ_14B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_14),0, ifelse(grepl(2, LEQ.3$Q3.2_14),1, ifelse(grepl(3, LEQ.3$Q3.2_14),2, ifelse(grepl(4, LEQ.3$Q3.2_14), 3,""))))
LEQ.3$LEQ_15A_3MOS <- ifelse(LEQ.3$Q3.1_15 == 2, 1, 0)
LEQ.3$LEQ_15A_12MOS <- ifelse(LEQ.3$Q3.1_15 ==3, 1, 0)
LEQ.3$LEQ_15B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_15),0, ifelse(grepl(2, LEQ.3$Q3.2_15),1, ifelse(grepl(3, LEQ.3$Q3.2_15),2, ifelse(grepl(4, LEQ.3$Q3.2_15), 3,""))))
LEQ.3$LEQ_16A_3MOS <- ifelse(LEQ.3$Q3.1_16 == 2, 1, 0)
LEQ.3$LEQ_16A_12MOS <- ifelse(LEQ.3$Q3.1_16 ==3, 1, 0)
LEQ.3$LEQ_16B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_16),0, ifelse(grepl(2, LEQ.3$Q3.2_16),1, ifelse(grepl(3, LEQ.3$Q3.2_16),2, ifelse(grepl(4, LEQ.3$Q3.2_16), 3,""))))
LEQ.3$LEQ_17A_3MOS <- ifelse(LEQ.3$Q3.1_17 == 2, 1, 0)
LEQ.3$LEQ_17A_12MOS <- ifelse(LEQ.3$Q3.1_17 ==3, 1, 0)
LEQ.3$LEQ_17B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_17),0, ifelse(grepl(2, LEQ.3$Q3.2_17),1, ifelse(grepl(3, LEQ.3$Q3.2_17),2, ifelse(grepl(4, LEQ.3$Q3.2_17), 3,""))))
LEQ.3$LEQ_18A_3MOS <- ifelse(LEQ.3$Q3.1_18 == 2, 1, 0)
LEQ.3$LEQ_18A_12MOS <- ifelse(LEQ.3$Q3.1_18 ==3, 1, 0)
LEQ.3$LEQ_18B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_18),0, ifelse(grepl(2, LEQ.3$Q3.2_18),1, ifelse(grepl(3, LEQ.3$Q3.2_18),2, ifelse(grepl(4, LEQ.3$Q3.2_18), 3,""))))
LEQ.3$LEQ_19A_3MOS <- ifelse(LEQ.3$Q3.1_19 == 2, 1, 0)
LEQ.3$LEQ_19A_12MOS <- ifelse(LEQ.3$Q3.1_19 ==3, 1, 0)
LEQ.3$LEQ_19B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_19),0, ifelse(grepl(2, LEQ.3$Q3.2_19),1, ifelse(grepl(3, LEQ.3$Q3.2_19),2, ifelse(grepl(4, LEQ.3$Q3.2_19), 3,""))))
LEQ.3$LEQ_20A_3MOS <- ifelse(LEQ.3$Q3.1_20 == 2, 1, 0)
LEQ.3$LEQ_20A_12MOS <- ifelse(LEQ.3$Q3.1_20 ==3, 1, 0)
LEQ.3$LEQ_20B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_20),0, ifelse(grepl(2, LEQ.3$Q3.2_20),1, ifelse(grepl(3, LEQ.3$Q3.2_20),2, ifelse(grepl(4, LEQ.3$Q3.2_20), 3,""))))
LEQ.3$LEQ_21A_3MOS <- ifelse(LEQ.3$Q3.1_21 == 2, 1, 0)
LEQ.3$LEQ_21A_12MOS <- ifelse(LEQ.3$Q3.1_21 ==3, 1, 0)
LEQ.3$LEQ_21B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_21),0, ifelse(grepl(2, LEQ.3$Q3.2_21),1, ifelse(grepl(3, LEQ.3$Q3.2_21),2, ifelse(grepl(4, LEQ.3$Q3.2_21), 3,""))))
LEQ.3$LEQ_22A_3MOS <- ifelse(LEQ.3$Q3.1_22 == 2, 1, 0)
LEQ.3$LEQ_22A_12MOS <- ifelse(LEQ.3$Q3.1_22 ==3, 1, 0)
LEQ.3$LEQ_22B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_22),0, ifelse(grepl(2, LEQ.3$Q3.2_22),1, ifelse(grepl(3, LEQ.3$Q3.2_22),2, ifelse(grepl(4, LEQ.3$Q3.2_22), 3,""))))
LEQ.3$LEQ_23A_3MOS <- ifelse(LEQ.3$Q3.1_23 == 2, 1, 0)
LEQ.3$LEQ_23A_12MOS <- ifelse(LEQ.3$Q3.1_23 ==3, 1, 0)
LEQ.3$LEQ_23B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_23),0, ifelse(grepl(2, LEQ.3$Q3.2_23),1, ifelse(grepl(3, LEQ.3$Q3.2_23),2, ifelse(grepl(4, LEQ.3$Q3.2_23), 3,""))))
LEQ.3$LEQ_24A_3MOS <- ifelse(LEQ.3$Q3.1_24 == 2, 1, 0)
LEQ.3$LEQ_24A_12MOS <- ifelse(LEQ.3$Q3.1_24 ==3, 1, 0)
LEQ.3$LEQ_24B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_24),0, ifelse(grepl(2, LEQ.3$Q3.2_24),1, ifelse(grepl(3, LEQ.3$Q3.2_24),2, ifelse(grepl(4, LEQ.3$Q3.2_24), 3,""))))
LEQ.3$LEQ_25A_3MOS <- ifelse(LEQ.3$Q3.1_25 == 2, 1, 0)
LEQ.3$LEQ_25A_12MOS <- ifelse(LEQ.3$Q3.1_25 ==3, 1, 0)
LEQ.3$LEQ_25B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_25),0, ifelse(grepl(2, LEQ.3$Q3.2_25),1, ifelse(grepl(3, LEQ.3$Q3.2_25),2, ifelse(grepl(4, LEQ.3$Q3.2_25), 3,""))))
LEQ.3$LEQ_26A_3MOS <- ifelse(LEQ.3$Q3.1_26 == 2, 1, 0)
LEQ.3$LEQ_26A_12MOS <- ifelse(LEQ.3$Q3.1_26 ==3, 1, 0)
LEQ.3$LEQ_26B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_26),0, ifelse(grepl(2, LEQ.3$Q3.2_26),1, ifelse(grepl(3, LEQ.3$Q3.2_26),2, ifelse(grepl(4, LEQ.3$Q3.2_26), 3,""))))
LEQ.3$LEQ_27A_3MOS <- ifelse(LEQ.3$Q3.1_27 == 2, 1, 0)
LEQ.3$LEQ_27A_12MOS <- ifelse(LEQ.3$Q3.1_27 ==3, 1, 0)
LEQ.3$LEQ_27B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_27),0, ifelse(grepl(2, LEQ.3$Q3.2_27),1, ifelse(grepl(3, LEQ.3$Q3.2_27),2, ifelse(grepl(4, LEQ.3$Q3.2_27), 3,""))))
LEQ.3$LEQ_28A_3MOS <- ifelse(LEQ.3$Q3.1_28 == 2, 1, 0)
LEQ.3$LEQ_28A_12MOS <- ifelse(LEQ.3$Q3.1_28 ==3, 1, 0)
LEQ.3$LEQ_28B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_28),0, ifelse(grepl(2, LEQ.3$Q3.2_28),1, ifelse(grepl(3, LEQ.3$Q3.2_28),2, ifelse(grepl(4, LEQ.3$Q3.2_28), 3,""))))
LEQ.3$LEQ_29A_3MOS <- ifelse(LEQ.3$Q3.1_29 == 2, 1, 0)
LEQ.3$LEQ_29A_12MOS <- ifelse(LEQ.3$Q3.1_29 ==3, 1, 0)
LEQ.3$LEQ_29B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_29),0, ifelse(grepl(2, LEQ.3$Q3.2_29),1, ifelse(grepl(3, LEQ.3$Q3.2_29),2, ifelse(grepl(4, LEQ.3$Q3.2_29), 3,""))))
LEQ.3$LEQ_30A_3MOS <- ifelse(LEQ.3$Q3.1_30 == 2, 1, 0)
LEQ.3$LEQ_30A_12MOS <- ifelse(LEQ.3$Q3.1_30 ==3, 1, 0)
LEQ.3$LEQ_30B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_30),0, ifelse(grepl(2, LEQ.3$Q3.2_30),1, ifelse(grepl(3, LEQ.3$Q3.2_30),2, ifelse(grepl(4, LEQ.3$Q3.2_30), 3,""))))
LEQ.3$LEQ_31A_3MOS <- ifelse(LEQ.3$Q3.1_31 == 2, 1, 0)
LEQ.3$LEQ_31A_12MOS <- ifelse(LEQ.3$Q3.1_31 ==3, 1, 0)
LEQ.3$LEQ_31B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_31),0, ifelse(grepl(2, LEQ.3$Q3.2_31),1, ifelse(grepl(3, LEQ.3$Q3.2_31),2, ifelse(grepl(4, LEQ.3$Q3.2_31), 3,""))))
LEQ.3$LEQ_32A_3MOS <- ifelse(LEQ.3$Q3.1_32 == 2, 1, 0)
LEQ.3$LEQ_32A_12MOS <- ifelse(LEQ.3$Q3.1_32 ==3, 1, 0)
LEQ.3$LEQ_32B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_32),0, ifelse(grepl(2, LEQ.3$Q3.2_32),1, ifelse(grepl(3, LEQ.3$Q3.2_32),2, ifelse(grepl(4, LEQ.3$Q3.2_32), 3,""))))
LEQ.3$LEQ_33A_3MOS <- ifelse(LEQ.3$Q3.1_33 == 2, 1, 0)
LEQ.3$LEQ_33A_12MOS <- ifelse(LEQ.3$Q3.1_33 ==3, 1, 0)
LEQ.3$LEQ_33B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_33),0, ifelse(grepl(2, LEQ.3$Q3.2_33),1, ifelse(grepl(3, LEQ.3$Q3.2_33),2, ifelse(grepl(4, LEQ.3$Q3.2_33), 3,""))))
LEQ.3$LEQ_34A_3MOS <- ifelse(LEQ.3$Q3.1_34 == 2, 1, 0)
LEQ.3$LEQ_34A_12MOS <- ifelse(LEQ.3$Q3.1_34 ==3, 1, 0)
LEQ.3$LEQ_34B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_34),0, ifelse(grepl(2, LEQ.3$Q3.2_34),1, ifelse(grepl(3, LEQ.3$Q3.2_34),2, ifelse(grepl(4, LEQ.3$Q3.2_34), 3,""))))
LEQ.3$LEQ_35A_3MOS <- ifelse(LEQ.3$Q3.1_35 == 2, 1, 0)
LEQ.3$LEQ_35A_12MOS <- ifelse(LEQ.3$Q3.1_35 ==3, 1, 0)
LEQ.3$LEQ_35B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_35),0, ifelse(grepl(2, LEQ.3$Q3.2_35),1, ifelse(grepl(3, LEQ.3$Q3.2_35),2, ifelse(grepl(4, LEQ.3$Q3.2_35), 3,""))))
LEQ.3$LEQ_36A_3MOS <- ifelse(LEQ.3$Q3.1_36 == 2, 1, 0)
LEQ.3$LEQ_36A_12MOS <- ifelse(LEQ.3$Q3.1_36 ==3, 1, 0)
LEQ.3$LEQ_36B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_36),0, ifelse(grepl(2, LEQ.3$Q3.2_36),1, ifelse(grepl(3, LEQ.3$Q3.2_36),2, ifelse(grepl(4, LEQ.3$Q3.2_36), 3,""))))
LEQ.3$LEQ_37A_3MOS <- ifelse(LEQ.3$Q3.1_37 == 2, 1, 0)
LEQ.3$LEQ_37A_12MOS <- ifelse(LEQ.3$Q3.1_37 ==3, 1, 0)
LEQ.3$LEQ_37B_IMPACT <- ifelse(grepl(1, LEQ.3$Q3.2_37),0, ifelse(grepl(2, LEQ.3$Q3.2_37),1, ifelse(grepl(3, LEQ.3$Q3.2_37),2, ifelse(grepl(4, LEQ.3$Q3.2_37), 3,""))))
LEQ.3$LEQ_38A_3MOS <- ifelse(LEQ.3$Q5.1_1 == 2, 1, 0)
LEQ.3$LEQ_38A_12MOS <- ifelse(LEQ.3$Q5.1_1 ==3, 1, 0)
LEQ.3$LEQ_38B_IMPACT <- ifelse(grepl(1, LEQ.3$Q5.2_1),0, ifelse(grepl(2, LEQ.3$Q5.2_1),1, ifelse(grepl(3, LEQ.3$Q5.2_1),2, ifelse(grepl(4, LEQ.3$Q5.2_1), 3,""))))
LEQ.3$LEQ_38_DESCRIP <- LEQ.3$Q4
LEQ.3$LEQ_39A_3MOS <- ifelse(LEQ.3$Q7.1_1 == 2, 1, 0)
LEQ.3$LEQ_39A_12MOS <- ifelse(LEQ.3$Q7.1_1 ==3, 1, 0)
LEQ.3$LEQ_39B_IMPACT <- ifelse(grepl(1, LEQ.3$Q7.2_1),0, ifelse(grepl(2, LEQ.3$Q7.2_1),1, ifelse(grepl(3, LEQ.3$Q7.2_1),2, ifelse(grepl(4, LEQ.3$Q7.2_1), 3,""))))
LEQ.3$LEQ_39_DESCRIP <- LEQ.3$Q6
LEQ.3$LEQ_40A_3MOS <- ifelse(LEQ.3$Q9.1_1 == 2, 1, 0)
LEQ.3$LEQ_40A_12MOS <- ifelse(LEQ.3$Q9.1_1 ==3, 1, 0)
LEQ.3$LEQ_40B_IMPACT <- ifelse(grepl(1, LEQ.3$Q9.2_1),0, ifelse(grepl(2, LEQ.3$Q9.2_1),1, ifelse(grepl(3, LEQ.3$Q9.2_1),2, ifelse(grepl(4, LEQ.3$Q9.2_1), 3,""))))
LEQ.3$LEQ_40_DESCRIP <- LEQ.3$Q8

#Delete old columns now that the new columns have been created
LEQ.4 <- LEQ.3[, -c(3:86)]

#Add max impact question at the end and enter data.
#This needs to be updated continuously
#Updated last for 6/28 extraction
LEQ.4$LEQ_MAX_IMPACT <- NA
LEQ.4 [1,126] = 3
LEQ.4 [2,126] = 1
LEQ.4 [3,126] = 2
LEQ.4 [4,126] = 3
LEQ.4 [5,126] = 3
LEQ.4 [6,126] = 3
LEQ.4 [7,126] = 3
LEQ.4 [8,126] = 3
LEQ.4 [9,126] = 3
LEQ.4 [10,126] = 2
LEQ.4 [11,126] = 2
LEQ.4 [12,126] = 3
LEQ.4 [13,126] = 3
LEQ.4 [14,126] = 3
LEQ.4 [15,126] = 1
LEQ.4 [16,126] = 3
LEQ.4 [17,126] = 3
LEQ.4 [18,126] = 2
LEQ.4 [19,126] = 3
LEQ.4 [20,126] = 0
LEQ.4 [21,126] = 3
LEQ.4 [22,126] = 3
LEQ.4 [23,126] = 3
LEQ.4 [24,126] = 2
LEQ.4 [25,126] = 1
LEQ.4 [26,126] = 1
LEQ.4 [27,126] = 2
LEQ.4 [28,126] = 3
LEQ.4 [29,126] = 1
LEQ.4 [30,126] = 2
LEQ.4 [31,126] = 2
LEQ.4 [32,126] = 0
LEQ.4 [33,126] = 0
LEQ.4 [34,126] = 0
LEQ.4 [35,126] = 0
LEQ.4 [36,126] = 0
LEQ.4 [37,126] = 0
LEQ.4 [38,126] = 0
LEQ.4 [39,126] = 0
LEQ.4 [40,126] = 0
LEQ.4 [41,126] = 0
LEQ.4 [42,126] = 0
LEQ.4 [43,126] = 0
LEQ.4 [44,126] = 0
LEQ.4 [45,126] = 0
LEQ.4 [46,126] = 0
LEQ.4 [47,126] = 1
LEQ.4 [48,126] = 0
LEQ.4 [49,126] = 0
LEQ.4 [50,126] = 0
LEQ.4 [51,126] = 0
LEQ.4 [52,126] = 0
LEQ.4 [53,126] = 0
LEQ.4 [54,126] = 0
LEQ.4 [55,126] = 0
LEQ.4 [56,126] = 0
LEQ.4 [57,126] = 0
LEQ.4 [58,126] = 0
LEQ.4 [59,126] = 0
LEQ.4 [60,126] = 0
LEQ.4 [61,126] = 0
LEQ.4 [62,126] = 0
LEQ.4 [63,126] = 0
LEQ.4 [64,126] = 0
LEQ.4 [65,126] = 0
LEQ.4 [66,126] = 0
LEQ.4 [67,126] = 0
LEQ.4 [68,126] = 0
LEQ.4 [69,126] = 0
LEQ.4 [70,126] = 0
LEQ.4 [71,126] = 1
LEQ.4 [72,126] = 2
LEQ.4 [73,126] = 3
LEQ.4 [74,126] = 0
LEQ.4 [75,126] = 1
LEQ.4 [76,126] = 3
LEQ.4 [77,126] = 2
LEQ.4 [78,126] = 1
LEQ.4 [79,126] = 0
LEQ.4 [80,126] = 0
LEQ.4 [81,126] = 3
LEQ.4 [82,126] = 2
LEQ.4 [83,126] = 1
LEQ.4 [84,126] = 1
LEQ.4 [85,126] = 3
LEQ.4 [86,126] = 1
LEQ.4 [87,126] = 0
LEQ.4 [88,126] = 0
LEQ.4 [89,126] = 0
LEQ.4 [90,126] = 2
LEQ.4 [91,126] = 2
LEQ.4 [92,126] = 2
LEQ.4 [93,126] = 3
LEQ.4 [94,126] = 3
LEQ.4 [95,126] = 0
LEQ.4 [96,126] = 0
LEQ.4 [97,126] = 0
LEQ.4 [98,126] = 1
LEQ.4 [99,126] = 3
LEQ.4 [100,126] = 3
LEQ.4 [101,126] = 1
LEQ.4 [102,126] = 2
LEQ.4 [103,126] = 0
LEQ.4 [104,126] = 2
LEQ.4 [105,126] = 1
LEQ.4 [106,126] = 2
LEQ.4 [107,126] = 2
LEQ.4 [108,126] = 1
LEQ.4 [109,126] = 1
LEQ.4 [110,126] = 2
LEQ.4 [111,126] = 0
LEQ.4 [112,126] = 1
LEQ.4 [113,126] = 1

#Change ID column name
names(LEQ.4) [1] <- "LEQ_DATE_COMPLETE"
names(LEQ.4) [2] <- "IDENT_SUBID"

#remove repeats in the new data - Tricia said keep originals b/c of the nature of this questionnaire. Past 3/12 months since they came in.No point in taking new answers that are more recent.
LEQ.4 <- LEQ.4 [-c(41,42,87,94,96,104,105,106,107),]


# write to csv file on lux
write.csv(LEQ.4, "QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_LEQ_Questionnaire.csv", row.names = FALSE)
