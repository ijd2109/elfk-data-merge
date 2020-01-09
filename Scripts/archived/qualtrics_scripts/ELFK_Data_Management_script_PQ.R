# Last extraction date was 7/18/2018 
# Started by Michelle VT
#edited and finished by Michelle Leon on 12/13/17-12/22/17

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# PARENT QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
setwd("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data")
Parent_Q <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Parent_Questionnaire_newformat.csv")
# open first 6 rows fo data
head(Parent_Q)
names(Parent_Q)
nrow(Parent_Q)
ncol(Parent_Q)

# get rid of extra columns with random qualtrics info
Parent_Q<- Parent_Q[,8:41]
Parent_Q.1<- Parent_Q[,-c(2,3,4,5,6,7,8,9,10)]

#add new column for foster care age (for DA subjects)
Parent_Q.1$"PQ_3_AGEFOSTER"<-NA

nrow(Parent_Q.1)
ncol(Parent_Q.1)
# get rid of extra rows with extra labels
# [ rows, columns]
row_num <-nrow(Parent_Q.1) #this step is to update number of rows based on new data#
Parent_Q.2 <- Parent_Q.1[3:row_num,]
Parent_Q.3 <- Parent_Q.2[-c(5, 23,42),]
nrow(Parent_Q.3)
ncol(Parent_Q.3)

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject <- Parent_Q.3$Q1
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
# only add P for Parent questionnaire! 
#IDENT_SUBID <- ifelse(grepl("P", subject) | grepl('p', subject), paste0(IDENT_SUBID, "P"), IDENT_SUBID)

# replace subject with your new subjectid 
Parent_Q.3$Q1 <- IDENT_SUBID

# start relabeling columns 
names(Parent_Q.3)
ncol(Parent_Q.3)

# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("PQ_DATE_COMPLETE","IDENT_SUBID", "PQ_1_COUNTRYBORN","PQ_2_CITYBORN","PQ_3_AGEORPH","PQ_4_AGEADOPT","PQ_5_GENHEALTHADOPT",
                   "PQ_6A_GROCHART","PQ_6B_PERCENTILE",  
                   "PQ_7A_HEALTHCONS","PQ_7B_EXPLAN","PQ_8A_BEHCONS","PQ_8B_EXPLAN","PQ_9_STRENGTH", "PQ_10A_HEALTHCONS_OVERC",
                   "PQ_10B_EXPLAN","PQ_11A_BEHACONS_OVERC", "PQ_11B_EXPLAN","PQ_12A_OTHERCHILD","PQ_12B_AGEOTHERCH",
                   "PQ_12C_ADOPTORBIOCH","PQ_13_MOTIVADOPT","PQ_14A_TALKADOSTORY","PQ_14B_CHENJOYADSTORY","PQ_14C_PENJOYADSTORY",
                   "PQ_3_AGEFOSTER")

# make this to check column length
ncolvars <- ncol(var_names)
# now change the variable names of our dataframe to match.
names(Parent_Q.3) <- var_names

#rearrange columns
Parent_Q.3<- Parent_Q.3[c("PQ_DATE_COMPLETE","IDENT_SUBID", "PQ_1_COUNTRYBORN","PQ_2_CITYBORN","PQ_3_AGEORPH","PQ_3_AGEFOSTER","PQ_4_AGEADOPT","PQ_5_GENHEALTHADOPT",
                          "PQ_6A_GROCHART","PQ_6B_PERCENTILE",  
                          "PQ_7A_HEALTHCONS","PQ_7B_EXPLAN","PQ_8A_BEHCONS","PQ_8B_EXPLAN","PQ_9_STRENGTH", "PQ_10A_HEALTHCONS_OVERC",
                          "PQ_10B_EXPLAN","PQ_11A_BEHACONS_OVERC", "PQ_11B_EXPLAN","PQ_12A_OTHERCHILD","PQ_12B_AGEOTHERCH",
                          "PQ_12C_ADOPTORBIOCH","PQ_13_MOTIVADOPT","PQ_14A_TALKADOSTORY","PQ_14B_CHENJOYADSTORY","PQ_14C_PENJOYADSTORY")]


#Delete double entries, keep later one since countryborn is correct for later entries - Tricia decided
Parent_Q.3<- Parent_Q.3[-c(41,42),]

# code data

#R needs to read the data as numeric or else it thinks it is a factor.
#You will be replacing blanks with NAs
Parent_Q.3$PQ_3_AGEORPH <- as.numeric(as.character(Parent_Q.3$PQ_3_AGEORPH))
Parent_Q.3$PQ_4_AGEADOPT <- as.numeric(as.character(Parent_Q.3$PQ_4_AGEADOPT))
#fill in data for PQ_3 and PQ_4 that was erased (last updated on 6/28/2018)
Parent_Q.3[16, 5] = 2.5
Parent_Q.3[17, 5] = 0
Parent_Q.3[19, 5] = 13
Parent_Q.3[16, 7] = 4
Parent_Q.3[19, 7] = 26
Parent_Q.3[22, 5] = 5
Parent_Q.3[22, 7] = 90
Parent_Q.3[23, 7] = 6
Parent_Q.3[25, 5] = 18
Parent_Q.3[25, 7] = 30
Parent_Q.3[26, 7] = 36
Parent_Q.3[27, 5] = 6 
Parent_Q.3[27, 7] = 20
Parent_Q.3[28, 5] = 0.23
Parent_Q.3[28, 7] = 48
Parent_Q.3[29, 7] = 0.47
Parent_Q.3[30, 5] = 0
Parent_Q.3[30, 7] = 6
Parent_Q.3[31, 5] = 7
Parent_Q.3[31, 7] = 9
Parent_Q.3[32, 5] = 1
Parent_Q.3[32, 7] = 60
Parent_Q.3[34, 7] = 18
Parent_Q.3[35, 7] = 8
Parent_Q.3[36, 5] = 3
Parent_Q.3[36, 7] = 22
Parent_Q.3[37, 5] = 0.33
Parent_Q.3[37, 7] = 15
Parent_Q.3[40, 5] = 0.47
Parent_Q.3[40, 7] = 11
Parent_Q.3[41, 5] = 12.2
Parent_Q.3[41, 7] = 12.2
Parent_Q.3[42, 5] = 8
Parent_Q.3[42, 7] = 30
Parent_Q.3[44, 5] = 0
Parent_Q.3[44, 7] = 1.87
Parent_Q.3[45, 5] = 1.17
Parent_Q.3[45, 7] = 7
Parent_Q.3[46, 5] = 0
Parent_Q.3[46, 7] = 9
Parent_Q.3[47, 5] = 0.47
Parent_Q.3[47, 7] = 6
Parent_Q.3[48, 5] = 1.17
Parent_Q.3[48, 7] = 1.87

#move PACCT/DA answers to PQ_3_AGEOPRH to PQ_3_AGEFOSTER

Parent_Q.3[41, 6] = Parent_Q.3[41, 5]
Parent_Q.3[42, 6] = Parent_Q.3[42, 5]
Parent_Q.3[43, 6] = Parent_Q.3[43, 5]
Parent_Q.3[44, 6] = Parent_Q.3[44, 5]
Parent_Q.3[45, 6] = Parent_Q.3[45, 5]
Parent_Q.3[46, 6] = Parent_Q.3[46, 5]
Parent_Q.3[47, 6] = Parent_Q.3[47, 5]
Parent_Q.3[48, 6] = Parent_Q.3[48, 5]

#delete PACCT/DA answers from PQ_3_AGEORPH
Parent_Q.3[41, 5] = ''
Parent_Q.3[42, 5] = ''
Parent_Q.3[43, 5] = ''
Parent_Q.3[44, 5] = ''
Parent_Q.3[45, 5] = ''
Parent_Q.3[46, 5] = ''
Parent_Q.3[47, 5] = ''
Parent_Q.3[48, 5] = ''

#now for PQ_6A and PQ_7A
Parent_Q.3$PQ_6A_GROCHART<- ifelse(grepl(1, Parent_Q.3$PQ_6A_GROCHART), 1, ifelse(grepl(2, Parent_Q.3$PQ_6A_GROCHART), 0, ""))
Parent_Q.3$PQ_7A_HEALTHCONS<- ifelse(grepl(1, Parent_Q.3$PQ_7A_HEALTHCONS), 1, ifelse(grepl(2, Parent_Q.3$PQ_7A_HEALTHCONS), 0, ""))
Parent_Q.3$PQ_8A_BEHCONS <-  ifelse(grepl(1, Parent_Q.3$PQ_8A_BEHCONS), 0, ifelse(grepl(2, Parent_Q.3$PQ_8A_BEHCONS), 1, ""))
Parent_Q.3$PQ_10A_HEALTHCONS_OVERC <-  ifelse(grepl(1, Parent_Q.3$PQ_10A_HEALTHCONS_OVERC), 0, ifelse(grepl(2, Parent_Q.3$PQ_10A_HEALTHCONS_OVERC), 1, ""))
Parent_Q.3$PQ_11A_BEHACONS_OVERC <-  ifelse(grepl(1, Parent_Q.3$PQ_11A_BEHACONS_OVERC), 0, ifelse(grepl(2, Parent_Q.3$PQ_11A_BEHACONS_OVERC), 1, ""))
Parent_Q.3$PQ_12A_OTHERCHILD <- ifelse(grepl(1, Parent_Q.3$PQ_12A_OTHERCHILD), 1, ifelse(grepl(2, Parent_Q.3$PQ_12A_OTHERCHILD), 0, ""))

#old data had different qualtrics format, once format changed, data was removed. 
#Add it back in. 
#New data will not need this. 
Parent_Q.3$PQ_8A_BEHCONS<- as.numeric(as.character(Parent_Q.3$PQ_8A_BEHCONS))
Parent_Q.3$PQ_10A_HEALTHCONS_OVERC <- as.numeric(as.character(Parent_Q.3$PQ_10A_HEALTHCONS_OVERC))
Parent_Q.3$PQ_11A_BEHACONS_OVERC <- as.numeric(as.character(Parent_Q.3$PQ_11A_BEHACONS_OVERC))

Parent_Q.3 [1,13] = 0 
Parent_Q.3 [2,13] = 0
Parent_Q.3 [3,13] = 1
Parent_Q.3 [4,13] = 0
Parent_Q.3 [5,13] = 1
Parent_Q.3 [6,13] = 1
Parent_Q.3 [7,13] = 0
Parent_Q.3 [8,13] = 1
Parent_Q.3 [9,13] = 0
Parent_Q.3 [10,13] = 1
Parent_Q.3 [11,13] = 0
Parent_Q.3 [13,13] = 0
Parent_Q.3 [13,13] = 1
Parent_Q.3 [14,13] = 0
Parent_Q.3 [15,13] = 1
Parent_Q.3 [16,13] = 1
Parent_Q.3 [17,13] = 1
Parent_Q.3 [18,13] = 1
Parent_Q.3 [19,13] = 0
Parent_Q.3 [20,13] = 1
Parent_Q.3 [21,13] = 0

Parent_Q.3$PQ_8B_EXPLAN <- as.character(Parent_Q.3$PQ_8B_EXPLAN)

Parent_Q.3 [3, 14] = "Just as it relates to depression and anxiety Self-harm  suicidal ideation impulsive reactions to coping rather than thoughtful"
Parent_Q.3 [5, 14] = "ADHD and depression"
Parent_Q.3 [6, 14] = "Some concern about handling anxiety + fear"
Parent_Q.3 [8, 14] = "Impulse control and anger issues"
Parent_Q.3 [10, 14] = "I wish she were more comfortable making friends."
Parent_Q.3 [14, 14] = "ADHD Extreme emotional responses when hungry. Always feeling hungry. Oppositional behaviors"
Parent_Q.3 [15, 14] = "Anxiety and Depression"
Parent_Q.3 [16, 14] = "ADHD behavioral issues"
Parent_Q.3 [17, 14] = "she needs to learn to regulate her emotions better, stop complaining about nothing and learn to be more flexible."
Parent_Q.3 [18, 14] = "anxious attachment"
Parent_Q.3 [20, 14] = "ADHD."

Parent_Q.3 [1,16] = 0
Parent_Q.3 [2,16] = 1
Parent_Q.3 [3,16] = 1
Parent_Q.3 [4,16] = 0
Parent_Q.3 [5,16] = 0
Parent_Q.3 [6,16] = 1
Parent_Q.3 [7,16] = 0
Parent_Q.3 [8,16] = 1
Parent_Q.3 [9,16] = 1
Parent_Q.3 [10,16] = 0
Parent_Q.3 [11,16] = 0
Parent_Q.3 [12,16] = 1
Parent_Q.3 [13,16] = 1
Parent_Q.3 [14,16] = 1
Parent_Q.3 [16,16] = 1
Parent_Q.3 [16,16] = 1
Parent_Q.3 [17,16] = 1
Parent_Q.3 [18,16] = 0
Parent_Q.3 [19,16] = 0
Parent_Q.3 [20,16] = 0
Parent_Q.3 [21,16] = 1

Parent_Q.3$PQ_10B_EXPLAN <- as.character(Parent_Q.3$PQ_10B_EXPLAN)

Parent_Q.3 [2,17] = "left china w/lymphangioma in one cheek, it is hardly apparent now"
Parent_Q.3 [3,17] = "Severe depression and suicidal ideation/attempts, 14 -15 yo
Tourettes- copes with and applies habit reversal therapy techniques
Measles - 10 days after coming to America"
Parent_Q.3 [6,17] = "Anxiety"
Parent_Q.3 [8,17] = "Rickets, anemia, developmentally behind when she came to the US"
Parent_Q.3 [9,17] = "She had Coxsackie virus when she was 8 years old.  It is accompanied by a high fever, we treated her at home, she was a great patient.  Thinking of ways to stay cool.  Got over it in about ten days.  No other unusual health problems."
Parent_Q.3 [12,17] = "Hep B - cleared after a drug trial. ADHD - controlled by medication"
Parent_Q.3 [13,17] = "Overcoming ADHD, Visual processing difficulties, asthma and metabolic issues (pre-diabetes)"
Parent_Q.3 [14,17] = "Asthma and allergies"
Parent_Q.3 [15,17] = "Survived starvation in the orphanage. Survived life-threatening health issues (1) malnutrition, strep throat, thrush, and pneumonia at time of adoptive placement (11 months) - hospitalized in China for four days; (2) Influenza B at age 4 - hospitalized for two days; (3) anxiety/depression/suicidal ideation at age 17 - hospitalized for one week
Continuing to battle anxiety/depression and her chronic GI issues (remains on a feeding tube)"
Parent_Q.3 [17,17] = "had sepsis as an infant in Ethiopia"
Parent_Q.3 [17,17] = "Cleft lip and palate"
Parent_Q.3 [21,17] = "heart defect"

Parent_Q.3 [1,18] = 0
Parent_Q.3 [2,18] = 0
Parent_Q.3 [3,18] = 1
Parent_Q.3 [4,18] = 0
Parent_Q.3 [5,18] = 1
Parent_Q.3 [6,18] = 1
Parent_Q.3 [7,18] = 0
Parent_Q.3 [8,18] = 1
Parent_Q.3 [9,18] = 1
Parent_Q.3 [10,18] = 0
Parent_Q.3 [11,18] = 0
Parent_Q.3 [12,18] = 1
Parent_Q.3 [13,18] = 1
Parent_Q.3 [14,18] = 0
Parent_Q.3 [15,18] = 1
Parent_Q.3 [16,18] = 1
Parent_Q.3 [18,18] = 1
Parent_Q.3 [18,18] = 1
Parent_Q.3 [19,18] = 0
Parent_Q.3 [20,18] = 1
Parent_Q.3 [21,18] = 0

Parent_Q.3$PQ_11B_EXPLAN<- as.character(Parent_Q.3$PQ_11B_EXPLAN)

Parent_Q.3 [3,19] = "Cutting, self-harm"
Parent_Q.3 [5,19] = "Working on it"
Parent_Q.3 [6,19] = "Anxiety"
Parent_Q.3 [8,19] = "She's trying"
Parent_Q.3 [9,19] = "She was reluctant to engage with strangers, and to go to events where there were strangers. Anxious.  Has mostly outgrown it."
Parent_Q.3 [12,19] = "ADHD has caused her to be impulsive and inattentive in school"
Parent_Q.3 [13,19] = "Visual processing has improved with therapy but will continue to be a challenge. All other behavioral concerns are a work in progress"
Parent_Q.3 [15,19] = "Anxiety and Depression"
Parent_Q.3 [16,19] = "learning to manage with ADHD"
Parent_Q.3 [17,19] = "she has gotten much better at controlling her emotions in the last 5-6 years."
Parent_Q.3 [19,19] = "she is working on recognizing her emotions and being mindful, having a healthy response to upsets/difficulties/challenges"
Parent_Q.3 [20,19] = "aggressiveness toward others (pinching, poking, hitting)"

#remove words and into list form 
Parent_Q.3$PQ_12B_AGEOTHERCH<- gsub("([A-z])", "", Parent_Q.3$PQ_12B_AGEOTHERCH)
Parent_Q.3$PQ_12B_AGEOTHERCH<- strsplit(as.character(gsub("([A-z])", "", Parent_Q.3$PQ_12B_AGEOTHERCH)), ",")


#convert to numeric, multiply by 12 to convert to months
for (ii in 1:length(Parent_Q.3$PQ_12B_AGEOTHERCH)){
  Parent_Q.3$PQ_12B_AGEOTHERCH[[ii]]<- as.numeric(Parent_Q.3$PQ_12B_AGEOTHERCH[[ii]])
  Parent_Q.3$PQ_12B_AGEOTHERCH[[ii]]<- Parent_Q.3$PQ_12B_AGEOTHERCH[[ii]]*12
  Parent_Q.3$PQ_12B_AGEOTHERCH[[ii]]<- paste(Parent_Q.3$PQ_12B_AGEOTHERCH[[ii]], collapse=",")} 
#unlist 
Parent_Q.3$PQ_12B_AGEOTHERCH <- unlist(Parent_Q.3$PQ_12B_AGEOTHERCH)

#add back data that was changed to NA
Parent_Q.3[22,21] = "228"   # suppose to be 19 years which is 228 months

#Convert 12c to numeric
#Then add in data that was removed once Qualtrics Questionnaire was edited. 
#New data won't need this. Will already be coded. 
Parent_Q.3$PQ_12C_ADOPTORBIOCH<- as.numeric(as.character(Parent_Q.3$PQ_12C_ADOPTORBIOCH))
Parent_Q.3 [2,22] = 2
Parent_Q.3 [4,22] = 2
Parent_Q.3 [5,22] = 1
Parent_Q.3 [8,22] = 1
Parent_Q.3 [10,22] = 2
Parent_Q.3 [11,22] = 2
Parent_Q.3 [12,22] = 3
Parent_Q.3 [13,22] = 4
Parent_Q.3 [14,22] = 4
Parent_Q.3 [15,22] = 4
Parent_Q.3 [16,22] = 1
Parent_Q.3 [17,22] = 1
Parent_Q.3 [20,22] = 3
Parent_Q.3 [22,22] = 3

#code 14A, 14B, and 14C
Parent_Q.3$PQ_14A_TALKADOSTORY<- ifelse(grepl(1, Parent_Q.3$PQ_14A_TALKADOSTORY), 1, ifelse(grepl(2, Parent_Q.3$PQ_14A_TALKADOSTORY), 0, ""))
Parent_Q.3$PQ_14B_CHENJOYADSTORY<- ifelse(grepl(1, Parent_Q.3$PQ_14B_CHENJOYADSTORY), 1, ifelse(grepl(2, Parent_Q.3$PQ_14B_CHENJOYADSTORY), 0, ""))
Parent_Q.3$PQ_14C_PENJOYADSTORY <-  ifelse(grepl(1, Parent_Q.3$PQ_14C_PENJOYADSTORY), 1, ifelse(grepl(2, Parent_Q.3$PQ_14C_PENJOYADSTORY), 0, ""))

#convert factors to charcters
Parent_Q.3$PQ_5_GENHEALTHADOPT <- as.character(Parent_Q.3$PQ_5_GENHEALTHADOPT )
saveRDS(Parent_Q.3, "/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/Data_Merge/ArchivedData/pq_archive.rds")
# write to csv file on lux
#write.csv(Parent_Q.3, "QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Parent_Questionnaire.csv", row.names = FALSE)
