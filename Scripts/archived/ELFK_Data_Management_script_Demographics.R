# ELFK DATA MANAGEMENT SCRIPT 
# last extraction on 7/13/2018
# Made my Michelle Leon

#############################################################################################
# DIRECTIONS
################################################################################################
# CLOSE ALL CSV FILES BEFORE YOU IMPORT INTO R.
# SCRIPT WILL NOT WORK IF ANY OF THESE FILES ARE ALREADY OPEN ON YOUR COMPUTER/LUX.

#############################################################################################
# DEMOGRAPHICs QUESTIONNAIRE 
################################################################################################

# load in qualtrics data
setwd("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/")
Demographics_Q<- read.csv("QUALTRICS_RAW_FOR_R/Qualtrics_Raw_Demographics_Questionnaire_new_7.13.2018.csv")
# open first 6 rows fo data
head(Demographics_Q)
names(Demographics_Q)
nrow(Demographics_Q)
ncol(Demographics_Q)

# get rid of extra columns with random qualtrics info
# [ rows, columns]
Demographics_Q <- Demographics_Q[,8:26]
Demographics_Q.1 <- Demographics_Q[,-c(2,3,4,5,6,7,8,9,10)]
ncol(Demographics_Q.1)
# get rid of extra rows with extra labels
row_num <- nrow(Demographics_Q.1)
Demographics_Q.2 <- Demographics_Q.1[10:row_num,]
nrow(Demographics_Q.2)
ncol(Demographics_Q.2)
#delete any remaining mock data. This always has to be updated with any new mock data. e.g. ID# "999","mock", "test", "ghgh", etc.
Demographics_Q.3 <- Demographics_Q.2[-c(108,146,161, 120,234),]
#fix IDs that are incorrect
Demographics_Q.3$Q1 <-as.character(Demographics_Q.3$Q1)
Demographics_Q.3[8,2] = "EL007"
Demographics_Q.3[33,2] = "EL030"
Demographics_Q.3 [228,2] = "EL092"

#fix any other duplicates in the data or mistakes in IDs
Demographics_Q.4<- Demographics_Q.3[-c(7,39,150,157,174,170,194,59,96,77,87,100,203,209,159,82,147,146,162,195,216,235,237,241,242),]
#fix IDs that are incorrect
Demographics_Q.4 [38,2] = "EL042P" 
Demographics_Q.4 [79,2] = "EL059P" 
Demographics_Q.4 [83,2] = "EL063P" 
Demographics_Q.4 [93,2] = "EL079" 
Demographics_Q.4 [175,2] = "EL125P" 
Demographics_Q.4 [176,2] = "El125"  
Demographics_Q.4 [111,2] = "EL070P" 
Demographics_Q.4 [112,2] = "EL070" 
Demographics_Q.4 [189,2] = "EL124P" 
Demographics_Q.4 [190,2] = "EL124" 
Demographics_Q.4 [99,2] = "EL073P" 
Demographics_Q.4 [97,2] = "EL073"  
Demographics_Q.4 [58,2] = "EL016P" 
Demographics_Q.4 [21,2] = "EL026P" 
Demographics_Q.4 [87,2] = "EL064P" 
Demographics_Q.4 [85,2] = "EL067P" 
Demographics_Q.4 [86,2] = "EL067" 
Demographics_Q.4 [123,2] = "EL072P" 
Demographics_Q.4 [124,2] = "EL072" 
Demographics_Q.4 [96,2] = "EL074" 
Demographics_Q.4 [98,2] = "EL075" 
Demographics_Q.4 [106,2] = "EL076P" 
Demographics_Q.4 [105,2] = "EL077" 
Demographics_Q.4 [95, 2] = "EL078" 
Demographics_Q.4 [121,2] = "EL090P" 
Demographics_Q.4 [125,2] = "EL093P" 
Demographics_Q.4 [126,2] = "EL093" 
Demographics_Q.4 [136,2] = "EL096" 
Demographics_Q.4 [137,2] = "EL096P" 
Demographics_Q.4 [127,2] = "EL091P" 
Demographics_Q.4 [132,2] = "EL095P"
Demographics_Q.4 [143,2] = "EL099P" 

nrow(Demographics_Q.4)

#fix any other duplicates in the data from new extraction. Done here so as to not mess up previous section (columns and rows will change if removed beforehand)
Demographics_Q.4<- Demographics_Q.4[-c(204,205),]

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
# removing characters /letters and x is only numbers
Demographics_Q.4$ID <- str_extract_all(Demographics_Q.4$Q1,"\\(?[0-9,.]+\\)?") # [[1]]

subjects <- Demographics_Q.4$Q1 
first_num <- substr(Demographics_Q.4$Q1, 1,1)

# removing zero if first number
Demographics_Q.4$ID<-as.numeric(ifelse(first_num == "0", substring(Demographics_Q.4$ID, 2), Demographics_Q.4$ID))

# adding EL and zeros as needed
Demographics_Q.4$ID <- ifelse(Demographics_Q.4$ID < 10, paste0("EL00", Demographics_Q.4$ID), ifelse(Demographics_Q.4$ID <100, 
                                                                                      paste0("EL0", Demographics_Q.4$ID), 
                                                                                      ifelse(Demographics_Q.4$ID >= 100, paste0("EL", Demographics_Q.4$ID), NA)))

# only add P for Parent questionnaire! 
Demographics_Q.4$ID <- ifelse(grepl("P", Demographics_Q.4$Q1) | grepl('p', Demographics_Q.4$Q1), paste0(Demographics_Q.4$ID, "P"), Demographics_Q.4$ID)

#compare ID column to Q1 then replace subject with your new subjectid 
Demographics_Q.4$Q1 <- Demographics_Q.4$ID
#remove ID column at end
Demographics_Q.5 <- Demographics_Q.4 [, -c(11)]

# start relabeling columns 
names(Demographics_Q.5)
ncol(Demographics_Q.5)


# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names <- cbind("DEMO_DATE_COMPLETE","IDENT_SUBID", "DEM_1_DATESESS1","DEM_2_DATEBIRTH","DEM_3_GENDER","DEM_4_RACE_CHILD","DEM_4_RACE_OTHER",
                   "DEM_5_ETHNICITY","DEM_6_NUMOLDSIBS", "DEM_7_NUMYOUNGSIBS")

# make this to check column length
ncolvars <- ncol(var_names)
# now change the variable names of our dataframe to match.
names(Demographics_Q.5) <- var_names

#erase one repeated subject that entered data twice
Demographics_Q.5 <- Demographics_Q.5[-c(38),]

#add Pacct subject data
#add overlapping PACCT/ELFK subject data 
DEMO_PACCT<- read.csv("/Volumes/space/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_RAW_FOR_R/PAACT_ELFK_Overlapping_Qs/DEMO_PACCT_ELFK_SUBJECTS.csv")
#view
head(DEMO_PACCT)
names(DEMO_PACCT)
nrow(DEMO_PACCT)
ncol(DEMO_PACCT)

##### reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
subject_pacct <- DEMO_PACCT$IDENT_ID
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
DEMO_PACCT$IDENT_ID <- IDENT_SUBID_pacct

#Edit PACCT subject IDs into ELFK subject IDs
DEMO_PACCT[1,1] = "EL144"
DEMO_PACCT[2,1] = "EL145"
DEMO_PACCT[3,1] = "EL148"
DEMO_PACCT[4,1] = "EL149"
#Add entries for parent demographics
DEMO_PACCT[5,1] = "EL144P"
DEMO_PACCT[6,1] = "EL148P"
DEMO_PACCT[7,1] = "EL149P"

#ADD missing columns
DEMO_PACCT$DEM_4_RACE_CHILD<- NA
DEMO_PACCT$DEM_4_RACE_OTHER<- NA
DEMO_PACCT$DEM_5_ETHNICITY<- NA
DEMO_PACCT$DEMO_DATE_COMPLETE<- NA
DEMO_PACCT$DEM_3_GENDER<- NA
#remove columns not used for ELFK
DEMO_PACCT <- DEMO_PACCT[,-c(7,8)]
#Add paact race categories into single question for elfk
DEMO_PACCT [1,29] = "1"
DEMO_PACCT [2,29] = "1"
DEMO_PACCT [3,29] = "6"
DEMO_PACCT [4,29] = "6"
DEMO_PACCT [5,29] = "6"
DEMO_PACCT [6,29] = "6"
DEMO_PACCT [7,29] = "5"
#now I can erase all separate race categories
DEMO_PACCT <- DEMO_PACCT[,-c(8,9,10,11,12,13,14,18,19,20,21,22,23,24)]
#add pacct ethnicity categories into single question for elfk
DEMO_PACCT [1,17] = "0"
DEMO_PACCT [2,17] = "0"
DEMO_PACCT [3,17] = "0"
DEMO_PACCT [4,17] = "0"
DEMO_PACCT [5,17] = "0"
DEMO_PACCT [6,17] = "0"
DEMO_PACCT [7,17] = "0"
#now delete all ethnicity categories of paact
DEMO_PACCT <- DEMO_PACCT[,-c(8,9,11,12)]
#delete both pacct gender categories. Gender, Session 2 date, subject ages  will be added when merged with SUBID key.
DEMO_PACCT <- DEMO_PACCT[,-c(3,5,6,7,8)]


# naming variables to match SPSS data entry master. 
# check with data entry manual for labels!! 
# its ok if they are out of order at this step.
# remember to put quotes around variable names! 
var_names_pacct <- cbind("IDENT_SUBID","DEM_1_DATESESS1","DEM_2_DATEBIRTH","DEM_6_NUMOLDSIBS","DEM_7_NUMYOUNGSIBS",
                         "DEM_4_RACE_CHILD","DEM_4_RACE_OTHER","DEM_5_ETHNICITY","DEMO_DATE_COMPLETE","DEM_3_GENDER")

# make this to check column length
ncolvars_pacct<-ncol(var_names_pacct)
# now change the variable names of our dataframe to match.
names(DEMO_PACCT) <- var_names_pacct


#put columns in right order 
DEMO_PACCT <- DEMO_PACCT[c("DEMO_DATE_COMPLETE","IDENT_SUBID", "DEM_1_DATESESS1","DEM_2_DATEBIRTH","DEM_3_GENDER","DEM_4_RACE_CHILD","DEM_4_RACE_OTHER",
                   "DEM_5_ETHNICITY","DEM_6_NUMOLDSIBS","DEM_7_NUMYOUNGSIBS")]

# get subjectIDS for each dataframe.
PACCT_list <- DEMO_PACCT$IDENT_SUBID
ELFK_list <- Demographics_Q.5$IDENT_SUBID

# check that col names are identical before merging!! 
identical(names(DEMO_PACCT), names(Demographics_Q.5))

# add pacct data into DEMOGRAPHICS_Q.5
DEMO_merged <- rbind(DEMO_PACCT,Demographics_Q.5)

# make sure all subjects were merged together successfully!! 
identical(nrow(DEMO_merged), nrow(DEMO_PACCT)+nrow(Demographics_Q.5))
Demographics_Q.5 <- DEMO_merged


# write to csv file on lux
write.csv(Demographics_Q.5, "QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Demographics_Questionnaire_notfinal.csv")



#merge this data with copy of demo subid copy to edit session dates, dob, gender, subject ages
DemoQ <- read.csv("QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Demographics_Questionnaire_notfinal.csv")
SubidQ <- read.csv("ELFK_Demo_7_13_2018.csv") 
#remove first column on demoQ
DemoQ <-DemoQ[,-c(1) ] 
#change name of Id column for subidQ
names(SubidQ)[1] <- "IDENT_SUBID"

# reformat the subject ID to be standard
# use dollar sign to get one column from dataframe
library(stringr)
# removing characters /letters and x is only numbers
SubidQ$newid <- str_extract_all(SubidQ$IDENT_SUBID,"\\(?[0-9,.]+\\)?") # [[1]]

subjects <- SubidQ$IDENT_SUBID 
first_num <- substr(SubidQ$IDENT_SUBID, 1,1)

# removing zero if first number
SubidQ$newid<-as.numeric(ifelse(first_num == "0", substring(SubidQ$newid, 2), SubidQ$newid))

# adding EL and zeros as needed
SubidQ$newid <- ifelse(SubidQ$newid < 10, paste0("EL00", SubidQ$newid), ifelse(SubidQ$newid <100, 
                                                                                                    paste0("EL0", SubidQ$newid), 
                                                                                                    ifelse(SubidQ$newid >= 100, paste0("EL", SubidQ$newid), NA)))

# only add P for Parent questionnaire! 
SubidQ$newid <- ifelse(grepl("P", SubidQ$IDENT_SUBID) | grepl('p', SubidQ$IDENT_SUBID), paste0(SubidQ$newid, "P"), SubidQ$newid)

#compare ID column to Q1 then replace subject with your new subjectid 
SubidQ$IDENT_SUBID <- SubidQ$newid
#remove ID column at end
SubidQ <- SubidQ [, -c(9)]

#MERGE INTO ONE VARIABLE
#if you add new questionnaire scripts, add to this.
df_merged <-merge(DemoQ, SubidQ, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged)
ncol(df_merged)

#replace session 1 date, dob, and sex
df_merged$DEM_1_DATESESS1 <- df_merged$Behav.Date
df_merged$DEM_2_DATEBIRTH <- df_merged$DOB
df_merged$DEM_3_GENDER <- df_merged$sex
#remove extra columns
df_merged <- df_merged[,-c(12,13,14)]

#change variable names
names(df_merged)
var_names2<- cbind("IDENT_SUBID","DEMO_DATE_COMPLETE","DEM_1_DATESESS1","DEM_2_DATEBIRTH","DEM_3_GENDER","DEM_4_RACE_CHILD","DEM_4_RACE_OTHER",
                              "DEM_5_ETHNICITY","DEM_6_NUMOLDSIBS", "DEM_7_NUMYOUNGSIBS", "IDENT_SUBTYPE", "SUBAGE_Session1",
                   "DEM_1_DATESESS2", "SUBAGE_Session2")
ncolvars2 <-ncol(var_names2)
names(df_merged) <-var_names2

#ADD column not included 
df_merged$DEM_5_ETHNICITY_OTHER <- NA

#reorder columns to match Manual
df_merged <- df_merged[c("DEMO_DATE_COMPLETE", "IDENT_SUBID","DEM_1_DATESESS1","DEM_1_DATESESS2","DEM_2_DATEBIRTH","IDENT_SUBTYPE","SUBAGE_Session1","SUBAGE_Session2",
                         "DEM_3_GENDER", "DEM_4_RACE_CHILD","DEM_4_RACE_OTHER", "DEM_5_ETHNICITY", "DEM_5_ETHNICITY_OTHER", "DEM_6_NUMOLDSIBS", 
                         "DEM_7_NUMYOUNGSIBS")]

#make copy to check change in date format
df_merged1 <-df_merged
#convert to standardized date format
library(lubridate)
df_merged1$DEM_1_DATESESS1 <- mdy(df_merged1$DEM_1_DATESESS1)
df_merged1$DEM_1_DATESESS2 <- mdy(df_merged1$DEM_1_DATESESS2)
df_merged1$DEM_2_DATEBIRTH <- mdy(df_merged$DEM_2_DATEBIRTH)

df_merged1$testdob <- df_merged1$DEM_2_DATEBIRTH

#year(df_merged1$DEM_1_DATESESS1) = ifelse(year(df_merged1$DEM_1_DATESESS1) >= 2019, year(df_merged1$DEM_1_DATESESS1)-100, year(df_merged1$DEM_1_DATESESS1))
#This step is to change wrong dates e.g. 2048 should be 1948. 
year(df_merged1$DEM_2_DATEBIRTH) = ifelse(year(df_merged1$DEM_2_DATEBIRTH) >= 2019, year(df_merged1$DEM_2_DATEBIRTH)-100, year(df_merged1$DEM_2_DATEBIRTH))
#parse dates to proper format for data entry master
df_merged1$newdob <- paste0(month(df_merged1$DEM_2_DATEBIRTH), '/', day(df_merged1$DEM_2_DATEBIRTH), '/', year(df_merged1$DEM_2_DATEBIRTH))
#put new dob format into dfmerged
df_merged$DEM_2_DATEBIRTH <- df_merged1$newdob

#same process for date of behav visit and mri visit
df_merged1$newdem1date <- paste0(month(df_merged1$DEM_1_DATESESS1), '/', day(df_merged1$DEM_1_DATESESS1), '/', year(df_merged1$DEM_1_DATESESS1))
df_merged1$newdem2date <- paste0(month(df_merged1$DEM_1_DATESESS2), '/', day(df_merged1$DEM_1_DATESESS2), '/', year(df_merged1$DEM_1_DATESESS2))

df_merged$DEM_1_DATESESS1 <- df_merged1$newdem1date
df_merged$DEM_1_DATESESS2 <- df_merged1$newdem2date

#code gender question
df_merged$DEM_3_GENDER <- ifelse(grepl("m", df_merged$DEM_3_GENDER),0, ifelse(grepl("f", df_merged$DEM_3_GENDER),1 ,""))

# Fix codes for race questionaire - multiple races should be coded as an 8 and then the specific race categories
df_merged$DEM_4_RACE_CHILD <- as.character(df_merged$DEM_4_RACE_CHILD)
df_merged$DEM_4_RACE_OTHER <- as.character(df_merged$DEM_4_RACE_OTHER)

df_merged [26,10] = "8"
df_merged [26,11] = "African American and Hispanic"
df_merged [28,10] = "8"
df_merged [28,11] = "African American and Hispanic"
df_merged [32,10] = "7"
df_merged [33,10] = "7"
df_merged [50,10] = "8"
df_merged [50,11] = "African American and Hispanic"
df_merged [56,10] = "8"
df_merged [56,11] = "African American, American Indian, Asian American and White"
df_merged [60,10] = "8"
df_merged [60,11] = "Asian American and White"
df_merged [61,10] = "8"
df_merged [61,11] = "Asian American and White"
df_merged [62,10] = "8"
df_merged [62,11] = "Asian American and White"
df_merged [63,10] = "8"
df_merged [63,11] = "Asian American and White"
df_merged [64,10] = "8"
df_merged [64,11] = "Asian American and White"
df_merged [70,10] = "8"
df_merged [70,11] = "African American, American Indian, and White"
df_merged [71,10] = "8"
df_merged [71,11] = "American and American Indian"
df_merged [85,10] = "8"
df_merged [85,11] = "African American and Hispanic"
df_merged [95,10] = "8"
df_merged [95,11] = "African American, American Indian, Asian American, Hispanic, and White"
df_merged [97,10] = "8"
df_merged [97,11] = "African American, American Indian, Asian American, Hispanic, and White"
df_merged [107,10] = "8"
df_merged [107,11] = "African American and Hispanic"
df_merged [130,10] = "8"
df_merged [130,11] = "Hispanic and White"
df_merged [131,10] = "8"
df_merged [131,11] = "Hispanic and White"
df_merged [140,10] = "8"
df_merged [140,11] = "African American and Hispanic"
df_merged [152,10] = "8"
df_merged [152,11] = "Asian American and Native Hawaiian"
df_merged [184,10] = "8"
df_merged [184,11] = "Asian American and Native Hawaiian"

#make copy to check coding was done correctly
df_merged_copyrace_ethnicity <- df_merged

#Code RACE based on ELFK DATA ENTRY MANUAL
df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD<- ifelse(grepl("African American", df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 1, 
                                      ifelse(grepl("American Indian", df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 2, 
                                      ifelse(grepl("Asian-American", df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 3,
                                      ifelse(grepl("White", df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 6,
                                      ifelse(grepl("Hispanic", df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 4,
                                      ifelse(grepl("Native Hawaiian", df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 5,
                                      ifelse(grepl("Other", df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 7,
                                      ifelse(grepl(8, df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 8,""))))))))
#add entries that were removed
df_merged_copyrace_ethnicity[32,10] = 7
df_merged_copyrace_ethnicity[33,10] = 7
df_merged_copyrace_ethnicity[219,10] = 1
df_merged_copyrace_ethnicity[220,10] = 6
df_merged_copyrace_ethnicity[221,10] = 1
df_merged_copyrace_ethnicity[222,10] = 6 
df_merged_copyrace_ethnicity[223,10] = 6
df_merged_copyrace_ethnicity[224,10] = 6
df_merged_copyrace_ethnicity[225,10] = 5

#If subject chose 4 (hispanic) for Demo_4_RACE then they should have a 1 for DEM_5_ETHNICITY
df_merged_copyrace_ethnicity$DEM_5_ETHNICITY<-ifelse(grepl(4, df_merged_copyrace_ethnicity$DEM_4_RACE_CHILD), 1,
                                              ifelse(grepl("Not", df_merged_copyrace_ethnicity$DEM_5_ETHNICITY), 0,
                                              ifelse(grepl("Hispanic", df_merged_copyrace_ethnicity$DEM_5_ETHNICITY), 1,"")))

#add entries that were removed
df_merged_copyrace_ethnicity[219,12] = 0
df_merged_copyrace_ethnicity[220,12] = 0
df_merged_copyrace_ethnicity[221,12] = 0
df_merged_copyrace_ethnicity[222,12] = 0 
df_merged_copyrace_ethnicity[223,12] = 0
df_merged_copyrace_ethnicity[224,12] = 0
df_merged_copyrace_ethnicity[225,12] = 0

#make these edits to df_merged
df_merged <- df_merged_copyrace_ethnicity

#add new column for number of deceased sibs - discussed with Bridget on 5/3/18
df_merged$DEM_8_NUMDECEASEDSIBS <- NA

df_merged [63,14] = "2"
df_merged [190,14] = "1"
df_merged [190,16] = "1"
df_merged [201,14] = "2"
df_merged [201,15] = "1"

df_merged$DEM_6_NUMOLDSIBS <- as.character(df_merged$DEM_6_NUMOLDSIBS)
df_merged$DEM_6_NUMOLDSIBS <- as.numeric(df_merged$DEM_6_NUMOLDSIBS)

df_merged$DEM_7_NUMYOUNGSIBS <- as.character(df_merged$DEM_7_NUMYOUNGSIBS)
df_merged$DEM_7_NUMYOUNGSIBS <- as.numeric(df_merged$DEM_7_NUMYOUNGSIBS)

#Fill in misssing dates for sessions/dob that were removed when demographic Qs were merged with SUBID
df_merged[20,3] ="10/14/2015"
df_merged[20,5] ="10/27/1961"
df_merged[81,5] ="7/7/1974"
df_merged[83,3] = "7/21/2016"
df_merged[83,5] = "6/16/1973"
df_merged[104,3] = "9/12/2016"
df_merged[104,5] = "5/16/1986"
df_merged[176,3] = "12/11/2017"
df_merged[176,5] = "8/30/1970"
df_merged[189,5] = "5/7/1964" 
df_merged[206,3] = "4/21/2018"
df_merged[206,5] = "5/18/1963"
df_merged[211,5] = "4/7/1965"
df_merged[216,3] = "6/13/2018"
df_merged[216,5] = "7/24/1959"
df_merged[219,3] = "4/3/2018"
df_merged[221,3] = "4/3/2018"
df_merged[222,3] = "4/4/2018"


# write to csv file on elvis
write.csv(df_merged, "QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_DEM_Questionnaire_FINAL.csv", row.names = FALSE)
