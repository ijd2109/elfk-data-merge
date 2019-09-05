
#Script for merging questionnaire files from qualtrics to make a master 
# Michelle VanTieghem

### 1. Merge cleaned qualtrics data
#add all questionnaires you want to merge
#add any new questionnaire scripts 
q1 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_AIOI_Questionnaire.csv")
q2 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Caregiving_Questionnaire.csv")
q3 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_CRPR_Questionnaire.csv")
q4 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_GRIT_Questionnaire.csv")
q5 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Parent_STAI_STATE_Questionnaire.csv")
q6 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Parent_STAI_TRAIT_Questionnaire.csv")
q7 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Peterson_Female_Questionnaire_new.csv")
q8 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Peterson_Male_Questionnaire_new.csv")
q9 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_SCARED_Questionnaire.csv")
q10 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_SecurityScale_Questionnaire.csv")
q11 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_DSM_Questionnaire.csv")
q12 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_HigginsMotivation_Questionnaire.csv")
q13 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_IF_Questionnaire.csv")
q14 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Relationship_Problems_Questionnaire.csv")
q15 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_RADS_Questionnaire.csv")
q16 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_DSMPIParents_Questionnaire.csv")
q17 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Friendship_Questionnaire.csv")
q18 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Adoption_Questionnaire.csv")
q19 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_LEQ_Questionnaire.csv")
q20 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_DEM_Questionnaire_FINAL.csv")
q21 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/Qualtrics_Cleaned_in_R_Parent_GRIT_Questionnaire.csv")
## later put BDI-1A and BDI2 here.
#q22 <-
# q23

#MERGE INTO ONE VARIABLE
#if you add new questionnaire scripts, add to this.
#df_merged1 <-merge(q1, q2, by = "IDENT_SUBID", all = TRUE)
#nrow(df_merged1)
df_merged1 <- merge(q1,q2, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged1)
df_merged2 <- merge(df_merged1,q3, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged2)
df_merged3 <- merge(df_merged2, q4, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged3)
df_merged4 <- merge(df_merged3, q5, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged4)
df_merged5 <- merge(df_merged4, q6, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged5)
df_merged6 <- merge(df_merged5, q7, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged6)
df_merged7 <- merge(df_merged6, q8, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged7)
df_merged8 <- merge(df_merged7, q9, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged8)
df_merged9 <- merge(df_merged8, q10, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged9)
df_merged10 <- merge(df_merged9, q11, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged10)
df_merged11 <- merge(df_merged10, q12, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged11)
df_merged12 <- merge(df_merged11, q13, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged12)
df_merged13 <- merge(df_merged12, q14, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged13)
df_merged14 <- merge(df_merged13, q15, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged14)
df_merged15 <- merge(df_merged14, q16, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged15)
df_merged16 <- merge(df_merged15, q17, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged16)
df_merged17 <- merge(df_merged16, q18, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged17)
df_merged18 <- merge(df_merged17, q19, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged18)
df_merged19 <- merge(df_merged18, q20, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged19)
df_merged20 <- merge(df_merged19, q21, by = "IDENT_SUBID", all = TRUE)
nrow(df_merged20)


#Save output
write.csv(df_merged20, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_merged_r_2019-01-22.csv", row.names =  FALSE)

#### 2. Separate parent & child data
#Sepertate long data sets into parent and child datasets

library(tidyverse)
# Import master data
Parent_And_Child <- df_merged20
# Create a data frame that only contains the parents
ParentOnly<-Parent_And_Child[grep("P", Parent_And_Child$IDENT_SUBID), ]
# remove all the kids questions
ParentOnlyWithNoKidsQuestionaires <- ParentOnly %>% 
  dplyr::select(grep("IDENT_SUBID", names(ParentOnly)), 
                 grep("^BDI", names(ParentOnly)),
                 grep("^AIOI", names(ParentOnly)), 
                 grep("^PARENT_STAI", names(ParentOnly)), 
                 grep("^PARENT_GRIT", names(ParentOnly)))
names(ParentOnlyWithNoKidsQuestionaires)
#Export as CSV file
write.csv(ParentOnlyWithNoKidsQuestionaires, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_AdultOnly.csv", row.names = F)

# Create a data frame that only contains the kids
KidsOnly<-Parent_And_Child[-grep("P", Parent_And_Child$IDENT_SUBID), ]
# remove all the parents questions
KidsOnlyWithNoParentsQuestionaires <- KidsOnly %>% dplyr:: select(-grep("^BDI", names(KidsOnly)), -grep("^AIOI", names(KidsOnly)), -grep("^PARENT_STAI", names(KidsOnly)), -grep("^PARENT_GRIT", names(KidsOnly)))
names(KidsOnlyWithNoParentsQuestionaires)
#Export as CSV file
write.csv(KidsOnlyWithNoParentsQuestionaires, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_ChildOnly.csv", row.names = F)


### 3. Child Add famlily code 
# add family code to child data
ChildOnly <- KidsOnlyWithNoParentsQuestionaires
# Import Family code data
FamilyCode <- read.csv("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/ELFK_Demo_11_14_2018.csv")
# Rename Subject.ID to IDENT_SUBID for FamilyCode
colnames(FamilyCode)[1]<-"IDENT_SUBID"
# Merge child and family code data
ChildWithFamilyCode <-merge(ChildOnly, FamilyCode, by = "IDENT_SUBID", all = FALSE)
# Export as CSV
write.csv(ChildWithFamilyCode, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_Cleaned_ChildWithFamilyCode.csv", row.names = F)

### 4. Adult add family code

# Import adult only data
AdultOnly <- ParentOnlyWithNoKidsQuestionaires
# Import Family code data
FamilyCode <- read.csv("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/ELFK_Demo_11_14_2018.csv")
# Rename Subject.ID to IDENT_SUBID for FamilyCode
colnames(FamilyCode)[1]<-"IDENT_SUBID"
# Merge Adult and family code data
AdultWithFamilyCode <-merge(AdultOnly, FamilyCode, by = "IDENT_SUBID", all = FALSE)
# Export as CSV
write.csv(AdultWithFamilyCode, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_Cleaned_AdultWithFamilyCode.csv", row.names = F)


#### 5. Merge Data by Family code.
#Script for merging child only and parents only data based on family codes
# install and load tidyverse
library(tidyverse)
# Load child only data with family codes

ChildWithFamilyCode$Age <- as.character(ChildWithFamilyCode$Age)
AdultWithFamilyCode$Age <- as.character(AdultWithFamilyCode$Age)
# Load parent only data with family codes
# Join Child only data with parent only data based on the family codes
ParentChild_FamilyCode <- left_join(ChildWithFamilyCode, AdultWithFamilyCode,
                                    by = c("Family_code"), all = T) #, "Group..0.control..1.PI.", "sex", "DOB", "Behav.Date", "Age", "MRI.Date", "mri.age"), all = T)

# MVT adding code to remove .x and .y variables
ParentChild_FamilyCode2 <- ParentChild_FamilyCode %>%
  select(-starts_with("Tricia_orig_code"), -ends_with(".y"))

# fix up variable names that were accidentally duplicated.
names(ParentChild_FamilyCode2)[1] <- "IDENT_SUBID" # get rid of .x
names(ParentChild_FamilyCode2)[names(ParentChild_FamilyCode2) == "Behav.Date.x"] <- "Behav.Date"
names(ParentChild_FamilyCode2)[names(ParentChild_FamilyCode2) == "DOB.x"] <- "DOB"
names(ParentChild_FamilyCode2)[names(ParentChild_FamilyCode2) == "Group..0.control..1.PI..x"] <- "GROUP"
names(ParentChild_FamilyCode2)[names(ParentChild_FamilyCode2) == "Age.x"] <- "Age"
names(ParentChild_FamilyCode2)[names(ParentChild_FamilyCode2) == "sex.x"] <- "sex"
names(ParentChild_FamilyCode2)[names(ParentChild_FamilyCode2) == "MRI.Date.x"] <- "MRI.Date"
names(ParentChild_FamilyCode2)[names(ParentChild_FamilyCode2) == "mri.age.x"] <- "MRI.Age"



# export as csv
write.csv(ParentChild_FamilyCode2, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_Cleaned_TotalDataWithFamilyCode.csv", row.names = F)
