#Sean Minns
# 11.16.2018
#updated by MVT Jan 2019
#Script for merging child only and parents only data based on family codes
# install and load tidyverse
library(tidyverse)
# Load child only data with family codes
ChildWithFamilyCodeData <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_Cleaned_ChildWithFamilyCode.csv", stringsAsFactors = F)
ChildWithFamilyCodeData$Age <- as.character(ChildWithFamilyCodeData$Age)
# Load parent only data with family codes
AdultWithFamilyCodeData <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_Cleaned_AdultWithFamilyCode.csv",stringsAsFactors = F)
# Join Child only data with parent only data based on the family codes
ParentChild_FamilyCode <- left_join(ChildWithFamilyCodeData, AdultWithFamilyCodeData,
                                    by = c("Family_code", "Group..0.control..1.PI.", "sex", "DOB", "Behav.Date", "Age", "MRI.Date", "mri.age"), all = T)

# MVT adding code to remove .x and .y variables
ParentChild_FamilyCode2 <- ParentChild_FamilyCode %>%
  select(-starts_with("Tricia_orig_code"), -IDENT_SUBID.y)
names(ParentChild_FamilyCode2)[1] <- "IDENT_SUBID" # get rid of .x

# export as csv
write.csv(ParentChild_FamilyCode2, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_Cleaned_TotalDataWithFamilyCode.csv", row.names = F)
