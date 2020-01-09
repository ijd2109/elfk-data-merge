# 11.15.2018
#Sepertate long data sets into parent and child datasets
# Sean Minns
# load tidy verse
library(tidyverse)
# Import master data
Parent_And_Child <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_merged_r_2019-01-22.csv")
# Create a data frame that only contains the parents
ParentOnly<-Parent_And_Child[grep("P", Parent_And_Child$IDENT_SUBID), ]
# remove all the kids questions
ParentOnlyWithNoKidsQuestionaires <- ParentOnly %>% dplyr:: select(grep("IDENT_SUBID", names(ParentOnly)), grep("^BDI", names(ParentOnly)), grep("^AIOI", names(ParentOnly)), grep("^PARENT_STAI", names(ParentOnly)), grep("^PARENT_GRIT", names(ParentOnly)))
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

