#11.15.2018
# Sean Minns
# Import adult only data
AdultOnly <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_AdultOnly.csv")
# Import Family code data
FamilyCode <- read.csv("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/ELFK_Demo_11_14_2018.csv")
# Rename Subject.ID to IDENT_SUBID for FamilyCode
colnames(FamilyCode)[1]<-"IDENT_SUBID"
# Merge Adult and family code data
AdultWithFamilyCode <-merge(AdultOnly, FamilyCode, by = "IDENT_SUBID", all = FALSE)
# Export as CSV
write.csv(AdultWithFamilyCode, "/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_Cleaned_AdultWithFamilyCode.csv", row.names = F)
