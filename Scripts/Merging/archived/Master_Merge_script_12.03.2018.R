#Sean Minns Nov 30 2018
#Merge master data sheet with non-qualtrics data
#load up master data sheet

# checked MVT Jan 2019
library("readxl")
nq1<- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/QUALTRICS_CLEANED_IN_R/ELFK_Qualtrics_Cleaned_TotalDataWithFamilyCode.csv")
#Load other data to add
nq2 <- read.csv("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/FAS_coding_data_COMPLETE.csv")
nq3 <- read_excel("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/Post_scan_debrief.xlsx", sheet = "Responses")
nq4 <- read_excel("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/WASI_complete.xlsx")
nq5 <- read_excel("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/4_17_MedHealth_Extraction.xlsx", sheet="Data")
nq6 <- read.csv("/Volumes/danl/ELFK/Data_Entry/Questionnaires/R_Cleaning_Qualtrics_Data/CBCL_CLEANED/CBCL_scales_and_indiv_items_cleaned_MVT_12_2_2018.csv")
nq7 <- read.csv("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/saliva_time_date_long.csv")
nq8 <- read_excel("/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/Finalized_copies_to_be_merged/Weight_from_MRI_completed_checked.xlsx")
#and find more from DATA_ENTRY_MASTER > finalized_to_be_merged
#check to see if the variable names are the same.

#MERGE INTO ONE VARIABLE=
ndf_merged1 <-merge(nq1, nq2, by = "IDENT_SUBID", all = TRUE)
nrow(ndf_merged1)
ndf_merged2 <- merge(ndf_merged1, nq3, by = "IDENT_SUBID", all = TRUE)
nrow(ndf_merged2)
ndf_merged3 <- merge(ndf_merged2, nq4, by = "IDENT_SUBID", all = TRUE)
nrow(ndf_merged3)
ndf_merged4 <- merge(ndf_merged3, nq5, by = "IDENT_SUBID", all = TRUE)
nrow(ndf_merged4)
ndf_merged5 <- merge(ndf_merged4, nq6, by = "IDENT_SUBID", all = TRUE)
nrow(ndf_merged5)
ndf_merged6 <- merge(ndf_merged5, nq7, by = "IDENT_SUBID", all = TRUE)
nrow(ndf_merged5)
ndf_merged7 <- merge(ndf_merged6, nq8, by = "IDENT_SUBID", all = TRUE)


#recode the medhealth -8 as NAs
ndf_merged5[ndf_merged5==-8]<-NA
ndf_merged5[ndf_merged5=="-8"]<-NA


#write to csv
write.csv(ndf_merged5, "/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/R_Merge_Master_MVT/merged_master/ELFK_FINAL_merged_r_MVT_2019-01-19.csv", row.names =  FALSE)

