#loading entire PACCT dataset
#location at extraction on 3/27/19:
# "/Volumes/danl/PACCT/data_entry/wave1_data/mini_masters/Nim_merge_20190227.xlsx"
pacct <- read_sav(file.choose())
#extract 6 participants from ELFK
pacct2 <- pacct %>% filter(IDENT_SUBID == "PA051_V1" | 
                             IDENT_SUBID == "PA052_V1" | 
                             IDENT_SUBID == "PA058_V1" | 
                             IDENT_SUBID == "PA059_V1" | 
                             IDENT_SUBID == "PA061_V1" | 
                             IDENT_SUBID == "PA109_V1")

write.csv(pacct2,"/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/Data_Merge/Raw/PACCT/pacct.csv")

#recode subject id's
pacct2 <-mutate(pacct2, IDENT_SUBID = recode(IDENT_SUBID, 
                                      PA051_V1 = "EL144",
                                      PA052_V1 = "EL145",
                                      PA058_V1 = "EL146",
                                      PA059_V1 = "EL147",
                                      PA061_V1 = "EL148",
                                      PA109_V1 = "EL149"))

#variable selection (example with the Security Scale)
# pacct_SS <- pacct2 %>% select(DEM_1_DATESESS1,
#                               IDENT_SUBID, 
#                               starts_with("SS_"))

write.csv(pacct2,"/Volumes/danl/ELFK/Data_Entry/DATA_ENTRY_MASTER/Data_Merge/Raw/PACCT/pacct.csv")
