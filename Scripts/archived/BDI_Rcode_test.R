##Modified R code for BDI analysis
##Lynne Krohn
##10-21-16

### set working directory to location of sub files
setwd("/Volumes/danl/SB/SB_server/Data_Entry/DATA_ENTRY_MASTER_POST_JAN_2017/")
data <- read.csv('J.Data_Entry_Master_12_20_16.csv')
nrow(data)        
ncol(data) 

##################################################
## original syntax in SPSS
#################################################
#first variable: total sum
#COMPUTE BDI_SUM = BDI_1 + BDI_2 + BDI_3 + BDI_4 + BDI_5 + BDI_6 + BDI_7 + BDI_8 + BDI_10 + BDI_11 + BDI_12 + BDI_13 + BDI_14 + BDI_15 + BDI_16 + BDI_17 + BDI_18 + BDI_19 + BDI_20 + BDI_21.
#EXECUTE. 
##################################################
#Sum

l.BDI_TOTAL <- data.frame(data$BDI_1, data$BDI_2, data$BDI_3, data$BDI_4, data$BDI_5, data$BDI_6, data$BDI_7, data$BDI_8,
                        data$BDI_10, data$BDI_11, data$BDI_12, data$BDI_13, data$BDI_14, data$BDI_15, data$BDI_16, data$BDI_17,
                        data$BDI_18, data$BDI_19, data$BDI_20, data$BDI_21)

data$l.BDI_numberNAs <- rowSums(is.na(l.BDI_TOTAL))

data$BDI_total_items <- ncol(l.BDI_TOTAL)
BDI_total_items <- ncol(l.BDI_TOTAL)

data$l.BDI_percentNAs <- (data$l.BDI_numberNAs/BDI_total_items )
head(data$l.BDI_percentNAs)

data$has.BDI <- ifelse(data$l.BDI_percentNAs <= .20, 1, 0)
head(data$has.BDI)

N.has.BDI <- sum(data$has.BDI, na.rm=TRUE)
N.has.BDI #347

data$l.BDI_SUM <- rowMeans(l.BDI_TOTAL, na.rm = TRUE)*20
  
check <- ifelse(data$l.BDI_SUM == data$BDI_SUM, 1, 0)
check ## GOOD

##########################
#next variable: Cognitive Affect
#COMPUTE BDI_COG_AFF_SUM = BDI_1 + BDI_2 + BDI_3 + BDI_4 + BDI_5 + BDI_6 + BDI_7 + BDI_8 + BDI_10 + BDI_11 + BDI_12 + BDI_13 + BDI_14 + BDI_17.
#EXECUTE.
###########################

l.BDI_COG_AFF_TOTAL <- data.frame(data$BDI_1, data$BDI_2, data$BDI_3, data$BDI_4, data$BDI_5, data$BDI_6, data$BDI_7,
                                data$BDI_8, data$BDI_10, data$BDI_11, data$BDI_12, data$BDI_13, data$BDI_14, data$BDI_17)

data$l.BDI_COG_AFF_numberNAs <- rowSums(is.na(l.BDI_COG_AFF_TOTAL))

data$BDI_COG_AFF_total_items <- ncol(l.BDI_COG_AFF_TOTAL)
BDI_COG_AFF_total_items <- ncol(l.BDI_COG_AFF_TOTAL)

data$l.BDI_COG_AFF_percentNAs <- (data$l.BDI_COG_AFF_numberNAs/BDI_COG_AFF_total_items )
head(data$l.BDI_COG_AFF_percentNAs)

data$has.BDI_COG_AFF <- ifelse(data$l.BDI_COG_AFF_percentNAs <= .20, 1, 0)
head(data$has.BDI_COG_AFF)

N.has.BDI_COG_AFF <- sum(data$has.BDI_COG_AFF, na.rm=TRUE)
N.has.BDI_COG_AFF #347

data$l.BDI_COG_AFF_SUM <- rowMeans(l.BDI_COG_AFF_TOTAL, na.rm=TRUE)*14


check <- ifelse(round(data$l.BDI_COG_AFF_SUM,6)==round(data$BDI_COG_AFF_SUM.J,6),1,0)
check ## GOOD

#######################
#third variable: Phsyical / Somatic
#COMPUTE BDI_SOM_SUM=  BDI_15 + BDI_16 + BDI_18 + BDI_19 + BDI_20 + BDI_21.
#EXECUTE.
#######################

l.BDI_SOM_TOTAL <- data.frame(data$BDI_15, data$BDI_16, data$BDI_18, data$BDI_19, data$BDI_20, data$BDI_21)

data$l.BDI_SOM_numberNAs <- rowSums(is.na(l.BDI_SOM_TOTAL))

data$BDI_SOM_total_items <- ncol(l.BDI_SOM_TOTAL)
BDI_SOM_total_items <- ncol(l.BDI_SOM_TOTAL)

data$l.BDI_SOM_percentNAs <- (data$l.BDI_SOM_numberNAs/BDI_SOM_total_items )
head(data$l.BDI_SOM_percentNAs)

data$has.BDI_SOM <- ifelse(data$l.BDI_SOM_percentNAs <= .20, 1, 0)
head(data$has.BDI_SOM)

N.has.BDI_SOM <- sum(data$has.BDI_SOM, na.rm=TRUE)
N.has.BDI_SOM #348

data$l.BDI_SOM_SUM <- rowMeans(l.BDI_SOM_TOTAL, na.rm = TRUE)*6

check <- ifelse(round(data$l.BDI_SOM_SUM,6)==round(data$BDI_SOM_SUM.J,6), 1, 0)
check ## GOOD

