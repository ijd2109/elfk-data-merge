#SEARCH FOR DUPLICATES IN LIST OF USER-ENTERED SUBJECT IDs

test<-mutate(df1, 
             Q3 = as.integer(str_extract(df1$Q3,pattern ="\\(?[0-9,.]+\\)?"))) %>%
  select(Q3)
#then define "dup"
dup <- which(duplicated(test$Q3))

#create a side-by-side comparison (2-column matrix with the duplicates)
duplicates <- matrix(cbind(rep(NA_character_,times=length(dup)),rep(NA_character_,times=length(dup)),
                           rep(NA_character_,times=length(dup)),rep(NA_character_,times=length(dup)),
                           rep(NA_character_,times=length(dup))), 
                     ncol=5)
#now fill it with your values that were duplicated
for (i in 1:length(dup)) {
  duplicates[i,] <- c(rep(NA_character_, times=ncol(duplicates)-length(which(test$Q3==test$Q3[dup[i]]))),
                      df1$Q3[which(test$Q3==test$Q3[dup[i]])])
}
