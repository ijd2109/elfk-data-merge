#########################
#Produced by Ian Douglas#
########################
# In order to code ELFK subject ID's *from qualtrics*
# in the format: "EL###"

#df <- as.data.frame(data)
# Qualtrics forms for ELFK typtically output `Q3`
# as the subject ID variable
df2 <- df %>%
  # extract id NUMBERS only
  mutate(Q3 = as.integer(str_extract(df$Q3, 
                                     pattern ="\\(?[0-9,.]+\\)?")))%>%
  # add prefix "EL00", "EL0", or "EL" as necessary; rename:
  mutate(IDENT_SUBID = paste0(ifelse(nchar(Q3)==1,"EL00",
                                     ifelse(nchar(Q3)==2,"EL0",
                                            "EL")), Q3)) %>%
  #optional step to reorder as: [Subject ID's], [other variables], [RecordedDate]
  select(IDENT_SUBID, Q2_1:Q2_41, RecordedDate)