#####################################################################
#If values in a vector are repeated at least once, return the index # 
# of the original, and that of all repeats.                         # 
#####################################################################
#-------------------------------------------------------------------#
index.repeats <- function(vector) {
  original <- vector
  index_of_rep <- which(duplicated(original))
  unique.vals <- unique(original[index_of_rep])
  index.out <- c()
  for (i in seq_along(unique.vals)) {
    index.out <- c(index.out,which(original == unique.vals[i]))
  }
  return(index.out)
}

#####################################################################
#Extract, from a vector of values, the entire value that contains a # 
# given regular expression. Like grep() or grepl(), but returns     #
# the value, not its index or TRUE/FALSE indicator                  # 
#####################################################################
#-------------------------------------------------------------------#
extr <- function(value, vector) {
  out <- vector[grep(value,vector)]
  return(out)
}

############################################################
# id.search()                                              #
#----------------------------------------------------------#
# Run a search for all duplicates in the subject ID field  #
# -------------------------------------------------------- #
# (1) Supply the name of the dataframe,                    #
# (2) Supply the name of the subject ID variable           #
# (3) Supply the name of the date or timestamp column      #
#*Note, the third column can be added for any additional   #
#*info you want to print along side the duplicated entries #
#*and was added in order to facilitate the manual search   #
#*for the desired duplicate that would be retained by      #
#*further data cleaning.                                   #
#----------------------------------------------------------#
#***Returns a tibble                                       #
############################################################

id.search<-function(data, id.variable.name, timestamp.var.name) {
  id.varname <- paste0(c("^", id.variable.name,"$"),collapse="") #find exact
  time.varname <- paste0(c("^", timestamp.var.name,"$"),collapse="") #find exact
  raw.id <- data[,grep(id.varname, names(data))]
  time.stamps <- data[,grep(time.varname, names(data))]
  digits<<- as.integer(str_extract(raw.id, pattern ="\\(?[0-9,.]+\\)?"))
  dup <<- which(duplicated(digits))
  unique.dup <<- unique(digits[dup])
  fill <- matrix(rep(NA, times = length(unique.dup)*5),ncol = 5)
  times <- matrix(rep(NA, times = length(unique.dup)*5),ncol = 5)
  if (length(unique.dup)==0) stop("There are no duplicates")
  for (i in 1:length(unique.dup)) {
    num.cases <- length(which(digits==unique.dup[i]))
    all.raw.cases <- raw.id[which(digits==unique.dup[i])]
    all.timestamps <- time.stamps[which(digits==unique.dup[i])]
    for (j in 1:num.cases) {
      fill[i,j] <- all.raw.cases[j]
      times[i,j] <- all.timestamps[j]
    }
  }
  ident.subid <- paste0(ifelse(nchar(unique.dup)==1,
                               "EL00",ifelse(nchar(unique.dup)==2,
                                             "EL0","EL")),unique.dup)
  search.matrix <- cbind(unique.dup,ident.subid, 
                         fill[,1],times[,1],
                         fill[,2],times[,2],
                         fill[,3],times[,3],
                         fill[,4],times[,4],
                         fill[,5],times[,5])
  colnames(search.matrix) <- c("digits.scraped","ID.generated",
                               "Raw.entry.1","Timestamp.1",
                               "Raw.entry.2","Timestamp.2",
                               "Raw.entry.3","Timestamp.3",
                               "Raw.entry.4","Timestamp.4",
                               "Raw.entry.5","Timestamp.5")
  return(as_tibble(search.matrix))
}

#########################################
# ELFK.identsubid()                     #
#---------------------------------------#
#Standardize subject ID numbers for ELFK#
#---------------------------------------#
#***Returns a new dataframe!            #
#########################################

ELFK.identsubid <- function(data, id.variable.name) {
  if (class(id.variable.name) != "character") {
    stop("variable names must be character strings")
  } else {
    i <- which(names(data)==id.variable.name)
    tmp.df <- as.character(data[,i])
    tmp.df2<- data %>% 
      mutate_at(i, 
                function(x) as.integer(str_extract(x,pattern ="\\(?[0-9,.]+\\)?"))) %>% 
      mutate_at(i, 
                function(x) paste0(ifelse(nchar(x)==1,
                                          "EL00",ifelse(nchar(x)==2,
                                                        "EL0","EL")),x))
  }
  return(tmp.df2)
  
}

#########################################
# all.letters()                         #
#---------------------------------------#
# Determine which entries in a vector   #
# are composed of an all-alphabetical   #
# character string only.                #
#---------------------------------------#
#Returns an index vector indicating if a# 
#subelement is an all-letter character  #
#string                                 #
#########################################

all.letters <- function(vector) {
  return(which(grepl("^[A-Za-z]+$", vector, perl = T)))
}

###################################################
#Check the classes of all variables in a dataframe#
#-------------------------------------------------#
get.variable.classes <- function(data){
  classes <- NULL
  for (i in names(data)) {
    classes[i]<-(select(data,i))[[1]] %>% class()
  }
  return(table(classes))
}

####################################################
# format.date()                                    #
#--------------------------------------------------#
# Format dates from FACTOR (to character) to "Date"#
# STARTING format: "yyyy-mm-dd"                    #
#--------------------------------------------------#
#returns a new dataframe, but the date is fixed!   #
####################################################
format.date<- function(data, date.variable.name) {
  if (class(date.variable.name) != "character") {
    stop("variable names must be character strings")
  } else {
    date.index <- grep(date.variable.name, names(data))
    out <- data %>% 
      mutate_at(date.index, function(x) as.character(x)) %>%
      mutate_at(date.index, function(x) strtrim(x,10)) %>%
      mutate_at(date.index, function(x) as.Date(x, format = "%Y-%m-%d"))
  }
  return(out)
}

##############################################################################
# Which columns have missing data (blanks, not NA), and how many such fields?#
#----------------------------------------------------------------------------#
quantity.missing = function(x) {
  apply(x,2, function(x) length(which(x == "")))
}

#########################################
# Which variables have NAs and how many?#
#---------------------------------------#
where.na = function(x) {
  apply(x,2, function(x) sum(is.na(x)))
}

###############################################################
# Which columns have zero's entered, and how many such fields?#
#-------------------------------------------------------------#
quantity.zero = function(x) {
  apply(x,2, function(x) length(which(x == 0)))
}

################################################
#Convert factors (that are numbers) to numeric)#
#----------------------------------------------#
factor2numeric <- function(x) {
  if (class(x) != "factor") {
    stop(print("Error: variable is not a factor"))
  } else {
    as.numeric(as.character(x))
  }
  
}
#may not work with NAs...further investigation required.


#####################################################################
#Calculate the exact percentage of NAs per participant across ####### 
#all columns as one vector ##########################################
#-------------------------------------------------------------------#
percentNA = function(df) {
  index <- numeric()
  for (i in 1:nrow(df)) {
    index[i] <- sum(filter(df,IDENT_SUBID == df$IDENT_SUBID[i])[,3:32] %>%
                      is.na())/length(3:32)
  }
  return(index)
}
################################################################
# What obs meet a logical criteria; print them and their index #
#--------------------------------------------------------------#

#cbind(seq(along=df$V1)[df$V1 < 5], df$V1[df$V1 < 5])




