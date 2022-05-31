library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Data documentation: https://www.cdc.gov/nchs/nsfg/nsfg_cycle6.htm

# There are two main problems with our download NSFG files:
# 1. They are large
# 2. They are messy
# We can't just simply load individually all csv files as separate data frames, 
# it will occupy to much of memory, also, it is not a very elegant solution. Therefore
# I read the csv files into lists. To save even more memory (there are thousands of variables)
# I specify what columns interest me. 
# I have chosen correct columns through the lecture of the data description.
# Why not a one list? The second problem is the reason. 
# Unfortunately there are some differences in questions that
# were asked in the surveys across the years. Some parameters have different names, some of them are missing,
# and some of them are relative to the gender of a subject. That's why I had to make several lists, 
# 3 basic groups, and 2 for females, one for males.

# First group -------------------------------------------------------------
# Here you need to copy paste your path to the data files
setwd("C:/Users/nikod/Documents/Praca_dyplomowa/csv_files_0200_0610")

temp_1 <-  list.files(pattern="*.csv")

col_function_1 <- function(x) {read.csv(file = x, sep = ",")[ ,c(
  "caseid", "age_a", "marstat", "reldlife",  "religion", "samesex",
  "gayadopt",  "lifeprt", "timesmar", "staytog", "sxok18", "attnd14", "achieve", 
  "lifprtnr", "cmintvw", "fmarit"
  
)]}

dat_list_1 <- lapply(temp_1, col_function_1)

# Putting names on dfs in the list

dat_list_1 = list(
  "df0610F" = dat_list_1[[1]],
  "df0610M" = dat_list_1[[2]],
  "df02F" = dat_list_1[[3]],
  "df02M" = dat_list_1[[4]]
)

# Extracting interview year from cmc date format in "cmintvw" column


dat_list_1$df0610F <- dat_list_1$df0610F %>% 
  mutate(cmintvw = 
           ifelse((1200 <= cmintvw  & cmintvw <= 1272), "2005",
           ifelse((1273 <= cmintvw  & cmintvw <= 1284), "2006",
           ifelse((1285 <= cmintvw  & cmintvw <= 1296), "2007", 
           ifelse((1297 <= cmintvw  & cmintvw <= 1308), "2008", 
           ifelse((1309 <= cmintvw  & cmintvw <= 1320), "2009",
           ifelse((1321 <= cmintvw  & cmintvw <= 1332), "2010", "Error"))))))) %>% 
  mutate(cmintvw = as.integer(cmintvw))
                                                                           
colnames(dat_list_1$df0610F)[15] <- "intvwyear" 


dat_list_1$df0610M <- dat_list_1$df0610M %>% 
  mutate(cmintvw = 
          ifelse((1200 <= cmintvw  & cmintvw <= 1272), "2005",
          ifelse((1273 <= cmintvw  & cmintvw <= 1284), "2006", 
          ifelse((1285 <= cmintvw  & cmintvw <= 1296), "2007", 
          ifelse((1297 <= cmintvw  & cmintvw <= 1308), "2008", 
          ifelse((1309 <= cmintvw  & cmintvw <= 1320), "2009",
          ifelse((1321 <= cmintvw  & cmintvw <= 1332), "2010", "Error"))))))) %>% 
  mutate(cmintvw = as.integer(cmintvw))
  
colnames(dat_list_1$df0610M)[15] <- "intvwyear"

# The following two surveys were done solely in 2002, therefore it is easy to fill the missing date

v1 <- rep(2002, dim(dat_list_1$df02F)[1])
v2 <- rep(2002, dim(dat_list_1$df02M)[1])

fafm <- function(x,y) {
  x %>% 
    mutate(intvwyear = as.integer(y)) %>% 
    select(-cmintvw) 
}

dat_list_1$df02F <- fafm(dat_list_1$df02F, v1)
dat_list_1$df02M <- fafm(dat_list_1$df02M, v2)

#Making one, big data frame
df_0210 <- bind_rows(dat_list_1)

# Second group ------------------------------------------------------------
setwd("C:/Users/nikod/Documents/Praca_dyplomowa/csv_files_main")

temp_main <-  list.files(pattern="*.csv")

col_function_main <- function(x) {read.csv(file = x, sep = ",")[ ,c(
  "caseid", "age_a", "marstat", "reldlife", "religion",  "samesex", 
  "gayadopt", "intvwyear", "lifeprt", "timesmar", "sxok18", "staytog", 
  "attnd14", "prvntdiv", "fmarit", "lifprtnr"
)]}

dat_list_main <- lapply(temp_main, col_function_main)

# No wrangling at this stage needed, just putting them into one df (year of the survey is included)
df_1115 <- bind_rows(dat_list_main)



# Third group -------------------------------------------------------------
setwd("C:/Users/nikod/Documents/Praca_dyplomowa/csv_files_1517_1719")

temp_3 <-  list.files(pattern="*.csv")

col_function_3 <- function(x) {read.csv(file = x, sep = ",")[ ,c(
  "caseid", "age_a", "marstat", "reldlife", "religion", "samesex",
  "intvwyear", "lifprtnr", "timesmar", "attnd14", "fmarit"
  
)]}

dat_list_3 <- lapply(temp_3, col_function_3)

# As earlier, no wrangling needed
df_1519 <- bind_rows(dat_list_3)


# Now we join our dfs and save it as a csv file for later

dat_main_partial <- full_join(df_1519, df_1115)# Applying full_join function
dat_main <- full_join(dat_main_partial, df_0210)

# Quick check if everything is ok
head(dat_main) 
names(dat_main)

# You ought to specify the location of the file that you want to create, pay attention to give it
# the same name as I
write.csv(dat_main, "C:\\Users\\nikod\\Documents\\Praca_dyplomowa\\main_data\\dat_main.csv", 
          row.names = FALSE)

# Woman only questions  ----------------------------------------------------
# First group

setwd("C:/Users/nikod/Documents/Praca_dyplomowa/csv_files_F1")

temp_F1 <-  list.files(pattern="*.csv")

col_function_F1 <- function(x) {read.csv(file = x, sep = ",")[ ,c(
  "caseid", "lvtoghx", "abortion", "pill", "mornpill", "condom"
)]} 

dat_list_F1 <- lapply(temp_F1, col_function_F1)


# Unifying column names
colnames_F1 <- c( "caseid", "livtogn", "abortion", "pill", "mornpill", "condom")
dat_list_F1 <- lapply(dat_list_F1, setNames, colnames_F1)



#Second group
setwd("C:/Users/nikod/Documents/Praca_dyplomowa/csv_files_Fa")

temp_Fa <-  list.files(pattern="*.csv")

col_function_Fa <- function(x) {read.csv(file = x, sep = ",")[ ,c(
  "caseid", "marendhx", "lvtoghx","agemarhx", "abortion", "pill", "mornpill", "condom"
)]} 

dat_list_Fa <- lapply(temp_Fa, col_function_Fa)

# Unifying column names again
colnames_Fa <- c( "caseid", "marrend", "livtogn","agemarr", "abortion", "pill", "mornpill", "condom")
dat_list_Fa <- lapply(dat_list_Fa, setNames, colnames_Fa)


# Making two dfs out of these two groups
df_Fa <- bind_rows(dat_list_Fa)
df_F1 <- bind_rows(dat_list_F1)


df_Fem <- full_join(df_F1, df_Fa)# full join

# sex: 1 Male, 0 Female
fun_M <- function(x) {rep(1, dim(x)[1])} # gender functions
fun_F <- function(x) {rep(0, dim(x)[1])}

df_Fem <- df_Fem %>%  # Adding gender value
  mutate(sex = fun_F(df_Fem))


# Man only questions ------------------------------------------------------
setwd("C:/Users/nikod/Documents/Praca_dyplomowa/csv_files_M")

temp_M <-  list.files(pattern="*.csv")

col_function_M <- function(x) {read.csv(file = x, sep = ",")[ ,c(
  "caseid" , "marrend", "livtogn", "agemarr"
)]}

dat_list_M <- lapply(temp_M, col_function_M)

# Normalizing column names
colnames_M <- c("caseid", "marrend", "livtogn", "agemarr")
dat_list_M <- lapply(dat_list_M, setNames, colnames_M)

df_M <- bind_rows(dat_list_M)

# Adding gender value
df_M <- df_M %>% 
  mutate(sex = fun_M(df_M))



# Now we create unite gender sepcific df
dat_genders <- full_join(df_M, df_Fem)  # Full join
dat_genders 

# Lets save it
write.csv(dat_genders, "C:\\Users\\nikod\\Documents\\Praca_dyplomowa\\main_data\\dat_genders.csv", 
          row.names = FALSE)


# All data ----------------------------------------------------------------
# Now in order to save some memory and make everything look more tidy I will save our data in 3 files
# and open second R script file for analyzing the data


# combining two data frames into one
dat_nsfg <- full_join(dat_genders, dat_main, "caseid")

write.csv(dat_nsfg, "C:\\Users\\nikod\\Documents\\Praca_dyplomowa\\main_data\\dat_nsfg.csv", 
          row.names = FALSE)

# Spoiler alert, dat_nsfg will be the only file that will be needed in the main part of my project :)


