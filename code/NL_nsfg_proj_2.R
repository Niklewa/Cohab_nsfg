# Firstly, I advice you to clear your environment and restart R session to not get too messy
# (variables from earlier file will not be needed here)

# Data documentation: https://www.cdc.gov/nchs/nsfg/nsfg_cycle6.htm

# Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(scales)

# Loading the data ------------------------------------------------------------

# You need to specify your location of that file
dat_nsfg <- read.csv("C:\\Users\\nikod\\Documents\\Praca_dyplomowa\\main_data\\dat_nsfg.csv")

summary(dat_nsfg) # quick look at the data
nsfg_len <-  dim(dat_nsfg)[1] # data length

  
n_occur <- data.frame(table(dat_nsfg$caseid)) # testing if all "caseid" are unique
n_occur[n_occur$Freq > 1,] 

n_occur2 <- data.frame(table(dat_nsfg$religion)) # all distinct values in "religion"
n_occur2[n_occur2$Freq > 1,] 



# Data wrangling ----------------------------------------------------------

# Erasing problematic NAs (only in columns that will be useful for me)
dat_nsfg$marrend[is.na(dat_nsfg$marrend)] <- 99 
dat_nsfg$livtogn[is.na(dat_nsfg$livtogn)] <- 99  
dat_nsfg$lifprtnr[is.na(dat_nsfg$lifprtnr)] <- 0 
dat_nsfg$reldlife[is.na(dat_nsfg$reldlife)] <- 0


# functions for data wrangling
fun_cohab <- function(x){
  ifelse((x == 9 | x == 8 | x == 99  ), 99, ifelse((x == 5 ), 0, 1))
}

fun_reldlife <- function(x){
  ifelse((x == 8 | x == 0  ), 0, ifelse((x == 9 ), 2, x))
}
# The values are chosen according to the data documentation


# Taking data that I will potentially need
dat_nsfg1 <- dat_nsfg %>% 
  mutate(divorced = ifelse ((fmarit == 3 | marrend == 2), 1, 0)) %>% 
  mutate(ever_married = ifelse ((fmarit == 5 ), 0, 1)) %>% 
  mutate(cohab_bef_marr = fun_cohab(livtogn)) %>% 
  mutate(separated = ifelse ((fmarit == 4), 1, 0)) %>% 
  mutate(reldlife = fun_reldlife(reldlife)) %>% 
  mutate(cohab_ever = ifelse ((livtogn == 1 | marstat ==2 ), 1, 0)) %>% 
  mutate(div_separated = ifelse((separated == 1 | divorced == 1), 1, 0)) %>% 
  select(caseid, sex, divorced, separated, div_separated, ever_married, 
         cohab_bef_marr, cohab_ever, intvwyear, age_a, lifprtnr, religion, reldlife) 

head(dat_nsfg1)  # quick look at the data
sum(dat_nsfg1$separated) # all people that are in separation
sum(dat_nsfg1$divorced) # all people that got divorced
sum(dat_nsfg1$div_separated) # all people that are in separation or got divorced

# Cohabitation (cohab) and divorce, nominal -------------------------------------------------------

# The first df is the base of my analysis, it refers only to people that are or were married
df <- dat_nsfg1 %>%   
  filter(cohab_bef_marr != 99)    

all_div <- sum(df$divorced) / dim(df)[1] # 0.270 of married people in population divorced
all_div_sep <- sum(df$div_separated) / dim(df)[1] # 0.354, people divorced or separate
  
df1 <- df %>% 
  filter(divorced == 1) %>%  # more people that cohabited divorced, 3070
  filter(cohab_bef_marr == 1)
  
df2 <- df %>% 
  filter(divorced == 1) %>% # less people that didnt cohab divorced, 2418
  filter(cohab_bef_marr == 0) 
  
dim(df1)[1] / sum(df$divorced) # 56 % of people that divorced cohab
dim(df2)[1] / sum(df$divorced) # 44 % of people that divorced didn't cohab

dim(df1)[1]  / dim(df)[1] # 0.15 divorced and cohab
dim(df2)[1]  / dim(df)[1] # 0.12 divorced and didn't cohab

df3 <- df %>% 
  filter(div_separated == 1) %>%  # divorce or separated
  filter(cohab_bef_marr == 1)

df4 <- df %>% 
  filter(div_separated == 1) %>%  # divorce or separated
  filter(cohab_bef_marr == 0)

dim(df3)[1] / sum(df$div_separated) # 58 % of people that divorced or sep cohab
dim(df4)[1] / sum(df$div_separated) # 42 % of people that divorced or sep didnt cohab

# At first glance it may seem that cohabitation correlation with divorce is, at least
# noticeable. But you should keep up in mind that the majority of married people in the population
# cohabited before marriage.

funx <- function(x){
  dim(df %>% 
        filter(cohab_bef_marr == x))[1]/dim(df)[1]
}
cohab_married_scale <- funx(1) # 0.57
ncohab_married_scale <- funx(0) # 0.43

# These values are surprisingly close to the percentage of people that divorced and cohab/ncohab !!!


# Age and div % -----------------------------------------------------------


# There are:
f <- sum(df$sex) # 478 males 
m <- (dim(df)[1] - f )# 1977 females 

# Females dominates as respondents in our survey

div <- length(df$divorced[ df$divorced == 1])
marr <- dim(df)[1] # all that were ever married
div_sep <- length(df$div_separated[ df$div_separated == 1])

div_to_marr <- div / marr # 0.27,  of married people are divorced 
div_sep_to_marr <- div_sep / marr # 0.35,  of married people are divorced or separated

# Pay attention that the people are relatively young

sum(dat_nsfg1$ever_married) / dim(dat_nsfg1)[1] # 0.422 that is 42% of sample ever was married


# Quick pick at the age of the married population

plot_age_married <- ggplot(df, aes(x=age_a)) +
  scale_y_continuous(ylab( "Number of people") ) +
  geom_bar(width = 0.9, fill = "dodgerblue2") + scale_x_continuous(xlab("Age"), limits = c(10,55))
plot_age_married

# Religion and divorce --------------------------------------------------------------------
funz <- function(x,y){
  x %>% 
    filter(religion == y )
}

dfa <- funz(df, 1) # 3444 atheists

dfa1 <- funz(df1, 1) # 713 atheists that cohab and divorced

dfa2 <- funz(df2, 1) # 364 atheists that didn't cohab and divorced

dfa3 <- funz(df3, 1) # 950  divorce and separation and cohab

dfa4 <- funz(df4, 1) # 476 divorce and separation and didn't cohab

dim(dfa1)[1] / sum(dfa$divorced) # 0.66 non believers that cohab and div
dim(dfa2)[1] / sum(dfa$divorced) # 0.34  non believers that didn't cohab and div
# 66% of divorced atheists cohabited 

dim(dfa3)[1] / sum(dfa$div_separated) # 0.67 non believers that cohab and div or sep
dim(dfa4)[1] / sum(dfa$div_separated) # 0.33 non believers that didn't cohab and div or sep

# There seem to be stronger correlation between atheism and divorce. But again, there is a 
# very important fact that the population of people that didn't cohabited is low. To take account of
# the fact that this values is small Bayesian probability will be needed.

# Bayesian conditionals -------------------------------------------------------------

# Here I will specify the df that will be the base for our further research
certain_df <- df %>% 
  mutate(cohabitation = cohab_bef_marr) %>% 
  mutate(believer = ifelse((religion !=1 ), 1, 0)) %>% 
  mutate(devoted = ifelse((reldlife == 1), 1, 0)) %>% 
  select(divorced, cohabitation, believer, devoted, separated, div_separated)

head(certain_df)

all_rell_married <- dim(certain_df)[1] # all maried with answer for religion and cohab

# Now I will define some variables that will be needed later
# You can skip them and proceed to conditionals

fun_one_var <- function(x){dim(certain_df %>% 
                                 filter(x) )[1]}


atheists <- fun_one_var(certain_df$believer == 0) # married atheists

believers <- fun_one_var(certain_df$believer == 1) 

c_div <- dim(df1)[1] # cohab and divorced
nc_div <- dim(df2)[1]# didnt cohab and divorced
c_sepdiv <- dim(df3)[1]# divorced or separated and cohab
nc_sepdiv <- dim(df4)[1]# divorced or separated and didnt cohab


cohab_nbel_divsep <- dim(certain_df %>% 
                           filter(div_separated == 1) %>% 
                           filter(believer == 0) %>% 
                           filter(cohabitation == 1))[1]

cohab_nbel_div <- dim(certain_df %>% 
                        filter(divorced == 1) %>% 
                        filter(believer == 0) %>% 
                        filter(cohabitation == 1))[1]

cohab_nbel <- dim(certain_df %>% 
                    filter(believer == 0) %>% 
                    filter(cohabitation == 1))[1]

cohab_nbel <- dim(certain_df %>% 
                    filter(believer == 0) %>% 
                    filter(cohabitation == 1))[1]


catholic <- dim(df %>% 
                  filter(religion == 2))[1]
divsep_catholic <- dim(df %>% 
                         filter(religion == 2) %>% 
                         filter(div_separated == 1))[1]
n_catholic <- dim(df %>% 
                    filter(religion !=2))[1]
divsep_n_catholic <- dim(df %>% 
                           filter(religion !=2) %>% 
                           filter(div_separated == 1))[1]
devot <- dim(df %>% 
               filter(reldlife == 1))[1]
n_devot <- dim(df %>% 
                 filter(reldlife !=1))[1]
dev_div <- dim(df %>% 
                 filter (divorced == 1) %>% 
                 filter(reldlife == 1))[1] # religion important
ndev_div <- dim(df %>% 
                  filter (divorced == 1) %>% 
                  filter(reldlife !=1))[1]

divsep_believer <- dim(certain_df %>% 
                         filter(div_separated == 1) %>% 
                         filter(believer == 1))[1]

cohab <- fun_one_var(certain_df$cohabitation == 1)
no_cohab <- fun_one_var(certain_df$cohabitation == 0)

all_divorced <- fun_one_var(certain_df$divorced == 1)

divorced_believer <- dim(certain_df %>% 
                           filter(divorced == 1) %>% 
                           filter(believer == 1))[1]

divorced_atheist <- dim(certain_df %>% 
                          filter(divorced == 1) %>% 
                          filter(believer == 0))[1]

divsep_atheist <- dim(certain_df %>% 
                        filter(div_separated == 1) %>% 
                        filter(believer == 0))[1]


# All right, now we will use those variables to conduct some calculations
# We will check how does variables affect the probability of divorce / div or separation

# Bayes theorem: P(A|B) = P(A 'intersection' B) / P(B)

bayes_fun <- function(x,y,z){
  (x/y)/(z/y)
}

# With that function we will test some conditionalisations, after that we will have
# a base for further research, and we will be able to test and post hypotheses

cond_P_DA <- bayes_fun(divorced_atheist,all_rell_married,atheists)
cond_P_DA #0.31
cond_P_DB <- bayes_fun(divorced_believer ,all_rell_married,believers)
cond_P_DB #0.26 

cond_P_DC <- bayes_fun(c_div,all_rell_married,cohab) #0.2657
cond_P_DC # prob of divorce when u cohabited before marriage
cond_P_DnC <-  bayes_fun(nc_div ,all_rell_married, no_cohab) #0.2778
cond_P_DnC # prob of divorce when u didn't cohab

# The values are very similar, it has an affect on the hypothesis that I want to
# falsify. There is even small tendency that not cohabitating creates greater prob of divorce,
# although pay attention that the difference is very small

cond_P_DsC <-  bayes_fun(c_sepdiv ,all_rell_married, cohab) # 0.36
cond_P_DsC # prob of divorce or separation when u cohabited before marriage
cond_P_DsnC <- bayes_fun(nc_sepdiv ,all_rell_married, no_cohab) # 0.34
cond_P_DsnC # prob of divorce or separation when u didnt cohab
# here it is slightly visible that prob of separation or divorce is greater if you cohab,
# still the difference is small


cond_P_DD <-  bayes_fun(dev_div ,all_rell_married, devot)  # divorce and devoted
cond_P_DD # 0.251
cond_P_DnD <- bayes_fun(ndev_div ,all_rell_married, n_devot) # divorce and not devoted
cond_P_DnD  # 0.292


# conditional of div or sep | believer 
cond_P_DsB <-  bayes_fun(divsep_believer ,all_rell_married, believers) 
cond_P_DsB #0.342
cond_P_DsnB <-  bayes_fun(divsep_atheist ,all_rell_married, atheists)
cond_P_DsnB #0.414

# catholic
cond_P_DsC <-  bayes_fun(divsep_catholic ,all_rell_married, catholic)
cond_P_DsC #0.291
cond_P_DsnC <-  bayes_fun(divsep_n_catholic ,all_rell_married, n_catholic)
cond_P_DsnC # 0.375


cond_P_CA <-  bayes_fun(cohab_nbel ,all_rell_married, atheists)
cond_P_CA #0.737


# prob of divorce / divorce or separation given being atheist and cohabitating bef marriage
cond_P_DsCnB <- bayes_fun(cohab_nbel_divsep ,all_rell_married, cohab_nbel)
cond_P_DsCnB #0.373
cond_P_DCnB <- bayes_fun(cohab_nbel_div ,all_rell_married, cohab_nbel)
cond_P_DCnB #0.280


pr_of <- c("Divorce", "Divorce or Separation")
co <- c(cond_P_DC, cond_P_DsC)
nb <- c(cond_P_DA, cond_P_DsnB)
co_and_nb <- c(cond_P_DCnB, cond_P_DsCnB)


df_conditionals <- data.frame(pr_of, co, nb, co_and_nb)
df_conditionals 

# The table sums up the conclusion of our conditionals, we can see that the 
# parameter that correlates the most with divorce is "atheist" status

# Now we will proceed to visualization and comparison of the data


# Visualizations ----------------------------------------------------------

# Divorce versus different variables

status6 <- c(rep("Cohab", 2),rep("No cohab", 2),
             rep("Believer", 2), rep("Non believer", 2),
             rep("Devoted", 2), rep("Not devoted", 2))
divorce_truth_value <- c("yes", "no", "yes", "no", "yes", 
                         "no", "yes", "no", "yes", "no", "yes", "no")

fun_22 <- function(x,y,z){
  dim(certain_df %>% 
        filter(x == y) %>% 
        filter(divorced == z) )[1]
}

divorce_or_not <- c(
  cohab_div <- fun_22(certain_df$cohabitation,1 , 1),
  cohab_ndiv <- fun_22(certain_df$cohabitation,1 , 0),
  ncohab_div <- fun_22(certain_df$cohabitation,0 , 1),
  ncohab_ndiv <- fun_22(certain_df$cohabitation,0 , 0),
  bel_div<- fun_22(certain_df$believer,1 , 1),
  bel_ndiv <- fun_22(certain_df$believer,1 , 0),
  nbel_div <- fun_22(certain_df$believer,0 , 1),
  nbel_ndiv <- fun_22(certain_df$believer,0 , 0),
  devot_div <- fun_22(certain_df$devoted,1, 1),
  devot_ndiv <- fun_22(certain_df$devoted,1 , 0),
  ndevot_div <- fun_22(certain_df$devoted,0 , 1),
  ndevot_ndiv<- fun_22(certain_df$devoted,0 , 0)
)

ciag <- c(rep(sum(cohab_div, cohab_ndiv ),2), 
          rep(sum(ncohab_div, ncohab_ndiv),2),
          rep(sum(bel_div, bel_ndiv),2),
          rep(sum(nbel_div,  nbel_ndiv),2),
          rep(sum(devot_div,  devot_ndiv),2),
          rep(sum(ndevot_div,  ndevot_ndiv),2) )

values <- divorce_or_not / ciag

val_perc <- label_percent(accuracy = 1)(values)

df69 <- data.frame(status6, divorce_truth_value, divorce_or_not, values, val_perc)
df69

p6 <- ggplot(df69 , aes(fill=divorce_truth_value , y=divorce_or_not, x=status6)) + 
  geom_bar(position="stack", stat="identity") + theme_tufte() + 
  scale_y_continuous(ylab("All Married"), limits = c(0, 20000)) +  
  scale_x_discrete(xlab("Status"), limits=c("Cohab", "No cohab", "Believer", "Non believer",
                                            "Devoted", "Not devoted")) +
  ggtitle("Scale of divorces accros different statuses") + scale_fill_brewer(palette = "Paired") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Divorced")
p6


p7 <- ggplot(df69 , aes(fill=divorce_truth_value , y=divorce_or_not, x=status6)) + 
  geom_bar(position="fill", stat="identity") + theme_tufte() + 
  scale_y_continuous(ylab("All married"), labels = percent) +  
  scale_x_discrete(xlab("Status"), limits=c("Cohab", "No cohab", "Believer", "Non believer",
                                            "Devoted", "Not devoted")) +
  geom_text(aes(label = val_perc, y=values), position = "stack") + scale_fill_brewer(palette = "Paired") +
  labs(fill = "Divorced") + ggtitle("Scale of divorces accros different statuses") +
  theme(plot.title = element_text(hjust = 0.5))
# theme(axis.text.x = element_text(angle = 45)) 
p7



# Divorce or separation versus different variables


fun_sep_div <- function(x,y,z){
  dim(certain_df %>% 
        filter(x == y) %>% 
        filter(div_separated == z) )[1]
}

div_sep_or_not <- c(
  cohab_divsep <- fun_sep_div(certain_df$cohabitation,1 , 1),
  cohab_ndivsep <- fun_sep_div(certain_df$cohabitation,1 , 0),
  ncohab_divsep <- fun_sep_div(certain_df$cohabitation,0 , 1),
  ncohab_ndivsepb <- fun_sep_div(certain_df$cohabitation,0 , 0),
  bel_divsep <- fun_sep_div(certain_df$believer,1 , 1),
  bel_ndivsep <- fun_sep_div(certain_df$believer,1 , 0),
  nbel_divsep <- fun_sep_div(certain_df$believer,0 , 1),
  nbel_ndivsep <- fun_sep_div(certain_df$believer,0 , 0),
  devot_divsep <- fun_sep_div(certain_df$devoted,1, 1),
  devot_ndivsep <- fun_sep_div(certain_df$devoted,1 , 0),
  ndevot_divsep <- fun_sep_div(certain_df$devoted,0 , 1),
  ndevot_ndivsep<- fun_sep_div(certain_df$devoted,0 , 0)
)


ciag3 <- c(rep(sum(cohab_divsep, cohab_ndivsep  ),2), 
           rep(sum(ncohab_divsep, ncohab_ndivsepb),2),
           rep(sum(bel_divsep, bel_ndivsep),2),
           rep(sum(nbel_divsep ,  nbel_ndivsep),2),
           rep(sum(devot_divsep,  devot_ndivsep),2),
           rep(sum(ndevot_divsep,  ndevot_ndivsep),2) )



values3 <- div_sep_or_not / ciag3
val_perc3 <- label_percent(accuracy = 1)(values3)

status_sep_div <- c(rep("Cohab", 2),rep("No cohab", 2),
                    rep("Believer", 2), rep("Non believer", 2),
                    rep("Devoted", 2), rep("Not devoted", 2))
divorce_or_sep_truth_value <- c("yes", "no", "yes", "no", "yes", 
                                "no", "yes", "no", "yes", "no", "yes", "no")

dfdiv <- data.frame(status_sep_div, divorce_or_sep_truth_value,
                    div_sep_or_not, values3, val_perc3)
dfdiv





p10 <- ggplot(dfdiv , aes(fill=divorce_or_sep_truth_value , y=div_sep_or_not, x=status_sep_div)) + 
  geom_bar(position="stack", stat="identity") + theme_tufte() + 
  scale_y_continuous(ylab("All married"), limits = c(0, 20000)) +  
  scale_x_discrete(xlab("Status"), limits=c("Cohab", "No cohab", "Believer", "Non believer",
                                            "Devoted", "Not devoted")) +
  ggtitle("Scale of divorce and separation accros different statuses") +
  scale_fill_brewer(palette = "Paired") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Div or Sep")
p10

p11 <- ggplot(dfdiv , aes(fill=divorce_or_sep_truth_value , y=div_sep_or_not, x=status_sep_div)) + 
  geom_bar(position="fill", stat="identity") + theme_tufte() + 
  scale_y_continuous(ylab("All married"), labels = percent) +  
  scale_x_discrete(xlab("Status"), limits=c("Cohab", "No cohab", "Believer", "Non believer",
                                            "Devoted", "Not devoted")) +
  geom_text(aes(label = val_perc3, y=values3), position = "stack") + 
  scale_fill_brewer(palette = "Paired") +
  labs(fill = "Div or Sep") + ggtitle("Scale of divorce and separation accros different statuses") +
  theme(plot.title = element_text(hjust = 0.5))
# theme(axis.text.x = element_text(angle = 45)) 
p11



# Cohabitation versus different variables


status7 <- c(rep("Divorced", 2),rep("No divorced", 2),
             rep("Believer", 2), rep("Non believer", 2),
             rep("Devoted", 2), rep("Not devoted", 2))
cohab_truth_value <- c("yes", "no", "yes", "no", "yes", 
                         "no", "yes", "no", "yes", "no", "yes", "no")


fun_cohab <- function(x,y,z){
  dim(certain_df %>% 
        filter(x == y) %>% 
        filter(cohabitation == z) )[1]
}
cohab_or_not <- c(
div_cohab <- fun_cohab(certain_df$divorced,1 , 1),
div_ncohab <- fun_cohab(certain_df$divorced,1 , 0),
ndiv_cohab <- fun_cohab(certain_df$divorced,0 , 1),
ndiv_ncohab <- fun_cohab(certain_df$divorced,0 , 0),
bel_cohab <- fun_cohab(certain_df$believer,1 , 1),
bel_ncohab <- fun_cohab(certain_df$believer,1 , 0),
nbel_cohab <- fun_cohab(certain_df$believer,0 , 1),
nbel_ncohab <- fun_cohab(certain_df$believer,0 , 0),
devot_cohab <- fun_cohab(certain_df$devoted,1, 1),
devot_ncohab <- fun_cohab(certain_df$devoted,1 , 0),
ndevot_cohab <- fun_cohab(certain_df$devoted,0 , 1),
ndevot_ncohab <- fun_cohab(certain_df$devoted,0 , 0)
)


ciag2 <- c(rep(sum(div_cohab,div_ncohab ),2), 
           rep(sum(ndiv_ncohab, ndiv_cohab),2),
           rep(sum(bel_ncohab, bel_cohab),2),
           rep(sum(nbel_ncohab,  nbel_cohab),2),
           rep(sum(devot_ncohab,  devot_cohab),2),
           rep(sum(ndevot_ncohab,  ndevot_cohab),2) )

values2 <- cohab_or_not / ciag2
val_perc2 <- label_percent(accuracy = 1)(values2)

df67 <- data.frame(status7, cohab_truth_value, cohab_or_not, values2, val_perc2)
df67



p8 <- ggplot(df67 , aes(fill=cohab_truth_value , y=cohab_or_not, x=status7)) + 
  geom_bar(position="stack", stat="identity") + theme_tufte() + 
  scale_y_continuous(ylab("All married"), limits = c(0, 20000)) +  
  scale_x_discrete(xlab("Status"), limits=c("Divorced", "No divorced", "Believer", "Non believer",
                                            "Devoted", "Not devoted")) +
  ggtitle("Scale of cohabitation accros different statuses") + scale_fill_brewer(palette = "Paired") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Cohabited")
p8

p9 <- ggplot(df67 , aes(fill=cohab_truth_value , y=cohab_or_not, x=status7)) + 
  geom_bar(position="fill", stat="identity") + theme_tufte() + 
  scale_y_continuous(ylab("All married"), labels = percent) +  
  scale_x_discrete(xlab("Status"), limits=c("Divorced", "No divorced", "Believer", "Non believer",
                                            "Devoted", "Not devoted")) +
  geom_text(aes(label = val_perc2, y=values2), position = "stack") + scale_fill_brewer(palette = "Paired") +
  labs(fill = "Cohabited") + ggtitle("Scale of cohabitation accros different statuses") +
  theme(plot.title = element_text(hjust = 0.5))
# theme(axis.text.x = element_text(angle = 45)) 
p9


# Joined plot


fun_33 <- function(x,y,z){
  dim(certain_df %>% 
        filter(x == y) %>% 
        filter(separated == z))[1]
  
}

fun_44 <- function(x,y){
  dim(certain_df %>% 
        filter(x == y) %>% 
        filter(div_separated == 0))[1]
  
}

three_comb <- c(
  cohab_ndiv2 <- fun_44(certain_df$cohabitation,1 ),
  cohab_div <- fun_22(certain_df$cohabitation,1 , 1),
  cohab_sep <- fun_33(certain_df$cohabitation,1, 1 ),
  
  ncohab_ndiv2 <- fun_44(certain_df$cohabitation,0 ),
  ncohab_div <- fun_22(certain_df$cohabitation,0 , 1),
  ncohab_sep <- fun_33(certain_df$cohabitation,0, 1 ),
  
  bel_ndiv2 <- fun_44(certain_df$believer,1 ),
  bel_div<- fun_22(certain_df$believer,1 , 1),
  bel_sep <- fun_33(certain_df$believer,1, 1 ),
  
  nbel_ndiv2 <- fun_44(certain_df$believer,0 ),
  nbel_div <- fun_22(certain_df$believer,0 , 1),
  nbel_sep <- fun_33(certain_df$believer,0, 1 ),
  
  devot_ndiv2 <- fun_44(certain_df$devoted,1 ),
  devot_div <- fun_22(certain_df$devoted,1, 1),
  devot_sep <- fun_33(certain_df$devoted,1, 1 ),
  
  
  ndevot_ndiv2 <- fun_44(certain_df$devoted,0 ),
  ndevot_div <- fun_22(certain_df$devoted,0 , 1),
  ndevot_sep <- fun_33(certain_df$devoted,0, 1 )
)

ciag4 <- c(rep(sum(cohab_div, cohab_ndiv2, cohab_sep  ),3), 
           rep(sum(ncohab_div, ncohab_ndiv2, ncohab_sep),3),
           rep(sum(bel_div, bel_ndiv2, bel_sep),3),
           rep(sum(nbel_div ,  nbel_ndiv2, nbel_sep),3),
           rep(sum(devot_div,  devot_ndiv2, devot_sep),3),
           rep(sum(ndevot_div,  ndevot_ndiv2, ndevot_sep),3) )



values4 <- three_comb / ciag4
val_perc4 <- label_percent(accuracy = 1)(values4)



status33 <- c(rep("Cohab", 3),rep("No cohab", 3),
                    rep("Believer", 3), rep("Non believer", 3),
                    rep("Devoted", 3), rep("Not devoted", 3))
x_truth_value <- c("married", "div",  "sep", "married", "div", "sep",  "married", "div",  "sep", 
                   "married", "div",  "sep", "married", "div",  "sep", "married", "div",  "sep")


dfdiv2 <- data.frame(status33 , x_truth_value,
                    three_comb, values4, val_perc4)
dfdiv2


p10 <- ggplot(dfdiv2 , aes(fill=x_truth_value, y=three_comb, x=status33 )) + 
  geom_bar(position="stack", stat="identity") + theme_tufte() + 
  scale_y_continuous(ylab("All married"), limits = c(0, 20000)) +  
  scale_x_discrete(xlab("Status"), limits=c("Cohab", "No cohab", "Believer", "Non believer",
                                            "Devoted", "Not devoted")) +
  ggtitle("Scale of divorced or separation accros different statuses") +
  scale_fill_brewer(palette = "Paired") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Div or Sep")
p10

p11 <- ggplot(dfdiv2 , aes(fill=x_truth_value , y=three_comb, x=status33 )) + 
  geom_bar(position="fill", stat="identity") + theme_tufte() + 
  scale_y_continuous(ylab("All married"), labels = percent) +  
  scale_x_discrete(xlab("Status"), limits=c("Cohab", "No cohab", "Believer", "Non believer",
                                            "Devoted", "Not devoted")) +
  geom_text(aes(label = val_perc4, y=values4), position = "stack") + 
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Scale of divorced or separation accros different statuses") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Div or Sep")

p11




# Difference tables ------------------------------------------------------------

# The correlation of a variable might be represented by the difference between probability
# of variables truth value, so when it is present and absent. The difference shows how does it 
# affect the prior. Therefore I created a table that sums up the most interesting difference
# parameters.


cohab_diff <- c(
nbel_bel_coh_diff <- (values2[7] - values2[5]),
ndev_dev_coh_diff <- (values2[11] - values2[9]),
nbel_dev_coh_diff <- (values2[7] - values2[9]), NA
)

div_diff <- c(
  nbel_bel_div_diff <- (values[7] - values[5]),
  ndev_dev_div_diff <- (values[11] - values[9]),
  nbel_dev_div_diff <- (values[7] - values[9]),
  coh_ncoh_div_diff <- (values[1] - values[3])
)

div_sep_diff <- c(
  nbel_bel_div_diff <- (values3[7] - values3[5]),
  ndev_dev_div_diff <- (values3[11] - values3[9]),
  nbel_dev_div_diff <- (values3[7] - values3[9]),
  coh_ncoh_div_diff <- (values3[1] - values3[3])
)

names <- c("Atheist - Believer", "Not Devoted - Devoted",
               "Atheist - Devoted", "Cohab - Not Cohab")
df_differences <- data.frame(names, div_diff, div_sep_diff, cohab_diff)
df_differences

# The strongest parameter pair is atheist - devoted, it is a mixed up value,
# although the measure is valuable because one cannot be both an atheist and a devoted believer

p44 <- ggplot(df_differences , aes(y=div_diff, x=names)) + 
  geom_bar(stat="identity", fill = "dodgerblue2") + theme_tufte() + 
  scale_y_continuous(ylab("All married") , limits = c(-0.013, 0.3)) +  
  scale_x_discrete(xlab("Status"), limits=c("Atheist - Believer", "Not Devoted - Devoted",
                                            "Atheist - Devoted", "Cohab - Not Cohab")) +
  ggtitle("Difference parameters to divorce") + 
  theme(plot.title = element_text(hjust = 0.5)) 
p44

p55 <- ggplot(df_differences , aes(y=div_sep_diff, x=names)) + 
  geom_bar(stat="identity", fill = "dodgerblue2") + theme_tufte() + 
  scale_y_continuous(ylab("All married") , limits = c(-0.013, 0.3)) +  
  scale_x_discrete(xlab("Status"), limits=c("Atheist - Believer", "Not Devoted - Devoted",
                                            "Atheist - Devoted", "Cohab - Not Cohab")) +
  ggtitle("Difference parameters to div or sep") + 
  theme(plot.title = element_text(hjust = 0.5)) 
p55

p66 <- ggplot(df_differences , aes(y=cohab_diff, x=names)) + 
  geom_bar(stat="identity", fill = "dodgerblue2") + theme_tufte() + 
  scale_y_continuous(ylab("All married") , limits = c(-0.013, 0.3)) +  
  scale_x_discrete(xlab("Status"), limits=c("Atheist - Believer", "Not Devoted - Devoted",
                                            "Atheist - Devoted")) +
  ggtitle("Difference parameters to cohabitation") + 
  theme(plot.title = element_text(hjust = 0.5)) 
p66

 

# Conclusion --------------------------------------------------------------

# According to my research there are better correlates to divorce or separation / divorce that
# cohabitation before marriage. Hypothesis that cohabitation causes divorces should be abandon. 
# The most closest hypothesis will be that being an atheist enlarges your probability of divorce.
# Furthermore, cohabitation correlates strongly with being an atheist, therefore your status of believe
# has a status of hidden correlate parameter.






