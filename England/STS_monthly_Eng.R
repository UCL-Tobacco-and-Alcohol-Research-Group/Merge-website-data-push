library(googlesheets4)
library(googledrive)
library(haven)
library(tigerstats)
library(dplyr)
library(tidyr)
library(DescTools)
library(zoo)
library(tibbletime)
library(lubridate)

#sts <- read_sav("C:/Toolkit merge files/Waves/186/omni186_39.1_65.2cot_31.3a_25.4s_recodes_72.5sa.sav")
#names(sts)[names(sts)=="@weight0"] <- "weight0" 

sts_eng <- sts %>% subset(xyear >= 2006 & gore < 10)

# Moving average function
avg.last.3 <- function (x) if (length(x) < 3) rep(NA, length(x)) else rollmeanr(x, 3, fill = NA)


## RUNNING IN HERE BUT ENTERING MANUALLY INTO PPT UNTIL HAVE WORKED OUT WHICH WAVES SKIPPED ETC...

################################
# CIGARETTE SMOKING PREVALENCE #
################################

#### Not uploading because of confusion about when waves were skipped e.g. for pandemic and others?

A <- ("")
B <- ("")
C <- ("")
D <- ("")

row1 <- cbind(A, B, C, D)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D")

row2 <- c("Month", "% cigarette smokers (3 month moving average)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("", "All", "ABC1", "C2DE")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$cigsmok + sts_eng$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_eng <- sts_eng %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_eng$weight0 ~ abc1_eng$cigsmok + abc1_eng$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_eng <- sts_eng %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_eng$weight0 ~ c2de_eng$cigsmok + c2de_eng$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, ab_df2, cd_df2)
colnames(df4) <- c("All", "ABC1", "C2DE")

df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.ABC1 = avg.last.3(ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1,2))

colnames(df5) <- c("All", "ABC1", "C2DE")

### INCREASE WAVE BY 1 EACH MONTH - FIX THIS
series <- create_series('2007' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(1:186))

df6 <- cbind(series, df5)
df6$date <- as.factor(df6$date)

colnames(df6) <- c("A", "B", "C", "D")
df6 <- rbind(headings, df6)
colnames(df6) <- c()

# sheet_write(df6, "1qu8VoRtEdQgq38EfzYfIbIqV6_okgW0I2OItSF6kpW4", sheet = 'Prevalence of cigarette smoking by social grade')

#######################################
# CIGARETTE SMOKING PREVALENCE BY AGE #
#######################################

# Not uploading because of confusion about when waves were skipped e.g. for pandemic and others?

row3 <- c("", "All", "Under 35", "Over 35")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)


under35 <- sts_eng %>% subset(over35 == 0)

under35_df1 <- t(colPerc(xtabs(under35$weight0 ~ under35$cigsmok + under35$xwave)))
under35_df2 <- under35_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

over35 <- sts_eng %>% subset(over35 == 1)

over35_df1 <- t(colPerc(xtabs(over35$weight0 ~ over35$cigsmok + over35$xwave)))
over35_df2 <- over35_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, under35_df2, over35_df2)
colnames(df4) <- c("All", "Under35", "Over35")

df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.under35 = avg.last.3(Under35)) %>%
  mutate(Avg.over35 = avg.last.3(Over35)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.under35, Avg.over35) %>%
  slice(-c(1:2))

colnames(df5) <- c("All", "Under35", "Over35")

df6 <- cbind(series, df5)
df6$date <- as.factor(df6$date)

colnames(df6) <- c("A", "B", "C", "D")
df6 <- rbind(headings, df6)

# sheet_write(df6, "1qu8VoRtEdQgq38EfzYfIbIqV6_okgW0I2OItSF6kpW4", sheet = 'Sheet1')

#######################################
# CIGARETTE SMOKING PREVALENCE BY SEX #
#######################################

# Not uploading because of confusion about when waves were skipped e.g. for pandemic and others?

# How was estimate for March calculated?
# How does SPSS deal with missing? Handful of missing in SPSS seems to effect estimate computed in R. Slightly different estimates for WOMEN...

stsmen <- sts_eng %>% subset(sexz == 1)

men_df1 <- t(colPerc(xtabs(stsmen$weight0 ~ stsmen$cigsmok + stsmen$xwave)))
men_df2 <- men_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

stswomen <- sts_eng %>% subset(sexz == 2)

women_df1 <- t(colPerc(xtabs(stswomen$weight0 ~ stswomen$cigsmok + stswomen$xwave)))
women_df2 <- women_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, men_df2, women_df2)
colnames(df4) <- c("All", "Men", "Women")

df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.men = avg.last.3(Men)) %>%
  mutate(Avg.women = avg.last.3(Women)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.men, Avg.women) %>%
  slice(-c(1:2))

colnames(df5) <- c("All", "Men", "Women")

df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)

# sheet_write(df6, "1qu8VoRtEdQgq38EfzYfIbIqV6_okgW0I2OItSF6kpW4", sheet = 'Sheet1')

####################################
# ATTEMPTS TO STOP BY SOCIAL GRADE #
####################################

### SOMETHING NOT RIGHT ESTIMATE FOR ALL IS HIGHER THAN ABC1 AND C2DE.

# How does SPSS deal with missing? Handful of missing in SPSS seems to effect estimate computed in R. Slightly different estimates...

smokly <- sts_eng %>% subset(smokly == 1)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(smokly$weight0 ~ smokly$trylyc + smokly$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Ns
n <- (t(round(addmargins(xtabs(smokly$weight0 ~ smokly$trylyc + smokly$xwave)))))

abc1_eng <- smokly %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_eng$weight0 ~ abc1_eng$trylyc + abc1_eng$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_eng <- smokly %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_eng$weight0 ~ c2de_eng$trylyc + c2de_eng$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, ab_df2, cd_df2)
colnames(df4) <- c("All", "ABC1", "C2DE")

df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.ABC1 = avg.last.3(ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1:2))

colnames(df5) <- c("All", "ABC1", "C2DE")

df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)
colnames(df6) <- c()

# sheet_write(df6, "1qu8VoRtEdQgq38EfzYfIbIqV6_okgW0I2OItSF6kpW4", sheet = 'Sheet1')

#################################
# SUPPORT USED IN QUIT ATTEMPTS #
#################################

ec_df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$ecig_hier + sts_eng$xwave)))
ec_df2 <- ec_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)


nrt_df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$nrtotc_hier + sts_eng$xwave)))
nrt_df2 <- nrt_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

medrx_df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$medrx_hier + sts_eng$xwave)))
medrx_df2 <- medrx_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

nhs_df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$nhs_hier + sts_eng$xwave)))
nhs_df2 <- nhs_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

no_df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$nothing_hier + sts_eng$xwave)))
no_df2 <- no_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(ec_df2, nrt_df2, medrx_df2, nhs_df2, no_df2)
colnames(df4) <- c("Ecig", "NRT", "MedRx", "NHS", "Nothing")

df5 <- df4 %>% 
  mutate(Avg.ec = avg.last.3(Ecig)) %>%
  mutate(Avg.nrt = avg.last.3(NRT)) %>%
  mutate(Avg.medrx = avg.last.3(MedRx)) %>%
  mutate(Avg.nhs = avg.last.3(NHS)) %>%
  mutate(Avg.noth = avg.last.3(Nothing)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.ec, Avg.nrt, Avg.medrx, Avg.nhs, Avg.noth) %>%
  slice(-c(1:2))

colnames(df5) <- c("E-cig", "NRT OTC", "MedRx", "NHS", "Nothing")

df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)
colnames(df6) <- c("", "E-cig", "NRT OTC", "MedRx", "NHS", "Nothing")
tail(df6)

# sheet_write(df6, "1qu8VoRtEdQgq38EfzYfIbIqV6_okgW0I2OItSF6kpW4", sheet = 'Sheet1')

#################################
# GP triggered quit attempts    #
#################################

gp_df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$gptrigatt2 + sts_eng$xwave)))
gp_df2 <- gp_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

colnames(gp_df2) <- c("GP")

df3 <- gp_df2 %>%
  mutate(Avg.gp = avg.last.3(GP)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.gp) %>%
  slice(-c(1:2))

colnames(df3) <- c("GP triggered quit")

df6 <- cbind(series, df3)
df6$date <- as.factor(df6$date)
colnames(df6) <- c()

# sheet_write(df6, "1qu8VoRtEdQgq38EfzYfIbIqV6_okgW0I2OItSF6kpW4", sheet = 'Sheet1')

######################
# Motivation to quit #
######################

motiv <- sts_eng %>% subset(xwave > 23 & cigsmok == 1)

df1 <- t(colPerc(xtabs(motiv$weight0 ~ motiv$qmotivwi + motiv$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

w2627 <- df2 %>% slice(c(1:2))

abc1_eng <- motiv %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_eng$weight0 ~ abc1_eng$qmotivwi + abc1_eng$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

ab2627 <- ab_df2 %>% slice(c(1:2))

c2de_eng <- motiv %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_eng$weight0 ~ c2de_eng$qmotivwi + c2de_eng$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

cd2627 <- cd_df2 %>% slice(c(1:2))

w2627 <- cbind(w2627, ab2627, cd2627)
colnames(w2627) <- c("All", "ABC1", "C2DE")

df4 <- cbind(df2, ab_df2, cd_df2)
colnames(df4) <- c("All", "ABC1", "C2DE")

df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.ABC1 = avg.last.3(ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1:3))

colnames(df5) <- c("All", "ABC1", "C2DE")

df5 <- rbind(w2627, df5)

# Add one each wave
series2 <- create_series('2009' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(1:162))

df6 <- cbind(series2, df5) 
df6$date <- as.factor(df6$date)
colnames(df6) <- c()

# sheet_write(df6, "1qu8VoRtEdQgq38EfzYfIbIqV6_okgW0I2OItSF6kpW4", sheet = 'Sheet1')

##################
# Harm reduction #
##################

# Some missing waves for cutting down and e-cigs, and then 0.00 values for NRT waves 67, 69, 71, 73, 75, 77. Struggling to reconcile...

cigsmok <- sts_eng %>% subset(cigsmok == 1 & xwave > 32)

cutdown_df1 <- t(colPerc(xtabs(cigsmok$weight0 ~ cigsmok$cutdown + cigsmok$xwave)))
cutdown_df2 <- cutdown_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

cdnrt_df1 <- t(colPerc(xtabs(cigsmok$weight0 ~ cigsmok$cdnrtc + cigsmok$xwave)))
cdnrt_df2 <- cdnrt_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) %>%
  slice(-c(35, 37, 39, 41, 43, 45))

cdecig_df1 <- t(colPerc(xtabs(cigsmok$weight0 ~ cigsmok$cdecig + cigsmok$xwave)))
cdecig_df2 <- cdecig_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- cbind(cutdown_df2, cdnrt_df2, cdecig_df2)
colnames(df3) <- c("Cuttingdown", "NRT", "Ecig")

df4 <- df3 %>% mutate(Avg.cd = avg.last.3(Cuttingdown)) %>%
  mutate(Avg.cdnrt = avg.last.3(NRT)) %>%
  mutate(Avg.cdecig = avg.last.3(Ecig)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.cd, Avg.cdnrt, Avg.cdecig) %>%
  slice(-c(1:2))

colnames(df4) <- c("Cutting down", "Using NRT to cut down", "Using ecigs to cut down")

# Add one each wave
series3 <- create_series('2009' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(-c(1:8)) %>%
  slice(c(7:154))

df5 <- cbind(series3, df4) 
df5$date <- as.factor(df5$date)
tail(df5)

#sheet_write(df5, "1qu8VoRtEdQgq38EfzYfIbIqV6_okgW0I2OItSF6kpW4", sheet = 'Sheet1')

