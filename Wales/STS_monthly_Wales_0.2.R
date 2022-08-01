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
sts_wal <- sts %>% subset(gore == 10)

# Function for 3 month rolling average
avg.last.3 <- function (x) if (length(x) < 3) rep(NA, length(x)) else rollmeanr(x, 3, fill = NA)

# INCREASE BY 1 EACH MONTH
series <- create_series('2020' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(12:30))

################################
# CIGARETTE SMOKING PREVALENCE #
################################

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
df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$cigsmok + sts_wal$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_wal <- sts_wal %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$cigsmok + abc1_wal$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_wal <- sts_wal %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$cigsmok + c2de_wal$xwave)))
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



df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)
colnames(df6) <- c("A", "B", "C", "D")

df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "1iCqa44Ez3-PfYkJWCDBmu_CgRd9qUuSiqTpe4SOJNgY", sheet = 'Sheet1')

#######################################
# CIGARETTE SMOKING PREVALENCE BY AGE #
#######################################

row3 <- c("", "All", "Under 35", "Over 35")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

under35 <- sts_wal %>% subset(over35 == 0)

under35_df1 <- t(colPerc(xtabs(under35$weight_wales ~ under35$cigsmok + under35$xwave)))
under35_df2 <- under35_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

over35 <- sts_wal %>% subset(over35 == 1)

over35_df1 <- t(colPerc(xtabs(over35$weight_wales ~ over35$cigsmok + over35$xwave)))
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
colnames(df6) <- c()

sheet_write(df6, "1ZBQqeS0hehrCelhE_FqcBP97HFhcSoI3svUpgQKMg2Y", sheet = 'Sheet1')

#######################################
# CIGARETTE SMOKING PREVALENCE BY SEX #
#######################################

row3 <- c("", "All", "Men", "Women")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

stsmen <- sts_wal %>% subset(sexz == 1)

men_df1 <- t(colPerc(xtabs(stsmen$weight_wales ~ stsmen$cigsmok + stsmen$xwave)))
men_df2 <- men_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

stswomen <- sts_wal %>% subset(sexz == 2)

women_df1 <- t(colPerc(xtabs(stswomen$weight_wales ~ stswomen$cigsmok + stswomen$xwave)))
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
colnames(df6) <- c("A", "B", "C", "D")

df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "1LHQXT0urrzuVAbFqwm03nxrUnFQvD5W0jAlj0j-rWHI", sheet = 'Sheet1')

####################################
# ATTEMPTS TO STOP BY SOCIAL GRADE #
####################################

row3 <- c("", "All", "ABC1", "C2DE")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

smokly <- sts_wal %>% subset(smokly == 1)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(smokly$weight_wales ~ smokly$trylyc + smokly$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_wal <- smokly %>% subset(randm == 0)
abc1_wal <- smokly %>% filter(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$trylyc + abc1_wal$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_wal <- smokly %>% subset(randm == 1)
c2de_wal <- smokly %>% filter(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$trylyc + c2de_wal$xwave)))
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
colnames(df6) <- c("A", "B", "C", "D")

df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "105OoFVGoGnJvrNh-DmTrlzJsW6np2xur_YnCsNV-o2Y", sheet = 'Sheet1')

#################################
# SUPPORT USED IN QUIT ATTEMPTS #
#################################

A <- ("")
B <- ("")
C <- ("")
D <- ("")
E <- ("")
F <- ("")

row1 <- cbind(A, B, C, D, E, F)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D", "E", "F")

row2 <- c("Month", "% of those trying to stop in the past year who used support (3 month moving average)", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F")

row3 <- c("", "E-cig", "NRT 0TC", "MedRx", "NHS", "Nothing")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E", "F")

headings <- rbind(row1, row2, row3)

ec_df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$ecig_hier + sts_wal$xwave)))
ec_df2 <- ec_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

nrt_df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$nrtotc_hier + sts_wal$xwave)))
nrt_df2 <- nrt_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

medrx_df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$medrx_hier + sts_wal$xwave)))
medrx_df2 <- medrx_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

nhs_df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$nhs_hier + sts_wal$xwave)))
nhs_df2 <- nhs_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

no_df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$nothing_hier + sts_wal$xwave)))
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
colnames(df6) <- c("A", "B", "C", "D", "E", "F")

df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "1YCFnUIl5pkDqvy1qzhbSvLwjg8tpY7J_m5wZOeTBDuU", sheet = 'Sheet1')

#################################
# GP triggered quit attempts    #
#################################

A <- ("")
B <- ("")

row1 <- cbind(A, B)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B")

row2 <- c("Month", "% of all those who smoked in the past year who reported a GP triggered quit attempt (3 month moving average)")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B")

row3 <- c("", "All")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B")

headings <- rbind(row1, row2, row3)

gp_df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$gptrigatt2 + sts_wal$xwave)))
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

df4 <- cbind(series, df3) 
df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B")

df5 <- rbind(headings, df4)
colnames(df5) <- c()

sheet_write(df5, "1J_fVewXosmSm1jTlpeIUy4i-4Uiae-ybNlhQ_3PcjGM", sheet = 'Sheet1')

######################
# Motivation to quit #
######################

A <- ("")
B <- ("")
C <- ("")
D <- ("")

row1 <- cbind(A, B, C, D)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D")

row2 <- c("Month", "% cigarette smokers wanting to stop and intending to stop soon (3 month moving average)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("", "All", "ABC1", "C2DE")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$qmotivwi + sts_wal$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_wal <- sts_wal %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$qmotivwi + abc1_wal$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_wal <- sts_wal %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$qmotivwi + c2de_wal$xwave)))
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
colnames(df6) <- c("A", "B", "C", "D")

df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "1mX10Y9gXqVdMcuwc7KwRZFCIJtBmd4mHDNE80S415l4", sheet = 'Sheet1')

##################
# Harm reduction #
##################

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

row3 <- c("", "Cutting down", "Using NRT to cut down", "Using e-cigs to cut down")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

# Create df
cigsmok <- sts_wal %>% subset(cigsmok == 1)

round(addmargins(xtabs(cigsmok$weight_wales ~ cigsmok$cutdown + cigsmok$xwave)))

cutdown_df1 <- t(colPerc(xtabs(cigsmok$weight_wales ~ cigsmok$cutdown + cigsmok$xwave)))
cutdown_df2 <- cutdown_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

cdnrt_df1 <- t(colPerc(xtabs(cigsmok$weight_wales ~ cigsmok$cdnrtc + cigsmok$xwave)))
cdnrt_df2 <- cdnrt_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

cdecig_df1 <- t(colPerc(xtabs(cigsmok$weight_wales ~ cigsmok$cdecig + cigsmok$xwave)))
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

df5 <- cbind(series, df4)
df5$date <- as.factor(df5$date)
colnames(df5) <- c("A", "B", "C", "D")

df6 <- rbind(headings, df5)
colnames(df6) <- c()

sheet_write(df6, "1c8efZtXO6Qk3zMyDZb6gEjGS-HEYrOxp9HMXZ5jg194", sheet = 'Sheet1')

