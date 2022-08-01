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
ats_wales <- sts %>% subset(gore == 10)

# drive_auth()
# gs4_auth(token = drive_token())

# gs4_auth(email = "smokingchartdata@gmail.com")

avg.last.3 <- function (x) if (length(x) < 3) rep(NA, length(x)) else rollmeanr(x, 3, fill = NA)

# INCREASE BY 1 EACH MONTH
series <- create_series('2020' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(12:30))

#############################################################
# Prevalence of increasing and higher risk drinking (AUDIT) #
#############################################################

A <- ("")
B <- ("")
C <- ("")
D <- ("")

row1 <- cbind(A, B, C, D)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D")

row2 <- c("Month", "% higher risk drinkers (3 month moving average)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("", "All", "ABC1", "C2DE")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)


# Create % dataframe
df1 <- t(colPerc(xtabs(ats_wales$weight_wales ~ ats_wales$highriskaudit + ats_wales$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_wal <- ats_wales %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$highriskaudit + abc1_wal$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_wal <- ats_wales %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$highriskaudit + c2de_wal$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, ab_df2, cd_df2)
colnames(df4) <- c("All", "ABC1", "C2DE")

# 3 month MA
df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.ABC1 = avg.last.3(ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1,2))

df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)

colnames(df6) <- c("A", "B", "C", "D")
df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "1BW6uivGW5S0fN7cKBFwanYE00odfTF8NQc82PM0_UyA", sheet = 'Sheet1')

#############################################################
# Prevalence of increasing and higher risk drinking (AUDITC) #
#############################################################

A <- ("")
B <- ("")
C <- ("")
D <- ("")

row1 <- cbind(A, B, C, D)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D")

row2 <- c("Month", "% higher risk drinkers (3 month moving average)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("", "All", "ABC1", "C2DE")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

# Create % dataframe
df1 <- t(colPerc(xtabs(ats_wales$weight_wales ~ ats_wales$highriskauditc + ats_wales$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_wal <- ats_wales %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$highriskauditc + abc1_wal$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_wal <- ats_wales %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$highriskauditc + c2de_wal$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, ab_df2, cd_df2)
colnames(df4) <- c("All", "ABC1", "C2DE")

# 3 month MA
df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.ABC1 = avg.last.3(ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1,2))

df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)

colnames(df6) <- c("A", "B", "C", "D")
df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "11Y4fQiTCErYgyQB3jhNIeAD3D96gMFTOeS9W4jn0lZo", sheet = 'Sheet1')


############################################
# Currently trying to restrict consumption #
############################################

A <- ("")
B <- ("")
C <- ("")
D <- ("")

row1 <- cbind(A, B, C, D)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D")

row2 <- c("Month", "% of higher risk drinkers currently trying to restrict consumption (3 month moving average)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("", "All", "ABC1", "C2DE")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

auditc <- ats_wales %>% subset(highriskauditc == 1)

# Create % dataframe
df1 <- t(colPerc(xtabs(auditc$weight_wales ~ auditc$alccutdown + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_wal <- auditc %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$alccutdown + abc1_wal$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_wal <- auditc %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$alccutdown + c2de_wal$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, ab_df2, cd_df2)
colnames(df4) <- c("All", "ABC1", "C2DE")

# 3 month MA
df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.ABC1 = avg.last.3(ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1,2))

df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)

colnames(df6) <- c("A", "B", "C", "D")
df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "1sjmqBCMSfRngrarFbSjHhdjMW0gAxy_fKecGDPyTrxg", sheet = 'Sheet1')

##########################
# Motivation to cut down #
##########################

A <- ("")
B <- ("")
C <- ("")
D <- ("")

row1 <- cbind(A, B, C, D)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D")

row2 <- c("Month", "% of higher risk drinkers believing should or wanting to cut down (3 month moving average)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("", "All", "ABC1", "C2DE")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

# Create % dataframe
df1 <- t(colPerc(xtabs(auditc$weight_wales ~ auditc$amotiv1to6 + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_wal <- auditc %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$amotiv1to6 + abc1_wal$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_wal <- auditc %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$amotiv1to6 + c2de_wal$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, ab_df2, cd_df2)
colnames(df4) <- c("All", "ABC1", "C2DE")

# 3 month MA
df5 <- df4 %>% mutate(Avg.all = avg.last.3(All)) %>%
  mutate(Avg.ABC1 = avg.last.3(ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1,2))

df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)

colnames(df6) <- c("A", "B", "C", "D")
df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "1Pf47G_mMIkLyzrrReijoK4wTMt_Km-KR7fU2Ugal1V8", sheet = 'Sheet1')

#############
# GP advice #
#############

A <- ("")
B <- ("")

row1 <- cbind(A, B)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B")

row2 <- c("Month", "% of higher risk drinkers who visited GP in past 12 months receiving advice to cut down (3 month moving average)")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B")

row3 <- c("", "All")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B")

headings <- rbind(row1, row2, row3)

# Create % dataframe
df1 <- t(colPerc(xtabs(auditc$weight_wales ~ auditc$alcgpadivce + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

colnames(df2) <- c("All")

# 3 month MA
df3 <- df2 %>% mutate(All = avg.last.3(All)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  slice(-c(1,2))

df4 <- cbind(series, df3)
df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B")

df5 <- rbind(headings, df4)
colnames(df5) <- c()

sheet_write(df5, "15a4PNazmzbsdtl0cSKJLBbwPFfWLAGRQrqN0uy5_58Y", sheet = 'Sheet1')

##########################################
# Past-year attempts to cut down or stop #
##########################################

# Need to allow NAs for first three waves of ALL values. Same as EC trends code.

### At least one attempt in past 12 months

A <- ("")
B <- ("")
C <- ("")
D <- ("")

row1 <- cbind(A, B, C, D)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D")

row2 <- c("Month", "% of higher risk drinkers who made attempt to cut down in previous 12 months (3 month moving average)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("", "All", "ABC1", "C2DE")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

headings <- rbind(row1, row2, row3)

# Create % dataframe
df1 <- t(colPerc(xtabs(auditc$weight_wales ~ auditc$tryalclyc2 + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

abc1_wal <- auditc %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$tryalclyc2 + abc1_wal$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2de_wal <- auditc %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$tryalclyc2 + c2de_wal$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df2, ab_df2, cd_df2)
colnames(df4) <- c("All_AE", "All_ABC1", "All_C2DE")

# 3 month MA
df5 <- df4 %>% mutate(Avg.all = avg.last.3(All_AE)) %>%
  mutate(Avg.ABC1 = avg.last.3(All_ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(All_C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1,2))

df6 <- cbind(series, df5) 
df6$date <- as.factor(df6$date)

colnames(df6) <- c("A", "B", "C", "D")
df6 <- rbind(headings, df6)
colnames(df6) <- c()

sheet_write(df6, "1IuzNiSSz1-GQkQE20M-Hkgjj-3pX97mZ3yotxugoTEE", sheet = 'Sheet1')

### At least one SERIOUS attempt in past 12 months

# Create % dataframe
df6 <- t(colPerc(xtabs(auditc$weight_wales ~ auditc$tryalclyc + auditc$xwave)))
df7 <- df6 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

ab_df3 <- t(colPerc(xtabs(abc1_wal$weight_wales ~ abc1_wal$tryalclyc + abc1_wal$xwave)))
ab_df4 <- ab_df3 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

cd_df3 <- t(colPerc(xtabs(c2de_wal$weight_wales ~ c2de_wal$tryalclyc + c2de_wal$xwave)))
cd_df4 <- cd_df3 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df8 <- cbind(df7, ab_df4, cd_df4)
colnames(df8) <- c("Serious_AE", "Serious_ABC1", "Serious_C2DE")

# 3 month MA
df9 <- df8 %>% mutate(Avg.all = avg.last.3(Serious_AE)) %>%
  mutate(Avg.ABC1 = avg.last.3(Serious_ABC1)) %>%
  mutate(Avg.C2DE = avg.last.3(Serious_C2DE)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.all, Avg.ABC1, Avg.C2DE) %>%
  slice(-c(1,2))

df9 <- cbind(series, df9) 
df9$date <- as.factor(df9$date)

colnames(df9) <- c("A", "B", "C", "D")
df9 <- rbind(headings, df9)
colnames(df9) <- c()

sheet_write(df9, "1FzdsqUyG1OSyabfHc04jtDJgyvMSBbJ5aTwIHQk6vJo", sheet = 'Sheet1')

#################################
# Support in past-year attempts #
#################################

# Need to allow NAs for first three waves of ALL values. Same as EC trends code.

A <- ("")
B <- ("")
C <- ("")

row1 <- cbind(A, B, C)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C")

row2 <- c("Month", "% of higher risk drinkers making attempts to cut down in past 12 months who used  support (3 month moving average)", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C")

row3 <- c("", "All", "Serious")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

headings <- rbind(row1, row2, row3)

### In recent attempt 

tryalclyc2 <- ats_wales %>% subset(tryalclyc2 == 1)

# Create % dataframe
df1 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$anyalcsupport2 + tryalclyc2$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

colnames(df2) <- c("All")

# 3 month MA
df3 <- df2 %>% mutate(All = avg.last.3(All)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  slice(-c(1,2))

df4 <- cbind(series, df3)

### In SERIOUS attempt

tryalclyc <- ats_wales %>% subset(tryalclyc == 1)

# Create % dataframe
df5 <- t(colPerc(xtabs(tryalclyc$weight_wales ~ tryalclyc$anyalcsupport + tryalclyc$xwave)))
df6 <- df5 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

colnames(df6) <- c("Serious")

# 3 month MA
df7 <- df6 %>% mutate(Serious = avg.last.3(Serious)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  slice(-c(1,2))

df8 <- cbind(df4, df7) 
df8$date <- as.factor(df8$date)
colnames(df8) <- c("A", "B", "C")

df8 <- rbind(headings, df8)
colnames(df8) <- c()

sheet_write(df8, "1PJ1dfvSS9QafBJb_PaBSqv2vG9R40aTmm2pM25VfsUY", sheet = 'Sheet1')


###################################
# Triggers for past-year attempts #
###################################

A <- ("")
B <- ("")
C <- ("")
D <- ("")
E <- ("")
F <- ("")
G <- ("")
H <- ("")
I <- ("")
J <- ("")
K <- ("")
L <- ("")

row1 <- cbind(A, B, C, D, E, F, G, H, I, J, K, L)
row1 <- row1 %>% as.data.frame()
colnames(row1) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

row2 <- c("Month", "% of higher risk drinkers who made serious attempt to cut down permanently in previous 12 months", "", "", "", "", "", "", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

row3 <- c("", "Weight loss", "Future health concern", "Fitness", "Current health problem", "Comment by family", "GP advice", "Too expensive", "Detox", "Govt advert", "Dry January", "Baby/pregnancy")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

headings <- rbind(row1, row2, row3)


# Weight loss
df_alcmot10 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot10 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot10) <- c("Weight loss")

df_alcmot10 <- df_alcmot10 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Future health concern
df_alcmot6 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot6 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot6) <- c("Future health concern")

df_alcmot6 <- df_alcmot6 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Fitness
df_alcmot9 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot9 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot9) <- c("Fitness")

df_alcmot9 <- df_alcmot9 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Current heath problem
df_alcmot5 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot5 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot5) <- c("Current health problem")

df_alcmot5 <- df_alcmot5 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Comment by family
df_alcmot7 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot7 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot7) <- c("Comment by family")

df_alcmot7 <- df_alcmot7 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# GP advice
df_alcmot1 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot1 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot1) <- c("GP advice")

df_alcmot1 <- df_alcmot1 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Too expensive
df_alcmot3 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot3 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot3) <- c("Too expensive")

df_alcmot3 <- df_alcmot3 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Detox
df_alcmot11 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot11 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot11) <- c("Detox")

df_alcmot11 <- df_alcmot11 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Govt advert
df_alcmot2 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot2 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot2) <- c("Govt advert")

df_alcmot2 <- df_alcmot2 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Dry January
df_alcmot19 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot19 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot19) <- c("Dry January")

df_alcmot19 <- df_alcmot19 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Baby/pregnant
df_alcmot17 <- t(colPerc(xtabs(tryalclyc2$weight_wales ~ tryalclyc2$alcmot17 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df_alcmot17) <- c("Baby/pregnant")

df_alcmot17 <- df_alcmot17 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Function for binding columns with different numbers of rows
cbind.fill<-function(...){
  nm <- list(...) 
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

df4 <- cbind.fill(df_alcmot10, df_alcmot6, df_alcmot9, df_alcmot5, df_alcmot7, df_alcmot1, df_alcmot3, df_alcmot11, df_alcmot2, df_alcmot19, df_alcmot17)
colnames(df4) <- c("Weight loss", "Future health concern", "Fitness", "Current health problem", "Comment by family", "GP advice", "Too expensive", "Detox", 
                   "Govt advert", "Dry January", "Baby/pregnancy")

df5 <- as.matrix(df4)  
df6 <- df5[nrow(df5):1, ] %>%
  as.data.frame() # Flip dataframe correct way around again (latest wave at bottom of datframe)
df6 <- round(df6, digits = 1)

colnames(df6)<-c("Weightloss","Futurehealthconcern","Fitness","Currenthealthproblem","Commentbyfamily","GPadvice","Tooexpensive","Detox",
                 "Govtadvert","DryJanuary","Baby/pregnancy")

# 3 month MA
df7 <- df6 %>% mutate(Avg.weight = avg.last.3(Weightloss)) %>%
  mutate(Avg.health = avg.last.3(Futurehealthconcern)) %>%
  mutate(Avg.fit = avg.last.3(Fitness)) %>%
  mutate(Avg.currhealth = avg.last.3(Currenthealthproblem)) %>%
  mutate(Avg.fam = avg.last.3(Commentbyfamily)) %>%
  mutate(Avg.gp = avg.last.3(GPadvice)) %>%
  mutate(Avg.exp = avg.last.3(Tooexpensive)) %>%
  mutate(Avg.det = avg.last.3(Detox)) %>%
  mutate(Avg.govt = avg.last.3(Govtadvert)) %>%
  mutate(Avg.dry = avg.last.3(DryJanuary)) %>%
  mutate(Avg.bab = avg.last.3(`Baby/pregnancy`)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(Avg.weight, Avg.health, Avg.fit, Avg.currhealth, Avg.fam, Avg.gp, Avg.exp, Avg.det, Avg.govt, Avg.dry, Avg.bab) %>%
  slice(-c(1,2))

df8 <- cbind(series, df7)
df8$date <- as.factor(df8$date)
colnames(df8) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

df9 <- rbind(headings, df8)
colnames(df9) <- c()

sheet_write(df9, "1N6pwM7IOX2Q1mKQGKa7T0VogMgc-3aMgNoFMgVUkUOU", sheet = 'Sheet1')
