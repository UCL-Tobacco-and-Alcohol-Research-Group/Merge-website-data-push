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

#sts <- read_sav("C:/Toolkit merge files/Waves/185/omni185_39.1_65.2cot_31.3a_25.4s_recodes_71.5sa.sav")
#names(sts)[names(sts)=="@weight0"] <- "weight0" 
sts_wal <- sts %>% subset(gore == 10)

# Increase each quarter
series <- create_series('2020' ~ '2022', 'quarterly', class = 'yearmon') %>%
  slice(c(4:10))

###################################################
# Prevalence of e-cigarette and heat-not-burn use #
###################################################

n.ec <- nrow(sts_wal) %>%
  as.data.frame()
colnames(n.ec) <- c("Freqec")

n.ec$Freqec <- interaction("(N=", n.ec$Freqec, sep = "")
n.ec$Freqec <- interaction(n.ec$Freqec, ")", sep = "")

n.juul <- sts_wal %>%
  nrow() %>%
  as.data.frame()
colnames(n.juul) <- c("Freqj")

n.juul$Freqj <- interaction("(N=", n.juul$Freqj, sep = "")
n.juul$Freqj <- interaction(n.juul$Freqj, ")", sep = "")

n.htb <- sts_wal %>%
  nrow() %>%
  as.data.frame()
colnames(n.htb) <- c("Freqhtb")

n.htb$Freqhtb <- interaction("(N=", n.htb$Freqhtb, sep = "")
n.htb$Freqhtb <- interaction(n.htb$Freqhtb, ")", sep = "")

row2 <- c("", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("Quarter", "Percent", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

row4 <- c("", "E-cig", "Juul", "HTB")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D")


row4 <- cbind(row4, n.ec, n.juul, n.htb)
row4 <- row4[c(1,2,5,3,6,4,7)]
row4 <- unite(row4, "B", B:Freqec, sep = " ")
row4 <- unite(row4, "C", C:Freqj, sep = " ")
row4 <- unite(row4, "D", D:Freqhtb, sep = " ")
colnames(row4) <- c("A", "B", "C", "D")

headings <- rbind(row2, row3, row4)

# E-cigarette
df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$allecig + sts_wal$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# JUUL
df2 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$allJuul + sts_wal$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# HNB
df3 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$allHNB + sts_wal$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- cbind(df1, df2, df3)

colnames(df4) <- c("E-cig", "JUUL", "HTP")
df4 <- round(df4, digits = 1)



df5 <- cbind(series, df4)
df5$date <- as.factor(df5$date)
colnames(df5) <- c("A", "B", "C", "D")  

df6 <- rbind(headings, df5)
colnames(df6) <- c()

# PUSH to google sheet
sheet_write(df6, "1X-CqrAio1m3bVNfHB02Q5nNRoaq3DUeQSwWjt_UQhco", sheet = 'Sheet1')

##########################################################
# Nicotine use by never smokers and long-term ex-smokers #
##########################################################

never <- sts_wal %>% subset(smokstat == 0)
ltex <- sts_wal %>% subset(smokstat == 1)

n <- nrow(ltex) + nrow(never)

row2 <- c(n, "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E")

row2$A <- interaction("(N=", row2$A, sep = "")
row2$A <- interaction(row2$A, ")", sep = "")

row3 <- c("Quarter", "Percent", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E")

row4 <- c("", "Never: E-cigs", "Long-term ex: E-cigs", "Never: NRT", "Long-term ex: NRT")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E")  

headings <- rbind(row2, row3, row4)

# E-cigarette - never smokers 
df1 <- t(colPerc(xtabs(never$weight_wales ~ never$allecig + never$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# E-cig long-term ex-smoker
df2 <- t(colPerc(xtabs(ltex$weight_wales ~ ltex$allecig + ltex$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT - never smokers 
df3 <- t(colPerc(xtabs(never$weight_wales ~ never$allnrt + never$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT long-term ex-smoker
df4 <- t(colPerc(xtabs(ltex$weight_wales ~ ltex$allnrt + ltex$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df5 <- cbind(df1, df2, df3, df4)
colnames(df5) <- c("A", "B", "C", "D")
df5 <- round(df5, digits = 1)

df6 <- cbind(series, df5)
df6$date <- as.factor(df6$date)
colnames(df6) <- c("A", "B", "C", "D", "E")  

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1Gx8PxelR4G22_C8yxWVJDpqfLicIKicJ63hsWpDeczU", sheet = 'Sheet1')

###########################################################
# Nicotine use by long-term ex-smokers (> and <= 5 years) #
###########################################################

ltex_over5 <- sts_wal %>% subset(yearsquit > 5 & smokstat == 1)
ltex_under5 <- sts_wal %>% subset(yearsquit <= 5 & smokstat == 1)

# Integrate over and under 5s into dataframe ###
n <- nrow(ltex_over5) + nrow(ltex_under5)
n_o5 <- nrow(ltex_over5)
n_u5 <- nrow(ltex_under5)

row2 <- c(n_o5, n_u5, "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F")

### REPLACE WITH UNITE FUNCTION - MUCH CLEANER...https://www.rdocumentation.org/packages/tidyr/versions/0.8.3/topics/unite

row2$A <- interaction("(N=", row2$A, sep = "")
row2$A <- interaction(row2$A, " ex-smokers >5y;", sep = "")
row2$A <- interaction("", row2$A, sep = "")
row2$A <- interaction(row2$A, " N=", sep = "")
row2 <- row2 %>% mutate("F" = NULL)

row2 <- transform(row2, newcol=interaction(A,B, sep = ''))
row2$newcol <- interaction(row2$newcol, " ex-smokers <=5y)", sep = "")
row2 <- row2 %>% mutate("A" = NULL, "B" = NULL)

row2 <- transform(row2, B='')
row2 <- row2 %>% select(newcol, B, C, D, E)
colnames(row2) <- c("A", "B", "C", "D", "E")  

row3 <- c("Quarter", "Percent", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E")

row4 <- c("", "Long-term ex (<=5 yrs): E-cig", "Long-term ex (>5 yrs): E-cig", "Long-term ex (<=5 yrs): NRT", "Long-term ex (>5 yrs): NRT")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E")  

headings <- rbind(row2, row3, row4)

# E-cigarette - long-term ex <=5y
df1 <- t(colPerc(xtabs(ltex_under5$weight_wales ~ ltex_under5$allecig + ltex_under5$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# E-cigarette - long-term ex > 5y
df2 <- t(colPerc(xtabs(ltex_over5$weight_wales ~ ltex_over5$allecig + ltex_over5$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT - ong-term ex <=5y
df3 <- t(colPerc(xtabs(ltex_under5$weight_wales ~ ltex_under5$allnrt + ltex_under5$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT long-term ex-smoker > 5y
df4 <- t(colPerc(xtabs(ltex_over5$weight_wales ~ ltex_over5$allnrt + ltex_over5$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df5 <- cbind(df1, df2, df3, df4)
colnames(df5) <- c("A", "B", "C", "D")
df5 <- round(df5, digits = 1)

df6 <- cbind(series, df5)
df6$date <- as.factor(df6$date)
colnames(df6) <- c("A", "B", "C", "D", "E")  

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1bAJZ5VR5BPg_LYSuJ-6D6t_SF23Wex-ermJIxl7OBqI", sheet = 'Sheet1')

#########################################################################
# Prevalence of electronic cigarette use: smokers and recent ex-smokers #
#########################################################################

smokly <- sts_wal %>% subset(smokly == 1) 

n.ec <- nrow(smokly) %>%
  as.data.frame()
colnames(n.ec) <- c("Freqec")

n.ec$Freqec <- interaction("(N=", n.ec$Freqec, sep = "")
n.ec$Freqec <- interaction(n.ec$Freqec, ")", sep = "")

n.juul <- smokly %>% 
  nrow() %>%
  as.data.frame()
colnames(n.juul) <- c("Freqj")

n.juul$Freqj <- interaction("(N=", n.juul$Freqj, sep = "")
n.juul$Freqj <- interaction(n.juul$Freqj, ")", sep = "")

n.htb <- smokly %>% 
  nrow() %>%
  as.data.frame()
colnames(n.htb) <- c("Freqhtb")

n.htb$Freqhtb <- interaction("(N=", n.htb$Freqhtb, sep = "")
n.htb$Freqhtb <- interaction(n.htb$Freqhtb, ")", sep = "")

row2 <- c("", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E")

row3 <- c("Quarter", "Percent", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E")

row4 <- c("", "Any E-cig","Daily E-cig", "Juul", "HTP")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E")


row4 <- cbind(row4, n.ec, n.ec, n.juul, n.htb)
row4 <- row4[c(1,2,6,3,6,4,8,5,9)]
row4 <- unite(row4, "B", B:Freqec, sep = " ")
row4 <- unite(row4, "C", C:Freqec.1, sep = " ")
row4 <- unite(row4, "D", D:Freqj, sep = " ")
row4 <- unite(row4, "E", E:Freqhtb, sep = " ")
colnames(row4) <- c("A", "B", "C", "D", "E")

headings <- rbind(row2, row3, row4)

# Any E-cig
df1 <- t(colPerc(xtabs(smokly$weight_wales ~ smokly$allecig + smokly$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)


# Daily E-cig
df2 <- t(colPerc(xtabs(smokly$weight_wales ~ smokly$dailyecig + smokly$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# JUUL
df3 <- t(colPerc(xtabs(smokly$weight_wales ~ smokly$allJuul + smokly$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# HNB
df4 <- t(colPerc(xtabs(smokly$weight_wales ~ smokly$allHNB + smokly$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df5 <- cbind(df1, df2, df3, df4)
colnames(df5) <- c("A", "B", "C", "D")
df5 <- round(df5, digits = 1)

df6 <- cbind(series, df5)
df6$date <- as.factor(df6$date)
colnames(df6) <- c("A", "B", "C", "D", "E")  

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1tV8mCaUO6X2zvEuXnjKdgNzaj5oZd5tYwLlr-ZxhmtU", sheet = 'Sheet1')

###########################################################
# Proportion of e-cigarette and NRT users who are smokers #
###########################################################

## JUST RUN AFTER 4 QUARTERS (1 YEAR) OF DATA

allecig <- sts_wal %>% subset(allecig == 1)
allnrt <- sts_wal %>% subset(allnrt == 1)

n.ec <- nrow(allecig) %>%
  as.data.frame()
colnames(n.ec) <- c("Freqec")

n.ec$Freqec <- interaction("(N=", n.ec$Freqec, sep = "")
n.ec$Freqec <- interaction(n.ec$Freqec, ")", sep = "")

n.nrt <- nrow(allnrt) %>%
  as.data.frame()
colnames(n.nrt) <- c("Freqnrt")

n.nrt$Freqnrt <- interaction("(N=", n.nrt$Freqnrt, sep = "")
n.nrt$Freqnrt <- interaction(n.nrt$Freqnrt, ")", sep = "")

row2 <- c("", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C")

row3 <- c("Quarter", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "E-cigs","NRT")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")

row4 <- cbind(row4, n.ec, n.nrt)
row4 <- row4[c(1,2,4,3,5)]
row4 <- unite(row4, "B", B:Freqec, sep = " ")
row4 <- unite(row4, "C", C:Freqnrt, sep = " ")
colnames(row4) <- c("A", "B", "C")

headings <- rbind(row2, row3, row4)

# E-cigarette 
df1 <- t(colPerc(xtabs(allecig$weight_wales ~ allecig$smokstat + allecig$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT
df2 <- t(colPerc(xtabs(allnrt$weight_wales ~ allnrt$smokstat + allnrt$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- cbind(df1, df2)
df3 <-  df3 %>% subset(select = c(3, 6))
df3 <-  round(df3, digits = 1) 

df4 <- cbind(series, df3)
df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B", "C")  

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "1XzQpOVU0kGiv3CbTOeTEyL6pcNHiNwyh2AhPHvr2qQ4", sheet = 'Sheet1')

#################################################################
# Proportion of daily e-cigarette and NRT users who are smokers #
#################################################################

## JUST RUN AFTER 4 QUARTERS (1 YEAR) OF DATA

allecig <- sts_wal %>% subset(allecig == 1 & allnrt == 0 & nicpdc >2)
allnrt <- sts_wal %>% subset(allecig == 0 & allnrt == 1 & nicpdc >2)

n.ec <- nrow(allecig) %>%
  as.data.frame()
colnames(n.ec) <- c("Freqec")

n.ec$Freqec <- interaction("(N=", n.ec$Freqec, sep = "")
n.ec$Freqec <- interaction(n.ec$Freqec, ")", sep = "")

n.nrt <- nrow(allnrt) %>%
  as.data.frame()
colnames(n.nrt) <- c("Freqnrt")

n.nrt$Freqnrt <- interaction("(N=", n.nrt$Freqnrt, sep = "")
n.nrt$Freqnrt <- interaction(n.nrt$Freqnrt, ")", sep = "")

row2 <- c("", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C")

row3 <- c("Quarter", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "E-cigs","NRT")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")

row4 <- cbind(row4, n.ec, n.nrt)
row4 <- row4[c(1,2,4,3,5)]
row4 <- unite(row4, "B", B:Freqec, sep = " ")
row4 <- unite(row4, "C", C:Freqnrt, sep = " ")
colnames(row4) <- c("A", "B", "C")

headings <- rbind(row2, row3, row4)

# E-cigarette 
df1 <- t(colPerc(xtabs(allecig$weight_wales ~ allecig$smokstat + allecig$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT
df2 <- t(colPerc(xtabs(allnrt$weight_wales ~ allnrt$smokstat + allnrt$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- cbind(df1, df2)
df3 <-  df3 %>% subset(select = c(3, 6))

colnames(df3) <- c("A", "B")

df3 <-  round(df3, digits = 1) 

df4 <- cbind(series, df3)
df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B", "C")  

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "1NMoFQE7dFn6p8tYDI_pdstaiXUvYWB-rOftc9MHNQbw", sheet = 'Sheet1')

############################
# Electronic cigarette use #
############################

freq <- sts_wal %>% subset(nicpd > 0 & allecig == 1 & allnrt == 0 & smokly == 1)

n <- nrow(freq)

row2 <- c(n, "e-cigarette users not using NRT)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Frequency", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "Smoker", "Ex-smoker")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

##

df1 <- (colPerc(xtabs(freq$weight_wales ~ freq$nicpdc + freq$smoker))) %>%
  as.data.frame() %>%
  slice(-c(7))

rownames(df1) <- c("Less than weekly", "Weekly but not daily", "1 per day", 
                   "2-5 per day", "6-10 per day", "11+ per day")
colnames(df1) <- c("Ex-smoker", "Smoker")

df1 <-  round(df1, digits = 1)
df1 <- df1[c(2,1)]

Frequency <- c("Less than weekly", "Weekly but not daily", "1 per day", 
               "2-5 per day", "6-10 per day", "11+ per day")

df2 <- cbind(Frequency, df1)
colnames(df2) <- c("A", "B", "C")

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# PUSH to google sheet
sheet_write(df3, "1inUj1akDJuhcLMGaBgiLysqVcFd7Gjtm5ddGlNxvcbM", sheet = 'Sheet1')

######################################
# Characteristics of the e-cigarette #
######################################

smokly <- sts_wal %>% subset(smokly == 1)
smokly.ec <- smokly %>% subset(qimw118_2 == 2 | qimw118_2 == 3 | qimw118_2 == 4)

n <- nrow(smokly.ec)

row2 <- c(n, "e-cigarette users who smoke or who stopped in the past year)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Type", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "Smoker", "Ex-smoker")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

df1 <- (colPerc(xtabs(smokly$weight_wales ~ smokly$qimw118_2 + smokly$smokstat))) %>%
  as.data.frame() %>%
  slice(-c(2, 3, 4))
df1 <- df1[c(2,1)]
rownames(df1) <- (c("Nicotine containing"))

df2 <- (colPerc(xtabs(smokly$weight_wales ~ smokly$qimw118_3 + smokly$smokstat))) %>%
  as.data.frame() %>%
  slice(-c(6))
rownames(df2) <- c("Disposable", "Rechargeable with pre-filled cartridges", "Rechargeable with tank to refill",
                   "Mod system", "Unknown")
df2 <- df2[c(2,1)]


type <- c("Nicotine containing", "Disposable", "Rechargeable with pre-filled cartridges", 
          "Rechargeable with tank to refill", "Mod system", "Unknown")

df3 <- rbind(df1, df2)
colnames(df3) <- c("Smoker", "Ex-smoker")
df3 <-  round(df3, digits = 1)

df4 <- cbind(type, df3)

colnames(df4) <- c("A", "B", "C")

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "1SbDhIqg2RFAFBqErwfLIwNUNF_Q1UpzAFD5j5c3Ji9Q", sheet = 'Sheet1')

############
# E-liquid #
############

smokly.el <- smokly %>% subset(qimw118_4 == 2 | qimw118_4 == 3 | qimw118_4 == 4 | qimw118_4 == 5 | qimw118_4 == 6)

n <- nrow(smokly.el)

row2 <- c(n, "nicotine containing e-cigarette users who smoke or who stopped in the past year)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Strength", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "Smoker", "Ex-smoker")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)


df1 <- (colPerc(xtabs(smokly$weight_wales ~ smokly$qimw118_4 + smokly$smokstat))) %>%
  as.data.frame() %>%
  slice(-c(6))
rownames(df1) <- c("6mg (0.6%) or less", "7mg (0.7%) to 11mg (1.1%)", "12mg (1.2%) to 19mg (1.9%)",
                   "20mg (2.0%) or more", "Unknown")
df1 <- df1[c(2,1)]
colnames(df1) <- c("Smoker", "Ex-smoker")
df1 <-  round(df1, digits = 1)

conc <- c("6mg (0.6%) or less", "7mg (0.7%) to 11mg (1.1%)", "12mg (1.2%) to 19mg (1.9%)",
          "20mg (2.0%) or more", "Unknown")

df2 <- cbind(conc, df1)
colnames(df2) <- c("A", "B", "C")

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# PUSH to google sheet
sheet_write(df3, "1zlugRzB9wEZHm1JEVYUIkBtUMJM52mHT6q-WmJaKhkw", sheet = 'Sheet1')

##########
# Source #
##########

attributes(sts_wal$qimw118_5)

smokly.source <- smokly %>% subset(qimw118_5 == 2 | qimw118_5 == 3 | qimw118_5 == 4 | qimw118_5 == 5 | qimw118_5 == 6 | qimw118_5 == 7 | qimw118_5 == 8
                                   | qimw118_5 == 9 | qimw118_5 == 10)
n <- nrow(smokly.source)

row2 <- c(n, "e-cigarette users who smoke or who stopped in the past year)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Source", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "Smoker", "Ex-smoker")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

df1 <- (colPerc(xtabs(smokly$weight_wales ~ smokly$qimw118_5 + smokly$smokstat))) %>%
  as.data.frame() %>%
  slice(-c(9))
rownames(df1) <- c("Specialist ('vape shop')", "Online specialist", "Other online retailer",
                   "Newsagent", "Petrol garage shop", "Supermarket", "Other", "Unknown") # No smoklys bought from friends. Add in later if need

df1 <- df1[c(2,1)]
colnames(df1) <- c("Smoker", "Ex-smoker")
df1 <-  round(df1, digits = 1)

source <- c("Specialist ('vape shop')", "Online specialist", "Other online retailer",
            "Newsagent", "Petrol garage shop", "Supermarket", "Other", "Unknown")

df2 <- cbind(source, df1)
colnames(df2) <- c("A", "B", "C")

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# PUSH to google sheet
sheet_write(df3, "1hSMT71xxSPh7q2eKRBCj39QEGK-648betowNbQ_58A4", sheet = 'Sheet1')

#################################################################
# Electronic cigarette and NRT use across the age range in 2021 # 
#################################################################

# UPDATE ANNUALLY

sts2021 <- sts_wal %>% subset(smokly == 1 & xyear == 2021)

n <- nrow(sts2021)

row2 <- c(n, "adults who smoke or who stopped in the past year and were surveyed in 2021)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Age", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "E-cig user", "NRT user")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

df1 <- t(colPerc(xtabs(sts2021$weight_wales ~ sts2021$allecig + sts2021$agez))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) %>%
  slice(-c(7))

df2 <- t(colPerc(xtabs(sts2021$weight_wales ~ sts2021$allnrt + sts2021$agez))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) %>%
  slice(-c(7))

df3 <- cbind(df1, df2)
df3 <- (round(df3, digits = 1))
colnames(df3) <- c("E-cig user", "NRT user")

age <- c("16-24", "25-34", "35-44", "45-54", "55-64", "65+")

df4 <- cbind(age, df3)
colnames(df4) <- c("A", "B", "C")

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "1MP_oTGYOesNsNP59wi9w9_0tTmjS-S4ysW3YESu3exg", sheet = 'Sheet1')

##########################################################
# Electronic cigarette and NRT use men and women in 2021 # 
##########################################################

# UPDATE ANNUALLY

n <- nrow(sts2021)

row2 <- c(n, "adults who smoke or who stopped in the past year and were surveyed in 2021)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Sex", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "E-cig user", "NRT user")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

df1 <- t(colPerc(xtabs(sts2021$weight_wales ~ sts2021$allecig + sts2021$sexz))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) %>%
  slice(-c(3))

df2 <- t(colPerc(xtabs(sts2021$weight_wales ~ sts2021$allnrt + sts2021$sexz))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) %>%
  slice(-c(3))

df3 <- cbind(df1, df2)
df3 <- (round(df3, digits = 1))
colnames(df3) <- c("E-cig user", "NRT user")

sex <- c("Men", "Women")

df4 <- cbind(sex, df3)
colnames(df4) <- c("A", "B", "C")

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "15j7sZPIwTXq3B0_b7JxvzcdcfSiMFUzkcha876A7RFg", sheet = 'Sheet1')

###########################################################
# Electronic cigarette across the social gradient in 2021 # 
###########################################################

# UPDATE ANNUALLY

n <- nrow(sts2021)
missingsgz <- count(is.na(sts2021$sgz))
n2 <- n - missingsgz

row2 <- c(n2, "adults who smoke or who stopped in the past year and were surveyed in 2021)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Social grade", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "E-cig user", "NRT user")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

df1 <- t(colPerc(xtabs(sts2021$weight_wales ~ sts2021$allecig + sts2021$sgz))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) 

df2 <- t(colPerc(xtabs(sts2021$weight_wales ~ sts2021$allnrt + sts2021$sgz))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- cbind(df1, df2)
df3 <- (round(df3, digits = 1))
colnames(df3) <- c("E-cig user", "NRT user")

sgrade <- c("AB", "C1", "C2", "D", "E")

df4 <- cbind(sgrade, df3)
colnames(df4) <- c("A", "B", "C")

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "1koiM8Zy4maexZxnLLKOmRidgpEp2WCAKRWVSv88RUXM", sheet = 'Sheet1')

##########################################
# Use of nicotine products while smoking # 
##########################################

smoker <- sts_wal %>% subset(smoker == 1 & quarter2 > 1)
smoker.ec <- smoker %>% subset(allecig == 0 | allecig == 1)

n <- nrow(smoker.ec)

row2 <- c(n, "smokers)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Quarter", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "E-cigs", "NRT")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

# E-cig
df1 <- t(colPerc(xtabs(smoker$weight_wales ~ smoker$allecig + smoker$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

#NRT
df2 <- t(colPerc(xtabs(smoker$weight_wales ~ smoker$allnrt + smoker$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- cbind(df1, df2)
colnames(df3) <- c("E-cigs", "NRT")
df3 <- round(df3, digits = 1) 

df4 <- cbind(series, df3)
df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B", "C")  

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "1PEBTC7voWwJyrn-RYqK6jEoaAEjqN64ZvpqHPvDyVRc", sheet = 'Sheet1')

#################################################
# Use of nicotine products in recent ex-smokers # 
#################################################

recentex <- sts_wal %>% subset(smoker == 0 & smokly == 1 & quarter2 > 1)
recentex.ec <- recentex %>% subset(allecig == 0 | allecig == 1)

n <- nrow(recentex.ec)

row2 <- c(n, "adults who stopped in the past year)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Quarter", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "E-cigs", "NRT")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

# E-cig
df1 <- t(colPerc(xtabs(recentex$weight_wales ~ recentex$allecig + recentex$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT

df2 <- t(colPerc(xtabs(recentex$weight_wales ~ recentex$allnrt + recentex$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- cbind(df1, df2)
colnames(df3) <- c("E-cigs", "NRT")
df3 <- round(df3, digits = 1) 

df4 <- cbind(series, df3)

df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B", "C")  

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# Push to google sheet
sheet_write(df5, "1QJp1_5msgOLiVgvIpX2NMEE9x8jaf1p7LY_zvQsQOMM", sheet = 'Sheet1')

#########################################
# Aids used in most recent quit attempt #
#########################################

# Make sure google sheets variable heading is correct for e-cigs and NRT

names(sts_wal)[names(sts_wal)=="@q632b1o"] <- "q632b1o"
names(sts_wal)[names(sts_wal)=="@q632b1p"] <- "q632b1p"

trylyc <- sts_wal %>% subset(trylyc == 1 & xwave > 32)

n.all <- nrow(trylyc)

row2 <- c(n.all, "adults who smoke and tried to stop or who stopped in the past year)", "", "", "", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C", "D", "E", "F", "G", "H")

#JUUL
n.juul <- trylyc %>% 
  nrow() %>%
  as.data.frame()
colnames(n.juul) <- c("Freqj")

n.juul$Freqj <- interaction("(N=", n.juul$Freqj, sep = "")
n.juul$Freqj <- interaction(n.juul$Freqj, ")", sep = "")

# HTP
n.htb <- trylyc %>%
  nrow() %>%
  as.data.frame()
colnames(n.htb) <- c("Freqhtb")

n.htb$Freqhtb <- interaction("(N=", n.htb$Freqhtb, sep = "")
n.htb$Freqhtb <- interaction(n.htb$Freqhtb, ")", sep = "")

row3 <- c("Quarter", "Percent of smokers trying to stop", "", "", "", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E", "F", "G", "H")

row4 <- c("", "E-cig", "NRT OTC", "NRT Rx", "Champix", "Beh'l supp", "HTP", "Juul")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E", "F", "G", "H")

row4 <- cbind(row4, n.juul, n.htb)
row4 <- row4[c(1,2,5,3,6,4,7,10,8,9)]
row4 <- unite(row4, "G", G:Freqhtb, sep = " ")
row4 <- unite(row4, "H", H:Freqj, sep = " ")
colnames(row4) <- c("A", "B", "C", "D", "E", "F", "G", "H")

headings <- rbind(row2, row3, row4)

##

# E-cigs
df1 <- t(colPerc(xtabs(trylyc$weight_wales ~ trylyc$ecig1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df1) <- c("E-cigs")

# NRT OTC
df2 <- t(colPerc(xtabs(trylyc$weight_wales ~ trylyc$nrto1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df2) <- c("NRT OTC")

# NRT Rx
df3 <- t(colPerc(xtabs(trylyc$weight_wales ~ trylyc$nrtr1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df3) <- c("NRT Rx")

# Champix
df4 <- t(colPerc(xtabs(trylyc$weight_wales ~ trylyc$cha1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df4) <- c("Champix")

# Behavioural support
df5 <- t(colPerc(xtabs(trylyc$weight_wales ~ trylyc$beh1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df5) <- c("Behavioual support")

# HTP
df6 <- t(colPerc(xtabs(trylyc$weight_wales ~ trylyc$q632b1o + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df6) <- c("HNB")

# Juul
df7 <- t(colPerc(xtabs(trylyc$weight_wales ~ trylyc$q632b1p + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df7) <- c("Juul")

df8 <- cbind(df1, df2, df3, df4, df5) # NO HTP OR JUUL USERS IN WALES YET...MONITOR
colnames(df8) <- c("E-cig", "NRT OTC", "NRT Rx", "Champix", "Behavioural support")
df8 <- round(df8, digits = 1)

df9 <- cbind(series, df8)
df9$date <- as.factor(df9$date)
colnames(df9) <- c("A", "B", "C", "D", "E", "F")  

headings2 <- headings %>% select (1:6)

df10 <- rbind(headings2, df9)
colnames(df10) <- c()

# Push to google sheet
sheet_write(df10, "1P05wNVIzdUEFOYE3m1XrP6vfFQHvsBrfkjheByt_ufg", sheet = 'Sheet1')

##########################################
# Current e-cigarette use after quitting #
##########################################

# CURRENTLY NO E-CIGARETTE USE AFTER QUITTING IN WALES...MONITOR

afterq <- sts_wal %>% subset(trylyc == 1 & quarter2 > 0 & ecig1 == 0 & smokly == 1 & smoker == 0)
afterq.ec <- afterq %>% subset(allecig == 1 | allecig == 0)

n <- nrow(afterq.ec)

row2 <- c(n, "adults who stopped in the past year and did not report using an e-cigarette to help during the quit attempt)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Year", "Percent currently using e-cigarette", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "Any", "Daily")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

# Any e-cig
df1 <- t(colPerc(xtabs(afterq$weight_wales ~ afterq$allecig + afterq$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df1) <- c("Any")
df1 <- round(df1, digits = 1)

# Daily e-cig
df2 <- t(colPerc(xtabs(afterq$weight_wales ~ afterq$dailyecig + afterq$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df2) <- c("Daily")
df2 <- round(df2, digits = 1)

df3 <- cbind(df1, df2) %>% 
  slice(-c(1))

year <- c(2021, 2022)

df3 <- cbind(year, df3)
colnames(df3) <- c("A", "B", "C")

df4 <- rbind(headings, df3)
colnames(df4) <- c()

# Push to google sheet
sheet_write(df4, "1aLOp39U0lJEr3NWBs0ZT8oXUYsZw6emZ_lJSDlFoEV0", sheet = 'Sheet1')

########################################
# Prevalence of nicotine/cigarette use #
########################################

cignic <- sts_wal %>% subset(cignic3 > -1 & xwave > 85)
cignic.n <- cignic %>% subset(smoker == 0 | smoker == 1)

n <- nrow(cignic)

row2 <- c(n, "adults)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Quarter", "Percent smoking cigs or using nicotine", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "Cigarettes", "Nicotine or cigarettes")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

#
df1 <- t(colPerc(xtabs(cignic$weight_wales ~ cignic$cigsmok + cignic$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df1) <- c("Cigarettes")
df1 <- round(df1, digits = 1)

#
df2 <- t(colPerc(xtabs(cignic$weight_wales ~ cignic$cignic3 + cignic$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df2) <- c("Nicotine or cigarettes")
df2 <- round(df2, digits = 1)

df3 <- cbind(df1, df2) 

df4 <- cbind(series, df3)
df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B", "C")  

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# Push to google sheet
sheet_write(df5, "1F1sFYF_2MRxyzE_joQ8_BfJ6HcGRHzQ1AxqPiSGxCkU", sheet = 'Sheet1')

#############################################################
# Harm perceptions of e-cigarettes compared with cigarettes #
#############################################################

smoker_noec <- sts_wal %>% subset(smoker == 1 & allecig == 0)

n <- as.data.frame(t(addmargins(xtabs( ~ smoker_noec$qimw982 + smoker_noec$quarter2))))
colnames(n) <- c("x", "y", "n")
n2 <- n[n$n > 500,] %>%
  select(n)

row2 <- c(n2, "current smokers who do not currently use e-cigarettes)", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C", "D", "E")

row3 <- c("Quarter", "Percent", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E")

row4 <- c("", "More harmful", "Equally harmful", "Less harmful", "Don't know")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E")  

headings <- rbind(row2, row3, row4)


df1 <- t(colPerc(xtabs(smoker$weight_wales ~ smoker$qimw982 + smoker$quarter2))) %>%
  as.data.frame() %>%
  mutate(Total = NULL)
colnames(df1) <- c("More harmful", "Equally harmful", "Less harmful", "Don't know")

df1 <- round(df1, digits = 1)

df2 <- cbind(series, df1)
df2$date <- as.factor(df2$date)
colnames(df2) <- c("A", "B", "C", "D", "E")  

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# Push to google sheet
sheet_write(df3, "1DfpQJh8udMZIeNrcjNQg3mLswO8MMlZ3Da4vGTX6qt0", sheet = 'Sheet1')
