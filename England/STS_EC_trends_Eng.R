library(googlesheets4)
library(googledrive)
library(haven)
library(tigerstats)
library(dplyr)
library(tidyr)
library(DescTools)
library(tidyverse)
library(zoo)
library(tibbletime)
library(lubridate)

#sts <- read_sav("C:/Toolkit merge files/Waves/185/omni185_39.1_65.2cot_31.3a_25.4s_recodes_71.5sa.sav") # Use Dec, Mar, Jun, Sep updates
#names(sts)[names(sts)=="@weight0"] <- "weight0" 

sts_eng <- sts %>% subset(gore < 10)

###################################################
# Prevalence of e-cigarette and heat-not-burn use #
###################################################

sts85 <- sts_eng %>% subset(xwave > 85)

n.ec <- nrow(sts85) %>%
  as.data.frame()
colnames(n.ec) <- c("Freqec")

n.ec$Freqec <- interaction("(N=", n.ec$Freqec, sep = "")
n.ec$Freqec <- interaction(n.ec$Freqec, ")", sep = "")

n.juul <- sts85 %>%
  subset(sts85$xwave > 141) %>% 
  nrow() %>%
  as.data.frame()
colnames(n.juul) <- c("Freqj")

n.juul$Freqj <- interaction("(N=", n.juul$Freqj, sep = "")
n.juul$Freqj <- interaction(n.juul$Freqj, ")", sep = "")

n.htp <- sts85 %>%
  subset(sts85$xwave > 122) %>% 
  nrow() %>%
  as.data.frame()
colnames(n.htp) <- c("Freqhtp")

n.htp$Freqhtp <- interaction("(N=", n.htp$Freqhtp, sep = "")
n.htp$Freqhtp <- interaction(n.htp$Freqhtp, ")", sep = "")

row2 <- c("", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("Quarter", "Percent", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

row4 <- c("", "E-cig", "Juul", "HTP")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D")


row4 <- cbind(row4, n.ec, n.juul, n.htp)
row4 <- row4[c(1,2,5,3,6,4,7)]
row4 <- unite(row4, "B", B:Freqec, sep = " ")
row4 <- unite(row4, "C", C:Freqj, sep = " ")
row4 <- unite(row4, "D", D:Freqhtp, sep = " ")
colnames(row4) <- c("A", "B", "C", "D")

headings <- rbind(row2, row3, row4)

# E-cigarette
df1 <- t(colPerc(xtabs(sts85$weight0 ~ sts85$allecig + sts85$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)


df1 <- df1 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# JUUL
df2 <- t(colPerc(xtabs(sts85$weight0 ~ sts85$allJuul + sts85$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df2 <- df2 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# HNB
df3 <- t(colPerc(xtabs(sts85$weight0 ~ sts85$allHNB + sts85$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- df3 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Function for binding columns with different numbers of rows
cbind.fill<-function(...){
  nm <- list(...) 
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

df4 <- cbind.fill(df1, df2, df3)
colnames(df4) <- c("E-cig", "JUUL", "HTP")

df5 <- as.matrix(df4)  
df6 <- df5[nrow(df5):1, ] %>%
  as.data.frame() # Flip dataframe correct way around again (latest wave at bottom of datframe)
df6 <- round(df6, digits = 1)

# INCREASE BY 1 EACH QUARTER
series <- create_series('2013' ~ '2022', 'quarterly', class = 'yearmon') %>%
slice(c(4:38))

df7 <- cbind(series, df6)
df7$date <- as.factor(df7$date)
colnames(df7) <- c("A", "B", "C", "D")  

df7 <- rbind(headings, df7)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1IYsvd3OrZsDLZghyoYac-irPd_JEZxFZJr2s8JQgKOI", sheet = 'Sheet1')

##########################################################
# Nicotine use by never smokers and long-term ex-smokers #
##########################################################

never <- sts85 %>% subset(smokstat == 0)
ltex <- sts85 %>% subset(smokstat == 1)

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
df1 <- t(colPerc(xtabs(never$weight0 ~ never$allecig + never$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# E-cig long-term ex-smoker
df2 <- t(colPerc(xtabs(ltex$weight0 ~ ltex$allecig + ltex$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT - never smokers 
df3 <- t(colPerc(xtabs(never$weight0 ~ never$allnrt + never$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT long-term ex-smoker
df4 <- t(colPerc(xtabs(ltex$weight0 ~ ltex$allnrt + ltex$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df5 <- cbind(df1, df2, df3, df4) 
df5 <- round(df5, digits = 1)

df6 <- cbind(series, df5)
df6$date <- as.factor(df6$date)
colnames(df6) <- c("A", "B", "C", "D", "E")  

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1GHonGQnVFueYhZ6NU10VbeQFdIhgckWZgfHHc-E6_Xs", sheet = 'Sheet1')

###########################################################
# Nicotine use by long-term ex-smokers (> and <= 5 years) #
###########################################################

ltex_over5 <- sts85 %>% subset(yearsquit > 5 & smokstat == 1)
ltex_under5 <- sts85 %>% subset(yearsquit <= 5 & smokstat == 1)

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
df1 <- t(colPerc(xtabs(ltex_under5$weight0 ~ ltex_under5$allecig + ltex_under5$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# E-cigarette - long-term ex > 5y
df2 <- t(colPerc(xtabs(ltex_over5$weight0 ~ ltex_over5$allecig + ltex_over5$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT - ong-term ex <=5y
df3 <- t(colPerc(xtabs(ltex_under5$weight0 ~ ltex_under5$allnrt + ltex_under5$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT long-term ex-smoker > 5y
df4 <- t(colPerc(xtabs(ltex_over5$weight0 ~ ltex_over5$allnrt + ltex_over5$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df5 <- cbind(df1, df2, df3, df4) 
df5 <- round(df5, digits = 1)

series <- create_series('2013' ~ '2022', 'quarterly', class = 'yearmon') %>%
  slice(c(4:38))

df6 <- cbind(series, df5)
df6$date <- as.factor(df6$date)
colnames(df6) <- c("A", "B", "C", "D", "E")  

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1c--1vEP73pg9lW_ijaux0BqYRMRg0NftXbkD0ACzE68", sheet = 'Sheet1')


#########################################################################
# Prevalence of electronic cigarette use: smokers and recent ex-smokers #
#########################################################################

smokly2010 <- sts_eng %>% subset(xyear > 2010 & smokly == 1) 

sts85 %>% subset(xyear > 2010 & smokly == 1) %>% nrow()

nrow(smokly2010)

n.ec <- nrow(smokly2010) %>%
  as.data.frame()
colnames(n.ec) <- c("Freqec")

n.ec$Freqec <- interaction("(N=", n.ec$Freqec, sep = "")
n.ec$Freqec <- interaction(n.ec$Freqec, ")", sep = "")

n.juul <- smokly2010 %>%
  subset(smokly2010$xwave > 141) %>% 
  nrow() %>%
  as.data.frame()
colnames(n.juul) <- c("Freqj")

n.juul$Freqj <- interaction("(N=", n.juul$Freqj, sep = "")
n.juul$Freqj <- interaction(n.juul$Freqj, ")", sep = "")

n.htp <- smokly2010 %>%
  subset(smokly2010$xwave > 122) %>% 
  nrow() %>%
  as.data.frame()
colnames(n.htp) <- c("Freqhtp")

n.htp$Freqhtp <- interaction("(N=", n.htp$Freqhtp, sep = "")
n.htp$Freqhtp <- interaction(n.htp$Freqhtp, ")", sep = "")

row2 <- c("", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E")

row3 <- c("Quarter", "Percent", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E")

row4 <- c("", "Any E-cig","Daily E-cig", "Juul", "HTP")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E")


row4 <- cbind(row4, n.ec, n.ec, n.juul, n.htp)
row4 <- row4[c(1,2,6,3,6,4,8,5,9)]
row4 <- unite(row4, "B", B:Freqec, sep = " ")
row4 <- unite(row4, "C", C:Freqec.1, sep = " ")
row4 <- unite(row4, "D", D:Freqj, sep = " ")
row4 <- unite(row4, "E", E:Freqhtp, sep = " ")
colnames(row4) <- c("A", "B", "C", "D", "E")

headings <- rbind(row2, row3, row4)

# Any E-cig
df1 <- t(colPerc(xtabs(smokly2010$weight0 ~ smokly2010$allecig + smokly2010$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df1 <- df1 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Daily E-cig
df2 <- t(colPerc(xtabs(smokly2010$weight0 ~ smokly2010$dailyecig + smokly2010$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df2 <- df2 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# JUUL
df3 <- t(colPerc(xtabs(smokly2010$weight0 ~ smokly2010$allJuul + smokly2010$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- df3 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# HNB
df4 <- t(colPerc(xtabs(smokly2010$weight0 ~ smokly2010$allHNB + smokly2010$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- df4 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

df5 <- cbind.fill(df1, df2, df3, df4)
df5 <- round(df5, digits = 1)

df6 <- as.matrix(df5)  
df7 <- df6[nrow(df6):1, ] %>%
  as.data.frame() # Flip dataframe correct way around again (latest wave at bottom of datframe)

series <- create_series('2011' ~ "2022", 'quarterly', class = 'yearmon') %>%
  slice(-c(1)) %>%
  slice(-c(46:47)) # Remove quarters that are in future

df8 <- cbind(series, df7)
df8$date <- as.factor(df8$date)
colnames(df8) <- c("A", "B", "C", "D", "E")  

df8 <- rbind(headings, df8)
colnames(df8) <- c()

# PUSH to google sheet
sheet_write(df8, "12z-erZdojP3PxTl6LDjkCAnQgrXicMLtd83PV2S2lMA", sheet = 'Sheet1')

###########################################################
# Proportion of e-cigarette and NRT users who are smokers #
###########################################################

#### Estimates marginally different from SPSS? SPSS lists handful of missing cases, R does not. Why?
# Is because of differences in how R computes weighted estimates (using Ns to 6dp) and SPSS does whole number Ns. 

allecig <- sts85 %>% subset(allecig == 1)
allnrt <- sts85 %>% subset(allnrt == 1)

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
df1 <- t(colPerc(xtabs(allecig$weight0 ~ allecig$smokstat + allecig$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT
df2 <- t(colPerc(xtabs(allnrt$weight0 ~ allnrt$smokstat + allnrt$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- cbind(df1, df2)
df3 <-  df3 %>% subset(select = c(3, 6))
df3 <-  round(df3, digits = 1)

series <- create_series('2013' ~ '2022', 'quarterly', class = 'yearmon') %>%
  slice(-c(1:3)) %>%
  slice(-c(36:37)) # remove future quarters

df4 <- cbind(series, df3)
df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B", "C")  

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "15Nux8B9_NY8jjpZ03nXeJIhWNsqAilsUNc96NKR7Kd8", sheet = 'Sheet1')

#################################################################
# Proportion of daily e-cigarette and NRT users who are smokers #
#################################################################

#### Estimates marginally different from SPSS? SPSS lists handful of missing cases, R does not. Why?
# Is because of differences in how R computes weighted estimates (using Ns to 6dp) and SPSS does whole number Ns. 

allecig <- sts85 %>% subset(allecig == 1 & allnrt == 0 & nicpdc >2)
allnrt <- sts85 %>% subset(allecig == 0 & allnrt == 1 & nicpdc >2)


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
df1 <- t(colPerc(xtabs(allecig$weight0 ~ allecig$smokstat + allecig$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT
df2 <- t(colPerc(xtabs(allnrt$weight0 ~ allnrt$smokstat + allnrt$quarter2))) %>%
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
sheet_write(df5, "1oCeVNOAhbPsiMm5CA8tXr6-DsGMSbFqquKGOpF4zCHI", sheet = 'Sheet1')


############################
# Electronic cigarette use #
############################

freq <- sts_eng %>% subset(nicpd > 0 & allecig == 1 & allnrt == 0 & smokly == 1)

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

df1 <- (colPerc(xtabs(freq$weight0 ~ freq$nicpdc + freq$smoker))) %>%
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
sheet_write(df3, "1RAO7h9UKqY81l9mgfeO9p2wV3stmCUeqYX7zXRRFd2w", sheet = 'Sheet1')

################################
# E-cigarette use daily ANNUAL #
################################

# RUN ANNUALLY

freq <- freq %>% 
  mutate(frequency_ec = case_when(nicpdc <= 2 ~ "Lessthandaily",
                                  nicpdc >= 3 ~ "Daily")) 

# SMOKERS

freq_smoker <- freq %>% subset(smoker == 1) 

df1 <- t(colPerc(xtabs(freq_smoker$weight0 ~ freq_smoker$frequency_ec + freq_smoker$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate(Total = NULL) %>%
  mutate(Lessthandaily = NULL)

df2 <- round(df2, digits = 1) %>%
  slice(-c(1:2))

Year <- c(2013:2021)

df3 <- cbind(Year, df2)
colnames(df3) <- c("A", "B")

# EX-SMOKERS 
freq_exsmoker <- freq %>% subset(smoker == 0) 

df1_ex <- t(colPerc(xtabs(freq_exsmoker$weight0 ~ freq_exsmoker$frequency_ec + freq_exsmoker$xyear)))
df2_ex <- df1_ex %>% as.data.frame() %>%
  mutate(Total = NULL) %>%
  mutate(Lessthandaily = NULL)

df2_ex <- round(df2_ex, digits = 1) %>%
  slice(-c(1:2))

# Combine smokers and ex-smoker data
df4 <- cbind(Year, df2, df2_ex)
colnames(df4) <- c("A", "B", "C")

n <- nrow(freq)

row2 <- c(n, "e-cigarette users not using NRT)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Year", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "Smoker", "Ex-smoker")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

df4 <- rbind(headings, df4)
colnames(df4) <- c()

sheet_write(df4, "1mWl2sgIM6eWMhjl407sgH2UNxEUOIY0k55f8qiDoxP4", sheet = 'Sheet1')

######################################
# Characteristics of the e-cigarette #
######################################

smokly <- sts_eng %>% subset(smokly == 1)
smokly.ec <- smokly %>% subset(qimw118_2 == 2 | qimw118_2 == 3 | qimw118_2 == 4)

n <- nrow(smokly.ec)

row2 <- c(n, "e-cigarette users who smoke or who stopped in the past year surveyed since Aug 16)", "", "")
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

df1 <- (colPerc(xtabs(smokly$weight0 ~ smokly$qimw118_2 + smokly$smokstat))) %>%
  as.data.frame() %>%
  slice(-c(2, 3, 4))
df1 <- df1[c(2,1)]
rownames(df1) <- (c("Nicotine containing"))

df2 <- (colPerc(xtabs(smokly$weight0 ~ smokly$qimw118_3 + smokly$smokstat))) %>%
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
sheet_write(df5, "1NPIk_jbYpJzVQEavP2-JOQo8LQbq0yOqY3aHr5iphUc", sheet = 'Sheet1')

###############################################
# Characteristics of the e-cigarette - ANNUAL #
###############################################

df1 <- t(colPerc(xtabs(smokly$weight0 ~ smokly$qimw118_3 + smokly$xyear)))
df2 <-  df1 %>% as.data.frame() %>%
  mutate(Total = NULL)
df2 <- df2[c(1:4)]
df2 <-  round(df2, digits = 1)

Year <- c(2016:2021)

df2 <- cbind(Year, df2)
colnames(df2) <- c("A", "B", "C", "D", "E")

n <- nrow(smokly.ec)

row2 <- c(n, "nicotine containing e-cigarette users who smoke or who stopped in the past year)", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C", "D", "E")

row3 <- c("Year", "Percent", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E")

row4 <- c("", "Disposable", "Rechargable with pre-filled cartridges", "Rechargable with tank to re-fill", "Mod system")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E")  

headings <- rbind(row2, row3, row4)

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# PUSH to google sheet
sheet_write(df3, "1t2u6Mjvsb2HMgzgw5TjAsXpo36NvC0dic8Wak4uHpiQ", sheet = 'Sheet1')

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


df1 <- (colPerc(xtabs(smokly$weight0 ~ smokly$qimw118_4 + smokly$smokstat))) %>%
  as.data.frame() %>%
  slice(-c(6))
rownames(df1) <- c("6mg (0.6%) or less", "7mg (0.7%) to 11mg (1.1%)", "12mg (1.2%) to 19mg (1.9%)",
                   "20mg (2.0%) or more", "Unsure")
df1 <- df1[c(2,1)]
colnames(df1) <- c("Smoker", "Ex-smoker")
df1 <-  round(df1, digits = 1)

conc <- c("6mg (0.6%) or less", "7mg (0.7%) to 11mg (1.1%)", "12mg (1.2%) to 19mg (1.9%)",
          "20mg (2.0%) or more", "Unsure")

df2 <- cbind(conc, df1)
colnames(df2) <- c("A", "B", "C")

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# PUSH to google sheet
sheet_write(df3, "17_TkrEihqF68qf6w3TvLkCXQYuZ_b2y1v4sKPYzenrU", sheet = 'Sheet1')

###################
# E-liquid ANNUAL #
###################

# RUN ANNUALLY

smokly <- smokly %>% 
  mutate(el20 = case_when(qimw118_4 ==  5 ~ "20mg(2.0%)ormore",
                          qimw118_4 < 5 | qimw118_4 == 6 ~ "Lessthan20unsure")) 

smokly_smok <- smokly %>% subset(smokstat == 3)

df1 <- t(colPerc(xtabs(smokly_smok$weight0 ~ smokly_smok$el20 + smokly_smok$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate(Total = NULL) %>%
  mutate(Lessthan20unsure = NULL)
df2 <- round(df2, digits = 1)

smokly_ex <- smokly %>% subset(smokstat == 2)

df1_ex <- t(colPerc(xtabs(smokly_ex$weight0 ~ smokly_ex$el20 + smokly_ex$xyear)))
df2_ex <- df1_ex %>% as.data.frame() %>%
  mutate(Total = NULL) %>%
  mutate(Lessthan20unsure = NULL)
df2_ex <- round(df2_ex, digits = 1)

Year <- c(2016:2021)

df3 <- cbind(Year, df2, df2_ex)
colnames(df3) <- c("A", "B", "C")

n <- nrow(smokly.el)

row2 <- c(n, "nicotine containing e-cigarette users who smoke or who stopped in the past year)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Year", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "Smoker", "Ex-smoker")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

df4 <- rbind(headings, df3)
colnames(df4) <- c()

sheet_write(df4, "1KBi7TnSqzvxVueAOKFbYPYfCvWoyypsvRyTwyYM5Cqo", sheet = 'Sheet1')

##########
# Source #
##########

attributes(sts$qimw118_5)

smokly.source <- smokly %>% subset(qimw118_5 == 2 | qimw118_5 == 3 | qimw118_5 == 4 | qimw118_5 == 5 | qimw118_5 == 6 | qimw118_5 == 7 | qimw118_5 == 8
                                   | qimw118_5 == 9 | qimw118_5 == 10)
n <- nrow(smokly.source)

row2 <- c(n, "e-cigarette users who smoke or who stopped in the past year surveyed since Aug 16)", "", "")
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

df1 <- (colPerc(xtabs(smokly$weight0 ~ smokly$qimw118_5 + smokly$smokstat))) %>%
  as.data.frame() %>%
  slice(-c(10))
rownames(df1) <- c("Specialist ('vape shop')", "Online specialist", "Other online",
                   "Newsagent", "Petrol garage", "Supermarket", "Friends", "Other", "Unknown")
df1 <- df1[c(2,1)]
colnames(df1) <- c("Smoker", "Ex-smoker")
df1 <-  round(df1, digits = 1)

source <- c("Specialist ('vape shop')", "Online specialist", "Other online",
          "Newsagent", "Petrol garage", "Supermarket", "Friends", "Other", "Unknown")

df2 <- cbind(source, df1)
colnames(df2) <- c("A", "B", "C")

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# PUSH to google sheet
sheet_write(df3, "1VT3OYFQMuP5E4Zenv9PL7vXv0gxxEM-RvoiAsnJ6HiQ", sheet = 'Sheet1')

#################
# Source ANNUAL #
#################

smokly <- smokly %>% 
  mutate(source_purchase = case_when(qimw118_5 ==  2 ~ "Specialist vape shop",
                          qimw118_5 > 7 ~ "Other",
                          qimw118_5 == 3 | qimw118_5 == 4 ~ "Online",
                          qimw118_5 >= 5 | qimw118_5 <= 7 ~ "Non-specialist shop")) 

df1 <- t(colPerc(xtabs(smokly$weight0 ~ smokly$source_purchase + smokly$xyear)))
df2 <-  df1 %>% as.data.frame() %>%
  mutate(Total = NULL)
df2 <-  round(df2, digits = 1)
df2 <- df2[c(4,1,2,3)]

Year <- c(2016:2021)

df2 <- cbind(Year, df2)
colnames(df2) <- c("A", "B", "C", "D", "E")

n <- nrow(smokly.source)

row2 <- c(n, "nicotine containing e-cigarette users who smoke or who stopped in the past year)", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C", "D", "E")

row3 <- c("Year", "Percent", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E")

row4 <- c("", "Specialist vape shop", "Non-specialist shop", "Online", "Other")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E")  

headings <- rbind(row2, row3, row4)

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# PUSH to google sheet
sheet_write(df3, "1MVBY5Ax5wNnjkkJsZYtpSjNtRB-m4X7hvhekO33WkOg", sheet = 'Sheet1')

#################################################################
# Electronic cigarette and NRT use across the age range in 2021 # 
#################################################################

# Estimates a few decimal points out - all Ns look the same...due to rounding of weighted Ns in R different to SPSS.

sts2021 <- sts_eng %>% subset(smokly == 1 & xyear == 2021)

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

df1 <- t(colPerc(xtabs(sts2021$weight0 ~ sts2021$allecig + sts2021$agez))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) %>%
  slice(-c(7,8))

df2 <- t(colPerc(xtabs(sts2021$weight0 ~ sts2021$allnrt + sts2021$agez))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) %>%
  slice(-c(7,8))

df3 <- cbind(df1, df2)
df3 <- (round(df3, digits = 1))
colnames(df3) <- c("E-cig user", "NRT user")

age <- c("16-24", "25-34", "35-44", "45-54", "55-64", "65+")

df4 <- cbind(age, df3)
colnames(df4) <- c("A", "B", "C")

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "16QthPXsIatWo8jrmhhZnJ6gDPGDF4yZBPniA2DGI75Q", sheet = 'Sheet1')

################################################
# Electronic cigarette use 16-24 annual trend #
###############################################

sts1621 <- sts_eng %>% subset(smokly == 1 & actage < 25 & xyear >= 2013)

n <- nrow(sts1621)

row2 <- c(n, "adults age 16-24 who smoke or who stopped in the past year)", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C")

row3 <- c("Year", "Percent", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C")

row4 <- c("", "E-cig user", "NRT user")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C")  

headings <- rbind(row2, row3, row4)

df1 <- t(colPerc(xtabs(sts1621$weight0 ~ sts1621$allecig + sts1621$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df2 <- t(colPerc(xtabs(sts1621$weight0 ~ sts1621$allnrt + sts1621$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

Year <- c(2013:2021)

df3 <- cbind(Year, df1, df2)
df3 <- (round(df3, digits = 1))
colnames(df3) <- c("E-cig user", "NRT user")
colnames(df3) <- c("A", "B", "C")

df4 <- rbind(headings, df3)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df4, "1bzL_U7E_oLKz6ecUQnYIbAhmn8th2qZS5DwRfSH02uw", sheet = 'Sheet1')

##########################################################
# Electronic cigarette and NRT use men and women in 2021 # 
##########################################################

# Estimates a few decimal points out - all Ns look the same.....due to rounding of weighted Ns in R different to SPSS.

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

df1 <- t(colPerc(xtabs(sts2021$weight0 ~ sts2021$allecig + sts2021$sexz))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) %>%
  slice(-c(3))

df2 <- t(colPerc(xtabs(sts2021$weight0 ~ sts2021$allnrt + sts2021$sexz))) %>%
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
sheet_write(df5, "1SOls0A1BPj3NzLeEZWMvNIe7RWiH7MO3QOiBg4bWAUc", sheet = 'Sheet1')

###############################################
# E-cigarette use men and women annual trends #
###############################################

sts_mw <- sts_eng %>% subset(smokly == 1 & xyear >= 2013)

n <- nrow(sts_mw)

row2 <- c(n, "adults who smoke or who stopped in the past year)", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C", "D", "E")

row3 <- c("Year", "Percent", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E")

row4 <- c("", "E-cig user (Men)", "E-cig user (Women)", "NRT user (Men)", "NRT user (Women)")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E")  

headings <- rbind(row2, row3, row4)

men <- sts_mw %>% subset(sexz == 1)

df1 <- t(colPerc(xtabs(men$weight0 ~ men$allecig + men$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

women <- sts_mw %>% subset(sexz == 2)

df2 <- t(colPerc(xtabs(women$weight0 ~ women$allecig + women$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- t(colPerc(xtabs(men$weight0 ~ men$allnrt + men$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df4 <- t(colPerc(xtabs(women$weight0 ~ women$allnrt + women$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

Year <- c(2013:2021)

df5 <- cbind(Year, df1, df2, df3, df4)
df5 <- (round(df5, digits = 1))
colnames(df5) <- c("A", "B", "C", "D", "E")

df6 <- rbind(headings, df5)
colnames(df6) <- c()

# PUSH to google sheet
sheet_write(df6, "102tJTjxfYpOwBEprcLub7QQLlHmA4qBCSQaskplewX0", sheet = 'Sheet1')


###########################################################
# Electronic cigarette across the social gradient in 2020 # 
###########################################################

# Estimates a few decimal points out - all Ns look the same.....due to rounding of weighted Ns in R different to SPSS.

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

df1 <- t(colPerc(xtabs(sts2021$weight0 ~ sts2021$allecig + sts2021$sgz))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL) 

df2 <- t(colPerc(xtabs(sts2021$weight0 ~ sts2021$allnrt + sts2021$sgz))) %>%
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
sheet_write(df5, "1u_6uM7vcc_RoVvLnwCQLgBnJlR_2JdD3BRNmYptvYW0", sheet = 'Sheet1')

################################################
# E-cigarette use by social grade annual trend #
################################################

sts_mw <- sts_eng %>% subset(smokly == 1 & xyear >= 2013)

n <- nrow(sts_mw)

row2 <- c(n, "adults who smoke or who stopped in the past year)", "", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F", "G")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C", "D", "E", "F")

row3 <- c("Year", "Percent", "", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E", "F")

row4 <- c("", "AB", "C1", "C2", "D", "E")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E", "F")  

headings <- rbind(row2, row3, row4)

ab <- sts_mw %>% subset(sgz == 1)

df1 <- t(colPerc(xtabs(ab$weight0 ~ ab$allecig + ab$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c1 <- sts_mw %>% subset(sgz == 2)

df2 <- t(colPerc(xtabs(c1$weight0 ~ c1$allecig + c1$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

c2 <- sts_mw %>% subset(sgz == 3)

df3 <- t(colPerc(xtabs(c2$weight0 ~ c2$allecig + c2$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

d <- sts_mw %>% subset(sgz == 4)

df4 <- t(colPerc(xtabs(d$weight0 ~ d$allecig + d$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

e <- sts_mw %>% subset(sgz == 5)

df5 <- t(colPerc(xtabs(e$weight0 ~ e$allecig + e$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df6 <- cbind(Year, df1, df2, df3, df4, df5)
df6 <- (round(df6, digits = 1))
colnames(df6) <- c("A", "B", "C", "D", "E", "F")

df6 <- rbind(headings, df6)
colnames(df6) <- c()

# PUSH to google sheet
sheet_write(df6, "1ugaOXiyOfTQaHLWv8L_mnbfg0dz47JBWHaRovmfHqoE", sheet = 'Sheet1')

##########################################
# Use of nicotine products while smoking # 
##########################################

smoker <- sts_eng %>% subset(smoker == 1 & quarter2 > 1)
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
df1 <- t(colPerc(xtabs(smoker$weight0 ~ smoker$allecig + smoker$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

#NRT
df2 <- t(colPerc(xtabs(smoker$weight0 ~ smoker$allnrt + smoker$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

df3 <- cbind(df1, df2)
colnames(df3) <- c("E-cigs", "NRT")
df3 <- round(df3, digits = 1)

series <- create_series('2011' ~ "2022", 'quarterly', class = 'yearmon') %>%
  slice(-c(1)) %>%
  slice(-c(46:47)) # Remove future quarters

df4 <- cbind(series, df3)
df4$date <- as.factor(df4$date)
colnames(df4) <- c("A", "B", "C")  

df5 <- rbind(headings, df4)
colnames(df5) <- c()

# PUSH to google sheet
sheet_write(df5, "19WLqJyLSLezecqUaQHxU9UwmZrDQPwnx4HbuveBtw7c", sheet = 'Sheet1')

#################################################
# Use of nicotine products in recent ex-smokers # 
#################################################

# Estimates a couple of decimals out...due to rounding differences of weighted Ns in R.

recentex <- sts_eng %>% subset(smoker == 0 & smokly == 1 & quarter2 > 1)
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
df1 <- t(colPerc(xtabs(recentex$weight0 ~ recentex$allecig + recentex$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# NRT

df2 <- t(colPerc(xtabs(recentex$weight0 ~ recentex$allnrt + recentex$quarter2))) %>%
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
sheet_write(df5, "1kApy_kp8vI1MXQ6IN6v6ZxZC82Xm7NjdabJHQxi_X-k", sheet = 'Sheet1')

#########################################
# Aids used in most recent quit attempt #
#########################################

# Make sure google slides variable heading is correct for e-cigs and NRT

names(sts_eng)[names(sts_eng)=="@q632b1o"] <- "q632b1o"
names(sts_eng)[names(sts_eng)=="@q632b1p"] <- "q632b1p"

trylyc <- sts_eng %>% subset(trylyc == 1 & xwave > 32)

n.all <- nrow(trylyc)

row2 <- c(n.all, "adults who smoke and tried to stop or who stopped in the past year)", "", "", "", "", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = " ")
colnames(row2) <- c("A", "B", "C", "D", "E", "F", "G", "H")

n.juul <- trylyc %>%
  subset(xwave > 141) %>% 
  nrow() %>%
  as.data.frame()
colnames(n.juul) <- c("Freqj")

n.juul$Freqj <- interaction("(N=", n.juul$Freqj, sep = "")
n.juul$Freqj <- interaction(n.juul$Freqj, ")", sep = "")

n.htp <- trylyc %>%
  subset(xwave > 114) %>% 
  nrow() %>%
  as.data.frame()
colnames(n.htp) <- c("Freqhtp")

n.htp$Freqhtp <- interaction("(N=", n.htp$Freqhtp, sep = "")
n.htp$Freqhtp <- interaction(n.htp$Freqhtp, ")", sep = "")

row3 <- c("Quarter", "Percent of smokers trying to stop", "", "", "", "", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D", "E", "F", "G", "H")

row4 <- c("", "E-cig", "NRT OTC", "NRT Rx", "Champix", "Beh'l supp", "HTP", "Juul")
row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E", "F", "G", "H")

row4 <- cbind(row4, n.juul, n.htp)
row4 <- row4[c(1,2,5,3,6,4,7,10,8,9)]
row4 <- unite(row4, "G", G:Freqhtp, sep = " ")
row4 <- unite(row4, "H", H:Freqj, sep = " ")
colnames(row4) <- c("A", "B", "C", "D", "E", "F", "G", "H")

headings <- rbind(row2, row3, row4)

##

# E-cigs
df1 <- t(colPerc(xtabs(trylyc$weight0 ~ trylyc$ecig1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df1) <- c("E-cigs")

df1 <- df1 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Champix
df4 <- t(colPerc(xtabs(trylyc$weight0 ~ trylyc$cha1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df4) <- c("Champix")

df4 <- df4 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# NRT OTC
df2 <- t(colPerc(xtabs(trylyc$weight0 ~ trylyc$nrto1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df2) <- c("NRT OTC")

df2 <- df2 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Behavioural support
df5 <- t(colPerc(xtabs(trylyc$weight0 ~ trylyc$beh1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df5) <- c("Behavioual support")

df5 <- df5 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# NRT Rx
df3 <- t(colPerc(xtabs(trylyc$weight0 ~ trylyc$nrtr1 + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df3) <- c("NRT Rx")

df3 <- df3 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# HTB
df6 <- t(colPerc(xtabs(trylyc$weight0 ~ trylyc$q632b1o + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df6) <- c("HTP")

df6 <- df6 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

# Juul
df7 <- t(colPerc(xtabs(trylyc$weight0 ~ trylyc$q632b1p + trylyc$quarter))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df7) <- c("Juul")

df7 <- df7 %>% map_df(rev) %>%
  as.data.frame() # Reverse dataframe order so can bind with NAs at bottom of df

df8 <- cbind.fill(df1, df4, df2, df5, df3, df6, df7)
colnames(df8) <- c("E-cig", "Champix", "NRT OTC", "Behavioural support", "NRT Rx", "HTP", "Juul")

df9 <- as.matrix(df8)  
df9 <- df9[nrow(df9):1, ] %>%
  as.data.frame() # Flip dataframe correct way around again (latest wave at bottom of datframe)
df9 <- round(df9, digits = 1)

series <- create_series('2009' ~ '2022', 'quarterly', class = 'yearmon') %>%
  slice(-c(1:2)) %>%
  slice(-c(53:55)) # Change depending on when in year running update

df10 <- cbind(series, df9)
df10$date <- as.factor(df10$date)
colnames(df10) <- c("A", "B", "C", "D", "E", "F", "G", "H")  

df11 <- rbind(headings, df10)
colnames(df11) <- c()

# Push to google sheet
sheet_write(df11, "1iWChErBio2vaHM-m9TScvCeTcB8jzkHnsdkv0IZxopY", sheet = 'Sheet1')

##########################################
# Current e-cigarette use after quitting #
##########################################

# JUST UPDATE ANNUALLY

# Estimates are little different to SPSS - rounding again?

afterq <- sts_eng %>% subset(trylyc == 1 & quarter2 > 0 & ecig1 == 0 & smokly == 1 & smoker == 0)
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
df1 <- t(colPerc(xtabs(afterq$weight0 ~ afterq$allecig + afterq$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df1) <- c("Any")
df1 <- round(df1, digits = 1)

# Daily e-cig
df2 <- t(colPerc(xtabs(afterq$weight0 ~ afterq$dailyecig + afterq$xyear))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df2) <- c("Daily")
df2 <- round(df2, digits = 1)

series <- create_series('2011' ~ '2021', 'yearly', class = 'Date')
year <- c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

df3 <- cbind(year, df1, df2)
colnames(df3) <- c("A", "B", "C")

df4 <- rbind(headings, df3)
colnames(df4) <- c()

# Push to google sheet
sheet_write(df4, "1YYp-RyketNxu3v9LBGNTBlokfzQkVT9ILCGxmqWcjpw", sheet = 'Sheet1')

########################################
# Prevalence of nicotine/cigarette use #
########################################

cignic <- sts_eng %>% subset(cignic3 > -1 & xwave > 85)
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
df1 <- t(colPerc(xtabs(cignic$weight0 ~ cignic$cigsmok + cignic$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df1) <- c("Cigarettes")
df1 <- round(df1, digits = 1)

#
df2 <- t(colPerc(xtabs(cignic$weight0 ~ cignic$cignic3 + cignic$quarter2))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)
colnames(df2) <- c("Nicotine or cigarettes")
df2 <- round(df2, digits = 1)

series <- create_series('2013' ~ '2022', 'quarterly', class = 'yearmon') %>%
  slice(-c(1:3)) %>%
  slice(-c(36:37)) # Change depending on when running update

df3 <- cbind(series, df1, df2)
df3$date <- as.factor(df3$date)
colnames(df3) <- c("A", "B", "C")  

df4 <- rbind(headings, df3)
colnames(df4) <- c()

# Push to google sheet
sheet_write(df4, "1O7cnImOaC7Yghz2gq0zd6PI3-GfEUPYKvUk2ufZ51WM", sheet = 'Sheet1')

#############################################################
# Harm perceptions of e-cigarettes compared with cigarettes # 
#############################################################

smoker_noec <- sts_eng %>% subset(smoker == 1 & allecig == 0)

n <- as.data.frame(t(addmargins(xtabs( ~ smoker_noec$qimw982 + smoker_noec$quarter2))))
colnames(n) <- c("x", "y", "n")
n2 <- n[n$n > 20000,] %>%
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

df1 <- t(colPerc(xtabs(smoker_noec$weight0 ~ smoker_noec$qimw982 + smoker_noec$quarter2))) %>%
  as.data.frame() %>%
  mutate(Total = NULL)
colnames(df1) <- c("More harmful", "Equally harmful", "Less harmful", "Don't know")

df1 <- round(df1, digits = 1)

series <- create_series('2014' ~ '2022', 'quarterly', class = 'yearmon') %>%
  slice(-c(1:3)) %>%
  slice(-c(32:33))

df2 <- cbind(series, df1)
df2$date <- as.factor(df2$date)
colnames(df2) <- c("A", "B", "C", "D", "E")  

df3 <- rbind(headings, df2)
colnames(df3) <- c()

# Push to google sheet
sheet_write(df3, "1F5Twui-_hCArlwImuEf3MumuQbC74ujseA-u7wMtQ_4", sheet = 'Sheet1')
