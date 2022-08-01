library(googlesheets4)
library(googledrive)
library(haven)
library(tigerstats)
library(dplyr)
library(tidyr)
library(DescTools)

#sts <- read_sav("C:/Toolkit merge files/Waves/186/omni186_39.1_65.2cot_31.3a_25.4s_recodes_72.5sa.sav")
#names(sts)[names(sts)=="@weight0"] <- "weight0" 
sts_wal <- sts %>% subset(gore == 10)

#######################
#######################
## TOP LINE FINDINGS ##
#######################
#######################

################################
# CIGARETTE SMOKING PREVALENCE #
################################

A <- c("Graph shows prevalence estimate and upper and lower 95% confidence intervals", "Year", "")
B <- c("", "Percent", "Prevalence estimate")
C <- c("", "", "Lower 95% CI")
D <- c("", "", "Upper 95% CI")
headings <- cbind(A, B, C, D)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_wal$weight_wales ~ sts_wal$cigsmok + sts_wal$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$cigsmok + sts_wal$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_wal.cigsmok != 0) %>%
  subset(sts_wal.cigsmok != 1) %>%
  subset(sts_wal.xyear != "Sum") %>%
  mutate(sts_wal.xyear = NULL) %>%
  mutate(sts_wal.cigsmok = NULL)

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$cigsmok)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.cigsmok == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_wal.cigsmok = NULL) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.cigsmok == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.cigsmok = NULL) %>%
  mutate(sts_wal.xyear = NULL)

x3 <- cbind(x1, x2)
colnames(x3) <- c("N", "TotalN")

CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
df4 <- cbind(df3,x3,CI)

# Remove redundant columns
df4 <- df4 %>%
  mutate(N = NULL) %>%
  mutate(TotalN = NULL) %>%
  mutate(est = NULL)

# Round to 1dp
df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
df5 <- cbind(df4, n2)

# Unite year and total N into one column
df5$Freq <- interaction("(N=", df5$Freq, sep = "")
df5$Freq <- interaction(df5$Freq, ")", sep = "")
df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
df6 <- unite(df5, "Year", Year:Freq, sep = " ")
colnames(df6) <- c("A", "B", "C", "D")

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1hUsaPKQdmDedJs6n8ccx7ql8oPUnZyewEp2Lhjs3qio", sheet = 'Sheet1')


################################################
# CIGARETTE SMOKING PREVALENCE 18-21 YEAR OLDS #
################################################

sts_wal1821 <- sts_wal %>% subset(actage >= 18 & actage <= 21) 

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_wal1821$weight_wales ~ sts_wal1821$cigsmok + sts_wal1821$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_wal1821$weight_wales ~ sts_wal1821$cigsmok + sts_wal1821$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_wal1821.cigsmok != 0) %>%
  subset(sts_wal1821.cigsmok != 1) %>%
  subset(sts_wal1821.xyear != "Sum") %>%
  mutate(sts_wal1821.xyear = NULL) %>%
  mutate(sts_wal1821.cigsmok = NULL)

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_wal1821$weight_wales ~ sts_wal1821$xyear + sts_wal1821$cigsmok)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal1821.cigsmok == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_wal1821.cigsmok = NULL) %>%
  mutate(sts_wal1821.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal1821.cigsmok == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_wal1821.cigsmok = NULL) %>%
  mutate(sts_wal1821.xyear = NULL)

x3 <- cbind(x1, x2)
colnames(x3) <- c("N", "TotalN")

CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
df4 <- cbind(df3,x3,CI)

# Remove redundant columns
df4 <- df4 %>%
  mutate(N = NULL) %>%
  mutate(TotalN = NULL) %>%
  mutate(est = NULL)

# Round to 1dp
df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
df5 <- cbind(df4, n2)

# Unite year and total N into one column
df5$Freq <- interaction("(N=", df5$Freq, sep = "")
df5$Freq <- interaction(df5$Freq, ")", sep = "")
df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
df6 <- unite(df5, "Year", Year:Freq, sep = " ")
colnames(df6) <- c("A", "B", "C", "D")

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1dtm6crMcYMJskJNZ5U9Q7DW2q98qMYus4aJq00uf5H4", sheet = 'Sheet1')


#####################################
# CIGARETTE SMOKING 16-17 YEAR OLDS #
#####################################

# Haven't pushed yet because of CIs

# <- sts_wal %>% subset(actage >= 16 & actage <= 17) 

# Create smoking % dataframe
#df1 <- t(colPerc(xtabs(sts_wal1617$weight_wales ~ sts_wal1617$cigsmok + sts_wal1617$xyear)))
#df2 <- df1 %>% as.data.frame() %>%
 # mutate("0" = NULL) %>%
#  mutate(Total = NULL)

# Create year variable
#colnames(df2) <- c("Value")
#Year <- c(2007:2020)
#df3 <- cbind(Year, df2)

# Compute Total Ns for each year
#n <- (t(round(addmargins(xtabs(sts_wal1617$weight_wales ~ sts_wal1617$cigsmok + sts_wal1617$xyear)))))
#n2 <- n %>% as.data.frame() %>%
 # subset(sts_wal1617.cigsmok != 0) %>%
#  subset(sts_wal1617.cigsmok != 1) %>%
 # mutate(sts_wal1617.xyear = NULL) %>%
  #mutate(sts_wal1617.cigsmok = NULL)

# Ns for 95% CIs 
#t1 <- round(addmargins(xtabs(sts_wal1617$weight_wales ~ sts_wal1617$xyear + sts_wal1617$cigsmok)))
#x1 <- t1 %>%
 # as.data.frame() %>%
  #subset(sts_wal1617.cigsmok == 1) %>%
  #slice(-c(15)) %>%
  #mutate(sts_wal1617.cigsmok = NULL) %>%
  #mutate(sts_wal1617.xyear = NULL)

#x2 <- t1 %>%
 # as.data.frame() %>%
  #subset(sts_wal1617.cigsmok == "Sum") %>%
  #slice(-c(15)) %>%
  #mutate(sts_wal1617.cigsmok = NULL) %>%
  #mutate(sts_wal1617.xyear = NULL)

#x3 <- cbind(x1, x2)
#colnames(x3) <- c("N", "TotalN")

#CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95, method = 'wald')*100)
#colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
#df4 <- cbind(df3,x3,CI)

# Remove redundant columns
#df4 <- df4 %>%
 # mutate(N = NULL) %>%
  #mutate(TotalN = NULL) %>%
  #mutate(est = NULL)

# Round to 1dp
#df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
#df5 <- cbind(df4, n2)

# Unite year and total N into one column
#df5$Freq <- interaction("(N=", df5$Freq, sep = "")
#df5$Freq <- interaction(df5$Freq, ")", sep = "")
#df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
#df6 <- unite(df5, "Year", Year:Freq, sep = " ")

# PUSH to google sheet
#sheet_write(df6, "1Gioc_e-Ww1AmPCjI9h-_rJGgpyTwY_ztZRA_FKZQeeI", sheet = 'Sheet1')


## What CI method does SPSS use???
#library(survey)
#sts_wal1617_16 <- sts_wal1617 %>% subset(xyear == 2016)
#sts_wal1617_17 <- sts_wal1617 %>% subset(xyear == 2017)
#sts_wal1617_19 <- sts_wal1617 %>% subset(xyear == 2019)
#sts_wal1617_20 <- sts_wal1617 %>% subset(xyear == 2020)

#svy_sts_wal1617_16 <- svydesign(ids = ~1, weights = ~weight_wales, data = as.data.frame(sts_wal1617_16))
#svy_sts_wal1617_17 <- svydesign(ids = ~1, weights = ~weight_wales, data = as.data.frame(sts_wal1617_17))
#svy_sts_wal1617_19 <- svydesign(ids = ~1, weights = ~weight_wales, data = as.data.frame(sts_wal1617_19))
#svy_sts_wal1617_20 <- svydesign(ids = ~1, weights = ~weight_wales, data = as.data.frame(sts_wal1617_20))

# method = c('mean', 'logit', ''likelihood', 'beta', 'asin') 
# https://www.rdocumentation.org/packages/survey/versions/4.0/topics/svyciprop
#svyciprop(~I(cigsmok==1), svy_sts_wal1617_16, method = 'mean', level = 0.95)

#confint(svymean(~I(cigsmok==1), svy_sts_wal1617_16)) # Alternative method...


#####################################
# STOPPED SMOKING IN PAST 12 MONTHS #
#####################################

sts_wal_smokly <- sts_wal %>% subset(smokly == 1) 

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_wal_smokly$weight_wales ~ sts_wal_smokly$notsmo + sts_wal_smokly$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_wal_smokly$weight_wales ~ sts_wal_smokly$notsmo + sts_wal_smokly$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_wal_smokly.notsmo != 0) %>%
  subset(sts_wal_smokly.notsmo != 1) %>%
  mutate(sts_wal_smokly.xyear = NULL) %>%
  mutate(sts_wal_smokly.notsmo = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_wal_smokly$weight_wales ~ sts_wal_smokly$xyear + sts_wal_smokly$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal_smokly.notsmo == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_smokly.notsmo = NULL) %>%
  mutate(sts_wal_smokly.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal_smokly.notsmo == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_wal_smokly.notsmo = NULL) %>%
  mutate(sts_wal_smokly.xyear = NULL)

x3 <- cbind(x1, x2)
colnames(x3) <- c("N", "TotalN")

CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95, method = 'wald')*100)
colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
df4 <- cbind(df3,x3,CI)

# Remove redundant columns
df4 <- df4 %>%
  mutate(N = NULL) %>%
  mutate(TotalN = NULL) %>%
  mutate(est = NULL)

# Round to 1dp
df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
df5 <- cbind(df4, n2)

# Unite year and total N into one column so shows as "Year (N = xyz)
df5$Freq <- interaction("(N=", df5$Freq, sep = "")
df5$Freq <- interaction(df5$Freq, ")", sep = "")
df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
df6 <- unite(df5, "Year", Year:Freq, sep = " ")
colnames(df6) <- c("A", "B", "C", "D")

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1_npGp0msT-ouuX8qS_JSHy30NscYY-jXNRM_Z0dp6Bo", sheet = 'Sheet1')


######################################
# Tried to stop smoking in past year #
######################################

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_wal_smokly$weight_wales ~ sts_wal_smokly$trylyc + sts_wal_smokly$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_wal_smokly$weight_wales ~ sts_wal_smokly$trylyc + sts_wal_smokly$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_wal_smokly.trylyc != 0) %>%
  subset(sts_wal_smokly.trylyc != 1) %>%
  mutate(sts_wal_smokly.trylyc = NULL) %>%
  mutate(sts_wal_smokly.xyear = NULL) %>%
  mutate(sts_wal_smokly.trylyc = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_wal_smokly$weight_wales ~ sts_wal_smokly$xyear + sts_wal_smokly$trylyc)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal_smokly.trylyc == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_smokly.trylyc = NULL) %>%
  mutate(sts_wal_smokly.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_wal_smokly$weight_wales ~ sts_wal_smokly$xyear + sts_wal_smokly$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_wal_smokly.trylyc != 1) %>%
  subset(sts_wal_smokly.trylyc != 0) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_smokly.trylyc = NULL) %>%
  mutate(sts_wal_smokly.xyear = NULL)

x3 <- cbind(x1, x2)
colnames(x3) <- c("N", "TotalN")

CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95, method = 'wald')*100)
colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
df4 <- cbind(df3,x3,CI)

# Remove redundant columns
df4 <- df4 %>%
  mutate(N = NULL) %>%
  mutate(TotalN = NULL) %>%
  mutate(est = NULL)

# Round to 1dp
df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
df5 <- cbind(df4, n2)

# Unite year and total N into one column
df5$Freq <- interaction("(N=", df5$Freq, sep = "")
df5$Freq <- interaction(df5$Freq, ")", sep = "")
df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
df6 <- unite(df5, "Year", Year:Freq, sep = " ")
colnames(df6) <- c("A", "B", "C", "D")

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1cu22jssOrmilBNj8MTGPdts4yM1f96WNqhTeZ5ShVuI", sheet = 'Sheet1')

###############################################
# Success rate in stopping in those who tried #
###############################################

sts_wal_trylyc <- sts_wal %>% subset(smokly == 1 & trylyc == 1)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_wal_trylyc$weight_wales ~ sts_wal_trylyc$notsmo + sts_wal_trylyc$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_wal_trylyc$weight_wales ~ sts_wal_trylyc$trylyc + sts_wal_trylyc$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_wal_trylyc.trylyc != 0) %>%
  subset(sts_wal_trylyc.trylyc != 1) %>%
  mutate(sts_wal_trylyc.trylyc = NULL) %>%
  mutate(sts_wal_trylyc.xyear = NULL) %>%
  mutate(sts_wal_trylyc.notsmo = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_wal_trylyc$weight_wales ~ sts_wal_trylyc$xyear + sts_wal_trylyc$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal_trylyc.notsmo == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_trylyc.notsmo = NULL) %>%
  mutate(sts_wal_trylyc.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_wal_trylyc$weight_wales ~ sts_wal_trylyc$xyear + sts_wal_trylyc$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_wal_trylyc.trylyc == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_wal_trylyc.trylyc = NULL) %>%
  mutate(sts_wal_trylyc.xyear = NULL)

x3 <- cbind(x1, x2)
colnames(x3) <- c("N", "TotalN")

CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95, method = 'wald')*100)
colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
df4 <- cbind(df3,x3,CI)

# Remove redundant columns
df4 <- df4 %>%
  mutate(N = NULL) %>%
  mutate(TotalN = NULL) %>%
  mutate(est = NULL)

# Round to 1dp
df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
df5 <- cbind(df4, n2)

# Unite year and total N into one column
df5$Freq <- interaction("(N=", df5$Freq, sep = "")
df5$Freq <- interaction(df5$Freq, ")", sep = "")
df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
df6 <- unite(df5, "Year", Year:Freq, sep = " ")
colnames(df6) <- c("A", "B", "C", "D")

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1W4na8UD-mNPsOz4xJ8dh0QRV-seqqsUjKmOL1LgMASI", sheet = 'Sheet1')

#######

##############################################
# Tried to stop smoking in past year (18-24) #
##############################################

# Wait for full year of 2021 data

sts_wal_smokly1824 <- sts_wal_smokly %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_wal_smokly1824$weight_wales ~ sts_wal_smokly1824$trylyc + sts_wal_smokly1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_wal_smokly1824$weight_wales ~ sts_wal_smokly1824$trylyc + sts_wal_smokly1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_wal_smokly1824.trylyc != 0) %>%
  subset(sts_wal_smokly1824.trylyc != 1) %>%
  mutate(sts_wal_smokly1824.trylyc = NULL) %>%
  mutate(sts_wal_smokly1824.xyear = NULL) %>%
  mutate(sts_wal_smokly1824.trylyc = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_wal_smokly1824$weight_wales ~ sts_wal_smokly1824$xyear + sts_wal_smokly1824$trylyc)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal_smokly1824.trylyc == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_smokly1824.trylyc = NULL) %>%
  mutate(sts_wal_smokly1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_wal_smokly1824$weight_wales ~ sts_wal_smokly1824$xyear + sts_wal_smokly1824$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_wal_smokly1824.trylyc != 1) %>%
  subset(sts_wal_smokly1824.trylyc != 0) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_smokly1824.trylyc = NULL) %>%
  mutate(sts_wal_smokly1824.xyear = NULL)

x3 <- cbind(x1, x2)
colnames(x3) <- c("N", "TotalN")

CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95, method = 'wald')*100)
colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
df4 <- cbind(df3,x3,CI)

# Remove redundant columns
df4 <- df4 %>%
  mutate(N = NULL) %>%
  mutate(TotalN = NULL) %>%
  mutate(est = NULL)

# Round to 1dp
df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
df5 <- cbind(df4, n2)

# Unite year and total N into one column
df5$Freq <- interaction("(N=", df5$Freq, sep = "")
df5$Freq <- interaction(df5$Freq, ")", sep = "")
df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
df6 <- unite(df5, "Year", Year:Freq, sep = " ")
colnames(df6) <- c("A", "B", "C", "D")

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1HrLEK5cXvGhjpjA2HcWnlxPsWlTL5zj6uB3NJCHxNCs", sheet = 'Sheet1')

################################################
# Success rate for stopping in 18-24 who tried #
################################################

# Wait for full 2021 data

sts_wal_trylyc1824 <- sts_wal_trylyc %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_wal_trylyc1824$weight_wales ~ sts_wal_trylyc1824$notsmo + sts_wal_trylyc1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_wal_trylyc1824$weight_wales ~ sts_wal_trylyc1824$trylyc + sts_wal_trylyc1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_wal_trylyc1824.trylyc != 0) %>%
  subset(sts_wal_trylyc1824.trylyc != 1) %>%
  mutate(sts_wal_trylyc1824.trylyc = NULL) %>%
  mutate(sts_wal_trylyc1824.xyear = NULL) %>%
  mutate(sts_wal_trylyc1824.notsmo = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_wal_trylyc1824$weight_wales ~ sts_wal_trylyc1824$xyear + sts_wal_trylyc1824$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal_trylyc1824.notsmo == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_trylyc1824.notsmo = NULL) %>%
  mutate(sts_wal_trylyc1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_wal_trylyc1824$weight_wales ~ sts_wal_trylyc1824$xyear + sts_wal_trylyc1824$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_wal_trylyc1824.trylyc == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_wal_trylyc1824.trylyc = NULL) %>%
  mutate(sts_wal_trylyc1824.xyear = NULL)

x3 <- cbind(x1, x2)
colnames(x3) <- c("N", "TotalN")

CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95, method = 'wald')*100)
colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
df4 <- cbind(df3,x3,CI)

# Remove redundant columns
df4 <- df4 %>%
  mutate(N = NULL) %>%
  mutate(TotalN = NULL) %>%
  mutate(est = NULL)

# Round to 1dp
df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
df5 <- cbind(df4, n2)

# Unite year and total N into one column
df5$Freq <- interaction("(N=", df5$Freq, sep = "")
df5$Freq <- interaction(df5$Freq, ")", sep = "")
df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
df6 <- unite(df5, "Year", Year:Freq, sep = " ")
colnames(df6) <- c("A", "B", "C", "D")

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "15UFgeEDjXLJm_jQo81t-c6m5Qb0Dob8dREsQoaYN2dc", sheet = 'Sheet1')

############################################
# Uptake: prevalence of ever smoking 18-24 #
############################################

sts_wal_1824 <- sts_wal %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_wal_1824$weight_wales ~ sts_wal_1824$eversmo + sts_wal_1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_wal_1824$weight_wales ~ sts_wal_1824$eversmo + sts_wal_1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_wal_1824.eversmo != 0) %>%
  subset(sts_wal_1824.eversmo != 1) %>%
  mutate(sts_wal_1824.eversmo = NULL) %>%
  mutate(sts_wal_1824.xyear = NULL) %>%
  mutate(sts_wal_1824.eversmo = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_wal_1824$weight_wales ~ sts_wal_1824$xyear + sts_wal_1824$eversmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal_1824.eversmo == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_1824.eversmo = NULL) %>%
  mutate(sts_wal_1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_wal_1824$weight_wales ~ sts_wal_1824$xyear + sts_wal_1824$eversmo)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_wal_1824.eversmo != 1) %>%
  subset(sts_wal_1824.eversmo != 0) %>%
  slice(-c(4)) %>%
  mutate(sts_wal_1824.eversmo = NULL) %>%
  mutate(sts_wal_1824.xyear = NULL)

x3 <- cbind(x1, x2)
colnames(x3) <- c("N", "TotalN")

CI <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95, method = 'wald')*100)
colnames(CI) <- c("est", "Lower 95% CI", "Upper 95% CI")

# Combine prevalence data with Ns and CIs
df4 <- cbind(df3,x3,CI)

# Remove redundant columns
df4 <- df4 %>%
  mutate(N = NULL) %>%
  mutate(TotalN = NULL) %>%
  mutate(est = NULL)

# Round to 1dp
df4 <- round(df4, digits = 1)

# Combine prevalence data with total N
df5 <- cbind(df4, n2)

# Unite year and total N into one column
df5$Freq <- interaction("(N=", df5$Freq, sep = "")
df5$Freq <- interaction(df5$Freq, ")", sep = "")
df5 <- df5[c(1,5,2,3,4)]

# Final dataframe to push to server
df6 <- unite(df5, "Year", Year:Freq, sep = " ")
colnames(df6) <- c("A", "B", "C", "D")

df7 <- rbind(headings, df6)
colnames(df7) <- c()

# PUSH to google sheet
sheet_write(df7, "1KHzeNDvp0MjbsRfBgm9FXfyep_zF57PqAamEbZ0xbCg", sheet = 'Sheet1')
