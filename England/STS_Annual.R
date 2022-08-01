### STS/ATS ANNUAL FINDINGS ###
library(tidyverse)


drive_auth() # Enter 1 for smokingchartdata@gmail.com

gs4_auth(token = drive_token()) # Only run this line once you have entered 1 

sts <- read_sav("C:/Toolkit merge files/Waves/183/omni183_39.1_65.2cot_31.3a_25.4s_recodes_69.5sa.sav")
names(sts)[names(sts)=="@weight0"] <- "weight0" 

sts_eng <- sts %>% subset(xyear >= 2007 & xyear < 2022 & gore < 10)

drive_auth() # Enter 1 for smokingchartdata@gmail.com

gs4_auth(token = drive_token()) # Only run this line once you have entered 1 

##########################################
# Cigarette smoking prevalence in adults #
##########################################

A <- c("Graph shows prevalence estimate and upper and lower 95% confidence intervals", "Year", "")
B <- c("", "Percent", "Prevalence estimate")
C <- c("", "", "Lower 95% CI")
D <- c("", "", "Upper 95% CI")
headings <- cbind(A, B, C, D)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$cigsmok + sts_eng$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
Year <- c(2007:2021)
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_eng$weight0 ~ sts_eng$cigsmok + sts_eng$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_eng.cigsmok != 0) %>%
  subset(sts_eng.cigsmok != 1) %>%
  subset(sts_eng.xyear != "Sum") %>%
  mutate(sts_eng.xyear = NULL) %>%
  mutate(sts_eng.cigsmok = NULL)

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$cigsmok)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.cigsmok == 1) %>%
  slice(-c(16)) %>%
  mutate(sts_eng.cigsmok = NULL) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.cigsmok == "Sum") %>%
  slice(-c(16)) %>%
  mutate(sts_eng.cigsmok = NULL) %>%
  mutate(sts_eng.xyear = NULL)

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
sheet_write(df7, "1VLMzalW5FmPeFA9B-XdFEC_6l7qmkuTjmh1_y8trTOo", sheet = 'Sheet1')

##################################################################
# Proportion of cigarette smokers who use hand-rolled cigarettes #
##################################################################

cigsmok <- sts_eng %>% subset(cigsmok == 1)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(cigsmok$weight0 ~ cigsmok$ryosmok + cigsmok$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(cigsmok$weight0 ~ cigsmok$ryosmok + cigsmok$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(cigsmok.ryosmok != 0) %>%
  subset(cigsmok.ryosmok != 1) %>%
  subset(cigsmok.xyear != "Sum") %>%
  mutate(cigsmok.xyear = NULL) %>%
  mutate(cigsmok.ryosmok = NULL)

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(cigsmok$weight0 ~ cigsmok$xyear + cigsmok$ryosmok)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(cigsmok.ryosmok == 1) %>%
  slice(-c(16)) %>%
  mutate(cigsmok.ryosmok = NULL) %>%
  mutate(cigsmok.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(cigsmok.ryosmok == "Sum") %>%
  slice(-c(16)) %>%
  mutate(cigsmok.ryosmok = NULL) %>%
  mutate(cigsmok.xyear = NULL)

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
sheet_write(df7, "1sqsUwLizjY42T6hQ0I3w7fW9W29HYFMQqH2hc5IMml8", sheet = 'Sheet1')

#############################################################
# Proportion of cigarette smokers who are non-daily smokers #
#############################################################

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(cigsmok$weight0 ~ cigsmok$nondaily + cigsmok$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(cigsmok$weight0 ~ cigsmok$nondaily + cigsmok$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(cigsmok.nondaily != 0) %>%
  subset(cigsmok.nondaily != 1) %>%
  subset(cigsmok.xyear != "Sum") %>%
  mutate(cigsmok.xyear = NULL) %>%
  mutate(cigsmok.nondaily = NULL)

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(cigsmok$weight0 ~ cigsmok$xyear + cigsmok$nondaily)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(cigsmok.nondaily == 1) %>%
  slice(-c(16)) %>%
  mutate(cigsmok.nondaily = NULL) %>%
  mutate(cigsmok.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(cigsmok.nondaily == "Sum") %>%
  slice(-c(16)) %>%
  mutate(cigsmok.nondaily = NULL) %>%
  mutate(cigsmok.xyear = NULL)

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
sheet_write(df7, "1aA9aE9QnOH3SP0ZGShuVOPb17tLoMCgYVW2CcFGajz8", sheet = 'Sheet1')



###############################################
# Daily cigarette consumption by social grade #
###############################################

# Haven't done in R yet. Need to work out how to do mean for basecpd2 etc by year.

A <- c("ABC1: Professional to clerical occupation C2DE: Manual occupation", "Year", "")
B <- c("", "Percent", "All")
C <- c("", "", "ABC1")
D <- c("", "", "C2DE")
headings <- cbind(A, B, C, D)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(cigsmok$weight0 ~ cigsmok$basecpd2 + cigsmok$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

library(survey)
sts2007 <- cigsmok %>% subset(xyear == 2007)
sts2008 <- cigsmok %>% subset(xyear == 2008)
sts2009 <- cigsmok %>% subset(xyear == 2009)
sts2010 <- cigsmok %>% subset(xyear == 2010)
sts2011 <- cigsmok %>% subset(xyear == 2011)
sts2012 <- cigsmok %>% subset(xyear == 2012)
sts2013 <- cigsmok %>% subset(xyear == 2013)
sts2014 <- cigsmok %>% subset(xyear == 2014)
sts2015 <- cigsmok %>% subset(xyear == 2015)
sts2016 <- cigsmok %>% subset(xyear == 2016)
sts2017 <- cigsmok %>% subset(xyear == 2017)
sts2018 <- cigsmok %>% subset(xyear == 2018)
sts2019 <- cigsmok %>% subset(xyear == 2019)
sts2020 <- cigsmok %>% subset(xyear == 2020)

svy_sts2007 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2007)) # convert dataframe to survey object
svy_sts2008 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2008)) 
svy_sts2009 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2009)) 
svy_sts2010 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2010)) 
svy_sts2011 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2011))
svy_sts2012 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2012)) 
svy_sts2013 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2013)) 
svy_sts2014 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2014)) 
svy_sts2015 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2015))
svy_sts2016 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2016)) 
svy_sts2017 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2017)) 
svy_sts2018 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2018)) 
svy_sts2019 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2019))
svy_sts2020 <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts2020))

basecpd2007 <- svymean(~basecpd2, svy_sts2007, na.rm = TRUE)
basecpd2008 <- svymean(~basecpd2, svy_sts2008, na.rm = TRUE)
basecpd2009 <- svymean(~basecpd2, svy_sts2009, na.rm = TRUE)
basecpd2010 <- svymean(~basecpd2, svy_sts2010, na.rm = TRUE)
basecpd2011 <- svymean(~basecpd2, svy_sts2011, na.rm = TRUE)
basecpd2012 <- svymean(~basecpd2, svy_sts2012, na.rm = TRUE)
basecpd2013 <- svymean(~basecpd2, svy_sts2013, na.rm = TRUE)
basecpd2014 <- svymean(~basecpd2, svy_sts2014, na.rm = TRUE)
basecpd2015 <- svymean(~basecpd2, svy_sts2015, na.rm = TRUE)
basecpd2016 <- svymean(~basecpd2, svy_sts2016, na.rm = TRUE)
basecpd2017 <- svymean(~basecpd2, svy_sts2017, na.rm = TRUE)
basecpd2018 <- svymean(~basecpd2, svy_sts2018, na.rm = TRUE)
basecpd2019 <- svymean(~basecpd2, svy_sts2019, na.rm = TRUE)
basecpd2020 <- svymean(~basecpd2, svy_sts2020, na.rm = TRUE)

all_basecpd <- rbind(basecpd2007, basecpd2008, basecpd2009, basecpd2010, basecpd2011, basecpd2012, basecpd2013, basecpd2014, basecpd2015,
      basecpd2016, basecpd2017, basecpd2018, basecpd2019, basecpd2020)


###########################################
# Support for Tobacco availability policy #
###########################################


sts_gb <- sts %>% filter(xwave == 179)

# Factor policy support

attributes(sts_gb$q632aplp21_15)

# Ban
sts_gb$q632aplp21_01 <- factor(sts_gb$q632aplp21_01, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_01) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

levels(sts_gb$q632aplp21_01)

# T21
sts_gb$q632aplp21_02 <- factor(sts_gb$q632aplp21_02, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_02) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Industry fund
sts_gb$q632aplp21_03 <- factor(sts_gb$q632aplp21_03, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_03) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Industry disclose
sts_gb$q632aplp21_04 <- factor(sts_gb$q632aplp21_04, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_04) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Licensing
sts_gb$q632aplp21_05 <- factor(sts_gb$q632aplp21_05, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_05) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Tax
sts_gb$q632aplp21_06 <- factor(sts_gb$q632aplp21_06, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_06) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Health warnings
sts_gb$q632aplp21_07 <- factor(sts_gb$q632aplp21_07, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_07) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Support SSS
sts_gb$q632aplp21_08 <- factor(sts_gb$q632aplp21_08, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_08) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Govt spending advertising
sts_gb$q632aplp21_09 <- factor(sts_gb$q632aplp21_09, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_09) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Banning smoking in homes with children
sts_gb$q632aplp21_10 <- factor(sts_gb$q632aplp21_10, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_10) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# E-cigs on prescription
sts_gb$q632aplp21_11 <- factor(sts_gb$q632aplp21_11, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_11) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Restrict e-cig advertising
sts_gb$q632aplp21_12 <- factor(sts_gb$q632aplp21_12, levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_12) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Density
sts_gb$q632aplp21_13 <- factor(sts_gb$q632aplp21_13, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_13) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Schools
sts_gb$q632aplp21_14 <- factor(sts_gb$q632aplp21_14, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$q632aplp21_14) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

Response <- c("Support", "Oppose", "No opinion/unsure")

# Ban
t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_01)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ban <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ban) <- c("est", "LCI", "UCI")
CI_ban <- round(CI_ban, digits = 1)
CI_ban <- subset(CI_ban, select = -c(LCI, UCI))
colnames(CI_ban) <- "Ban"

# T21

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_02)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_21 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_21) <- c("est", "LCI", "UCI")

CI_21 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_21) <- c("est", "LCI", "UCI")
CI_21 <- round(CI_21, digits = 1)
CI_21 <- subset(CI_21, select = -c(LCI, UCI))
colnames(CI_21) <- "T21"

# Industry fund

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_03)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_fund <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_fund) <- c("est", "LCI", "UCI")

CI_fund <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_fund) <- c("est", "LCI", "UCI")
CI_fund <- round(CI_fund, digits = 1)
CI_fund <- subset(CI_fund, select = -c(LCI, UCI))
colnames(CI_fund) <- "Industry fund"

# Industry disclose

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_04)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")
CI_disclose <- round(CI_disclose, digits = 1)
CI_disclose <- subset(CI_disclose, select = -c(LCI, UCI))
colnames(CI_disclose) <- "Industry disclose"

# License

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_05)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_license <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_license) <- c("est", "LCI", "UCI")

CI_license <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_license) <- c("est", "LCI", "UCI")
CI_license <- round(CI_license, digits = 1)
CI_license <- subset(CI_license, select = -c(LCI, UCI))
colnames(CI_license) <- "Retailer license"

# Tax

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_06)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")
CI_tax <- round(CI_tax, digits = 1)
CI_tax <- subset(CI_tax, select = -c(LCI, UCI))
colnames(CI_tax) <- "Tax"

# Health warnings

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_07)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_health <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_health) <- c("est", "LCI", "UCI")

CI_health <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_health) <- c("est", "LCI", "UCI")
CI_health <- round(CI_health, digits = 1)
CI_health <- subset(CI_health, select = -c(LCI, UCI))
colnames(CI_health) <- "Health warnings"

# Support to quit

t1 <- round(addmargins(xtabs(sts_gb_gb$weight_gb ~ sts_gb_gb$xyear + sts_gb_gb$q632aplp21_08)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_quit <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_quit) <- c("est", "LCI", "UCI")

CI_quit <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_quit) <- c("est", "LCI", "UCI")
CI_quit <- round(CI_quit, digits = 1)
CI_quit <- subset(CI_quit, select = -c(LCI, UCI))
colnames(CI_quit) <- "Support to quit"

# Govt spend

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_09)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_spend <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_spend) <- c("est", "LCI", "UCI")

CI_spend <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_spend) <- c("est", "LCI", "UCI")
CI_spend <- round(CI_spend, digits = 1)
CI_spend <- subset(CI_spend, select = -c(LCI, UCI))
colnames(CI_spend) <- "Increase government spend"

# Banning smoking in homes with children

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_10)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_homes <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_homes) <- c("est", "LCI", "UCI")

CI_homes <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_homes) <- c("est", "LCI", "UCI")
CI_homes <- round(CI_homes, digits = 1)
CI_homes <- subset(CI_homes, select = -c(LCI, UCI))
colnames(CI_homes) <- "Ban smoking in homes with children"

# E-cigs on prescription

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_11)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ECRx <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECRx) <- c("est", "LCI", "UCI")

CI_ECRx <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECRx) <- c("est", "LCI", "UCI")
CI_ECRx <- round(CI_ECRx, digits = 1)
CI_ECRx <- subset(CI_ECRx, select = -c(LCI, UCI))
colnames(CI_ECRx) <- "EC on prescription"

# Restrict e-cig advertising

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ECAd <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECAd) <- c("est", "LCI", "UCI")

CI_ECAd <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECAd) <- c("est", "LCI", "UCI")
CI_ECAd <- round(CI_ECAd, digits = 1)
CI_ECAd <- subset(CI_ECAd, select = -c(LCI, UCI))
colnames(CI_ECAd) <- "Restrict EC advertising"

# Density of retailers

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_13)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_dens <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_dens) <- c("est", "LCI", "UCI")

CI_dens <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_dens) <- c("est", "LCI", "UCI")
CI_dens <- round(CI_dens, digits = 1)
CI_dens <- subset(CI_dens, select = -c(LCI, UCI))
colnames(CI_dens) <- "Reduce retailer numbers"

# Schools

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$q632aplp21_14)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_sch <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_sch) <- c("est", "LCI", "UCI")

CI_sch <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_sch) <- c("est", "LCI", "UCI")
CI_sch <- round(CI_sch, digits = 1)
CI_sch <- subset(CI_sch, select = -c(LCI, UCI))
colnames(CI_sch) <- "Reduce retailer numbers"


df <- cbind(Response, CI_ban, CI_21, CI_fund, CI_disclose, CI_license, CI_tax, CI_health, CI_quit, CI_spend, CI_homes, CI_ECRx, CI_ECAd, CI_dens, CI_sch)

colnames(df) <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                  "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")
colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O")

df_pivot <- as.data.frame(t(df))

rownames(df_pivot) <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                        "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")

policy_name <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                 "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")

df_pivot_2 <- cbind(policy_name, df_pivot)

colnames(df_pivot_2) <- c("A", "B", "C", "D")

n <- 2013

row2 <- c(n, ")", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E")

row2$A <- interaction("(N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = "")
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("Policy", "Percent", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

row4 <- t(row4) %>% as.data.frame()
colnames(row4) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O")  

df2 <- rbind(row2, row3, df_pivot_2)
colnames(df2) <- c()

# PUSH to google sheet
sheet_write(df2, "10a9__6sIGBQTOdts2CUyf_F9YHhPq3YFPIIoNVztJ-g", sheet = 'Sheet1')

############
# SCOTLAND #


sts_scot <- sts_gb %>% subset(gore == 11)

# Factor policy support

# Ban
t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_01)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ban <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ban) <- c("est", "LCI", "UCI")
CI_ban <- round(CI_ban, digits = 1)
CI_ban <- subset(CI_ban, select = -c(LCI, UCI))
colnames(CI_ban) <- "Ban"

# T21

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_02)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_21 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_21) <- c("est", "LCI", "UCI")

CI_21 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_21) <- c("est", "LCI", "UCI")
CI_21 <- round(CI_21, digits = 1)
CI_21 <- subset(CI_21, select = -c(LCI, UCI))
colnames(CI_21) <- "T21"

# Industry fund

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_03)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_fund <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_fund) <- c("est", "LCI", "UCI")

CI_fund <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_fund) <- c("est", "LCI", "UCI")
CI_fund <- round(CI_fund, digits = 1)
CI_fund <- subset(CI_fund, select = -c(LCI, UCI))
colnames(CI_fund) <- "Industry fund"

# Industry disclose

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_04)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")
CI_disclose <- round(CI_disclose, digits = 1)
CI_disclose <- subset(CI_disclose, select = -c(LCI, UCI))
colnames(CI_disclose) <- "Industry disclose"

# License

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_05)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_license <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_license) <- c("est", "LCI", "UCI")

CI_license <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_license) <- c("est", "LCI", "UCI")
CI_license <- round(CI_license, digits = 1)
CI_license <- subset(CI_license, select = -c(LCI, UCI))
colnames(CI_license) <- "Retailer license"

# Tax

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_06)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")
CI_tax <- round(CI_tax, digits = 1)
CI_tax <- subset(CI_tax, select = -c(LCI, UCI))
colnames(CI_tax) <- "Tax"

# Health warnings

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_07)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_health <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_health) <- c("est", "LCI", "UCI")

CI_health <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_health) <- c("est", "LCI", "UCI")
CI_health <- round(CI_health, digits = 1)
CI_health <- subset(CI_health, select = -c(LCI, UCI))
colnames(CI_health) <- "Health warnings"

# Support to quit

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_08)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_quit <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_quit) <- c("est", "LCI", "UCI")

CI_quit <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_quit) <- c("est", "LCI", "UCI")
CI_quit <- round(CI_quit, digits = 1)
CI_quit <- subset(CI_quit, select = -c(LCI, UCI))
colnames(CI_quit) <- "Support to quit"

# Govt spend

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_09)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_spend <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_spend) <- c("est", "LCI", "UCI")

CI_spend <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_spend) <- c("est", "LCI", "UCI")
CI_spend <- round(CI_spend, digits = 1)
CI_spend <- subset(CI_spend, select = -c(LCI, UCI))
colnames(CI_spend) <- "Increase government spend"

# Banning smoking in homes with children

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_10)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_homes <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_homes) <- c("est", "LCI", "UCI")

CI_homes <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_homes) <- c("est", "LCI", "UCI")
CI_homes <- round(CI_homes, digits = 1)
CI_homes <- subset(CI_homes, select = -c(LCI, UCI))
colnames(CI_homes) <- "Ban smoking in homes with children"

# E-cigs on prescription

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_11)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ECRx <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECRx) <- c("est", "LCI", "UCI")

CI_ECRx <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECRx) <- c("est", "LCI", "UCI")
CI_ECRx <- round(CI_ECRx, digits = 1)
CI_ECRx <- subset(CI_ECRx, select = -c(LCI, UCI))
colnames(CI_ECRx) <- "EC on prescription"

# Restrict e-cig advertising

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ECAd <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECAd) <- c("est", "LCI", "UCI")

CI_ECAd <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECAd) <- c("est", "LCI", "UCI")
CI_ECAd <- round(CI_ECAd, digits = 1)
CI_ECAd <- subset(CI_ECAd, select = -c(LCI, UCI))
colnames(CI_ECAd) <- "Restrict EC advertising"

# Density of retailers

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_13)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_dens <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_dens) <- c("est", "LCI", "UCI")

CI_dens <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_dens) <- c("est", "LCI", "UCI")
CI_dens <- round(CI_dens, digits = 1)
CI_dens <- subset(CI_dens, select = -c(LCI, UCI))
colnames(CI_dens) <- "Reduce retailer numbers"

# Schools

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$q632aplp21_14)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_sch <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_sch) <- c("est", "LCI", "UCI")

CI_sch <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_sch) <- c("est", "LCI", "UCI")
CI_sch <- round(CI_sch, digits = 1)
CI_sch <- subset(CI_sch, select = -c(LCI, UCI))
colnames(CI_sch) <- "Reduce retailer numbers"


df <- cbind(Response, CI_ban, CI_21, CI_fund, CI_disclose, CI_license, CI_tax, CI_health, CI_quit, CI_spend, CI_homes, CI_ECRx, CI_ECAd, CI_dens, CI_sch)

colnames(df) <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                  "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")
colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O")

df_pivot <- as.data.frame(t(df))

rownames(df_pivot) <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                        "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")

policy_name <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                 "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")

df_pivot_2 <- cbind(policy_name, df_pivot)

colnames(df_pivot_2) <- c("A", "B", "C", "D")

nrow(sts_scot)
n <- 361

row2 <- c(n, ")", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E")

row2$A <- interaction("(Unweighted N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = "")
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("Policy", "Percent", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

df2 <- rbind(row2, row3, df_pivot_2)
colnames(df2) <- c()

# PUSH to google sheet
sheet_write(df2, "1mMJpZsbkCe7X3ZTmmFIA1LPvLF7uKW4fvQnVuY4Z10k", sheet = 'Sheet1')

############
# WALES #


sts_wal <- sts_gb %>% subset(gore == 10)

# Factor policy support

attributes(sts_wal$q632aplp21_15)

Response <- c("Support", "Oppose", "No opinion/unsure")

# Ban
t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_01)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ban <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ban) <- c("est", "LCI", "UCI")
CI_ban <- round(CI_ban, digits = 1)
CI_ban <- subset(CI_ban, select = -c(LCI, UCI))
colnames(CI_ban) <- "Ban"

# T21

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_02)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_21 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_21) <- c("est", "LCI", "UCI")

CI_21 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_21) <- c("est", "LCI", "UCI")
CI_21 <- round(CI_21, digits = 1)
CI_21 <- subset(CI_21, select = -c(LCI, UCI))
colnames(CI_21) <- "T21"

# Industry fund

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_03)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_fund <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_fund) <- c("est", "LCI", "UCI")

CI_fund <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_fund) <- c("est", "LCI", "UCI")
CI_fund <- round(CI_fund, digits = 1)
CI_fund <- subset(CI_fund, select = -c(LCI, UCI))
colnames(CI_fund) <- "Industry fund"

# Industry disclose

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_04)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")
CI_disclose <- round(CI_disclose, digits = 1)
CI_disclose <- subset(CI_disclose, select = -c(LCI, UCI))
colnames(CI_disclose) <- "Industry disclose"

# License

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_05)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_license <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_license) <- c("est", "LCI", "UCI")

CI_license <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_license) <- c("est", "LCI", "UCI")
CI_license <- round(CI_license, digits = 1)
CI_license <- subset(CI_license, select = -c(LCI, UCI))
colnames(CI_license) <- "Retailer license"

# Tax

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_06)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")
CI_tax <- round(CI_tax, digits = 1)
CI_tax <- subset(CI_tax, select = -c(LCI, UCI))
colnames(CI_tax) <- "Tax"

# Health warnings

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_07)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_health <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_health) <- c("est", "LCI", "UCI")

CI_health <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_health) <- c("est", "LCI", "UCI")
CI_health <- round(CI_health, digits = 1)
CI_health <- subset(CI_health, select = -c(LCI, UCI))
colnames(CI_health) <- "Health warnings"

# Support to quit

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_08)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_quit <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_quit) <- c("est", "LCI", "UCI")

CI_quit <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_quit) <- c("est", "LCI", "UCI")
CI_quit <- round(CI_quit, digits = 1)
CI_quit <- subset(CI_quit, select = -c(LCI, UCI))
colnames(CI_quit) <- "Support to quit"

# Govt spend

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_09)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_spend <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_spend) <- c("est", "LCI", "UCI")

CI_spend <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_spend) <- c("est", "LCI", "UCI")
CI_spend <- round(CI_spend, digits = 1)
CI_spend <- subset(CI_spend, select = -c(LCI, UCI))
colnames(CI_spend) <- "Increase government spend"

# Banning smoking in homes with children

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_10)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_homes <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_homes) <- c("est", "LCI", "UCI")

CI_homes <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_homes) <- c("est", "LCI", "UCI")
CI_homes <- round(CI_homes, digits = 1)
CI_homes <- subset(CI_homes, select = -c(LCI, UCI))
colnames(CI_homes) <- "Ban smoking in homes with children"

# E-cigs on prescription

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_11)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ECRx <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECRx) <- c("est", "LCI", "UCI")

CI_ECRx <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECRx) <- c("est", "LCI", "UCI")
CI_ECRx <- round(CI_ECRx, digits = 1)
CI_ECRx <- subset(CI_ECRx, select = -c(LCI, UCI))
colnames(CI_ECRx) <- "EC on prescription"

# Restrict e-cig advertising

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ECAd <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECAd) <- c("est", "LCI", "UCI")

CI_ECAd <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECAd) <- c("est", "LCI", "UCI")
CI_ECAd <- round(CI_ECAd, digits = 1)
CI_ECAd <- subset(CI_ECAd, select = -c(LCI, UCI))
colnames(CI_ECAd) <- "Restrict EC advertising"

# Density of retailers

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_13)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_dens <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_dens) <- c("est", "LCI", "UCI")

CI_dens <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_dens) <- c("est", "LCI", "UCI")
CI_dens <- round(CI_dens, digits = 1)
CI_dens <- subset(CI_dens, select = -c(LCI, UCI))
colnames(CI_dens) <- "Reduce retailer numbers"

# Schools

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$q632aplp21_14)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.61496)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_sch <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_sch) <- c("est", "LCI", "UCI")

CI_sch <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_sch) <- c("est", "LCI", "UCI")
CI_sch <- round(CI_sch, digits = 1)
CI_sch <- subset(CI_sch, select = -c(LCI, UCI))
colnames(CI_sch) <- "Reduce retailer numbers"


df <- cbind(Response, CI_ban, CI_21, CI_fund, CI_disclose, CI_license, CI_tax, CI_health, CI_quit, CI_spend, CI_homes, CI_ECRx, CI_ECAd, CI_dens, CI_sch)

colnames(df) <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                  "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")
colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O")

df_pivot <- as.data.frame(t(df))

rownames(df_pivot) <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                        "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")

policy_name <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                 "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")

df_pivot_2 <- cbind(policy_name, df_pivot)

colnames(df_pivot_2) <- c("A", "B", "C", "D")

nrow(sts_wal)
n <- 183

row2 <- c(n, ")", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E")

row2$A <- interaction("(Unweighted N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = "")
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("Policy", "Percent", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

df2 <- rbind(row2, row3, df_pivot_2)
colnames(df2) <- c()

# PUSH to google sheet
sheet_write(df2, "1LQx711BWmR5adA1FabQkKzCCmEmL_G1XUsmOaeMDsHg", sheet = 'Sheet1')


############
# ENGLAND  #


sts_eng <- sts_gb %>% subset(gore < 10)

# Factor policy support

attributes(sts_wal$q632aplp21_15)

Response <- c("Support", "Oppose", "No opinion/unsure")

# Ban
t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_01)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ban <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ban) <- c("est", "LCI", "UCI")
CI_ban <- round(CI_ban, digits = 1)
CI_ban <- subset(CI_ban, select = -c(LCI, UCI))
colnames(CI_ban) <- "Ban"

# T21

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_02)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_21 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_21) <- c("est", "LCI", "UCI")

CI_21 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_21) <- c("est", "LCI", "UCI")
CI_21 <- round(CI_21, digits = 1)
CI_21 <- subset(CI_21, select = -c(LCI, UCI))
colnames(CI_21) <- "T21"

# Industry fund

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_03)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_fund <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_fund) <- c("est", "LCI", "UCI")

CI_fund <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_fund) <- c("est", "LCI", "UCI")
CI_fund <- round(CI_fund, digits = 1)
CI_fund <- subset(CI_fund, select = -c(LCI, UCI))
colnames(CI_fund) <- "Industry fund"

# Industry disclose

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_04)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")
CI_disclose <- round(CI_disclose, digits = 1)
CI_disclose <- subset(CI_disclose, select = -c(LCI, UCI))
colnames(CI_disclose) <- "Industry disclose"

# License

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_05)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_license <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_license) <- c("est", "LCI", "UCI")

CI_license <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_license) <- c("est", "LCI", "UCI")
CI_license <- round(CI_license, digits = 1)
CI_license <- subset(CI_license, select = -c(LCI, UCI))
colnames(CI_license) <- "Retailer license"

# Tax

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_06)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")
CI_tax <- round(CI_tax, digits = 1)
CI_tax <- subset(CI_tax, select = -c(LCI, UCI))
colnames(CI_tax) <- "Tax"

# Health warnings

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_07)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_health <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_health) <- c("est", "LCI", "UCI")

CI_health <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_health) <- c("est", "LCI", "UCI")
CI_health <- round(CI_health, digits = 1)
CI_health <- subset(CI_health, select = -c(LCI, UCI))
colnames(CI_health) <- "Health warnings"

# Support to quit

t1 <- (addmargins(xtabs(sts_eng_gb$weight0 ~ sts_eng_gb$xyear + sts_eng_gb$q632aplp21_08)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_quit <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_quit) <- c("est", "LCI", "UCI")

CI_quit <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_quit) <- c("est", "LCI", "UCI")
CI_quit <- round(CI_quit, digits = 1)
CI_quit <- subset(CI_quit, select = -c(LCI, UCI))
colnames(CI_quit) <- "Support to quit"

# Govt spend

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_09)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_spend <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_spend) <- c("est", "LCI", "UCI")

CI_spend <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_spend) <- c("est", "LCI", "UCI")
CI_spend <- round(CI_spend, digits = 1)
CI_spend <- subset(CI_spend, select = -c(LCI, UCI))
colnames(CI_spend) <- "Increase government spend"

# Banning smoking in homes with children

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_10)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_homes <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_homes) <- c("est", "LCI", "UCI")

CI_homes <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_homes) <- c("est", "LCI", "UCI")
CI_homes <- round(CI_homes, digits = 1)
CI_homes <- subset(CI_homes, select = -c(LCI, UCI))
colnames(CI_homes) <- "Ban smoking in homes with children"

# E-cigs on prescription

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_11)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ECRx <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECRx) <- c("est", "LCI", "UCI")

CI_ECRx <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECRx) <- c("est", "LCI", "UCI")
CI_ECRx <- round(CI_ECRx, digits = 1)
CI_ECRx <- subset(CI_ECRx, select = -c(LCI, UCI))
colnames(CI_ECRx) <- "EC on prescription"

# Restrict e-cig advertising

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ECAd <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECAd) <- c("est", "LCI", "UCI")

CI_ECAd <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ECAd) <- c("est", "LCI", "UCI")
CI_ECAd <- round(CI_ECAd, digits = 1)
CI_ECAd <- subset(CI_ECAd, select = -c(LCI, UCI))
colnames(CI_ECAd) <- "Restrict EC advertising"

# Density of retailers

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_13)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_dens <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_dens) <- c("est", "LCI", "UCI")

CI_dens <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_dens) <- c("est", "LCI", "UCI")
CI_dens <- round(CI_dens, digits = 1)
CI_dens <- subset(CI_dens, select = -c(LCI, UCI))
colnames(CI_dens) <- "Reduce retailer numbers"

# Schools

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$q632aplp21_14)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_sch <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_sch) <- c("est", "LCI", "UCI")

CI_sch <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_sch) <- c("est", "LCI", "UCI")
CI_sch <- round(CI_sch, digits = 1)
CI_sch <- subset(CI_sch, select = -c(LCI, UCI))
colnames(CI_sch) <- "Reduce retailer numbers"


df <- cbind(Response, CI_ban, CI_21, CI_fund, CI_disclose, CI_license, CI_tax, CI_health, CI_quit, CI_spend, CI_homes, CI_ECRx, CI_ECAd, CI_dens, CI_sch)

colnames(df) <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                  "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")
colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O")

df_pivot <- as.data.frame(t(df))

rownames(df_pivot) <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                        "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")

policy_name <- c("", "Ban", "T21", "Industry fund", "Industry disclose", "Retailer license", "Tax", "Health warnings", "Support to quit", "Government spend",
                 "Ban in homes with children", "EC on prescription", "EC advertising", "Retailer numbers", "Retail near schools")

df_pivot_2 <- cbind(policy_name, df_pivot)

colnames(df_pivot_2) <- c("A", "B", "C", "D")

nrow(sts_eng)
n <- 1653

row2 <- c(n, ")", "", "", "")
row2 <- t(row2) %>% as.data.frame()
colnames(row2) <- c("A", "B", "C", "D", "E")

row2$A <- interaction("(Unweighted N=", row2$A, sep = "")
row2 <- unite(row2, "A", A:B, sep = "")
colnames(row2) <- c("A", "B", "C", "D")

row3 <- c("Policy", "Percent", "", "")
row3 <- t(row3) %>% as.data.frame()
colnames(row3) <- c("A", "B", "C", "D")

df2 <- rbind(row2, row3, df_pivot_2)
colnames(df2) <- c()

# PUSH to google sheet
sheet_write(df2, "10a9__6sIGBQTOdts2CUyf_F9YHhPq3YFPIIoNVztJ-g", sheet = 'Sheet1')
