library(googlesheets4)
library(googledrive)
library(haven)
library(tigerstats)
library(dplyr)
library(tidyr)
library(DescTools)

sts <- read_sav("C:/Toolkit merge files/Waves/188/omni188_39.1_65.2cot_31.3a_25.4s_recodes_74.5sa.sav")
names(sts)[names(sts)=="@weight0"] <- "weight0" 

saveRDS(sts, "sts.RDS") # Saves data as RDS file (more efficient)

sts <- readRDS("sts.RDS") # Loads RDS data file and replaces sav loaded file

sts_eng <- sts %>% subset(xyear >= 2007 & gore < 10) # Subsets England data

## USEFUL LINKS FOR GOOGLESHEETS PACKAGE SUPPORT
# https://github.com/tidyverse/googlesheets4
# https://googlesheets4.tidyverse.org/articles/googlesheets4.html
# https://googlesheets4.tidyverse.org/reference/sheets_edit.html
# https://googlesheets4.tidyverse.org/articles/index.html
# https://www.tidyverse.org/blog/2020/05/googlesheets4-0-2-0/
# Auth code 4/1AY0e-g5apbdigUznt74ElXOLH92hEEXU0IADkhMhLH1k7Hg-RiBSzYvaVQc
# https://googledrive.tidyverse.org/
# https://www.rdocumentation.org/packages/googledrive/versions/0.1.3/topics/drive_mkdir

# gs4_auth(email = "smokingchartdata@gmail.com")

drive_auth() # Enter 1 for smokingchartdata@gmail.com

gs4_auth(token = drive_token()) # Only run this line once you have entered 1 when prompted after drive_auth()


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
df1 <- t(colPerc(xtabs(sts_eng$weight0 ~ sts_eng$cigsmok + sts_eng$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
Year <- c(2007:2022)
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
  slice(-c(17)) %>%
  mutate(sts_eng.cigsmok = NULL) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.cigsmok == "Sum") %>%
  slice(-c(17)) %>%
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
sheet_write(df7, "1flfqt38fGvUg4YdWoZ_YDVAAiwOzbVsri32HsfOhjrk", sheet = 'Sheet1')

##########################################
# Change in cigarette smoking prevalence #
##########################################

A <- c("", "Year", "")
B <- c("", "", "Change in prevalence")
C <- c("", "", "")
headings <- cbind(A, B)
colnames(headings) <- c("A", "B")

df2 <- round(df2, digits = 1)

changecig <- df2 %>% 
  mutate(Percent = Value - lag(Value)) %>%
  mutate(Value = NULL) %>%
  slice(-1)
changecig <- round(changecig, digits = 1)

Years <- c("2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013",
          "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021", "2021-2022")

changecig <- cbind(Years, changecig)
colnames(changecig) <- c("A", "B")

df3 <- rbind(headings, changecig)
colnames(df3) <- c()

# PUSH to google sheet
sheet_write(df3, "1T_J15VvmlAIEa2S6TIF0uMTdVKjo3tQ1HoHNOEq7HHg", sheet = 'Sheet1')

################################################
# CIGARETTE SMOKING PREVALENCE 18-21 YEAR OLDS #
################################################

A <- c("Graph shows prevalence estimate and upper and lower 95% confidence intervals", "Year", "")
B <- c("", "Percent", "Prevalence estimate")
C <- c("", "", "Lower 95% CI")
D <- c("", "", "Upper 95% CI")
headings <- cbind(A, B, C, D)

sts1821 <- sts_eng %>% subset(actage >= 18 & actage <= 21) 

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts1821$weight0 ~ sts1821$cigsmok + sts1821$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts1821$weight0 ~ sts1821$cigsmok + sts1821$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts1821.cigsmok != 0) %>%
  subset(sts1821.cigsmok != 1) %>%
  subset(sts1821.xyear != "Sum") %>%
  mutate(sts1821.xyear = NULL) %>%
  mutate(sts1821.cigsmok = NULL)

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts1821$weight0 ~ sts1821$xyear + sts1821$cigsmok)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts1821.cigsmok == 1) %>%
  slice(-c(17)) %>%
  mutate(sts1821.cigsmok = NULL) %>%
  mutate(sts1821.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts1821.cigsmok == "Sum") %>%
  slice(-c(17)) %>%
  mutate(sts1821.cigsmok = NULL) %>%
  mutate(sts1821.xyear = NULL)

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
sheet_write(df7, "1Gioc_e-Ww1AmPCjI9h-_rJGgpyTwY_ztZRA_FKZQeeI", sheet = 'Sheet1')


#####################################
# CIGARETTE SMOKING 16-17 YEAR OLDS # For 2021 add a row with 0 value, then add 2022 row?
#####################################

sts1617 <- sts_eng %>% subset(actage >= 16 & actage <= 17) 

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts1617$weight0 ~ sts1617$cigsmok + sts1617$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
Year <- c(2007:2021)
df3 <- cbind(Year, df2)

# Compute Total Ns for each year
n <- (t(round(addmargins(xtabs(sts1617$weight0 ~ sts1617$cigsmok + sts1617$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts1617.cigsmok != 0) %>%
  subset(sts1617.cigsmok != 1) %>%
  mutate(sts1617.xyear = NULL) %>%
  mutate(sts1617.cigsmok = NULL) %>%
  slice(-c(16))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts1617$weight0 ~ sts1617$xyear + sts1617$cigsmok)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts1617.cigsmok == 1) %>%
  slice(-c(16)) %>%
  mutate(sts1617.cigsmok = NULL) %>%
  mutate(sts1617.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts1617.cigsmok == "Sum") %>%
  slice(-c(16)) %>%
  mutate(sts1617.cigsmok = NULL) %>%
  mutate(sts1617.xyear = NULL)

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

# PUSH to google sheet
#sheet_write(df6, "1Gioc_e-Ww1AmPCjI9h-_rJGgpyTwY_ztZRA_FKZQeeI", sheet = 'Sheet1')

#####################################
# STOPPED SMOKING IN PAST 12 MONTHS #
#####################################

sts_smokly <- sts_eng %>% subset(smokly == 1) 

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_smokly$weight0 ~ sts_smokly$notsmo + sts_smokly$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
Year <- c(2007:2022)
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_smokly$weight0 ~ sts_smokly$notsmo + sts_smokly$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_smokly.notsmo != 0) %>%
  subset(sts_smokly.notsmo != 1) %>%
  mutate(sts_smokly.xyear = NULL) %>%
  mutate(sts_smokly.notsmo = NULL) %>%
  slice(-c(17))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_smokly$weight0 ~ sts_smokly$xyear + sts_smokly$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_smokly.notsmo == 1) %>%
  slice(-c(17)) %>%
  mutate(sts_smokly.notsmo = NULL) %>%
  mutate(sts_smokly.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_smokly.notsmo == "Sum") %>%
  slice(-c(17)) %>%
  mutate(sts_smokly.notsmo = NULL) %>%
  mutate(sts_smokly.xyear = NULL)

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
sheet_write(df7, "1H1_QVfafonFF7iHRi_n5ONWi0hdqqF6ccAXR_JTKD34", sheet = 'Sheet1')


######################################
# Tried to stop smoking in past year #
######################################

# Use sts_smokly
# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_smokly$weight0 ~ sts_smokly$trylyc + sts_smokly$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_smokly$weight0 ~ sts_smokly$trylyc + sts_smokly$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_smokly.trylyc != 0) %>%
  subset(sts_smokly.trylyc != 1) %>%
  mutate(sts_smokly.trylyc = NULL) %>%
  mutate(sts_smokly.xyear = NULL) %>%
  mutate(sts_smokly.trylyc = NULL) %>%
  slice(-c(17))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_smokly$weight0 ~ sts_smokly$xyear + sts_smokly$trylyc)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_smokly.trylyc == 1) %>%
  slice(-c(17)) %>%
  mutate(sts_smokly.trylyc = NULL) %>%
  mutate(sts_smokly.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_smokly$weight0 ~ sts_smokly$xyear + sts_smokly$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_smokly.trylyc != 1) %>%
  subset(sts_smokly.trylyc != 0) %>%
  slice(-c(17)) %>%
  mutate(sts_smokly.trylyc = NULL) %>%
  mutate(sts_smokly.xyear = NULL)

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
sheet_write(df7, "1OAEmOKFhAvnTg6dDGPo-5i78vLXoYPQ86bVmy9Y1Oyg", sheet = 'Sheet1')

###############################################
# Success rate in stopping in those who tried #
###############################################

sts_trylyc <- sts_eng %>% subset(smokly == 1 & trylyc == 1)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_trylyc$weight0 ~ sts_trylyc$notsmo + sts_trylyc$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_trylyc$weight0 ~ sts_trylyc$trylyc + sts_trylyc$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_trylyc.trylyc != 0) %>%
  subset(sts_trylyc.trylyc != 1) %>%
  mutate(sts_trylyc.trylyc = NULL) %>%
  mutate(sts_trylyc.xyear = NULL) %>%
  mutate(sts_trylyc.notsmo = NULL) %>%
  slice(-c(17))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_trylyc$weight0 ~ sts_trylyc$xyear + sts_trylyc$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_trylyc.notsmo == 1) %>%
  slice(-c(17)) %>%
  mutate(sts_trylyc.notsmo = NULL) %>%
  mutate(sts_trylyc.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_trylyc$weight0 ~ sts_trylyc$xyear + sts_trylyc$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_trylyc.trylyc == "Sum") %>%
  slice(-c(17)) %>%
  mutate(sts_trylyc.trylyc = NULL) %>%
  mutate(sts_trylyc.xyear = NULL)

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
sheet_write(df7, "1A2hJgDcIk7d3kNnIUDfeGg6tb3IfBTmJlLfOCrsGHyo", sheet = 'Sheet1')

##############################################
# Tried to stop smoking in past year (18-24) #
##############################################

sts_smokly1824 <- sts_smokly %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_smokly1824$weight0 ~ sts_smokly1824$trylyc + sts_smokly1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_smokly1824$weight0 ~ sts_smokly1824$trylyc + sts_smokly1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_smokly1824.trylyc != 0) %>%
  subset(sts_smokly1824.trylyc != 1) %>%
  mutate(sts_smokly1824.trylyc = NULL) %>%
  mutate(sts_smokly1824.xyear = NULL) %>%
  mutate(sts_smokly1824.trylyc = NULL) %>%
  slice(-c(17))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_smokly1824$weight0 ~ sts_smokly1824$xyear + sts_smokly1824$trylyc)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_smokly1824.trylyc == 1) %>%
  slice(-c(17)) %>%
  mutate(sts_smokly1824.trylyc = NULL) %>%
  mutate(sts_smokly1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_smokly1824$weight0 ~ sts_smokly1824$xyear + sts_smokly1824$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_smokly1824.trylyc != 1) %>%
  subset(sts_smokly1824.trylyc != 0) %>%
  slice(-c(17)) %>%
  mutate(sts_smokly1824.trylyc = NULL) %>%
  mutate(sts_smokly1824.xyear = NULL)

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
sheet_write(df7, "1XuFmRHslBMhuMmLKorEhrqQkjbbE0B6t4eYhB0CytUg", sheet = 'Sheet1')

###########################################################
# Success rate for stopping in those aged 18-24 who tried #
###########################################################

# Slightly different CIs to SPSS. Can't reconcile. Point estimate the same though.

sts_trylyc1824 <- sts_trylyc %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_trylyc1824$weight0 ~ sts_trylyc1824$notsmo + sts_trylyc1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_trylyc1824$weight0 ~ sts_trylyc1824$trylyc + sts_trylyc1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_trylyc1824.trylyc != 0) %>%
  subset(sts_trylyc1824.trylyc != 1) %>%
  mutate(sts_trylyc1824.trylyc = NULL) %>%
  mutate(sts_trylyc1824.xyear = NULL) %>%
  mutate(sts_trylyc1824.notsmo = NULL) %>%
  slice(-c(17))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_trylyc1824$weight0 ~ sts_trylyc1824$xyear + sts_trylyc1824$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_trylyc1824.notsmo == 1) %>%
  slice(-c(17)) %>%
  mutate(sts_trylyc1824.notsmo = NULL) %>%
  mutate(sts_trylyc1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_trylyc1824$weight0 ~ sts_trylyc1824$xyear + sts_trylyc1824$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_trylyc1824.trylyc == "Sum") %>%
  slice(-c(17)) %>%
  mutate(sts_trylyc1824.trylyc = NULL) %>%
  mutate(sts_trylyc1824.xyear = NULL)

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
sheet_write(df7, "1FJV26h8K_D0E-ReAQwxd61Zgh-nLiMQ8yZ60NvecNbw", sheet = 'Sheet1')

############################################
# Uptake: prevalence of ever smoking 18-24 #
############################################

sts_1824 <- sts_eng %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_1824$weight0 ~ sts_1824$eversmo + sts_1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_1824$weight0 ~ sts_1824$eversmo + sts_1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_1824.eversmo != 0) %>%
  subset(sts_1824.eversmo != 1) %>%
  mutate(sts_1824.eversmo = NULL) %>%
  mutate(sts_1824.xyear = NULL) %>%
  mutate(sts_1824.eversmo = NULL) %>%
  slice(-c(17))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_1824$weight0 ~ sts_1824$xyear + sts_1824$eversmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_1824.eversmo == 1) %>%
  slice(-c(17)) %>%
  mutate(sts_1824.eversmo = NULL) %>%
  mutate(sts_1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_1824$weight0 ~ sts_1824$xyear + sts_1824$eversmo)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_1824.eversmo != 1) %>%
  subset(sts_1824.eversmo != 0) %>%
  slice(-c(17)) %>%
  mutate(sts_1824.eversmo = NULL) %>%
  mutate(sts_1824.xyear = NULL)

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
sheet_write(df7, "1m967J-t9OJ7tsVq98Cs9eOUSNd4qwAn4px99OUAlavk", sheet = 'Sheet1')
