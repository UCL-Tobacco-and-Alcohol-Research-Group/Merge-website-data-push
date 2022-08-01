library(googlesheets4)
library(googledrive)
library(haven)
library(tigerstats)
library(dplyr)
library(tidyr)
library(DescTools)
library(ggthemes)

#sts <- read_sav("C:/Toolkit merge files/Waves/185/omni185_39.1_65.2cot_31.3a_25.4s_recodes_71.5sa.sav")
#names(sts)[names(sts)=="@weight0"] <- "weight0" 
sts_scot <- sts %>% subset(gore == 11)

#drive_auth()
#gs4_auth(token = drive_token())

#### SET WORKING DIRECTORY AS SCOTLAND SLIDES FOLDER
setwd("C:/Toolkit merge files/R Script/Slides/Scotland/Top line")
getwd()

################################
# CIGARETTE SMOKING PREVALENCE #
################################

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$cigsmok + sts_scot$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$cigsmok + sts_scot$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_scot.cigsmok != 0) %>%
  subset(sts_scot.cigsmok != 1) %>%
  subset(sts_scot.xyear != "Sum") %>%
  mutate(sts_scot.xyear = NULL) %>%
  mutate(sts_scot.cigsmok = NULL)

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$cigsmok)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.cigsmok == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_scot.cigsmok = NULL) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.cigsmok == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.cigsmok = NULL) %>%
  mutate(sts_scot.xyear = NULL)

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
colnames(df6) <- c("Year", "Prev", "LCI", "UCI")


df6_long <- gather(df6, intervals, estimate, Prev:UCI, factor_key=TRUE)

top_colours <- c("deeppink2", "hotpink", "hotpink")

df6_long %>% ggplot(aes(Year, estimate, group = intervals)) + 
  geom_line(aes(color = intervals), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Prev","LCI","UCI"), values = top_colours) +
  ylab("Percent") + ylim(0,20) +
  theme_clean(base_size = 18) + theme(legend.position = "none")
  
ggsave("cigsmok_scot.png")


################################################
# CIGARETTE SMOKING PREVALENCE 18-21 YEAR OLDS #
################################################

sts_scot1821 <- sts_scot %>% subset(actage >= 18 & actage <= 21) 

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot1821$weight_scotland ~ sts_scot1821$cigsmok + sts_scot1821$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_scot1821$weight_scotland ~ sts_scot1821$cigsmok + sts_scot1821$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_scot1821.cigsmok != 0) %>%
  subset(sts_scot1821.cigsmok != 1) %>%
  subset(sts_scot1821.xyear != "Sum") %>%
  mutate(sts_scot1821.xyear = NULL) %>%
  mutate(sts_scot1821.cigsmok = NULL)

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_scot1821$weight_scotland ~ sts_scot1821$xyear + sts_scot1821$cigsmok)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot1821.cigsmok == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_scot1821.cigsmok = NULL) %>%
  mutate(sts_scot1821.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot1821.cigsmok == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_scot1821.cigsmok = NULL) %>%
  mutate(sts_scot1821.xyear = NULL)

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
colnames(df6) <- c("Year", "Prev", "LCI", "UCI")

df6_long <- gather(df6, intervals, estimate, Prev:UCI, factor_key=TRUE)

top_colours <- c("deeppink2", "hotpink", "hotpink")

df6_long %>% ggplot(aes(Year, estimate, group = intervals)) + 
  geom_line(aes(color = intervals), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Prev","LCI","UCI"), values = top_colours) +
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  ylab("Percent") + ylim(0, 40) +
  theme_clean(base_size = 18) + theme(legend.position = "none")

ggsave("cigsmok1821_scot.png")

#####################################
# STOPPED SMOKING IN PAST 12 MONTHS #
#####################################

sts_scot_smokly <- sts_scot %>% subset(smokly == 1) 

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot_smokly$weight_scotland ~ sts_scot_smokly$notsmo + sts_scot_smokly$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_scot_smokly$weight_scotland ~ sts_scot_smokly$notsmo + sts_scot_smokly$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_scot_smokly.notsmo != 0) %>%
  subset(sts_scot_smokly.notsmo != 1) %>%
  mutate(sts_scot_smokly.xyear = NULL) %>%
  mutate(sts_scot_smokly.notsmo = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_scot_smokly$weight_scotland ~ sts_scot_smokly$xyear + sts_scot_smokly$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot_smokly.notsmo == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_smokly.notsmo = NULL) %>%
  mutate(sts_scot_smokly.xyear = NULL)

x2 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot_smokly.notsmo == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_scot_smokly.notsmo = NULL) %>%
  mutate(sts_scot_smokly.xyear = NULL)

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
colnames(df6) <- c("Year", "Prev", "LCI", "UCI")

df6_long <- gather(df6, intervals, estimate, Prev:UCI, factor_key=TRUE)

top_colours <- c("deeppink2", "hotpink", "hotpink")

df6_long %>% ggplot(aes(Year, estimate, group = intervals)) + 
  geom_line(aes(color = intervals), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Prev","LCI","UCI"), values = top_colours) +
  ylab("Percent") + ylim(0,30) +
  theme_clean(base_size = 18) + theme(legend.position = "none")

ggsave("stopped_scot.png")

######################################
# Tried to stop smoking in past year #
######################################

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot_smokly$weight_scotland ~ sts_scot_smokly$trylyc + sts_scot_smokly$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_scot_smokly$weight_scotland ~ sts_scot_smokly$trylyc + sts_scot_smokly$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_scot_smokly.trylyc != 0) %>%
  subset(sts_scot_smokly.trylyc != 1) %>%
  mutate(sts_scot_smokly.trylyc = NULL) %>%
  mutate(sts_scot_smokly.xyear = NULL) %>%
  mutate(sts_scot_smokly.trylyc = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_scot_smokly$weight_scotland ~ sts_scot_smokly$xyear + sts_scot_smokly$trylyc)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot_smokly.trylyc == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_smokly.trylyc = NULL) %>%
  mutate(sts_scot_smokly.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_scot_smokly$weight_scotland ~ sts_scot_smokly$xyear + sts_scot_smokly$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_scot_smokly.trylyc != 1) %>%
  subset(sts_scot_smokly.trylyc != 0) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_smokly.trylyc = NULL) %>%
  mutate(sts_scot_smokly.xyear = NULL)

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
colnames(df6) <- c("Year", "Prev", "LCI", "UCI")

df6_long <- gather(df6, intervals, estimate, Prev:UCI, factor_key=TRUE)

top_colours <- c("deeppink2", "hotpink", "hotpink")

df6_long %>% ggplot(aes(Year, estimate, group = intervals)) + 
  geom_line(aes(color = intervals), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Prev","LCI","UCI"), values = top_colours) +
  ylab("Percent") + ylim(0,50) +
  theme_clean(base_size = 18) + theme(legend.position = "none")

ggsave("triedstop_scot.png")

###############################################
# Success rate in stopping in those who tried #
###############################################

sts_scot_trylyc <- sts_scot %>% subset(smokly == 1 & trylyc == 1)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot_trylyc$weight_scotland ~ sts_scot_trylyc$notsmo + sts_scot_trylyc$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_scot_trylyc$weight_scotland ~ sts_scot_trylyc$trylyc + sts_scot_trylyc$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_scot_trylyc.trylyc != 0) %>%
  subset(sts_scot_trylyc.trylyc != 1) %>%
  mutate(sts_scot_trylyc.trylyc = NULL) %>%
  mutate(sts_scot_trylyc.xyear = NULL) %>%
  mutate(sts_scot_trylyc.notsmo = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_scot_trylyc$weight_scotland ~ sts_scot_trylyc$xyear + sts_scot_trylyc$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot_trylyc.notsmo == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_trylyc.notsmo = NULL) %>%
  mutate(sts_scot_trylyc.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_scot_trylyc$weight_scotland ~ sts_scot_trylyc$xyear + sts_scot_trylyc$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_scot_trylyc.trylyc == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_scot_trylyc.trylyc = NULL) %>%
  mutate(sts_scot_trylyc.xyear = NULL)

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
colnames(df6) <- c("Year", "Prev", "LCI", "UCI")

df6_long <- gather(df6, intervals, estimate, Prev:UCI, factor_key=TRUE)

top_colours <- c("deeppink2", "hotpink", "hotpink")

df6_long %>% ggplot(aes(Year, estimate, group = intervals)) + 
  geom_line(aes(color = intervals), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Prev","LCI","UCI"), values = top_colours) +
  ylab("Percent") + ylim(0,50) +
  theme_clean(base_size = 18) + theme(legend.position = "none")

ggsave("successstop_scot.png")

##############################################
# Tried to stop smoking in past year (18-24) #
##############################################

sts_scot_smokly1824 <- sts_scot_smokly %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot_smokly1824$weight_scotland ~ sts_scot_smokly1824$trylyc + sts_scot_smokly1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_scot_smokly1824$weight_scotland ~ sts_scot_smokly1824$trylyc + sts_scot_smokly1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_scot_smokly1824.trylyc != 0) %>%
  subset(sts_scot_smokly1824.trylyc != 1) %>%
  mutate(sts_scot_smokly1824.trylyc = NULL) %>%
  mutate(sts_scot_smokly1824.xyear = NULL) %>%
  mutate(sts_scot_smokly1824.trylyc = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_scot_smokly1824$weight_scotland ~ sts_scot_smokly1824$xyear + sts_scot_smokly1824$trylyc)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot_smokly1824.trylyc == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_smokly1824.trylyc = NULL) %>%
  mutate(sts_scot_smokly1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_scot_smokly1824$weight_scotland ~ sts_scot_smokly1824$xyear + sts_scot_smokly1824$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_scot_smokly1824.trylyc != 1) %>%
  subset(sts_scot_smokly1824.trylyc != 0) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_smokly1824.trylyc = NULL) %>%
  mutate(sts_scot_smokly1824.xyear = NULL)

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
colnames(df6) <- c("Year", "Prev", "LCI", "UCI")

df6_long <- gather(df6, intervals, estimate, Prev:UCI, factor_key=TRUE)

top_colours <- c("deeppink2", "hotpink", "hotpink")

df6_long %>% ggplot(aes(Year, estimate, group = intervals)) + 
  geom_line(aes(color = intervals), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Prev","LCI","UCI"), values = top_colours) +
  ylab("Percent") + ylim(0,80) +
  theme_clean(base_size = 18) + theme(legend.position = "none")

ggsave("triedstop1824_scot.png")

################################################
# Success rate for stopping in 18-24 who tried #
################################################

sts_scot_trylyc1824 <- sts_scot_trylyc %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot_trylyc1824$weight_scotland ~ sts_scot_trylyc1824$notsmo + sts_scot_trylyc1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute Ns for each year
n <- (t(round(addmargins(xtabs(sts_scot_trylyc1824$weight_scotland ~ sts_scot_trylyc1824$trylyc + sts_scot_trylyc1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_scot_trylyc1824.trylyc != 0) %>%
  subset(sts_scot_trylyc1824.trylyc != 1) %>%
  mutate(sts_scot_trylyc1824.trylyc = NULL) %>%
  mutate(sts_scot_trylyc1824.xyear = NULL) %>%
  mutate(sts_scot_trylyc1824.notsmo = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_scot_trylyc1824$weight_scotland ~ sts_scot_trylyc1824$xyear + sts_scot_trylyc1824$notsmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot_trylyc1824.notsmo == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_trylyc1824.notsmo = NULL) %>%
  mutate(sts_scot_trylyc1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_scot_trylyc1824$weight_scotland ~ sts_scot_trylyc1824$xyear + sts_scot_trylyc1824$trylyc)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_scot_trylyc1824.trylyc == "Sum") %>%
  slice(-c(4)) %>%
  mutate(sts_scot_trylyc1824.trylyc = NULL) %>%
  mutate(sts_scot_trylyc1824.xyear = NULL)

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
colnames(df6) <- c("Year", "Prev", "LCI", "UCI")

df6_long <- gather(df6, intervals, estimate, Prev:UCI, factor_key=TRUE)

top_colours <- c("deeppink2", "hotpink", "hotpink")

df6_long %>% ggplot(aes(Year, estimate, group = intervals)) + 
  geom_line(aes(color = intervals), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Prev","LCI","UCI"), values = top_colours) +
  ylab("Percent") + ylim(0,80) +
  theme_clean(base_size = 18) + theme(legend.position = "none")

ggsave("successstop1824_scot.png")

############################################
# Uptake: prevalence of ever smoking 18-24 #
############################################

sts_scot_1824 <- sts_scot %>% subset(actage >= 18 & actage <= 24)

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot_1824$weight_scotland ~ sts_scot_1824$eversmo + sts_scot_1824$xyear)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

# Create year variable
colnames(df2) <- c("Value")
df3 <- cbind(Year, df2)

# Compute total Ns for each year
n <- (t(round(addmargins(xtabs(sts_scot_1824$weight_scotland ~ sts_scot_1824$eversmo + sts_scot_1824$xyear)))))
n2 <- n %>% as.data.frame() %>%
  subset(sts_scot_1824.eversmo != 0) %>%
  subset(sts_scot_1824.eversmo != 1) %>%
  mutate(sts_scot_1824.eversmo = NULL) %>%
  mutate(sts_scot_1824.xyear = NULL) %>%
  mutate(sts_scot_1824.eversmo = NULL) %>%
  slice(-c(4))

# Ns for 95% CIs 
t1 <- round(addmargins(xtabs(sts_scot_1824$weight_scotland ~ sts_scot_1824$xyear + sts_scot_1824$eversmo)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot_1824.eversmo == 1) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_1824.eversmo = NULL) %>%
  mutate(sts_scot_1824.xyear = NULL)

t2 <- round(addmargins(xtabs(sts_scot_1824$weight_scotland ~ sts_scot_1824$xyear + sts_scot_1824$eversmo)))
x2 <- t2 %>%
  as.data.frame() %>%
  subset(sts_scot_1824.eversmo != 1) %>%
  subset(sts_scot_1824.eversmo != 0) %>%
  slice(-c(4)) %>%
  mutate(sts_scot_1824.eversmo = NULL) %>%
  mutate(sts_scot_1824.xyear = NULL)

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
colnames(df6) <- c("Year", "Prev", "LCI", "UCI")

df6_long <- gather(df6, intervals, estimate, Prev:UCI, factor_key=TRUE)

top_colours <- c("deeppink2", "hotpink", "hotpink")

df6_long %>% ggplot(aes(Year, estimate, group = intervals)) + 
  geom_line(aes(color = intervals), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Prev","LCI","UCI"), values = top_colours) +
  ylab("Percent") + ylim(0, 50) +
  theme_clean(base_size = 18) + theme(legend.position = "none")

ggsave("uptake1824_scot.png")
