library(ggthemes)
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
library(tinytex)
library(scales)

#### SET WORKING DIRECTORY AS SCOTLAND SLIDES FOLDER
setwd("C:/Toolkit merge files/R Script/Slides/Scotland/Alcohol")
getwd()

# Read in data
# sts <- read_sav("C:/Toolkit merge files/Waves/186/omni186_39.1_65.2cot_31.3a_25.4s_recodes_72.5sa.sav")
#names(sts)[names(sts)=="@weight0"] <- "weight0" 
ats_scot <- sts %>% subset(gore == 11)

# Function for 3 month rolling average
avg.last.3 <- function (x) if (length(x) < 3) rep(NA, length(x)) else rollmeanr(x, 3, fill = NA)

#############
# IMPORTANT # - for date variable, increase by 1 each month
#############
series1 <- create_series('2020' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(12:30))
series2 <- create_series('2020' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(12:30))
series3 <- create_series('2020' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(12:30))
series4 <- create_series('2020' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(12:30))
series5 <- create_series('2020' ~ '2022', 'monthly', class = 'yearmon') %>%
  slice(c(12:30))

#############################################################
# Prevalence of increasing and higher risk drinking (AUDIT) #
#############################################################

# Create % dataframe
df1 <- t(colPerc(xtabs(ats_scot$weight_scotland ~ ats_scot$highriskaudit + ats_scot$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("All")
df2 <- cbind(sgz, df2)
colnames(df2) <- c("sgz", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

abc1_scot <- ats_scot %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$highriskaudit + abc1_scot$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("ABC1")
ab_df2 <- cbind(sgz, ab_df2)
colnames(ab_df2) <- c("sgz", "perc")

ab_df2 <- ab_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

c2de_scot <- ats_scot %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$highriskaudit + c2de_scot$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("C2DE")
cd_df2 <- cbind(sgz, cd_df2)
colnames(cd_df2) <- c("sgz", "perc")

cd_df2 <- cd_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(df2, ab_df2, cd_df2)

date <- rbind(series1, series2, series3)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = sgz), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","ABC1","C2DE"), values = sgz_colours) +
  ylab("% higher risk drinkers (3 month moving average)") + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  theme_clean(base_size = 17) + theme(legend.position = "bottom") +
  ylim(0, 30) +
  labs(title = paste0("Unweighted N=", nrow(ats_scot)))

ggsave("audit_scot.png")

#############################################################
# Prevalence of increasing and higher risk drinking (AUDIT-C) #
#############################################################

# Create % dataframe
df1 <- t(colPerc(xtabs(ats_scot$weight_scotland ~ ats_scot$highriskauditc + ats_scot$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("All")
df2 <- cbind(sgz, df2)
colnames(df2) <- c("sgz", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

abc1_scot <- ats_scot %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$highriskauditc + abc1_scot$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("ABC1")
ab_df2 <- cbind(sgz, ab_df2)
colnames(ab_df2) <- c("sgz", "perc")

ab_df2 <- ab_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

c2de_scot <- ats_scot %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$highriskauditc + c2de_scot$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("C2DE")
cd_df2 <- cbind(sgz, cd_df2)
colnames(cd_df2) <- c("sgz", "perc")

cd_df2 <- cd_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(df2, ab_df2, cd_df2)

date <- rbind(series1, series2, series3)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = sgz), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","ABC1","C2DE"), values = sgz_colours) +
  ylab("% higher risk drinkers (3 month moving average)") + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") + 
  ylim(0, 50) +
  theme_clean(base_size = 17) + theme(legend.position = "bottom")

ggsave("auditc_scot.png")


############################################
# Currently trying to restrict consumption #
############################################

auditc <- ats_scot %>% subset(highriskauditc == 1)

# Create % dataframe
df1 <- t(colPerc(xtabs(auditc$weight_scotland ~ auditc$alccutdown + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("All")
df2 <- cbind(sgz, df2)
colnames(df2) <- c("sgz", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

abc1_scot <- auditc %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$alccutdown + abc1_scot$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("ABC1")
ab_df2 <- cbind(sgz, ab_df2)
colnames(ab_df2) <- c("sgz", "perc")

ab_df2 <- ab_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

c2de_scot <- auditc %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$alccutdown + c2de_scot$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("C2DE")
cd_df2 <- cbind(sgz, cd_df2)
colnames(cd_df2) <- c("sgz", "perc")

cd_df2 <- cd_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(df2, ab_df2, cd_df2)

date <- rbind(series1, series2, series3)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = sgz), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","ABC1","C2DE"), values = sgz_colours) +
  ylab(expression(atop("% of higher risk drinkers currently trying to restrict", paste("consumption (3 month moving average)")))) + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  theme_clean(base_size = 17) + theme(legend.position = "bottom") +
  ylim(0,30) +
  labs(title = paste0("Unweighted N=", nrow(auditc)))

ggsave("restrict_cons_scot.png")

##########################
# Motivation to cut down #
##########################

# Create % dataframe
df1 <- t(colPerc(xtabs(auditc$weight_scotland ~ auditc$amotiv1to6 + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("All")
df2 <- cbind(sgz, df2)
colnames(df2) <- c("sgz", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

abc1_scot <- auditc %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$amotiv1to6 + abc1_scot$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("ABC1")
ab_df2 <- cbind(sgz, ab_df2)
colnames(ab_df2) <- c("sgz", "perc")

ab_df2 <- ab_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

c2de_scot <- auditc %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$amotiv1to6 + c2de_scot$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("C2DE")
cd_df2 <- cbind(sgz, cd_df2)
colnames(cd_df2) <- c("sgz", "perc")

cd_df2 <- cd_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(df2, ab_df2, cd_df2)

date <- rbind(series1, series2, series3)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = sgz), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","ABC1","C2DE"), values = sgz_colours) +
  ylab(expression(atop("% of higher risk drinkers  believing should or wanting", paste("to cut down (3 month moving average)")))) + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 40) +
  theme_clean(base_size = 16) + theme(legend.position = "bottom")

ggsave("motiv_scot.png")

#############
# GP advice #
#############

# Create % dataframe
df1 <- t(colPerc(xtabs(auditc$weight_scotland ~ auditc$alcgpadivce + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("All")
df2 <- cbind(sgz, df2)
colnames(df2) <- c("sgz", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

df3 <- cbind(series1, df2)

colour_gp <- c("deeppink")

df3$date <- as.Date(df3$date)

# Chart for slide
df3 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = sgz), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All"), values = colour_gp) +
  ylab(expression(atop("% of higher risk drinkers who visited GP in past 12 months", paste("receiving advice to cut down (3 month moving average)")))) + 
  xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 10) +
  theme_clean(base_size = 15) + theme(legend.position = "bottom")

ggsave("gp_scot.png")

##########################################
# Past-year attempts to cut down or stop #
##########################################

df1 <- t(colPerc(xtabs(auditc$weight_scotland ~ auditc$tryalclyc2 + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("All")
df2 <- cbind(sgz, df2)
colnames(df2) <- c("sgz", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

abc1_scot <- auditc %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$tryalclyc2 + abc1_scot$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("ABC1")
ab_df2 <- cbind(sgz, ab_df2)
colnames(ab_df2) <- c("sgz", "perc")

ab_df2 <- ab_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

c2de_scot <- auditc %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$tryalclyc2 + c2de_scot$xwave)))
cd_df2 <- cd_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("C2DE")
cd_df2 <- cbind(sgz, cd_df2)
colnames(cd_df2) <- c("sgz", "perc")

cd_df2 <- cd_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(df2, ab_df2, cd_df2)

date <- rbind(series1, series2, series3)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = sgz), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","ABC1","C2DE"), values = sgz_colours) +
  ylab(expression(atop("% of higher risk drinkers who made attempt to cut down in", paste("previous 12 months (3 month moving average)")))) + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 50) +
  theme_clean(base_size = 15) + theme(legend.position = "bottom")

ggsave("all_pyattempts_scot.png")

##################################################
# At least one SERIOUS attempt in past 12 months #
##################################################

df1 <- t(colPerc(xtabs(auditc$weight_scotland ~ auditc$tryalclyc + auditc$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("All")
df2 <- cbind(sgz, df2)
colnames(df2) <- c("sgz", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$tryalclyc + abc1_scot$xwave)))
ab_df2 <- ab_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("ABC1")
ab_df2 <- cbind(sgz, ab_df2)
colnames(ab_df2) <- c("sgz", "perc")

ab_df2 <- ab_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$tryalclyc + c2de_scot$xwave)))
cd_df2 <- cd_df3 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sgz <- c("C2DE")
cd_df2 <- cbind(sgz, cd_df2)
colnames(cd_df2) <- c("sgz", "perc")

cd_df2 <- cd_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sgz, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(df2, ab_df2, cd_df2)

date <- rbind(series1, series2, series3)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = sgz), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","ABC1","C2DE"), values = sgz_colours) +
  ylab(expression(atop("% of higher risk drinkers who made attempt to cut down in", paste("previous 12 months (3 month moving average)")))) + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 20) +
  theme_clean(base_size = 15) + theme(legend.position = "bottom")

ggsave("serious_pyattempts_scot.png")

#################################
# Support in past-year attempts #
#################################

tryalclyc2 <- ats_scot %>% subset(tryalclyc2 == 1)

# Create % dataframe
df1 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$anyalcsupport2 + tryalclyc2$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

categ <- c("All")
df2 <- cbind(categ, df2)
colnames(df2) <- c("All", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(categ, Avg.all) %>%
  slice(-c(1,2))

tryalclyc <- ats_scot %>% subset(tryalclyc == 1)

# Create % dataframe
ser_df5 <- t(colPerc(xtabs(tryalclyc$weight_scotland ~ tryalclyc$anyalcsupport + tryalclyc$xwave)))
ser_df6 <- ser_df5 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

categ <- c("Serious")
ser_df6 <- cbind(categ, ser_df6)
colnames(ser_df6) <- c("Serious", "perc")

ser_df6 <- ser_df6 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(categ, Avg.all) %>%
  slice(-c(1,2))

colnames(ser_df6) <- c("All", "Avg.all")

df3 <- rbind(df2, ser_df6)

date <- rbind(series1, series2)

df4 <- cbind(date, df3)

colour_supp <- c("deeppink", "midnightblue")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = All), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","Serious"), values = colour_supp) +
  ylab(expression(atop("% of higher risk drinkers making attempts to cut down in past", paste("12 months who used  support (3 month moving average)")))) + 
  xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 30) +
  theme_clean(base_size = 13) + theme(legend.position = "bottom")

ggsave("support_pyattempts_scot.png")

###################################
# Triggers for past-year attempts #
###################################

df_alcmot10 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot10 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Weight")
df_alcmot10 <- cbind(trig, df_alcmot10)
colnames(df_alcmot10) <- c("trig", "perc")

df_alcmot10 <- df_alcmot10 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot6 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot6 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Future health concern")
df_alcmot6 <- cbind(trig, df_alcmot6)
colnames(df_alcmot6) <- c("trig", "perc")

df_alcmot6 <- df_alcmot6 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot9 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot9 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Fitness")
df_alcmot9 <- cbind(trig, df_alcmot9)
colnames(df_alcmot9) <- c("trig", "perc")

df_alcmot9 <- df_alcmot9 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot5 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot5 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Current health problem")
df_alcmot5 <- cbind(trig, df_alcmot5)
colnames(df_alcmot5) <- c("trig", "perc")

df_alcmot5 <- df_alcmot5 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot7 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot7 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Comment by family")
df_alcmot7 <- cbind(trig, df_alcmot7)
colnames(df_alcmot7) <- c("trig", "perc")

df_alcmot7 <- df_alcmot7 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot1 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot1 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("GP advice")
df_alcmot1 <- cbind(trig, df_alcmot1)
colnames(df_alcmot1) <- c("trig", "perc")

df_alcmot1 <- df_alcmot1 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot3 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot3 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Too expensive")
df_alcmot3 <- cbind(trig, df_alcmot3)
colnames(df_alcmot3) <- c("trig", "perc")

df_alcmot3 <- df_alcmot3 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot11 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot11 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Detox")
df_alcmot11 <- cbind(trig, df_alcmot11)
colnames(df_alcmot11) <- c("trig", "perc")

df_alcmot11 <- df_alcmot11 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot2 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot2 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Govt advert")
df_alcmot2 <- cbind(trig, df_alcmot2)
colnames(df_alcmot2) <- c("trig", "perc")

df_alcmot2 <- df_alcmot2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot19 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot19 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Dry January")
df_alcmot19 <- cbind(trig, df_alcmot19)
colnames(df_alcmot19) <- c("trig", "perc")

df_alcmot19 <- df_alcmot19 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))

df_alcmot17 <- t(colPerc(xtabs(tryalclyc2$weight_scotland ~ tryalclyc2$alcmot17 + tryalclyc2$xwave))) %>%
  as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

trig <- c("Baby/pregnant")
df_alcmot17 <- cbind(trig, df_alcmot17)
colnames(df_alcmot17) <- c("trig", "perc")

df_alcmot17 <- df_alcmot17 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(trig, Avg.all) %>%
  slice(-c(1,2))


df3 <- rbind(df_alcmot10, df_alcmot6, df_alcmot9, df_alcmot5, df_alcmot7, df_alcmot1, df_alcmot3, df_alcmot11, df_alcmot2, df_alcmot19, df_alcmot17)

date <- rbind(series1, series2, series3, series4, series5, series1, series2, series3, series4, series5, series1)

df4 <- cbind(date, df3)

#install.packages("RColorBrewer")
library(RColorBrewer)

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = trig), size = 1.3) +
  scale_colour_brewer(name = "", labels = c("Baby/pregnancy", "Comment by family", "Current health problem", "Detox", "Dry January", "Weight loss", "Future health concern", 
                                            "Govt advert", "GP advice", "Too expensive", "Fitness"), palette = "Paired") +
  ylab(expression(atop("% higher risk drinkers who made serious attempt to cut down", paste("permanently in past 12 months (3 month moving average)")))) + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  theme_clean(base_size = 12.5) + theme(legend.position = "bottom", legend.text = element_text(size = 14))

ggsave("triggers_pyattempts_scot.png")
