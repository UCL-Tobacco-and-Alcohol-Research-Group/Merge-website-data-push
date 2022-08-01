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
setwd("C:/Toolkit merge files/R Script/Slides/Scotland/Monthly")
getwd()

# Read in data
# sts <- read_sav("C:/Toolkit merge files/Waves/186/omni186_39.1_65.2cot_31.3a_25.4s_recodes_72.5sa.sav")
#names(sts)[names(sts)=="@weight0"] <- "weight0" 
sts_scot <- sts %>% subset(gore == 11)

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

################################
# CIGARETTE SMOKING PREVALENCE #
################################

# Create smoking % dataframe
df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$cigsmok + sts_scot$xwave)))
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

abc1_scot <- sts_scot %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$cigsmok + abc1_scot$xwave)))
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

c2de_scot <- sts_scot %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$cigsmok + c2de_scot$xwave)))
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
  ylab("% cigarette smokers (3 month moving average)") + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 30) +
  theme_clean(base_size = 16) + theme(legend.position = "bottom")

ggsave("cigsmok_scot.png")

#######################################
# CIGARETTE SMOKING PREVALENCE BY AGE #
#######################################

df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$cigsmok + sts_scot$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

agez <- c("All")
df2 <- cbind(agez, df2)
colnames(df2) <- c("agez", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(agez, Avg.all) %>%
  slice(-c(1,2))

under35 <- sts_scot %>% subset(over35 == 0)

under35_df1 <- t(colPerc(xtabs(under35$weight_scotland ~ under35$cigsmok + under35$xwave)))
under35_df2 <- under35_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

agez <- c("Under 35")
under35_df2 <- cbind(agez, under35_df2)
colnames(under35_df2) <- c("agez", "perc")

under35_df2 <- under35_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(agez, Avg.all) %>%
  slice(-c(1,2))

over35 <- sts_scot %>% subset(over35 == 1)

over35_df1 <- t(colPerc(xtabs(over35$weight_scotland ~ over35$cigsmok + over35$xwave)))
over35_df2 <- over35_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

agez <- c("Over 35")
over35_df2 <- cbind(agez, over35_df2)
colnames(over35_df2) <- c("agez", "perc")

over35_df2 <- over35_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(agez, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(df2, under35_df2, over35_df2)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = agez), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","Under 35","Over 35"), values = sgz_colours) +
  ylab("% cigarette smokers (3 month moving average)") + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0,30) +
  theme_clean(base_size = 16) + theme(legend.position = "bottom")

ggsave("cigsmok_scot_age.png")


#######################################
# CIGARETTE SMOKING PREVALENCE BY SEX #
#######################################

df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$cigsmok + sts_scot$xwave)))
df2 <- df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sexz <- c("All")
df2 <- cbind(sexz, df2)
colnames(df2) <- c("sexz", "perc")

df2 <- df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sexz, Avg.all) %>%
  slice(-c(1,2))

stsmen <- sts_scot %>% subset(sexz == 1)

men_df1 <- t(colPerc(xtabs(stsmen$weight_scotland ~ stsmen$cigsmok + stsmen$xwave)))
men_df2 <- men_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sexz <- c("Men")
men_df2 <- cbind(sexz, men_df2)
colnames(men_df2) <- c("sexz", "perc")

men_df2 <- men_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sexz, Avg.all) %>%
  slice(-c(1,2))

stswomen <- sts_scot %>% subset(sexz == 2)

women_df1 <- t(colPerc(xtabs(stswomen$weight_scotland ~ stswomen$cigsmok + stswomen$xwave)))
women_df2 <- women_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

sexz <- c("Women")
women_df2 <- cbind(sexz, women_df2)
colnames(women_df2) <- c("sexz", "perc")

women_df2 <- women_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(sexz, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(df2, men_df2, women_df2)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = sexz), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("All","Men","Women"), values = sgz_colours) +
  ylab("% cigarette smokers (3 month moving average)") + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 20) +
  theme_clean(base_size = 16) + theme(legend.position = "bottom")

ggsave("cigsmok_scot_sex.png")


####################################
# ATTEMPTS TO STOP BY SOCIAL GRADE #
####################################

df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$trylyc + sts_scot$xwave)))
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

abc1_scot <- sts_scot %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$trylyc + abc1_scot$xwave)))
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

c2de_scot <- sts_scot %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$trylyc + c2de_scot$xwave)))
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
  ylab(expression(atop("% of those who were smokers in the past yr who", paste("tried to stop that yr (3 month moving average)")))) + 
  xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 60) +
  theme_clean(base_size = 16) + theme(legend.position = "bottom")

ggsave("quit_attempt_scot.png")

#################################
# SUPPORT USED IN QUIT ATTEMPTS #
#################################

ec_df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$ecig_hier + sts_scot$xwave)))
ec_df2 <- ec_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

support <- c("E-cig")
ec_df2 <- cbind(support, ec_df2)
colnames(ec_df2) <- c("support", "perc")

ec_df2 <- ec_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(support, Avg.all) %>%
  slice(-c(1,2))

nrt_df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$nrtotc_hier + sts_scot$xwave)))
nrt_df2 <- nrt_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

support <- c("NRT OTC")
nrt_df2 <- cbind(support, nrt_df2)
colnames(nrt_df2) <- c("support", "perc")

nrt_df2 <- nrt_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(support, Avg.all) %>%
  slice(-c(1,2))

medrx_df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$medrx_hier + sts_scot$xwave)))
medrx_df2 <- medrx_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

support <- c("MedRx")
medrx_df2 <- cbind(support, medrx_df2)
colnames(medrx_df2) <- c("support", "perc")

medrx_df2 <- medrx_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(support, Avg.all) %>%
  slice(-c(1,2))

nhs_df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$nhs_hier + sts_scot$xwave)))
nhs_df2 <- nhs_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

support <- c("NHS")
nhs_df2 <- cbind(support, nhs_df2)
colnames(nhs_df2) <- c("support", "perc")

nhs_df2 <- nhs_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(support, Avg.all) %>%
  slice(-c(1,2))

no_df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$nothing_hier + sts_scot$xwave)))
no_df2 <- no_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

support <- c("Nothing")
no_df2 <- cbind(support, no_df2)
colnames(no_df2) <- c("support", "perc")

no_df2 <- no_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(support, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(ec_df2, nrt_df2, medrx_df2, nhs_df2, no_df2)


date <- rbind(series1, series2, series3, series4, series5)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2", "gray70", "mediumpurple1")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = support), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("E-cig","NRT OTC","MedRx", "NHS", "Nothing"), values = sgz_colours) +
  ylab(expression(atop("% of those trying to stop in the past year", paste("who used support 3 month moving average)")))) + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  theme_clean(base_size = 16) + theme(legend.position = "bottom")

ggsave("support_scot.png")

#################################
# GP triggered quit attempts    #
#################################

gp_df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$gptrigatt2 + sts_scot$xwave)))
gp_df2 <- gp_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

support <- c("GP")
gp_df2 <- cbind(support, gp_df2)
colnames(gp_df2) <- c("support", "perc")

gp_df2 <- gp_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(support, Avg.all) %>%
  slice(-c(1,2))

date <- series1
df3 <- cbind(date, gp_df2)
df3$date <- as.Date(df3$date)

# Chart for slide
df3 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = support), size = 1.3) +
  scale_colour_manual(name = "", values = sgz_colours) +
  ylab("% cigarette smokers (3 month moving average)") + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") + 
  ylim(0,10) +
  theme_clean(base_size = 18) + theme(legend.position = "none")

ggsave("gp_scot.png")

######################
# Motivation to quit #
######################

df1 <- t(colPerc(xtabs(sts_scot$weight_scotland ~ sts_scot$qmotivwi + sts_scot$xwave)))
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

abc1_scot <- sts_scot %>% subset(randm == 0)

ab_df1 <- t(colPerc(xtabs(abc1_scot$weight_scotland ~ abc1_scot$qmotivwi + abc1_scot$xwave)))
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

c2de_scot <- sts_scot %>% subset(randm == 1)

cd_df1 <- t(colPerc(xtabs(c2de_scot$weight_scotland ~ c2de_scot$qmotivwi + c2de_scot$xwave)))
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
  ylab(expression(atop("% of cigarettes smokers wanting to stop and intending", paste("to stop soon (3 month moving average)")))) +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 50) +
  theme_clean(base_size = 15) + theme(legend.position = "bottom")

ggsave("motiv_scot.png")

##################
# Harm reduction #
##################

cigsmok <- sts_scot %>% subset(cigsmok == 1)

cutdown_df1 <- t(colPerc(xtabs(cigsmok$weight_scotland ~ cigsmok$cutdown + cigsmok$xwave)))
cutdown_df2 <- cutdown_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

harm_reduct <- c("Cutting down")
cutdown_df2 <- cbind(harm_reduct, cutdown_df2)
colnames(cutdown_df2) <- c("harm_reduct", "perc")

cutdown_df2 <- cutdown_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(harm_reduct, Avg.all) %>%
  slice(-c(1,2))

cdnrt_df1 <- t(colPerc(xtabs(cigsmok$weight_scotland ~ cigsmok$cdnrtc + cigsmok$xwave)))
cdnrt_df2 <- cdnrt_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

harm_reduct <- c("Using NRT to cut down")
cdnrt_df2 <- cbind(harm_reduct, cdnrt_df2)
colnames(cdnrt_df2) <- c("harm_reduct", "perc")

cdnrt_df2 <- cdnrt_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(harm_reduct, Avg.all) %>%
  slice(-c(1,2))

cdecig_df1 <- t(colPerc(xtabs(cigsmok$weight_scotland ~ cigsmok$cdecig + cigsmok$xwave)))
cdecig_df2 <- cdecig_df1 %>% as.data.frame() %>%
  mutate("0" = NULL) %>%
  mutate(Total = NULL)

harm_reduct <- c("Using e-cigs to cut down")
cdecig_df2 <- cbind(harm_reduct, cdecig_df2)
colnames(cdecig_df2) <- c("harm_reduct", "perc")

cdecig_df2 <- cdecig_df2 %>% mutate(Avg.all = avg.last.3(perc)) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  select(harm_reduct, Avg.all) %>%
  slice(-c(1,2))

df3 <- rbind(cutdown_df2, cdnrt_df2, cdecig_df2)

date <- rbind(series1, series2, series3)

df4 <- cbind(date, df3)

sgz_colours <- c("deeppink", "midnightblue", "dodgerblue2")

df4$date <- as.Date(df4$date)

# Chart for slide
df4 %>% ggplot(aes(date, Avg.all)) +
  geom_line(aes(color = harm_reduct), size = 1.3) +
  scale_colour_manual(name = "", breaks = c("Cutting down","Using NRT to cut down","Using e-cigs to cut down"), values = sgz_colours) +
  ylab("% cigarette smokers (3 month moving average)") + xlab("Month") +
  scale_x_date(labels = date_format("%m/%y"), breaks = "months") +
  ylim(0, 70) +
  theme_clean(base_size = 16) + theme(legend.position = "bottom")

ggsave("harm_reduction_scot.png")
