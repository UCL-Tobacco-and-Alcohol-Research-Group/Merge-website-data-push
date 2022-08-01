sts_gb <- sts %>% filter(xwave == 179)

# Factor policy support

attributes(sts_gb$qaap1lp21_12)

# MUP
sts_gb$qaap1lp21_01 <- factor(sts_gb$qaap1lp21_01, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_01) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

levels(sts_gb$qaap1lp21_01)

# Heath warnings
sts_gb$qaap1lp21_02 <- factor(sts_gb$qaap1lp21_02, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_02) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Sales 10am-10pm
sts_gb$qaap1lp21_03 <- factor(sts_gb$qaap1lp21_03, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_03) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Tax
sts_gb$qaap1lp21_04 <- factor(sts_gb$qaap1lp21_04, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_04) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Pubilc Health alcohol outlet applications
sts_gb$qaap1lp21_05 <- factor(sts_gb$qaap1lp21_05, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_05) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Children advertising 
sts_gb$qaap1lp21_06 <- factor(sts_gb$qaap1lp21_06, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_06) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Independent regulator promotion
sts_gb$qaap1lp21_07 <- factor(sts_gb$qaap1lp21_07, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_07) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Driving
sts_gb$qaap1lp21_08 <- factor(sts_gb$qaap1lp21_08, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_08) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Training advice from health professional
sts_gb$qaap1lp21_09 <- factor(sts_gb$qaap1lp21_09, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_09) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Accesss support
sts_gb$qaap1lp21_10 <- factor(sts_gb$qaap1lp21_10, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_10) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Industry disclose
sts_gb$qaap1lp21_11 <- factor(sts_gb$qaap1lp21_11, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_11) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Alcohol calorie label
sts_gb$qaap1lp21_12 <- factor(sts_gb$qaap1lp21_12, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_12) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

# Visibility in shops
sts_gb$qaap1lp21_13 <- factor(sts_gb$qaap1lp21_13, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c("Strongly support", "Tend to support", "No opinion either way", "Tend to oppose", "Strongly oppose", "Unsure/Don't know")) 
levels(sts_gb$qaap1lp21_13) <- list(Support = c("Strongly support", "Tend to support"), Oppose = c("Tend to oppose", "Strongly oppose"), No_opinion_unsure = c("No opinion either way", "Unsure/Don't know")) 

Response <- c("Support", "Oppose", "No opinion/unsure")

# MUP
t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_01)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_mup <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_mup) <- c("est", "LCI", "UCI")
CI_mup <- round(CI_mup, digits = 1)
CI_mup <- subset(CI_mup, select = -c(LCI, UCI))
colnames(CI_mup) <- "MUP"

# Health warnings

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_02)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_hw <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_hw) <- c("est", "LCI", "UCI")

CI_hw <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_hw) <- c("est", "LCI", "UCI")
CI_hw <- round(CI_hw, digits = 1)
CI_hw <- subset(CI_hw, select = -c(LCI, UCI))
colnames(CI_hw) <- "Health warnings"

# Sale 10am-10pm

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_03)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_10 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_10) <- c("est", "LCI", "UCI")

CI_10 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_10) <- c("est", "LCI", "UCI")
CI_10 <- round(CI_10, digits = 1)
CI_10 <- subset(CI_10, select = -c(LCI, UCI))
colnames(CI_10) <- "10am-10pm"

# Tax

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_04)))
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

# Public health alcohol application

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_05)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ph <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ph) <- c("est", "LCI", "UCI")

CI_ph <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ph) <- c("est", "LCI", "UCI")
CI_ph <- round(CI_ph, digits = 1)
CI_ph <- subset(CI_ph, select = -c(LCI, UCI))
colnames(CI_ph) <- "Public Health considered in retail application"

# Children advertising

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_06)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_child <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_child) <- c("est", "LCI", "UCI")

CI_child <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_child) <- c("est", "LCI", "UCI")
CI_child <- round(CI_child, digits = 1)
CI_child <- subset(CI_child, select = -c(LCI, UCI))
colnames(CI_child) <- "Limit children exposure to advertising"

# Independent regulation alcohol promotion

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_07)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_reg <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_reg) <- c("est", "LCI", "UCI")

CI_reg <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_reg) <- c("est", "LCI", "UCI")
CI_reg <- round(CI_reg, digits = 1)
CI_reg <- subset(CI_reg, select = -c(LCI, UCI))
colnames(CI_reg) <- "Independent regulator promotion"

# Legal limit driving

t1 <- round(addmargins(xtabs(sts_gb_gb$weight_gb ~ sts_gb_gb$xyear + sts_gb_gb$qaap1lp21_08)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_drive <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_drive) <- c("est", "LCI", "UCI")

CI_drive <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_drive) <- c("est", "LCI", "UCI")
CI_drive <- round(CI_drive, digits = 1)
CI_drive <- subset(CI_drive, select = -c(LCI, UCI))
colnames(CI_drive) <- "0mg/100ml limit drink driving"

# Train professionals give advice

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_09)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_train <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_train) <- c("est", "LCI", "UCI")

CI_train <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_train) <- c("est", "LCI", "UCI")
CI_train <- round(CI_train, digits = 1)
CI_train <- subset(CI_train, select = -c(LCI, UCI))
colnames(CI_train) <- "Train  healthcare professionals"

# Access to support

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_10)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_support <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_support) <- c("est", "LCI", "UCI")

CI_support <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_support) <- c("est", "LCI", "UCI")
CI_support <- round(CI_support, digits = 1)
CI_support <- subset(CI_support, select = -c(LCI, UCI))
colnames(CI_support) <- "Access to support"

# Industry disclose

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_11)))
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

# Alcohol calorie labelling

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_cal <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_cal) <- c("est", "LCI", "UCI")

CI_cal <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_cal) <- c("est", "LCI", "UCI")
CI_cal <- round(CI_cal, digits = 1)
CI_cal <- subset(CI_cal, select = -c(LCI, UCI))
colnames(CI_cal) <- "Alcohol calorie label"

# Density of retailers

t1 <- round(addmargins(xtabs(sts_gb$weight_gb ~ sts_gb$xyear + sts_gb$qaap1lp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_gb.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_gb.xyear = NULL)

x2 <- c(2013)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_vis <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_vis) <- c("est", "LCI", "UCI")

CI_vis <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_vis) <- c("est", "LCI", "UCI")
CI_vis <- round(CI_vis, digits = 1)
CI_vis <- subset(CI_vis, select = -c(LCI, UCI))
colnames(CI_vis) <- "Reduce visibility in supermarkets"



df <- cbind(Response, CI_mup, CI_hw, CI_10, CI_tax, CI_ph, CI_child, CI_reg, CI_drive, CI_train, CI_support, CI_disclose, CI_cal, CI_vis)

colnames(df) <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertise", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                  "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in supermarkets")
colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")

df_pivot <- as.data.frame(t(df))

rownames(df_pivot) <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertise", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                        "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in supermarkets")

policy_name <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertising", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                 "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in shops/supermarkets")

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

df2 <- rbind(row2, row3, df_pivot_2)
colnames(df2) <- c()

# PUSH to google sheet
sheet_write(df2, "1584ICYVJeIfQe8UiOOYlrK-U-r5AAjSnza_YdFqH7zA", sheet = 'Sheet1')

############
# SCOTLAND #

sts_scot <- sts_gb %>% subset(gore == 11)

# MUP
t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_01)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_mup <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_mup) <- c("est", "LCI", "UCI")
CI_mup <- round(CI_mup, digits = 1)
CI_mup <- subset(CI_mup, select = -c(LCI, UCI))
colnames(CI_mup) <- "MUP"

# Health warnings

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_02)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_hw <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_hw) <- c("est", "LCI", "UCI")

CI_hw <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_hw) <- c("est", "LCI", "UCI")
CI_hw <- round(CI_hw, digits = 1)
CI_hw <- subset(CI_hw, select = -c(LCI, UCI))
colnames(CI_hw) <- "Health warnings"

# Sale 10am-10pm

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_03)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_10 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_10) <- c("est", "LCI", "UCI")

CI_10 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_10) <- c("est", "LCI", "UCI")
CI_10 <- round(CI_10, digits = 1)
CI_10 <- subset(CI_10, select = -c(LCI, UCI))
colnames(CI_10) <- "10am-10pm"

# Tax

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_04)))
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

# Public health alcohol application

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_05)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ph <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ph) <- c("est", "LCI", "UCI")

CI_ph <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ph) <- c("est", "LCI", "UCI")
CI_ph <- round(CI_ph, digits = 1)
CI_ph <- subset(CI_ph, select = -c(LCI, UCI))
colnames(CI_ph) <- "Public Health considered in retail application"

# Children advertising

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_06)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_child <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_child) <- c("est", "LCI", "UCI")

CI_child <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_child) <- c("est", "LCI", "UCI")
CI_child <- round(CI_child, digits = 1)
CI_child <- subset(CI_child, select = -c(LCI, UCI))
colnames(CI_child) <- "Limit children exposure to advertising"

# Independent regulation alcohol promotion

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_07)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_reg <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_reg) <- c("est", "LCI", "UCI")

CI_reg <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_reg) <- c("est", "LCI", "UCI")
CI_reg <- round(CI_reg, digits = 1)
CI_reg <- subset(CI_reg, select = -c(LCI, UCI))
colnames(CI_reg) <- "Independent regulator promotion"

# Legal limit driving

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_08)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_drive <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_drive) <- c("est", "LCI", "UCI")

CI_drive <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_drive) <- c("est", "LCI", "UCI")
CI_drive <- round(CI_drive, digits = 1)
CI_drive <- subset(CI_drive, select = -c(LCI, UCI))
colnames(CI_drive) <- "0mg/100ml limit drink driving"

# Train professionals give advice

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_09)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_train <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_train) <- c("est", "LCI", "UCI")

CI_train <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_train) <- c("est", "LCI", "UCI")
CI_train <- round(CI_train, digits = 1)
CI_train <- subset(CI_train, select = -c(LCI, UCI))
colnames(CI_train) <- "Train  healthcare professionals"

# Access to support

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_10)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_support <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_support) <- c("est", "LCI", "UCI")

CI_support <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_support) <- c("est", "LCI", "UCI")
CI_support <- round(CI_support, digits = 1)
CI_support <- subset(CI_support, select = -c(LCI, UCI))
colnames(CI_support) <- "Access to support"

# Industry disclose

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_11)))
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

# Alcohol calorie labelling

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_cal <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_cal) <- c("est", "LCI", "UCI")

CI_cal <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_cal) <- c("est", "LCI", "UCI")
CI_cal <- round(CI_cal, digits = 1)
CI_cal <- subset(CI_cal, select = -c(LCI, UCI))
colnames(CI_cal) <- "Alcohol calorie label"

# Density of retailers

t1 <- (addmargins(xtabs(sts_scot$weight_scotland ~ sts_scot$xyear + sts_scot$qaap1lp21_13)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_scot.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_scot.xyear = NULL)

x2 <- c(248.96043)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_vis <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_vis) <- c("est", "LCI", "UCI")

CI_vis <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_vis) <- c("est", "LCI", "UCI")
CI_vis <- round(CI_vis, digits = 1)
CI_vis <- subset(CI_vis, select = -c(LCI, UCI))
colnames(CI_vis) <- "Reduce visibility in supermarkets"



df <- cbind(Response, CI_mup, CI_hw, CI_10, CI_tax, CI_ph, CI_child, CI_reg, CI_drive, CI_train, CI_support, CI_disclose, CI_cal, CI_vis)

colnames(df) <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertise", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                  "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in supermarkets")
colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")

df_pivot <- as.data.frame(t(df))

rownames(df_pivot) <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertise", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                        "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in supermarkets")

policy_name <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertising", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                 "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in shops/supermarkets")

df_pivot_2 <- cbind(policy_name, df_pivot)

colnames(df_pivot_2) <- c("A", "B", "C", "D")

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
sheet_write(df2, "1FYGq_iXVa9bTbNVJ-rm-KwxkuE7WRP4UozL3IirVqpY", sheet = 'Sheet1')


############
# WALES #

sts_wal <- sts_gb %>% subset(gore == 10)

# MUP
t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_01)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_mup <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_mup) <- c("est", "LCI", "UCI")
CI_mup <- round(CI_mup, digits = 1)
CI_mup <- subset(CI_mup, select = -c(LCI, UCI))
colnames(CI_mup) <- "MUP"

# Health warnings

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_02)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_hw <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_hw) <- c("est", "LCI", "UCI")

CI_hw <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_hw) <- c("est", "LCI", "UCI")
CI_hw <- round(CI_hw, digits = 1)
CI_hw <- subset(CI_hw, select = -c(LCI, UCI))
colnames(CI_hw) <- "Health warnings"

# Sale 10am-10pm

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_03)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_10 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_10) <- c("est", "LCI", "UCI")

CI_10 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_10) <- c("est", "LCI", "UCI")
CI_10 <- round(CI_10, digits = 1)
CI_10 <- subset(CI_10, select = -c(LCI, UCI))
colnames(CI_10) <- "10am-10pm"

# Tax

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_04)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")

CI_tax <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_tax) <- c("est", "LCI", "UCI")
CI_tax <- round(CI_tax, digits = 1)
CI_tax <- subset(CI_tax, select = -c(LCI, UCI))
colnames(CI_tax) <- "Tax"

# Public health alcohol application

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_05)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ph <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ph) <- c("est", "LCI", "UCI")

CI_ph <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ph) <- c("est", "LCI", "UCI")
CI_ph <- round(CI_ph, digits = 1)
CI_ph <- subset(CI_ph, select = -c(LCI, UCI))
colnames(CI_ph) <- "Public Health considered in retail application"

# Children advertising

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_06)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_child <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_child) <- c("est", "LCI", "UCI")

CI_child <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_child) <- c("est", "LCI", "UCI")
CI_child <- round(CI_child, digits = 1)
CI_child <- subset(CI_child, select = -c(LCI, UCI))
colnames(CI_child) <- "Limit children exposure to advertising"

# Independent regulation alcohol promotion

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_07)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_reg <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_reg) <- c("est", "LCI", "UCI")

CI_reg <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_reg) <- c("est", "LCI", "UCI")
CI_reg <- round(CI_reg, digits = 1)
CI_reg <- subset(CI_reg, select = -c(LCI, UCI))
colnames(CI_reg) <- "Independent regulator promotion"

# Legal limit driving

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_08)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_drive <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_drive) <- c("est", "LCI", "UCI")

CI_drive <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_drive) <- c("est", "LCI", "UCI")
CI_drive <- round(CI_drive, digits = 1)
CI_drive <- subset(CI_drive, select = -c(LCI, UCI))
colnames(CI_drive) <- "0mg/100ml limit drink driving"

# Train professionals give advice

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_09)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_train <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_train) <- c("est", "LCI", "UCI")

CI_train <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_train) <- c("est", "LCI", "UCI")
CI_train <- round(CI_train, digits = 1)
CI_train <- subset(CI_train, select = -c(LCI, UCI))
colnames(CI_train) <- "Train  healthcare professionals"

# Access to support

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_10)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_support <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_support) <- c("est", "LCI", "UCI")

CI_support <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_support) <- c("est", "LCI", "UCI")
CI_support <- round(CI_support, digits = 1)
CI_support <- subset(CI_support, select = -c(LCI, UCI))
colnames(CI_support) <- "Access to support"

# Industry disclose

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_11)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")

CI_disclose <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_disclose) <- c("est", "LCI", "UCI")
CI_disclose <- round(CI_disclose, digits = 1)
CI_disclose <- subset(CI_disclose, select = -c(LCI, UCI))
colnames(CI_disclose) <- "Industry disclose"

# Alcohol calorie labelling

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_cal <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_cal) <- c("est", "LCI", "UCI")

CI_cal <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_cal) <- c("est", "LCI", "UCI")
CI_cal <- round(CI_cal, digits = 1)
CI_cal <- subset(CI_cal, select = -c(LCI, UCI))
colnames(CI_cal) <- "Alcohol calorie label"

# Visibility

t1 <- (addmargins(xtabs(sts_wal$weight_wales ~ sts_wal$xyear + sts_wal$qaap1lp21_13)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_wal.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_wal.xyear = NULL)

x2 <- c(116.614964)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_vis <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_vis) <- c("est", "LCI", "UCI")

CI_vis <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_vis) <- c("est", "LCI", "UCI")
CI_vis <- round(CI_vis, digits = 1)
CI_vis <- subset(CI_vis, select = -c(LCI, UCI))
colnames(CI_vis) <- "Reduce visibility in supermarkets"



df <- cbind(Response, CI_mup, CI_hw, CI_10, CI_tax, CI_ph, CI_child, CI_reg, CI_drive, CI_train, CI_support, CI_disclose, CI_cal, CI_vis)

colnames(df) <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertise", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                  "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in supermarkets")
colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")

df_pivot <- as.data.frame(t(df))

rownames(df_pivot) <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertise", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                        "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in supermarkets")

policy_name <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertising", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                 "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in shops/supermarkets")

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
sheet_write(df2, "1Ps0b7IkDVArMf-wGCbAnlsEpbdso66Kx5U-AmhARtTk", sheet = 'Sheet1')



############
# ENGLAND #

sts_eng <- sts_gb %>% subset(gore < 10)

# MUP
t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_01)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_mup <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_mup) <- c("est", "LCI", "UCI")
CI_mup <- round(CI_mup, digits = 1)
CI_mup <- subset(CI_mup, select = -c(LCI, UCI))
colnames(CI_mup) <- "MUP"

# Health warnings

t1 <- round(addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_02)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_hw <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_hw) <- c("est", "LCI", "UCI")

CI_hw <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_hw) <- c("est", "LCI", "UCI")
CI_hw <- round(CI_hw, digits = 1)
CI_hw <- subset(CI_hw, select = -c(LCI, UCI))
colnames(CI_hw) <- "Health warnings"

# Sale 10am-10pm

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_03)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_10 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_10) <- c("est", "LCI", "UCI")

CI_10 <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_10) <- c("est", "LCI", "UCI")
CI_10 <- round(CI_10, digits = 1)
CI_10 <- subset(CI_10, select = -c(LCI, UCI))
colnames(CI_10) <- "10am-10pm"

# Tax

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_04)))
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

# Public health alcohol application

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_05)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_ph <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ph) <- c("est", "LCI", "UCI")

CI_ph <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_ph) <- c("est", "LCI", "UCI")
CI_ph <- round(CI_ph, digits = 1)
CI_ph <- subset(CI_ph, select = -c(LCI, UCI))
colnames(CI_ph) <- "Public Health considered in retail application"

# Children advertising

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_06)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_child <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_child) <- c("est", "LCI", "UCI")

CI_child <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_child) <- c("est", "LCI", "UCI")
CI_child <- round(CI_child, digits = 1)
CI_child <- subset(CI_child, select = -c(LCI, UCI))
colnames(CI_child) <- "Limit children exposure to advertising"

# Independent regulation alcohol promotion

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_07)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_reg <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_reg) <- c("est", "LCI", "UCI")

CI_reg <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_reg) <- c("est", "LCI", "UCI")
CI_reg <- round(CI_reg, digits = 1)
CI_reg <- subset(CI_reg, select = -c(LCI, UCI))
colnames(CI_reg) <- "Independent regulator promotion"

# Legal limit driving

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_08)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_drive <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_drive) <- c("est", "LCI", "UCI")

CI_drive <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_drive) <- c("est", "LCI", "UCI")
CI_drive <- round(CI_drive, digits = 1)
CI_drive <- subset(CI_drive, select = -c(LCI, UCI))
colnames(CI_drive) <- "0mg/100ml limit drink driving"

# Train professionals give advice

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_09)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_train <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_train) <- c("est", "LCI", "UCI")

CI_train <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_train) <- c("est", "LCI", "UCI")
CI_train <- round(CI_train, digits = 1)
CI_train <- subset(CI_train, select = -c(LCI, UCI))
colnames(CI_train) <- "Train  healthcare professionals"

# Access to support

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_10)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_support <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_support) <- c("est", "LCI", "UCI")

CI_support <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_support) <- c("est", "LCI", "UCI")
CI_support <- round(CI_support, digits = 1)
CI_support <- subset(CI_support, select = -c(LCI, UCI))
colnames(CI_support) <- "Access to support"

# Industry disclose

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_11)))
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

# Alcohol calorie labelling

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_12)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_cal <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_cal) <- c("est", "LCI", "UCI")

CI_cal <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_cal) <- c("est", "LCI", "UCI")
CI_cal <- round(CI_cal, digits = 1)
CI_cal <- subset(CI_cal, select = -c(LCI, UCI))
colnames(CI_cal) <- "Alcohol calorie label"

# Density of retailers

t1 <- (addmargins(xtabs(sts_eng$weight0 ~ sts_eng$xyear + sts_eng$qaap1lp21_13)))
x1 <- t1 %>%
  as.data.frame() %>%
  subset(sts_eng.xyear == "2021") %>%
  slice(-c(4)) %>%
  mutate(sts_eng.xyear = NULL)

x2 <- c(1639)

x3 <- cbind(x1, x2)
colnames(x3) <- c("Response", "N", "TotalN")

CI_vis <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_vis) <- c("est", "LCI", "UCI")

CI_vis <- as.data.frame(BinomCI(x3$N, x3$TotalN, conf.level = 0.95)*100)
colnames(CI_vis) <- c("est", "LCI", "UCI")
CI_vis <- round(CI_vis, digits = 1)
CI_vis <- subset(CI_vis, select = -c(LCI, UCI))
colnames(CI_vis) <- "Reduce visibility in supermarkets"


df <- cbind(Response, CI_mup, CI_hw, CI_10, CI_tax, CI_ph, CI_child, CI_reg, CI_drive, CI_train, CI_support, CI_disclose, CI_cal, CI_vis)

colnames(df) <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertise", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                  "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in supermarkets")
colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")

df_pivot <- as.data.frame(t(df))

rownames(df_pivot) <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertise", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                        "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in supermarkets")

policy_name <- c("", "MUP", "Health warning labels", "Restrict sales 10am-10pm", "Tax", "Public health considered in retail applications", "Limit children exposure to advertising", "Independent regulator promotion", "Zero tolerance drink driving", "Train healthcare professionals",
                 "Access to support for alcohol problems", "Indsutry disclose", "Alcohol calorie labelling", "Reduce visibility in shops/supermarkets")

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
sheet_write(df2, "1584ICYVJeIfQe8UiOOYlrK-U-r5AAjSnza_YdFqH7zA", sheet = 'Sheet1')


