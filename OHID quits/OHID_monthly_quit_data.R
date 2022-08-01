library(haven)
library(tigerstats)

sts <- read_sav("C:/Toolkit merge files/Waves/186/omni186_39.1_65.2cot_31.3a_25.4s_recodes_72.5sa.sav")

names(sts)[names(sts)=="@weight0"] <- "weight0"

sts_eng <- sts %>% subset(gore < 10)
# sts_eng <- sts %>% subset(xwave > 168 & gore < 10)

smokly <- subset(sts_eng, smokly == 1)

x <- t(colPerc(xtabs(weight0 ~ trylmc + xwave, data = smokly)))

#write.csv(x, "quits.csv")

y <- round(addmargins(table(smokly$xwave, smokly$trylmc))) # Unweighted

#write.csv(y, "quits.csv")

z <- t(round(addmargins(table(sts_eng$smokly, sts_eng$xwave)))) # Unweighted

#write.csv(z, "quits.csv")

round(addmargins(table(sts_eng$xwave, sts_eng$smokstat))) # Unweighted



