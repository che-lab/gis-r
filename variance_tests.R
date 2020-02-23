library("tidyverse")
library("ggpubr")

setwd("C:/Users/elmsc/Documents/gis/gis_r")

sla <- read.csv("output_all/csv/final_df.csv")

sla <- sla %>% mutate(PROPERTY_TYPE = factor(PROPERTY_TYPE))

sla15 <- sla %>% subset(YEAR == 1)
sla18 <- sla %>% subset(YEAR == 4)
sla_comb <- rbind(sla15,sla18)

plot(VC_500 ~ PROPERTY_TYPE, data = sla_comb)
plot(VC_500 ~ PROPERTY_TYPE, data = sla18)

ggline(sla_comb, x = "PROPERTY_TYPE", y = "VC_500", 
       add = c("mean_se", "jitter"), 
       order = c("AS", "MJ", "TS"),
       ylab = "VIOLENT CRIME 500FT", xlab = "PROPERTY TYPE")

VC_AOV <- aov(VC_500~PROPERTY_TYPE, data=sla_comb)
summary(VC_AOV)

PC_AOV <- aov(PC_500~PROPERTY_TYPE, data=sla_comb)
summary(PC_AOV)

#do 500 and 1200 PC-VC
kruskal.test(VC_500~PROPERTY_TYPE, data=sla_comb)

pairwise.wilcox.test(sla_comb$VC_500, sla_comb$PROPERTY_TYPE,
                     p.adjust.method = "BH")
