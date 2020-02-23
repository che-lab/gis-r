library("tidyverse")
library("sf")
library("varhandle")


setwd("C:/Users/elmsc/Documents/gis/gis_r")

ls_2015 <- read.csv("output_all/csv/buffers/ls_2015.csv")
ls_2015 <- ls_2015 %>% rename("PROPERTY" = "Full_name")
ls_2015 <- ls_2015 %>% mutate(CAT = 1)

ls_2016 <- read.csv("output_all/csv/buffers/ls_2016.csv")
ls_2016 <- ls_2016 %>% mutate(CAT = 1)


ls_2017 <- read.csv("output_all/csv/buffers/ls_2017.csv")
ls_2017 <- ls_2017 %>% mutate(CAT = 1)

ls_2018 <- read.csv("output_all/csv/buffers/ls_2018.csv")
ls_2018 <- ls_2018 %>% mutate(CAT = 1)


ts_2015 <- read.csv("output_all/csv/buffers/ts_2015.csv")
ts_2015 <- ts_2015 %>% mutate(CAT = 2)


ts_2016 <- read.csv("output_all/csv/buffers/ts_2016.csv")
ts_2016 <- ts_2016 %>% mutate(CAT = 2)

ts_2017 <- read.csv("output_all/csv/buffers/ts_2017.csv")
ts_2017 <- ts_2017 %>% mutate(CAT = 2)

ts_2018 <- read.csv("output_all/csv/buffers/ts_2018.csv")
ts_2018 <- ts_2018 %>% mutate(CAT = 2)



mj_2015 <- read.csv("output_all/csv/buffers/mj_2015.csv")
mj_2015 <- mj_2015 %>% mutate(CAT = 3)

mj_2016 <- read.csv("output_all/csv/buffers/mj_2016.csv")
mj_2016 <- mj_2016 %>% mutate(CAT = 3)

mj_2017 <- read.csv("output_all/csv/buffers/mj_2017.csv")
mj_2017 <- mj_2017 %>% mutate(CAT = 3)

mj_2018 <- read.csv("output_all/csv/buffers/mj_2018.csv")
mj_2018 <- mj_2018 %>% mutate(CAT = 3)

final_df <- data.frame(YEAR=integer(),
                       PROPERTY=character(),
                       Full_name=character(),
                       PROPERTY_TYPE=character(),
                       CAT=integer(),
                       PC_100=integer(),
                       VC_100=integer(),
                       FPC_100=integer(),
                       FVC_100=integer(),
                       PC_200=integer(),
                       VC_200=integer(),
                       FPC_200=integer(),
                       FVC_200=integer(),
                       PC_300=integer(),
                       VC_300=integer(),
                       FPC_300=integer(),
                       FVC_300=integer(),
                       PC_500=integer(),
                       VC_500=integer(),
                       FPC_500=integer(),
                       FVC_500=integer(),
                       PC_1000=integer(),
                       VC_1000=integer(),
                       FPC_1000=integer(),
                       FVC_1000=integer(),
                       PC_1200=integer(),
                       VC_1200=integer(),
                       FPC_1200=integer(),
                       FVC_1200=integer(),
                       stringsAsFactors=FALSE)



only_these <- as.vector(c('PROPERTY', 'PROPERTY_TYPE', 'CAT',
                       'PC_100','VC_100','FPC_100','FVC_100',
                       'PC_200','VC_200','FPC_200','FVC_200',
                       'PC_300','VC_300','FPC_300','FVC_300',
                       'PC_500','VC_500','FPC_500','FVC_500',
                       'PC_1000','VC_1000','FPC_1000','FVC_1000',
                       'PC_1200','VC_1200','FPC_1200','FVC_1200'))





# _________ls_2015___________#
ls_2015 <- ls_2015[only_these]
ls_2015 <- ls_2015 %>% mutate(YEAR = 1)
ls_2015 <- ls_2015 %>%
  select(YEAR, everything())



# _________ts_2015___________#
ts_2015 <- ts_2015[only_these]
ts_2015 <- ts_2015 %>% mutate(YEAR = 1)
ts_2015 <- ts_2015 %>%
  select(YEAR, everything())



# _________mj_2015___________#
mj_2015 <- mj_2015[only_these]
mj_2015 <- mj_2015 %>% mutate(YEAR = 1)
mj_2015 <- mj_2015 %>%
  select(YEAR, everything())
























# _________ls_2016___________#
ls_2016 <- ls_2016[only_these]
ls_2016 <- ls_2016 %>% mutate(YEAR = 2)
ls_2016 <- ls_2016 %>%
  select(YEAR, everything())



# _________ts_2016___________#
ts_2016 <- ts_2016[only_these]
ts_2016 <- ts_2016 %>% mutate(YEAR = 2)
ts_2016 <- ts_2016 %>%
  select(YEAR, everything())



# _________mj_2016___________#
mj_2016 <- mj_2016[only_these]
mj_2016 <- mj_2016 %>% mutate(YEAR = 2)
mj_2016 <- mj_2016 %>%
  select(YEAR, everything())










# _________ls_2017___________#
ls_2017 <- ls_2017[only_these]
ls_2017 <- ls_2017 %>% mutate(YEAR = 3)
ls_2017 <- ls_2017 %>%
  select(YEAR, everything())



# _________ts_2017___________#
ts_2017 <- ts_2017[only_these]
ts_2017 <- ts_2017 %>% mutate(YEAR = 3)
ts_2017 <- ts_2017 %>%
  select(YEAR, everything())



# _________mj_2017___________#
mj_2017 <- mj_2017[only_these]
mj_2017 <- mj_2017 %>% mutate(YEAR = 3)
mj_2017 <- mj_2017 %>%
  select(YEAR, everything())







# _________ls_2018___________#
ls_2018 <- ls_2018[only_these]
ls_2018 <- ls_2018 %>% mutate(YEAR = 4)
ls_2018 <- ls_2018 %>%
  select(YEAR, everything())



# _________ts_2018___________#
ts_2018 <- ts_2018[only_these]
ts_2018 <- ts_2018 %>% mutate(YEAR = 4)
ts_2018 <- ts_2018 %>%
  select(YEAR, everything())



# _________mj_2018___________#
mj_2018 <- mj_2018[only_these]
mj_2018 <- mj_2018 %>% mutate(YEAR = 4)
mj_2018 <- mj_2018 %>%
  select(YEAR, everything())





final_df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ls_2015, ts_2015, mj_2015,
                                                              ls_2016, ts_2016, mj_2016,
                                                              ls_2017, ts_2017, mj_2017,
                                                              ls_2018, ts_2018, mj_2018))




write.csv(final_df,"output_all/csv/final_df.csv", row.names = FALSE)








