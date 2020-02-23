library("tidyverse")
library("sf")
library("varhandle")


setwd("C:/Users/elmsc/Documents/gis/gis_r")

crime <- read.csv("output_all/csv/crime_2016.csv")



#------------------------LS_2016_100----------------------#

crime <- crime %>% st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
crime <- crime %>% mutate(COUNT = 1) %>% 
  mutate(PC_100 = ifelse(CRIME_TYPE == "PC", 1,0)) %>% 
  mutate(VC_100 = ifelse(CRIME_TYPE == "VC", 1,0)) %>% 
  mutate(FPC_100 = ifelse(CRIME_TYPE == "PC" & Felony == 1, 1,0)) %>% 
  mutate(FVC_100 = ifelse(CRIME_TYPE == "VC" & Felony == 1, 1,0))




LS_2016_100 <- st_read("data/shp/buffers/LS_2016_100.shp")
LS_2016_100 <- st_set_crs(LS_2016_100,4326)

crime <- st_join(crime, left = FALSE, LS_2016_100)


###### ALCOHOL ONLY ###########

DBA_Name <- data.frame(LS_2016_100$DBA_Name)
DBA_Name <- DBA_Name %>% drop_na()

STATE <- data.frame(LS_2016_100$Full_name)
STATE <- STATE %>% slice(88:107)

names(DBA_Name) <- "DBA_Name"
names(STATE)<- "DBA_Name"

DBA_Name <- rbind(DBA_Name,STATE)
rm(STATE)

LS_2016_100 <- LS_2016_100 %>% select(-c(DBA_Name))

LS_2016_100 <- LS_2016_100 %>%  mutate(DBA_Name = DBA_Name$DBA_Name)

rm(DBA_Name)
####################################

# Get Property Name
LS_2016_100 <- rename(LS_2016_100, PROPERTY = "DBA_Name")


# Get Property Type
LS_2016_100 <- LS_2016_100 %>% mutate(PROPERTY_TYPE = "AS")



# Get Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$PC_100,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("PC_100" = "x")

LS_2016_100 <- left_join(LS_2016_100, buffer_count, by = "ORIG_FID")
LS_2016_100$PC_100 <- LS_2016_100$PC_100 %>% replace_na(0)


# Get Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$VC_100,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("VC_100" = "x")

LS_2016_100 <- left_join(LS_2016_100, buffer_count, by = "ORIG_FID")
LS_2016_100$VC_100 <- LS_2016_100$VC_100 %>% replace_na(0)


# Get Felony Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FPC_100,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FPC_100" = "x")

LS_2016_100 <- left_join(LS_2016_100, buffer_count, by = "ORIG_FID")
LS_2016_100$FPC_100 <- LS_2016_100$FPC_100 %>% replace_na(0)


# Get Felony Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FVC_100,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FVC_100" = "x")

LS_2016_100 <- left_join(LS_2016_100, buffer_count, by = "ORIG_FID")
LS_2016_100$FVC_100 <- LS_2016_100$FVC_100 %>% replace_na(0)

write.csv(LS_2016_100,"test.csv")


#------------------------LS_2016_200----------------------#

crime <- read.csv("output_all/csv/crime_2016.csv")

crime <- crime %>% st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
crime <- crime %>% mutate(COUNT = 1) %>% 
  mutate(PC_200 = ifelse(CRIME_TYPE == "PC", 1,0)) %>% 
  mutate(VC_200 = ifelse(CRIME_TYPE == "VC", 1,0)) %>% 
  mutate(FPC_200 = ifelse(CRIME_TYPE == "PC" & Felony == 1, 1,0)) %>% 
  mutate(FVC_200 = ifelse(CRIME_TYPE == "VC" & Felony == 1, 1,0))
  
  
  
  
LS_2016_200 <- st_read("data/shp/buffers/LS_2016_200.shp")
LS_2016_200 <- st_set_crs(LS_2016_200,4326)

###### ALCOHOL ONLY ###########

DBA_Name <- data.frame(LS_2016_200$DBA_Name)
DBA_Name <- DBA_Name %>% drop_na()

STATE <- data.frame(LS_2016_200$Full_name)
STATE <- STATE %>% slice(88:107)

names(DBA_Name) <- "DBA_Name"
names(STATE)<- "DBA_Name"

DBA_Name <- rbind(DBA_Name,STATE)
rm(STATE)

LS_2016_200 <- LS_2016_200 %>% select(-c(DBA_Name))

LS_2016_200 <- LS_2016_200 %>%  mutate(DBA_Name = DBA_Name$DBA_Name)

rm(DBA_Name)
####################################

# Get Property Name
LS_2016_200 <- rename(LS_2016_200, PROPERTY = "DBA_Name")


# Get Property Type
LS_2016_200 <- LS_2016_200 %>% mutate(PROPERTY_TYPE = "AS")

crime <- st_join(crime, left = FALSE, LS_2016_200)



# Get Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$PC_200,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("PC_200" = "x")

LS_2016_200 <- left_join(LS_2016_200, buffer_count, by = "ORIG_FID")
LS_2016_200$PC_200 <- LS_2016_200$PC_200 %>% replace_na(0)


# Get Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$VC_200,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("VC_200" = "x")

LS_2016_200 <- left_join(LS_2016_200, buffer_count, by = "ORIG_FID")
LS_2016_200$VC_200 <- LS_2016_200$VC_200 %>% replace_na(0)


# Get Felony Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FPC_200,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FPC_200" = "x")

LS_2016_200 <- left_join(LS_2016_200, buffer_count, by = "ORIG_FID")
LS_2016_200$FPC_200 <- LS_2016_200$FPC_200 %>% replace_na(0)


# Get Felony Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FVC_200,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FVC_200" = "x")

LS_2016_200 <- left_join(LS_2016_200, buffer_count, by = "ORIG_FID")
LS_2016_200$FVC_200 <- LS_2016_200$FVC_200 %>% replace_na(0)




#------------------------LS_2016_300----------------------#

crime <- read.csv("output_all/csv/crime_2016.csv")

crime <- crime %>% st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
crime <- crime %>% mutate(COUNT = 1) %>% 
  mutate(PC_300 = ifelse(CRIME_TYPE == "PC", 1,0)) %>% 
  mutate(VC_300 = ifelse(CRIME_TYPE == "VC", 1,0)) %>% 
  mutate(FPC_300 = ifelse(CRIME_TYPE == "PC" & Felony == 1, 1,0)) %>% 
  mutate(FVC_300 = ifelse(CRIME_TYPE == "VC" & Felony == 1, 1,0))




LS_2016_300 <- st_read("data/shp/buffers/AS_2016_300.shp")
LS_2016_300 <- st_set_crs(LS_2016_300,4326)

###### ALCOHOL ONLY ###########

DBA_Name <- data.frame(LS_2016_300$DBA_Name)
DBA_Name <- DBA_Name %>% drop_na()

STATE <- data.frame(LS_2016_300$Full_name)
STATE <- STATE %>% slice(88:107)

names(DBA_Name) <- "DBA_Name"
names(STATE)<- "DBA_Name"

DBA_Name <- rbind(DBA_Name,STATE)
rm(STATE)

LS_2016_300 <- LS_2016_300 %>% select(-c(DBA_Name))

LS_2016_300 <- LS_2016_300 %>%  mutate(DBA_Name = DBA_Name$DBA_Name)

rm(DBA_Name)
####################################

# Get Property Name
LS_2016_300 <- rename(LS_2016_300, PROPERTY = "DBA_Name")


# Get Property Type
LS_2016_300 <- LS_2016_300 %>% mutate(PROPERTY_TYPE = "AS")

crime <- st_join(crime, left = FALSE, LS_2016_300)



# Get Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$PC_300,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("PC_300" = "x")

LS_2016_300 <- left_join(LS_2016_300, buffer_count, by = "ORIG_FID")
LS_2016_300$PC_300 <- LS_2016_300$PC_300 %>% replace_na(0)


# Get Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$VC_300,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("VC_300" = "x")

LS_2016_300 <- left_join(LS_2016_300, buffer_count, by = "ORIG_FID")
LS_2016_300$VC_300 <- LS_2016_300$VC_300 %>% replace_na(0)


# Get Felony Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FPC_300,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FPC_300" = "x")

LS_2016_300 <- left_join(LS_2016_300, buffer_count, by = "ORIG_FID")
LS_2016_300$FPC_300 <- LS_2016_300$FPC_300 %>% replace_na(0)


# Get Felony Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FVC_300,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FVC_300" = "x")

LS_2016_300 <- left_join(LS_2016_300, buffer_count, by = "ORIG_FID")
LS_2016_300$FVC_300 <- LS_2016_300$FVC_300 %>% replace_na(0)





#------------------------LS_2016_500----------------------#

crime <- read.csv("output_all/csv/crime_2016.csv")

crime <- crime %>% st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
crime <- crime %>% mutate(COUNT = 1) %>% 
  mutate(PC_500 = ifelse(CRIME_TYPE == "PC", 1,0)) %>% 
  mutate(VC_500 = ifelse(CRIME_TYPE == "VC", 1,0)) %>% 
  mutate(FPC_500 = ifelse(CRIME_TYPE == "PC" & Felony == 1, 1,0)) %>% 
  mutate(FVC_500 = ifelse(CRIME_TYPE == "VC" & Felony == 1, 1,0))




LS_2016_500 <- st_read("data/shp/buffers/AS_2016_500.shp")
LS_2016_500 <- st_set_crs(LS_2016_500,4326)

###### ALCOHOL ONLY ###########

DBA_Name <- data.frame(LS_2016_500$DBA_Name)
DBA_Name <- DBA_Name %>% drop_na()

STATE <- data.frame(LS_2016_500$Full_name)
STATE <- STATE %>% slice(88:107)

names(DBA_Name) <- "DBA_Name"
names(STATE)<- "DBA_Name"

DBA_Name <- rbind(DBA_Name,STATE)
rm(STATE)

LS_2016_500 <- LS_2016_500 %>% select(-c(DBA_Name))

LS_2016_500 <- LS_2016_500 %>%  mutate(DBA_Name = DBA_Name$DBA_Name)

rm(DBA_Name)
####################################

# Get Property Name
LS_2016_500 <- rename(LS_2016_500, PROPERTY = "DBA_Name")


# Get Property Type
LS_2016_500 <- LS_2016_500 %>% mutate(PROPERTY_TYPE = "AS")

crime <- st_join(crime, left = FALSE, LS_2016_500)



# Get Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$PC_500,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("PC_500" = "x")

LS_2016_500 <- left_join(LS_2016_500, buffer_count, by = "ORIG_FID")
LS_2016_500$PC_500 <- LS_2016_500$PC_500 %>% replace_na(0)


# Get Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$VC_500,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("VC_500" = "x")

LS_2016_500 <- left_join(LS_2016_500, buffer_count, by = "ORIG_FID")
LS_2016_500$VC_500 <- LS_2016_500$VC_500 %>% replace_na(0)


# Get Felony Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FPC_500,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FPC_500" = "x")

LS_2016_500 <- left_join(LS_2016_500, buffer_count, by = "ORIG_FID")
LS_2016_500$FPC_500 <- LS_2016_500$FPC_500 %>% replace_na(0)


# Get Felony Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FVC_500,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FVC_500" = "x")

LS_2016_500 <- left_join(LS_2016_500, buffer_count, by = "ORIG_FID")
LS_2016_500$FVC_500 <- LS_2016_500$FVC_500 %>% replace_na(0)



#------------------------LS_2016_1000----------------------#

crime <- read.csv("output_all/csv/crime_2016.csv")

crime <- crime %>% st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
crime <- crime %>% mutate(COUNT = 1) %>% 
  mutate(PC_1000 = ifelse(CRIME_TYPE == "PC", 1,0)) %>% 
  mutate(VC_1000 = ifelse(CRIME_TYPE == "VC", 1,0)) %>% 
  mutate(FPC_1000 = ifelse(CRIME_TYPE == "PC" & Felony == 1, 1,0)) %>% 
  mutate(FVC_1000 = ifelse(CRIME_TYPE == "VC" & Felony == 1, 1,0))




LS_2016_1000 <- st_read("data/shp/buffers/AS_2016_1000.shp")
LS_2016_1000 <- st_set_crs(LS_2016_1000,4326)

###### ALCOHOL ONLY ###########

DBA_Name <- data.frame(LS_2016_1000$DBA_Name)
DBA_Name <- DBA_Name %>% drop_na()

STATE <- data.frame(LS_2016_1000$Full_name)
STATE <- STATE %>% slice(88:107)

names(DBA_Name) <- "DBA_Name"
names(STATE)<- "DBA_Name"

DBA_Name <- rbind(DBA_Name,STATE)
rm(STATE)

LS_2016_1000 <- LS_2016_1000 %>% select(-c(DBA_Name))

LS_2016_1000 <- LS_2016_1000 %>%  mutate(DBA_Name = DBA_Name$DBA_Name)

rm(DBA_Name)
####################################


# Get Property Name
LS_2016_1000 <- rename(LS_2016_1000, PROPERTY = "DBA_Name")


# Get Property Type
LS_2016_1000 <- LS_2016_1000 %>% mutate(PROPERTY_TYPE = "AS")
crime <- st_join(crime, left = FALSE, LS_2016_1000)




# Get Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$PC_1000,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("PC_1000" = "x")

LS_2016_1000 <- left_join(LS_2016_1000, buffer_count, by = "ORIG_FID")
LS_2016_1000$PC_1000 <- LS_2016_1000$PC_1000 %>% replace_na(0)


# Get Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$VC_1000,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("VC_1000" = "x")

LS_2016_1000 <- left_join(LS_2016_1000, buffer_count, by = "ORIG_FID")
LS_2016_1000$VC_1000 <- LS_2016_1000$VC_1000 %>% replace_na(0)


# Get Felony Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FPC_1000,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FPC_1000" = "x")

LS_2016_1000 <- left_join(LS_2016_1000, buffer_count, by = "ORIG_FID")
LS_2016_1000$FPC_1000 <- LS_2016_1000$FPC_1000 %>% replace_na(0)


# Get Felony Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FVC_1000,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FVC_1000" = "x")

LS_2016_1000 <- left_join(LS_2016_1000, buffer_count, by = "ORIG_FID")
LS_2016_1000$FVC_1000 <- LS_2016_1000$FVC_1000 %>% replace_na(0)


#------------------------LS_2016_1200----------------------#

crime <- read.csv("output_all/csv/crime_2016.csv")

crime <- crime %>% st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
crime <- crime %>% mutate(COUNT = 1) %>% 
  mutate(PC_1200 = ifelse(CRIME_TYPE == "PC", 1,0)) %>% 
  mutate(VC_1200 = ifelse(CRIME_TYPE == "VC", 1,0)) %>% 
  mutate(FPC_1200 = ifelse(CRIME_TYPE == "PC" & Felony == 1, 1,0)) %>% 
  mutate(FVC_1200 = ifelse(CRIME_TYPE == "VC" & Felony == 1, 1,0))




LS_2016_1200 <- st_read("data/shp/buffers/AS_2016_1200.shp")
LS_2016_1200 <- st_set_crs(LS_2016_1200,4326)

###### ALCOHOL ONLY ###########

DBA_Name <- data.frame(LS_2016_1200$DBA_Name)
DBA_Name <- DBA_Name %>% drop_na()

STATE <- data.frame(LS_2016_1200$Full_name)
STATE <- STATE %>% slice(88:107)

names(DBA_Name) <- "DBA_Name"
names(STATE)<- "DBA_Name"

DBA_Name <- rbind(DBA_Name,STATE)
rm(STATE)

LS_2016_1200 <- LS_2016_1200 %>% select(-c(DBA_Name))

LS_2016_1200 <- LS_2016_1200 %>%  mutate(DBA_Name = DBA_Name$DBA_Name)

rm(DBA_Name)
####################################

# Get Property Name
LS_2016_1200 <- rename(LS_2016_1200, PROPERTY = "DBA_Name")


# Get Property Type
LS_2016_1200 <- LS_2016_1200 %>% mutate(PROPERTY_TYPE = "AS")

crime <- st_join(crime, left = FALSE, LS_2016_1200)



# Get Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$PC_1200,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("PC_1200" = "x")

LS_2016_1200 <- left_join(LS_2016_1200, buffer_count, by = "ORIG_FID")
LS_2016_1200$PC_1200 <- LS_2016_1200$PC_1200 %>% replace_na(0)


# Get Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$VC_1200,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("VC_1200" = "x")

LS_2016_1200 <- left_join(LS_2016_1200, buffer_count, by = "ORIG_FID")
LS_2016_1200$VC_1200 <- LS_2016_1200$VC_1200 %>% replace_na(0)


# Get Felony Property Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FPC_1200,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FPC_1200" = "x")

LS_2016_1200 <- left_join(LS_2016_1200, buffer_count, by = "ORIG_FID")
LS_2016_1200$FPC_1200 <- LS_2016_1200$FPC_1200 %>% replace_na(0)


# Get Felony Violent Crime Counts within buffer zone around each property
buffer_count <- aggregate(crime$FVC_1200,list(ORIG_FID = crime$ORIG_FID), sum) %>% 
  rename("FVC_1200" = "x")

LS_2016_1200 <- left_join(LS_2016_1200, buffer_count, by = "ORIG_FID")
LS_2016_1200$FVC_1200 <- LS_2016_1200$FVC_1200 %>% replace_na(0)



LS_2016_100 <- LS_2016_100 %>% 
  mutate(PC_200 = LS_2016_200$PC_200) %>% 
  mutate(VC_200 = LS_2016_200$VC_200) %>% 
  mutate(FPC_200 = LS_2016_200$FPC_200) %>% 
  mutate(FVC_200 = LS_2016_200$FVC_200)

LS_2016_100 <- LS_2016_100 %>% 
  mutate(PC_300 = LS_2016_300$PC_300) %>% 
  mutate(VC_300 = LS_2016_300$VC_300) %>% 
  mutate(FPC_300 = LS_2016_300$FPC_300) %>% 
  mutate(FVC_300 = LS_2016_300$FVC_300)

LS_2016_100 <- LS_2016_100 %>% 
  mutate(PC_500 = LS_2016_500$PC_500) %>% 
  mutate(VC_500 = LS_2016_500$VC_500) %>% 
  mutate(FPC_500 = LS_2016_500$FPC_500) %>% 
  mutate(FVC_500 = LS_2016_500$FVC_500)

LS_2016_100 <- LS_2016_100 %>% 
  mutate(PC_1000 = LS_2016_1000$PC_1000) %>% 
  mutate(VC_1000 = LS_2016_1000$VC_1000) %>% 
  mutate(FPC_1000 = LS_2016_1000$FPC_1000) %>% 
  mutate(FVC_1000 = LS_2016_1000$FVC_1000)

LS_2016_100 <- LS_2016_100 %>% 
  mutate(PC_1200 = LS_2016_1200$PC_1200) %>% 
  mutate(VC_1200 = LS_2016_1200$VC_1200) %>% 
  mutate(FPC_1200 = LS_2016_1200$FPC_1200) %>% 
  mutate(FVC_1200 = LS_2016_1200$FVC_1200)


# COMMENT A LOT 
# TODO: delete FC_COUNT and make FPC_100 and FVC_100 ------ redo entire output_all to new folder




st_write(LS_2016_100, dsn = "output_all/csv/buffers/ls_2016.csv", layer_options = "GEOMETRY=LS_XY", delete_dsn=TRUE)
st_write(LS_2016_100, dsn = "output_all/shp/buffers/ls_2016.shp", layer = "delete")


rm(buffer_count)
rm(crime)
rm(LS_2016_100)
rm(LS_2016_200)
rm(LS_2016_300)
rm(LS_2016_500)
rm(LS_2016_1000)
rm(LS_2016_1200)





