library("tidyverse")
library("readxl")
library("sf")

setwd("C:/Users/elmsc/Documents/gis/gis_r")

pc_desc <- c("ARSON", "PETTY", "THEFT", "BURGLURY", "STOLEN",
             "VANDALISM", "VEHICLE", "BOMB", "CARJACKING",
             "DRUNK", "DUI", "FIRE", "BIKE", "BUNCO", "FORGERY",
             "EMBEZZLEMENT", "SHOPLIFTING", "FRAUD")

vc_desc <- c("WEAPON", "ROBBERY", "BATTERY", "CRIMINAL", "FIREARM", 
             "RAPE", "MURDER", "PURSE", "SHOTS", "ABUSE", "FORCE",
             "DEADLY", "HOMICIDE", "RESIST", "MOLEST", "NARCOTICS",
             "KIDNAPPING", "MALICIOUS", "ARMED", "BEATING", "THREAT",
             "CRUELTY", "FIGHT", "VIOLENCE", "EXTORTION", "HIT", "HATE",
             "STAB", "SHOOTING", "ASSAULT", "DISCHARGE", "OBSTRUCT", "THEFT FROM PERSON",
             "THREAT")

crime <- read.csv("data/csv/Crime_2017.csv")
unique(crime$CRIME)

pc_crime <- crime %>% filter(str_detect(CRIME, paste(pc_desc,collapse = "|")))
unique(pc_crime$CRIME)

#ONLY USE AFTER INSPECTING UNIQUE NAMES
pc_crime <- pc_crime %>% filter(str_detect(CRIME, 'SHOTS|WEAPON LAWS', negate = TRUE))
unique(pc_crime$CRIME) #Work on getting sums of each crime (summary?)
pc_crime <- pc_crime %>% mutate(CRIME_TYPE = "PC")

vc_crime <- crime %>% filter(str_detect(CRIME, paste(vc_desc,collapse = "|")))
unique(vc_crime$CRIME)

unique(vc_crime$CRIME) #Work on getting sums of each crime (summary?)
vc_crime <- vc_crime %>% mutate(CRIME_TYPE = "VC")

total_crime <- rbind(vc_crime,pc_crime)
rm(crime,vc_crime,pc_crime)

felonies <- read.csv("data/csv/felonies_2017.csv")

total_crime <- merge.data.frame(x=total_crime, y=felonies, by.x = "CRIME", by.y = "CRIME")
rm(felonies)

total_crime <- total_crime %>% st_as_sf(coords = c("LON", "LAT"), crs = 4326, remove = FALSE)

st_write(total_crime, dsn = "output_all/csv/crime_2017.csv", layer_options = "GEOMETRY=AS_XY", delete_dsn=TRUE)
st_write(total_crime, dsn = "output_all/shp/crime_2017.shp", layer = "delete")
