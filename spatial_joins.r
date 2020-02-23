library("tidyverse")
library("sf")
library("varhandle")

setwd("C:/Users/elmsc/Documents/gis/gis_r")

sla_sf <- st_read("data/shp/SLA_Tracts.shp")
sla_sf <- st_set_crs(sla_sf,4326)

sla_sf <- sla_sf[5]

sla_sf <- sla_sf %>% mutate(GEOID = varhandle::unfactor(GEOID))

sla <- read.csv("output_all/csv/sla.csv")

sla_sf <- left_join(sla_sf,sla, by = "GEOID")

rm(sla)

crime <- read.csv("output_all/csv/crime_2018.csv")

crime <- crime %>% st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)
crime <- crime %>% mutate(COUNT = 1) %>% 
                   mutate(VC_COUNT = ifelse(CRIME_TYPE == "VC", 1,0)) %>% 
                   mutate(PC_COUNT = ifelse(CRIME_TYPE == "PC", 1,0)) %>% 
                   mutate(FPC_COUNT = ifelse(CRIME_TYPE == "PC" & Felony == 1, 1,0)) %>% 
                   mutate(FVC_COUNT = ifelse(CRIME_TYPE == "VC" & Felony == 1, 1,0))


crime <- st_join(crime, left = FALSE, sla_sf %>% dplyr::select(`GEOID`))

crime_count <- aggregate(crime$COUNT,list(GEOID = crime$GEOID), sum) %>% 
               rename("CRIME_COUNT" = "x")

sla_sf <- left_join(sla_sf, crime_count, by = "GEOID")

crime_count <- aggregate(crime$FPC_COUNT,list(GEOID = crime$GEOID), sum) %>%
               rename("FPC_COUNT" = "x")

sla_sf <- left_join(sla_sf, crime_count, by = "GEOID")

crime_count <- aggregate(crime$FVC_COUNT,list(GEOID = crime$GEOID), sum) %>%
  rename("FVC_COUNT" = "x")

sla_sf <- left_join(sla_sf, crime_count, by = "GEOID")

crime_count <- aggregate(crime$PC_COUNT,list(GEOID = crime$GEOID), sum) %>% 
               rename("PC_COUNT" = "x")

sla_sf <- left_join(sla_sf, crime_count, by = "GEOID")

crime_count <- aggregate(crime$VC_COUNT,list(GEOID = crime$GEOID), sum) %>% 
               rename("VC_COUNT" = "x")

sla_sf <- left_join(sla_sf, crime_count, by = "GEOID")
rm(crime_count)

sla_sf$CRIME_COUNT <- sla_sf$CRIME_COUNT %>% replace_na(0)
sla_sf$FPC_COUNT <- sla_sf$FPC_COUNT %>% replace_na(0)
sla_sf$FVC_COUNT <- sla_sf$FVC_COUNT %>% replace_na(0)
sla_sf$PC_COUNT <- sla_sf$PC_COUNT %>% replace_na(0)

# crime_plot <- ggplot(data = sla_sf)+ 
#              geom_sf()+
#              geom_sf(data = crime, size = .05, fill = "red")
# crime_plot
# rm(crime,crime_plot)

mj <- st_read("data/shp/properties/MJ_2018.shp")
mj <- st_set_crs(mj,4326)
mj <- mj %>% mutate(COUNT = 1)

mj <- st_join(mj, left = FALSE, sla_sf %>% dplyr::select(`GEOID`))

property_count <- aggregate(mj$COUNT,list(GEOID = mj$GEOID), sum) %>% 
                  rename("MJ_COUNT" = "x")

sla_sf <- left_join(sla_sf, property_count, by = "GEOID")
sla_sf$MJ_COUNT <- sla_sf$MJ_COUNT %>% replace_na(0)
rm(mj)

ls <- st_read("data/shp/properties/LS_2018.shp")
ls <- st_set_crs(ls,4326)
ls <- ls %>% mutate(LS_COUNT = 1)

ls <- st_join(ls, left = FALSE, sla_sf %>% dplyr::select(`GEOID`))

property_count <- aggregate(ls$LS_COUNT,list(GEOID = ls$GEOID), sum) %>% 
                  rename("LS_COUNT" = "x")

sla_sf <- left_join(sla_sf, property_count, by = "GEOID")
sla_sf$LS_COUNT <- sla_sf$LS_COUNT %>% replace_na(0)
rm(ls)


ts <- st_read("data/shp/properties/TS_2018.shp")
ts <- st_set_crs(ts,4326)
ts <- ts %>% mutate(TS_COUNT = 1)

ts <- st_join(ts, left = FALSE, sla_sf %>% dplyr::select(`GEOID`))

property_count <- aggregate(ts$TS_COUNT,list(GEOID = ts$GEOID), sum) %>% 
  rename("TS_COUNT" = "x")

sla_sf <- left_join(sla_sf, property_count, by = "GEOID")
sla_sf$TS_COUNT <- sla_sf$TS_COUNT %>% replace_na(0)
rm(ts)

st_write(sla_sf, dsn = "output_all/csv/sla_sf_2018.csv", layer_options = "GEOMETRY=AS_XY", delete_dsn=TRUE)
st_write(sla_sf, dsn = "output_all/shp/sla_sf_2018.shp", layer = "delete")

