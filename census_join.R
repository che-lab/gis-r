#Look into tidycensus API
library("tidyverse")
library("readxl")
library("sf")
library("varhandle")

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}


setwd("C:/Users/elmsc/Documents/gis/gis_r")


sla_sf <- st_read("data/shp/SLA_Tracts.shp")
sla_sf <- st_set_crs(sla_sf,4326)

sla <- sla_sf[5] %>% st_drop_geometry()
rm(sla_sf)


demographics <- read.csv("data/csv/demographic/AD_2017.csv")

demographics <- demographics %>% rename("GEOID" = "GEO.id2",
                                        "total_population" = "HC01_VC03",
                                        "hispanic" = "HC01_VC93",
                                        "white" = "HC01_VC99",
                                        "black" = "HC01_VC100",
                                        "native_american"= "HC01_VC101",
                                        "asian" = "HC01_VC102",
                                        "pacific_islander" = "HC01_VC103",
                                        "other_alone" = "HC01_VC104",
                                        "two_or_more_other" = "HC01_VC105")

cols <- c("GEOID","total_population","hispanic","white","black","asian","native_american","pacific_islander",
          "other_alone","two_or_more_other")

sla <- left_join(sla, demographics, by = "GEOID")
sla <- sla %>% subset(select=cols)
rm(demographics)

mobility_char <- read.csv("data/csv/geographic_mobility/GM_2017.csv")

mobility_char <- mobility_char %>% rename("GEOID" = "GEO.id2",
                                          "same_house_one_to_four" = "HC01_EST_VC03")

cols <- cols %>% append("same_house_one_to_four") 

sla <- left_join(sla, mobility_char, by = "GEOID")
sla <- sla %>% subset(select=cols)
rm(mobility_char)

poverty <- read.csv("data/csv/poverty/poverty_2017.csv")

poverty <- poverty %>% rename("GEOID" = "GEO.id2",
                              "pop_pov" = "HC01_EST_VC01",
                              "below_pov" = "HC02_EST_VC01")

cols <- cols %>% append("pop_pov") %>% 
                 append("below_pov")

sla <- left_join(sla, poverty, by = "GEOID")
sla <- sla %>% subset(select=cols)
rm(poverty)

renters <- read.csv("data/csv/renters/ACS_17_5YR_B25003_with_ann.csv")

renters <- renters %>% rename("GEOID" = "GEO.id2",
                              "home_owner" = "HD01_VD02",
                              "renter" = "HD01_VD03",
                              "total_units" = "HD01_VD01")

cols <- cols %>% append("home_owner") %>% 
                 append("renter") %>% 
                 append("total_units")
                

sla <- left_join(sla, renters, by = "GEOID")
sla <- sla %>% subset(select=cols)
rm(renters,cols)

#################################################

sla_geo <- sla[1]

sla <- sla %>% varhandle::unfactor()

sla <- sla %>% mutate(p_hispanic = hispanic/total_population,
                      p_white = white/total_population,
                      p_black = black/total_population,
                      p_native = native_american/total_population,
                      p_asian = asian/total_population,
                      p_pacifc = pacific_islander/total_population,
                      p_other = other_alone/total_population,
                      p_other_two = two_or_more_other/total_population,
                      p_renter = renter/total_units,
                      p_moved = same_house_one_to_four/total_units,
                      p_pov = below_pov/pop_pov)

#CLOSER TO 0 MEANS MORE HOMOGENOUS - AKA LESS DIVERSITY
#CLOSER TO 1 MEANS MORE DIVERSITY - AKA LESS SINGULARITY

sla <- sla %>% mutate(heterogeneity = p_hispanic^2 + p_white^2 +p_black^2+
                      p_native^2 + p_asian^2 + p_pacifc^2+
                      p_other^2 + p_other_two^2)

sla[1] <- sla_geo
rm(sla_geo)

sla_sf <- st_read("data/shp/SLA_Tracts.shp")
sla_sf <- st_set_crs(sla_sf,4326)


sla_sf <- dplyr::left_join(sla_sf,sla, by = "GEOID")

#THIS IS REAL WORLD SIZE. NOT GEOMETRIC SIZE

sla_sf <- sla_sf %>% mutate(area_sq_miles = ALAND/2589988.1103) 
sla_sf <- sla_sf %>% mutate(pop_density = total_population/area_sq_miles)

rm(sla)

st_write(sla_sf, dsn = "output/shp/sla.shp", delete_layer = TRUE)
st_write(sla_sf, dsn = "output/csv/sla.csv", layer_options = "GEOMETRY=AS_XY", delete_dsn=TRUE)