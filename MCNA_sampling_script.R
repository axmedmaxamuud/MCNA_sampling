library(xlsx)
library(sp)
library(rgdal)
library(raster)
library(dismo)
library(plotKML)
library(rgeos)
library(dplyr)

setwd("c:/Users/REACH-IRQ-GIS/Documents/201904 MCNA sampling script")
source("functions.R")
WGS84 <- crs("+init=epsg:4326")
UTM38N <- crs("+init=epsg:32638")

##### 0. Initiate #####
# IDP out of camp:
group <- "IDP_out_of_camp"
file <- "IDPs_MasterList_dataset_DTM_IOMFeb 28, 2019.xlsx"
sheet <- "Sheet"
families <-10
camps <- 30
lat=8
long=9

# Returnee:
group <- "Returnee"
file <- "Returnee_MasterList_dataset_DTM_IOMFeb 28, 2019.xlsx"
sheet <- "Sheet"
families <-12
camps <- 32
lat=7
long=8

# Host:
group <- "Host"
file <- "IDPs_MasterList_dataset_DTM_IOMFeb 28, 2019.xlsx"
sheet <- "Sheet"


##### 1. Loading in data and filter on camps #####
poplist <- read.population.list(paste("data/",file,sep=""), sheet=sheet, lat=lat, families=families, camps=camps)
# tweak IDP input for wrong subdistrict shape
# poplist[c(133,2152),c(lat,long)] <- data.frame(Latitude=c(37.06,36.805), Longitude=c(42.38,42.084))
# poplist <- poplist[-23,]
# tweak Returnee input for wrong subdistrict shape
# poplist[c(1,1230,54),c(lat,long)] <- data.frame(Latitude=c(37.06,36.805,34.43), Longitude=c(42.38,42.084,41.1))

##### 2. Spatial join with governorates, districts and subdistricts #####
pop_spdf <- SpatialPointsDataFrame(poplist[,c(long,lat)], poplist, proj4string=WGS84)

# IAU shapes:
subdistricts <- readOGR("IAU_DIBs_SubDistricts/irq_polbnda_adm3_500k_UNAMI_PA.shp","irq_polbnda_adm3_500k_UNAMI_PA")

# OCHA shapes:
districts <- readOGR("IAU_DIBs_SubDistricts/districts.shp","districts")

pop_spdf <- spatial.join.locs(pop_spdf, districts, targetcolumn=c("A2NameEn","A1NameEn"), 
                  column.names=c("OCHA_District","OCHA_Governorate"))
#which(is.na(poplist$IAU_subdistrict))

##### 2.1 Selecting Host population data from raster dataset
if (group == "Host"){
  # THIESSEN POLYGONS
  
  pop_area_spdf <- create.buffers(pop_spdf, 1000, UTM38N)
  
  #extract popdata
  #worldpop_wgs <- raster("data/IRQ_ppp_v2b_2015_UNadj.tif")
  #worldpop_utm <- projectRaster(worldpop, crs=UTM38N)
  #writeRaster(worldpop, "data/IRQ_ppp_v2b_2015_UNadj_UTM.tif")
  
  worldpop <- raster("data/IRQ_ppp_v2b_2015_UNadj_UTM.tif")
  worldpop[is.na(worldpop)] <- 0
  beginCluster()
  population <- extract(worldpop, fun=sum, pop_area_spdf, na.rm=T)
  endCluster()
  
  pop_spdf$hostpop <- population
  pop_spdf$IDP_perc <- pop_spdf$Families / (pop_spdf$hostpop / 6) * 100
  
  #plotKML(pop_area_spdf[which(is.na(pop_area_spdf$hostpop)),])
  
  kml(pop_area_spdf[which(pop_area_spdf$IDP_perc >= 10),], colour=IDP_perc, open.kml=F, plot.labpt=T,
      file.name="IDP percentage on host population.kml",
      folder.name="sampling input",
      labels=paste(round(pop_area_spdf$IDP_perc[which(pop_area_spdf$IDP_perc >= 10),],1),"Percent IDP's"))
  
}

##### 3. write to sampling input csv #####
#select on governorates and subdistricts > 125 IDP families
if (group != "Host"){ 
  sumlist <- poplist[,c(families,ncol(poplist)-2)] 
summarised <- sumlist %>%
  group_by(IAU_subdistrict) %>%
  summarise_if(is.numeric, sum, na.rm=T)
selection <- pull(summarised,IAU_subdistrict)[which(summarised[,2]>125)]
select <- poplist[which(poplist$IAU_governorates %in% c("Ninewa","Salah Al-Din","Kirkuk","Al Anbar") &
                          poplist$IAU_subdistrict %in% selection),]
write.csv(select, sprintf("sampling_input/sampling_input_%s.csv",group),row.names=F)
} else {
  pop_spdf$hostpop <- pop_spdf$hostpop / 6
  #select <- poplist[which(poplist$IAU_governorates %in% c("Ninewa","Salah Al-Din","Kirkuk","Al Anbar")),]
  selected <- pop_spdf[which(pop_spdf$IDP_perc >= 10),]
  write.csv(selected, 
            sprintf("20190417sampling_input_MCNA/sampling_input_%s.csv",group),row.names=F)
}

##### 4. read results #####
sampling_frame <- read.csv("20190417sampling_input_MCNA/sampling_frame.csv", stringsAsFactors=F)
sampling_boundaries <- pop_area_spdf
sampling_boundaries$Survey <- NA
sampling_boundaries$Survey[match(sampling_frame$Location.ID,sampling_boundaries$Location.ID)] <- 
  sampling_frame$Survey
sampling_boundaries <- sampling_boundaries[which(is.na(sampling_boundaries$Survey)==FALSE),]

library(sf)
p1 <- st_sample(sampling_boundaries[1:5,], sampling_boundaries$Survey[1:5])