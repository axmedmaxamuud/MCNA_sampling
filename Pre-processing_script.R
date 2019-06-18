library(rJava)
library(xlsx)
library(sp)
library(rgdal)
library(raster)
library(dismo)
library(plotKML)
library(rgeos)
library(dplyr)

setwd("c:/Users/REACH-IRQ-GIS/Documents/201904 MCNA sampling script")
source("sampling_tool_function.R")
source("functions.R")
WGS84 <- crs("+init=epsg:4326")
UTM38N <- crs("+init=epsg:32638")

output_folder <- "20190619_final_sampling_new_shapes"

##### Stratification #####
# IAU shapes:
stratification <- readOGR("IAU_DIBs_SubDistricts/irq_admbnda_adm3_cso_20190603.shp","irq_admbnda_adm3_cso_20190603")

# OCHA shapes:
# stratification <- readOGR("IAU_DIBs_SubDistricts/districts.shp","districts")

# 0. Initiate 
##### IDP out of camp: #####
group <- "IDP_out_of_camp"
file <- "IDPs_MasterList_dataset_DTM_IOMApr 30, 2019.xlsx"
sheet <- "Sheet"
families <-10
camps <- 30
lat=8
lon=9

# Parameters:
minimum_per_location <- 6
minimum_per_strata <- 200

poplist <- read.population.list(paste("data/",file,sep=""), sheet=sheet, lat=lat, lon=lon, families=families, camps=camps)
# tweak IDP input for wrong subdistrict shape
poplist[c("143","2189"),c(lat,lon)] <- data.frame(Latitude=c(37.06,36.805), Longitude=c(42.38,42.084))
#poplist <- poplist[-23,]

# Remove locations with less than 6 HH's
poplist <- poplist[which(poplist$Families>=minimum_per_location),]

# 2. Spatial join with governorates, districts and subdistricts
pop_spdf <- SpatialPointsDataFrame(poplist[,c(lon,lat)], poplist, proj4string=WGS84)

pop_spdf <- spatial.join.locs(pop_spdf, stratification, targetcolumn=c("ADM3_EN","ADM2_EN","ADM1_EN"), 
                              column.names=c("COD_Subdistrict","COD_District","COD_Governorate"))
#pop_spdf@data[which(is.na(pop_spdf$IAU_District)),]
sumlist <- pop_spdf@data[,c(families,ncol(pop_spdf)-1)] 
summarised <- sumlist %>%
  group_by(COD_District) %>%
  summarise_if(is.numeric, sum, na.rm=T)
selection <- pull(summarised,COD_District)[which(summarised[,2]>minimum_per_strata)]
select <- pop_spdf@data[which(#poplist$IAU_governorates %in% c("Ninewa","Salah Al-Din","Kirkuk","Al Anbar") &
                          pop_spdf$COD_District %in% selection),]
write.csv(select, sprintf("%s/sampling_input_%s.csv",output_folder,group),row.names=F)

##### Run tool
sampling_frame <- sampling.frame(select, samp_type="Cluster sampling", stratified=TRUE, strata="COD_District", 
                                 psu="Location.ID", pop="Families")
target_frame <- target.frame(frame=sampling_frame, c_lev=0.9, proport=0.5, error_marg=0.1)
result <- sampling.tool(cens=sampling_frame, sam=target_frame, samp_type="Cluster sampling", 
                       stratified=TRUE, cls=6, ICC=0.06, buf=0.03, conf_level=0.9, e_marg=0.1)
write.csv(result[[1]], sprintf("%s/sampling_frame_script_%s.csv", output_folder, group))
write.csv(result[[2]], sprintf("%s/sampling_summary_script_%s.csv", output_folder, group))

##### Returnee: #####
group <- "Returnee"
file <- "Returnee_MasterList_dataset_DTM_IOMApr 30, 2019.xlsx"
sheet <- "Sheet"
families <-12
camps <- 32
lat=7
lon=8

poplist <- read.population.list(paste("data/",file,sep=""), sheet=sheet, lat=lat, lon=lon, families=families, camps=camps)
# tweak Returnee input for wrong subdistrict shape
poplist[c(1,1277,54),c(lat,lon)] <- data.frame(Latitude=c(37.06,36.805,34.43), Longitude=c(42.38,42.084,41.1))
poplist[c(711,712),c(lat,lon)] <- poplist[c(711,712),c(lon,lat)]

# Remove locations with less than 6 people
poplist <- poplist[which(poplist[,families]>=minimum_per_location),]

# 2. Spatial join with governorates, districts and subdistricts
pop_spdf <- SpatialPointsDataFrame(poplist[,c(lon,lat)], poplist, proj4string=WGS84)

pop_spdf <- spatial.join.locs(pop_spdf, stratification, targetcolumn=c("ADM3_EN","ADM2_EN","ADM1_EN"), 
                              column.names=c("COD_Subdistrict","COD_District","COD_Governorate"))
#pop_spdf@data[which(is.na(pop_spdf$IAU_District)),]
sumlist <- pop_spdf@data[,c(families,ncol(pop_spdf)-1)] 
summarised <- sumlist %>%
  group_by(COD_District) %>%
  summarise_if(is.numeric, sum, na.rm=T)
selection <- pull(summarised,COD_District)[which(summarised[,2]>minimum_per_strata)]
select <- pop_spdf@data[which(#poplist$IAU_governorates %in% c("Ninewa","Salah Al-Din","Kirkuk","Al Anbar") &
                          pop_spdf$COD_District %in% selection),]
write.csv(select, sprintf("%s/sampling_input_%s.csv",output_folder,group),row.names=F)

##### Run tool
sampling_frame <- sampling.frame(select, samp_type="Cluster sampling", stratified=TRUE, strata="COD_District", 
                                 psu="Location.ID", pop="Returnee.Families")
target_frame <- target.frame(frame=sampling_frame, c_lev=0.9, proport=0.5, error_marg=0.1)
result <- sampling.tool(cens=sampling_frame, sam=target_frame, samp_type="Cluster sampling", 
                        stratified=TRUE, cls=6, ICC=0.06, buf=0.03, conf_level=0.9, e_marg=0.1)
write.csv(result[[1]], sprintf("%s/sampling_frame_script_%s.csv", output_folder, group))
write.csv(result[[2]], sprintf("%s/sampling_summary_script_%s.csv", output_folder, group))

##### Host: #####
group <- "Host"
file <- "IDPs_MasterList_dataset_DTM_IOMApr 30, 2019.xlsx"
sheet <- "Sheet"
families <-10
camps <- 30
lat=8
lon=9
HHsize=6

poplist <- read.population.list.critical(paste("data/",file,sep=""), sheet=sheet, lat=lat, 
                                         families=families, camps=camps,unfinished=40)
# tweak IDP input for wrong subdistrict shape
poplist[c("143","2189"),c(lat,lon)] <- data.frame(Latitude=c(37.06,36.805), Longitude=c(42.38,42.084))
#poplist <- poplist[-23,]

pop_spdf <- SpatialPointsDataFrame(poplist[,c(lon,lat)], poplist, proj4string=WGS84)

pop_spdf <- spatial.join.locs(pop_spdf, stratification, targetcolumn=c("ADM3_EN","ADM2_EN","ADM1_EN"), 
                              column.names=c("COD_Subdistrict","COD_District","COD_Governorate"))
#which(is.na(poplist$IAU_subdistrict))
# THIESSEN POLYGONS
pop_area_spdf <- create.buffers(pop_spdf, 2000, UTM38N)

#extract popdata
#worldpop_wgs <- raster("data/IRQ_ppp_v2b_2015_UNadj.tif")
#worldpop_utm <- projectRaster(worldpop, crs=UTM38N)
#writeRaster(worldpop, "data/IRQ_ppp_v2b_2015_UNadj_UTM.tif")

pop_spdf <- extract.population("data/IRQ_ppp_v2b_2015_UNadj_UTM.tif", pop_area_spdf, pop_spdf, "Families",HHsize)

quantile(pop_spdf$Families)

criteria1 = list(IDP5=5,IDP13=13)
criteria2 = list(host200=200, host450=450)
criteria3 = list(CS=TRUE, ALL=c(TRUE,FALSE))
criteria4 = list(dist199=199,dist499=499)

i1=2
i2=1
i3=1
i4=2


# for(i1 in 1:length(criteria1)){
#   for(i2 in 1:length(criteria2)){
#     for(i3 in 1:length(criteria3)){
#       for(i4 in 1:length(criteria4)){

pop_spdf_selection <- pop_spdf[which(pop_spdf$hostpop > criteria2[[i2]] & pop_spdf$Families >= criteria1[[i1]] &
                                       pop_spdf$critical_shelter %in% criteria3[[i3]]),]
pop_area_spdf_selection <- pop_area_spdf[which(pop_spdf$hostpop > criteria2[[i2]] & 
                                                 pop_spdf$Families >= criteria1[[i1]] &
                                                           pop_spdf$critical_shelter %in% criteria3[[i3]]),]

IDP_districts <- read.csv(sprintf("%s/sampling_summary_script_IDP_out_of_camp.csv",output_folder))

sumlist <- pop_spdf_selection@data[,c(match("hostpop",names(pop_spdf_selection)),ncol(pop_spdf_selection)-3)] 
summarised <- sumlist %>%
  group_by(COD_District) %>%
  summarise_if(is.numeric, sum, na.rm=T)
selection <- pull(summarised,COD_District)[which(summarised[,2]>criteria4[[i4]])]
selection <- selection[which(selection %in% IDP_districts$Stratification)]
selected <- pop_spdf_selection[which(#pop_spdf$IDP_perc >= 10 &
                             pop_spdf_selection$COD_District %in% selection),]
selected_area <- pop_area_spdf_selection[which(#pop_spdf$IDP_perc >= 10 &
  pop_spdf_selection$COD_District %in% selection),]
selected$hostpop <- selected$hostpop[,1]
selected$IDP_perc <- selected$IDP_perc[,1]
writeOGR(selected, output_folder, "host_locations",driver="ESRI Shapefile",
         overwrite_layer=T)
writeOGR(selected_area, output_folder, overwrite_layer=T,
         "host_location_areas",driver="ESRI Shapefile")

# pop_area_spdf$IDP_perc <- pop_spdf$IDP_perc
# kml(pop_area_spdf, colour=IDP_perc, open.kml=F, plot.labpt=T,
#     file.name=sprintf("%s/IDP percentage on host populationt.kml",output_folder),
#     folder.name="sampling input",
#     shape = "http://maps.google.com/mapfiles/kml/pal2/icon15.png",
#     labels=paste(round(pop_area_spdf$IDP_perc,1),"Percent IDP's"))
#select <- poplist[which(poplist$IAU_governorates %in% c("Ninewa","Salah Al-Din","Kirkuk","Al Anbar")),]
write.csv(selected, 
          sprintf("%s/sampling_input_different_scenarios_%s_%s_%s_%s_%s.csv",output_folder,group,
                  names(criteria1)[i1],names(criteria2)[i2],names(criteria3)[i3],names(criteria4)[i4]),row.names=F)
# }}}}

##### Run tool
sampling_frame <- sampling.frame(selected@data, samp_type="Cluster sampling", stratified=TRUE, strata="COD_District", 
                                 psu="Location.ID", pop="hostpop")
target_frame <- target.frame(frame=sampling_frame, c_lev=0.9, proport=0.5, error_marg=0.1)
result <- sampling.tool(cens=sampling_frame, sam=target_frame, samp_type="Cluster sampling", 
                        stratified=TRUE, cls=6, ICC=0.06, buf=0.03, conf_level=0.9, e_marg=0.1)
write.csv(result[[1]], sprintf("%s/sampling_frame_script_%s.csv", output_folder, group))
write.csv(result[[2]], sprintf("%s/sampling_summary_script_%s.csv", output_folder, group))
