to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))
}

read.population.list <- function(file, sheet, families, lat, lon, filter.on.camps=TRUE, camps=30) {
  poplist <- read.xlsx(file ,sheetName=sheet, startRow=1, colIndex=1:camps)
  poplist[,c(lat,lon)] <- lapply(poplist[,c(lat,lon)], as.numeric)
  poplist <- poplist[which(is.na(poplist[,camps]) | poplist[,camps]==0),c(1:families)]
  poplist <- poplist[-which(poplist[,lat]<5 | is.na(poplist[,lat])),]
  poplist
}
read.population.list.critical <- function(file, sheet, families, lat, filter.on.camps=TRUE, camps=30, unfinished=38) {
  poplist <- read.xlsx(file,sheetName=sheet, startRow=1, colIndex=1:unfinished)
  critical <- (poplist[,unfinished]>0 | poplist[,unfinished-1]>0 | poplist[,unfinished-5]>0 | 
                 poplist[,unfinished-7]>0)
  poplist$critical_shelter <- critical
  poplist <- poplist[which((is.na(poplist[,camps]) | poplist[,camps]==0)),c(1:families,ncol(poplist))]
  poplist <- poplist[-which(poplist[,lat]<5 | is.na(poplist[,lat])),]
  poplist
}

spatial.join.locs <- function(points, shapes, targetcolumn, column.names){
  shapes <- spTransform(shapes, crs(points))
  for (i in 1:length(targetcolumn)){
    points@data[,column.names[i]] <- over(points, shapes)[,targetcolumn[i]]
  }
  points
}

create.buffers <- function(points, buffer, utm, eps=1e-09){
  
  # Buffer
  points_UTM <- spTransform(points,utm)
  points_buffer <- gBuffer(points_UTM, width=buffer,byid=F)
  
  # Intersect
  vor <- voronoi(points_UTM, eps=eps)
  pop_area <- gIntersection(vor,points_buffer, byid=T, id=rownames(vor@data))
  pop_area_spdf <- SpatialPolygonsDataFrame(pop_area, vor@data)
  
  pop_area_spdf
}

extract.population <- function(rasterfile, areas, points, column, HHsize){
  gc()
  worldpop <- raster(rasterfile)
  worldpop[is.na(worldpop)] <- 0
  beginCluster()
  population <- extract(worldpop, fun=sum, areas, na.rm=T)
  endCluster()
  
  points$hostpop <- population / HHsize
  points$IDP_perc <- points@data[,column] / (points$hostpop) * 100
  points
}
sampling <- function(x,i){
  sp <- SpatialPointsDataFrame(spsample(x[i,],x$survey_buffer[i], "random"),
                               x@data[rep(i,x$survey_buffer[i]),])
  sp
}
area.sampling <- function(areas){
  samples_points <- sampling(areas,1)
  for (i in 2:nrow(areas)){
    samples_points <- rbind(samples_points, sampling(areas,i))
  }
  samples_points
}
create.sample.area <- function(samples, mil_areas){
  spdf <- SpatialPointsDataFrame(samples[,c("Longitude","Latitude")], samples, proj4string=WGS84)
  areas <- create.buffers(spdf,500,UTM38N)
  areas_filtered <- gDifference(areas,mil_areas,byid=c(T,F))
  af <- SpatialPolygonsDataFrame(areas_filtered,
                                 areas@data[as.numeric(sapply(areas_filtered@polygons,FUN = function(x)x@ID)),])
  if(nrow(af)<nrow(spdf)){
    missing <- areas[-as.numeric(sapply(areas_filtered@polygons,FUN = function(x)x@ID)),]
    warning(sprintf("Some areas are completely covered by military area and are therefore not 
                    included in the result. Missing areas: %s", missing$Location.Name))
  }
  af_wgs <- spTransform(af,WGS84)
}
sample.to.kml <- function(area, type, color, samplepoints=T, output_folder){
  colour <- ifelse(color=="green", "darkgreen", ifelse(color=="brown","darkbrown",color))
  centers <- data.frame(gCentroid(area, byid = TRUE))
  centers$label <- sprintf("This marker is not the specific place for the interview! For Host look at the red markers and for IDP/Returnee find households within the green area.\n\nName: %s\nSurveys: %d\nPopulation group: %s",area$label,area$survey_buffer,type)
  centers$lbl <- area$label
  cen <- SpatialPointsDataFrame(centers[,c(1,2)],proj4string=WGS84,data=centers)
  
  ls <- c()
  for (i in 1:length(area@polygons)){
    if(length(area@polygons[[i]]@Polygons)>1) {ls <- c(ls, area@polygons[[i]]@Polygons[[2]])}
  }
  
  lines <- as(area,'SpatialLinesDataFrame')
  if (!is.null(ls)){
    pols <- lapply(ls, FUN=function(x){Polygons(list(x),as.character(sample(1000:10000,1)))})
    line <- SpatialLinesDataFrame(as(SpatialPolygons(pols,proj4string = WGS84),
                                     'SpatialLines'), area@data[1:length(pols),], match.ID=F)
    lines <- rbind(lines,line)
  } 
  filename <- sprintf("%s/%s/kml_files/%s_%s_%s_%s.kml",getwd(),output_folder, area$COD_Governorate,
                      area$strata, type, gsub("/","-",area$label))
  kml_open(file.name=filename,
           folder.name=sprintf("%s %s", type, area$label), kml_open=F)
  if (colour=="blue"){
  kml_layer(lines,plot.labpt=F, subfolder.name="area", colour="blue")
  } else if (colour=="darkbrown"){
    kml_layer(lines,plot.labpt=F, subfolder.name="area", colour="chocolate4")
  }else if (colour=="darkgreen"){
    kml_layer(lines,plot.labpt=F, subfolder.name="area", colour="darkgreen")
  }
  kml_layer(cen,plot.labpt=T, points_names=gsub("&","-",cen$lbl), html.table=gsub("&","-",cen$label),
            subfolder.name="centrepoint")
  
  if (samplepoints){
    sample_points <- sampling(area,1)
    kml_layer(sample_points, plot.labpt=T, points_names=gsub("&","-",sample_points$label),
              html.table="single samplepoint", subfolder.name="samplepoints")
  }
  kml_close(file.name=filename)
  fix.styling.for.maps.me(filename, color)
  if (samplepoints){return(sample_points)}
}
fix.styling.for.maps.me <- function(file, color){
  tx <- readLines(file)
  matches <- grep("<styleUrl>#[a-zA-Z0-9]+</styleUrl>",tx,perl=F)
  matches2 <- grep("#",tx,perl=F)
  tx2 <- tx
  tx2[matches2[1]] <- sub("#","",tx[matches2[1]])
  tx2[matches[2]] <- sub(pattern="<styleUrl>#[a-zA-Z0-9]+</styleUrl>",replacement=sprintf("<styleUrl>#placemark-%s</styleUrl>", color),
                         x=tx[matches[2]])
  writeLines(tx2,con=file)
}
map_military <- function(){
  if(F){ # <-- dont run!
  library(leaflet)
  library(htmlwidgets)
  url <- "https://api.mapbox.com/v3/mapbox.iraq/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiYm91a2VwaWV0ZXJvdHRvdyIsImEiOiJjanZrem82ZnAwdTliNDRtbDljdHptaXpkIn0.vlYoVkiHTdvkHONpNqy_Sg"
  
  leaf <- rmapshaper::ms_simplify(h_areas)
  m <- leaflet(spdf) %>%
    addTiles() %>%
    addMarkers() %>%
    addMarkers(missing$Longitude, missing$Latitude, icon=~icons(iconUrl="http://leafletjs.com/examples/custom-icons/leaf-red.png")) %>%
    addPolygons(data=military_areas,color="green",highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                      bringToFront = TRUE),
                fillOpacity=0.75,
                popupOptions=popupOptions(maxWidth="100%",closeOnClick=T)) %>%
    addPolygons(data=af_wgs,color = "grey", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    #addCircleMarkers(data=h_sample_points, color="blue",radius=1)%>%
    addLegend(colors=c("green","grey","blue"), labels=c("military areas","host pop sample areas","sample locations"))
  saveWidget(m,sprintf("%s/%s/map.html",getwd(),output_folder))
  }
}
