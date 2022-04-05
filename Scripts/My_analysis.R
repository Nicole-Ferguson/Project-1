#Creating polygons of Irlanda, Hamburgo and Forest

library(sf)
library(terra)
library(dplyr)
library(lidR)
library(leafR)
library(ggplot2)
library(rGEDI)
library(sp)
library(rnaturalearth)
library(tidyverse)
library(lubridate)
library(raster)
library(rgdal)

#creating matrix of longitude and latitude of occurrences for each place

# Filter point counts per zone
pointCounts <- read.csv("resources/Point_counts.csv")
pointCounts_irlanda <- pointCounts[pointCounts$Zone == "irlanda",]
pointCounts_forest <- pointCounts[pointCounts$Zone == "restoration" | pointCounts$Zone == "bosque",]
pointCounts_hamburgo <- pointCounts[pointCounts$Zone == "hamburgo",]

#Extract latitude and longitude for each zone
IrlandaLat<-pointCounts_irlanda[,"Lat"]
IrlandaLon<-pointCounts_irlanda[,"Lon"]
IrlandaCoords<-data.frame(IrlandaLat,IrlandaLon)
IrlandaCoords2<-unique(IrlandaCoords)
IrlandaCoords2<-IrlandaCoords2[-23,]

HamburgoLat<-pointCounts_hamburgo[,"Lat"]
HamburgoLon<-pointCounts_hamburgo[,"Lon"]
HamburgoCoords<-data.frame(HamburgoLat,HamburgoLon)
HamburgoCoords2<-unique(HamburgoCoords)


ForestLat<-pointCounts_forest[,"Lat"]
ForestLon<-pointCounts_forest[,"Lon"]
ForestCoords<-data.frame(ForestLat,ForestLon)
ForestCoords2<-unique(ForestCoords)

ForestCoords3<-data.matrix(ForestCoords2)

#creating polygons

Irlanda_polygon<-chull(IrlandaCoords2)
Hamburgo_polygon<-chull(HamburgoCoords2)
Forest_polygon<-chull(ForestCoords2)

#Drawing Forest polygon
coord5 <- data.frame(name = "Point 5", x = 	
                       -92.33737, y = 15.16781)
point5 <- st_as_sf(coord5, coords = c("x", "y"), crs = 4326)

coord6 <- data.frame(name = "Point 6", x = 	
                       -92.33570, y = 15.16850)
point6 <- st_as_sf(coord6, coords = c("x", "y"), crs = 4326)
pts<-rbind(point5, point6)
coord1 <- data.frame(name = "Point 1", x = 	
                       -92.33514, y = 15.17273)
point1 <- st_as_sf(coord1, coords = c("x", "y"), crs = 4326)
coord2 <- data.frame(name = "Point 2", x = 	
                       -92.33579, y = 15.17157)
point2 <- st_as_sf(coord2, coords = c("x", "y"), crs = 4326)

pts<-rbind(pts,point1)
pts<-rbind(pts,point2)
Forestpolygon_final <- st_sfc(st_polygon(list(st_coordinates(rbind(pts, point5)))), crs = 4326)

# Get elevation data
srtm <- terra::rast("Resources/elevation/N15W093.hgt")

pointCounts <- pointCounts[pointCounts$?..Point_count <= 45,]
pointCounts_sf <- st_as_sf(pointCounts, coords = c("Lon", "Lat"), crs = crs(srtm))

# Plot polygon on map
plot(srtm, xlim=c(-92.36, -92.32), ylim=c(15.15,15.19))
plot(pointCounts_sf["Zone"],add=TRUE)
plot(Forestpolygon_final, add=TRUE)

plot(Forestpolygon_final, col = "cyan", axes = TRUE)
plot(pts$geometry, add = TRUE, cex = 2)
text(x = st_coordinates(pts), labels = pts$name)

#Drawing Hamburgo polygon

coord8 <- data.frame(name = "Point 8", x = HamburgoCoords2[8,2], y= HamburgoCoords2[8,1])
point8 <- st_as_sf(coord8,coords = c("x","y"),crs = 4326)

coord10 <- data.frame(name = "Point 10", x =  HamburgoCoords2[10,2], y= HamburgoCoords2[10,1])
point10 <- st_as_sf(coord10, coords = c("x", "y"), crs = 4326)

coord15 <- data.frame(name = "Point 15", x = HamburgoCoords2[15,2], y=HamburgoCoords2[15,1])
point15 <- st_as_sf(coord15, coords = c("x", "y"), crs = 4326)

coord6 <- data.frame(name = "Point 6", x =HamburgoCoords2[6,2], y= HamburgoCoords2[6,1])
point6 <- st_as_sf(coord6, coords = c("x", "y"), crs = 4326)

coord7 <- data.frame(name = "Point 7", x =HamburgoCoords2[7,2], y= HamburgoCoords2[7,1])
point7 <- st_as_sf(coord7, coords = c("x", "y"), crs = 4326)

coord13 <- data.frame(name = "Point 13", x = HamburgoCoords2[13,2], y= HamburgoCoords2[13,1]) 	
point13 <- st_as_sf(coord13, coords = c("x", "y"), crs = 4326)

coord12 <- data.frame(name = "Point 12", x = HamburgoCoords2[12,2], y= HamburgoCoords2[12,1])
point12 <- st_as_sf(coord12, coords = c("x", "y"), crs = 4326)


ptsH<-rbind(point8, point10, point15,point6, point7,point13, point12)
Hamburgopolygon_final <- st_sfc(st_polygon(list(st_coordinates(rbind(ptsH, point8)))), crs = 4326)
# Plot polygon on map
plot(srtm, xlim=c(-92.36, -92.32), ylim=c(15.15,15.19))
plot(pointCounts_sf["Zone"],add=TRUE)
plot(Hamburgopolygon_final, add=TRUE)

#Drawing Irlanda polygon

coord2 <- data.frame(name = "Point 2", x = IrlandaCoords2[2,2], y= IrlandaCoords2[2,1])
point2 <- st_as_sf(coord2,coords = c("x","y"),crs = 4326)

coord3 <- data.frame(name = "Point 3", x =  IrlandaCoords2[3,2], y= IrlandaCoords2[3,1])
point3 <- st_as_sf(coord3, coords = c("x", "y"), crs = 4326)

coord10 <- data.frame(name = "Point 10", x = IrlandaCoords2[10,2], y=IrlandaCoords2[10,1])
point10 <- st_as_sf(coord10, coords = c("x", "y"), crs = 4326)

coord14 <- data.frame(name = "Point 14", x = IrlandaCoords2[14,2], y= IrlandaCoords2[14,1])
point14 <- st_as_sf(coord14, coords = c("x", "y"), crs = 4326)

coord18 <- data.frame(name = "Point 18", x = IrlandaCoords2[18,2], y= IrlandaCoords2[18,1])
point18 <- st_as_sf(coord18, coords = c("x", "y"), crs = 4326)

coord17 <- data.frame(name = "Point 17", x = IrlandaCoords2[17,2], y= IrlandaCoords2[17,1]) 	
point17 <- st_as_sf(coord17, coords = c("x", "y"), crs = 4326)

coord16 <- data.frame(name = "Point 16", x = IrlandaCoords2[16,2], y= IrlandaCoords2[16,1])
point16 <- st_as_sf(coord16, coords = c("x", "y"), crs = 4326)

coord15 <- data.frame(name = "Point 15", x = IrlandaCoords2[15,2], y= IrlandaCoords2[15,1]) 	
point15 <- st_as_sf(coord15, coords = c("x", "y"), crs = 4326)

coord1 <- data.frame(name = "Point 1", x = IrlandaCoords2[1,2], y= IrlandaCoords2[1,1])
point1 <- st_as_sf(coord1, coords = c("x", "y"), crs = 4326)


ptsI<-rbind(point2,point3,point10,point14,point18,point17,point16, point15, point1)
Irlandapolygon_final <- st_sfc(st_polygon(list(st_coordinates(rbind(ptsI, point2)))), crs = 4326)

# Plot polygon on map
plot(srtm, xlim=c(-92.36, -92.32), ylim=c(15.15,15.19))
plot(pointCounts_sf["Zone"],add=TRUE)
plot(Hamburgopolygon_final, add=TRUE)
plot(Forestpolygon_final, add=TRUE)
plot(Irlandapolygon_final, add=TRUE)

# Reading GEDI data

gedilevel2b <- readLevel2B(level2Bpath = paste0("resources/lidar/", list.files("resources/lidar/"))[1])
level2BVPM<-getLevel2BVPM(gedilevel2b)
head(level2BVPM[,c("beam","shot_number","pai","fhd_normal","omega","pgap_theta","cover")])
level2BVPM$shot_number<-paste0(level2BVPM$shot_number)
toremove <- which(is.na(level2BVPM$longitude_bin0) == T)
if(length(toremove > 0)){
  level2BVPM <- level2BVPM[-toremove,]
}
level2BVPM_spdf<-SpatialPointsDataFrame(cbind(level2BVPM$longitude_bin0,level2BVPM$latitude_bin0),data=level2BVPM)
gedi_irlanda <- level2BVPM_spdf[which(level2BVPM_spdf$latitude_bin0 > 15.15 & level2BVPM_spdf$latitude_bin0 < 15.19 & level2BVPM_spdf$longitude_bin0 >= -92.36 & level2BVPM_spdf$longitude_bin0 <= -92.32),]

for(i in 2:length(list.files("resources/lidar/"))){
  gedilevel2b <- readLevel2B(level2Bpath = paste0("resources/lidar/", list.files("resources/lidar/"))[i])
  level2BVPM<-getLevel2BVPM(gedilevel2b)
  head(level2BVPM[,c("beam","shot_number","pai","fhd_normal","omega","pgap_theta","cover")])
  level2BVPM$shot_number<-paste0(level2BVPM$shot_number)
  toremove <- which(is.na(level2BVPM$longitude_bin0) == T)
  if(length(toremove > 0)){
    level2BVPM <- level2BVPM[-toremove,]
  }
  level2BVPM_spdf<-SpatialPointsDataFrame(cbind(level2BVPM$longitude_bin0,level2BVPM$latitude_bin0),data=level2BVPM)
  gedi_irlanda <- rbind(gedi_irlanda, level2BVPM_spdf[which(level2BVPM_spdf$latitude_bin0 > 15.15 & level2BVPM_spdf$latitude_bin0 < 15.19 & level2BVPM_spdf$longitude_bin0 >= -92.36 & level2BVPM_spdf$longitude_bin0 <= -92.32),])
}

#Find which GEDI points fall within each polygon and extract those
gedi_irlanda2<-as.data.frame(gedi_irlanda)
gedi_irlanda_coords<-dplyr::select(gedi_irlanda2, coords.x2, coords.x1)
gedi_irlanda_coords2<-st_as_sf(gedi_irlanda_coords, coords = c("coords.x1", "coords.x2"), crs = 4326)

Irlanda_gedi<-st_contains(Irlandapolygon_final,gedi_irlanda_coords2)
Hamburgo_gedi<-st_contains(Hamburgopolygon_final, gedi_irlanda_coords2)
Forest_gedi<-st_contains(Forestpolygon_final,gedi_irlanda_coords2)

#Find metrics for those gedi points

#Get canopy height of GEDI points for each location

Irlanda_metrics<-gedi_irlanda2[Irlanda_gedi[[1]], ]
Canopy_h_Irlanda<-Irlanda_metrics$rh100
toremove<-which(Canopy_h_Irlanda==0)
Canopy_h_Irlanda<-Canopy_h_Irlanda[-toremove]
Canopy_h_Irlanda<-Canopy_h_Irlanda/100

Hamburgo_metrics<-gedi_irlanda2[Hamburgo_gedi[[1]], ]
Canopy_h_Hamburgo<-Hamburgo_metrics$rh100
toremove<-which(Canopy_h_Hamburgo==0)
Canopy_h_Hamburgo<-Canopy_h_Hamburgo[-toremove]
Canopy_h_Hamburgo<-Canopy_h_Hamburgo/100

#Forest_metrics<-gedi_irlanda2[Forest_gedi[[1]], ]
#Canopy_h_Forest<-Forest_metrics$rh100


mean(Canopy_h_Irlanda)
mean(Canopy_h_Hamburgo)
mean(Canopy_h_Forest)


Irlanda_height<-data.frame("Irlanda",Canopy_h_Irlanda)
names(Irlanda_height)<-c("Area","Canopy_height")

Hamburgo_height<-data.frame("Hamburgo",Canopy_h_Hamburgo)
names(Hamburgo_height)<-c("Area","Canopy_height")

Forest_height<-data.frame("Forest",Canopy_h_Forest)
names(Forest_height)<-c("Area","Canopy_height")

Canopy_h_table<-rbind(Irlanda_height,Hamburgo_height)


boxplot(Canopy_height~Area,data=Canopy_h_table)

#Get canopy cover of GEDI points for each location

Canopy_cov_Irlanda<-Irlanda_metrics$cover
Canopy_cov_Hamburgo<-Hamburgo_metrics$cover
Canopy_cov_Forest<-Forest_metrics$cover#not enough data


toremove <- which(Canopy_cov_Irlanda == -9999)
if(length(toremove > 0)){
  Canopy_cov_Irlanda <- Canopy_cov_Irlanda[-toremove]
}

toremove <- which(Canopy_cov_Hamburgo == -9999)
if(length(toremove > 0)){
  Canopy_cov_Hamburgo <- Canopy_cov_Hamburgo[-toremove]
}

Canopy_cov_Irlanda<-Canopy_cov_Irlanda*100
Canopy_cov_Hamburgo<-Canopy_cov_Hamburgo*100


mean(Canopy_cov_Irlanda) 
mean(Canopy_cov_Hamburgo)
mean(Canopy_cov_Forest)

Irlanda_cover<-data.frame("Irlanda",Canopy_cov_Irlanda)
names(Irlanda_cover)<-c("Area","Canopy_cover")

Hamburgo_cover<-data.frame("Hamburgo",Canopy_cov_Hamburgo)
names(Hamburgo_cover)<-c("Area","Canopy_cover")

#Forest_cover<-data.frame("Forest",Canopy_cov_Forest)
#names(Forest_cover)<-c("Area","Canopy_cover")


Canopy_cov_table<-rbind(Irlanda_cover,Hamburgo_cover)


boxplot(Canopy_cover~Area,data=Canopy_cov_table)

#Canopy cover proxy for comparison with handheld lidar
Gedi_layers_Irlanda$above3m <- rowSums(Gedi_layers_Irlanda[2:30])
Gedi_layers_Hamburgo$above3m <- rowSums(Gedi_layers_Hamburgo[2:30])




#Get structural diversity of GEDI points for each location


#Getting PAI data

gedilevel2b <- readLevel2B(level2Bpath = paste0("resources/lidar/", list.files("resources/lidar/"))[1])
level2BPAI<-getLevel2BPAIProfile(gedilevel2b)

level2BPAI$shot_number<-paste0(level2BPAI$shot_number)
toremove <- which(is.na(level2BPAI$lon_lowestmode) == T)
if(length(toremove > 0)){
  level2BPAI <- level2BPAI[-toremove,]
}
level2BPAI_spdf<-SpatialPointsDataFrame(cbind(level2BPAI$lon_lowestmode,level2BPAI$lat_lowestmode),data=level2BPAI)
all_PAI<- level2BPAI_spdf[which(level2BPAI_spdf$lat_lowestmode > 15.15 & level2BPAI_spdf$lat_lowestmode < 15.19 & level2BPAI_spdf$lon_lowestmode >= -92.36 & level2BPAI_spdf$lon_lowestmode <= -92.32),]


for(i in 2:length(list.files("resources/lidar/"))){
  gedilevel2b <- readLevel2B(level2Bpath = paste0("resources/lidar/", list.files("resources/lidar/"))[i])
  level2BPAI<-getLevel2BPAIProfile(gedilevel2b)
  level2BPAI$shot_number<-paste0(level2BPAI$shot_number)
  toremove <- which((is.na(level2BPAI$lon_lowestmode) == T)|level2BPAI$pai_z0_5m==-9999)
  if(length(toremove > 0)){
    level2BPAI <- level2BPAI[-toremove,]
  }
  level2BPAI_spdf<-SpatialPointsDataFrame(cbind(level2BPAI$lon_lowestmode,level2BPAI$lat_lowestmode),data=level2BPAI)
  all_PAI<- rbind(all_PAI, level2BPAI_spdf[which(level2BPAI_spdf$lat_lowestmode > 15.15 & level2BPAI_spdf$lat_lowestmode< 15.19 & level2BPAI_spdf$lon_lowestmode >= -92.36 & level2BPAI_spdf$lon_lowestmode <= -92.32),])
}
all_PAI2<-as.data.frame(all_PAI)

#getting PAI values for each area

all_PAI3<-all_PAI2[,12:41]
Gedi_layers_Irlanda<-all_PAI3[Irlanda_gedi[[1]], ]
Gedi_layers_Hamburgo<-all_PAI3[Hamburgo_gedi[[1]], ]
Gedi_layers_Forest<-all_PAI3[Forest_gedi[[1]], ]#not enough data


#Total PAI per area
Gedi_layers_Irlanda$total <- rowSums(Gedi_layers_Irlanda[1:30])
Gedi_layers_Hamburgo$total <- rowSums(Gedi_layers_Hamburgo[1:30])
#Gedi_layers_Forest$total <- rowSums(Gedi_layers_Forest[1:30])

Irlanda_PAI<-data.frame("Irlanda",Gedi_layers_Irlanda$total)
names(Irlanda_PAI)<-c("Area","Total_PAI")
Hamburgo_PAI<-data.frame("Hamburgo",Gedi_layers_Hamburgo$total)
names(Hamburgo_PAI)<-c("Area","Total_PAI")
#Forest_PAI<-data.frame("Forest",Gedi_layers_Forest$total)
#names(Forest_PAI)<-c("Area","Total_PAI")
total_PAI<-rbind(Irlanda_PAI,Hamburgo_PAI)
boxplot(Total_PAI~Area,data=total_PAI)

#understory or coffee PAI per area 0-5m
Irlanda_PAI_05<-data.frame("Irlanda",Gedi_layers_Irlanda$pai_z0_5m)
names(Irlanda_PAI_05)<-c("Area","Understory_PAI")
Hamburgo_PAI_05<-data.frame("Hamburgo",Gedi_layers_Hamburgo$pai_z0_5m)
names(Hamburgo_PAI_05)<-c("Area","Understory_PAI")
#Forest_PAI_05<-data.frame("Forest",Gedi_layers_Forest$pai_z0_5m)
#names(Forest_PAI_05)<-c("Area","Understory_PAI")
Understory_PAI<-rbind(Irlanda_PAI_05,Hamburgo_PAI_05)
boxplot(Understory_PAI~Area,data=Understory_PAI)

#mid PAI 5-10 m
Irlanda_PAI_510<-data.frame("Irlanda",Gedi_layers_Irlanda$pai_z5_10m)
names(Irlanda_PAI_510)<-c("Area","Mid_PAI")
Hamburgo_PAI_510<-data.frame("Hamburgo",Gedi_layers_Hamburgo$pai_z5_10m)
names(Hamburgo_PAI_510)<-c("Area","Mid_PAI")
#Forest_PAI_510<-data.frame("Forest",Gedi_layers_Forest$pai_z5_10m)
#names(Forest_PAI_510)<-c("Area","Mid_PAI")
Mid_PAI<-rbind(Irlanda_PAI_510,Hamburgo_PAI_510)
boxplot(Mid_PAI~Area,data=Mid_PAI)
#canopy PAI 10-20m
Gedi_layers_Irlanda$canopy <- rowSums(Gedi_layers_Irlanda[3:4])
Gedi_layers_Hamburgo$canopy <- rowSums(Gedi_layers_Hamburgo[3:4])
#Gedi_layers_Forest$canopy <- rowSums(Gedi_layers_Forest[3:4])

Irlanda_PAI_1020<-data.frame("Irlanda",Gedi_layers_Irlanda$canopy)
names(Irlanda_PAI_1020)<-c("Area","Canopy_PAI")

Hamburgo_PAI_1020<-data.frame("Hamburgo",Gedi_layers_Hamburgo$canopy)
names(Hamburgo_PAI_1020)<-c("Area","Canopy_PAI")

#Forest_PAI_1020<-data.frame("Forest",Gedi_layers_Forest$canopy)
#names(Forest_PAI_1020)<-c("Area","Canopy_PAI")

Canopy_PAI<-rbind(Irlanda_PAI_1020,Hamburgo_PAI_1020)
boxplot(Canopy_PAI~Area,data=Canopy_PAI)
#high canopy PAI 20+m
Gedi_layers_Irlanda$high_canopy <- rowSums(Gedi_layers_Irlanda[5:30])
Gedi_layers_Hamburgo$high_canopy <- rowSums(Gedi_layers_Hamburgo[5:30])
#Gedi_layers_Forest$high_canopy <- rowSums(Gedi_layers_Forest[5:30])

Irlanda_PAI_20plus<-data.frame("Irlanda",Gedi_layers_Irlanda$high_canopy)
names(Irlanda_PAI_20plus)<-c("Area","High_canopy_PAI")

Hamburgo_PAI_20plus<-data.frame("Hamburgo",Gedi_layers_Hamburgo$high_canopy)
names(Hamburgo_PAI_20plus)<-c("Area","High_canopy_PAI")

#Forest_PAI_20plus<-data.frame("Forest",Gedi_layers_Forest$high_canopy)
#names(Forest_PAI_20plus)<-c("Area","High_canopy_PAI")

High_canopy_PAI<-rbind(Irlanda_PAI_20plus,Hamburgo_PAI_20plus)
boxplot(High_canopy_PAI~Area,data=High_canopy_PAI)


mean(Gedi_layers_Irlanda$total)
mean(Gedi_layers_Hamburgo$total)

#Get FHD values for each area
all_FHD<-gedi_irlanda$fhd_normal
Gedi_FHD_Irlanda<-all_FHD[Irlanda_gedi[[1]]]
toremove<-which(Gedi_FHD_Irlanda==-9999)
Gedi_FHD_Irlanda<-Gedi_FHD_Irlanda[-toremove]
Gedi_FHD_Hamburgo<-all_FHD[Hamburgo_gedi[[1]]]
toremove<-which(Gedi_FHD_Hamburgo==-9999)
Gedi_FHD_Hamburgo<-Gedi_FHD_Hamburgo[-toremove]
Gedi_FHD_Forest<-all_FHD[Forest_gedi[[1]]]#not enough data

Gedi_FHD_Irlanda2<-data.frame("Irlanda",Gedi_FHD_Irlanda)
names(Gedi_FHD_Irlanda2)<-c("Area","FHD")

Gedi_FHD_Hamburgo2<-data.frame("Hamburgo",Gedi_FHD_Hamburgo)
names(Gedi_FHD_Hamburgo2)<-c("Area","FHD")

Gedi_FHD_Forest2<-data.frame("Forest",Gedi_FHD_Forest)
names(Gedi_FHD_Forest2)<-c("Area","FHD")

Gedi_FHD<-rbind(Gedi_FHD_Irlanda2,Gedi_FHD_Hamburgo2,Gedi_FHD_Forest2)
boxplot(FHD~Area,data=Gedi_FHD)


#Draw plot of canopy height against canopy cover for each area for GEDI

options(scipen=999) #get rid of scientific notation


I<-data.frame("Irlanda",Canopy_cov_Irlanda,Canopy_h_Irlanda)
names(I)<-c("Area","Canopy_cover","Canopy_height")

H<-data.frame("Hamburgo",Canopy_cov_Hamburgo,Canopy_h_Hamburgo)
names(H)<-c("Area","Canopy_cover","Canopy_height")

df<-rbind(I,H)


gg <- ggplot(df, aes(x=Canopy_cover, y=Canopy_height)) + 
  geom_point(aes(col=Area)) +
  scale_color_manual(values=c("#33CC99","#FF6666"))+
  labs(y="Canopy height (m)", 
       x="Canopy cover (%)")+
  theme_classic()
plot(gg)
# main title
gg + theme(plot.title = element_text(face="bold",size=16))

#plot canopy height vs cover for each area for handheld lidar





#plot height to PAI

#PAI table

PAI_20_plus<-rbind(Irlanda_PAI_20plus,Hamburgo_PAI_20plus)
PAI_20_plus<-data.frame(PAI_20_plus,"high canopy (20+)")
PAI_1020<-rbind(Irlanda_PAI_1020,Hamburgo_PAI_1020)
PAI_1020<-data.frame(PAI_1020,"canopy (10-20)")
PAI_510<-rbind(Irlanda_PAI_510,Hamburgo_PAI_510)
PAI_510<-data.frame(PAI_510,"mid height (5-10)")
PAI_05<-rbind(Irlanda_PAI_05,Hamburgo_PAI_05)
PAI_05<-data.frame(PAI_05,"understory (0-5)")

total_PAI$Type<-"total"
names(total_PAI)<-c("Area","PAI","Type")

names(PAI_20_plus)<-c("Area","PAI","Type")
names(PAI_1020)<-c("Area","PAI","Type")
names(PAI_510)<-c("Area","PAI","Type")
names(PAI_05)<-c("Area","PAI","Type")

PAI.table1<-rbind(PAI_05,PAI_510,PAI_1020,PAI_20_plus)

library(ggthemes)
g <- ggplot(PAI.table1, aes(x=reorder(Type,-PAI),y=PAI,fill=Area,col=Area))+
  geom_boxplot() +
  scale_fill_manual(values=c("#33CC99","#FF6666"))+
  scale_color_manual(values=c("#33CC99","#FF6666"))+
  labs(x="Canopy height (m)",
       y=bquote('PAI'~(m^2/m^2)))+
         coord_flip()+
  theme_classic()
 plot(g)      
#g + theme(plot.title = element_text(face="bold",size=16))


#Handheld lidar analysis

#PC1
Point_Count_1_NORM<-readLAS("Resources/PCs/PC1_heightNorm.las")
Point_Count_1_NORM@data

Points<-dim(Point_Count_1_LAS@data)
Point_nr<-Points[[1]]
trajectory<-read.table("PC1/PC1/2022-02-11_00-46-53_traj.txt", sep=" ",header=TRUE)
length(trajectory$X.time)
duration<-trajectory[length(trajectory$X.time),1]-trajectory[1,1]
point_density<-Point_nr/duration 

#Canopy height from Handheld lidar

#Z values give height
Z<-Point_Count_1_NORM$Z
Z_top<-subset(Z, Z > quantile(Z, prob = 0.9))
mean(Z_top)# only 6m?
highest<-max(Point_Count_1_NORM$Z)

#automate for every point count
PCs <- list.files("Resources/PCs")
Point_count<-PCs[2] #nr two is nr 10
library(gtools)
PCs<- mixedsort(PCs)


Height_vector <- vector()
for(i in 1:length(PCs)){
  Point_count<-readLAS(paste0("Resources/PCs/",PCs[i]))
  Z<-Point_count$Z
  Z_top<-subset(Z,Z>quantile(Z,prob=0.9))
  Height_vector<-c(Height_vector,mean(Z_top))
}

#Get maximum height values
max_vector<-vector()
for(i in 1:length(PCs)){
  Point_count<-readLAS(paste0("Resources/PCs/",PCs[i]))
  Z<-Point_count$Z
  highest<-max(Z)
  max_vector<-c(max_vector,highest)
}

#Get height for each area
Irlanda_height<-Height_vector[PC_irlanda]
Hamburgo_height<-Height_vector[PC_hamburgo]
Forest_height<-Height_vector[PC_forest]

Irlanda_max<-max_vector[PC_irlanda]
Hamburgo_max<-max_vector[PC_hamburgo]
Forest_max<-max_vector[PC_forest]

mean(Irlanda_height,na.rm=TRUE)
mean(Hamburgo_height,na.rm=TRUE)
mean(Forest_height)

mean(Irlanda_max,na.rm=TRUE)
mean(Hamburgo_max,na.rm=TRUE)
mean(Forest_max)

#Canopy cover from handheld lidar

#subset to points above coffee trees (3+m) and find LAi as cover proxy
cc_handheld<-vector()
for(i in 1:length(PCs)){
  VOXELS_LAD <- lad.voxels(paste0("Resources/PCs/",PCs[i]), grain.size=1)
  lad_profile <- lad.profile(VOXELS_LAD)
  lidar.lai <- lai(lad_profile); lidar.lai
  cc.lai <- lai(lad_profile, min = 3, max = 150); cc.lai
  cc_handheld<-c(cc_handheld,cc.lai)
}

#canopy cover per area
cc_hh_Irlanda<-cc_handheld[PC_irlanda]
cc_hh_Hamburgo<-cc_handheld[PC_hamburgo]
cc_hh_Forest<-cc_handheld[PC_forest]

mean(cc_hh_Irlanda,na.rm=TRUE)
mean(cc_hh_Hamburgo,na.rm=TRUE)
mean(cc_hh_Forest)


#PAI from handheld

VOXELS_LAD <- lad.voxels("Resources/PCs/PC1_heightNorm.las",
                         grain.size = 1)

lad_profile <- lad.profile(VOXELS_LAD)

lidar.lai <- lai(lad_profile); lidar.lai
understory.lai <- lai(lad_profile, min = 0, max = 5); understory.lai
mid.lai<-lai(lad_profile,min=5,max=10);mid.lai
canopy.lai<-lai(lad_profile,min=10,max=20);canopy.lai
high.canopy.lai<-lai(lad_profile,min=20,max=150);high.canopy.lai
PC1_LAI<-data.frame("1",understory.lai,mid.lai,canopy.lai,high.canopy.lai)

#automate for every point count
All_HH_LAI<-vector()
for(i in 1:length(PCs)){
  VOXELS_LAD <- lad.voxels(paste0("Resources/PCs/",PCs[i]), grain.size=1)
  lad_profile <- lad.profile(VOXELS_LAD)
  lidar.lai <- lai(lad_profile); lidar.lai
  understory.lai <- lai(lad_profile, min = 0, max = 5); understory.lai
  mid.lai<-lai(lad_profile,min=5,max=10);mid.lai
  canopy.lai<-lai(lad_profile,min=10,max=20);canopy.lai
  high.canopy.lai<-lai(lad_profile,min=20,max=150);high.canopy.lai
  PC_LAI<-data.frame(i,understory.lai,mid.lai,canopy.lai,high.canopy.lai)
  All_HH_LAI<-rbind(All_HH_LAI,PC_LAI) 
}
All_HH_LAI$Total<-rowSums(All_HH_LAI[,2:5])

#sort LAI by location, use unfiltered PC_ vectors!
# Filter point counts per zone
Irlanda_LAI<-data.frame("Irlanda",All_HH_LAI[PC_irlanda,])
names(Irlanda_LAI)<-c("Area","i","understory.lai","mid.lai","canopy.lai","high.canopy.lai","total.lai")
Hamburgo_LAI<-data.frame("Hamburgo",All_HH_LAI[PC_hamburgo,])
Hamburgo_LAI<-Hamburgo_LAI[-12,]
names(Hamburgo_LAI)<-c("Area","i","understory.lai","mid.lai","canopy.lai","high.canopy.lai","total.lai")
Forest_LAI<-data.frame("Forest",All_HH_LAI[PC_forest,])
names(Forest_LAI)<-c("Area","i","understory.lai","mid.lai","canopy.lai","high.canopy.lai","total.lai")

#Mean total LAI
mean(Irlanda_LAI$total.lai)
mean(Hamburgo_LAI$total.lai)

#Reorder Lai to draw graph
Irlanda_LAI<-Irlanda_LAI[,-2]
total<-data.frame("Irlanda",Irlanda_LAI$total.lai,"Total")
names(total)<-c("Area","PAI","Type")
hc<-data.frame("Irlanda",Irlanda_LAI$high.canopy.lai,"high canopy (20+)")
names(hc)<-c("Area","PAI","Type")
c<-data.frame("Irlanda",Irlanda_LAI$canopy.lai,"canopy (10-20)")
names(c)<-c("Area","PAI","Type")
m<-data.frame("Irlanda",Irlanda_LAI$mid.lai,"mid height (5-10)")
names(m)<-c("Area","PAI","Type")
u<-data.frame("Irlanda",Irlanda_LAI$understory.lai,"understory (0-5)")
names(u)<-c("Area","PAI","Type")
Irlanda_LAI<-rbind(u,m,c,hc,total)


Hamburgo_LAI<-Hamburgo_LAI[,-2]
total<-data.frame("Hamburgo",Hamburgo_LAI$total.lai,"Total")
names(total)<-c("Area","PAI","Type")
hc<-data.frame("Hamburgo",Hamburgo_LAI$high.canopy.lai,"high canopy (20+)")
names(hc)<-c("Area","PAI","Type")
c<-data.frame("Hamburgo",Hamburgo_LAI$canopy.lai,"canopy (10-20)")
names(c)<-c("Area","PAI","Type")
m<-data.frame("Hamburgo",Hamburgo_LAI$mid.lai,"mid height (5-10)")
names(m)<-c("Area","PAI","Type")
u<-data.frame("Hamburgo",Hamburgo_LAI$understory.lai,"understory (0-5)")
names(u)<-c("Area","PAI","Type")
Hamburgo_LAI<-rbind(u,m,c,hc,total)


Forest_LAI<-Forest_LAI[,-2]
total<-data.frame("Forest",Forest_LAI$total.lai,"Total")
names(total)<-c("Area","PAI","Type")
hc<-data.frame("Forest",Forest_LAI$high.canopy.lai,"high canopy (20+)")
names(hc)<-c("Area","PAI","Type")
c<-data.frame("Forest",Forest_LAI$canopy.lai,"canopy (10-20)")
names(c)<-c("Area","PAI","Type")
m<-data.frame("Forest",Forest_LAI$mid.lai,"mid height (5-10)")
names(m)<-c("Area","PAI","Type")
u<-data.frame("Forest",Forest_LAI$understory.lai,"understory (0-5)")
names(u)<-c("Area","PAI","Type")
Forest_LAI<-rbind(u,m,c,hc,total)


#for graph without total values
Hamburgo_LAI<-Hamburgo_LAI[-(45:55),]
Irlanda_LAI<-Irlanda_LAI[-(89:110),]
Forest_LAI<-Forest_LAI[-(25:30),]

#draw boxplot of HH LAI
All_LAI<-rbind(Irlanda_LAI,Hamburgo_LAI,Forest_LAI)

  library(ggthemes)
g <- ggplot(All_LAI, aes(x=reorder(Type,-PAI),y=PAI,fill=Area,col=Area))+
  geom_boxplot() +
  scale_fill_manual(values=c("#FF9933","#33CC99","#FF6666"))+
  scale_color_manual(values=c("#FF9933","#33CC99","#FF6666"))+
  labs(x="Canopy height (m)",
       y=bquote('PAI'~(m^2/m^2)))+
  coord_flip()+
  theme_classic()
plot(g)
#g + theme(plot.title = element_text(face="bold",size=16))



#FHD
lad.voxels(Point_Count_1_NORM, grain.size = 1, k = 1)

#Comparing GEDI vegetation metrics with Handheld metrics








#Species richness

# Filter point counts per zone-remember to filter confidence
pointCounts_irlanda <- pointCounts[pointCounts$Zone == "irlanda",]
pointCounts_forest <- pointCounts[pointCounts$Zone == "restoration" | pointCounts$Zone == "bosque",]
pointCounts_hamburgo <- pointCounts[pointCounts$Zone == "hamburgo",]

# Filter point counts based on confidence value
conf_threshold <- 60
pointCounts <- pointCounts[which(pointCounts$Confidence >= conf_threshold),]
pointCounts_irlanda <- pointCounts_irlanda[which(pointCounts_irlanda$Confidence >= conf_threshold),]
pointCounts_forest <- pointCounts_forest[which(pointCounts_forest$Confidence >= conf_threshold),]
pointCounts_hamburgo <- pointCounts_hamburgo[which(pointCounts_hamburgo$Confidence >= conf_threshold),]

#nr of point counts per zone after confidence filtering(above)
PC_irlanda <- unique(pointCounts_irlanda$ï..Point_count)
PC_irlanda<-PC_irlanda[-23]
PC_forest <- unique(pointCounts_forest$ï..Point_count)
PC_hamburgo <- unique(pointCounts_hamburgo$ï..Point_count)

# Species per zone
species_irlanda <- unique(pointCounts_irlanda$Species)
species_forest <- unique(pointCounts_forest$Species)
species_hamburgo <- unique(pointCounts_hamburgo$Species)

overall_richness<-c(length(species_irlanda), length(species_hamburgo),length(species_forest))
names<-c("Irlanda","Hamburgo","Forest")
overall_richness<-data.frame(names,overall_richness)
#account for effort by dividing by nr of point counts
points<-c(length(PC_irlanda),length(PC_hamburgo),length(PC_forest))
overall_richness$points<-points
overall_richness$richness_by_effort<-overall_richness$overall_richness/overall_richness$points

boxplot(overall_richness~names, data=overall_richness)


#richness for each point count 

#try exporting point counts into excel file and getting richness
write.table(pointCounts_irlanda, file="pointcounts_irlanda.csv", sep=",", row.names = F)
write.table(pointCounts_hamburgo,file="pointcounts_hamburgo.csv",sep=",",row.names = F)
write.table(pointCounts_forest,file="pointcounts_forest.csv",sep=",",row.names=F)

Irlanda_richness<-read.delim("Resources/Irlanda_richness.csv",sep=",")
Irlanda_richness$Area<-"Irlanda"
names(Irlanda_richness)<-c("Point_count","Richness","Area")
Hamburgo_richness<-read.delim("Resources/Hamburgo_richness.csv",sep=",")
Hamburgo_richness$Area<-"Hamburgo"
names(Hamburgo_richness)<-c("Point_count","Richness","Area")
Forest_richness<-read.delim("Resources/Forest_richness.csv",sep=",")
Forest_richness$Area<-"Forest"
names(Forest_richness)<-c("Point_count","Richness","Area")

All_HH_richness<-rbind(Irlanda_richness,Hamburgo_richness,Forest_richness)

boxplot(Richness~Area,data=All_HH_richness)

#adding richness from audiomoth data
PCs <- list.files("Resources/audiomoths")
PCs<- mixedsort(PCs)
audiomoth_detections <- vector()
for(i in 1:length(PCs)){
  PC_path <- paste0("Resources/audiomoths/", PCs[i])
  audiomoth_det <- vector()
  for(j in 1:length(list.files(PC_path))){
    f = read.csv(paste(PC_path, list.files(PC_path)[j], sep="/"), sep=";")
    audiomoth_det <- rbind(audiomoth_det, f)
  }
  audiomoth_det <- cbind(rep(as.numeric(substr(PCs[i], 3, nchar(PCs[i]))), dim(audiomoth_det)[1]), audiomoth_det)
  audiomoth_detections <- rbind(audiomoth_detections, audiomoth_det)
}
colnames(audiomoth_detections)[1] <- "point_count"

write.csv(audiomoth_detections, "results/audiomoth_detections.csv")

## Filter detections per zone
#audiomoth_detections_filtered_irlanda <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count %in% PC_irlanda,]
#audiomoth_detections_filtered_forest <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count %in% PC_forest,]
#audiomoth_detections_filtered_hamburgo <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count %in% PC_hamburgo,]

# Filter the results of BirdNET detection above a threshold for the confidence value

confidence_threshold = 0.75
audiomoth_detections_filtered <- audiomoth_detections[which(audiomoth_detections$Confidence >= confidence_threshold),]

#Filter detections per point count
audiomoth_detections_filtered_1 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==1,]
unique(audiomoth_detections_filtered_1$Common.name)

audiomoth_detections_filtered_2 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==2,]
unique(audiomoth_detections_filtered_2$Common.name)

audiomoth_detections_filtered_3 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==3,]
unique(audiomoth_detections_filtered_3$Common.name)

audiomoth_detections_filtered_4 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==4,]
unique(audiomoth_detections_filtered_4$Common.name)

audiomoth_detections_filtered_5 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==5,]
unique(audiomoth_detections_filtered_5$Common.name)

audiomoth_detections_filtered_6 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==6,]
unique(audiomoth_detections_filtered_6$Common.name)

audiomoth_detections_filtered_7 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==7,]
unique(audiomoth_detections_filtered_7$Common.name)

audiomoth_detections_filtered_8 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==8,]
unique(audiomoth_detections_filtered_8$Common.name)

audiomoth_detections_filtered_9 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==9,]
unique(audiomoth_detections_filtered_9$Common.name)

audiomoth_detections_filtered_10 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==10,]
unique(audiomoth_detections_filtered_10$Common.name)

audiomoth_detections_filtered_11 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==11,]
unique(audiomoth_detections_filtered_11$Common.name)

audiomoth_detections_filtered_12 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==12,]
unique(audiomoth_detections_filtered_12$Common.name)

audiomoth_detections_filtered_13 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==13,]
unique(audiomoth_detections_filtered_13$Common.name)

audiomoth_detections_filtered_14 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==14,]
unique(audiomoth_detections_filtered_14$Common.name)

audiomoth_detections_filtered_15 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==15,]
unique(audiomoth_detections_filtered_15$Common.name)

audiomoth_detections_filtered_16 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==16,]
unique(audiomoth_detections_filtered_16$Common.name)

audiomoth_detections_filtered_17 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==17,]
unique(audiomoth_detections_filtered_17$Common.name)

audiomoth_detections_filtered_18 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==18,]
unique(audiomoth_detections_filtered_18$Common.name)

audiomoth_detections_filtered_27 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==27,]
unique(audiomoth_detections_filtered_27$Common.name)

audiomoth_detections_filtered_28 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==28,]
unique(audiomoth_detections_filtered_28$Common.name)

audiomoth_detections_filtered_29 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==29,]
unique(audiomoth_detections_filtered_29$Common.name)

audiomoth_detections_filtered_30 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==30,]
unique(audiomoth_detections_filtered_30$Common.name)

audiomoth_detections_filtered_31 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==31,]
unique(audiomoth_detections_filtered_31$Common.name)

audiomoth_detections_filtered_32 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==32,]
unique(audiomoth_detections_filtered_32$Common.name)

audiomoth_detections_filtered_33 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==33,]
unique(audiomoth_detections_filtered_33$Common.name)

audiomoth_detections_filtered_34 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==34,]
unique(audiomoth_detections_filtered_34$Common.name)

audiomoth_detections_filtered_35 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==35,]
unique(audiomoth_detections_filtered_35$Common.name)

audiomoth_detections_filtered_36 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==36,]
unique(audiomoth_detections_filtered_36$Common.name)

audiomoth_detections_filtered_38 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==38,]
unique(audiomoth_detections_filtered_38$Common.name)

audiomoth_detections_filtered_40 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==40,]
unique(audiomoth_detections_filtered_40$Common.name)

audiomoth_detections_filtered_41 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==41,]
unique(audiomoth_detections_filtered_41$Common.name)

audiomoth_detections_filtered_42 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==42,]
unique(audiomoth_detections_filtered_42$Common.name)

audiomoth_detections_filtered_43 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==43,]
unique(audiomoth_detections_filtered_43$Common.name)

audiomoth_detections_filtered_45 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==45,]
unique(audiomoth_detections_filtered_45$Common.name)

audiomoth_detections_filtered_19 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==19,]
unique(audiomoth_detections_filtered_19$Common.name)

audiomoth_detections_filtered_20 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==20,]
unique(audiomoth_detections_filtered_20$Common.name)

audiomoth_detections_filtered_21 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==21,]
unique(audiomoth_detections_filtered_21$Common.name)

audiomoth_detections_filtered_22 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==22,]
unique(audiomoth_detections_filtered_22$Common.name)

audiomoth_detections_filtered_23 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==23,]
unique(audiomoth_detections_filtered_23$Common.name)

audiomoth_detections_filtered_24 <- audiomoth_detections_filtered[audiomoth_detections_filtered$point_count==24,]
unique(audiomoth_detections_filtered_24$Common.name)

#read in combined richness table
Irlanda_richness<-read.delim("Resources/Irlanda_richness.csv",sep=",")
names(Irlanda_richness)<-c("Point_count","Richness","Audiomoth","Overall_richness","Area")
Hamburgo_richness<-read.delim("Resources/Hamburgo_richness.csv",sep=",")
names(Hamburgo_richness)<-c("Point_count","Richness","Audiomoth","Overall_richness","Area")
Forest_richness<-read.delim("Resources/Forest_richness.csv",sep=",")
names(Forest_richness)<-c("Point_count","Richness","Audiomoth","Overall_richness","Area")
All_HH_richness<-rbind(Irlanda_richness,Hamburgo_richness,Forest_richness)


#combine richness, canopy cover, height in a table, then do relationship..?
vector<-c(1:45)
vector<-vector[-37]
vector<-vector[-38]
max_frame<-data.frame(vector,max_vector)
mean_frame<-data.frame(vector,Height_vector)
cc_handheld2<-data.frame(vector,cc_handheld)
Total_LAI<-data.frame(vector,All_HH_LAI$Total)

#rearrange richness by point count
All_HH_richness<-All_HH_richness%>%arrange(Point_count)
All_HH_richness<-All_HH_richness[-41,]
richness<-All_HH_richness$Overall_richness
vector2<-vector[-26]
vector2<-vector2[-25]
vector2<-vector2[-40]
richness<-data.frame(vector2,richness)
names(richness)<-c("vector", "richness")

richness_vegindex<-merge(max_frame,mean_frame)
richness_vegindex<-merge(richness_vegindex, cc_handheld2)
richness_vegindex<-merge(richness_vegindex,Total_LAI)
richness_vegindex<-merge(richness_vegindex, richness)
names(richness_vegindex)<-c("point_count","max_canopy_height","mean_canopy_height","canopy_cover","total_LAI","richness")
plot(richness~max_canopy_height, data=richness_vegindex)

g<-ggplot(richness_vegindex,aes(x=richness,y=max_canopy_height))+
  geom_point()+
  theme_classic()+
  labs(title="Species richness vs canopy height", 
       y="Canopy height (m)", 
       x="Species richness",
       caption="Source: Handheld lidar data")
g+theme(plot.title = element_text(face="bold",size=16))

#absolutely no correlation

g<-ggplot(richness_vegindex,aes(x=richness,y=mean_canopy_height))+
  geom_point()+
  theme_classic()+
  labs(title="Species richness vs canopy height", 
                      y="Canopy height (m)", 
                      x="Species richness",
                      caption="Source: Handheld lidar data")
g+theme(plot.title = element_text(face="bold",size=16))

g<-ggplot(richness_vegindex,aes(x=richness,y=canopy_cover))+
  geom_point()+
  theme_classic()+
  labs(title="Species richness vs canopy cover", 
                      y=bquote('Canopy cover'~(m^2/m^2)), 
                      x="Species richness",
                      caption="Source: Handheld lidar data")
g+theme(plot.title = element_text(face="bold",size=16))

g<-ggplot(richness_vegindex,aes(x=richness,y=total_LAI))+
geom_point()+
  theme_classic()+
  labs(title="Species richness vs total PAI", 
                      y=bquote('PAI'~(m^2/m^2)), 
                      x="Species richness",
                      caption="Source: Handheld lidar data")
g+theme(plot.title = element_text(face="bold",size=16))

m4<-lm(richness_vegindex$richness~richness_vegindex$total_LAI)
summary(m4)

#richness GLMs
richness_vegindex$Area<-"Irlanda"
richness_vegindex$Area[29:40]<-"Hamburgo"
richness_vegindex$Area[19:24]<-"Forest"
m<-glm(richness~max_canopy_height+mean_canopy_height+canopy_cover+total_LAI+Area,family=poisson,data=richness_vegindex)
summary(m)

#richness mixed effects model
library(lme4)

m2<-lmer(richness~max_canopy_height+mean_canopy_height+canopy_cover+total_LAI+(1|Area), data=richness_vegindex)
summary(m2)#no effects explained by area???

#Shannon index
Irlanda_species_table<-read.csv("Resources/Irlanda_species.csv")
Irlanda_Shannon<-Irlanda_species_table[41,3]
Irlanda_Shannon


Hamburgo_species_table<-read.csv("Resources/Hamburgo_species.csv")
Hamburgo_Shannon<-Hamburgo_species_table[16,3]
Hamburgo_Shannon


Forest_species_table<-read.csv("Resources/Forest_species.csv")
Forest_Shannon<-Forest_species_table[19,4]
Forest_Shannon

#effective species number,The number of species required to give H' if all species were equally abundant
ESN_Irlanda<-exp(Irlanda_Shannon)
ESN_Hamburgo<-exp(Hamburgo_Shannon)
ESN_Forest<-exp(Forest_Shannon)

#Simpson index
#shows probability of getting two different species when choosing two individuals at random

Irlanda_Simpson<-Irlanda_species_table[42,5]
Irlanda_Simpson

Hamburgo_Simpson<-Hamburgo_species_table[17,5]
Hamburgo_Simpson

Forest_Simpson<-Forest_species_table[20,5]
Forest_Simpson

#draw plot of all diversity indices against area
#make table of all indices
install.packages("fmsb")
library(fmsb)

Diversity_indices <- data.frame(
  row.names = c("Irlanda", "Hamburgo", "Forest"),
  Richness = overall_richness$overall_richness,
  Shannon_index = c(Irlanda_Shannon, Hamburgo_Shannon, Forest_Shannon),
  Simpson_index = c(Irlanda_Simpson, Hamburgo_Simpson, Forest_Simpson),
  Effective_species_number = c(ESN_Irlanda, ESN_Hamburgo, ESN_Forest))

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  Richness=c(40,0), Shannon_index = c(3.7, 0), Simpson_index = c(1, 0), Effective_species_number = c(30, 0)
)
rownames(max_min) <- c("Max", "Min")

df<-rbind(max_min,Diversity_indices)

Irlanda_data <- df[c("Max", "Min", "Irlanda"), ]
op <- par(mar = c(1, 1, 1, 1))
radarchart(Irlanda_data, axistype = 1,color = "#00AFBB", 
           vlabels = colnames(data), vlcex = 1,
           caxislabels = NULL, title = NULL,
           # Customize the polygon
           pcol = "green", pfcol = scales::alpha("green", 0.2), plwd = 2, plty = 1,
           # Customize the grid
           cglcol = "grey", cglty = 1, cglwd = 0.8,
           # Customize the axis
           axislabcol = "grey",
)
#chart for all areas
radarchart(df, axistype = 2,color = c("#00AFBB","#E7B800", "#FC4E07"), 
           vlabels = colnames(data), vlcex = 1,
           caxislabels = NULL, title = NULL,
           # Customize the polygon
           pcol = c("green","red","blue"), pfcol = scales::alpha(c("green","red","blue"),0.2), plwd = 2, plty = 1,
           # Customize the grid
           cglcol = "grey", cglty = 1, cglwd = 0.8,
           # Customize the axis
           axislabcol = "grey",
)
legend(
x="topright", legend = rownames(df[-c(1,2),]), title="Area",
pch = 20 , col = c("green", "red", "blue"),
text.col = "black", cex = 1, pt.cex = 2)

#Beta diversity and Jaccard index

install.packages("betapart")
install.packages("vegan")
library(betapart)
library(vegan)
All_HH_richness$overall_richness
#need table of species in each point count and their presence/absence
Forest_presab<-read.csv("Resources/Forest_presab.csv")
Hamburgo_presab<-read.csv("Resources/Hamburgo_presab.csv")
Irlanda_presab<-read.csv("Resources/Irlanda_presab.csv")
All_presab<-read.csv("Resources/All_presab.csv")
row_labels<-All_presab$Row.Labels
All_presab<-All_presab[,-1]
All_presab<-as.data.frame(All_presab)

All_presab$Area<-"Irlanda"
All_presab[29:40,53]<-"Hamburgo"
All_presab[19:24,53]<-"Forest"
All_presab<-arrange(All_presab,Area)
All_presab<-All_presab[,-1]
All_presab<-All_presab[,-52]
groups <- factor(c(rep(1,6), rep(2,12), rep(3,22)),labels = c("Forest","Hamburgo","Irlanda"))

dist<-beta.pair(All_presab, index.family="jaccard")
bd<-betadisper(dist[[3]],groups)
plot(bd)
legend(
  x="topright", legend = rownames(df[-c(1,2),]), title="Area",
  pch = 20 , col = c("green", "red", "blue"),
  text.col = "black", cex = 1, pt.cex = 2)

boxplot(bd)
anova(bd)#areas do not differ significantly in relation to how communities vary from each other


#plot our visual estimates and compare to real values
Irlanda_obs_cov<-pointCounts_irlanda$Canopy_cover #filtered by conf value
Hamburgo_obs_cov<-pointCounts_hamburgo$Canopy_cover
a<-data.frame("Irlanda",Irlanda_obs_cov)
names(a)<-c("Area","Canopy_cover")
b<-data.frame("Hamburgo",Hamburgo_obs_cov)
names(b)<-c("Area","Canopy_cover")
c<-rbind(a,b)

boxplot(c$Observed_cover~c$Area)


Irlanda_cover<-data.frame("Irlanda",Canopy_cov_Irlanda)
names(Irlanda_cover)<-c("Area","Canopy_cover")

Hamburgo_cover<-data.frame("Hamburgo",Canopy_cov_Hamburgo)
names(Hamburgo_cover)<-c("Area","Canopy_cover")
Canopy_cov_table<-rbind(Irlanda_cover,Hamburgo_cover)

boxplot(Canopy_cov_table$Canopy_cover~Canopy_cov_table$Area)

mean(Irlanda_obs_cov)
mean(Hamburgo_obs_cov)

mean(Canopy_cov_Irlanda)
mean(Canopy_cov_Hamburgo)

#we underestimated cover of both areas? 
Canopy_cov_table$Type<-"GEDI"
c$Type<-"Observed"
all.cover<-rbind(c,Canopy_cov_table)

cov <- ggplot(all.cover, aes(x=Type,y=Canopy_cover,fill=Area,col=Area))+
  geom_boxplot() +
 theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Visually estimated vs measured canopy cover",
      x="Type",
     y="Canopy cover")
plot(cov)
