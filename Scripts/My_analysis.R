# Loading necessary packages:

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

## Creating polygons of Irlanda, Hamburgo and Forest ##

# Creating a matrix of longitude and latitude of occurrences for each place

# Filter point counts per zone
pointCounts <- read.csv("Resources/Point_counts.csv")
pointCounts_irlanda <- pointCounts[pointCounts$Zone == "irlanda",]
pointCounts_forest <- pointCounts[pointCounts$Zone == "restoration" | pointCounts$Zone == "bosque",]
pointCounts_hamburgo <- pointCounts[pointCounts$Zone == "hamburgo",]

# Number of point counts per zone without species detection confidence filtering
PC_irlanda <- unique(pointCounts_irlanda$ï..Point_count)
PC_irlanda<-PC_irlanda[-(23:24)]
PC_forest <- unique(pointCounts_forest$ï..Point_count)
PC_hamburgo <- unique(pointCounts_hamburgo$ï..Point_count)

# Extract latitude and longitude for each zone
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

# Creating polygons

Irlanda_polygon<-chull(IrlandaCoords2)
Hamburgo_polygon<-chull(HamburgoCoords2)
Forest_polygon<-chull(ForestCoords2)

# Drawing Forest polygon
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

srtm2 <- raster(srtm)

pointCounts <- pointCounts[pointCounts$ï..Point_count<= 45,]
pointCounts_sf <- st_as_sf(pointCounts, coords = c("Lon", "Lat"), crs = crs(srtm))

# Plot polygon on map
plot(srtm, xlim=c(-92.36, -92.32), ylim=c(15.15,15.19))
plot(pointCounts_sf["Zone"],add=TRUE)
plot(Forestpolygon_final, add=TRUE)

# Drawing Hamburgo polygon

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

# Drawing Irlanda polygon

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


pdf(file="map_coffeefarms.pdf", width=6, height=6)
plot(srtm2, xlim=c(-92.36, -92.32), ylim=c(15.15,15.19),
     axes=F, col=colorRampPalette(c("green", "yellow", "brown4"))(100))

plot(Hamburgopolygon_final, add=TRUE, border="green", lwd=3)
plot(Forestpolygon_final, add=TRUE, border="orange", lwd=3)
plot(Irlandapolygon_final, add=TRUE, border="red", lwd=3)
plot(pointCounts_sf["Zone"],add=TRUE,
    pch = 19, col="black", cex=0.5)
legend("topleft", legend = c("Irlanda", "Hamburgo","Forest"),
       col = c("red", "green","orange"),lty=1:1,box.lty=0, lwd=3)
terra::sbar(1, xy=click(), divs=2, cex=1, lwd=1, label=c(0,"km",1))

dev.off()



## GEDI data analysis ##

# Reading GEDI data

gedilevel2b <- readLevel2B(level2Bpath = paste0("Resources/lidar/", list.files("Resources/lidar/"))[1])
level2BVPM<-getLevel2BVPM(gedilevel2b)
head(level2BVPM[,c("beam","shot_number","pai","fhd_normal","omega","pgap_theta","cover")])
level2BVPM$shot_number<-paste0(level2BVPM$shot_number)
toremove <- which(is.na(level2BVPM$longitude_bin0) == T)
if(length(toremove > 0)){
  level2BVPM <- level2BVPM[-toremove,]
}
level2BVPM_spdf<-SpatialPointsDataFrame(cbind(level2BVPM$longitude_bin0,level2BVPM$latitude_bin0),data=level2BVPM)
gedi_irlanda <- level2BVPM_spdf[which(level2BVPM_spdf$latitude_bin0 > 15.15 & level2BVPM_spdf$latitude_bin0 < 15.19 & level2BVPM_spdf$longitude_bin0 >= -92.36 & level2BVPM_spdf$longitude_bin0 <= -92.32),]

for(i in 2:length(list.files("Resources/lidar/"))){
  gedilevel2b <- readLevel2B(level2Bpath = paste0("Resources/lidar/", list.files("Resources/lidar/"))[i])
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

# Find which GEDI points fall within each polygon and extract those
gedi_irlanda2<-as.data.frame(gedi_irlanda)
gedi_irlanda_coords<-dplyr::select(gedi_irlanda2, coords.x2, coords.x1)
gedi_irlanda_coords2<-st_as_sf(gedi_irlanda_coords, coords = c("coords.x1", "coords.x2"), crs = 4326)

Irlanda_gedi<-st_contains(Irlandapolygon_final,gedi_irlanda_coords2)
Hamburgo_gedi<-st_contains(Hamburgopolygon_final, gedi_irlanda_coords2)
Forest_gedi<-st_contains(Forestpolygon_final,gedi_irlanda_coords2) #too few points, discounted in analysis

# Find metrics for those gedi points

# Get canopy height of GEDI points for each location

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


mean(Canopy_h_Irlanda)
sd(Canopy_h_Irlanda)
mean(Canopy_h_Hamburgo)
sd(Canopy_h_Hamburgo)


Irlanda_height<-data.frame("Irlanda",Canopy_h_Irlanda)
names(Irlanda_height)<-c("Area","Canopy_height")

Hamburgo_height<-data.frame("Hamburgo",Canopy_h_Hamburgo)
names(Hamburgo_height)<-c("Area","Canopy_height")

Canopy_h_table<-rbind(Irlanda_height,Hamburgo_height)
boxplot(Canopy_height~Area,data=Canopy_h_table)

#Get canopy cover of GEDI points for each location

Canopy_cov_Irlanda<-Irlanda_metrics$cover
Canopy_cov_Hamburgo<-Hamburgo_metrics$cover

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
sd(Canopy_cov_Irlanda)
mean(Canopy_cov_Hamburgo)
sd(Canopy_cov_Hamburgo)

Irlanda_cover<-data.frame("Irlanda",Canopy_cov_Irlanda)
names(Irlanda_cover)<-c("Area","Canopy_cover")

Hamburgo_cover<-data.frame("Hamburgo",Canopy_cov_Hamburgo)
names(Hamburgo_cover)<-c("Area","Canopy_cover")


Canopy_cov_table<-rbind(Irlanda_cover,Hamburgo_cover)
boxplot(Canopy_cover~Area,data=Canopy_cov_table)

# Get structural diversity of GEDI points for each location


# Getting PAI data

gedilevel2b <- readLevel2B(level2Bpath = paste0("Resources/lidar/", list.files("Resources/lidar/"))[1])
level2BPAI<-getLevel2BPAIProfile(gedilevel2b)

level2BPAI$shot_number<-paste0(level2BPAI$shot_number)
toremove <- which(is.na(level2BPAI$lon_lowestmode) == T)
if(length(toremove > 0)){
  level2BPAI <- level2BPAI[-toremove,]
}
level2BPAI_spdf<-SpatialPointsDataFrame(cbind(level2BPAI$lon_lowestmode,level2BPAI$lat_lowestmode),data=level2BPAI)
all_PAI<- level2BPAI_spdf[which(level2BPAI_spdf$lat_lowestmode > 15.15 & level2BPAI_spdf$lat_lowestmode < 15.19 & level2BPAI_spdf$lon_lowestmode >= -92.36 & level2BPAI_spdf$lon_lowestmode <= -92.32),]


for(i in 2:length(list.files("Resources/lidar/"))){
  gedilevel2b <- readLevel2B(level2Bpath = paste0("Resources/lidar/", list.files("Resources/lidar/"))[i])
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

# Getting PAI values for each area

all_PAI3<-all_PAI2[,12:41]
Gedi_layers_Irlanda<-all_PAI3[Irlanda_gedi[[1]], ]
Gedi_layers_Hamburgo<-all_PAI3[Hamburgo_gedi[[1]], ]

# Total PAI per area
Gedi_layers_Irlanda$total <- rowSums(Gedi_layers_Irlanda[1:30])
Gedi_layers_Hamburgo$total <- rowSums(Gedi_layers_Hamburgo[1:30])

Irlanda_PAI<-data.frame("Irlanda",Gedi_layers_Irlanda$total)
names(Irlanda_PAI)<-c("Area","Total_PAI")
Hamburgo_PAI<-data.frame("Hamburgo",Gedi_layers_Hamburgo$total)
names(Hamburgo_PAI)<-c("Area","Total_PAI")
total_PAI<-rbind(Irlanda_PAI,Hamburgo_PAI)
boxplot(Total_PAI~Area,data=total_PAI)

# Understory or coffee PAI per area 0-5m
Irlanda_PAI_05<-data.frame("Irlanda",Gedi_layers_Irlanda$pai_z0_5m)
names(Irlanda_PAI_05)<-c("Area","Understory_PAI")
Hamburgo_PAI_05<-data.frame("Hamburgo",Gedi_layers_Hamburgo$pai_z0_5m)
names(Hamburgo_PAI_05)<-c("Area","Understory_PAI")
Understory_PAI<-rbind(Irlanda_PAI_05,Hamburgo_PAI_05)
boxplot(Understory_PAI~Area,data=Understory_PAI)

# Mid PAI 5-10 m
Irlanda_PAI_510<-data.frame("Irlanda",Gedi_layers_Irlanda$pai_z5_10m)
names(Irlanda_PAI_510)<-c("Area","Mid_PAI")
Hamburgo_PAI_510<-data.frame("Hamburgo",Gedi_layers_Hamburgo$pai_z5_10m)
names(Hamburgo_PAI_510)<-c("Area","Mid_PAI")
Mid_PAI<-rbind(Irlanda_PAI_510,Hamburgo_PAI_510)
boxplot(Mid_PAI~Area,data=Mid_PAI)

# Canopy PAI 10-20m
Gedi_layers_Irlanda$canopy <- rowSums(Gedi_layers_Irlanda[3:4])
Gedi_layers_Hamburgo$canopy <- rowSums(Gedi_layers_Hamburgo[3:4])

Irlanda_PAI_1020<-data.frame("Irlanda",Gedi_layers_Irlanda$canopy)
names(Irlanda_PAI_1020)<-c("Area","Canopy_PAI")

Hamburgo_PAI_1020<-data.frame("Hamburgo",Gedi_layers_Hamburgo$canopy)
names(Hamburgo_PAI_1020)<-c("Area","Canopy_PAI")

Canopy_PAI<-rbind(Irlanda_PAI_1020,Hamburgo_PAI_1020)
boxplot(Canopy_PAI~Area,data=Canopy_PAI)

# High canopy PAI 20+m
Gedi_layers_Irlanda$high_canopy <- rowSums(Gedi_layers_Irlanda[5:30])
Gedi_layers_Hamburgo$high_canopy <- rowSums(Gedi_layers_Hamburgo[5:30])

Irlanda_PAI_20plus<-data.frame("Irlanda",Gedi_layers_Irlanda$high_canopy)
names(Irlanda_PAI_20plus)<-c("Area","High_canopy_PAI")

Hamburgo_PAI_20plus<-data.frame("Hamburgo",Gedi_layers_Hamburgo$high_canopy)
names(Hamburgo_PAI_20plus)<-c("Area","High_canopy_PAI")

High_canopy_PAI<-rbind(Irlanda_PAI_20plus,Hamburgo_PAI_20plus)
boxplot(High_canopy_PAI~Area,data=High_canopy_PAI)


mean(Gedi_layers_Irlanda$total)
sd(Gedi_layers_Irlanda$total)
mean(Gedi_layers_Hamburgo$total)
sd(Gedi_layers_Hamburgo$total)

# Draw plot of canopy height against canopy cover for each area for GEDI

options(scipen=999) #get rid of scientific notation


I<-data.frame("Irlanda",Canopy_cov_Irlanda,Canopy_h_Irlanda)
names(I)<-c("Area","Canopy_cover","Canopy_height")

H<-data.frame("Hamburgo",Canopy_cov_Hamburgo,Canopy_h_Hamburgo)
names(H)<-c("Area","Canopy_cover","Canopy_height")

df<-rbind(I,H)
write.csv(df,"canopycov_height_GEDI.csv")

gg <- ggplot(df, aes(x=Canopy_cover, y=Canopy_height)) + 
  geom_point(aes(col=Area)) +
  scale_color_manual(values=c("#33CC99","#FF6666"))+
  labs(y="Canopy height (m)", 
       x="Canopy cover (%)")+
  theme_classic()+
  theme(text=element_text(size=20))
plot(gg)

# Plot height to PAI

# PAI table

PAI_20_plus<-rbind(Irlanda_PAI_20plus,Hamburgo_PAI_20plus)
PAI_20_plus<-data.frame(PAI_20_plus,"high canopy \n (20+)")
PAI_1020<-rbind(Irlanda_PAI_1020,Hamburgo_PAI_1020)
PAI_1020<-data.frame(PAI_1020,"canopy \n (10-20)")
PAI_510<-rbind(Irlanda_PAI_510,Hamburgo_PAI_510)
PAI_510<-data.frame(PAI_510,"mid height \n (5-10)")
PAI_05<-rbind(Irlanda_PAI_05,Hamburgo_PAI_05)
PAI_05<-data.frame(PAI_05,"understory \n (0-5)")

total_PAI$Type<-"total"
names(total_PAI)<-c("Area","PAI","Type")

names(PAI_20_plus)<-c("Area","PAI","Type")
names(PAI_1020)<-c("Area","PAI","Type")
names(PAI_510)<-c("Area","PAI","Type")
names(PAI_05)<-c("Area","PAI","Type")

PAI.table1<-rbind(PAI_05,PAI_510,PAI_1020,PAI_20_plus)
write.csv(PAI.table1,"PAI.table1.csv")

library(ggthemes)
g <- ggplot(PAI.table1, aes(x=reorder(Type,-PAI),y=PAI,fill=Area,col=Area))+
  geom_boxplot() +
  scale_fill_manual(values=c("#33CC99","#FF6666"))+
  scale_color_manual(values=c("#33CC99","#FF6666"))+
  labs(x="Canopy height (m)",
       y=bquote('PAI'~(m^2/m^2)))+
         coord_flip()+
  theme_classic()+
  theme(text=element_text(size=20))
 plot(g)      




 ## Handheld lidar analysis ##
 
 
 PCs <- list.files("Resources/PCs")
 library(gtools)
 PCs<- mixedsort(PCs)
 
 # Remove ground points
 for(i in 1:length(PCs)){
   Point_count<-readLAS(paste0("Resources/PCs/PC",i,"_heightNorm.las"))
   Point_count <- Point_count[which(Point_count@data$Classification != 2)]
   writeLAS(Point_count, paste0("Resources/PCs/PC",i,"_heightNorm_noGround.las"))
 }
 
 # Canopy height
 canopy_height_mean90 <- canopy_height_99 <- canopy_height_max <- vector()
 for(i in 1:length(PCs)){
   Point_count<-readLAS(paste0("Resources/PCs/PC",i,"_heightNorm_noGround.las"))
   Z<-Point_count$Z
   canopy_height_mean90[i] <- mean(subset(Z,Z>quantile(Z,prob=0.9)))
   canopy_height_99[i] <- quantile(Z,prob=0.99)
   canopy_height_max[i] <- max(Z)
 }
 
 # Canopy cover and PAI
 # Subset to points above coffee trees (3+m) and find LAI as cover proxy
 cc_handheld <- All_HH_LAI <-vector()
 for(i in 1:length(PCs)){
   VOXELS_LAD <- lad.voxels(paste0("Resources/PCs/PC",i,"_heightNorm_noGround.las"), grain.size=1)
   lad_profile <- lad.profile(VOXELS_LAD)
   lidar.lai <- lai(lad_profile)
   cc_handheld[i] <- lai(lad_profile, min = 3, max = 150)
   understory.lai <- lai(lad_profile, min = 0, max = 5)
   mid.lai<-lai(lad_profile,min=5,max=10)
   canopy.lai<-lai(lad_profile,min=10,max=20)
   high.canopy.lai<-lai(lad_profile,min=20,max=150)
   All_HH_LAI<-rbind(All_HH_LAI, c(understory.lai,mid.lai,canopy.lai,high.canopy.lai)) 
 }
 
 vegetation_metrics <- as.data.frame(cbind(1:45, canopy_height_mean90, canopy_height_99, canopy_height_max, cc_handheld, All_HH_LAI))
 colnames(vegetation_metrics) <- c("point_count_ID", "canopy_height_mean90", "canopy_height_99", "canopy_height_max", "canopy_cover", "PAI_understory", "PAI_mid", "PAI_canopy", "PAI_high_canopy")
 write.csv(vegetation_metrics, "results/vegatation_metrics_PC.csv")

# Get height for each area, use unfiltered PC_ vectors!
Irlanda_height<-vegetation_metrics$canopy_height_mean90[PC_irlanda]
Hamburgo_height<-vegetation_metrics$canopy_height_mean90[PC_hamburgo]
Forest_height<-vegetation_metrics$canopy_height_mean90[PC_forest]

Irlanda_max<-vegetation_metrics$canopy_height_max[PC_irlanda]
Hamburgo_max<-vegetation_metrics$canopy_height_max[PC_hamburgo]
Forest_max<-vegetation_metrics$canopy_height_max[PC_forest]

mean(Irlanda_height,na.rm=TRUE)
sd(Irlanda_height)
mean(Hamburgo_height,na.rm=TRUE)
sd(Hamburgo_height)
mean(Forest_height)

mean(Irlanda_max,na.rm=TRUE)
sd(Irlanda_max)
mean(Hamburgo_max,na.rm=TRUE)
sd(Hamburgo_max)
mean(Forest_max)

# Canopy cover per area
cc_hh_Irlanda<-vegetation_metrics$canopy_cover[PC_irlanda]
cc_hh_Hamburgo<-vegetation_metrics$canopy_cover[PC_hamburgo]
cc_hh_Forest<-vegetation_metrics$canopy_cover[PC_forest]
write.csv(vegetation_metrics,"vegetation_metrics2.csv")

mean(cc_hh_Irlanda,na.rm=TRUE)
sd(cc_hh_Irlanda)
mean(cc_hh_Hamburgo,na.rm=TRUE)
sd(cc_hh_Hamburgo)
mean(cc_hh_Forest)

vegetation_metrics$total_PAI<-rowSums(vegetation_metrics[,6:9])
vegetation_metrics$Area<-"Irlanda"
vegetation_metrics[31:45,11]<-"Hamburgo"
vegetation_metrics[19:24,11]<-"Forest"

All_HH_LAI<-vegetation_metrics[,6:11]
write.csv(All_HH_LAI,"All_HH_LAI.csv")

# LAI per each area 
# Filter point counts per zone
Irlanda_LAI<-data.frame(All_HH_LAI[PC_irlanda,])
names(Irlanda_LAI)<-c("understory.lai","mid.lai","canopy.lai","high.canopy.lai","total.lai","Area")
Hamburgo_LAI<-data.frame(All_HH_LAI[PC_hamburgo,])
names(Hamburgo_LAI)<-c("understory.lai","mid.lai","canopy.lai","high.canopy.lai","total.lai","Area")
Forest_LAI<-data.frame(All_HH_LAI[PC_forest,])
names(Forest_LAI)<-c("understory.lai","mid.lai","canopy.lai","high.canopy.lai","total.lai","Area")

# Mean total LAI
mean(Irlanda_LAI$total.lai)
sd(Irlanda_LAI$total.lai)
mean(Hamburgo_LAI$total.lai)
sd(Hamburgo_LAI$total.lai)

# Reorder LAI to draw graph
total<-data.frame("Irlanda",Irlanda_LAI$total.lai,"Total")
names(total)<-c("Area","PAI","Type")
hc<-data.frame("Irlanda",Irlanda_LAI$high.canopy.lai,"high canopy \n (20+)")
names(hc)<-c("Area","PAI","Type")
c<-data.frame("Irlanda",Irlanda_LAI$canopy.lai,"canopy \n (10-20)")
names(c)<-c("Area","PAI","Type")
m<-data.frame("Irlanda",Irlanda_LAI$mid.lai,"mid height \n (5-10)")
names(m)<-c("Area","PAI","Type")
u<-data.frame("Irlanda",Irlanda_LAI$understory.lai,"understory \n (0-5)")
names(u)<-c("Area","PAI","Type")
Irlanda_LAI<-rbind(u,m,c,hc,total)


total<-data.frame("Hamburgo",Hamburgo_LAI$total.lai,"Total")
names(total)<-c("Area","PAI","Type")
hc<-data.frame("Hamburgo",Hamburgo_LAI$high.canopy.lai,"high canopy \n (20+)")
names(hc)<-c("Area","PAI","Type")
c<-data.frame("Hamburgo",Hamburgo_LAI$canopy.lai,"canopy \n (10-20)")
names(c)<-c("Area","PAI","Type")
m<-data.frame("Hamburgo",Hamburgo_LAI$mid.lai,"mid height \n (5-10)")
names(m)<-c("Area","PAI","Type")
u<-data.frame("Hamburgo",Hamburgo_LAI$understory.lai,"understory \n (0-5)")
names(u)<-c("Area","PAI","Type")
Hamburgo_LAI<-rbind(u,m,c,hc,total)

total<-data.frame("Forest",Forest_LAI$total.lai,"Total")
names(total)<-c("Area","PAI","Type")
hc<-data.frame("Forest",Forest_LAI$high.canopy.lai,"high canopy \n (20+)")
names(hc)<-c("Area","PAI","Type")
c<-data.frame("Forest",Forest_LAI$canopy.lai,"canopy \n (10-20)")
names(c)<-c("Area","PAI","Type")
m<-data.frame("Forest",Forest_LAI$mid.lai,"mid height \n (5-10)")
names(m)<-c("Area","PAI","Type")
u<-data.frame("Forest",Forest_LAI$understory.lai,"understory \n (0-5)")
names(u)<-c("Area","PAI","Type")
Forest_LAI<-rbind(u,m,c,hc,total)

# For graph without total values
Hamburgo_LAI<-Hamburgo_LAI[-(61:75),]
Irlanda_LAI<-Irlanda_LAI[-(89:110),]
Forest_LAI<-Forest_LAI[-(33:40),]

# Draw boxplot of HH LAI
All_LAI<-rbind(Irlanda_LAI,Hamburgo_LAI,Forest_LAI)

  library(ggthemes)
g <- ggplot(All_LAI, aes(x=reorder(Type,-PAI),y=PAI,fill=Area,col=Area))+
  geom_boxplot() +
  scale_fill_manual(values=c("#FF9933","#33CC99","#FF6666"))+
  scale_color_manual(values=c("#FF9933","#33CC99","#FF6666"))+
  labs(x="Canopy height (m)",
       y=bquote('PAI'~(m^2/m^2)))+
  coord_flip()+
  theme_classic()+
  theme(text=element_text(size=20))
plot(g)

