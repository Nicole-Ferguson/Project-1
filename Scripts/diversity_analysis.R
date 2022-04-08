
# Load point count data
pointCounts <- read.csv("resources/Point_counts.csv")

# Filter point counts per zone
pointCounts_irlanda <- pointCounts[pointCounts$Zone == "irlanda",]
pointCounts_forest <- pointCounts[pointCounts$Zone == "restoration" | pointCounts$Zone == "bosque",]
pointCounts_hamburgo <- pointCounts[pointCounts$Zone == "hamburgo",]

# Filter point counts based on confidence value
conf_threshold <- 60
pointCounts <- pointCounts[which(pointCounts$Confidence >= conf_threshold),]

# Point counts per zone
PC_irlanda <- unique(pointCounts_irlanda$ï..Point_count)
PC_forest <- unique(pointCounts_forest$ï..Point_count)
PC_hamburgo <- unique(pointCounts_hamburgo$ï..Point_count)


# Audiomoths
# Create a single data frame with all the audiomoths results

PCs <- list.files("results/audiomoths_results")
audiomoth_detections <- vector()
for(i in 1:length(PCs)){
  PC_path <- paste0("results/audiomoths_results/", PCs[i])
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


# Filter the results of BirdNET detection above a threshold for the confidence value
audiomoth_detections <- read.csv("results/audiomoth_detections.csv",)[,-1]
confidence_threshold = 0.75
audiomoth_detections_filtered <- audiomoth_detections[which(audiomoth_detections$Confidence >= confidence_threshold),]




## Combine point counts and audiomoths ##

# load species names and acronyms
spp_names <- read.csv("resources/species_names_acronyms.csv")

species_common_name <- spp_names$ï..Species.name[match(unique(pointCounts$Species), spp_names$acronym)]
species_total <- unique(c(species_common_name, unique(audiomoth_detections_filtered$Common.name)))
species_total <- species_total[-which(is.na(species_total))]


## Calculate alpha and beta diversity ##

comm.mat <- vector() # create a presence-absence community matrix
species.richness <- vector()
for(i in 1:45){
  species_total_PC <- unique(c(spp_names$ï..Species.name[match(pointCounts$Species[which(pointCounts$ï..Point_count == i)], spp_names$acronym)],
                               audiomoth_detections_filtered$Common.name[audiomoth_detections_filtered$point_count == i]))
  comm.mat <- rbind(comm.mat, species_total %in% species_total_PC)
  species.richness[i] <- length(species_total_PC)
}
row.names(comm.mat) <- 1:45
colnames(comm.mat) <- species_total
comm.mat[which(comm.mat == TRUE)] <- 1
comm.mat[which(comm.mat == FALSE)] <- 0

# Species richness
species.richness <- species.richness[-(46:47)]
species.richness.irlanda <- species.richness[PC_irlanda]
species.richness.forest <- species.richness[PC_forest]
species.richness.hamburgo <- species.richness[PC_hamburgo]

Irlanda_richness<-data.frame("Irlanda",species.richness.irlanda)
names(Irlanda_richness)<-c("Area","Richness")
Hamburgo_richness<-data.frame("Hamburgo",species.richness.hamburgo)
names(Hamburgo_richness)<-c("Area","Richness")
Forest_richness<-data.frame("Forest",species.richness.forest)
names(Forest_richness)<-c("Area","Richness")
Richness_table<-rbind(Irlanda_richness,Hamburgo_richness,Forest_richness)
Richness_table$Area<-factor(Richness_table$Area, levels=c("Irlanda", "Hamburgo", "Forest"))

g <- ggplot(Richness_table, aes(x=Area,y=Richness,fill=Area))+
  geom_boxplot() +
  scale_fill_manual(values=c("#FF6666","#33CC99","#FF9933"))+
  scale_color_manual(values=c("black"))+
  labs(x="Area",
       y="Species richness")+
  #scale_y_discrete(limits=c("0.0","2.0","4.0","6.0","8.0","10.0","12.0"))+
  theme(text=element_text(size=15))+
  theme_classic()
plot(g)


# Beta diversity
library(betapart)
library(vegan)
dist.jaccard <- beta.pair(comm.mat, index.family="jaccard")

groups <- rep(NA,45)
groups[PC_forest] <- "forest"
groups[PC_irlanda] <- "irlanda"
groups[PC_hamburgo] <- "hamburgo"
groups <- groups[-(46:47)]

bd = betadisper(dist.jaccard[[3]], groups)

#richness GLMs

#veg_metrics_PC<-read.csv("Resources/vegatation_metrics_PC.csv")
#veg_metrics_PC$total_PAI<-rowSums(veg_metrics_PC[,7:10])
#veg_metrics_PC$Area<-"Irlanda"
#veg_metrics_PC[31:45,12]<-"Hamburgo"
#veg_metrics_PC[19:24,12]<-"Forest"

#point_count_ID<-All_HH_richness$Point_count
#alpha_diversity<-All_HH_richness$`Total richness`
#r<-data.frame(point_count_ID,alpha_diversity)

Irlanda_r<-data.frame(species.richness.irlanda,PC_irlanda)
names(Irlanda_r)<-c("richness","point_count_ID")
Hamburgo_r<-data.frame(species.richness.hamburgo,PC_hamburgo)
names(Hamburgo_r)<-c("richness","point_count_ID")
Forest_r<-data.frame(species.richness.forest,PC_forest)
names(Forest_r)<-c("richness","point_count_ID")
r<-rbind(Irlanda_r,Hamburgo_r,Forest_r)

richness_veg_index2<-merge(r,vegetation_metrics2)
m<-glm(richness~canopy_height_max+canopy_cover+PAI_understory+PAI_mid+PAI_canopy+PAI_high_canopy+Area,family=poisson,data=richness_veg_index2)
summary(m)


##Model selection

m2<-glm(richness~canopy_cover+PAI_understory+PAI_mid+PAI_canopy+PAI_high_canopy+Area,family=poisson,data=richness_veg_index2)
m3<-glm(richness~canopy_height_max+PAI_understory+PAI_mid+PAI_canopy+PAI_high_canopy+Area,family=poisson,data=richness_veg_index2)
m4<-glm(richness~canopy_height_max+canopy_cover+PAI_mid+PAI_canopy+PAI_high_canopy+Area,family=poisson,data=richness_veg_index2)
m5<-glm(richness~canopy_height_max+canopy_cover+PAI_understory+PAI_canopy+PAI_high_canopy+Area,family=poisson,data=richness_veg_index2)
m6<-glm(richness~canopy_height_max+canopy_cover+PAI_understory+PAI_mid+PAI_high_canopy+Area,family=poisson,data=richness_veg_index2)
m7<-glm(richness~canopy_height_max+canopy_cover+PAI_understory+PAI_mid+PAI_canopy+Area,family=poisson,data=richness_veg_index2)
m8<-glm(richness~canopy_height_max+canopy_cover+PAI_understory+PAI_mid+PAI_canopy+PAI_high_canopy,family=poisson,data=richness_veg_index2)

#test each pair 
anova(m,m2,test="Chi")
anova(m,m3,test="Chi")
anova(m,m4,test="Chi")
anova(m,m5,test="Chi")
anova(m,m6,test="Chi")
anova(m,m7,test="Chi")
anova(m,m8,test="Chi")
#no significance detected

#test assumptions
library(DHARMa)
simResids <- simulateResiduals(m)
plot(simResids)#assumptions broken

#richness mixed effects model
library(lme4)

m2<-lmer(richness~canopy_height_max+canopy_cover+PAI_understory+PAI_mid+PAI_canopy+PAI_high_canopy+(1|Area),data=richness_veg_index2)
summary(m2)

#Draw graphs of richness against vegetation metrics for poster
g<-ggplot(richness_veg_index2,aes(x=canopy_height_max,y=richness))+
  geom_point()+
  theme_classic()+
  labs(y="Species richness", 
       x="Canopy height (m)")+
  theme(text=element_text(size=18))
plot(g)

#absolutely no correlation

g<-ggplot(richness_veg_index2,aes(x=richness,y=mean_canopy_height))+
  geom_point()+
  theme_classic()+
  labs(y="Canopy height (m)", 
       x="Species richness")+
  theme(text=element_text(size=18))
plot(g)

g<-ggplot(richness_veg_index2,aes(x=canopy_cover,y=richness))+
  geom_point()+
  theme_classic()+
  labs(y="Species richness", 
       x=bquote('Canopy cover'~(m^2/m^2)))+
theme(text=element_text(size=18))
plot(g)

g<-ggplot(richness_veg_index2,aes(x=total_PAI,y=richness))+
  geom_point()+
  theme_classic()+
  labs(y="Species richness", 
       x=bquote('PAI'~(m^2/m^2)))+
  theme(text=element_text(size=18))
plot(g)

m4<-lm(richness_vegindex$richness~richness_vegindex$total_LAI)
summary(m4)


#Beta diversity GLM
height_max_diff<-as.numeric(dist(richness_veg_index2$canopy_height_max))
canopy_cover_diff<-as.numeric(dist(richness_veg_index2$canopy_cover))
understory_PAI_diff<-as.numeric(dist(richness_veg_index2$PAI_understory))
mid_PAI_diff<-as.numeric(dist(richness_veg_index2$PAI_mid))
canopy_Pai_diff<-as.numeric(dist(richness_veg_index2$PAI_canopy))
high_canopy_PAi_diff<-as.numeric(dist(richness_veg_index2$PAI_high_canopy))

beta<-as.numeric(dist.jaccard$beta.jac)
beta_veg<-data.frame(beta,height_max_diff,canopy_cover_diff,understory_PAI_diff,mid_PAI_diff,canopy_Pai_diff,high_canopy_PAi_diff)
#how to know which ones relate to which ones?? or which area they belong to??

n<-glm(beta~height_max_diff+canopy_cover_diff+understory_PAI_diff+mid_PAI_diff+canopy_Pai_diff+high_canopy_PAi_diff+Area,family=poisson,data=beta_veg)
summary(n)

Total_PAI_diff<-as.numeric(dist(richness_veg_index2$total_PAI))
beta_veg<-data.frame(beta_veg,Total_PAI_diff)

g<-ggplot(beta_veg,aes(x=height_max_diff,y=beta))+
  geom_point()+
  theme_classic()+
  labs(y="Jaccard index", 
       x="Height (m)")+
  theme(text=element_text(size=18))
plot(g)

g<-ggplot(beta_veg,aes(x=canopy_cover_diff,y=beta))+
  geom_point()+
  theme_classic()+
  labs(y="Jaccard index", 
       x=bquote('Canopy cover'~(m^2/m^2)))+
  theme(text=element_text(size=18))
plot(g)

g<-ggplot(beta_veg,aes(x=Total_PAI_diff,y=beta))+
  geom_point()+
  theme_classic()+
  labs(y="Jaccard index", 
       x=bquote('Total PAI'~(m^2/m^2)))+
  theme(text=element_text(size=18))
plot(g)
