#Statistical tests to check if hamburgo and irlanda are different
#GEDI Canopy height 
Canopy_h_table<-Canopy_h_table[-(35:38),]
a1<-aov(Canopy_h_table$Canopy_height~Canopy_h_table$Area)
summary(a1)

#normal distributiion of residuals
qqnorm(residuals(a1))
qqline(residuals(a1))
hist(residuals(a1))

#equality of variance
boxplot(Canopy_h_table$Canopy_height~Canopy_h_table$Area)
boxplot(residuals(a1)~Canopy_h_table$Area)

#assumptions broken- log transform response variable
Canopy_h_table$Log_height<-log(Canopy_h_table$Canopy_height)

a1<-aov(Canopy_h_table$Log_height~Canopy_h_table$Area)
summary(a1)

#GEDI Canopy cover


Canopy_cov_table<-rbind(Irlanda_cover,Hamburgo_cover)
a2<-aov(Canopy_cov_table$Canopy_cover~Canopy_cov_table$Area)
summary(a2)

#normal distributiion of residuals
qqnorm(residuals(a2))
qqline(residuals(a2))
hist(residuals(a2))

#equality of variance
boxplot(Canopy_cov_table$Canopy_cover~Canopy_cov_table$Area)
boxplot(residuals(a2)~Canopy_cov_table$Area)

#variance not the same but normal distr

#FHD

Gedi_FHD<-rbind(Gedi_FHD_Irlanda2,Gedi_FHD_Hamburgo2)
a3<-aov(Gedi_FHD$FHD~Gedi_FHD$Area)
summary(a3)
#normal distributiion of residuals
qqnorm(residuals(a3))
qqline(residuals(a3))
hist(residuals(a3))

#equality of variance
boxplot(Gedi_FHD$FHD~Gedi_FHD$Area)
boxplot(residuals(a3)~Gedi_FHD$Area)

#not full normal distr but variance ok

#PAI
total_PAI
a4<-aov(total_PAI$PAI~total_PAI$Area)
summary(a4)
#normal distributiion of residuals
qqnorm(residuals(a4))
qqline(residuals(a4))
hist(residuals(a4))

#equality of variance
boxplot(total_PAI$PAI~total_PAI$Area)
boxplot(residuals(a4)~total_PAI$Area)

#assumptions broken - log
total_PAI$Log_PAI<-log(total_PAI$PAI)

a4<-aov(total_PAI$Log_PAI~total_PAI$Area)
summary(a4)


#P value table

Metric<-c("Canopy height","Canopy cover","FHD","Total PAI")
P_value<-c(0.00000196,0.0000112,0.000223,0.0338)
P_value_table<-data.frame(Metric,P_value)




#Handheld height

Irlanda_height<-data.frame("Irlanda",Irlanda_height)
names(Irlanda_height)<-c("Area","Mean_height")
Hamburgo_height<-data.frame("Hamburgo",Hamburgo_height)
names(Hamburgo_height)<-c("Area","Mean_height")
Forest_height<-data.frame("Forest",Forest_height)
names(Forest_height)<-c("Area","Mean_height")
height_mean_table<-rbind(Irlanda_height,Hamburgo_height,Forest_height)

a_hh1<-aov(height_mean_table$height_mean~height_mean_table$Area)
summary(a_hh1)

#normal distributiion of residuals
qqnorm(residuals(a_hh1))
qqline(residuals(a_hh1))
hist(residuals(a_hh1))

#equality of variance
boxplot(height_mean_table$height_mean~height_mean_table$Area)
boxplot(residuals(a_hh1)~height_mean_table$Area)

#maximum height

Irlanda_max<-data.frame("Irlanda",Irlanda_max)
names(Irlanda_max)<-c("Area","Max_height")
Hamburgo_max<-data.frame("Hamburgo",Hamburgo_max)
names(Hamburgo_max)<-c("Area","Max_height")
Forest_max<-data.frame("Forest",Forest_max)
names(Forest_max)<-c("Area","Max_height")
height_max_table<-rbind(Irlanda_max,Hamburgo_max,Forest_max)

a_hh2<-aov(height_max_table$Max_height~height_max_table$Area)
summary(a_hh2)

#normal distributiion of residuals
qqnorm(residuals(a_hh2))
qqline(residuals(a_hh2))
hist(residuals(a_hh2))

#equality of variance
boxplot(height_max_table$Max_height~height_max_table$Area)
boxplot(residuals(a_hh2)~height_max_table$Area)


#Handheld canopy cover

Irlanda_cc<-data.frame("Irlanda",cc_hh_Irlanda)
names(Irlanda_cc)<-c("Area","Canopy_cover")
Hamburgo_cc<-data.frame("Hamburgo",cc_hh_Hamburgo)
names(Hamburgo_cc)<-c("Area","Canopy_cover")
Forest_cc<-data.frame("Forest",cc_hh_Forest)
names(Forest_cc)<-c("Area","Canopy_cover")
CC_table<-rbind(Irlanda_cc,Hamburgo_cc,Forest_cc)

a_hh3<-aov(CC_table$Canopy_cover~CC_table$Area)
summary(a_hh3)#P not significant

#normal distributiion of residuals
qqnorm(residuals(a_hh3))
qqline(residuals(a_hh3))
hist(residuals(a_hh3))

#equality of variance
boxplot(CC_table$Canopy_cover~CC_table$Area)
boxplot(residuals(a_hh3)~CC_table$Area)




#Handheld PAI






#ANOVA to compare GEDI vs Handheld metrics















#richness compared

arich<-aov(overall_richness$richness_by_effort~overall_richness$names)
summary(arich)
