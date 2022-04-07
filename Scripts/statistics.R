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


Understory_PAI
a1.1<-aov(Understory_PAI$Understory_PAI~Understory_PAI$Area)
summary(a1.1)
#normal distributiion of residuals
qqnorm(residuals(a1.1))
qqline(residuals(a1.1))
hist(residuals(a1.1))

#equality of variance
boxplot(Understory_PAI$Understory_PAI~Understory_PAI$Area)
boxplot(residuals(a1.1)~Understory_PAI$Area)


Mid_PAI
a1.2<-aov(Mid_PAI$Mid_PAI~Mid_PAI$Area)
summary(a1.2)
#normal distributiion of residuals
qqnorm(residuals(a1.2))
qqline(residuals(a1.2))
hist(residuals(a1.2))

#equality of variance
boxplot(Mid_PAI$Mid_PAI~Mid_PAI$Area)
boxplot(residuals(a1.2)~Mid_PAI$Area)

Canopy_PAI
a1.3<-aov(Canopy_PAI$Canopy_PAI~Canopy_PAI$Area)
summary(a1.3)
#normal distributiion of residuals
qqnorm(residuals(a1.3))
qqline(residuals(a1.3))
hist(residuals(a1.3))

High_canopy_PAI
a1.4<-aov(High_canopy_PAI$High_canopy_PAI~High_canopy_PAI$Area)
summary(a1.4)
#normal distributiion of residuals
qqnorm(residuals(a1.4))
qqline(residuals(a1.4))
hist(residuals(a1.4))

#equality of variance
boxplot(High_canopy_PAI$High_canopy_PAI~High_canopy_PAI$Area)
boxplot(residuals(a1.4)~High_canopy_PAI$Area)
#assumptions broken - log
total_PAI$Log_PAI<-log(total_PAI$PAI)

#assumptions broken - log
High_canopy_PAI$Log_PAI<-log(High_canopy_PAI$High_canopy_PAI)

a1.4<-aov(High_canopy_PAI$Log_PAI~High_canopy_PAI$Area)#doesnt work
summary(a1.4)


#P value table

Metric<-c("Canopy height","Canopy cover","FHD","Total PAI")
P_value<-c(0.00000196,0.0000112,0.000223,0.0338)
P_value_table<-data.frame(Metric,P_value)




#Handheld height

Irlanda_height<-data.frame("Irlanda",Irlanda_height)
names(Irlanda_height)<-c("Area","Mean_height")
Irlanda_height<-Irlanda_height[-23,]
Hamburgo_height<-data.frame("Hamburgo",Hamburgo_height)
names(Hamburgo_height)<-c("Area","Mean_height")
Hamburgo_height<-Hamburgo_height[-12,]
Forest_height<-data.frame("Forest",Forest_height)
names(Forest_height)<-c("Area","Mean_height")
height_mean_table<-rbind(Irlanda_height,Hamburgo_height,Forest_height)

a_hh1<-aov(height_mean_table$Mean_height~height_mean_table$Area)
summary(a_hh1)

#normal distributiion of residuals
qqnorm(residuals(a_hh1))
qqline(residuals(a_hh1))
hist(residuals(a_hh1))

#equality of variance
boxplot(height_mean_table$Mean_height~height_mean_table$Area)
boxplot(residuals(a_hh1)~height_mean_table$Area)

#maximum height

Irlanda_max<-data.frame("Irlanda",Irlanda_max)
names(Irlanda_max)<-c("Area","Max_height")
Irlanda_max<-Irlanda_max[-23,]
Hamburgo_max<-data.frame("Hamburgo",Hamburgo_max)
names(Hamburgo_max)<-c("Area","Max_height")
Hamburgo_max<-Hamburgo_max[-12,]
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
Irlanda_cc<-Irlanda_cc[-23,]
Hamburgo_cc<-data.frame("Hamburgo",cc_hh_Hamburgo)
names(Hamburgo_cc)<-c("Area","Canopy_cover")
Hamburgo_cc<-Hamburgo_cc[-12,]
Forest_cc<-data.frame("Forest",cc_hh_Forest)
names(Forest_cc)<-c("Area","Canopy_cover")
CC_table<-rbind(Irlanda_cc,Hamburgo_cc,Forest_cc)

a_hh3<-aov(CC_table$Canopy_cover~CC_table$Area)
summary(a_hh3)

#normal distributiion of residuals
qqnorm(residuals(a_hh3))
qqline(residuals(a_hh3))
hist(residuals(a_hh3))

#equality of variance
boxplot(CC_table$Canopy_cover~CC_table$Area)
boxplot(residuals(a_hh3)~CC_table$Area)


#Handheld PAI
Total_LAI<-All_LAI[which(All_LAI$Type=="Total"),]

a_hh4<-aov(Total_LAI$PAI~Total_LAI$Area)
summary(a_hh4)

#normal distributiion of residuals
qqnorm(residuals(a_hh4))
qqline(residuals(a_hh4))
hist(residuals(a_hh4))

#equality of variance
boxplot(Total_LAI$PAI~Total_LAI$Area)
boxplot(residuals(a_hh4)~Total_LAI$Area)


Understory_LAI<-All_LAI[which(All_LAI$Type=="understory \n (0-5)"),]

a_hh5<-aov(Understory_LAI$PAI~Understory_LAI$Area)
summary(a_hh5)

#normal distributiion of residuals
qqnorm(residuals(a_hh5))
qqline(residuals(a_hh5))
hist(residuals(a_hh5))

#equality of variance
boxplot(Understory_LAI$PAI~Understory_LAI$Area)
boxplot(residuals(a_hh5)~Understory_LAI$Area)


Mid_LAI<-All_LAI[which(All_LAI$Type=="mid height \n (5-10)"),]

a_hh6<-aov(Mid_LAI$PAI~Mid_LAI$Area)
summary(a_hh6)

#normal distributiion of residuals
qqnorm(residuals(a_hh6))
qqline(residuals(a_hh6))
hist(residuals(a_hh6))

#equality of variance
boxplot(Mid_LAI$PAI~Mid_LAI$Area)
boxplot(residuals(a_hh6)~Mid_LAI$Area)


Canopy_LAI<-All_LAI[which(All_LAI$Type=="canopy \n (10-20)"),]

a_hh7<-aov(Canopy_LAI$PAI~Canopy_LAI$Area)
summary(a_hh7)

#normal distributiion of residuals
qqnorm(residuals(a_hh7))
qqline(residuals(a_hh7))
hist(residuals(a_hh7))

#equality of variance
boxplot(Canopy_LAI$PAI~Canopy_LAI$Area)
boxplot(residuals(a_hh7)~Canopy_LAI$Area)

Canopy_LAI$Log<-log(Mid_LAI$PAI)

a_hh7<-aov(Canopy_LAI$Log~Canopy_LAI$Area)
summary(a_hh7)


High_canopy_LAI<-All_LAI[which(All_LAI$Type=="high canopy \n (20+)"),]

a_hh8<-aov(High_canopy_LAI$PAI~High_canopy_LAI$Area)
summary(a_hh8)

#normal distributiion of residuals
qqnorm(residuals(a_hh8))
qqline(residuals(a_hh8))
hist(residuals(a_hh8))

#equality of variance
boxplot(High_canopy_LAI$PAI~High_canopy_LAI$Area)
boxplot(residuals(a_hh8)~High_canopy_LAI$Area)

#assumptions broken
High_canopy_LAI$Log<-log(High_canopy_LAI$PAI)

a_hh8<-aov(High_canopy_LAI$Log~High_canopy_LAI$Area)#doesnt work
summary(a_hh8)



#ANOVA to compare GEDI vs Handheld metrics

#GEDI height vs max_height

#Irlanda
#laast Irlanda_max is NA
Irlanda_max<-Irlanda_max[-23]
#Canopy_h_Irlanda has too many points:remove two last ones
Canopy_h_Irlanda2<-Canopy_h_Irlanda[-(23:24)]


h1<-data.frame(Canopy_h_Irlanda2,Irlanda_max)
gh1<-aov(h1$Canopy_h_Irlanda2~h1$Irlanda_max)
summary(gh1)
#normal distributiion of residuals
qqnorm(residuals(gh1))
qqline(residuals(gh1))
hist(residuals(gh1))

#equality of variance
boxplot(h1$Canopy_h_Irlanda2~h1$Irlanda_max)
boxplot(residuals(gh1)~h1$Irlanda_max)

#assumptions broken
h1_log<-data.frame(log(Canopy_h_Irlanda2),Irlanda_max)
gh1<-aov(h1_log$log.Canopy_h_Irlanda2.~h1_log$Irlanda_max)
summary(gh1)



#Hamburgo
#Hamburgo_max has too many values - remove 2
Hamburgo_max2<-Hamburgo_max[-(11:12)]

h2<-data.frame(Canopy_h_Hamburgo,Hamburgo_max2)
gh2<-aov(h2$Canopy_h_Hamburgo~h2$Hamburgo_max)
summary(gh2)
#normal distributiion of residuals
qqnorm(residuals(gh2))
qqline(residuals(gh2))
hist(residuals(gh2))

#equality of variance
boxplot(h2$Canopy_h_Hamburgo~h2$Hamburgo_max)
boxplot(residuals(gh2)~h2$Hamburgo_max)#residuals and all dont make sense 


#GEDI height vs mean height

#Irlanda
Irlanda_height<-Irlanda_height[-23]
Canopy_h_Irlanda2<-Canopy_h_Irlanda[-(23:24)]

h3<-data.frame(Canopy_h_Irlanda2,Irlanda_height)
gh3<-aov(h3$Canopy_h_Irlanda2~h3$Irlanda_height)
summary(gh3)

#normal distributiion of residuals
qqnorm(residuals(gh3))
qqline(residuals(gh3))
hist(residuals(gh3))

#equality of variance
boxplot(h3$Canopy_h_Irlanda2~h3$Irlanda_height)
boxplot(residuals(gh3)~h3$Irlanda_height)



#Hamburgo
Hamburgo_height2<-Hamburgo_height[-(11:12)]

h4<-data.frame(Canopy_h_Hamburgo,Hamburgo_height2)
gh4<-aov(h4$Canopy_h_Hamburgo~h4$Hamburgo_height)
summary(gh4)

#normal distributiion of residuals
qqnorm(residuals(gh4))
qqline(residuals(gh4))
hist(residuals(gh4))

#equality of variance
boxplot(h4$Canopy_h_Hamburgo~h4$Hamburgo_height)
boxplot(residuals(gh4)~h4$Hamburgo_height)




#GEDI Total PAI vs HH total PAI

#Irlanda
length(Gedi_layers_Irlanda$total)
length(Irlanda_LAI$Total)
gedi_tPAI<-Gedi_layers_Irlanda$total[1:22]
p<-data.frame(gedi_tPAI,Irlanda_LAI$Total)

gh4<-aov(p$gedi_tPAI~p$Irlanda_LAI.Total)
summary(gh4)

#normal distributiion of residuals
qqnorm(residuals(gh4))
qqline(residuals(gh4))
hist(residuals(gh4))

#equality of variance
boxplot(p$gedi_tPAI~p$Irlanda_LAI.Total)
boxplot(residuals(gh4)~p$Irlanda_LAI.Total)





#Hamburgo
length(Gedi_layers_Hamburgo$total)
length(Hamburgo_LAI$total.lai)
gedi_tPAI2<-Gedi_layers_Hamburgo$total[-(12:13)]

gh5<-aov(gedi_tPAI2~Hamburgo_LAI$total.lai)
summary(gh5)

#normal distributiion of residuals
qqnorm(residuals(gh5))
qqline(residuals(gh5))
hist(residuals(gh5))

#equality of variance
boxplot(gedi_tPAI2~Hamburgo_LAI$total.lai)
boxplot(residuals(gh5)~Hamburgo_LAI$total.lai) #values actually more similar

#no normal distribution of residuals
gh5<-aov(log(gedi_tPAI2)~Hamburgo_LAI$total.lai)
summary(gh5)


#Relative differemce between areas for GEDI vs HH
mean(Canopy_h_Irlanda)

Ham_h_Gedi_diff<-(mean(Canopy_h_Hamburgo)-mean(Canopy_h_Irlanda))/mean(Canopy_h_Irlanda)
Ham_hmax_diff<-(mean(Hamburgo_max, na.rm=T)-mean(Irlanda_max))/mean(Irlanda_max)
Ham_hmean_diff<-(mean(Hamburgo_height, na.rm=T)-mean(Irlanda_height))/mean(Irlanda_height)
Ham_h_Gedi_diff
Ham_hmax_diff
Ham_hmean_diff


Ham_cov_Gedi_diff<-(mean(Canopy_cov_Hamburgo)-mean(Canopy_cov_Irlanda))/mean(Canopy_cov_Irlanda)
Ham_cov_diff<-(mean(cc_hh_Hamburgo, na.rm=T)-mean(cc_hh_Irlanda, na.rm=T))/mean(cc_hh_Irlanda, na.rm=T)
Ham_cov_Gedi_diff
Ham_cov_diff

Ham_PAI_Gedi_diff<-(mean(Gedi_layers_Hamburgo$total)-mean(Gedi_layers_Irlanda$total))/mean(Gedi_layers_Irlanda$total)
Ham_PAI_diff<-(mean(Hamburgo_LAI$total.lai)-mean(Irlanda_LAI$total.lai)/mean(Irlanda_LAI$total.lai))

#relative difference value table

GEDI<-c(Ham_h_Gedi_diff,"NA",Ham_cov_Gedi_diff,Ham_PAI_Gedi_diff)
Handheld<-c(Ham_hmax_diff,Ham_hmean_diff,Ham_cov_diff,Ham_PAI_diff)
metrics<-c("Max height","Mean height","Canopy cover", "PAI")
diff_table<-data.frame(metrics,GEDI,Handheld)
diff_table

#richness compared

arich<-aov(overall_richness$richness_by_effort~overall_richness$names)
summary(arich)
