## Statistical tests to check if Hamburgo and Irlanda are different ##

# GEDI PAI ANOVAs

# Total_PAI
a4<-aov(total_PAI$PAI~total_PAI$Area)
summary(a4)

# Normal distributiion of residuals
qqnorm(residuals(a4))
qqline(residuals(a4))
hist(residuals(a4))

# Equality of variance
boxplot(total_PAI$PAI~total_PAI$Area)
boxplot(residuals(a4)~total_PAI$Area)

# Assumptions broken - log
total_PAI$Log_PAI<-log(total_PAI$PAI)

a4<-aov(total_PAI$Log_PAI~total_PAI$Area)
summary(a4)



# Understory PAI
a1.1<-aov(Understory_PAI$Understory_PAI~Understory_PAI$Area)
summary(a1.1)

# Normal distributiion of residuals
qqnorm(residuals(a1.1))
qqline(residuals(a1.1))
hist(residuals(a1.1))

# Equality of variance
boxplot(Understory_PAI$Understory_PAI~Understory_PAI$Area)
boxplot(residuals(a1.1)~Understory_PAI$Area)



# Mid_PAI
a1.2<-aov(Mid_PAI$Mid_PAI~Mid_PAI$Area)
summary(a1.2)

# Normal distributiion of residuals
qqnorm(residuals(a1.2))
qqline(residuals(a1.2))
hist(residuals(a1.2))

# Equality of variance
boxplot(Mid_PAI$Mid_PAI~Mid_PAI$Area)
boxplot(residuals(a1.2)~Mid_PAI$Area)



# Canopy_PAI
a1.3<-aov(Canopy_PAI$Canopy_PAI~Canopy_PAI$Area)
summary(a1.3)

# Normal distributiion of residuals
qqnorm(residuals(a1.3))
qqline(residuals(a1.3))
hist(residuals(a1.3))



# High_canopy_PAI
a1.4<-aov(High_canopy_PAI$High_canopy_PAI~High_canopy_PAI$Area)
summary(a1.4)

# Normal distributiion of residuals
qqnorm(residuals(a1.4))
qqline(residuals(a1.4))
hist(residuals(a1.4))

# Equality of variance
boxplot(High_canopy_PAI$High_canopy_PAI~High_canopy_PAI$Area)
boxplot(residuals(a1.4)~High_canopy_PAI$Area)

# Assumptions broken - log
High_canopy_PAI$Log_PAI<-log(High_canopy_PAI$High_canopy_PAI)

a1.4<-aov(High_canopy_PAI$Log_PAI~High_canopy_PAI$Area)#doesnt work
summary(a1.4)



# Handheld LIDAR PAI ANOVAs

# Total PAI
Total_LAI<-All_LAI[which(All_LAI$Type=="Total"),]

a_hh4<-aov(Total_LAI$PAI~Total_LAI$Area)
summary(a_hh4)

# Normal distributiion of residuals
qqnorm(residuals(a_hh4))
qqline(residuals(a_hh4))
hist(residuals(a_hh4))

# Equality of variance
boxplot(Total_LAI$PAI~Total_LAI$Area)
boxplot(residuals(a_hh4)~Total_LAI$Area)



# Understory PAI
Understory_LAI<-All_LAI[which(All_LAI$Type=="understory \n (0-5)"),]

a_hh5<-aov(Understory_LAI$PAI~Understory_LAI$Area)
summary(a_hh5)

# Normal distributiion of residuals
qqnorm(residuals(a_hh5))
qqline(residuals(a_hh5))
hist(residuals(a_hh5))

# Equality of variance
boxplot(Understory_LAI$PAI~Understory_LAI$Area)
boxplot(residuals(a_hh5)~Understory_LAI$Area)



# Mid PAI
Mid_LAI<-All_LAI[which(All_LAI$Type=="mid height \n (5-10)"),]

a_hh6<-aov(Mid_LAI$PAI~Mid_LAI$Area)
summary(a_hh6)

# Normal distributiion of residuals
qqnorm(residuals(a_hh6))
qqline(residuals(a_hh6))
hist(residuals(a_hh6))

# Equality of variance
boxplot(Mid_LAI$PAI~Mid_LAI$Area)
boxplot(residuals(a_hh6)~Mid_LAI$Area)



# Canopy PAI
Canopy_LAI<-All_LAI[which(All_LAI$Type=="canopy \n (10-20)"),]

a_hh7<-aov(Canopy_LAI$PAI~Canopy_LAI$Area)
summary(a_hh7)

# Normal distributiion of residuals
qqnorm(residuals(a_hh7))
qqline(residuals(a_hh7))
hist(residuals(a_hh7))

# Equality of variance
boxplot(Canopy_LAI$PAI~Canopy_LAI$Area)
boxplot(residuals(a_hh7)~Canopy_LAI$Area)

# Assumptions broken
Canopy_LAI$Log<-log(Mid_LAI$PAI)

a_hh7<-aov(Canopy_LAI$Log~Canopy_LAI$Area)
summary(a_hh7)



# High canopy PAI
High_canopy_LAI<-All_LAI[which(All_LAI$Type=="high canopy \n (20+)"),]

a_hh8<-aov(High_canopy_LAI$PAI~High_canopy_LAI$Area)
summary(a_hh8)

# Normal distributiion of residuals
qqnorm(residuals(a_hh8))
qqline(residuals(a_hh8))
hist(residuals(a_hh8))

# Equality of variance
boxplot(High_canopy_LAI$PAI~High_canopy_LAI$Area)
boxplot(residuals(a_hh8)~High_canopy_LAI$Area)

# Assumptions broken
High_canopy_LAI$Log<-log(High_canopy_LAI$PAI)

a_hh8<-aov(High_canopy_LAI$Log~High_canopy_LAI$Area)#doesnt work
summary(a_hh8)


## Pearson correlation between canopy cover and canopy heigght in GEDI data ##
cor(df$Canopy_height,df$Canopy_cover)


## T tests to check if Irlanda is significantly greater than Hamburgo with regards to vegetation metrics ##

# GEDI 

t.test(Canopy_h_Irland,Canopy_h_Hamburgo, alternative="greater")
t.test(Canopy_cov_Irlanda,Canopy_cov_Hamburgo, alternative = "greater")
t.test(Gedi_layers_Irlanda$total,Gedi_layers_Hamburgo$total,alternative = "greater")

# Handheld LIDAR

t.test(Irlanda_height, Hamburgo_height, alternative = "greater")
t.test(Irlanda_max,Hamburgo_max,alternative = "greater")
t.test(cc_hh_Irlanda,cc_hh_Hamburgo,alternative="greater")
t.test(Irlanda_LAI$total.lai,Hamburgo_LAI$total.lai, alternative="greater")


## T test to compare GEDI vs Handheld metrics ##

# GEDI height vs max_height
# Irlanda
t.test(Canopy_h_Irlanda,Irlanda_max)

# Hamburgo
t.test(Canopy_h_Hamburgo,Hamburgo_max)


# GEDI height vs mean height
# Irlanda
t.test(Canopy_h_Irlanda,Irlanda_height)

# Hamburgo
t.test(Canopy_h_Hamburgo,Hamburgo_height)


# GEDI Total PAI vs HH total PAI
# Irlanda
t.test(Gedi_layers_Irlanda$total,Irlanda_LAI$Total)

# Hamburgo
t.test(Gedi_layers_Hamburgo$total,Hamburgo_LAI$total.lai)


## Relative difference between areas for GEDI vs Handheld LIDAR ##

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

# Relative difference value table

GEDI<-c(Ham_h_Gedi_diff,"NA",Ham_cov_Gedi_diff,Ham_PAI_Gedi_diff)
Handheld<-c(Ham_hmax_diff,Ham_hmean_diff,Ham_cov_diff,Ham_PAI_diff)
metrics<-c("Max height","Mean height","Canopy cover", "PAI")
diff_table<-data.frame(metrics,GEDI,Handheld)
diff_table

