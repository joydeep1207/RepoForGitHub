##
# name : Joydeep
#
#
##


# locating the working directory
setwd("C:/Users/deepj/Desktop/Data/Mean_temp")

myData = read.csv("T12.1_msme_4th_census.csv", header = T)

prepData = myData[1:35,]

install.packages("rgdal")
require("rgdal")  # needed to load shapefiles

install.packages("maptools")
library(maptools)

library(RColorBrewer)

# Read the shape file into a SpatialPolygonsDataFrame object
india = readShapePoly('C:/Users/deepj/Desktop/Data/Mean_temp/IND_adm/IND_adm1')
summary(india)

india$NAME_1


#sort by mpg (ascending) and cyl (descending)
newdata <- prepData[order(prepData$State.UT),] 

# for hindus data
india$imr = newdata$Hindu..Values.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,18,2))


# for muslim data
india$imr = newdata$Muslim..Values.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,6,0.66))


# for sikh data
india$imr = newdata$Sikh..Values.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,6,0.66))


# for christian data
india$imr = newdata$Christian..Values.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,1.8,0.2))


# for Jain data
india$imr = newdata$Jain..Values.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,0.1,0.02))


# for BUddhist data
india$imr = newdata$Buddhist..Values.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,0.28,0.04))


# for others data
india$imr = newdata$Others..Values.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,0.15,0.03))


# for total data
india$imr = newdata$Total..Values.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,24,3))

#############################################################################
# new data set for sex
myData1 = read.csv("T7.1_msme_4th_census.csv", header = T)

prepData = myData1[1:35,]


#sort by mpg (ascending) and cyl (descending)
newdata <- prepData[order(prepData$State.UT),] 


# for female data
india$imr = newdata$Female..Numbers.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,3.05,0.35))

# for male data
india$imr = newdata$Male..Numbers.in.Lakh.

# Brew a color palette
incol<-brewer.pal(n=9,name='YlOrRdb')
# plot the map
spplot(india,'imr',col.regions=incol,at=seq(0,23,2.5))



