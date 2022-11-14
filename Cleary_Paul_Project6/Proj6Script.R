library("raster")
library("sf")
library("tidyr")
library("tidyverse")
library("plyr")
library("dplyr")
library("tmap")
library("tmaptools")
library("RColorBrewer")
library("lwgeom")
library("Hmisc")
library("exactextractr")
library("GISTools")
library("rgeos")
library("spdep")
#--------------------Data Preperation-------------------------
# Set the working directory
setwd('~/Fall 19/Data Analytics/Project6')
wd <- '~/Fall 19/Data Analytics/Project6'
IN <- st_read("Census_Counties/Census_County_TIGER00_IN.shp")
tmap_mode("plot")
INPoP <- tm_shape(IN) + tm_fill("POP2000", style = "jenks", palette = "RdYlBu") + tm_polygons() +
  tm_layout(title = "Population of Indiana 2000",title.size = 2,title.position = c(.04,.96), inner.margins = c(0.1,0.1,0.07,0.05), frame = TRUE, bg.color = "grey85", legend.text.size = .45, legend.title.size = .75, frame.double.line = TRUE) +
  tm_legend(legend.position = c("RIGHT", "BOTTOM")) + tm_compass(type = "arrow", position = c(0.02,0.7)) +
  tm_scale_bar(breaks = c(0, 50, 100), position = c("left","BOTTOM"), text.size = .5)
print(INPoP)
# Plot the connection of counties under two different neighborhood definitions
# Use QUEEN neighborhood
IN_Q <- poly2nb(IN, queen = T, row.names = IN$NAME_L)
IN_cents <- st_centroid(IN$geometry)
plot(main = "Queen Link",IN$geometry) 
plot(IN_Q,IN_cents, col = 'red', add=TRUE)
summary(IN_Q)
# Use ROOK neighborhood
IN_R <- poly2nb(IN, queen = F, row.names = IN$NAME_L)
plot(main = "Rook Link", IN$geometry)
plot(IN_R,IN_cents, col = 'blue', add=T)
summary(IN_R)
# Generate the three weight matrices, using W,S,U
weightW <- nb2listw(IN_Q, style = 'W')
weightS <- nb2listw(IN_Q, style = 'S')
weightU <- nb2listw(IN_Q, style = 'U')
# Evaluate Moran's I statistic for POP2000 & W,S,& U weights.
moranW <- moran.test(IN$POP2000, weightW)
moranS <- moran.test(IN$POP2000, weightS)
moranU <- moran.test(IN$POP2000, weightU)
moranU.F <- moran.test(IN$POP2000, weightU, randomisation = F)
moranS.F <- moran.test(IN$POP2000, weightS, randomisation = F)
moranW.F <- moran.test(IN$POP2000, weightW, randomisation = F)
moranW
moranS
moranU
moranU.F
moranS.F
moranW.F
# All above P-Values > 0.05, accept Ho
# Plot the Moran's I Values
Wpop <- moran.plot(IN$POP2000, weightW) 
Spop <- moran.plot(IN$POP2000, weightS) 
Upop <- moran.plot(IN$POP2000, weightU) 
# Calculate the Monte Carlo Value of Moran's I
Wmc <- moran.mc(IN$POP2000, weightW, 50000)
Smc <- moran.mc(IN$POP2000, weightS, 50000)
Umc <- moran.mc(IN$POP2000, weightU, 50000)
plot(Wmc, type='b', cex=0.5)
plot(Smc, type='b', cex=0.5)
plot(Umc, type='b', cex=0.5)
# Moran’s I evaluation Select two relevant attributes of your interest (other than POP2000)
# Selected HSE_UNITS and VACANT 
#Use moran.test on the selcted attributes
HSE.units.moran <- moran.test(IN$HSE_UNITS, weightW)
HSE.units.moranF <- moran.test(IN$HSE_UNITS, weightW, randomisation = F)
HSE.units.moran
HSE.units.moranF
vacant.moran <- moran.test(IN$VACANT, weightW)
vacant.moranF <- moran.test(IN$VACANT, weightW, randomisation = F)
vacant.moran
vacant.moranF
HSE.units.mc <- moran.mc(IN$HSE_UNITS, weightW, 50000)
Vacant.mc <- moran.mc(IN$VACANT, weightS, 50000)
HSE.units.mc
Vacant.mc
plot(HSE.units.mc, type='b', cex=0.5)
plot(Vacant.mc, type='b', cex=0.5)
#Local Moran on selected attributes
HSE.units.lmoran <- localmoran(IN$HSE_UNITS, weightW)
vacant.lmoran <- localmoran(IN$VACANT, weightW)
# Histogram and plot of local moran
IN.units.lm <- cbind(IN, HSE.units.lmoran)
IN.vacant.lm <- cbind(IN, vacant.lmoran)
hist(IN.units.lm[["Ii"]])
hist(IN.vacant.lm[["Ii"]])
Units.lm.map <- tm_shape(IN.units.lm) + tm_fill("Ii", style = "jenks", palette = "RdYlBu") + tm_polygons() +
  tm_layout(title = "Local Moran's I of Housing Units",title.size = 2,title.position = c(.04,.96), inner.margins = c(0.1,0.1,0.07,0.05), frame = TRUE, bg.color = "grey85", legend.text.size = .45, legend.title.size = .75, frame.double.line = TRUE) +
  tm_legend(legend.position = c("RIGHT", "BOTTOM")) + tm_compass(type = "arrow", position = c(0.02,0.7)) +
  tm_scale_bar(breaks = c(0, 50, 100), position = c("left","BOTTOM"), text.size = .5)
Vacant.lm.map <- tm_shape(IN.vacant.lm) + tm_fill("Ii", style = "jenks", palette = "RdYlBu") + tm_polygons() +
  tm_layout(title = "Local Moran's I of Vacant Units",title.size = 2,title.position = c(.03,.96), inner.margins = c(0.1,0.1,0.07,0.05), frame = TRUE, bg.color = "grey85", legend.text.size = .45, legend.title.size = .75, frame.double.line = TRUE) +
  tm_legend(legend.position = c("RIGHT", "BOTTOM")) + tm_compass(type = "arrow", position = c(0.02,0.7)) +
  tm_scale_bar(breaks = c(0, 50, 100), position = c("left","BOTTOM"), text.size = .5)
print(Units.lm.map)
print(Vacant.lm.map)
