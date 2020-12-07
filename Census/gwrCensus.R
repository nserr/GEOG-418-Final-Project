#####################################
## Geographically Weighted Regression
#####################################

#Add polygon coordinates to SPDF
income.tracts.no0.coords <- sp::coordinates(income.tracts.no0)

income.tracts.no0$X <- income.tracts.no0.coords[,1]
income.tracts.no0$Y <- income.tracts.no0.coords[,2]

## Determine the bandwidth for GWR
GWRbandwidth <- gwr.sel(income.tracts.no0$Income~income.tracts.no0$Pm2.5, 
                        data=income.tracts.no0, coords=cbind(income.tracts.no0$X,income.tracts.no0$Y),adapt=T) 

## Perform GWR
gwr.model = gwr(income.tracts.no0$Income~income.tracts.no0$Pm2.5, 
                data=income.tracts.no0, coords=cbind(income.tracts.no0$X,income.tracts.no0$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

gwr.model

results<-as.data.frame(gwr.model$SDF)
head(results)

#Add local R-square values
income.tracts.no0$localr <- results$localR2

#Create choropleth map of r-square values
map_r2 <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "localr",
              title = "R2 values",
              style = "jenks",
              palette = "viridis", n = 6,
              midpoint = 0, border.alpha = 0.1) +
  tm_legend(legend.outside = TRUE)

png("r2.png")
map_r2
dev.off()

#Add coefficient values
income.tracts.no0$coeff <- results$income.tracts.no0.Pm2.5

#Create choropleth map of the coefficients
map_coeff <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "coeff",
              title = "Coefficients",
              style = "jenks",
              palette = "viridis", n = 6,
              midpoint = 0, border.alpha = 0.1) +
  tm_legend(legend.outside = TRUE)

png("coeff.png")
map_coeff
dev.off()
