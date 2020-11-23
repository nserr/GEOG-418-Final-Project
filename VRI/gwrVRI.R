####Geographically Weighted Regression
#Let's say you are continuing with 
#your data from the regression analysis. 
#The first thing you need to do is to add the 
#polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the 
#"coordinates" function from the sp library
VRI.no0.coords <- sp::coordinates(VRI.no0)
#Observe the result:
head(VRI.no0.coords)
#Now add the coordinates back to the spatialpolygondataframe
VRI.no0$X <- VRI.no0.coords[,1]
VRI.no0$Y <- VRI.no0.coords[,2]

###Determine the bandwidth for GWR: this will take a while
GWRbandwidth <- gwr.sel(VRI$DEPENDENT ~ VRI$INDEPENDENT, 
                        data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model = gwr(VRI$DEPENDENT ~ VRI$INDEPENDENT, 
                data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
VRI.no0$localr <- results$localR2

#Create choropleth map of r-square values
map_r2 <- tm_shape(VRI.no0) +
  tm_polygons(col = "COLUMN",
              title = "R2 values",
              style = "jenks",
              palette = "viridis", n = 6)
map_r2

#Time for more magic. Let's map the coefficients
VRI.no0$coeff <- results$VRI.no0.Pm2.5
#Create choropleth map of the coefficients
map_coef <- tm_shape(VRI.no0) +
  tm_polygons(col = "COLUMN",
              title = "Coefficients",
              style = "jenks",
              palette = "viridis", n = 6)
map_coef