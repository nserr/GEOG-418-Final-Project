#These steps will help you combine the outputs 
#from your spatial interpolation with your income data.
#Convert your interpolation into a raster and map it:
r <- raster(INTERPOLATED SURFACE)
sufaceMap <- tm_shape(r) + 
  tm_raster(n=5,palette = "viridis",
            title="Elev (m)") +
  tm_shape(POINTS) + tm_dots(size=0.2)
sufaceMap

#If you have too many cells, 
#you can reduce the number by aggregating values
#agg <- aggregate(yourRasterFromKriging, fact=??, fun=mean)

#Extract average elev for each polygon
VRI$Elev <- extract(r, VRI, fun = mean)[,1]