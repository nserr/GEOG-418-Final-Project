###########################
## Combine Income and PM2.5
###########################

# Convert interpolated surface into a raster and map it
#r <- raster(INTERPOLATED SURFACE)
sufaceMap <- tm_shape(r) + 
  tm_raster(n=5,palette = "-RdBu",
            title="PM 2.5 \n(in ppm)") +
  tm_shape(pm25.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
sufaceMap

#Extract average pm2.5 for each polygon
income.tracts$Pm2.5 <- round(extract(r, income.tracts, fun = mean)[,1], 5)
