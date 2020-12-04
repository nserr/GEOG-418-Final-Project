####################
## Linear Regression
####################

#Let's say your dataset with both PM2.5 and Income 
#are stored in a dataset called income.tracts.
#Plot income and PM2.5 from the income.tracts dataset you created
plot(income.tracts$Income~income.tracts$Pm2.5)

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
income.tracts.no0 <-  income.tracts[which(income.tracts$Pm2.5 > 0), ]

#Now plot the data again
plot(income.tracts.no0$Income~income.tracts.no0$Pm2.5)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(income.tracts.no0$Income~income.tracts.no0$Pm2.5)
#Add the regression model to the plot you created
plot(income.tracts.no0$Income~income.tracts.no0$Pm2.5)
abline(lm.model, col = "red")
#Get the summary of the results
summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
income.tracts.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
income.tracts.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(income.tracts.no0)

#Now, create choropleth map of residuals
map_resid <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "residuals",
              title = "Residuals",
              style = "jenks",
              palette = "viridis", n = 6,
              midpoint = 0, border.alpha = 0.1) +
  tm_legend(legend.outside=TRUE)

png("residuals.png")
map_resid
dev.off()


## Global Moran's I
income.no0.nb <- poly2nb(income.tracts.no0)
income.no0.net <- nb2lines(income.no0.nb, coords=coordinates(income.tracts.no0))
crs(income.no0.net) <- crs(income.tracts.no0)

tm_shape(income.tracts.no0) + tm_borders(col='lightgrey') + 
  tm_shape(income.no0.net) + tm_lines(col='red')

income.no0.lw <- nb2listw(income.no0.nb, zero.policy = TRUE, style = "W")
print.listw(income.no0.lw, zero.policy = TRUE)

income.no0.mi <- moran.test(income.tracts.no0$residuals, income.no0.lw, zero.policy = TRUE)
income.no0.mi

mI.res <- income.no0.mi$estimate[[1]]
eI.res <- income.no0.mi$estimate[[2]]
var.res <- income.no0.mi$estimate[[3]]

z.res <- (mI.res - eI.res) / sqrt(var.res)
