######Linear Regression##########
#Let's say your dataset with both Elev and Height are stored in a dataset called VRI.
#Plot Height and Elev from the VRI dataset you created
plot(VRI$DEPENDENT ~ VRI$INDEPENDENT)

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
VRI.no0 <-  VRI[which(VRI$WHOLE_STEM > 0), ]
VRI.no0 <-  VRI.no0[which(VRI.no0$Elev > 0), ]

#Now plot the data again
plot(VRI$DEPENDENT ~ VRI$INDEPENDENT)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(VRI$DEPENDENT ~ VRI$INDEPENDENT)

#Add the regression model to the plot you created
plot(VRI$DEPENDENT ~ VRI$INDEPENDENT)
abline(lm.model, col = "red")

#Get the summary of the results
summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
VRI.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
VRI.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(VRI.no0@data)

#Now, create choropleth map of residuals
map_resid <- tm_shape(VRI.no0) +
  tm_polygons(col = "residuals",
              title = "Stand Biomass Residuals",
              style = "jenks",
              palette = "viridis", n = 6)

map_resid
##################################################