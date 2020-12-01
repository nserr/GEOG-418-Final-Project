###################
## Data Preparation
###################

#Libraries
library(spgwr)
library(spatstat)
library(tmap)
library(gstat)
library(sf)
library(raster)
library(rgdal)
library(e1071)
library(spdep)
library(gtable)
library(grid)
library(gridExtra)

#Set working directory
dir <- "C:/Users/noahs/OneDrive/Desktop/School/GEOG 418/Final Project"
setwd(dir)

#Reading in particulate matter dataset
#Read in PM2.5 data:
pm2.5 <- readOGR("./Census", "Pm25Sample")
pm2.5 <- spTransform(pm2.5, CRS("+init=epsg:26910"))

#Reading in dissemination tract and income data
#Read in census income data:
income <- read.csv("./Census/Income.csv")  
#Select only ID and Income columns:
colnames(income) <- c("DAUID", "Income")
#Read in dissemination tract shapefile:
census.tracts <- readOGR("./Census", "BC_DA")
#Merge income and dissemination data:
income.tracts <- merge(census.tracts,income, by = "DAUID")
#Determine the number of columns in the dataframe:
nrow(income.tracts)
#Remove NA values:
income.tracts <- income.tracts[!is.na(income.tracts$Income),]
#Reproject the data:
income.tracts <- spTransform(income.tracts, CRS("+init=epsg:26910"))

#Create choropleth map of income:
map_Income <- tm_shape(income.tracts) +
  tm_polygons(col = "Income",
              title = "Median Income",
              style = "jenks",
              palette = "viridis", n = 6) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"))

map_Income

#Merge PM2.5 and dissemination data
pm25.tracts <- merge(census.tracts, pm2.5, by = "DAUID")
#Remove NA values
pm25.tracts <- pm25.tracts[!is.na(pm25.tracts$PM25),]
#Reproject the data
pm25.tracts <- spTransform(pm25.tracts, CRS("+init=epsg:26910"))


#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(pm2.5, "regular", n=5000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
# Create SpatialPixel object:
gridded(grd)     <- TRUE  
# Create SpatialGrid object:
fullgrid(grd)    <- TRUE  
#Reproject the grid:
proj4string(grd) <- proj4string(income.tracts)


#########################
## Descriptive Statistics
#########################

#Income
meanIncome <- mean(income.tracts$Income)
sdIncome <- sd(income.tracts$Income)
modeIncome <- as.numeric(names(sort(table(income.tracts$Income), decreasing = TRUE))[1])
medianIncome <- median(income.tracts$Income)
skewIncome <- skewness(income.tracts$Income)[1]
kurtIncome <- kurtosis(income.tracts$Income)[1]
CovIncome <- (sdIncome / meanIncome) * 100
normIncome_PVAL <- shapiro.test(income.tracts$Income)$p.value

#PM2.5
meanPM25 <- mean(pm25.tracts$PM25)
sdPM25 <- sd(pm25.tracts$PM25)
modePM25 <- as.numeric(names(sort(table(pm25.tracts$PM25), decreasing = TRUE))[1])
medianPM25 <- median(pm25.tracts$PM25)
skewPM25 <- skewness(pm25.tracts$PM25)[1]
kurtPM25 <- kurtosis(pm25.tracts$PM25)[1]
CovPM25 <- (sdPM25 / meanPM25) * 100
normPM25_PVAL <- shapiro.test(pm25.tracts$PM25)$p.value

#Set table data
samples = c("Income", "PM 2.5")
means <- round(c(meanIncome, meanPM25), 3)
sds <- round(c(sdIncome, sdPM25), 3)
modes <- round(c(modeIncome, modePM25), 3)
medians <- round(c(medianIncome, medianPM25), 3)
skews <- round(c(skewIncome, skewPM25), 3)
kurts <- round(c(kurtIncome, kurtPM25), 3)
CoVs <- round(c(CovIncome, CovPM25), 3)
norms <- (c(normIncome_PVAL, normPM25_PVAL))

data.for.table <- data.frame(samples, means, sds, modes,
                             medians, skews, kurts, CoVs, norms)

#Create table
table <- tableGrob(data.for.table, rows = c("","")) 
tableCaption <- textGrob("Descriptive Statistics for Income and PM2.5 in Vancouver", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table <- gtable_add_rows(table, 
                         heights = grobHeight(tableCaption) + padding, 
                         pos = 0)

table <- gtable_add_grob(table,
                         tableCaption, t = 1, l = 2, r = ncol(data.for.table) + 1)


grid.arrange(table, newpage = TRUE)


################################
## Spatial Segregation of Income
################################

##Global Moran's I
income.nb <- poly2nb(income.tracts)
income.net <- nb2lines(income.nb, coords=coordinates(income.tracts))
crs(income.net) <- crs(income.tracts)

tm_shape(income.tracts) + tm_borders(col='lightgrey') + 
  tm_shape(income.net) + tm_lines(col='red')

income.lw <- nb2listw(income.nb, zero.policy = TRUE, style = "W")
print.listw(income.lw, zero.policy = TRUE)

income.mi <- moran.test(income.tracts$Income, income.lw, zero.policy = TRUE)
income.mi

mI <- income.mi$estimate[[1]]
eI <- income.mi$estimate[[2]]
var <- income.mi$estimate[[3]]

z <- (mI - eI) / sqrt(var)


##Local Moran's I (LISA)
lisa.test <- localmoran(income.tracts$Income, income.lw, zero.policy = TRUE)
lisa.test

income.tracts$Ii <- lisa.test[,1]
income.tracts$E.Ii<- lisa.test[,2]
income.tracts$Var.Ii<- lisa.test[,3]
income.tracts$Z.Ii<- lisa.test[,4]
income.tracts$P<- lisa.test[,5]

map_LISA <- tm_shape(income.tracts) + 
  tm_polygons(col = "Z.Ii", 
              title = "Local Moran's I for Income (Z Value)", 
              style = "jenks", 
              palette = "viridis", n = 6) 


map_LISA


#############################
## Spatial Interpolation (A4)
#############################

#Ordinary Kriging



# Point Pattern Analysis (A2)
### 
