###################
## Data Preparation
###################

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

dir <- "C:/Users/noahs/OneDrive/Desktop/School/GEOG 418/Final Project"
setwd(dir)

#Read in particulate matter dataset
pm2.5 <- readOGR("./Census", "Pm25Sample")
pm2.5 <- spTransform(pm2.5, CRS("+init=epsg:26910"))

#Read in census income data
income <- read.csv("./Census/Income.csv")  
colnames(income) <- c("DAUID", "Income")
#Read in dissemination tract shapefile:
census.tracts <- readOGR("./Census", "BC_DA")
#Merge income and dissemination data:
income.tracts <- merge(census.tracts,income, by = "DAUID")
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
              palette = "viridis", n = 6,
              border.alpha = 0.1) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"))

map_Income

#Load and observe PM25 data
mean.pm25 = aggregate(PM25 ~ DAUID, pm2.5, mean)
mrg.tab.mean <- sp::merge(income.tracts, mean.pm25, by = "DAUID", all.x = FALSE)
pm25.mean.spdf = na.omit(mrg.tab.mean)

tm_shape(income.tracts) + 
  tm_polygons() +
  tm_shape(pm25.mean.spdf) +
  tm_dots(col="PM25", palette = "-RdBu", 
          title="Sampled PM2.5", size=0.2) + 
  tm_legend(legend.outside=TRUE)

#Set up empty grid for use in interpolation
grd <- as.data.frame(spsample(pm2.5, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")

gridded(grd)     <- TRUE  
fullgrid(grd)    <- TRUE  
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
meanPM25 <- mean(pm25.mean.spdf$PM25)
sdPM25 <- sd(pm25.mean.spdf$PM25)
modePM25 <- as.numeric(names(sort(table(pm25.mean.spdf$PM25), decreasing = TRUE))[1])
medianPM25 <- median(pm25.mean.spdf$PM25)
skewPM25 <- skewness(pm25.mean.spdf$PM25)[1]
kurtPM25 <- kurtosis(pm25.mean.spdf$PM25)[1]
CovPM25 <- (sdPM25 / meanPM25) * 100
normPM25_PVAL <- shapiro.test(pm25.mean.spdf$PM25)$p.value

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
              palette = "viridis", n = 6,
              border.alpha = 0.1) 


map_LISA


########################
## Spatial Interpolation
########################

## Inverse Distance Weighting

P.idw <- gstat::idw(PM25 ~ 1, pm25.mean.spdf, newdata=grd, idp=5)
r       <- raster(P.idw)
r.m     <- mask(r, income.tracts)


tm_shape(r.m) + 
  tm_raster(n=10,palette = "-RdBu",
            title="Predicted PM25") + 
  tm_shape(pm25.mean.spdf) + tm_dots(size=0.1) +
  tm_legend(legend.outside=TRUE)


## Leave-One-Out Validation

IDW.out <- vector(length = length(pm25.mean.spdf))
for (i in 1:length(pm25.mean.spdf)) {
  IDW.out[i] <- idw(PM25 ~ 1, pm25.mean.spdf[-i,], pm25.mean.spdf[i,], idp=5.0)$var1.pred
}

OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ pm25.mean.spdf$PM25, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ pm25.mean.spdf$PM25), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
sqrt( sum((IDW.out - pm25.mean.spdf$PM25)^2) / length(pm25.mean.spdf))


#########################
## Point Pattern Analysis
#########################

## Quadrat Analysis

kma <- pm2.5
kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]

zd <- zerodist(kma)
kma <- remove.duplicates(kma)

kma.ext <- as.matrix(extent(kma))
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)

quads <- 10

qcount <- quadratcount(kma.ppp, nx = quads, ny = quads)

plot(kma.ppp, pch = "+", cex = 0.5)
plot(qcount, add = T, col = "red")

qcount.df <- as.data.frame(qcount)
qcount.df <- plyr::count(qcount.df, 'Freq')

colnames(qcount.df) <- c("x", "f")

sum.f.x2 <- sum(qcount.df$f * (qcount.df$x)^2)

M <- quads * quads
N <- nrow(kma)

sum.fx.2 <- sum((qcount.df$x * qcount.df$f))^2

VAR <- (sum.f.x2 - (sum.fx.2 / M)) / (M - 1)
MEAN <- N / M

VMR <- VAR / MEAN

chi.square = sqrt(VMR * (M-1))
p = 1 - pchisq(chi.square, (M - 1))
