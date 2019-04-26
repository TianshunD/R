setwd("C:/Users/10492/Desktop/Graduate/Programming for GIS/Final/multi")
library(raster)
r <- brick('champaign_raster.tif')
r$champaign_raster.1
plot(r)
# For Landsat 8 image, r = 4 (red), g = 3(green), b = 2(blue) will plot 
#the true color composite (vegetation in green, water blue etc). 
#Selecting r = 5 (NIR), g = 4 (red), b = 3(green) will plot the false color 
#composite (very popular in remote sensing with vegetation as red). 
plotRGB(r, r = 4, g = 3, b = 2, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite") 
plotRGB(r, r = 5, g = 4, b = 3, axes = TRUE, stretch = "lin", main = "Landsat False Color Composite")
plotRGB(r, r = 4, g = 3, b = 2,  stretch = "lin")
plotRGB(r, r = 5, g = 4, b = 3,  stretch = "lin")
#Page 5 crop
e <- extent(400000, 410000, 4450000, 4460000)
r_e <- crop(r, e) 
plotRGB(r_e, r = 5, g = 4, b = 3,  stretch = "lin")
#For example, fact=3 will result in a new Raster object with 3*3=9 times fewer cells and 3 times
#both x and y resolution fact=c(2,3), the first will be used for aggregating in the horizontal
#direction, and the second for aggregating in the vertical direction, and the returned object 
#will have 2*3=6 times fewer cells. Likewise, fact=c(2,3,4) aggregates cells in groups of 
#2 (rows) by 3 (columns) and 4 (layers).
#page 7
r_a <- aggregate(r, fact=3) 
writeRaster(r_a, filename="champaign_aggregate3.tif", format="GTiff", overwrite=TRUE)
vi <- function(img, i, k)
{
  bi <- img[[i]] 
  bk <- img[[k]] 
  vi <- (bk-bi)/(bk+bi) 
  return(vi) 
}
#NDVI = (NIR -Red)/ (NIR+Red) For champaign_raster.tif NIR = 5, red = 4. 
ndvi <- vi(r, 4,5)
#Moderate values represent shrub and grassland
plot(ndvi, col = rev(terrain.colors(30)))
#Very low values of NDVI (0.1 and below) correspond to barren areas of rock, sand, or snow.
#Moderate values represent shrub and grassland (0.2 to 0.3)
#Highvalues indicate temperate and tropical rainforests (0.3 to 0.8).
veg <- calc(ndvi, function(x){x[x < 0.1] <- NA; return(x)})
plot(veg, main = 'Veg cover')

#You can also create classes for di???erent density of vegetation, 0 being no-vegetation.
vegc <- reclassify(veg, c(-Inf,0.1,0, 0.1,0.2,1, 0.2,0.3,2, 0.3,0.4,3, 0.4, Inf, 4)) 
plot(vegc,col = rev(terrain.colors(4)), main = 'NDVI based reclassify')
