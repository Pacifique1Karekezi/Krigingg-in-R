library(sp)
library(gstat)
library(dplyr) # for "glimpse"
library(ggplot2)
library(scales) # for "comma"
library(magrittr)
data(meuse)
glimpse(meuse)
#?meuse
meuse %>% as.data.frame %>% 
ggplot(aes(x, y)) + geom_point(aes(size=zinc), color="blue", alpha=3/4) + 
  ggtitle("Zinc Concentration (ppm)") + coord_equal() + theme_bw()

#Creating a SPDF
coordinates(meuse)<-~x+y
class(meuse)

bbox(meuse)
coordinates(meuse) %>% glimpse
identical( bbox(meuse), meuse@bbox )
identical( coordinates(meuse), meuse@coords )
meuse@data %>% glimpse
meuse %>% as.data.frame %>% glimpse


# Fitting a variogram

lzn.vgm <- variogram(log(zinc)~1, meuse) # calculates sample variogram values 
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900, 1)) # fit model

#show.vgms()

plot(lzn.vgm, lzn.fit) # plot the sample values, along with the fit model

# Performing Kriging

# load spatial domain to interpolate over
data("meuse.grid")

# to compare, recall the bubble plot above; those points were what there were values for. this is much more sparse
plot1 <- meuse %>% as.data.frame %>%
  ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

# this is clearly gridded over the region of interest
plot2 <- meuse.grid %>% as.data.frame %>%
  ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate")

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

#COMPUTATION

coordinates(meuse.grid) <- ~ x + y # step 3 above
lzn.kriged <- krige(log(zinc) ~ 1, meuse, meuse.grid, model=lzn.fit)

lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()


# APPLICATION OF KRIGING Over KIGALI SIMULATED DATASET

simulated <-RDT_gaussian_field

grids <- shp %>% 
  st_make_grid(cellsize = 0.001, what = "centers") %>% # grid of points
  st_intersection(shp)

#grids<-as(grids, "Spatial")

ggplot() + 
  geom_sf(data = shp) + 
  geom_sf(data = grids)

# To convert into SPDF

grids<-as(grids, "Spatial")
simulated<-as(simulated, "Spatial")

# Fitting a variogram

lzn.vgm_k <- variogram(log(abs(sim1))~1, simulated) # calculates sample variogram values 
lzn.fit_k <- fit.variogram(lzn.vgm_k, model=vgm(NA,"Exp",1,0.00004)) # fit model

#show.vgms()

plot(lzn.vgm_k, lzn.fit_k) # plot the sample values, along with the fit model


# to compare, recall the bubble plot above; those points were what there were values for. this is much more sparse
plot11 <- simulated %>% as.data.frame %>%
  ggplot(aes(coords.x1, coords.x2)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

# this is clearly gridded over the region of interest
plot22 <- grids %>% as.data.frame %>%
  ggplot(aes(coords.x1, coords.x2)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate")

library(gridExtra)
grid.arrange(plot11, plot22, ncol = 2)
#View(as.data.frame(simulated))

lzn.kriged_k <- krige(log(abs(sim1)) ~ 1, simulated, grids, model=lzn.fit_k)

lzn.kriged_k  %>% as.data.frame %>%
  ggplot(aes(x=coords.x1, y=coords.x2)) + geom_point(aes(fill=var1.pred, color=var1.pred))+
  scale_color_gradient(low = "yellow", high = "black")

View(as.data.frame(lzn.kriged_k))  
