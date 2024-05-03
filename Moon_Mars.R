library(sf)
options(rgl.useNULL = TRUE)  # For Macbook
library(rgl)
library(Directional)
library(circular)
library(CircStats)
library(tidyverse)
library(Matrix)
library(mixtools)
library(ggplot2)
library(skmeans)
library(hrbrthemes)  # hrbrmstr themes
library(magick)      # For animation
library(mapproj)     # Needed for projection
theme_set(theme_ipsum())
library(NPCirc)
library(rgl)
library(colorspace)
library(mclust)
library(maps)
library(movMF)
library(maptools)
#  Meteor lalnding(1) dataset 

library(readr)

lat_lon_to_xyz <- function(lat, lon, radius = 1) {
  lat_rad <- lat * pi / 180  # Convert latitude to radians
  lon_rad <- lon * pi / 180  # Convert longitude to radians
  
  x <- radius * cos(lat_rad) * cos(lon_rad)
  y <- radius * cos(lat_rad) * sin(lon_rad)
  z <- radius * sin(lat_rad)
  
  return(cbind(x, y, z))
}

vmf_density_grid = 
  function(u, ngrid = 100) {
    # Translate to (0,180) and (0,360)
    u[,1] <- u[,1] + 90
    u[,2] <- u[,2] + 180
    res <- vmf.kerncontour(u, thumb = "none", den.ret = T, full = T,
                           ngrid = ngrid)
    
    # Translate back to (-90, 90) and (-180, 180) and create a grid of
    # coordinates
    ret <- expand.grid(Lat = res$lat - 90, Long = res$long - 180)
    ret$Density <- c(res$den)
    ret
  }



mars <- read_csv("Lunar_impact.csv")
fireball = data.frame(mars)
dim(fireball)
head(fireball)

sum(is.na(fireball$LONGITUDE_CIRCLE_IMAGE))
#View(fireball)
head(fireball)
tail(fireball)
fireball$X4..Longitude.... = circular(fireball$X4..Longitude....)
fireball$X3..Latitude....  = circular(fireball$X3..Latitude....)

watson.test(fireball$X3..Latitude...., alpha = 0.01, dist = "vonmises")
watson.test(fireball$X4..Longitude...., alpha = 0.01, dist = "vonmises")

watson.test(fireball$X3..Latitude...., alpha = 0.01, dist = "uniform")
watson.test(fireball$X4..Longitude...., alpha = 0.01, dist = "uniform")

head(fireball)
dim(fireball)

fire = cbind(fireball$X3..Latitude.... , fireball$X4..Longitude....)
set.seed(2022)
EvMFs <- 
  function(K){
    movMF(fire, k = K, control= list(nruns = 20))
  }

Esd = lapply(1:10, EvMFs)
gt = sapply(Esd, BIC)
gt
Esd

#View(fireball)
final_data = fireball
dim(final_data)
tail(final_data)

fireball.densities <- vmf_density_grid(fire,
                                       ngrid = 300);
summary(fireball.densities)
dens_data_m <- subset(fireball.densities, Density > 0 & Density < 1)
dim(dens_data_m)

m.moon <- ggplot(fireball1, aes(x = fireball1$X4..Longitude...., 
                               y = fireball1$X3..Latitude....)) +
  geom_point(data = fireball1,
             mapping = aes(x = fireball1$X4..Longitude...., 
                           y = fireball1$X3..Latitude....),
             color = "red", alpha = 1, size = 1, stroke = 0.1) +
  geom_point(data = dens_data_m,
             mapping = aes(x = Long, 
                           y = Lat),
             color = "black", alpha = .3, size = 2, stroke = 0.1) +
  geom_density_2d(data = fireball1,
                  aes(x = fireball1$X4..Longitude...., 
                      y = fireball1$X3..Latitude....),
                  color = "green", alpha = 1.5) +
  geom_contour(data = fireball.densities, aes(x=Long, y=Lat, z=Density),
               color = "blue") +
  scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) +
  scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) +
  coord_map("orthographic", orientation = c(100, 90, 100)) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  ggtitle("Density of Moon") +
  xlab("") +
  ylab("") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.ontop = TRUE,
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_line(color = "black" ),
        panel.background = element_rect(fill = NA))
m.moon

g.fireball <- ggplot(fireball, aes(x = fireball$X4..Longitude...., 
                                   y = fireball$X3..Latitude....)) +
geom_point(data = fireball,
             mapping = aes(x = fireball$X4..Longitude...., 
                           y = fireball$X3..Latitude....),
             color = "red", alpha = .5, size = 1, stroke = 0.1) +
  geom_density_2d(data = fireball,
                  aes(x = fireball$X4..Longitude...., 
                      y = fireball$X3..Latitude....),
                  color = "#B266FF", alpha = 1) +
  geom_contour(data = fireball.densities, aes(x=Long, y=Lat, z=Density),
               color = "blue") +
  scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) +
  scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) +
  coord_map("orthographic", orientation = c(-10, 0, 0)) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  ggtitle("Density of Moon") +
  xlab("") +
  ylab("") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.ontop = TRUE,
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_line(color = "black" ),
        panel.background = element_rect(fill = NA))

g.fireball

summary(fireball$X3..Latitude....)
fireball = data.frame(fireball)


moon_landing_dataset = 
  data.frame(Mission = c("Luna 9", "Luna 13", "Luna 16", "Luna 17", "Luna 20", "Luna 21", "Luna 23",
                         "Luna 24", "Chang'e 3", "Change'e 4", "Change'e 5", "Chandrayaan-3",
                         "Surveyor 1", "Surveyor 3", "Surveyor 5", "Surveyor 6", "Surveyor 7",
                         "Apollo 12", "Apollo 11", "Apollo 14", "Apollo 15","Apollo 16", "SLIM" ), 
             Latitude = c(7.08, 18.87, -0.5137, 38.23764, 3.7863,
                          25.9994, 13, 12.7142,44.1214, -45.5, 43.1,
                          -69.37, -2.46, -3, 1.41, 0.46, -40.97, -3.01239,
                          -0.6875, -3.64530, 26.13239, -8.97301, -13), 
             Longitude = c(-64.37,62.05, 56.3638, -35.00163, 56.6242,
                           30.4076, 62, 62.2129,-19.5117, 177.6, -51.8,
                           32.35, -43.32, -23.41, 23.18, -1.37, -11.44,-23.42157,
                           23.47293, 17.47136, 3.63330, 15.49812,25))
# Subset the data frame to exclude rows with latitude greater than 90
fireball1 <- fireball[fireball$X3..Latitude.... <= 100, ]

moon_map <- readPNG("lroc_color_poles_1k.png")
dim(moon_map)
# Define Mars map dimensions
moon_width <- dim(mars_map)[2]
moon_height <- dim(mars_map)[1]

# Create ggplot object with Mars map background
gg <- ggplot() +
  annotation_raster(moon_map, xmin=-180, xmax=180, ymin=-90, ymax=90) +
  coord_fixed(ratio = moon_height/moon_width)

ma.moon <- gg+
  geom_point(data = fireball1,
             mapping = aes(x = X4..Longitude...., 
                           y = X3..Latitude....),
             color = "red", alpha = 1, size = 0.2, stroke = 0.1) +
  geom_point(data = moon_landing_dataset,
             mapping = aes(x = Longitude, 
                           y = Latitude ),
             color = "purple4", alpha = 1, size = 2, pch = 23, fill = "yellow3") + 
  geom_point(data = dens_data_m,
             mapping = aes(x = Long, 
                           y = Lat),
             color = "black", alpha = .3, size = 2, stroke = 0.1) +
  geom_density_2d(data = fireball1,
                  aes(x = fireball1$X4..Longitude...., 
                      y = fireball1$X3..Latitude....),
                  color = "green", alpha = 1.5) +
  geom_contour(data = fireball.densities, aes(x=Long, y=Lat, z=Density),
               color = "blue")

ma.moon

m.moon <- ggplot(fireball, aes(x = fireball$X4..Longitude...., 
                               y = fireball$X3..Latitude....)) +
  geom_point(data = moon_landing_dataset,
             mapping = aes(x = Longitude, 
                           y = Latitude ),
             color = "purple4", alpha = 1, size = 4, pch = 23, fill = "yellow") + 
  geom_point(data = fireball,
             mapping = aes(x = fireball$X4..Longitude...., 
                           y = fireball$X3..Latitude....),
             color = "red", alpha = .3, size = 1, stroke = 0.1) +
  geom_point(data = dens_data_m,
             mapping = aes(x = Long, 
                           y = Lat),
             color = "black", alpha = .3, size = 2, stroke = 0.1) +
  geom_density_2d(data = fireball,
                  aes(x = fireball$X4..Longitude...., 
                      y = fireball$X3..Latitude....),
                  color = "green", alpha = 1.5) +
  geom_contour(data = fireball.densities, aes(x=Long, y=Lat, z=Density),
               color = "blue") +
  scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) +
  scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) +
  coord_map("orthographic", orientation = c(10, 50, 0)) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  ggtitle("Orthographic Projection of Moon Density", "Top / Front View") +
  xlab("") +
  ylab("") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.ontop = TRUE,
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_line(color = "black" ),
        panel.background = element_rect(fill = NA))
m.moon


moon_landing_dataset
# Display the data frame
View(lunar_landings)

watson.test(fireball$fireball.Latitude , alpha = 0.05 , dist = "vonmises")
watson.test(fireball$fireball.Longitude , alpha = 0.05 , dist = "vonmises")

xy_x= euclid(fire)
Directional::fishkent(xy_x)

# Print the data frame
print(moon.landings)
