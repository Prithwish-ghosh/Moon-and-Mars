

mars <- read_csv("Mars Crater info.csv")
mars$LONGITUDE_CIRCLE_IMAGE = circular(mars$LONGITUDE_CIRCLE_IMAGE)
mars$LATITUDE_CIRCLE_IMAGE  = circular(mars$LATITUDE_CIRCLE_IMAGE)
head(mars)

View(mars)


watson.test(mars$LATITUDE_CIRCLE_IMAGE, alpha = 0.01, dist = "vonmises")
watson.test(mars$LONGITUDE_CIRCLE_IMAGE, alpha = 0.01, dist = "vonmises")

watson.test(mars$LATITUDE_CIRCLE_IMAGE, alpha = 0.01, dist = "uniform")
watson.test(mars$LONGITUDE_CIRCLE_IMAGE, alpha = 0.01, dist = "uniform")


dd <- mars[sample(nrow(mars), 10000), ]

dd = cbind(dd$LATITUDE_CIRCLE_IMAGE , dd$LONGITUDE_CIRCLE_IMAGE)

set.seed(2022)
EvMFs <- 
  function(K){
    movMF(dd, k = K, control= list(nruns = 20))
  }

Esd = lapply(1:10, EvMFs)
gt = sapply(Esd, BIC)
gt
Esd


crat.densities <- vmf_density_grid(dd,
                                   ngrid = 300)

crat.densities
dens_data <- subset(crat.densities, Density > 0 & Density < 0.25)
dim(dens_data)

m.fireball <- ggplot(mars, aes(x = LONGITUDE_CIRCLE_IMAGE, 
                               y = LATITUDE_CIRCLE_IMAGE)) +
  geom_point(data = mars,
             mapping = aes(x = LONGITUDE_CIRCLE_IMAGE, 
                           y = LATITUDE_CIRCLE_IMAGE),
             color = "red", alpha = .3, size = 0.1, stroke = 0.1) +
  geom_point(data = dens_data,
             mapping = aes(x = Long, 
                           y = Lat),
             color = "black", alpha = .3, size = 2, stroke = 0.1) +
  geom_density_2d(data = mars,
                  aes(x = LONGITUDE_CIRCLE_IMAGE, 
                      y = LATITUDE_CIRCLE_IMAGE),
                  color = "green", alpha = 1.5) +
  geom_contour(data = crat.densities, aes(x=Long, y=Lat, z=Density),
               color = "blue") +
  scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) +
  scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) +
  coord_map("orthographic", orientation = c(100, 50, 0)) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  ggtitle("Density of Mars") +
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

m.fireball

# Create a data frame for Mars landing locations
mars_landings <- data.frame(
  Mission = c("Viking 1 Lander", "Viking 2 Lander", "Mars Pathfinder", "Spirit Rover (MER-A)", "Opportunity Rover (MER-B)", "Curiosity Rover", "Perseverance Rover"),
  Latitude = c(22.27, 47.64, 19.33, -14.5684, -1.9462, -4.5895, 18.4447),
  Longitude = c(47.95, -134.29, 33.55, 175.4726, -5.5257, 137.4417, -77.4508)
)

# Print the data frame
print(mars_landings)


# Load necessary packages
library(ggplot2)
library(png)

# Read Mars map image
mars_map <- readPNG("mars.png")
dim(mars_map)
# Define Mars map dimensions
mars_width <- dim(mars_map)[2]
mars_height <- dim(mars_map)[1]

# Create ggplot object with Mars map background
gg <- ggplot() +
  annotation_raster(mars_map, xmin=-180, xmax=180, ymin=-90, ymax=90) +
  coord_fixed(ratio = mars_height/mars_width)

gg_ff <- gg + 
  geom_point(data = dens_data,
             mapping = aes(x = Long, 
                           y = Lat),
             color = "black", alpha = 1, size = 2, stroke = 0.1) +
  theme(legend.position = "bottom")

gg_ff

# Add data points to the plot
gg_f <- gg + 
  geom_point(data = dens_data,
             mapping = aes(x = Long, 
                           y = Lat),
             color = "black", alpha = 1, size = 1, stroke = 0.1) +
  geom_density_2d(data = mars,
                  aes(x = LONGITUDE_CIRCLE_IMAGE, 
                      y = LATITUDE_CIRCLE_IMAGE),
                  color = "green", alpha = 1.5) +
  geom_contour(data = crat.densities, aes(x=Long, y=Lat, z=Density),
               color = "blue") + 
  geom_point(data = mars_landings,
             mapping = aes(x = Longitude, 
                           y = Latitude),
             size = 3, pch = 23, fill = "yellow") +
  theme(legend.position = "bottom")+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

# Show plot
print(gg_f)

library(raster)
library(rgl)

gg3d <- ggplot() +
  annotation_raster(mars_map, xmin=-180, xmax=180, ymin=-90, ymax=90) +
  coord_fixed(ratio = mars_height/mars_width)

d3m.fireball <-  gg3d+
  geom_point(data = mars,
             mapping = aes(x = LONGITUDE_CIRCLE_IMAGE, 
                           y = LATITUDE_CIRCLE_IMAGE),
             color = "red", alpha = .3, size = 0.1, stroke = 0.1) +
  geom_point(data = dens_data,
             mapping = aes(x = Long, 
                           y = Lat),
             color = "black", alpha = .3, size = 2, stroke = 0.1) +
  geom_density_2d(data = mars,
                  aes(x = LONGITUDE_CIRCLE_IMAGE, 
                      y = LATITUDE_CIRCLE_IMAGE),
                  color = "green", alpha = 1.5) +
  geom_contour(data = crat.densities, aes(x=Long, y=Lat, z=Density),
               color = "blue") +
  scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) +
  scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) +
  coord_map("orthographic", orientation = c(100, 50, 0)) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  ggtitle("Density of Mars") +
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


d3m.fireball

mm.fireball <- ggplot(mars, aes(x = LONGITUDE_CIRCLE_IMAGE, 
                                y = LATITUDE_CIRCLE_IMAGE)) +
  geom_point(data = mars,
             mapping = aes(x = LONGITUDE_CIRCLE_IMAGE, 
                           y = LATITUDE_CIRCLE_IMAGE),
             color = "red", alpha = .3, size = 0.1, stroke = 0.1) +
  geom_point(data = dens_data,
             mapping = aes(x = Long, 
                           y = Lat),
             color = "black", alpha = .3, size = 2, stroke = 0.1) +
  geom_point(data = mars_landings,
             mapping = aes(x = Longitude, 
                           y = Latitude),
             size = 8, pch = 23, fill = "yellow") +
  geom_density_2d(data = mars,
                  aes(x = LONGITUDE_CIRCLE_IMAGE, 
                      y = LATITUDE_CIRCLE_IMAGE),
                  color = "green", alpha = 1.5) +
  geom_contour(data = crat.densities, aes(x=Long, y=Lat, z=Density),
               color = "blue") +
  theme(legend.position = "bottom")

mm.fireball

watson.test(fireball$fireball.Latitude , alpha = 0.05 , dist = "vonmises")
watson.test(fireball$fireball.Longitude , alpha = 0.05 , dist = "vonmises")

xy_x= euclid(dd)
xy_x
Directional::fishkent(xy_x)

pp = function (x,col,col1, ref.line = TRUE )
{
  n <- length(x)
  x <- sort(x%%(2 * pi))
  z <- c(1:n)/(n + 1)
  mu <- circ.mean(x)%%(2 * pi)
  kappa <- est.kappa(x)
  y <- c(1:n)
  for (i in 1:n) {
    y[i] <- pvm(x[i], mu, kappa)
  }
  plot(z, y, xlab = "Von Mises Distribution", ylab = "Empirical Distribution" , col = col , lwd = 0.75)
  if (ref.line) 
    abline(0, 1 , col = col1 , lwd = 3.0)
  data.frame(mu, kappa)
}

pp(mars$LONGITUDE_CIRCLE_IMAGE , col = "red" , col1 = "blue" , ref.line = TRUE)
pp(mars$LATITUDE_CIRCLE_IMAGE, col = "red" , col1 = "blue", ref.line = TRUE)

ppunif = function (x, ref.line = TRUE, frac = NULL, xlab = "Uniform Distribution", 
                   ylab = "Empirical Distribution", col = NULL, col.inf = NULL, 
                   col.sup = NULL, col1,...) 
{
  x <- na.omit(x)
  if (length(x) == 0) {
    warning("No observations (at least after removing missing values)")
    return(NULL)
  }
  x <- conversion.circular(x, units = "radians", zero = 0, 
                           rotation = "counter", modulo = "2pi")
  attr(x, "class") <- attr(x, "circularp") <- NULL
  y <- sort(x%%(2 * pi))/(2 * pi)
  n <- length(y)
  z <- (1:n)/(n + 1)
  if (is.null(col)) 
    col <- rep(1, n)
  else col <- rep(col, length.out = n)
  if (!is.null(frac)) {
    if (!is.numeric(frac) || (frac < 0 | frac > 1)) {
      stop("'frac' must be in the interval [0,1]")
    }
    f <- round(frac * n)
    if (f) {
      zm <- -1 + ((n - f + 1):n)/(n + 1)
      zp <- 1 + (1:f)/(n + 1)
      ym <- -1 + y[(n - f + 1):n]
      yp <- 1 + y[1:f]
      y <- c(ym, y, yp)
      z <- c(zm, z, zp)
      if (is.null(col.inf)) 
        col.inf <- rep(2, f)
      else col.inf <- rep(col.inf, length.out = f)
      if (is.null(col.sup)) 
        col.sup <- rep(2, f)
      else col.sup <- rep(col.sup, length.out = f)
      col <- c(col.inf, col, col.sup)
    }
  }
  plot.default(z, y, xlab = xlab, ylab = ylab, col = col, ...)
  if (ref.line) {
    abline(0, 1 , col = col1, lwd = 3.0)
    if (!is.null(frac)) {
      abline(h = c(0, 1), lty = 3)
      abline(v = c(0, 1), lty = 3)
    }
  }
}

ppunif(mars$LATITUDE_CIRCLE_IMAGE , col = "red" , col1 = "blue")
ppunif(mars$LONGITUDE_CIRCLE_IMAGE , col = "red" , col1 = "blue")

x= mars$LATITUDE_CIRCLE_IMAGE
y <- rvonmises(n=1000, mu=circ.mean(mars$LATITUDE_CIRCLE_IMAGE), kappa=est.kappa(mars$LATITUDE_CIRCLE_IMAGE))
resx <- density(x, bw=25)
resy <- density(y, bw=25)
pp=plot(density.circular(mars$LATITUDE_CIRCLE_IMAGE , bw = 30) ,points.plot=F, pty = 10, lty=1, lwd=2, col="blue",xlim=c(-1.2,1), ylim=c(-1.1, 1.2), main="Comparison of  estimated sample density\n with fitted Von Mises\n for Martian Latitude data", cex.main=0.25)
lines(resy, points.plot=F, col="red", points.col=2,lwd=2, lty=4, plot.info=pp)


x= mars$LONGITUDE_CIRCLE_IMAGE
y <- rvonmises(n=1000, mu=circ.mean(mars$LONGITUDE_CIRCLE_IMAGE), kappa=est.kappa(mars$LONGITUDE_CIRCLE_IMAGE))
resx <- density(x, bw=25)
resy <- density(y, bw=25)
pp=plot(density.circular(mars$LONGITUDE_CIRCLE_IMAGE , bw = 30) ,points.plot=F, pty = 10, lty=1, lwd=2, col="blue",xlim=c(-1.2,1), ylim=c(-1.1, 1.2), main="Comparison of  estimated sample density\n with fitted Von Mises\n for Martian Longitude data", cex.main=0.25)
lines(resy, points.plot=F, col="red", points.col=2,lwd=2, lty=4, plot.info=pp)

legend("topleft", legend=c("estimated  \n kernel density", "von mises density"),
       col=c("blue", "red"), lty=c(1,4),lwd=c(2,2), cex=0.59,
       box.lty=0)

ss = euclid(dd)
sphereplot(ss)

