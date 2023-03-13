l <- list()
l[[1]] <- prettyVastPlot(fit, process = "catch",
                         re = "s",
                         n_vir = 7, min_v = 2,
                         bnd1_convex = -0.06,
                         max_v = 8,
                         ncave = 17)$loc_xy
l[[2]] <- prettyVastPlot(fit, process = "catch",
                         re = "st",
                         n_vir = 7, min_v = 2,
                         bnd1_convex = -0.08,
                         max_v = 8,
                         ncave = 17)$loc_xy
l[[3]] <- prettyVastPlot(fit, process = "encounter",
                         re = "s",
                         n_vir = 6, min_v = 0,
                         bnd1_convex = -0.07,
                         max_v = 1,
                         ncave = 17)$loc_xy
l[[4]] <- prettyVastPlot(fit, process = "encounter",
                         re = "st",
                         n_vir = 6, min_v = 0,
                         bnd1_convex = -0.07,
                         max_v = 1,
                         ncave = 17)$loc_xy

#Eventhough prettyVASTplot produces the plots
#You need to do this so you can combine the different categories 
lcatch <- rbind(l[[1]],l[[2]])
lencounter <- rbind(l[[3]],l[[4]])

df <- sdmTMB::add_utm_columns(as.data.frame(fit$data_frame[,c("Lat_i","Lon_i", "t_i", "c_iz", "b_i")]),
                              ll_names=c("Lon_i","Lat_i"),
                              utm_names = c("utm_lon","utm_lat"),
                              utm_crs = "+proj=utm +zone=10 +datum=WGS84",
                              units = "km")
df$b_i <- drop_units(df$b_i)
df$spatial <- as.character(df$t_i)

cat <- c("Subyearling \nChinook", 
         "Yearling \nChinook",
         "Yearling \nCoho", 
         "Sablefish")
df$cat <- cat[df$c_iz+1]
df$val <- log(df$b_i)
df <- df
pnts <- df %>%
  st_as_sf(coords = c("utm_lon", "utm_lat"), crs = "+proj=utm +zone=10 +datum=WGS84")

#Create a polygon
# polygon <- concaveman(pnts, concavity = 17)
# polygon <- st_coordinates(polygon)
# pin <- sp::point.in.polygon(lcatch$x[],
#                             lcatch$y[],
#                             polygon[,1],
#                             polygon[,2])

#  #Create an edge polygon using INLA
max.edge = c(10, 10)
cutoff = 2
max.n = c(100,100)
bnd1 <- as.data.frame(INLA::inla.nonconvex.hull(as.matrix(cbind(df$utm_lon,df$utm_lat)), convex = bnd1_convex)$loc)
names(bnd1) <- c("E_km", "N_km")
bnd1 <- bnd1 %>%
  st_as_sf(coords = c("E_km", "N_km"), crs = "+proj=utm +zone=10 +datum=WGS84")

polygon <- st_coordinates(bnd1)
pin <- sp::point.in.polygon(lcatch$x[],
                            lcatch$y[],
                            polygon[,1],
                            polygon[,2])

#Grab the spatial data you need
world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')

world2 <- sf::st_transform(world,crs="+proj=utm +zone=10 +datum=WGS84  +units=km")

p <- list()

p[[1]] <- ggplot(data = world2) +
  coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
  xlim(300,510)+
  ylim(4925,5400)+
  scale_x_continuous(
    limits = c(300,470),
    breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
  ) +
  ggplot2::geom_raster(data = lcatch[pin==1,], aes(x = x, y = y, fill= val)) +
  scale_fill_gradientn(colors = viridis_pal()(7), limits=c(2, 8)) +
  facet_grid(spatial ~ cat) +
  # guides(fill = guide_legend(reverse=TRUE)) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf()+
  theme_bw() +
  ylab('') +
  xlab('') +
  geom_point(data = df[df$t_i==2020 & !is.finite(df$val),], 
             aes(x=utm_lon, y = utm_lat),
             color = "red", 
             alpha = 1,
             shape = 3)+
  geom_point(data = df[df$t_i==2020,], 
             aes(x=utm_lon, y = utm_lat, size = val),
             color = "red", 
             alpha = 0.3,
             shape = 16) +
  labs(fill="", size="log(catch km^2)")

p[[2]] <- ggplot(data = world2) +
  coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
  xlim(310,510)+
  ylim(4925,5375)+
  scale_x_continuous(
    limits = c(310,470),
    breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
  ) +   
  ggplot2::geom_raster(data = lencounter[pin==1,], aes(x = x, y = y, fill= val)) +
  scale_fill_gradientn(colors = viridis_pal()(7), limits=c(0, 1)) +
  facet_grid(spatial ~ cat) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  guides(fill=guide_legend("Encounter\nrate")) +
  geom_sf()+
  theme_bw() +
  ylab('') +
  xlab('') + 
  geom_point(data = df[df$t_i==2020 & !is.finite(df$val),], 
             aes(x=utm_lon, y = utm_lat),
             color = "red", 
             shape = 3)+
  geom_point(data = df[df$t_i==2020 & is.finite(df$val),], 
             aes(x=utm_lon, y = utm_lat),
             color = "red", 
             shape = 16)+
  labs(fill="", size="Encounter rate")


png("prettyVASToutput_catch.png", width = 780, height = 780, units = "px")
ggpubr::ggarrange(p[[1]],
                        common.legend = FALSE,
                        legend = 'right')
dev.off()

png("prettyVASToutput_encounter.png")
ggpubr::ggarrange(p[[2]],
                  # nrow= 2,
                  common.legend = FALSE,
                  legend = 'right')
dev.off()

