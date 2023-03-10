p <- list()
l <- list()

p[[1]] <- prettyVastPlot(fit, process = "catch", 
                          re = "s", 
                          n_vir = 7, min_v = 2, 
                          max_v = 8, 
                          ncave = 17)$p
l[[1]] <- prettyVastPlot(fit, process = "catch", 
                         re = "s", 
                         n_vir = 7, min_v = 2, 
                         max_v = 8, 
                         ncave = 17)$loc_xy

p[[2]] <- prettyVastPlot(fit, process = "catch", 
                         re = "st", 
                         n_vir = 7, min_v = 2, 
                         max_v = 8, 
                         ncave = 17)$p
l[[2]] <- prettyVastPlot(fit, process = "catch", 
                         re = "st", 
                         n_vir = 7, min_v = 2, 
                         max_v = 8, 
                         ncave = 17)$loc_xy

p[[3]] <- prettyVastPlot(fit, process = "encounter", 
                         re = "s", 
                         n_vir = 6, min_v = 0, 
                         max_v = 1, 
                         ncave = 17)$p
l[[3]] <- prettyVastPlot(fit, process = "encounter", 
                         re = "s", 
                         n_vir = 6, min_v = 0, 
                         max_v = 1, 
                         ncave = 17)$loc_xy

p[[4]] <- prettyVastPlot(fit, process = "encounter", 
                         re = "st", 
                         n_vir = 6, min_v = 0, 
                         max_v = 1, 
                         ncave = 17)$p
l[[4]] <- prettyVastPlot(fit, process = "encounter", 
                         re = "st", 
                         n_vir = 6, min_v = 0, 
                         max_v = 1, 
                         ncave = 17)$loc_xy

lcatch <- rbind(l[[1]],l[[2]])
lencounter <- rbind(l[[3]],l[[4]])

df <- sdmTMB::add_utm_columns(as.data.frame(fit$data_frame[,c("Lat_i","Lon_i")]),
                              ll_names=c("Lon_i","Lat_i"),
                              utm_names = c("utm_lon","utm_lat"),
                              utm_crs = "+proj=utm +zone=10 +datum=WGS84",
                              units = "km")
pnts <- df %>%
  st_as_sf(coords = c("utm_lon", "utm_lat"), crs = "+proj=utm +zone=10 +datum=WGS84")

#Create a polygon
polygon <- concaveman(pnts, concavity = 17)
polygon <- st_coordinates(polygon)
pin <- sp::point.in.polygon(lcatch$x[],
                            lcatch$y[],
                            polygon[,1],
                            polygon[,2])
l$pin <- pin

#Grab the spatial data you need
world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')

world2 <- sf::st_transform(world,crs="+proj=utm +zone=10 +datum=WGS84  +units=km")

p <- list()

p[[1]] <- ggplot(data = world2) +
  coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
  xlim(310,510)+
  ylim(4925,5375)+
  scale_x_continuous(
    limits = c(310,470),
    breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
  )

p[[1]] <- p[[1]] +   ggplot2::geom_raster(data = lcatch[pin==1,], aes(x = x, y = y, fill= val)) +
  scale_fill_gradientn(colors = viridis_pal()(6), limits=c(2, 8)) +
  facet_grid(spatial ~ cat) + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  guides(fill=guide_legend("log(Catch)")) 

p[[1]] <- p[[1]] +
  geom_sf()+
  theme_bw() +
  ylab('') +
  xlab('')

p[[2]] <- ggplot(data = world2) +
  coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
  xlim(310,510)+
  ylim(4925,5375)+
  scale_x_continuous(
    limits = c(310,470),
    breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
  )

p[[2]] <- p[[2]] +   ggplot2::geom_raster(data = lencounter[pin==1,], aes(x = x, y = y, fill= val)) +
  scale_fill_gradientn(colors = viridis_pal()(7), limits=c(0, 1)) +
  facet_grid(spatial ~ cat) + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  guides(fill=guide_legend("Encounter")) 

p[[2]] <- p[[2]] +
  geom_sf()+
  theme_bw() +
  ylab('') +
  xlab('')


png("prettyVASToutput_catch.png")
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

