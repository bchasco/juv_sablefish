library(ggplot2)

# s <- prettyVastPlot(fit,
#                          re = "s",
#                          n_vir = 7, min_v = 2,
#                          bnd1_convex = -0.06,
#                          max_v = 8,
#                          ncave = 17)
st <- prettyVastPlot(fit,
                         re = "st",
                         n_vir = 7, min_v = 2,
                         bnd1_convex = -0.06,
                         max_v = 8,
                         ncave = 17)
years_to_include <- c(1998:2022)
st$loc_xy <- st$loc_xy[st$loc_xy$yr%in%years_to_include,]
st$loc_xy$yr <- as.factor(st$loc_xy$yr)
all <- rbind(st$loc_xy)
all_pin <- c(st$pin)


df <- sdmTMB::add_utm_columns(as.data.frame(fit$data_frame[,c("Lat_i","Lon_i", "t_i", "c_iz", "b_i")]),
                              ll_names=c("Lon_i","Lat_i"),
                              utm_names = c("utm_lon","utm_lat"),
                              utm_crs = "+proj=utm +zone=10 +datum=WGS84",
                              units = "km")
df$b_i <- units::drop_units(df$b_i)
df$yr <- as.character(df$t_i)

cat <- c("Subyearling \nChinook", 
         "Yearling \nChinook",
         "Yearling \nCoho", 
         "Sablefish"
         )
df$cat <- cat[df$c_iz+1]
df$val <- log(df$b_i)
# df$yr <- df
pnts <- df %>%
  st_as_sf(coords = c("utm_lon", "utm_lat"), crs = "+proj=utm +zone=10 +datum=WGS84")


#Grab the spatial data you need
world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')

world2 <- sf::st_transform(world,crs="+proj=utm +zone=10 +datum=WGS84  +units=km")

for(i in 1:4){
  p <- ggplot(data = world2) +
    coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
    xlim(300,510)+
    ylim(4925,5400)+
    scale_x_continuous(
      limits = c(300,470),
      breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
    ) +
    ggplot2::geom_raster(data = na.omit(all[all_pin==1 & all$cat==cat[i],]), aes(x = x, y = y, fill= catch*encounter)) +
    scale_fill_gradientn(colors = viridis_pal()(7), limits=c(0,7.5))+
    guides(fill = guide_colourbar(reverse = TRUE)) +
    facet_wrap( ~ yr, ncol = 7) +
    # guides(fill = guide_legend(reverse=TRUE)) +
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
    geom_sf()+
    theme_bw() +
    ylab('') +
    xlab('') +
    geom_point(data = df[df$t_i%in%years_to_include & !is.finite(df$val) & df$c_iz == (i-1),],
               aes(x=utm_lon, y = utm_lat),
               color = "white",
               alpha = 0.5,
               shape = 3,
               size = 0.7)+
    geom_point(data = df[df$t_i%in%years_to_include & df$c_iz == (i-1),],
               aes(x=utm_lon, y = utm_lat, size = val),
               color = "red",
               alpha = 0.1,
               shape = 16) +
    labs(fill="Predicted\nlog(catch km^2)", size="Observed\nlog(catch km^2)")
  
  # print(p)
  file_sp <- c("Subyearling_Chinook", 
               "Yearling_Chinook",
               "Yearling_Coho", 
               "Sablefish"
  )[i]
  
  ggsave(paste0("C:\\noaa\\projects\\juv_sablefish\\output\\Supplemental_",file_sp,".png"), p, width = 6, height = 7, units="in")
  
}
