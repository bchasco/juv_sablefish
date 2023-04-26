
prettyData <- function(fit = fit){
  
  library(sf)
  
  df <- sdmTMB::add_utm_columns(as.data.frame(fit$data_frame[,c("Lat_i","Lon_i", "t_i", "c_iz", "b_i")]),
                                ll_names=c("Lon_i","Lat_i"),
                                utm_names = c("utm_lon","utm_lat"),
                                utm_crs = "+proj=utm +zone=10 +datum=WGS84",
                                units = "km")
  df$b_i <- units::drop_units(df$b_i)
  df$yr <- as.character(df$t_i)
  
  mesh <- as.data.frame(fit$spatial_list$MeshList$anisotropic_spde$mesh$loc[,1:2])
  names(mesh) <- c("x","y")
  
  cat <- c("Subyearling \nChinook", 
           "Yearling \nChinook",
           "Yearling \nCoho", 
           "Sablefish")
  df$cat <- cat[df$c_iz+1]
  df$val <- log(df$b_i)
  # df$yr <- df
  pnts <- df %>%
    st_as_sf(coords = c("utm_lon", "utm_lat"), crs = "+proj=utm +zone=10 +datum=WGS84")
  
  
  #Grab the spatial data you need
  world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
  usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')
  
  world2 <- sf::st_transform(world,crs="+proj=utm +zone=10 +datum=WGS84  +units=km")
  
  
  p <- ggplot(data = world2) +
    coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
    xlim(300,510)+
    ylim(4925,5400)+
    scale_x_continuous(
      limits = c(300,470),
      breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
    ) +
    # guides(fill = guide_legend(reverse=TRUE)) +
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
    geom_sf()+
    theme_bw() +
    ylab('') +
    xlab('') +
    ggplot2::geom_point(data = df[,],
               aes(x=utm_lon, y = utm_lat),
               color = "black",
               alpha = 0.5,
               shape = 16,
               size = 1) +
    facet_wrap(~t_i, ncol = 6)

  print(p)
  
  ggsave("prettyData.png", p, width = 8, height = 10, units="in", dpi = 500)
  
}

prettyData(fit = fit)
