prettyVastPlot <- function(fit, 
                   process = "catch", 
                   re = "s",
                   n_vir = 8,
                   min_v = 2, 
                   max_v = 8,
                   ncave = 10){
  library(data.table)
  library(sf)
  library(tidyr)
  library(dplyr)
  library(concaveman)
  library(RANN)
  library(ggplot2)
  library(viridis)
  library(viridisLite)
  
  
  df <- sdmTMB::add_utm_columns(as.data.frame(fit$data_frame[,c("Lat_i","Lon_i")]),
                                ll_names=c("Lon_i","Lat_i"),
                                utm_names = c("utm_lon","utm_lat"),
                                utm_crs = "+proj=utm +zone=10 +datum=WGS84",
                                units = "km")
  pnts <- df %>%
    st_as_sf(coords = c("utm_lon", "utm_lat"), crs = "+proj=utm +zone=10 +datum=WGS84")


  #Model categories
  cat <- c("Subyearling \nChinook", 
           "Yearling \nChinook",
           "Yearling \nCoho", 
           "Sablefish")



  #Processes with different random effects
  if(process == "encounter"){
    if(re == "s"){
      om <- t(t(fit$Report$Omega1_sc[,]) + fit$Report$beta1_tc[1,])
    }
    if(re == "st"){
      om <- t(t(fit$Report$Omega1_sc[,] +fit$Report$Epsilon1_sct[,,23]) + fit$Report$beta1_tc[1,])
    }
  }
  if(process == "catch"){
    if(re == "s"){
      # Equilibrium catch rate
      om <- t(fit$Report$beta2_tc[1,] +t(fit$Report$Omega2_sc[,]))
    }
    if(re == "st"){
      #2020 catch rate
      om <- t(fit$Report$beta2_tc[1,] + t(fit$Report$Omega2_sc[,] + fit$Report$Epsilon2_sct[,,23]))
    }
  }


  #grid
  loc <- as.data.frame(fit$spatial_list$MeshList$anisotropic_mesh$loc[,1:2])
  x_seq <- seq(min(loc[,1]),max(loc[,1]), length.out=100)
  y_seq <- seq(min(loc[,2]),max(loc[,2]), length.out=100)
  loc_xy <- as.data.frame(expand.grid(x_seq,y_seq))
  
  names(loc_xy) <- c('x','y')
  for(j in 1:4){
    nn <- nn2(loc, loc_xy[,c('x','y')], k = 1)
    if(process == "encounter"){
      loc_xy[,cat[j]] <- plogis(om[nn$nn.idx,j])
    }
    if(process == "catch"){
      loc_xy[,cat[j]] <- om[nn$nn.idx[,1],j]
    }
  }
  loc_xy <- loc_xy %>%
    pivot_longer(!c(x,y), names_to = "cat", values_to = "val")

  loc_xy$process <- process
  if(re =="s"){
    loc_xy$spatial <- "Equilibrium"
  }else{
    loc_xy$spatial <- "2020"
  }
  
  #Create a polygon
  polygon <- concaveman(pnts, concavity = ncave)
  polygon <- st_coordinates(polygon)
  pin <- sp::point.in.polygon(loc_xy$x[],
                              loc_xy$y[],
                              polygon[,1],
                              polygon[,2])

  #Grab the spatial data you need
  world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
  usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')

  world2 <- sf::st_transform(world,crs="+proj=utm +zone=10 +datum=WGS84  +units=km")

  p <- ggplot(data = world2) +
    coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
    xlim(310,510)+
    ylim(4900,5375)+
    scale_x_continuous(
      limits = c(310,470),
      breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
    )

  p <- p +   ggplot2::geom_raster(data = loc_xy[pin==1,], aes(x = x, y = y, fill= val)) +
    scale_fill_gradientn(colors = viridis_pal()(n_vir), limits=c(min_v, max_v)) +
    facet_wrap(~cat) + 
    theme(plot.margin = margin(0, 0, 0, 0, "cm"))

  p <- p +
    geom_sf()+
    theme_bw() +
    ylab('') +
    xlab('')

  if(process == "encounter"){
    p <- p + guides(fill=guide_legend("Encounter")) 
  }  
  if(process == "catch"){
    p <- p + guides(fill=guide_legend("log(Catch)")) 
  }  
  
  return(list(p = p,
              loc_xy = loc_xy))
  
}

