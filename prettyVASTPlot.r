prettyVastPlot <- function(fit, 
                   process = "catch", 
                   re = "s",
                   n_vir = 8,
                   min_v = 2, 
                   max_v = 8,
                   ncave = 10,
                   bnd1_convex = -0.06,
                   include_stations = TRUE){
  library(data.table)
  library(sf)
  library(tidyr)
  library(dplyr)
  library(concaveman)
  library(RANN)
  library(ggplot2)
  library(viridis)
  library(viridisLite)
  
  
  df <- sdmTMB::add_utm_columns(as.data.frame(fit$data_frame[,c("Lat_i","Lon_i",'t_i')]),
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
  loc_mat <- matrix(NA, 100,100)
  
  loc_xy <- as.data.frame(expand.grid(x_seq,y_seq))
  names(loc_xy) <- c('x','y')

  for(k in 1:4){
    for(i in 1:100){
      for(j in 1:100){
        om_id <- nn2(loc,matrix(c(x_seq[i],y_seq[j]),1,2),k=1)
        loc_mat[i,j] <- plogis(om[om_id$nn.idx,k])
        if(process == "encounter"){
          loc_mat[i,j] <- plogis(om[om_id$nn.idx,k])
        }
        if(process == "catch"){
          loc_mat[i,j] <- om[om_id$nn.idx,k]
        }
      }
    }
    #This is for smoothing the display
    rd <- raster::disaggregate(raster::raster(t(loc_mat)),
                               fact = c(1,1),
                               method="bilinear")
    rd <- raster::focal(rd, w=matrix(1, 5, 5), mean)
    loc_xy[,cat[k]] <- rd@data@values
  }

  # names(loc_xy) <- c('x','y')
  # for(j in 1:4){
  #   nn <- nn2(loc, loc_xy[,c('x','y')], k = 1)
  #   if(process == "encounter"){
  #     loc_xy[,cat[j]] <- plogis(om[nn$nn.idx,j])
  #   }
  #   if(process == "catch"){
  #     loc_xy[,cat[j]] <- om[nn$nn.idx[,1],j]
  #   }
  # }
  loc_xy <- loc_xy %>%
    pivot_longer(!c(x,y), names_to = "cat", values_to = "val")

  if(re =="s"){
    loc_xy$spatial <- "Average"
  }else{
    loc_xy$spatial <- "2020"
  }
  
  #Create an edge polygon using INLA
  max.edge = c(10, 10)
  cutoff = 2
  max.n = c(100,100)
  bnd1 <- as.data.frame(INLA::inla.nonconvex.hull(as.matrix(cbind(df$utm_lon,df$utm_lat)), convex = bnd1_convex)$loc)
  names(bnd1) <- c("E_km", "N_km")
  bnd1 <- bnd1 %>%
    st_as_sf(coords = c("E_km", "N_km"), crs = "+proj=utm +zone=10 +datum=WGS84")

  polygon <- st_coordinates(bnd1)
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
  
  if(include_stations){
    p <- p + geom_point(data = df[df$t_i==2020,],
                        aes(x = utm_lon, y = utm_lat),
                        inherit.aes = FALSE)
  }
  
  return(list(p = p, #ggplot
              loc_xy = loc_xy #coordinate data
              ))
  
}

