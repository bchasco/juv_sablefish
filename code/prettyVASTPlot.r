prettyVastPlot <- function(fit, 
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

  #grid
  loc <- as.data.frame(fit$spatial_list$MeshList$anisotropic_mesh$loc[,1:2])
  x_seq <- seq(min(loc[,1]),max(loc[,1]), length.out=50)
  y_seq <- seq(min(loc[,2]),max(loc[,2]), length.out=50)
  if(re == "s")
    yr_seq <- "average"
  if(re == "st")
    yr_seq <- unique(fit$data_frame$t_i)
  
  loc_mat <- matrix(NA, 50,50)
  mydim <- dim(expand.grid(x_seq,y_seq))
  process <- c("encounter","catch")
  loc_xy <- expand.grid(x_seq,y_seq,cat,yr_seq)

  names(loc_xy) <- c('x','y',"cat", 'yr')
  loc_xy[,c('catch','encounter')] <- NA

  om_id <- (matrix(nn2(loc,expand.grid(x_seq,y_seq),k=1)$nn.idx,50,50))
  
  
  for(p in c("catch","encounter")){
    #Processes with different random effects
    if(re=="s")
      om <- array(NA,c(dim(fit$Report$Omega1_sc),1))
    if(re=="st")
      om <- array(NA,dim(fit$Report$Epsilon1_sct))
    
    if(p == "encounter"){
      if(re == "s"){
        om <- t(t(fit$Report$Omega1_sc[,]) + fit$Report$beta1_tc[1,])
        om <- array(om,c(dim(om),1))
      }
      if(re == "st"){
        for(ii in 1:dim(fit$Report$Epsilon1_sct)[3]){
          om[,,ii] <- t(t(fit$Report$Omega1_sc[,] + fit$Report$beta1_tc[1,]  + fit$Report$Epsilon1_sct[,,ii]))
        }
      }
    }
    if(p == "catch"){
      if(re == "s"){
        # Equilibrium catch rate
        om <- t(fit$Report$beta2_tc[1,] +t(fit$Report$Omega2_sc[,]))
        om <- array(om,c(dim(om),1))
      }
      if(re == "st"){
        #2020 catch rate
        for(ii in 1:dim(fit$Report$Epsilon1_sct)[3]){
          om[,,ii] <- t(fit$Report$beta2_tc[1,] + t(fit$Report$Omega2_sc[,] + fit$Report$Epsilon2_sct[,,ii]))
        }
      }
    }

    for(y in 1:dim(om)[3]){ #year or average
      for(k in 1:4){ #categories 
        for(i in 1:50){ # utm_lon
          for(j in 1:50){ #utm_lat
            if(p == "encounter"){
              loc_mat[i,j] <- plogis(om[om_id[i,j],k,y])
            }
            if(p == "catch"){
              loc_mat[i,j] <- om[om_id[i,j],k,y]
            }
          }
        }
        #This is for smoothing the display
        rd <- raster::disaggregate(raster::raster(t(loc_mat)),
                                   fact = c(1,1),
                                   method="bilinear")
        rd <- raster::focal(rd, w=matrix(1, 5, 5), mean)
        loc_xy[loc_xy$cat == cat[k] & loc_xy$yr == yr_seq[y],p] <- rd@data@values
      }
    }
    
  }

  #Create an edge polygon using INLA
  max.edge = c(10, 10)
  cutoff = 2
  max.n = c(100,100)
  bnd1_convex = -0.06
  bnd1 <- as.data.frame(INLA::inla.nonconvex.hull(as.matrix(cbind(df$utm_lon,df$utm_lat)), convex = bnd1_convex)$loc)
  names(bnd1) <- c("E_km", "N_km")
  bnd1 <- bnd1 %>%
    st_as_sf(coords = c("E_km", "N_km"), crs = "+proj=utm +zone=10 +datum=WGS84")

  polygon <- st_coordinates(bnd1)
  pin <- sp::point.in.polygon(loc_xy$x[],
                              loc_xy$y[],
                              polygon[,1],
                              polygon[,2])

  return(list(
    pin = pin, #ggplot
    loc_xy = loc_xy #coordinate data
    ))
  
}

