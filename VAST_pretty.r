fit <- readRDS("fit.rds")

myPlot <- function(fit, 
                   process = "catch", 
                   re = "s",
                   n_vir = 8,
                   min_v = 2, 
                   max_v = 8){
  library(data.table)
  library(sf)
  library(tidyr)
  library(dplyr)
  library(concaveman)
  library(RANN)
  library(ggplot2)
  library(viridis)
  library(viridisLite)
  
  
  loc <- as.data.frame(fit$spatial_list$MeshList$anisotropic_mesh$loc[,1:2])
  df <- sdmTMB::add_utm_columns(as.data.frame(fit$data_frame[,c("Lat_i","Lon_i")]),
                                ll_names=c("Lon_i","Lat_i"),
                                utm_names = c("utm_lon","utm_lat"),
                                utm_crs = "+proj=utm +zone=10 +datum=WGS84",
                                units = "km")
  pnts <- df %>%
    st_as_sf(coords = c("utm_lon", "utm_lat"), crs = "+proj=utm +zone=10 +datum=WGS84")


  cat <- c("Subyearling \nChinook", 
           "Yearling \nChinook",
           "Yearling \nCoho", 
           "Sablefish")



  #Equlibrium condition
  if(process == "encounter"){
    if(re == "s"){
      om <- t(t(fit$Report$Omega1_sc[,]) + fit$Report$beta1_tc[1,])
    }
    if(re == "st"){
      om1 <- t(t(fit$Report$Omega1_sc[,] +fit$Report$Epsilon1_sct[,,23]) + fit$Report$beta1_tc[1,])
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
  x_seq <- seq(min(loc[,1]),max(loc[,1]), length.out=500)
  y_seq <- seq(min(loc[,2]),max(loc[,2]), length.out=500)
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

  #Create a polygon
  polygon <- concaveman(pnts, concavity = 10)
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
    ylim(4900,5420)+
    scale_x_continuous(
      limits = c(310,470),
      breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
    )

  p <- p +   ggplot2::geom_raster(data = loc_xy[pin==1,], aes(x = x, y = y, fill= val)) +
    scale_fill_gradientn(colors = viridis_pal()(n_vir), limits=c(min_v, max_v)) +
    facet_wrap(~cat)

  p <- p +
    geom_sf()+
    theme_bw() +
    ylab('') +
    xlab('')
  
  return(p)
}

# 
# #Grab the spatial data you need
# world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
# usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')
# 
# world2 <- sf::st_transform(world,crs="+proj=utm +zone=10 +datum=WGS84  +units=km")
# #
# p <- list()
# 
# p[[1]] <-  #ggplot2::
#   ggplot(data = world2) +
#   coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
#   xlim(310,510)+
#   ylim(4900,5420)+
#   scale_x_continuous(
#     limits = c(310,470),
#     breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
#   )
# 
# p[[1]] <- p[[1]] +   ggplot2::geom_raster(data = loc_xy[pin==1,], aes(x = x, y = y, fill= Encounter)) +
#   scale_fill_viridis(alpha=0.99) +
#   facet_wrap(~cat) 
# 
# p[[1]] <- p[[1]] +
#   geom_sf()+
#   theme_bw() +
#   ylab('') +
#   xlab('')
# 
# 
# # fit$Report$beta1_tc[1,] + 
# om1 <- t(t(fit$Report$Omega1_sc[,] +fit$Report$Epsilon1_sct[,,23]) + fit$Report$beta1_tc[1,])
# loc_xy <- as.data.frame(expand.grid(x_seq,y_seq))
# names(loc_xy) <- c('x','y')
# 
# for(j in 1:4){
#   myK <- 1
#   nn <- nn2(loc, loc_xy[,c('x','y')], k = myK)
#   wt <- (1/sqrt(nn$nn.dists))/rowSums(1/sqrt(nn$nn.dists))
#   id <- wt*0
#   for(i in 1:myK){
#     id[,i] <- om1[nn$nn.idx[,i],j]
#   }
#   
#   loc_xy[,cat[j]] <- plogis(rowSums(wt*id))
#   
# }
# 
# loc_xy <- loc_xy %>% 
#   pivot_longer(!c(x,y), names_to = "cat", values_to = "Encounter")
# 
# 
# #
# p[[2]] <-  #ggplot2::
#   ggplot(data = world2) +
#   coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
#   xlim(310,490)+
#   ylim(4900,5420)+
#   scale_x_continuous(
#     limits = c(310,470),
#     breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
#   )
# 
# p[[2]] <- p[[2]] +   ggplot2::geom_raster(data = loc_xy[pin==1,], aes(x = x, y = y, fill= Encounter)) +
#   # geom_point(data = df, aes(x=utm_lon, y = utm_lat), inherit.aes = FALSE, color = "grey", alpha =0.1) + 
#   scale_fill_viridis(alpha=0.99) +
#   facet_wrap(~cat) 
# 
# p[[2]] <- p[[2]] +
#   geom_sf()+
#   theme_bw() +
#   ylab('') +
#   xlab('')
# 
# g1 <- ggpubr::ggarrange(p[[1]],p[[2]], 
#                        labels = c("(A)","(B)"),
#                        legend = 'right', 
#                        common.legend = TRUE)
# 
# #############################################
# 
# loc_xy <- as.data.frame(expand.grid(x_seq,y_seq))
# names(loc_xy) <- c('x','y')
# 
# for(j in 1:4){
#   myK <- 1
#   nn <- nn2(loc, loc_xy[,c('x','y')], k = myK)
#   wt <- (1/sqrt(nn$nn.dists))/rowSums(1/sqrt(nn$nn.dists))
#   id <- wt*0
#   for(i in 1:myK){
#     id[,i] <- om1[nn$nn.idx[,i],j]
#   }
#   
#   loc_xy[,cat[j]] <- rowSums(wt*id)
#   
# }
# 
# loc_xy <- loc_xy %>% 
#   pivot_longer(!c(x,y), names_to = "cat", values_to = "Catch")
# 
# 
# pin <- sp::point.in.polygon(loc_xy$x[],
#                         loc_xy$y[],
#                         polygon[,1],
#                         polygon[,2])
# 
# g <- list()
# 
# g[[1]] <-  #ggplot2::
#   ggplot(data = world2) +
#   coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
#   xlim(310,510)+
#   ylim(4900,5420)+
#   scale_x_continuous(
#     limits = c(310,470),
#     breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
#   )
# 
# g[[1]] <- g[[1]] +   ggplot2::geom_raster(data = loc_xy[pin==1,], aes(x = x, y = y, fill= (Catch))) +
#   scale_fill_gradientn(colors = viridis_pal()(8), limits=c(0, 8)) +
#   facet_wrap(~cat) 
# 
# g[[1]] <- g[[1]] +
#   geom_sf()+
#   theme_bw() +
#   ylab('') +
#   xlab('')
# 
# print(g[[1]])
# 
# loc_xy <- as.data.frame(expand.grid(x_seq,y_seq))
# names(loc_xy) <- c('x','y')
# 
# for(j in 1:4){
#   myK <- 1
#   nn <- nn2(loc, loc_xy[,c('x','y')], k = myK)
#   wt <- (1/sqrt(nn$nn.dists))/rowSums(1/sqrt(nn$nn.dists))
#   id <- wt*0
#   for(i in 1:myK){
#     id[,i] <- om2[nn$nn.idx[,i],j]
#   }
#   
#   loc_xy[,cat[j]] <- rowSums(wt*id)
#   
# }
# 
# loc_xy <- loc_xy %>% 
#   pivot_longer(!c(x,y), names_to = "cat", values_to = "Catch")
# 
# 
# pin <- sp::point.in.polygon(loc_xy$x[],
#                         loc_xy$y[],
#                         polygon[,1],
#                         polygon[,2])
# 
# g[[2]] <-  #ggplot2::
#   ggplot(data = world2) +
#   coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
#   xlim(310,490)+
#   ylim(4900,5420)+
#   scale_x_continuous(
#     limits = c(310,470),
#     breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
#   )
# 
# g[[2]] <- g[[2]] +   
#   ggplot2::geom_raster(data = loc_xy[pin==1,], aes(x = x, y = y, fill= (Catch))) +
#   scale_fill_gradientn(colors = viridis_pal()(7), limits=c(2, 8)) +
#   facet_wrap(~cat) 
# 
# g[[2]] <- g[[2]] +
#   geom_sf()+
#   theme_bw() +
#   ylab('') +
#   xlab('')
# 
# print(g[[1]])
# 
# g2 <- ggpubr::ggarrange(g[[1]],g[[2]],
#                        labels = c("(B)","(D)"),
#                        legend = 'top',
#                        ncol = 1,
#                        common.legend = TRUE)
# 
# png("catch.png")
# print(g2)
# dev.off()
# 
# 
# gg <- ggpubr::ggarrange(g1,g2,
#                         legend = 'right')
# 
# 
# print(g2)
