library(sp)
library(OEI)

world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')
world2 <- sf::st_transform(world,crs="+proj=utm +zone=10 +datum=WGS84  +units=km")
# 
p <- ggplot2::ggplot(data = world2) +
  coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km")+
  xlim(320,440)+
  ylim(5000,5300)+
  scale_x_continuous(
    limits = c(340,440),
    breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
  )

p <- p +
  geom_sf()+
  theme_bw() +
  ylab('') +
  xlab('')

mySpecies <- levels(as.factor(df$sp))[1:4]
CogName <- "mean_Z_ctm"
# CogName <- "Index_ctl"

Sdreport <- fit$parameter_estimates$SD
SD <- TMB::summary.sdreport(Sdreport)
COG <- SD[which(rownames(SD)==CogName),]
d <- expand.grid(sp=levels(as.factor(df$sp)),yr = unique(df$Year))
d <- cbind(d,data.frame(cog_est = array(COG[,c('Estimate')],c(nrow(COG)/2/4*4,2))))
d <- cbind(d,data.frame(array(COG[,c('Std. Error')],c(nrow(COG)/2/4*4,2))))
names(d)[3:6] <- c('lat','lon','lat_sd','lon_sd')
myYears <- c(2010,2013,2015,2020)

d <- d[d$sp%in%mySpecies,]

p <- p +
  geom_path(data = d[,],aes(x = lat,y  = lon, 
                         color=as.factor(yr),
                         group=1), 
            arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
            inherit.aes = FALSE, show.legend = FALSE
            )+
  geom_text(data = d[d$yr%in%myYears,],aes(x = lat,y  = lon, 
                         label=as.factor(yr)), 
            inherit.aes = FALSE, show.legend = FALSE
  )+
  facet_wrap(.~sp, nrow = 1)

# png("new_COG_plot.png", width = 800, height = 600)
print(p)
# dev.off()


