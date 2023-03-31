df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey_temperature_Including2021_2022.csv"), header=TRUE))

df2 <- df %>%
  filter(Month == 'June') %>%
  filter(Study == 'JSOES_MaxCatch' | Study == 'JSOES_Regular') %>%
  group_by(Station) %>% summarize(count = n(), Lat = mean(Lat), Long = mean(Long))

df2 <- sdmTMB::add_utm_columns(df2,
                               ll_names=c("Long","Lat"),
                               utm_names = c("utm_lon","utm_lat"),
                               utm_crs = "+proj=utm +zone=10 +datum=WGS84",
                               units = "km")

#Grab the spatial data you need
world <- rnaturalearth::ne_countries(continent='north america', scale = "large", returnclass = "sf")
usa_states <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf')

world2 <- sf::st_transform(world,crs="+proj=utm +zone=10 +datum=WGS84  +units=km")


g <- ggplot(data = world2) +
  coord_sf(crs = "+proj=utm +datum=WGS84 +no_defs +zone=10 +units=km") +
  xlim(270,550)+
  ylim(4850,5450)+
  scale_x_continuous(
    limits = c(300,550),
    breaks = seq(-124.5,-124.5,1) #oddly enough this in decimal degrees
  ) +
  geom_point(data=df2, aes(x = utm_lon, y = utm_lat, size = count), 
             alpha = 0.1, colour = "black") +
  # guides(fill = guide_legend(reverse=TRUE)) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf()+
  theme_bw() +
  ylab('') +
  xlab('') +
  labs(size="Total observations \n across all years")
ggsave(filename = "sampleSizeMap.png", g, width=4, height = 6, units="in")
