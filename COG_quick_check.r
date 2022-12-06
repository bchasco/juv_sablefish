cog_check <- df %>%
  group_by(sp, Year) %>%
  summarise(lat = sum(Lat * catch)/sum(catch),
            lon = sum(Long * catch)/sum(catch))

p <- ggplot2::ggplot(data = cog_check) +
  geom_path(data = cog_check,aes(y = lat, x  = lon, 
                            color=as.factor(Year),
                            group=1), 
            arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
            inherit.aes = FALSE, show.legend = FALSE
  )+
  facet_wrap(.~sp, nrow = 1)

# png("new_COG_plot.png", width = 800, height = 600)
print(p)
