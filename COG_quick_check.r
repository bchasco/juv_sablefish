library(ggplot2)
cog_check <- df %>%
  group_by(sp, Year) %>%
  summarise(lat = sum(Lat * catch)/sum(catch),
            lon = sum(Long * catch)/sum(catch))

write.csv(cog_check, file = "cog_check.csv")

p <- ggplot2::ggplot(data = cog_check) +
  ggplot2::geom_path(data = cog_check,aes(y = lat, x  = lon, 
                            color=as.factor(Year),
                            group=1), 
            arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
            inherit.aes = FALSE, show.legend = FALSE
  )+
  ggplot2::facet_wrap(.~sp, nrow = 1)

# png("new_COG_plot.png", width = 800, height = 600)
print(p)
