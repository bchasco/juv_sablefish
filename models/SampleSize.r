png("sampleSize.png")
samples <- data.frame(table(df$Year)/4)
names(samples) = c("Year", "n")
ggplot(samples, aes(x = Year, y = n)) + 
  geom_col( ) + 
  theme_bw() +
  ylab("Number of stations sampled") +
  theme(axis.text.x=element_text(angle=90,hjust=1))
dev.off()

