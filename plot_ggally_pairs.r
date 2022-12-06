library(GGally)

tmp <- data.frame(a = c(fit$Report$Epsilon2_sct[,1,]),
b = c(fit$Report$Epsilon2_sct[,2,]),
c = c(fit$Report$Epsilon2_sct[,3,]),
d = c(fit$Report$Epsilon2_sct[,4,]))
names(tmp) <- levels(as.factor(df$sp))
tmp$year <- c(rep('other',dim(fit$Report$Epsilon2_sct)[1]*22),
              rep(2020,dim(fit$Report$Epsilon2_sct)[1]))



ggpairs(tmp,
        aes(color = year))

# pairs(tmp_2020,
#       col="black",
#       pch = 16,
#       add=TRUE)
