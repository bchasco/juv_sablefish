require(corrplot)
library(ggcorrplot)
g <- list()

png("CorEquilibriumEncounterProb.png")
Cov_omega1 = fit$Report$L_omega1_cf %*% t(fit$Report$L_omega1_cf)
mySp <- sort(c('Sablefish \n juvenile', 'Chinook \n subyearling', 'Chinook \n yearling', 'Coho \n yearling'))
row.names(Cov_omega1) <- mySp
colnames(Cov_omega1) <- mySp
g[[1]] <- ggcorrplot(cov2cor(Cov_omega1), hc.order = FALSE, type = "upper",
           lab = TRUE,
           colors=c("red","white","blue"),
           title = "Encounter equilibrium" )
# corrplot( cov2cor(Cov_omega1), method="pie", type="lower")
# corrplot.mixed( cov2cor(Cov_omega1)  , mar=c(2,0,5,0),main = "Encounter equilbrium")
dev.off()

png("CorYearlyEncounterProb.png")
Cov_epsilon1 = fit$Report$L_epsilon1_cf %*% t(fit$Report$L_epsilon1_cf)
row.names(Cov_epsilon1) <- mySp
colnames(Cov_epsilon1) <- mySp
# corrplot.mixed( cov2cor(Cov_epsilon1) , mar=c(2,0,5,0),main = "Encounter annual" )
g[[2]] <- ggcorrplot(cov2cor(Cov_epsilon1), hc.order = FALSE, type = "upper",
                     lab = TRUE,
                     colors=c("red","white","blue"),
                     title = "Encounter yearly" )
dev.off()

png("CorEquilibriumPosCatch.png")
Cov_omega2 = fit$Report$L_omega2_cf %*% t(fit$Report$L_omega2_cf)
row.names(Cov_omega2) <- mySp
colnames(Cov_omega2) <- mySp
# corrplot.mixed( cov2cor(Cov_omega2) , mar=c(2,0,5,0),main = "Catch equilibrium" )
g[[3]] <- ggcorrplot(cov2cor(Cov_omega2), hc.order = FALSE, type = "upper",
           lab = TRUE,
           colors=c("red","white","blue"),
           title = "Catch equilibrium" )
dev.off()


png("CorYearlyPosCatch.png")
Cov_epsilon2 = fit$Report$L_epsilon2_cf %*% t(fit$Report$L_epsilon2_cf)
row.names(Cov_epsilon2) <- mySp
colnames(Cov_epsilon2) <- mySp
# corrplot.mixed( cov2cor(Cov_epsilon2) , mar=c(2,0,5,0),main = "Catch annual")
g[[4]] <- ggcorrplot(cov2cor(Cov_epsilon2), hc.order = FALSE, type = "upper",
           lab = TRUE,
           colors=c("red","white","blue"),
           title = "Catch yearly" )
dev.off()

library(ggpubr)
png("corelation_plot.png")
ggarrange(
  plotlist = g,
  nrow = 2,
  ncol = 2,
  labels = paste('(',LETTERS[1:4],')'),
  common.legend = TRUE,
  legend = 'right')
dev.off()

