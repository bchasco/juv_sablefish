
library(effects)  # Used to visualize covariate effects
# Plot 1st linear predictor, but could use `transformation` to apply link function
pred = Effect.fit_model( fit,
                         focal.predictors = c("sstArc", "sp"),
                         which_formula = "Q1",
                         pad_values = 1,
                         # category_number = 1,
                         # xlevels = 10,
                         transformation = list(link=identity, inverse=identity) ,
                         ylab="Encounter rate")
png(file = "./output/sstArc.png", width = 400, height = 400)
plot(pred, 
     ylab = "Encounter rate effect",
     xlab = "sstArc",
     main = "")
dev.off()

pred = Effect.fit_model( fit,
                         focal.predictors = c("TemperatureC_3m","sp"),
                         which_formula = "X2",
                         pad_values = rep(1),
                         # category_number = 1,
                         xlevels = 10,
                         transformation = list(link=identity, inverse=identity), ylab = "Encounter rate" )
png("./output/Temp3m.png", width = 400, height = 400)
plot(pred, 
     ylab = "Density effect", 
     xlab = "Temperature at 3m",
     main = "")
dev.off()
