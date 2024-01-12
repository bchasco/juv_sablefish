# Must add data-frames to global environment (hope to fix in future)
covariate_data_full = fit$effects$covariate_data_full
catchability_data_full = fit$effects$catchability_data_full

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
                         focal.predictors = c("AvgOf_Top10mTempC","sp"),
                         which_formula = "X2",
                         pad_values = rep(1),
                         category_number = 1,
                         xlevels = 10,
                         transformation = list(link=identity, inverse=identity), ylab = "Encounter rate" )
png("./output/Temp3m.png", width = 400, height = 400)
plot(pred, 
     ylab = "Density effect", 
     xlab = "Temperature at 10m",
     main = "")
dev.off()
