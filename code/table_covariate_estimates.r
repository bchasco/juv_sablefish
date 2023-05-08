SD <- fit$parameter_estimates$SD
lam_id <- grep("lam",names(fit$parameter_estimates$SD$par.fixed))
val <- matrix(as.character(paste(round(SD$par.fixed[lam_id],2), "pm", round(sqrt(diag(SD$cov.fixed)[lam_id]),2))),4,4)
cat <- c("Subyearling Chinook", 
         "Yearling Chinook",
         "Yearling Coho", 
         "Sablefish")
var_names <- c("Station Temp. (Encounter)", "SST (Encounter)", "Station Temp. (catch)", "SST (catch)")
row.names(val) <- cat
colnames(val) <- var_names

covariate_vals <- val

write.csv(val, file = "table_covariate_estimates.csv") 
