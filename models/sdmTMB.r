# Models that work
# Modify settings to allow model to run faster for demo 
# settings$ObsModel <- c(4,0)
# settings$FieldConfig['Beta',] = 4
# settings$FieldConfig['Omega',] = c(3,2)
# settings$FieldConfig['Epsilon',] = c(3,3)
# settings$RhoConfig['Beta1'] = 3 # constant
# settings$RhoConfig['Beta2'] = 3
# settings$RhoConfig['Epsilon1'] = 0 #0 = fixed, 1 = iid, 2 = rw
# settings$RhoConfig['Epsilon2'] = 1

library(VAST)
library(dplyr)
library(tidyr)
#Read in the data
df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey.csv"), header=TRUE))
df <- df[df$Lat>=44.25 & df$Lat<=48.3,]
df <- df %>% 
  filter(Month == 'June') %>%
  gather(key="sp", 
         value="catch",
         c(Sablefish_juv, CK_subyearling, CK_yearling, Coho_yearling))
df <- na.omit(df)


strata.limits <- data.frame(
  'STRATA' = c("OR/WA"),
  'north_border' = c(48.3),
  'south_border' = c(44.25)
)



#Center of gravity
Options = c(SD_site_density = 0 
            ,SD_site_logdensity = 0
            ,Calculate_Range = TRUE #Center of gravity 
            ,Calculate_evenness = 0 
            ,Calculate_effective_area = 0
            ,Calculate_Cov_SE = 0 
            ,Calculate_Synchrony = 0
            ,Calculate_Coherence = 0)

# Make settings
settings = make_settings( n_x = 150, #50 
                          Options = Options,
                          treat_nonencounter_as_zero = TRUE,
                          Region = "california_current",
                          strata.limits = strata.limits,
                          purpose = "index")

settings$ObsModel <- c(4,0)
settings$FieldConfig['Beta',] = 4 #intercept rank
settings$FieldConfig['Omega',] = c(3,3) #Equilibrium ranks
settings$FieldConfig['Epsilon',] = c(4,4) #Postive catch rate ranks
settings$RhoConfig['Beta1'] = 3 # constant fixed intercept
settings$RhoConfig['Beta2'] = 3 #constant fixed intercept
settings$RhoConfig['Epsilon1'] = 1 #yearly equilbrium encounter correlation, 0 = fixed, 1 = iid
settings$RhoConfig['Epsilon2'] = 1 #yearly catch rate temporal correlation

# Run model
fit = fit_model( settings = settings,
                 # e_i = c(7,0),
                 Lat_i = df[,'Lat'], 
                 Lon_i = df[,'Long'], 
                 t_i = df[,'Year'], 
                 b_i = as_units(df[,'catch'],'count'),
                 c_i = as.numeric(as.factor(df[,'sp']))-1,
                 a_i = as_units(df[,'Trawling_distance_.km.']*0.02,'km^2'),
                 getsd = TRUE,
                 fine_scale = FALSE) #Some years have no sablefish observations



# Plot results
xx <- plot_results(fit,
             years_to_plot = round(seq(1998,2020,1))-1997,
            plot_set = c(16,17),
            check_residuals = FALSE,
            Panel = "Year",
            n_cells = 500,
)

# plot_maps(fit, xx$map_list$PlotDF,
#                    years_to_plot = round(seq(2020,2020,1))-1997,
#                    plot_set = c(16,17),
#                    check_residuals = FALSE,
#                    Panel = "Year",
#                    n_cells = 500)
# 
# # Plot correlations (showing Omega1 as example)
# 
# 
# 
# require(corrplot)
# library(ggcorrplot)
# g <- list()
# 
# png("CorEquilibriumEncounterProb.png")
# Cov_omega1 = fit$Report$L_omega1_cf %*% t(fit$Report$L_omega1_cf)
# mySp <- sort(c('Sablefish \n juvenile', 'Chinook \n subyearling', 'Chinook \n yearling', 'Coho \n yearling'))
# row.names(Cov_omega1) <- mySp
# colnames(Cov_omega1) <- mySp
# g[[1]] <- ggcorrplot(cov2cor(Cov_omega1), hc.order = FALSE, type = "upper",
#            lab = TRUE,
#            colors=c("red","white","blue"),
#            title = "Encounter equilibrium" )
# # corrplot( cov2cor(Cov_omega1), method="pie", type="lower")
# # corrplot.mixed( cov2cor(Cov_omega1)  , mar=c(2,0,5,0),main = "Encounter equilbrium")
# dev.off()
# 
# png("CorYearlyEncounterProb.png")
# Cov_epsilon1 = fit$Report$L_epsilon1_cf %*% t(fit$Report$L_epsilon1_cf)
# row.names(Cov_epsilon1) <- mySp
# colnames(Cov_epsilon1) <- mySp
# # corrplot.mixed( cov2cor(Cov_epsilon1) , mar=c(2,0,5,0),main = "Encounter annual" )
# g[[2]] <- ggcorrplot(cov2cor(Cov_epsilon1), hc.order = FALSE, type = "upper",
#                      lab = TRUE,
#                      colors=c("red","white","blue"),
#                      title = "Encounter yearly" )
# dev.off()
# 
# png("CorEquilibriumPosCatch.png")
# Cov_omega2 = fit$Report$L_omega2_cf %*% t(fit$Report$L_omega2_cf)
# row.names(Cov_omega2) <- mySp
# colnames(Cov_omega2) <- mySp
# # corrplot.mixed( cov2cor(Cov_omega2) , mar=c(2,0,5,0),main = "Catch equilibrium" )
# g[[3]] <- ggcorrplot(cov2cor(Cov_omega2), hc.order = FALSE, type = "upper",
#            lab = TRUE, 
#            colors=c("red","white","blue"),
#            title = "Catch equilibrium" )
# dev.off()
# 
# 
# png("CorYearlyPosCatch.png")
# Cov_epsilon2 = fit$Report$L_epsilon2_cf %*% t(fit$Report$L_epsilon2_cf)
# row.names(Cov_epsilon2) <- mySp
# colnames(Cov_epsilon2) <- mySp
# # corrplot.mixed( cov2cor(Cov_epsilon2) , mar=c(2,0,5,0),main = "Catch annual")
# g[[4]] <- ggcorrplot(cov2cor(Cov_epsilon2), hc.order = FALSE, type = "upper",
#            lab = TRUE,
#            colors=c("red","white","blue"),
#            title = "Catch yearly" )
# dev.off()
# 
# library(ggpubr)
# png("corelation_plot.png")
# ggarrange(
#   plotlist = g,
#   nrow = 2,
#   ncol = 2,
#   labels = paste('(',LETTERS[1:4],')'),
#   common.legend = TRUE,
#   legend = 'right')
# dev.off()
# 
