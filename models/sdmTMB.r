
library(VAST)
library(dplyr)
library(tidyr)
#Read in the data
df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey.csv"), header=TRUE))
df <- df[df$Lat>42,]
df <- df %>% 
  gather(key="sp", 
         value="catch",
         c(Sablefish_juv, CK_subyearling, CK_yearling, Coho_yearling))
df <- df[df$Month=="June",] #Elizabeth thinks we should only use June.
df <- na.omit(df)


strata.limits <- data.frame(
  'STRATA' = c("OR/WA"),
  'north_border' = c(49.0),
  'south_border' = c(42.0)
)

# Make settings
settings = make_settings( n_x = 50, 
                          Region = "california_current",
                          strata.limits = strata.limits,
                          purpose = "ordination",
                          n_categories = 2 )

# Modify settings to allow model to run faster for demo 
settings$FieldConfig['Beta',] = "IID"
settings$FieldConfig['Epsilon',] = 0
settings$RhoConfig[] = 0


# Run model
fit = fit_model( settings = settings,
                 # e_i = c(7,0),
                 Lat_i = df[,'Lat'], 
                 Lon_i = df[,'Long'], 
                 t_i = df[,'Year'], 
                 b_i = as_units(df[,'catch'],'count'),
                 c_i = as.numeric(as.factor(df[,'sp']))-1,
                 a_i = (df[,'Trawling_distance_.km.']*0.02),
                 getsd = TRUE,
                 fine_scale = FALSE,
                 Options = c('treat_nonencounter_as_zero' = TRUE)) #Some years have no sablefish observations



# Plot results
plot_results(fit, 
             years_to_plot = round(seq(1998,2020,length.out=6))-1997,
            plot_set = c(1,2,3,4), 
            n_cells = 100,
)

# Plot correlations (showing Omega1 as example)
require(corrplot)
Cov_omega1 = fit$Report$L_omega1_cf %*% t(fit$Report$L_omega1_cf)
corrplot( cov2cor(Cov_omega1), method="pie", type="lower")
corrplot.mixed( cov2cor(Cov_omega1) )

Cov_omega2 = fit$Report$L_omega2_cf %*% t(fit$Report$L_omega2_cf)
corrplot( cov2cor(Cov_omega2), method="pie", type="lower")
corrplot.mixed( cov2cor(Cov_omega2) )
