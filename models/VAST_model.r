
library(VAST)
library(dplyr)
library(tidyr)

#Read in the data
# df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey_temperature.csv"), header=TRUE))
df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey_temperature_Including2021_2022.csv"), header=TRUE))



df <- df[df$Lat>=44.25 & df$Lat<=48.3,]
df <- df %>% 
  filter(Month == 'June') %>%
  filter(Study == 'JSOES_MaxCatch' | Study == 'JSOES_Regular') %>%
  gather(key="sp", 
         value="catch",
         c(Sablefish_juv, CK_subyearling, CK_yearling, Coho_yearling))
# df <- na.omit(df)
# df <- df[df$Year<2020,]

sst <- read.csv("./data/ersstArc.csv")
sst <- sst[sst$month==6,]
df$sst <- scale(sst$sstarc[match(df$Year, sst$year)])
df$TemperatureC_3m <- scale(df$TemperatureC_3m )

#Spatial extent of the survey
strata.limits <- data.frame(
  'STRATA' = c("OR/WA"),
  'north_border' = c(48.3),
  'south_border' = c(44.25)
)



#Center of gravity
Options = c(SD_site_density = 0 
            ,SD_site_logdensity = 0
            ,Calculate_Range = 0 #Center of gravity 
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

settings$ObsModel <- c(4,0) #Delta log-normal
settings$FieldConfig['Beta',] = 4 #intercept rank
settings$FieldConfig['Omega',] = c(3,3) #Spatial ranks for encounter and catch
settings$FieldConfig['Epsilon',] =  c(3,3) #Spatiotemporal ranks for encounter and catch
# settings$FieldConfig['Omega',] = c(0,0) #Spatial ranks for encounter and catch
# settings$FieldConfig['Epsilon',] = c(0,0) #Spatiotemporal ranks for encounter and catch

#Correlation for modeled processes
settings$RhoConfig['Beta1'] = 3 #Encounter constant fixed intercept
settings$RhoConfig['Beta2'] = 3 #Catch constant fixed intercept
settings$RhoConfig['Epsilon1'] = 1 #yearly encounter iid
settings$RhoConfig['Epsilon2'] = 1 #yearly catch rate iid
# settings$RhoConfig['Epsilon1'] = 0 #yearly encounter iid
# settings$RhoConfig['Epsilon2'] = 0 #yearly catch rate iid

#Make a dataset of the catchability data
catchability_data <- df[,c('TemperatureC_3m','AvgOf_Top10mTempC','AvgOf_Top20mTempC','sst','sp')]

# Run model
fit = fit_model( settings = settings, #read in settings
                 Lat_i = df[,'Lat'],  
                 Lon_i = df[,'Long'], 
                 t_i = df[,'Year'], 
                 b_i = as_units(df[,'catch'],'count'), #Standardized by Cheryl
                 c_i = as.numeric(as.factor(df[,'sp']))-1, #-1 so factors start at 0 not 1
                 a_i = as_units(df[,'Trawling_distance_.km.']*0.02,'km^2'), #area offset
                 catchability_data = catchability_data,
                 Q1_formula = ~ TemperatureC_3m:sp + sst:sp,
                 Q2_formula = ~ TemperatureC_3m:sp + sst:sp,
                 # Q1_formula = ~ AvgOf_Top20mTempC:sp,
                 # Q2_formula = ~ AvgOf_Top20mTempC:sp,
                 getsd = TRUE,
                 fine_scale = FALSE) #Some years have no sablefish observations

saveRDS(fit, "fit_w3_sst.rds")

# readRDS("fit.rds")
# Plot results
# xx <- plot_results(fit,
#              years_to_plot = round(seq(2020,2020,1))-1997,
#             plot_set = c(16),
#             check_residuals = FALSE,
#             Panel = "Year",
#             n_cells = 50,
# )

