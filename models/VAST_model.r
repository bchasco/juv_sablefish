
library(VAST)
library(dplyr)
library(tidyr)

#Read in the data
df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey.csv"), header=TRUE))
df <- df[df$Lat>=44.25 & df$Lat<=48.3,]
df <- df %>% 
  filter(Month == 'June') %>%
  filter(Study == 'JSOES_MaxCatch' | Study == 'JSOES_Regular') %>%
  gather(key="sp", 
         value="catch",
         c(Sablefish_juv, CK_subyearling, CK_yearling, Coho_yearling))
# df <- na.omit(df)

# table(df$sp, df$Year)
# table(df$Study, df$Year)
# table(df$Station[df$Study!="JSOES_Regular"], df$Study[df$Study!="JSOES_Regular"])
# table(df$Sample_Date[df$Year==2006 & df$sp=="CK_yearling"])
# for (yy in unique(df$Year)) cat(yy, length(table(df$Sample_Date[df$Year==yy & df$sp=="CK_yearling"])), "\n")
# table(df$Station[df$Year==2006 & df$sp=="CK_yearling"])

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
settings$FieldConfig['Omega',] = c(3,2) #Spatial ranks for encounter and catch
settings$FieldConfig['Epsilon',] = c(3,3) #Spatiotemporal ranks for encounter and catch

#Correlation for modeled processes
settings$RhoConfig['Beta1'] = 3 #Encounter constant fixed intercept
settings$RhoConfig['Beta2'] = 3 #Catch constant fixed intercept
settings$RhoConfig['Epsilon1'] = 1 #yearly encounter iid
settings$RhoConfig['Epsilon2'] = 1 #yearly catch rate iid

# Run model
fit = fit_model( settings = settings, #read in settings
                 Lat_i = df[,'Lat'],  
                 Lon_i = df[,'Long'], 
                 t_i = df[,'Year'], 
                 b_i = as_units(df[,'catch'],'count'), #Standardized by Cheryl
                 c_i = as.numeric(as.factor(df[,'sp']))-1, #-1 so factors start at 0 not 1
                 a_i = as_units(df[,'Trawling_distance_.km.']*0.02,'km^2'), #area offset
                 getsd = TRUE,
                 fine_scale = FALSE) #Some years have no sablefish observations

# saveRDS(fit, "fit.rds")


# Plot results
# xx <- plot_results(fit,
#              years_to_plot = round(seq(2020,2020,1))-1997,
#             plot_set = c(16,17),
#             check_residuals = FALSE,
#             Panel = "Year",
#             n_cells = 500,
# )

