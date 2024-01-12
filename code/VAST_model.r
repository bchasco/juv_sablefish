
library(VAST)
library(dplyr)
library(tidyr)
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects


#Read in the data
# df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey_temperature.csv"), header=TRUE))
df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey_temperature_Including2021_2022_2023.csv"), header=TRUE))



#This removes a line of samples off of Tatoosh and another line very far south
#Neither of them were regualrly sampled before.
df <- df[df$Lat>=44.25 & df$Lat<=48.3,]

df <- df %>% 
  filter(Month == 'June') %>%
  filter(Study == 'JSOES_MaxCatch' | Study == 'JSOES_Regular') %>%
  gather(key="sp", 
         value="catch",
         c(Sablefish_juv, CK_subyearling, CK_yearling, Coho_yearling))
# df <- na.omit(df)
# df <- df[df$Year<2020,]

options(pillar.sigfig=6)

#This speeds up the mesh making by group lat lon coordinates by station
locs <- df %>% 
  group_by(Station_ID = Station) %>%
  summarise(m_lat = mean(Lat), m_long = mean(Long))


#Read the sst data
sstArc <- read.csv("./data/ersstArc.csv")
sstArc <- sstArc[sstArc$month==5,]
df$sstArc <- sstArc$sstarc[match(df$Year, sstArc$year)]

#You need this for the covariate and catchability predictions. You have to use the 
#column headings of "Lat" and "Lon", I think.
df$Lat <- as.numeric(unlist(locs[match(df$Station,locs$Station_ID),'m_lat']))
df$Lon <- as.numeric(unlist(locs[match(df$Station,locs$Station_ID),'m_long']))

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
settings = make_settings( n_x = length(unique(df$Station)), #Set knots equal to the number of unique stations 
                          Options = Options,
                          treat_nonencounter_as_zero = FALSE,
                          Region = "california_current",
                          strata.limits = strata.limits,
                          max_cells = 5000,
                          purpose = "index")

settings$ObsModel <- c(4,0) #Delta log-normal
settings$FieldConfig['Beta',] = 4 #intercept rank for each "species"
settings$FieldConfig['Omega',] = c(3,3) #Spatial ranks for encounter and catch
settings$FieldConfig['Epsilon',] =  c(3,3) #Spatiotemporal ranks for encounter and catch

#Correlation for modeled processes
settings$RhoConfig['Beta1'] = 3 #Encounter constant fixed intercept
settings$RhoConfig['Beta2'] = 3 #Catch constant fixed intercept
settings$RhoConfig['Epsilon1'] = 0 #yearly encounter iid
settings$RhoConfig['Epsilon2'] = 0 #yearly catch rate iid
# settings$RhoConfig['Epsilon1'] = 0 #yearly encounter iid
# settings$RhoConfig['Epsilon2'] = 0 #yearly catch rate iid

#Make a dataset of the catchability data. What's important is that the Latitude and Longitude columns are labeled 'Lat' and 'Lon'
covariate_data <- df[,c('Lat','Lon','Year','TemperatureC_3m','AvgOf_Top10mTempC','AvgOf_Top20mTempC','sstArc','sp')]
names(covariate_data)[1] <- "Lat"
names(covariate_data)[2] <- "Lon"


# Run model
fit = fit_model( settings = settings, #read in settings
                 Lat_i = df[,'Lat'],  
                 Lon_i = df[,'Lon'], 
                 t_i = df[,'Year'], 
                 b_i = as_units(df[,'catch'],'count'), #Standardized by Cheryl
                 c_i = as.numeric(as.factor(df[,'sp']))-1, #-1 so factors start at 0 not 1
                 a_i = as_units(df[,'Trawling_distance_.km.']*0.028,'km^2'), #area offset
                 covariate_data = covariate_data, #This is habitat-specific 
                 catchability_data = covariate_data, #This is sample specific, Something that relates to the sampling
                 #For the covariate formulas, you have to remove the intercept 
                 #if you are going to make the marginal plots.
                 #The Q is sample-specific effect -- catchability data
                 Q1_formula = ~ poly(sstArc, degree = 1) : sp + 0, #sst, species-specific
                 # Q2_formula = ~ poly(TemperatureC_3m, degree = 3) : sp, #sst, species-specific
                 # X1_formula = ~ poly(TemperatureC_3m, degree = 3) : sp, #temp, not species specific, they're all negative with similar slopes
                 #The X is for the habitat effects that occur at each spatial location in the mesh
                 X2_formula = ~ poly(TemperatureC_3m, degree = 1) : sp + 0, #temp, species-specific
                 getsd = TRUE,
                 fine_scale = FALSE) #Some years have no sablefish observations

# saveRDS(fit, "C:/NOAA/large_data/juvenile_sablefish/fit_w3_sst.rds")

# readRDS("fit.rds")
# Plot results
# xx <- plot_results(fit,
#              years_to_plot = round(seq(2020,2020,1))-1997,
#             plot_set = c(16),
#             check_residuals = FALSE,
#             Panel = "Year",
#             # n_cells = 50,
# )

# plot_results(fit, plot_set = c(12),
#              n_cells = 100,
#              years_to_plot = round(seq(2020,2023,1))-1997)
