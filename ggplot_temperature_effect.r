fit <- fit
beta1 <- fit$Report$beta1_tc
temp1 <- fit$ParHat$lambda1_k[1:4]
sst1 <- fit$ParHat$lambda1_k[5:8]


beta2 <- fit$Report$beta2_tc
lam2 <- fit$ParHat$lambda2_k[1:4]
sst2 <- fit$ParHat$lambda2_k[5:8]

#Read in the data
df_t <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey_temperature.csv"), header=TRUE))



df_t <- df_t[df_t$Lat>=44.25 & df_t$Lat<=48.3,]
df_t <- df_t %>% 
  filter(Month == 'June') %>%
  filter(Study == 'JSOES_MaxCatch' | Study == 'JSOES_Regular') %>%
  gather(key="sp", 
         value="catch",
         c(Sablefish_juv, CK_subyearling, CK_yearling, Coho_yearling))

sst <- read.csv("./data/ersstArc.csv")
sst <- sst[sst$month==5,]
df_t$sst <- scale(sst$sstarc[match(df_t$Year, sst$year)])
df_t$TemperatureC_3m <- scale(df_t$TemperatureC_3m)

temp <- seq(range(df_t$TemperatureC_3m)[1],range(df_t$TemperatureC_3m)[2],length.out = 20)
sst <- seq(range(df_t$sst)[1],range(df_t$sst)[2],length.out = 20)

temp_grid <- expand.grid(1:4,temp,sst)
names(temp_grid) <- c("sp","ctd","sst")
temp_grid$val <- NA
for(i in 1:nrow(temp_grid)){
  sp <- temp_grid[i,1]
  ctd_i <- temp_grid[i,2]
  sst_i <- temp_grid[i,3]
  temp_grid$val[i] <- plogis(beta1[sp] + ctd_i * lam1[sp] + sst_i * sst1[sp])
}

df_cat <- data.frame(id = 1:4, cat = c("Subyearling \nChinook", 
         "Yearling \nChinook",
         "Yearling \nCoho", 
         "Sablefish"))

temp_grid$cat <- df_cat$cat[match(temp_grid$sp,df_cat$id)]

library(ggplot2)

# g <- ggplot(data = temp_grid[temp_grid$sp==4,], aes( x = ctd, y = sst, fill = val)) + 
#   geom_raster() + 
#   facet_wrap(~as.factor(cat), scales = "free")
# print(g)


temp <- replicate(4,seq(range(df_t$TemperatureC_3m)[1],range(df_t$TemperatureC_3m)[2],length.out = 20))
sst <- replicate(4,seq(range(df_t$sst)[1],range(df_t$sst)[2],length.out = 20))
p_t <- data.frame(plogis( t(t(temp)*temp1 + beta1[1,])))
p_sst <- data.frame(plogis( t(t(sst)*sst1 + beta1[1,])))

names(p_t) <- c("Subyearling \nChinook",
                "Yearling \nChinook",
                "Yearling \nCoho",
                "Sablefish")

par(mfrow=c(2,2))
for(i in 1:4) plot(temp[,i], p_t[,i], main = names(p_t)[i], type="l", xlab="CTD 3m temp")
par(mfrow=c(2,2))
for(i in 1:4) plot(sst[,i], p_sst[,i], main = names(p_t)[i], type="l", xlab="SST_arc temp")

# hist(df$)
# 
#           