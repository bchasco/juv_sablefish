png("COG_plot_MLE_Daly.png", height=720,width=480)
source("new_cog_plot.r")
ii <- 1
par(mfrow=c(1,1), mai=c(1.0,1.5,0.1,1), fig=c(0,1,0.4,1))
mySurveys <- c("JSOES")
for(subDat in mySurveys){
  
  raw <- read.csv("Update_Comb_Catch_wTrawlDist_flat_Daly_4_7_22.csv")
  # subDat <- "SWFSC"
  if(subDat!="All"){
    raw <- raw[raw$Survey==subDat,]
  }
  load(paste0(subDat,"MLE_Daly.Rdata"))
  ifelse(ii==1,myPlot<-TRUE,myPlot<-FALSE)
  # myPlot <- TRUE
  cog_plot(get(paste0(subDat,"_fit")),plot=myPlot,col=ii,myYears=c(2014:2016,2019))
  ii <- ii+ 1
}

dev.off()
# 
# #https://www.integratedecosystemassessment.noaa.gov/regions/california-current/cc-projects-blobtracker
# #
# hw <- read.table("Marine_Heatwave.dat", header=TRUE)
# hw <- hw[hw$year>=1998 & hw$year<=2019,]
# par(mfrow=c(1,1), mai=c(1.,1.5,0.1,1), fig=c(0,1,0.0,0.4))
# par(new=TRUE)
# plot(hw$year,hw$wtd_area/1000, type="l",
#      xlab="Calendar year",
#      axes=FALSE,
#      lwd=2,
#      yaxs="i",
#      ylab="Weighted area \n (1,000 km^2)")
# axis(2,las=2)
# axis(1)
# 
# par(new=TRUE)
# plot(hw$year,
#      hw$min_dist/1000, type="l",
#      lty=2,
#      lwd=2,
#      yaxs="i",
#      xlab="",
#      ylab="",
#      axes=FALSE)
# legend(2002,
#        2,
#        legend=c("Area","Distance"),
#        lty=c(1,2),
#        lwd=2,
#        bty="n")
# 
# axis(4,las=2)
# box()
# mtext("Minimum distance \n  (1,000 km) from shore", side = 4,
#       line = 3.5)
# rect(2013.5,0,2016.5,300,
#      col=grey(0.2,0.3), border=grey(0.2,0.3))
# rect(2018.5,0,2019.5,300,
#      col=grey(0.2,0.3), border=grey(0.2,0.3))
# dev.off()
