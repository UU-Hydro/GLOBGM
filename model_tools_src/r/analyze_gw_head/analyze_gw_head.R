# setwd('f:/models/pcr-globwb-30arcsec/model_new/validation/statistics/')
# load datacombine & ggplot2 & scales
# annual_slope = lm(head~year)$coefficients[2]

# packages needed and clear all available existing objects:
require('psych'); require('RColorBrewer'); require('timeSeries'); require('topmodel'); require('zoo');

#most_consecutive_val = function(x, val = 1) {smRhoMin
#  with(rle(x), max(lengths[values == val]))
#}

most_consecutive_val = function(x, val = 1) {
  with(rle(x), max(lengths[values == val]))
}


# plotting functions
plot_comparison <- function(outpltfl, model_measu, date, startDate, endDate, y_min, y_max, site, iqm, iqo, qre, rho, rmse, bias, yrev) {
  tss_dfr = model_measu
  #y_min         = -10
  #y_max         =  5

  outplott <- ggplot()
  outplott <- outplott +
    geom_line(data = tss_dfr[!is.na(tss_dfr$data),], aes(x = date, y = data), colour =  "green" , size = 0.35, linetype = "solid") +
    geom_point(data = tss_dfr, aes(x = date, y = data ), shape=1,  colour =  "green") +
    geom_line(data = tss_dfr, aes(x = date, y = model ), colour =  "red", size = 0.35) +
    scale_y_continuous(limits=c(y_min,y_max)) +
    scale_x_date('',limits=c(as.Date(startDate,"%Y-%m-%d"),as.Date(endDate,"%Y-%m-%d"))) +
    ggtitle(paste("site ",site,
     "; iq_mod = ",sprintf(iqm,fmt='%#.2f'),
     ", iq_obs = ",sprintf(iqo,fmt='%#.2f'),
     ", qre = ",sprintf(qre,fmt='%#.2f'),
     ", rho = ",sprintf(rho,fmt='%#.2f'),
     ", rmse = ",sprintf(rmse,fmt='%#.2f'),
     ", bias = ",sprintf(bias,fmt='%#.2f'),sep=""))
  if (yrev){
   outplott <- outplott + scale_y_reverse()
   outplott <- outplott + ylab("Relative groundwater depth [m]")
  }else{
   outplott <- outplott + ylab("Relative groundwater head [m]")
  }
  outplott <- outplott + theme(axis.title.y = element_text(size = 10))
  outplott <- outplott + theme(plot.title = element_text(size = 10))
  ggsave(paste(outpltfl,sep=""), plot = outplott,width=27,height=7,units='cm')
  rm(outplott)
}

plot_soilm <- function(outpltfl, model_measu, date, startDate, endDate, y_min, y_max, site, smRho, yrev) {
  tss_dfr = model_measu
  #y_min         = -10
  #y_max         =  5

  outplott <- ggplot()
  outplott <- outplott +
    geom_line(data = tss_dfr[!is.na(tss_dfr$data),], aes(x = date, y = data), colour =  "green" , size = 0.35, linetype = "solid") +
    geom_point(data = tss_dfr, aes(x = date, y = data ), shape=1,  colour =  "green") +
    geom_line(data = tss_dfr, aes(x = date, y = model ), colour =  "red", size = 0.35) +
    geom_point(data = tss_dfr, aes(x = date, y = model ), shape=1,  colour =  "red") +
    scale_y_continuous(limits=c(y_min,y_max)) +
    scale_x_date('',limits=c(as.Date(startDate,"%Y-%m-%d"),as.Date(endDate,"%Y-%m-%d"))) +
   ggtitle(paste("site ",site,
     "; rho = ",sprintf(smRho,fmt='%#.2f'),sep=""))
   if (yrev){
   outplott <- outplott + scale_y_reverse()
   outplott <- outplott + ylab("Relative groundwater depth [m]")
  }else{
   outplott <- outplott + ylab("Relative groundwater head [m]")
  }
  outplott <- outplott + theme(axis.title.y = element_text(size = 10))
  outplott <- outplott + theme(plot.title = element_text(size = 10))
  ggsave(paste(outpltfl,sep=""), plot = outplott,width=27,height=7,units='cm')
  rm(outplott)
}


# all functions:  ######################################################################################################################

rmse    <- function(obs, pred) sqrt(mean((obs-pred)^2 ,na.rm=T))
 mae    <- function(obs, pred)      mean(abs(obs-pred),na.rm=T)
bias    <- function(obs, pred) mean(pred[which(!is.na(obs) & !is.na(pred))]) - mean(obs[which(!is.na(obs) & !is.na(pred))])  # POSITIVE indicates that the average prediction is higher than average observation.
R2      <- function(obs, pred) summary(lm(obs ~ pred))$r.squared
R2ad    <- function(obs, pred) summary(lm(obs ~ pred))$adj.r.squared

QRE7525 <- function(obs, pred) {
# percentile value that we used
  pairs      = length(pred[which(!is.na(obs))])
  percentius = c(0.25,0.75)
# calculate model and data percentile
  mdpr       = quantile(as.numeric(pred),probs=percentius,na.rm=T)
  mdpr7525   = as.numeric(mdpr[which(percentius==0.75)] - mdpr[which(percentius==0.25)])
  dtpr       = quantile(as.numeric(obs ),probs=percentius,na.rm=T)
  dtpr7525   = as.numeric(dtpr[which(percentius==0.75)] - dtpr[which(percentius==0.25)])
  QRE7525    = (mdpr7525-dtpr7525)/dtpr7525
  return(c(QRE7525,mdpr7525,dtpr7525))
#  return(QRE7525)
}

NSeff <- function (Qobs, Qsim)
{
    Qsim <- Qsim[!is.na(Qobs)]
        Qobs <- Qobs[!is.na(Qobs)]
	    Qobs <- Qobs[!is.na(Qsim)]
	        Qsim <- Qsim[!is.na(Qsim)]
		    if (length(Qobs) == 0 || length(Qsim) == 0)
		            return(NA)
			        NS <- 1 - (sum((Qobs - Qsim)^2)/sum((Qobs - mean(Qobs))^2))
				    return(NS)
}

########################################################################################################################################
########################################################################################################################################

#minSummary = TRUE
minSummary = FALSE
#plot_sm = TRUE
plot_sm = FALSE
plot_timeseries = FALSE
#plot_timeseries = TRUE

AWR2017    = FALSE
ncFolder   = "../inge/"
#ncFileName = paste(ncFolder,"head_topMF.nc", sep= "")
#varName    = "head_topMF"
ncFileName = paste(ncFolder,"head_bottomMF.nc", sep= "")
varName    = "head_bottomMF"

#forcePlot = TRUE
forcePlot = FALSE
plotC1MinRho  = 0.9
plotC1MaxAqre = 0.1
plotC4MinRho  = 0.1
plotC4MaxRho  = 0.4
plotC4MinAqre = 0.6
plotC4MaxAqre = 0.9

region = "USGS"
startDate = "1958-01-01"
endDate   = "2015-12-31"
minYear   = 5
#minYear   = 0
minAmp    = 0.
1
smFilter  = TRUE
#smFilter  = FALSE
smRhoMin  = 0.5
smNcFileName= "./research-globgm/input/version_1.0/monmean_sm_ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED_1978-2019.nc3"
smVarName= "sm"
all_ilay = TRUE

# input files:
if (region == "USGS"){
 input_folder = "./nwis/"
 measurement_folder = input_folder
##################################
# BEGIN Using soil-moisture filter
 if (TRUE){
  # START FROM 1958:
  modelresults_folder_top = "./model_output/30_arcsec/modelresults_folder_top/"
  modelresults_folder_bot = "./model_output/30_arcsec/modelresults_folder_bot/"
  station_with_xy_file = "./model_output/30_arcsec/summary_proc_d_top_2.txt"
  summaryFile = "summary_1km.txt"
 }
# END Using soil-moisture filter
##################################
#
# 5 arcmin:
##################################
# BEGIN Using soil-moisture filter
if (FALSE){
 modelresults_folder_top = "./model_output/5_arcmin/modelresults_folder_top/"
 modelresults_folder_bot = "./model_output/5_arcmin/modelresults_folder_bot/"
 station_with_xy_file = "./input/30_arcsec/summary_proc_d_top_2.txt"
 summaryFile = "summary_10km.txt"
}
# END Using soil-moisture filter
##################################

}
output_folder = "f:/models/pcr-globwb-30arcsec/model_new/validation/statistics/"

########################################################################################################################################
########################################################################################################################################

output_folder = paste(output_folder,region,'/',sep="")
output_folder_plots = paste(output_folder,"plots",sep="")
output_folder_plots_sm = paste(output_folder,"plots_sm",sep="")
dir.create(output_folder)
dir.create(output_folder_plots)
dir.create(output_folder_plots_sm)
dir.create(paste(output_folder_plots,'/1/',sep=""))
dir.create(paste(output_folder_plots,'/2/',sep=""))
dir.create(paste(output_folder_plots,'/3/',sep=""))
dir.create(paste(output_folder_plots,'/4/',sep=""))

output_summary_txt = paste(output_folder,summaryFile,sep="")

station_with_xy = read.table(station_with_xy_file, header=T,na.strings="nodata")

# list of the months that we want to use
monthly_used = unique(substr(as.character(seq(as.Date(startDate,"%Y-%m-%d"),as.Date(endDate,"%Y-%m-%d"),1)),1,7))

datseq = seq(as.Date(startDate,"%Y-%m-%d"),as.Date(endDate,"%Y-%m-%d"),1)
quarter_used = substr(quarters(as.Date(datseq)),2,2)
quarter_used = sort(unique(paste(substr(monthly_used,1,4),quarter_used,sep="-")))
yearly_used    = sort(unique(substr(datseq,1,4)))

sites = station_with_xy$site_no

first = TRUE
n_top <- 0; n_bot <- 0
for (is in 1:length(sites)) {

 print(paste("***** Station ",is,"/",length(sites),"..."))

 #################################################################################################################################################################
 # read measurement file
 #################################################################################################################################################################

 namemsfile = paste(measurement_folder, sites[is],".txt", sep = "")
 readmsfile = read.table(namemsfile,header=TRUE)

 # throw away data without date
 readmsfile = readmsfile[(which(!is.na(readmsfile[,1]))),]

 # throw away data without water depth or head
 if (region != "USGS"){
  use_wtd = FALSE
  wtd_flag = FALSE
  readmsfile = readmsfile[(which(!is.na(readmsfile$head))),]
 }else{
  n_wtd  = length(which(!is.na(readmsfile$lev_va)))
  n_head = length(which(!is.na(readmsfile$sl_lev_va)))
  if ((n_wtd == 0) & (n_head == 0)){
   print(paste("Skipping for:",sites[is],sep=" "))
   next
  }
  if (n_wtd > 0){
   use_wtd = TRUE
   if (n_head > n_wtd){
    use_wtd = FALSE
   }
  }else{
   use_wtd = FALSE
   if (n_wtd > n_head){
    use_wtd = TRUE
   }
  }

  #if (use_wtd){
  # print(paste("Skipping wtd for:",sites[is],sep=" "))
  # next
  #}

  if (use_wtd){
   wtd_flag = TRUE
   readmsfile = readmsfile[(which(!is.na(readmsfile$lev_va))),] # water-level feet below land surface
  }else{
   wtd_flag = FALSE
   readmsfile = readmsfile[(which(!is.na(readmsfile$sl_lev_va))),] # water-level feet above datum
  }
 }

 # using groundwater head data or depth
 if (region != "USGS"){
  readmsfile = cbind(as.character(readmsfile[,1]),as.numeric(readmsfile$head)) # use actual groundwater heads
 }else{
  if (wtd_flag){
   readmsfile = cbind(as.character(readmsfile[,4]),as.numeric(readmsfile$lev_va)*0.3048) # water table depth to meters
  }else{
   readmsfile = cbind(as.character(readmsfile[,4]),as.numeric(readmsfile$sl_lev_va)*0.3048) # water-level feet above datum
  }
 }

 if (length(readmsfile) <= 2) {
  print(paste("Skipping for:",sites[is]," for not having enough data",sep=" "))
  next
 }

 # sort-ordering data based on date
 readmsfile = readmsfile[order(as.Date(readmsfile[,1],"%Y-%m-%d")),]

 # all data for the entire period
 readmsfile_all = readmsfile[order(as.Date(readmsfile[,1],"%Y-%m-%d")),]

 # ignoring data outside the period of interest
 if (length(readmsfile) > 2) {
 # throwing NODATA VALUES (NA)
  readmsfile  =	  readmsfile[(which(!is.na(readmsfile[,1]))),]
  readmsfile  =	  readmsfile[(which(!is.na(readmsfile[,2]))),]
 if (length(readmsfile) > 2) {
  readmsfile  =	  readmsfile[(which(as.numeric(as.Date(readmsfile[,1],"%Y-%m-%d")) > (as.numeric(as.Date(startDate,"%Y-%m-%d")) - 1))),]
 }
 if (length(readmsfile) > 2) {
  readmsfile  =	  readmsfile[(which(as.numeric(as.Date(readmsfile[,1],"%Y-%m-%d")) < (as.numeric(as.Date(  endDate,"%Y-%m-%d")) + 1))),]}
 }

 # converting to monthly time series
 if (length(readmsfile) > 2) {
  mongw_head  = mat.or.vec(length(monthly_used),1); mongw_head[] = NA
  quargw_head = mat.or.vec(length(quarter_used),1); quargw_head[] = NA
  yeargw_head = mat.or.vec(length(yearly_used),1); yeargw_head[] = NA
  for (im in 1:length(monthly_used)){
   mongw_head[im] = mean(as.numeric(readmsfile[which(substr(readmsfile[,1],1,7) == monthly_used[im]),2]),na.rm=T)
  }
  quarter_meas = paste(substr(readmsfile[,1],1,4),substr(quarters(as.Date(readmsfile[,1])),2,2),sep='-')
  for (iq in 1:length(quarter_used)){
   quargw_head[iq] = mean(as.numeric(readmsfile[which(quarter_meas == quarter_used[iq]),2]),na.rm=T)
  }
  quargw_head[!is.nan(quargw_head)] <- 1
  quargw_head[is.nan(quargw_head)] <- 0
  for (iy in 1:length(yearly_used)){
   yeargw_head[iy] = mean(as.numeric(readmsfile[which(substr(readmsfile[,1],1,4) == yearly_used[iy]),2]),na.rm=T)
  }
  year_measured_data = data.frame(as.numeric(yearly_used), as.numeric(yeargw_head))
  names(year_measured_data)[1] <- "year"
  names(year_measured_data)[2] <- "head"

  measured_data = data.frame(paste(as.character(monthly_used),"-15",sep=""),mongw_head)
  names(measured_data)[1] <- "date"
  names(measured_data)[2] <- "measurement"
  measured_data$date = as.Date(measured_data$date,"%Y-%m-%d")
 }

 # skip in case
 ny = most_consecutive_val(quargw_head)/4
 if (ny < minYear){
  print(paste("Skipping for:",sites[is],', number of years found:',ny,sep=" "))
  next
 }

 # only select parts that >= minYear
 quargw_head_filt <- quargw_head
 quar_rle = rle(quargw_head)
 i_beg = 1
 for (i in 1:length(quar_rle$lengths)) {
  i_end = i_beg + quar_rle$lengths[i] - 1
  ny = quar_rle$lengths[i]/4; flag = quar_rle$values[i]
  if (flag == 1){
   if (ny < minYear){
     quargw_head_filt[i_beg:i_end] <- 0
   }
   # print(paste("i_beg=",i_beg,', i_end=',i_end," val=",ny,sep=" "))
  }
  i_beg <- i_end + 1
 }

 for (iq in 1:length(measured_data$date)){
  d <- measured_data$date[iq]
  q <- paste(substr(d,1,4),substr(quarters(as.Date(d)),2,2),sep='-')
  idx <- which(q == quarter_used)
  if (quargw_head_filt[idx] == 0){
   # print(paste("date=",d," quarter=",q,sep=" "))
   measured_data$measurement[iq] <- NaN
  }
 }

 # detrend
 #tmp = data.frame(as.numeric((1:length(measured_data$measurement))),as.numeric(measured_data$measurement))
 #names(tmp)[1] <- "date"
 #names(tmp)[2] <- "measurement"
 #mslm = lm(formula=measurement~date, data=tmp)
 #measured_data$measurement <- residuals(mslm) + coefficients(mslm)[1]

 for (iq in 1:length(year_measured_data$year)){
  d <- year_measured_data$year[iq]
  qcnt = 0
  for (iqq in (1:4)){
   q <- paste(d,iqq,sep='-')
   idx <- which(q == quarter_used)
   if (quargw_head_filt[idx] != 0){
    qcnt = qcnt + 1
   }
  }
  if (qcnt == 0){
   year_measured_data$head[iq] <- NaN
  }
 }

 if (region == "ADES"){
  latitude  = as.character(station_with_xy$ylatdeg[is])
  longitude = as.character(station_with_xy$xlondeg[is])
 }else if (region == "DINO"){
  latitude  = as.character(station_with_xy$ylatdeg[is])
  longitude = as.character(station_with_xy$xlondeg[is])
 }else if (region == "DINOF"){
  latitude  = as.character(station_with_xy$latitude[is])
  longitude = as.character(station_with_xy$longitude[is])
 }else{
  latitude  = as.character(station_with_xy$dec_lat_va[is])
  longitude = as.character(station_with_xy$dec_long_va[is])
 }

 #################################################################################################################################################################
 # Layer selection; apply soil moister filter
 #################################################################################################################################################################
 skip <- FALSE
 ilay <- 0

 well_depth = as.numeric(station_with_xy$well_depth_va[is])
 if (!is.na(well_depth)){
  well_depth = well_depth * 0.3048 # feet --> m
  thick_top_lay = as.numeric(station_with_xy$d_top_2[is])
  if (well_depth < thick_top_lay){
   ilay <- 1
  }else{
   ilay <- 2
  }
 }

 if (smFilter & (ilay == 0)){
  command_line = paste("get_value.bat", latitude, longitude,
                       as.character(smNcFileName), as.character(smVarName), sep=" ")
  tv = system(command_line, intern = TRUE); # print(tv)
  smfile = read.table("tmp.txt",sep=";",header=F,na.strings="-9999.0")
  # converting to monthly time series
  mongw_sm = mat.or.vec(length(monthly_used),1); mongw_sm[] = NA

  for (im in 1:length(monthly_used)){
   mongw_sm[im] = mean(as.numeric(smfile[which(substr(smfile[,1],1,7) == monthly_used[im]),2]),na.rm=T)
  }
  sm_data = data.frame(paste(as.character(monthly_used),"-15",sep=""),mongw_sm)
  names(sm_data)[1] <- "date"
  names(sm_data)[2] <- "soil_moisture"
  sm_data$date = as.Date(sm_data$date,"%Y-%m-%d")

  ##############
  compare_sm = mat.or.vec(length(monthly_used),1); compare_sm[] = 0
  compare_sm[!is.na(sm_data$soil_moisture) & !is.na(measured_data$measurement)] <- 1
  quarter_sm = paste(substr(sm_data$date,1,4),substr(quarters(as.Date(sm_data$date)),2,2),sep='-')
  quar_compare_sm = mat.or.vec(length(quarter_used),1); quar_compare_sm[] = 0

  for (iq in 1:length(quarter_used)){
   quar_compare_sm[iq] = max(as.numeric(compare_sm[which(quarter_sm == quarter_used[iq])]),na.rm=T)
  }
  quar_compare_sm_filt <- quar_compare_sm
  quar_rle = rle(quar_compare_sm)
  i_beg = 1
  for (i in 1:length(quar_rle$lengths)) {
   i_end = i_beg + quar_rle$lengths[i] - 1
   ny = quar_rle$lengths[i]/4; flag = quar_rle$values[i]
   if (flag == 1){
    # print(ny)
    if (ny < minYear){
      quar_compare_sm_filt[i_beg:i_end] <- 0
    }
    # print(paste("i_beg=",i_beg,', i_end=',i_end," val=",ny,sep=" "))
   }
   i_beg <- i_end + 1
  }

  sm_data_filt <- sm_data
  measured_data_filt <- measured_data
  for (iq in 1:length(sm_data$date)){
   d <- sm_data$date[iq]
   q <- paste(substr(d,1,4),substr(quarters(as.Date(d)),2,2),sep='-')
   idx <- which(q == quarter_used)
   if (quar_compare_sm_filt[idx] == 0){
    # print(paste("date=",d," quarter=",q,sep=" "))
    sm_data_filt$soil_moisture[iq] <- NaN
    measured_data_filt$measurement[iq] <- NaN
   }
  }
  #
  nsm <- length(which(!is.na(sm_data_filt$soil_moisture)))
  if (nsm == 0){
   print(paste("Skipping for ",sites[is],": no soil-moisture data found",sep=" "))
  }else{
   print(paste("Soil moisture data found for ",sites[is],nsm,sep=" "))
   #print(paste(sites[is]))
   #stop
   # change sign in case measurement is a water depth
   if (use_wtd){
    sm_data_filt[,2] <- sm_data_filt[,2]*-1.0
   }
   #smRho = cor(as.numeric(measured_data_filt$measurement),as.numeric(sm_data_filt$soil_moisture),use="complete.obs");
   smRho = cor(as.numeric(measured_data_filt$measurement),as.numeric(sm_data_filt$soil_moisture),use="na.or.complete");
   smRhoLag <- NA
   rholag = FALSE
   if (is.na(smRho)){
    print(paste("Skipping for",sites[is],": could not compute soil-moisture correlation",sep=" "))
   }else{
    # try to find the best correlation using lags
    for (lag in -3:3){
     if (lag == 0){
      next
     }
     if (rholag){
      next
     }
     sm_data_filt <- slide(sm_data_filt, "soil_moisture", NewVar = "soil_moisture_lag", slideBy = lag)
     smRhoLag = cor(as.numeric(measured_data_filt$measurement),as.numeric(sm_data_filt$soil_moisture_lag),use="na.or.complete")
     if (!is.na(smRhoLag)){
      smRho = max(smRhoLag,smRho)
     }
    } # end for
    if (smRho < smRhoMin){
     print(paste("No soil-moisture correlation found for ",sites[is],": rho =",smRho,sep=" "))
     ilay = 2
    }else{
     print(paste("Soil-moisture correlation found for ",sites[is],": rho =",smRho,sep=" "))
     rholag = TRUE
     ilay = 1
    }
   }
   #
   if (is.na(smRhoLag)){
    rholag = FALSE
   }
   #
   # plot
   if ((plot_sm) & (!is.na(smRho))){
    merged_date = merge(measured_data_filt, sm_data_filt, by = c("date"), all = TRUE)
    month_measu = as.numeric(merged_date$measurement)
    avg_msmonth = mean(measured_data_filt$measurement,na.rm=T)
    if (rholag){
     month_soilm = as.numeric(merged_date$soil_moisture_lag)
     avg_smmonth = mean(sm_data_filt$soil_moisture_lag,na.rm=T)
    }else{
     month_soilm = as.numeric(merged_date$soil_moisture)
     avg_smmonth = mean(sm_data_filt$soil_moisture,na.rm=T)
    }
    month_measu <- month_measu - avg_msmonth
    month_soilm <- month_soilm - avg_smmonth
    month_measu <- rescale(month_measu)
    month_soilm <- rescale(month_soilm)
    measu = data.frame(merged_date$date,month_measu); names(measu) = cbind("date", "data")
    soilm = data.frame(merged_date$date,month_soilm); names(soilm) = cbind("date", "model")

    if (smRho > 0.5){
     classstr = "good"
     if (smRho > 0.9){
      classstr = "very_good"
     }
    }else{
     classstr = "bad"
     if (smRho < 0.1){
      classstr = "very_bad"
     }
    }

    if (wtd_flag){
     outpltfl = paste(output_folder_plots_sm,"/",classstr,"_",sites[is],"_gwtd.png",sep="")
    }else{
     outpltfl = paste(output_folder_plots_sm,"/",classstr,"_",sites[is],"_gwhead.png",sep="")
    }
    measu_soilm = merge(measu,soilm,by="date",all.x=TRUE)
    measu_soilm$date = as.Date(as.character(measu_soilm[,1]),"%Y-%m-%d")
    print(outpltfl)
    y_min = min(min(measu_soilm[,2],na.rm=T),min(measu_soilm[,3],na.rm=T))
    y_max = max(max(measu_soilm[,2],na.rm=T),max(measu_soilm[,3],na.rm=T))
    #if((classstr == "very_good")||(classstr == "very_bad")){
     plot_soilm(outpltfl, measu_soilm, date, startDate, endDate, y_min, y_max, sites[is], smRho, wtd_flag)
    #}
    skip = TRUE
   }
   # cleanup
   rm(smRho)
  }
  # cleanup
  unlink("tmp.txt")
  rm(smfile,sm_data,sm_data_filt, measured_data_filt, mongw_sm)
  rm(quar_compare_sm_filt)
 }

 if (ilay == 0){
  print(paste("No matching model layer found for ",sites[is]," "))
  skip = TRUE
 }

 if (ilay == 2){
  print(paste("Skipping bottom layer for ",sites[is]," "))
  if (!all_ilay){
    skip = TRUE
  }
 }

 if (skip){
  next
 }

 #################################################################################################################################################################
 # read model file
 #################################################################################################################################################################

 if (AWR2017){
  skip = FALSE
  command_line = paste("get_value.bat", latitude, longitude,
                       as.character(ncFileName), as.character(varName), sep=" ")
  tv = system(command_line, intern = TRUE); #print(tv)
  # - read temporary file and delete it
  model_result = read.table("tmp.txt",sep=";",header=F)
  system("rm -r tmp.txt")
  model_result[,1] = as.Date(model_result[,1],origin=as.Date(originDate,"%Y-%m-%d"))
  names(model_result)[1] <- "date"
  names(model_result)[2] <- "model_result"
  # - change the date to 15
  correct_date = paste(substr(as.character(model_result$date),1,7),"-15",sep="")
  model_result$date = as.Date(correct_date,"%Y-%m-%d")

 }else{
  if (ilay == 1){
   modelresults_folder = modelresults_folder_top
  }else{
  modelresults_folder = modelresults_folder_bot
  }
  namemofile = paste(modelresults_folder, sites[is],".txt", sep = "")
  if (!file.exists(namemofile)){
   print(paste("Skipping: could not find model file for",sites[is]," "))
   next
  }
  readmofile = read.table(namemofile,header=TRUE)
  #
  if (ilay == 1){
   n_top <- n_top + 1
  }else{
   n_bot <- n_bot + 1
  }
  #
  # throw away unused data
  if (wtd_flag){
   readmofile = subset(readmofile,select=-head) # throw away head
  }else{
   readmofile = subset(readmofile,select=-wtd) # throw away wtd
  }

  # converting to monthly time series
  mongw_head = mat.or.vec(length(monthly_used),1)
  mongw_head[] = NA
  for (im in 1:length(monthly_used)){
   mongw_head[im] = mean(as.numeric(readmofile[which(substr(readmofile[,1],1,7) == monthly_used[im]),2]),na.rm=T)
  }
  model_result = data.frame(paste(as.character(monthly_used),"-15",sep=""),mongw_head)
  names(model_result)[1] <- "date"
  names(model_result)[2] <- "model_result"
  model_result$date = as.Date(model_result$date,"%Y-%m-%d")
 }

 yeargw_head = mat.or.vec(length(yearly_used),1)
 yeargw_head[] = NA
 for (iy in 1:length(yearly_used)){
  yeargw_head[iy] = mean(as.numeric(readmofile[which(substr(readmofile[,1],1,4) == yearly_used[iy]),2]),na.rm=T)
 }
 year_model_result = data.frame(as.numeric(yearly_used), as.numeric(yeargw_head))
 names(year_model_result)[1] <- "year"
 names(year_model_result)[2] <- "head"
 for (iq in 1:length(year_measured_data$year)){
   if (is.na(year_measured_data$head[iq])){
    year_model_result$head[iq] <- NaN
   }
 }

 #################################################################################################################################################################
 # merging model and measurement
 #################################################################################################################################################################

 # test
 #model_result = measured_data
 #names(model_result)[2] <- "model_result"

 if (length(readmsfile) > 2) {
  merged_date = merge(measured_data, model_result, by = c("date"), all = TRUE)
 }

 #################################################################################################################################################################
 # evaluation
 #################################################################################################################################################################
 if (length(readmsfile) > 2) {
 month_msdata   = as.numeric(merged_date$measurement)
 month_modelo   = as.numeric(merged_date$model_result)

 RHO_p_month    =   cor(month_msdata,month_modelo,use="pairwise.complete.obs"); # print(paste("RHO_p_month",RHO_p_month,sep=" "))
# RHO_p_month    =   cor(month_msdata,month_modelo,use="complete.obs"); # print(paste("RHO_p_month",RHO_p_month,sep=" "))
 if ((is.na(RHO_p_month))||(is.infinite(RHO_p_month))){
  print(paste("Skipping for ",sites[is],": infinite/nan RHO_p_month!",sep=" "))
  next
 }

 QRE7525_evalua_all = QRE7525(month_msdata,month_modelo); # print(paste("QRE7525_evalua",QRE7525_evalua,sep=" "))
 QRE7525_evalua  = QRE7525_evalua_all[1]
 IQ7525_moevalua = QRE7525_evalua_all[2]
 IQ7525_msevalua = QRE7525_evalua_all[3]

 if ((is.na(QRE7525_evalua))||(is.infinite(QRE7525_evalua))){
  print(paste("Skipping for ",sites[is],": infinite/nan QRE7525_evalua!",sep=" "))
  next
 }

 if (!minSummary){
  R2____month    =    R2(month_msdata,month_modelo); # print(paste("R2____month",R2____month,sep=" "))
  R2adj_month    =  R2ad(month_msdata,month_modelo); # print(paste("R2adj_month",R2adj_month,sep=" "))
  #
  NSeff_month    = NSeff(month_msdata,month_modelo); # print(paste("NSeff_month",NSeff_month,sep=" "))
  RMSE__month    =  rmse(month_msdata,month_modelo); # print(paste("RMSE__month",RMSE__month,sep=" "))
  MAE___month    =   mae(month_msdata,month_modelo); # print(paste("MAE___month",MAE___month,sep=" "))
  BIAS__month    =  bias(month_msdata,month_modelo); # print(paste("BIAS__month",BIAS__month,sep=" "))
  #
  # calculate monthly model performance
  avg_msmonth    =  mean(month_msdata,na.rm=T); # print(paste("avg_msmonth",avg_msmonth,sep=" "))
  avg_momonth    =  mean(month_modelo,na.rm=T); # print(paste("avg_momonth",avg_momonth,sep=" "))

  NSeff_month_nb = NSeff(month_msdata-avg_msmonth,month_modelo-avg_momonth); # print(paste("NSeff_month_nb",NSeff_month_nb,sep=" "))
  RMSE__month_nb =  rmse(month_msdata-avg_msmonth,month_modelo-avg_momonth); # print(paste("RMSE__month_nb",RMSE__month_nb,sep=" "))
  MAE___month_nb =   mae(month_msdata-avg_msmonth,month_modelo-avg_momonth); # print(paste("MAE___month_nb",MAE___month_nb,sep=" "))
  BIAS__month_nb =  bias(month_msdata-avg_msmonth,month_modelo-avg_momonth); # print(paste("BIAS__month_nb",BIAS__month_nb,sep=" "))
  #
  ms_slope <- lm(formula=head~year, data=year_measured_data)$coefficients[2]
  mo_slope <- lm(formula=head~year, data=year_model_result)$coefficients[2]
  if (use_wtd){
    ms_slope <- -ms_slope
    mo_slope <- -mo_slope
  }
 }

 # classify
 if (abs(QRE7525_evalua) <= 0.5){
  if (RHO_p_month > 0.5){
   iperf = 1
  }else{
   iperf = 3
  }
 }else{
  if (RHO_p_month > 0.5){
   iperf = 2
  }else{
   iperf = 4
  }
 }

 if (abs(IQ7525_msevalua) < minAmp){
  print(paste("Skipping for:",sites[is],', minmimum meas. amp found:',abs(IQ7525_msevalua),sep=" "))
  next
 }

 print(paste("IQ7525_msevalua:",IQ7525_msevalua,sep=" "))

 } # end if (length(readmsfile) > 2)

 #################################################################################################################################################################
 # plotting time series
 #################################################################################################################################################################
 if ((length(readmsfile) > 2) & plot_timeseries) {
  plotts = FALSE
  # class I
  if (iperf == 1){
   if ((RHO_p_month >= plotC1MinRho) && (abs(QRE7525_evalua) <= plotC1MaxAqre)){
    plotts = TRUE
   }
  }
  if (iperf == 4){
   # worst matches
   if ((RHO_p_month <= plotC4MinRho) && (abs(QRE7525_evalua) >= plotC4MaxAqre)){
    plotts = TRUE
   }
   # matches close to class II and III
   if ((RHO_p_month >= plotC4MaxRho) || (abs(QRE7525_evalua) <= plotC4MinAqre)){
    plotts = TRUE
   }
  }
  if (plotts || forcePlot){
   ## Plotting the chart !!!
   model = data.frame(merged_date$date,month_modelo - avg_momonth); names(model) = cbind("date","model")
   measu = data.frame(merged_date$date,month_msdata - avg_msmonth); names(measu) = cbind("date", "data")
   if (wtd_flag){
    outpltfl = paste(output_folder_plots,"/",iperf,"/",sites[is],"_gwtd.png",sep="")
   }else{
    outpltfl = paste(output_folder_plots,"/",iperf,"/",sites[is],"_gwhead.png",sep="")
   }
   model_measu = merge(model,measu,by="date",all.x=TRUE)

   model_measu$date = as.Date(as.character(model_measu[,1]),"%Y-%m-%d")
   print(outpltfl)
   y_min = min(min(model_measu[,2],na.rm=T),min(model_measu[,3],na.rm=T))
   y_max = max(max(model_measu[,2],na.rm=T),max(model_measu[,3],na.rm=T))
   plot_comparison(outpltfl, model_measu, date, startDate, endDate, y_min, y_max, sites[is],
    IQ7525_moevalua, IQ7525_msevalua, QRE7525_evalua, RHO_p_month, RMSE__month, BIAS__month, wtd_flag)
  }
 } # end if (length(readmsfile) > 2)

 #################################################################################################################################################################
 # summary
 #################################################################################################################################################################
 if (first){
  if (minSummary){
   header = c("station_name",   "latitude",     "longitude",     "RHO_p_month",   "QRE7525_evalua", "performance")
  }else{
   header = c("station_name",   "latitude",    "longitude",     "avg_msmonth",   "avg_momonth",
              "RHO_p_month" ,   "R2____month", "R2adj_month",   "NSeff_month",   "RMSE__month",
              "MAE___month",    "BIAS__month", "NSeff_month_nb","RMSE__month_nb","MAE___month_nb",
              "BIAS__month_nb", "QRE7525_evalua", "performance", "ms_slope", "mo_slope", "layer")
  }
  cat(header,"\n",sep="\t",file=output_summary_txt,append=FALSE)
  first = FALSE
 }

 if (length(readmsfile) > 2) {
  if (minSummary){
   summary = c(sites[is], latitude, longitude, RHO_p_month, QRE7525_evalua, iperf)
  }else{
   summary = c(sites[is],      latitude,    longitude,      avg_msmonth,    avg_momonth,
               RHO_p_month,    R2____month, R2adj_month,    NSeff_month,    RMSE__month,
               MAE___month,    BIAS__month, NSeff_month_nb, RMSE__month_nb, MAE___month_nb,
               BIAS__month_nb, QRE7525_evalua, iperf,       ms_slope,       mo_slope,
               ilay)
  }
  cat(summary,"\n",sep="\t",file=output_summary_txt,append=TRUE)
 } # end if (length(readmsfile) > 2)

} # end for (is in 1:length(station_with_xy$station))

print(paste("Number classified as top:",n_top,sep=" "))
print(paste("Number classified as bot:",n_bot,sep=" "))
print("***** Done *****")
