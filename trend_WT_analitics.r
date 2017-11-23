# Definisci una directory di lavoro

setwd(".")

source("load_lib_trends.r")
source("aux_trend.r")

library(lubridate)
library(igraph)

####################################################################################
WTS_xts=readRDS("data/WTS_xts.rds")
WTS_xts_1979_2015=WTS_xts["1979-01-01/2015-12-31"]

mat_data_79_15=data.frame(dates=as.Date(index(WTS_xts_1979_2015)),
                          Y_M=format(as.Date(index(WTS_xts_1979_2015)),"%Y-%m"),
                          as.data.frame(WTS_xts_1979_2015))
row.names(mat_data_79_15)=NULL

leapdays=!grepl("-02-29",mat_data_79_15$dates)

mat_data_79_15=mat_data_79_15[which(leapdays==T),]
mat_data_79_15$season=month2season(as.numeric(format(mat_data_79_15$dates,"%m")),label=T)
mat_data_79_15$year=format(mat_data_79_15$dates,"%Y")
label_year=sapply(1:nrow(mat_data_79_15),function(x) ifelse((as.numeric(format(mat_data_79_15$dates[x],"%j")>335) & (mat_data_79_15$season[x]=="DJF")),as.numeric(mat_data_79_15$year[x])+1,as.numeric(mat_data_79_15$year[x])))
mat_data_79_15$year_season=paste0(mat_data_79_15$season,"_",label_year)
mat_data_79_15$month=as.numeric(format(mat_data_79_15$dates,"%m"))

saveRDS(mat_data_79_15,"mat_data_79_15.rds")


mat_data_79_15_year=split(mat_data_79_15,mat_data_79_15$year)
mat_data_79_15_season=split(mat_data_79_15,mat_data_79_15$year_season)
mat_data_79_15_month=split(mat_data_79_15,mat_data_79_15$month)

mat_data_79_15_season_DJF=mat_data_79_15_season[1:38]
mat_data_79_15_season_JJA=mat_data_79_15_season[39:75]
mat_data_79_15_season_MAM=mat_data_79_15_season[76:112]
mat_data_79_15_season_SON=mat_data_79_15_season[113:149]


###################################################################################

freqsWTpct=function(y,par="WT_pct09") sapply(1:9,function(x) length(which(y[par]==x)))
freqsWTsan=function(y,par="WT_san09") sapply(1:9,function(x) length(which(y[par]==x)))

mat_data_79_15_year_f_pct9=do.call("rbind",lapply(mat_data_79_15_year,freqsWTpct))
mat_data_79_15_year_f_san9=do.call("rbind",lapply(mat_data_79_15_year,freqsWTsan))

mat_data_79_15_season_f_pct9=list()[1:4]
mat_data_79_15_season_f_san9=list()[1:4]

mat_data_79_15_month_f_pct9=list()[1:12]
mat_data_79_15_month_f_san9=list()[1:12]

####################################################################################################################################################################################

mat_data_79_15_month=split(mat_data_79_15,as.character(mat_data_79_15$month))

####################################################################################################################################################################################



mat_data_79_15_season_f_pct9[[1]]=do.call("rbind",lapply(mat_data_79_15_season_DJF,freqsWTpct))
mat_data_79_15_season_f_pct9[[2]]=do.call("rbind",lapply(mat_data_79_15_season_MAM,freqsWTpct))
mat_data_79_15_season_f_pct9[[3]]=do.call("rbind",lapply(mat_data_79_15_season_JJA,freqsWTpct))
mat_data_79_15_season_f_pct9[[4]]=do.call("rbind",lapply(mat_data_79_15_season_SON,freqsWTpct))

mat_data_79_15_season_f_san9[[1]]=do.call("rbind",lapply(mat_data_79_15_season_DJF,freqsWTsan))
mat_data_79_15_season_f_san9[[2]]=do.call("rbind",lapply(mat_data_79_15_season_MAM,freqsWTsan))
mat_data_79_15_season_f_san9[[3]]=do.call("rbind",lapply(mat_data_79_15_season_JJA,freqsWTsan))
mat_data_79_15_season_f_san9[[4]]=do.call("rbind",lapply(mat_data_79_15_season_SON,freqsWTsan))

meselist=c(1,5,6,7,8,9,10,11,12,2,3,4)
mat_data_79_15_month=mat_data_79_15_month[meselist]

for ( i in 1:12) {
  
  mat_data_79_15_month_f_pct9[[i]]=do.call("rbind",lapply(split(mat_data_79_15_month[[i]],as.character(mat_data_79_15_month[[i]]$year)),freqsWTpct))
  mat_data_79_15_month_f_san9[[i]]=do.call("rbind",lapply(split(mat_data_79_15_month[[i]],as.character(mat_data_79_15_month[[i]]$year)),freqsWTsan))
  
}



####################################################################################################################################################################################

analize_year_trend=function(x,ystart = 1979,yend=2015) {
                                              res_model=list()[1:dim(x)[2]]
                                              res_model_best=list()[1:dim(x)[2]]
                                              res_linear_trend=list()[1:dim(x)[2]]
                                              res_sen_slope=list()[1:dim(x)[2]]
                                              res_devst=list()[1:dim(x)[2]]
                                              1:length(ystart:yend)
                                              for ( i in 1:dim(x)[2]) {res_model[[i]]=envcpt(ts(as.numeric(x[,i]),start=ystart,end=yend,frequency = 1));
                                                                       res_model_best[[i]]= which.min(AIC(res_model[[i]]))
                                                                       res_linear_trend[[i]]=res_model[[i]]$trend
                                                                       res_sen_slope[[i]]=sens.slope(ts(as.numeric(x[,i]),start=ystart,end=yend,frequency = 1), conf.level = 0.95)
                                                                       res_devst[[i]]=sd(as.numeric(x[1:length(ystart:yend),i]))
                                                                       }
                                              
                                              
                                              res=list(res_model=res_model,
                                                       res_model_best=res_model_best,
                                                       res_linear_trend=res_linear_trend,
                                                       res_sen_slope,
                                                       res_devst
                                                       )
                                              return(res)
                                              }

  

####################################################################################################################################################################################



pct9_yearly_trend=analize_year_trend(mat_data_79_15_year_f_pct9)
san9_yearly_trend=analize_year_trend(mat_data_79_15_year_f_san9)

saveRDS(pct9_yearly_trend,"pct9_yearly_trend.rds")
saveRDS(san9_yearly_trend,"san9_yearly_trend.rds")



pct9_DJF_trend=analize_year_trend(mat_data_79_15_season_f_pct9[[1]][2:37,],ystart = 1980)
pct9_MAM_trend=analize_year_trend(mat_data_79_15_season_f_pct9[[2]])
pct9_JJA_trend=analize_year_trend(mat_data_79_15_season_f_pct9[[3]])
pct9_SON_trend=analize_year_trend(mat_data_79_15_season_f_pct9[[4]])


saveRDS(pct9_DJF_trend,"pct9_DJF_trend.rds")
saveRDS(pct9_MAM_trend,"pct9_MAM_trend.rds")
saveRDS(pct9_JJA_trend,"pct9_JJA_trend.rds")
saveRDS(pct9_SON_trend,"pct9_SON_trend.rds")

san9_DJF_trend=analize_year_trend(mat_data_79_15_season_f_san9[[1]][2:37,],ystart = 1980)
san9_MAM_trend=analize_year_trend(mat_data_79_15_season_f_san9[[2]])
san9_JJA_trend=analize_year_trend(mat_data_79_15_season_f_san9[[3]])
san9_SON_trend=analize_year_trend(mat_data_79_15_season_f_san9[[4]])

saveRDS(san9_DJF_trend,"san9_DJF_trend.rds")
saveRDS(san9_MAM_trend,"san9_MAM_trend.rds")
saveRDS(san9_JJA_trend,"san9_JJA_trend.rds")
saveRDS(san9_SON_trend,"san9_SON_trend.rds")


res_monthly_trend_pct9=list()[1:12]
res_monthly_trend_san9=list()[1:12]

for ( i in 1:12) {
  
  res_monthly_trend_pct9[[i]]=analize_year_trend(mat_data_79_15_month_f_pct9[[i]])
  res_monthly_trend_san9[[i]]=analize_year_trend(mat_data_79_15_month_f_san9[[i]])
  
}

saveRDS(res_monthly_trend_pct9,"res_monthly_trend_pct9.rds")
saveRDS(res_monthly_trend_san9,"res_monthly_trend_san9.rds")


###################################################################################




####################################################################################################################################################################################


dir.create("images_trend")



  for ( i in 1:9)  {
    png(paste0("images_trend/","WT_",i,"_PCT9_yearly_FIT_models.png"))
    try(plot(pct9_yearly_trend$res_model[[i]],type="fit",main=paste0("WT_",i," PCT9 Yearly Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_yearly_AIC_models.png"))
    try(plot(pct9_yearly_trend$res_model[[i]],type="aic",main=paste0("WT_",i," PCT9 Yearly AIC ")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_season_DJF_FIT_models.png"))
    try(plot(pct9_DJF_trend$res_model[[i]],type="fit",main=paste0("WT_",i," PCT9 DJF Season Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_season_DJF_AIC_models.png"))
    try(plot(pct9_DJF_trend$res_model[[i]],type="aic",main=paste0("WT_",i," PCT9 DJF Season AIC ")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_season_MAM_FIT_models.png"))
    try(plot(pct9_MAM_trend$res_model[[i]],type="fit",main=paste0("WT_",i," PCT9 MAM Season Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_season_MAM_AIC_models.png"))
    try(plot(pct9_MAM_trend$res_model[[i]],type="aic",main=paste0("WT_",i," PCT9 MAM Season AIC ")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_season_JJA_FIT_models.png"))
    try(plot(pct9_JJA_trend$res_model[[i]],type="fit",main=paste0("WT_",i," PCT9 JJA Season Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_season_JJA_AIC_models.png"))
    try(plot(pct9_JJA_trend$res_model[[i]],type="aic",main=paste0("WT_",i," PCT9 JJA Season AIC ")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_season_SON_FIT_models.png"))
    try(plot(pct9_SON_trend$res_model[[i]],type="fit",main=paste0("WT_",i," PCT9 SON Season Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_PCT9_season_SON_AIC_models.png"))
    try(plot(pct9_SON_trend$res_model[[i]],type="aic",main=paste0("WT_",i," PCT9 SON Season AIC ")))
    dev.off()    
    
    png(paste0("images_trend/","WT_",i,"_SAN9_yearly_FIT_models.png"))
    try(plot(san9_yearly_trend$res_model[[i]],type="fit",main=paste0("WT_",i," SAN9 Yearly Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_yearly_AIC_models.png"))
    try(plot(san9_yearly_trend$res_model[[i]],type="aic",main=paste0("WT_",i," SAN9 Yearly AIC ")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_season_DJF_FIT_models.png"))
    try(plot(san9_DJF_trend$res_model[[i]],type="fit",main=paste0("WT_",i," SAN9 DJF Season Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_season_DJF_AIC_models.png"))
    try(plot(san9_DJF_trend$res_model[[i]],type="aic",main=paste0("WT_",i," SAN9 DJF Season AIC ")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_season_MAM_FIT_models.png"))
    try(plot(san9_MAM_trend$res_model[[i]],type="fit",main=paste0("WT_",i," SAN9 MAM Season Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_season_MAM_AIC_models.png"))
    try(plot(san9_MAM_trend$res_model[[i]],type="aic",main=paste0("WT_",i," SAN9 MAM Season AIC ")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_season_JJA_FIT_models.png"))
    try(plot(san9_JJA_trend$res_model[[i]],type="fit",main=paste0("WT_",i," SAN9 JJA Season Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_season_JJA_AIC_models.png"))
    try(plot(san9_JJA_trend$res_model[[i]],type="aic",main=paste0("WT_",i," SAN9 JJA Season AIC ")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_season_SON_FIT_models.png"))
    try(plot(san9_SON_trend$res_model[[i]],type="fit",main=paste0("WT_",i," SAN9 SON Season Model Series")))
    dev.off()
    
    png(paste0("images_trend/","WT_",i,"_SAN9_season_SON_AIC_models.png"))
    try(plot(san9_SON_trend$res_model[[i]],type="aic",main=paste0("WT_",i," SAN9 SON Season AIC ")))
    dev.off()
  }

dir.create("images_trend_mese")

  for (i in 1:9)  {
         for (j in 1:12)  {
           png(paste0("images_trend_mese/","WT_",i,"_PCT9_month_",j,"_FIT_models.png"))
           try(plot(res_monthly_trend_pct9[[j]]$res_model[[i]],type="fit",main=paste0("WT_",i," PCT9 Month ",j," Model Series")))
           dev.off()
           
           png(paste0("images_trend_mese/","WT_",i,"_PCT9_month_",j,"_AIC_models.png"))
           try(plot(res_monthly_trend_pct9[[j]]$res_model[[i]],type="aic",main=paste0("WT_",i," PCT9 Month ",j," AIC Model")))
           dev.off()
           png(paste0("images_trend_mese/","WT_",i,"_SAN9_month_",j,"_FIT_models.png"))
           try(plot(res_monthly_trend_san9[[j]]$res_model[[i]],type="fit",main=paste0("WT_",i," SAN9 Month ",j," Model Series")))
           dev.off()
           
           png(paste0("images_trend_mese/","WT_",i,"_SAN9_month_",j,"_AIC_models.png"))
           try(plot(res_monthly_trend_san9[[j]]$res_model[[i]],type="aic",main=paste0("WT_",i," SAN9 Month ",j," AIC Model")))
           dev.off()
           
           }
   
           }

dir.create("data_xls")


names_WT=c("year",paste0("WT_",1:9))

temp=data.frame(year=1979:2015,mat_data_79_15_year_f_pct9)
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Year_freq.xls",data=temp,"freq_PCT9")
temp=data.frame(year=1979:2015,mat_data_79_15_year_f_san9)
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Year_freq.xls",data=temp,"freq_SAN9")



#########################################################################################

temp=data.frame(year=1980:2015,mat_data_79_15_season_f_pct9[[1]][2:37,])
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Season_freq.xls",data=temp,"DJF_freq_PCT9")
temp=data.frame(year=1979:2015,mat_data_79_15_season_f_pct9[[2]])
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Season_freq.xls",data=temp,"MAM_freq_PCT9")
temp=data.frame(year=1979:2015,mat_data_79_15_season_f_pct9[[3]])
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Season_freq.xls",data=temp,"JJA_freq_PCT9")
temp=data.frame(year=1979:2015,mat_data_79_15_season_f_pct9[[4]])
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Season_freq.xls",data=temp,"SON_freq_PCT9")

temp=data.frame(year=1980:2015,mat_data_79_15_season_f_san9[[1]][2:37,])
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Season_freq.xls",data=temp,"DJF_freq_SAN9")
temp=data.frame(year=1979:2015,mat_data_79_15_season_f_san9[[2]])
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Season_freq.xls",data=temp,"MAM_freq_SAN9")
temp=data.frame(year=1979:2015,mat_data_79_15_season_f_san9[[3]])
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Season_freq.xls",data=temp,"JJA_freq_SAN9")
temp=data.frame(year=1979:2015,mat_data_79_15_season_f_san9[[4]])
names(temp)=names_WT
XLConnect::writeWorksheetToFile("data_xls/Season_freq.xls",data=temp,"SON_freq_SAN9")


  for (j in 1:12)  {
    temp=data.frame(year=1979:2015,mat_data_79_15_month_f_pct9[[j]])
    names(temp)=names_WT
    XLConnect::writeWorksheetToFile("data_xls/Month_freq.xls",data=temp,paste0("Month ",j," PCT9"))
    
    temp=data.frame(year=1979:2015,mat_data_79_15_month_f_san9[[j]])
    names(temp)=names_WT
    XLConnect::writeWorksheetToFile("data_xls/Month_freq.xls",data=temp,paste0("Month ",j," SAN9"))
    
    }
  
