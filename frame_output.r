setwd(".")
options(java.parameters = "-Xmx4g" )
library(XLConnect)

source("load_lib_trends.r")
source("aux_trend.r")



res_pct9=list(
pct9_DJF_trend=readRDS("data/pct9_DJF_trend.rds"),
pct9_MAM_trend=readRDS("data/pct9_MAM_trend.rds"),
pct9_JJA_trend=readRDS("data/pct9_JJA_trend.rds"),
pct9_SON_trend=readRDS("data/pct9_SON_trend.rds"),
pct9_yearly_trend=readRDS("data/pct9_yearly_trend.rds"),
res_monthly_trend_pct9=readRDS("data/res_monthly_trend_pct9.rds"))

res_san9=list(
san9_DJF_trend=readRDS("data/san9_DJF_trend.rds"),
san9_MAM_trend=readRDS("data/san9_MAM_trend.rds"),
san9_JJA_trend=readRDS("data/san9_JJA_trend.rds"),
san9_SON_trend=readRDS("data/san9_SON_trend.rds"),
san9_yearly_trend=readRDS("data/san9_yearly_trend.rds"),
res_monthly_trend_san9=readRDS("data/res_monthly_trend_san9.rds"))


periods=c("DJF","MAM","JJA","SON","YEAR",paste0("MONTH_",1:12))

####################################################################################################
res_sd=data.frame(t(1:length(periods)))
res_sd[1:9,]=NA

res_z=data.frame(t(1:length(periods)))
res_z[1:9,]=NA

res_pvalue=data.frame(t(1:length(periods)))
res_pvalue[1:9,]=NA

res_sen=data.frame(t(1:length(periods)))
res_sen[1:9,]=NA

res_I=data.frame(t(1:length(periods)))
res_I[1:9,]=NA

res_pen=data.frame(t(1:length(periods)))
res_pen[1:9,]=NA


######################################################

for ( j in 1:5) {
for ( i in 1:9)
  {
   
  res_sd[i,j]=res_san9[[j]][[5]][[i]]
  res_sen[i,j]=as.numeric(res_san9[[j]][[4]][[i]]$estimates)
  res_z[i,j]=as.numeric(res_san9[[j]][[4]][[i]]$statistic)
  res_pvalue[i,j]=as.numeric(res_san9[[j]][[4]][[i]]$p.value)
  res_I[i,j]=res_san9[[j]][[3]][[i]]$coefficients[1]
  res_pen[i,j]=res_san9[[j]][[3]][[i]]$coefficients[2]
  
  
}
}

for ( i in 1:9)
{
  
  for ( j in 1:12)
  {
    res_sd[i,(j+5)]=res_san9[[6]][[j]][[5]][[i]]
    res_sen[i,(j+5)]=as.numeric(res_san9[[6]][[j]][[4]][[i]]$estimates)
    res_z[i,(j+5)]=as.numeric(res_san9[[6]][[j]][[4]][[i]]$statistic)
    res_pvalue[i,(j+5)]=as.numeric(res_san9[[6]][[j]][[4]][[i]]$p.value)
    res_I[i,(j+5)]=res_san9[[6]][[j]][[3]][[i]]$coefficients[1]
    res_pen[i,(j+5)]=res_san9[[6]][[j]][[3]][[i]]$coefficients[2]
    
  }
}  

names(res_sd)=periods
names(res_sen)=periods
names(res_z)=periods
names(res_pvalue)=periods
names(res_I)=periods
names(res_pen)=periods

XLConnect::writeWorksheetToFile("results_trend_SAN9.xls",data=res_sd,"SD_SAN9")
XLConnect::writeWorksheetToFile("results_trend_SAN9.xls",data=res_sen,"SEN_SAN9")
XLConnect::writeWorksheetToFile("results_trend_SAN9.xls",data=res_z,"Z_SAN9")
XLConnect::writeWorksheetToFile("results_trend_SAN9.xls",data=res_pvalue,"PVALUE_SAN9")
XLConnect::writeWorksheetToFile("results_trend_SAN9.xls",data=res_I,"INTERCEPT_SAN9")
XLConnect::writeWorksheetToFile("results_trend_SAN9.xls",data=res_pen,"Pendenza_SAN9")

#######################################################################################################

res_sd=data.frame(t(1:length(periods)))
res_sd[1:9,]=NA

res_z=data.frame(t(1:length(periods)))
res_z[1:9,]=NA

res_pvalue=data.frame(t(1:length(periods)))
res_pvalue[1:9,]=NA

res_sen=data.frame(t(1:length(periods)))
res_sen[1:9,]=NA

res_I=data.frame(t(1:length(periods)))
res_I[1:9,]=NA

res_pen=data.frame(t(1:length(periods)))
res_pen[1:9,]=NA


######################################################

for ( j in 1:5) {
  for ( i in 1:9)
  {
    
    res_sd[i,j]=res_pct9[[j]][[5]][[i]]
    res_sen[i,j]=as.numeric(res_pct9[[j]][[4]][[i]]$estimates)
    res_z[i,j]=as.numeric(res_pct9[[j]][[4]][[i]]$statistic)
    res_pvalue[i,j]=as.numeric(res_pct9[[j]][[4]][[i]]$p.value)
    res_I[i,j]=res_pct9[[j]][[3]][[i]]$coefficients[1]
    res_pen[i,j]=res_pct9[[j]][[3]][[i]]$coefficients[2]
    
    
  }
}

for ( i in 1:9)
{
  
  for ( j in 1:12)
  {
    res_sd[i,(j+5)]=res_pct9[[6]][[j]][[5]][[i]]
    res_sen[i,(j+5)]=as.numeric(res_pct9[[6]][[j]][[4]][[i]]$estimates)
    res_z[i,(j+5)]=as.numeric(res_pct9[[6]][[j]][[4]][[i]]$statistic)
    res_pvalue[i,(j+5)]=as.numeric(res_pct9[[6]][[j]][[4]][[i]]$p.value)
    res_I[i,(j+5)]=res_pct9[[6]][[j]][[3]][[i]]$coefficients[1]
    res_pen[i,(j+5)]=res_pct9[[6]][[j]][[3]][[i]]$coefficients[2]
    
  }
}  

names(res_sd)=periods
names(res_sen)=periods
names(res_z)=periods
names(res_pvalue)=periods
names(res_I)=periods
names(res_pen)=periods

XLConnect::writeWorksheetToFile("results_trend_PCT9.xls",data=res_sd,"SD_PCT9")
XLConnect::writeWorksheetToFile("results_trend_PCT9.xls",data=res_sen,"SEN_PCT9")
XLConnect::writeWorksheetToFile("results_trend_PCT9.xls",data=res_z,"Z_PCT9")
XLConnect::writeWorksheetToFile("results_trend_PCT9.xls",data=res_pvalue,"PVALUE_PCT9")
XLConnect::writeWorksheetToFile("results_trend_PCT9.xls",data=res_I,"INTERCEPT_PCT9")
XLConnect::writeWorksheetToFile("results_trend_PCT9.xls",data=res_pen,"Pendenza_PCT9")





