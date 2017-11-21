pct09=read.csv("data/pct09.cla",sep="",header=F)
san09=read.csv("data/san09_500HGT.cla",sep="",header=F)

WTS=data.frame(WT_pct09=as.factor(pct09$V5),
               WT_san09=as.factor(san09$V5[1:13545]))

rownames(WTS)=ISOdate(pct09$V1,pct09$V2,pct09$V3)

saveRDS(WTS,"data/WTS_df.rds")

WTS_xts=as.xts(WTS)

saveRDS(WTS_xts,"data/WTS_xts.rds")

times_WTS=index(WTS_xts)

saveRDS(times_WTS,"data/times_WTS.rds")

months_WTS=month(times_WTS)

saveRDS(months_WTS,"data/months_WTS.rds")

