library(xts)
library(igraph)
library(lubridate)
library(dplyr)


setwd("/home/alf/Documenti/aa_recent_work/lav_messeri")

source("WT_aux.r")

#################################################################################à

pct_cla_9=read.csv("pct09.cla",sep="",header=F)
names(pct_cla_9)=c("year","month","date","time","WTs")
rownames(pct_cla_9)=ISOdate(pct_cla_9$year,pct_cla_9$month,pct_cla_9$date)
pct_cla_9$season=month2season(pct_cla_9$month)
pct_cla_9_xts=as.xts(pct_cla_9)

saveRDS(pct_cla_9_xts,"pct_cla_9_xts.rds")

Freq_month_pct9=table(pct_cla_9$month,pct_cla_9$WTs)


lents=length(pct_cla_9$WTs)
matrix_WTs=cbind(pct_cla_9$season[1:lents-1],pct_cla_9$WTs[1:lents-1],pct_cla_9$WTs[2:lents])
adj_tot_pct9=as.data.frame.array(table(pct_cla_9$WTs[1:lents-1],pct_cla_9$WTs[2:lents]))
node_pct9=as.data.frame.array(table(pct_cla_9$WTs))
names(node_pct9)="freq_pct9"
g1 <- graph_from_adjacency_matrix( as.matrix(adj_tot_pct9), weighted=TRUE) 
V(g1)$size=as.numeric(node_pct9[,1])
library(igraph)

