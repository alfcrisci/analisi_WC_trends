month2season=function(x,mseq="DJF",label=F){
  result=x
  res=list()
  
  for ( i in 1:12) {
    res[[i]]=which(x==i)
  }
  
  labels=c(1,2,3,4)

  if(label==T) {labels=c("DJF","MAM","JJA","SON")}
  if((label==T) & (mseq=="JFM")) {labels=c("JFM","AMJ","JAS","OND")}
  
  if ( mseq=="DJF") {
    result[c(res[[12]],res[[1]],res[[2]])]=labels[1]
    result[c(res[[3]],res[[4]],res[[5]])]=labels[2]
    result[c(res[[6]],res[[7]],res[[8]])]=labels[3]
    result[c(res[[9]],res[[10]],res[[11]])]=labels[4]
  } 
  
  if ( mseq=="JFM") {
    result[c(res[[1]],res[[2]],res[[12]])]=labels[1]
    result[c(res[[3]],res[[4]],res[[5]])]=labels[2]
    result[c(res[[6]],res[[7]],res[[8]])]=labels[3]
    result[c(res[[9]],res[[10]],res[[11]])]=labels[4]
    
  }   
  
  return(result)
  
}

month_days=c(31,28,31,30,31,30,31,31,30,31,30,31)

