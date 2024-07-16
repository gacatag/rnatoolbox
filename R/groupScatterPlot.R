groupScatterPlot<- function(l,col=1,lwd, lty, FUN=mean, ...){
  x=unlist(lapply(1:length(l), function(x){return(rep(x,length(l[[x]])))}))
  
  a<- unlist(lapply(l, function(x){return(-.45 + .9*((1:length(x))*(1/(length(x)+1))))}))
  if(length(col)==1){
    col2=rep(col, length(unlist(l)))
  } else {
    col2=unlist(lapply(1:length(l), function(x){return(rep(col[x],length(l[[x]])))}))
  }
  plot(x+a, as.numeric(unlist(l)), xaxt='n', xlab="", col=col2, 
       xlim=c(0.5,length(l)+.5),...)
  sumVals<- sapply(l, FUN)
  for(i in 1:length(l))
    lines(x=c(i-.45,i+.45), y=c(sumVals[i],sumVals[i]), col=col[i],lwd=lwd, lty=lty)
  axis(1, at=1:length(l), labels=names(l))
}
