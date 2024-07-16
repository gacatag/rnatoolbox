getReadCount<- function(bamFiles, sampleNames=c(), 
                     chr=paste("chr",c(1:22, "X", "Y"), sep=""), 
                     plotResults=TRUE, plotFile="", fileFormat="pdf", 
                     mar=c(5.1, 4.1, 4.1, 2.1),
                     lwdhLine=1.5, vLineBool=TRUE, 
                     colvLine="lightgrey", lwdvLine= 1.5,
                     ltyvLine=2, col="black", pch=1, 
                     outlierPch=c(2,6), outlierCol="red", cex=1, 
                     xlab="", ylab="Fragment count",
                     main="", ...){
  
  if(length(names(bamFiles))==0&length(sampleNames)!=0 & plotResults){
    names(bamFiles)<- sampleNames
  } else if (length(names(bamFiles))==0&length(sampleNames)==0 & plotResults){
    stop("The sample names must be defined either as the name of elements
         in 'bamFiles' vector or in 'sampleNames' parameter.")
  }
  
  if(length(outlierPch)==1)
    outlierPch<- rep(outlierPch,2)
  
  if(length(outlierCol)==1)
    outlierCol<- rep(outlierCol,2)


  countAll<- unlist(lapply(as.character(bamFiles), 
                         function(x) {return(derfinder::getTotalMapped(x, chr))}))
  
  if(plotResults&fileFormat=="pdf"& plotFile!="")
    pdf(plotFile, ...)
  if(plotResults&fileFormat=="png"& plotFile!="")
    png(plotFile, ...)
  par(mar=mar)
 
  plot(1:length(countAll), countAll, pch=1.5, xaxt='n', xlab=xlab, ylab=ylab,
       main=main, col=NA)
  boxVal<- boxplot.stats(countAll)$stats
  colhLine= "grey"
  abline(h=boxVal[3], col=colhLine, lwd=4)
  abline(h=boxVal[2], col=colhLine, lwd=1)
  abline(h=boxVal[4], col=colhLine, lwd=1)
  abline(h=boxVal[1], col=colhLine, lwd=1, lty=2)
  abline(h=boxVal[5], col=colhLine, lwd=1, lty=2)
  if(length(pch)==1)
    pch<- rep(pch, length(countAll))
  if(length(outlierPch)>0){
    pch[which(countAll<boxVal[1])]<-outlierPch[2]
    pch[which(countAll>boxVal[5])]<-outlierPch[1]
  }
  if(length(col)==1)
    cols<- rep(col,length(countAll))
  cols[which(countAll<boxVal[1])]<-outlierCol[2]
  cols[which(countAll>boxVal[5])]<-outlierCol[1]
  axis(1, at=1:length(countAll), labels = names(bamFiles), las=2)
  
  if(vLineBool){
    for(j in 1:length(countAll))
      abline(v=j, col=colvLine, lty=ltyvLine)
  }
    points(1:length(countAll), countAll, pch=pch, col=cols)

  


  
  if(plotResults&(fileFormat=="pdf" | fileFormat=="png")& plotFile!="")
    dev.off()
  
  return(countAll)
}
