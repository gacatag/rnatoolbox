classifySex<- function(bamFiles, sampleNames=c(), numChr="chrY", 
                     denumChr="chrX",
                     plotResults=TRUE, plotFile="", fileFormat="pdf", 
                     mar=c(5.1, 4.1, 4.1, 2.1), hLine=c(), colhLine="red", 
                     lwdhLine=1.5, vLineBool=TRUE, 
                     colvLine="lightgrey", lwdvLine= 1.5,
                     ltyvLine=2, col="black", pch=1, cex=1, 
                     xlab="", ylab="Fragment count", main="", ...){
  
  if(length(names(bamFiles))==0&length(sampleNames)!=0 & plotResults){
    names(bamFiles)<- sampleNames
  } else if (length(names(bamFiles))==0&length(sampleNames)==0 & plotResults){
    stop("The sample names must be defined either as the name of elements
         in 'bamFiles' vector or in 'sampleNames' parameter.")
  }
  

  countY<- unlist(lapply(as.character(bamFiles), 
                         function(x) {return(derfinder::getTotalMapped(x, numChr))}))
  countAll<- unlist(lapply(as.character(bamFiles), 
                         function(x) {return(derfinder::getTotalMapped(x, denumChr))}))
  frac=countY/countAll
  
  if(plotResults&fileFormat=="pdf"& plotFile!="")
    pdf(plotFile, ...)
  if(plotResults&fileFormat=="png"& plotFile!="")
    png(plotFile, ...)
  par(mar=mar)

    plot(1:length(frac), frac, pch=pch, xaxt='n', xlab="", main=main, col=NA)
  axis(1, at=1:length(frac), labels = names(bamFiles), las=2)
  
  if(vLineBool){
    for(j in 1:length(frac))
      abline(v=j, col=colvLine, lty=ltyvLine)
  }
    points(1:length(frac), frac, col=col, pch=pch, cex=cex)
 
  
  if(length(hLine)>0)
    abline(h=hLine, col=colhLine, lwd=lwdhLine)

  
  if(plotResults&(fileFormat=="pdf" | fileFormat=="png")& plotFile!="")
    dev.off()
  
  return(frac)
}
