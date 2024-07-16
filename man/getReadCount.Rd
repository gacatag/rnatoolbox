\name{getReadCount}
\alias{getReadCount}
\title{
	Get and plot the number of mapped fragments from bam files. 
}
\description{
Get and plot the number of mapped fragments from RNAseq Binary Alignment Map 
(i.e. BAM) files. Similar to a \code{\link[graphics]{boxplot}}, median (i.e. 
thick horizontal gray line), interquartile range (i.e. solid gray horizontal 
lines), the 1.5 time the interquartile range (i.e. dahsed gray horizontal 
lines) and the outliers (i.e. see \code{outlierPch} and \code{outlierCol} 
parameters) are shown.
}
\usage{
  getReadCount(bamFiles, sampleNames=c(), 
                     chr=paste("chr",c(1:22, "X", "Y"), sep=""), 
                     plotResults=TRUE, plotFile="", fileFormat="pdf", 
                     mar=c(5.1, 4.1, 4.1, 2.1),
                     lwdhLine=1.5, vLineBool=TRUE, 
                     colvLine="lightgrey", lwdvLine= 1.5,
                     ltyvLine=2, col="black", pch=1, 
                     outlierPch=c(2,6), outlierCol="red", cex=1, 
                     xlab="", ylab="Fragment count", main="", ...)
}
\arguments{
\item{bamFiles}{ 
A character vector that includes the paht to the BAM files. The name of the
vector elements can be defined to be used in the plot.
}
\item{sampleNames}{
  Characater vector with the same size as 'bamFiles' that includes the name of 
  the samples. Although it is an optional parameter, if 'plotResults' is set to
  TRUE the sample names must be defined either using this parameter or as the 
  names of the elements in vector 'bamFiles'.
}
\item{chr}{
  A character vector containing name of the chromosomes. The number of reads 
  mapping to these chromosomes are considered only.
}
\item{plotResults}{
  Boolean variable (with either TRUE or FALSE values), indciating whether the 
  results should be plotted.
}
\item{plotFile}{
  Path and file name to use for writing the plot into a png/pdf file.
}
\item{fileFormat}{
  The output file format for the plot. Options "pdf" and "png" are currently 
  available.
}
\item{mar}{
  Marjins to use for the plot. See mar in \code{\link[graphics]{par}} for more 
  info.
}
\item{lwdhLine}{
  The width of the horizontal line (1 by deault).
}
\item{vLineBool}{
  Whether draw the vertical lines that are helpful for tracing the result for
  each sample (TRUE by default).
}
\item{colvLine}{
  The colour of the vecrtical lines ("lightgrey" by default).
}
\item{lwdvLine}{
  The width of the vecrtical lines (1 by default).
}
\item{ltyvLine}{
  The type of the vecrtical lines (1 by default).
}
\item{col}{
  The colorur of the dots ("black" by default).
}
\item{pch}{
  The symbol of the dots (1 black by default).
}
\item{outlierPch}{
  The symbol of the points to be used for outliers (different for high and low
  ). Byt default 2 is used for high and 6 for low outliers.
}
\item{outlierCol}{
  The colours of the points to be used for outliers. If the siz of the vector 
  is 2 it is considered for high and low outliers. By default "red" is used for
  high and low outliers.
}
\item{cex}{
  The size of the points (1 black by default).
}
\item{xlab}{
  X axis label.
}
\item{ylab}{
  Y axis label.
}
\item{main}{
  The main title. It is omitted by deault.
}
\item{...}{
  Further parameters of \code{\link[grDevices]{pdf}} and 
  \code{\link[grDevices]{png}} fucntions.
}
}
\value{
A numeric vector that includes the number of the reads mapping to the defined
chromosomes.
}

\seealso{
	\code{\link{getGender}}
}

\author{Ali Oghabian}

\examples{

  \dontrun{
    getReadCount(bamFiles=c("sample1.bam", "sample2.bam", "sample3.bam"), 
      sampleNames=c("sample1", "sample2", "sample3"),
      file="./getMappedReadsCount_Output.pdf", 
          height=9, width=9)
  }

}

