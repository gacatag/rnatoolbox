\name{getGender}
\alias{getGender}
\title{
	Extract gender/sex information from bam file. 
}
\description{
Extract gender/sex information from an RNAseq Binary Alignment Map (i.e. BAM) 
file.
}
\usage{
  getGender(bamFiles, sampleNames=c(), numChr="chrY", 
                     denumChr="chrX",
                     plotResults=TRUE, plotFile="", fileFormat="pdf", 
                     mar=c(5.1, 4.1, 4.1, 2.1), hLine=c(), colhLine="red", 
                     lwdhLine=1.5, main="", vLineBool=TRUE, 
                     colvLine="lightgrey", lwdvLine= 1.5,
                     ltyvLine=2, col="black", pch=1,  cex=1, ...)
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
\item{numChr}{
  A character vector containing name of the chromosomes. The number of reads 
  mapping to these chromosomes are considered as the numerator in the ratio.
}
\item{denumChr}{
  A character vector containing name of the chromosomes. The number of reads 
  mapping to these chromosomes are considered as the denuminator in the ratio.
}
\item{plotResults}{
  Boolean variable (with either TRUE or FALSE values), indciating whether the 
  results should be plotted. Note that if the "plotFile" parameter is not 
  defined ir is defined as "" the results will not be plotted eventhough this 
  parameter is set as TRUE.
}
\item{plotFile}{
  Path and file name to use for the plot.
}
\item{fileFormat}{
  The output format for the plot. Options "pdf" and "png" are currently 
  available.
}
\item{mar}{
  Marjins to use for the plot. See mar in \code{\link[graphics]{par}} for more 
  info.
}
\item{hLine}{
  Where to draw the horizontal line ? It is omitted by default.
}
\item{colhLine}{
  The colour of the horizontal line ("red" bydefault).
}
\item{lwdhLine}{
  The width of the horizontal line (1.5 by deault).
}
\item{main}{
  The main comment. It is omitted by deault.
}
\item{vLineBool}{
  Whether draw the vertical lines that are helpful for tracing the result for
  each sample (TRUE by default).
}
\item{colvLine}{
  The colour of the vecrtical lines ("lightgrey" by default).
}
\item{lwdvLine}{
  The width of the vecrtical lines (1.5 by default).
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
\item{cex}{
  The size of the dots (1 black by default).
}
\item{...}{
  Further parameters of \code{\link[grDevices]{pdf}} and 
  \code{\link[grDevices]{png}} fucntions.
}
}
\value{
A numeric vector that includes the ratio of the reads mapping to  \code{numChr} 
to the reads mapping to  \code{denumChr}.
}

\seealso{
	\code{\link{getReadCount}}
}

\author{Ali Oghabian}

\examples{

  \dontrun{
    getGender(bamFiles=c("sample1.bam", "sample2.bam", "sample3.bam"), 
      sampleNames=c("sample1", "sample2", "sample3"),
      hLine=0.05, file="./getGender_Output.pdf", 
          height=9, width=9)
  }

}

