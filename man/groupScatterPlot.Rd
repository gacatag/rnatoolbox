\name{groupScatterPlot}
\alias{groupScatterPlot}
\title{
	Extract gender/sex information from bam file. 
}
\description{
Extract gender/sex information from an RNAseq Binary Alignment Map (i.e. BAM) 
file.
}
\usage{
  groupScatterPlot(l,col=1,lwd, lty,  FUN=mean, ...)
}
\arguments{
\item{l}{ 
List of numeric vectors. Name of the list elements will be sued as the name of
the groups.
}
\item{col}{ 
A vector that includes the colour assigned to the groups. If the length is one
all groups are plotted with the same colour (The default colour is 1, i.e. 
"black"). 
}
\item{lwd}{ 
The width of the lines (1 by default).
}
\item{lty}{ 
The type of the lines (1 by default).
}
\item{FUN}{ 
The function to ise for the summarization. By default the mean is measured for
each group. The summary value for eah group is shown with a line. 
}
\item{...}{
  Further parameters of the
  \code{\link[graphics]{plot}} fucntion.
}
}

\author{Ali Oghabian}

\examples{
  datList<- list(
    l1=rnorm(n=30, mean = 10, sd = 3),
    l2=rnorm(n=20, mean = 0, sd = 1),
    l3=rnorm(n=25, mean = 10, sd = 1)
  )

  groupScatterPlot(l=datList, col=rainbow(3), lty=1, lwd=1.5,  
    ylab="Test values")

}

