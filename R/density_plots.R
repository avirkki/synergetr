
#' Plot Value Distribution
#'
#' A combined categorical distribution computation and plot. 
#'
#' @details The function calls getCounts for frequency computations
#' which, in turn, may call database engine for data. 
#' 
#' @param tbl Table name to inspect
#' @param cols Vector of table fields
#' @param hlp Per cent of points to hightlight in graph
#' @param ntop Number of rows to return as a sample
#'
#' @export
plotCatDist <- function( tbl, cols, hlp=0.5, ntop=10, ... ) {

  par("xpd"=NA) 
  par("mar"=c(5.1,4.1,5.1,2.1))
  
  cnt <- getCounts( tbl, cols, ... ) 
  n <- nrow(cnt)
  datasize <- sum(cnt$N)

  npoints <- round(n*hlp/100)
  x <- (1:n)/n*100

  plot(x, cnt$cdf, ylab="Cumulative probability", type="n",
       xlab=sprintf("Distinct values %% (n=%i, first %.1f%% highlighted)",
                    n, hlp), bty="L", ylim=c(0,1), xlim=c(0,100))

  title(sprintf("Distribution of Categorical Values (N=%i)",
               datasize), adj=0 )
  mtext(paste0(tbl,".(",paste0(cols, collapse=", "),")"), 
        adj=0, line=1 )

  points(x[1:npoints], cnt$cdf[1:npoints], pch="_", col="gray30")
  lines(x, cnt$cdf)

  x80 <- sum(cnt$cdf <= 0.8)/n*100
  lines(c(x80,x80), c(0,1), lwd=2, col="blue", lty="dashed")
  text(x=x80, y=0.1, pos=4, 
       labels=paste0("80.0% of data\n",
                     sprintf("%0.1f %% of all distinct values", x80)))

  xSingle <- which(cnt$n == 1)[1]/n*100
  lines(c(xSingle,xSingle), c(0,1), lwd=2, col="red", lty="dotted")
  text(x=xSingle, y=0.3, pos=4, 
       labels=sprintf("%0.1f%%\nunique\nvalues", 100-xSingle))

  return( cnt[1:min(ntop,nrow(cnt)),] )
}
