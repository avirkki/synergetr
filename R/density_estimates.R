#
# Functions For Generating Synthetic Data
#

# Author(s)  : Arho Virkki
# Copyright  : TUH Centre for Clinical Informatics
# Date       : 2017-08-01


#' Compute Frequencies for Categorical Variables
#'
#' Count, group and arrange categorical values from the most to the
#' least frequent. Optionally, compute cumulative distribution
#' function (cdf) and suppress entries that occur less than min_count
#' of times.
#' 
#' @details If option("synergetr_con") points to a database
#' connection, the computation of the frequencies will be done at the
#' database and the \code{tbl} should be a character string
#' (e.g. tbl == "schemaname.table_name"). If "synergetr_con" is
#' not set (i.e.  equals NULL), the computations will be done in R
#' memory using the data.table package, and \code{tbl} can be
#' either the actual data.frame or its variable name as a character
#' string.
#'
#' @param tbl Table name to inspect 
#' @param cols Vector of table fields 
#' @param compute_cdf Add cumulative distribution function to result
#'        set as a 'cdf' column.
#' @param min_count The minimum amount of times a distinct value must
#'        appear in raw data to be included in the frequency count
#'        (this parameter is used to exclude rare values from
#'        appearing at all).
#' @param sample_max The maximum number of rows to use
#'       
#' @import data.table
#' @export
getCounts <- function(tbl, cols, compute_cdf=TRUE, min_count = 1,
                      default="''", con=options("synergetr_con")[[1]], 
                      sample_max=options("synergetr_sample_max")[[1]] ) {

  if( is.null(con) ) {
    # Computations will be done in memory (with data.table)
    
    if( is.character(tbl) ) {
      # We got the name of the table
      dtb <- data.table(get(tbl))
    } else {
      # Suppose the that the argument is the table itself
      dtb <- as.data.table(tbl)
    }

    # Group by cols, order by n desc using data.table's syntax
    res <- dtb[, .N, by=cols][order( -N )]

    if( min_count > 1 ) {
      res <- res[N >= min_count]
    }
    # Coerce back to ordinary data frame
    res <- as.data.frame(res)

  } else {  
    # We are working with an SQL Connection
    
    if( is.na(min_count) ) {
      filterClause <- ""
    } else {
      filterClause <- 
        paste0("HAVING count(coalesce( ",cols[1],"::VARCHAR, '')) >=", 
               min_count)
    }

    if( sample_max == Inf ) {
      sampleClause <- ""
    } else {
      sampleClause <- paste("TABLESAMPLE SYSTEM_ROWS(", sample_max, ")")
    }
    sql <- paste0("SELECT ", 
                  "count(coalesce(",cols[1],"::VARCHAR, '')) as \"N\", ",
                  paste(cols, collapse=", "),
                  " FROM ", tbl, " ",
                  sampleClause,
                  " GROUP BY ", paste(cols, collapse=", "), " ",
                  filterClause, " ", 
                  " ORDER BY \"N\" DESC ")

    res <- dbGetQuery(con, sql)
  }

  # Cumulative probability density
  if( compute_cdf ) {
    res$cdf <- cumsum(res$N)/sum(res$N) 
  }

  return(res) 
}


#' Draw a random sample from a data frame 
#'
#' Generate a random sample from an empirical probability density.
#'
#' @details The 'cdf' and count_col columns will be discarded.
#' 
#' @param ctable Count table as returned by \code{\link{getCounts}} 
#' @param size The number of rows
#' @param count_col The column name for counts, usually "N".
#' @export
epdfSample <- function( ctable, size, count_col="N" ) { 

  idx <- sample(nrow(ctable), size=size, 
                replace=TRUE, prob=ctable[[count_col]])
  return(ctable[idx, drop=FALSE,
         setdiff(colnames(ctable), c(count_col, "cdf"))] ) 
}


#' Construct synthetic factorial data 
#'
#' @export
rfactor <- function(tbl, cols, size, min_count=5, partial=FALSE,
                    nsplits=1, split_sep=" ", ... ) {

  if(unlist(options("synergetr_verbose"))) cat("Generating", cols, "...\n")

  cnt <- getCounts( tbl, cols, min_count=min_count, ... ) 
  sdata <- epdfSample(cnt, size=size)

  if( partial ) {
    # Keep only a substring of the whole string
    res <- unlist( lapply(strsplit(sdata[[1]], split_sep),  
                          function(x) { 
                            paste(na.omit(x[1:nsplits]),
                                  collapse=split_sep)
                          }))

    sdata <- data.frame(res)
    names(sdata) <- "cols"
  }

  return( sdata )
}


#' Generate Random Finnish Personal IDs
#'
#' Generate random Pseudo-Finnish personal IDs (hetu-codes) using
#' \code{\link{rpid}} function. In addition to generating nonsense,
#' the functions estimates the real distribution of birth years and
#' uses that information as a 'yearString' argument to
#' \code{\link{rpid}}.
#'
#' @param tbl Table name
#' @param cols Column name 
#' @param size Number of rows for the output
#' @export
rhetu <- function( tbl, cols, size, ... ) {

  if(unlist(options("synergetr_verbose"))) cat("Generating", cols, "...\n")

  cRandom <- getCounts( tbl, cols, min_count=NA, ... )
  cRandom[[cols[1]]] <- 
    rpid( yearString=substring(cRandom$henkilotunnus,5,7) )

  return(epdfSample(cRandom, size))
}


#' Build random dates
#'
#' Based on the dates at the given table column, generate a
#' kernel density estimate and return a random column of dates (with
#' the same distribution) 
#'
#' @details The date distribution is estimated as a one-dimensional
#' kernel density with \code{\link[stats]{density}} with the standard
#' notation that day zero equals 1970-01-01 00:00:00 (see the output
#' of \code{unclass(as.Date("1970-01-01"))}), and the consecutive days
#' are numbered sequentially as 1,2,3...  
#'
#' @param tbl Table name
#' @param cols Column name 
#' @param size Number of rows for the output
#' @param sparse Consider and estimate also the fraction of missing data
#' @param trim Passed to \link{getDensity}
#' @param adj Passed to \link{getDensity}
#' @param n Passed to \link{getDensity}
#' @export
rdate <- function(tbl, cols, size, sparse=FALSE, trim=0.001, adj=0.1, n=1000 ) {

  if(unlist(options("synergetr_verbose"))) cat("Generating", cols, "...\n")

  d <- getDensity(tbl, cols, trim=trim, adj=adj, discrete=TRUE, 
                  n=n, is_date=TRUE)  
  dates <- as.Date(sample(d$x, size=size, replace=TRUE, prob=d$y), 
                   origin="1970-01-01")

  # Estimate how much of the values are missing and simulate that
  # behaviour
  if( sparse ) {
    m <- get_missing(tbl, cols)
    dates[runif(N) <= m] <- NA
  }

  df <- data.frame( dates )
  colnames(df) <- cols 
  return( df )
}


#' Build random timestamps
#'
#' Based on the timestamps at the given table column, generate a
#' kernel density estimate and return a random column of timestamps (with
#' the same distribution) 
#'
#' @param tbl Table name
#' @param cols Column name 
#' @param size Number of rows for the output
#' @param onlyDates Build a timestamp field that contains factually
#'        only dates (e.g. 2017-08-02 00:00:00)
#' @param sparse Consider and estimate also the fraction of missing data
#' @param trim Passed to \link{getDensity}
#' @param adj Passed to \link{getDensity}
#' @param n Passed to \link{getDensity}
#' @export
rtime <- function(tbl, cols, size, onlyDates=FALSE, sparse=FALSE, 
                  trim=0.001, adj=0.1, n=100000, ...) {

  if(unlist(options("synergetr_verbose"))) cat("Generating", cols, "...\n")

  d <- getDensity(tbl, cols, trim=trim, adj=adj, n=n, ...)
  times <- as.POSIXct(sample(d$x, size=size, replace=TRUE, prob=d$y), 
                      origin="1970-01-01 00:00:00")

  if( onlyDates ) {
    times <- as.Date(times)
  }
  if( sparse ) {
    m <- get_missing(tbl, cols)
    times[runif(N) <= m] <- NA
  }

  df <- data.frame( times )
  colnames(df) <- cols 
  return( df )
}


#' Compute probability density estimate from numerical fields
#'
#' One-dimensional numerical kernel density estimation based on
#' \code{\link[stats]{density}}.  This function abstacts away database
#' operations and provides sensible defauts for trimming out outlier
#' data, handling computational cost and handling special formats
#' (like Date fields).
#'
#' @param tbl Table name
#' @param cols Column name
#' @param con Connection object to the database, or NULL for in-memory
#'        data.frames
#' @param trim The quentile of data to be discarded from both ends
#'        (min and max) for robust estimation of the probability
#'        density
#' @param adj Kernel width parameter to be passed to 
#'        \code{\link[stats]{density}}
#' @param discrete TRUE for integer-valued output
#' @param n Default number of point for the kernel density estimation
#' @param sample_max Maximum number of data points to read from the
#'        source table. Can be used to limit the data transfer from a
#'        database (and the consecutive memory consumption and
#'        computational costs)
#' @param is_date Convert data first into Date before casting it into
#'        integers (where day 0 equals 1970-01-01)
#'
#' @export
getDensity <- function(tbl, cols, con=options("synergetr_con")[[1]], 
                       trim=0.001, adj=1, discrete=FALSE, n=500,
                       sample_max=options("synergetr_sample_max")[[1]], 
                       is_date=FALSE ) {
  
  if( is.null(con) ) {
    # Computations will be done in memory

    if(is.character(tbl)) { tbl <- get(tbl) }

    if( sample_max < nrow(tbl) ) {
      res <- tbl[sample(smax), cols, drop=FALSE]
    } else {
      res <- tbl[, cols, drop=FALSE]
    }

  } else {
    # We are using an SQL connection

    if( sample_max == Inf ) {
      sampleClause <- ""
    } else {
      sampleClause <- paste("TABLESAMPLE SYSTEM_ROWS(", sample_max, ")")
    }

    sql <- paste("SELECT", cols,
                 "FROM", tbl, sampleClause, 
                 "WHERE", cols, "IS NOT NULL")

    res <- dbGetQuery(con, sql)
  }


  if( is_date ) {
    x <- as.numeric(as.Date(res[[cols[1]]]))
  } else {
    x <- as.numeric(res[[cols[1]]])
  }

  xinf <- quantile(x,trim)
  xsup <- quantile(x,1-trim)

  if( discrete ) {
    n <- xsup - xinf +1
  } 

  d <- density(x, from=xinf, to=xsup, adj=adj, n = n)
  return( d )
}

#' Get random indices for missing values
#'
#' @export
rmissing <- function(tbl, cols, size, ...) {
  return(runif( size ) < get_missing( tbl, cols,... ))
}

