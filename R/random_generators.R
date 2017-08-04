#
# Functions For Generating Synthetic Data
#

# Author(s)  : Arho Virkki
# Copyright  : TUH Centre for Clinical Informatics
# Date       : 2017-05-29 


#' Generate random code field
#'
#' Build random code fields using \code{\link{rchar}} as the
#' underlying workhorse. 
#'
#' @details For extra parameters such as code field length and type,
#' see \code{\link{rchar}}.
#' 
#' @param nlevels Number of levels for the code keys
#' @param cols Column name for the generated data
#' @param size Number of rows to be generated
#' @param codevec Explicit vector of codes to use
#' @param along A vector (or one-column data.frame) to match the code
#'        keys with.
#' @examples
#'
#' # Basic use
#' rcode(2, "codes", 10)
#' rcode(NA, "codes", 10, codevec=c("foo", "bar","xyzzy"))
#'
#' # Using the along feature
#' docname <- sample(c("Jofn A Doc", "Jane D Doc"), 10, replace=TRUE)
#' rcode(along=docname, cols="doccode", type="A0", slength=3)
#'
#' @export
rcode <- function( nlevels=10, cols, size, codevec=NULL, along=NULL, ... ) {

  if( !is.null(along) ) {
    if( is.data.frame(along) ) {
      z <- along[[1]] 
    } else {
      z <- along 
    }
    codeidx <- as.numeric(as.factor(z))
    nlevels <- max(codeidx)
  }

  if( is.null(codevec) ) {
    # Generate a code vector
    codevec <- rchar(cols=cols, size=nlevels, ... )[[cols]]
  }

  if( is.null(along) ) {
    # Ramdomize codes
    df <- data.frame(sample(codevec, size=size, replace=TRUE))

  } else {
    # Use the existing ordering of 'along'
    df <- data.frame(codevec[codeidx])
  }

  names(df) <- cols
  return( df )
}


#' Generate random primary integer keys
#'
#' A convenience function to generate sequences of numbers (similar to
#' primary keys in database tables).
#'
#' @param cols Column name for the generated data
#' @param size Number of rows to be generated
#' @param min_value Integer value to start the sequence
#' @examples
#' rpk("id", 10)
#'
#' @export
rpk <- function( cols, size, min_value=1 ) {

  pk <- data.frame(sample(min_value:(size+min_value-1)))
  colnames(pk) <- cols
  return( pk )
}


#' Generate random character fields
#'
#' Generate artificial code fields from random characters and
#' integers.
#'
#' @param slength String length
#' @param cols Column name for the generated data
#' @param size Number of rows to be generated
#' @param type Fied type specification. "0" generates numbers, "A" 
#'        capital letters and "a" small letters. The values "0Aa"
#'        can be combined and given in any order. The value "m" 
#'        produces a special hour-minute timestamp of the
#'        form [00-59][00-59].
#' @examples
#' rchar(4, "code", 20, type="A0")
#'
#' @export
rchar <- function( slength=8, cols, size, type="0Aa" ) {

  if(unlist(options("synergetr_verbose"))) cat("Generating", cols, "...\n")

  ch <- ""
  typevec <- unlist(strsplit(type,""))

  if( "0" %in% typevec ) {
    ch <- paste0(ch, "0123456789")
  }
  if( "A" %in% typevec ) {
    ch <- paste0(ch, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  } 
  if( "a" %in% typevec ) {
    ch <- paste0(ch, "abcdefghijklmnopqrstuvwxyz")
  } 
  
  charSet <- unlist(strsplit(ch,""))
 
  if( "m" %in% typevec ) {
    # Special case: generate HHMM-type of hour-minute timestamps

    slength <- 2 
    charSet <- c("00", "01", "02", "03", "04", "05", "06", "07", "08",
                 "09", "10", "11", "12", "13", "14", "15", "16", "17",
                 "18", "19", "20", "21", "22", "23", "24", "25", "26",
                 "27", "28", "29", "30", "31", "32", "33", "34", "35",
                 "36", "37", "38", "39", "40", "41", "42", "43", "44",
                 "45", "46", "47", "48", "49", "50", "51", "52", "53",
                 "54", "55", "56", "57", "58", "59")
  }

  res <- character(size)
  for( k in 1:size ) {
    res[k] <- paste(sample(charSet, slength, replace=TRUE), collapse="")
  }
  df <- data.frame( res )
  colnames(df) <- cols 
  return( df )
}


#' Generate Imaginary Personal IDs
#'
#' The generated codes resemble Finnish national personal id
#' convention in the field length. that the year of birth can be deduced from
#' the code in a similar way, and the field width is the same.
#' Otherwise, there are no similarities or well-formedness rules.
#'
#' @param yearString String to contruct the year part (in Finnish
#'        convention). See the examples.
#' @param size Number of elements to generate
#' @examples
#' rpid(NA, 10)
#' rpid(yearString=c("78-","03A","98-"))
#'
#' @export
rpid <- function( yearString=NA, size=length(yearString) ) {
  
  # Generate uniform year distribution
  if( is.na(yearString[1]) ) {

    y <- ceiling(runif(size, 1940, as.POSIXlt(Sys.time())$year + 1900))
    sepchar <- rep("-", size)
    sepchar[y >= 2000] <- "A"
    yearString <- paste0(substring(as.character(y),3,4), sepchar)
  }

  dayString <- sprintf("%0.2i",ceiling(runif(size, 0, 30)))
  monthString <- sprintf("%0.2i",ceiling(runif(size, 0, 12)))

  ch <- "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  suffixChars <- unlist(strsplit(ch,""))

  suffix <- character(size)
  for( k in 1:size ) {
    suffix[k] <- paste(sample(suffixChars,4), collapse="")
  }

  return(paste0(dayString,monthString,yearString,suffix))
}

#' Generate random numbers
#'
#' A Wrapper to R random number generators that follows the same
#' semantics as the other data generating functions.
#'
#' @details The function can be used to call R random number
#' generators like \code{\link[stats]{runif}},
#' \code{\link[stats]{rnorm}}, \code{\link[stats]{rpois}}, etc. that
#' accept 'n' as the number of values to be generated. The unnamed
#' arguments are passed to these functions. Examples illustrate best
#' the usage of this function.
#' 
#' @param fun Function to be called
#' @param cols Column name
#' @param size Number of rows to be generated
#' @param integers Coerce the results to integers (using
#'        \code{\link[base]{floor}})
#' @param lower Lower limit
#' @param upper Upper limit
#' @examples
#' rnumeric(runif, "dot_count", 10, integers=TRUE, min=5, max=15)
#' rnumeric(rnorm, "root_lenght", 10, mean=30, sd=5)
#' rnumeric(rpois, "visit_time_min", 10, lower=10, lambda=20)
#'
#' @export
rnumeric <- function(fun=runif, cols, size, integers=FALSE, 
                     lower=NA, upper=NA, ... ) {
 
  if( integers ) {
    x <- floor(fun( n=size, ... )) 
  } else {
    x <- fun( n=size, ... )
  }
  if( !is.na(lower) ) {
    x <- pmax( x, lower )
  }
  if( !is.na(upper) ) {
    x <- pmin( x, upper )
  }

  res <- data.frame( x )
  colnames(res) <- cols

  return( res )
}


#' Generate constant column
#' 
#' Repeat a constant value for a column
#' 
#' @param value Constant value
#' @param cols Column name for the generated data
#' @param size Number of rows to be generated
#'
#' @export
ccol <- function( value, cols, size ) {
  df <- data.frame(rep(value, size))
  names(df) <- cols
  return( df )
}
