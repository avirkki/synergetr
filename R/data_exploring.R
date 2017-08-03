#
# Functions for Exploring and Browsing Data
#

# Author(s)  : Arho Virkki
# Copyright  : TUH Centre for Clinical Informatics
# Date       : 2017-07-12


#' Get sample from a table
#'
#' Draw a random sample from a table. If size == Inf, return all rows.
#'
#' @param tbl Table name to search
#' @param size Sample size
#'
#' @export
get_sample <- function( tbl, size=1000 ) {

  if( is.null(con) ) { # In-memory data.frame
    
    if(is.character(tbl)) { tbl <- get(tbl) }
    res <- tbl[sample(size),]

  } else { # SQL connection 
    
    sql <- paste("SELECT * FROM", tbl)
    if( size != Inf ) {
      sql <- paste(sql, "TABLESAMPLE SYSTEM_ROWS(", size, ")")
    }
    res <- dbGetQuery(options("synergetr_con")[[1]], sql)
  }

  return( res )
}

#' Get table names
#'
#' Get table column names as a character vector 
#'
#' @param tbl Table name to search
#'
#' @export
get_colnames <- function( tbl ) {

  con <- options("synergetr_con")[[1]]
  if( is.null(con) ) { # In-memory data.frame

    if(is.character(tbl)) { tbl <- get(tbl) }
    return( colnames(tbl) )

  } else { # SQL connection 
  
    sql <- paste("SELECT * FROM", tbl, "LIMIT 1")
    X <- dbGetQuery(con, sql)
    return( colnames(X) )
  }
}

#' Fraction of missing values
#'
#' Compute the fraction of missing values in a colum
#'
#' @param tbl Table name to search
#' @param cols Column name
#' @param sample_max Limit the number of rows when estimating
#'        the fraction of missing values
#'
#' @export
get_missing <- function(tbl, cols, 
                        sample_max=options("synergetr_sample_max")[[1]] ) {

  if(unlist(options("synergetr_verbose"))) {
    cat("Counting missing values from", cols, "...\n")
  }

  con <- options("synergetr_con")[[1]]
  if( is.null(con) ) { # In-memory data.frame

    if(is.character(tbl)) { tbl <- get(tbl) }
    res <- list( nmissing=sum(is.na(tbl[[cols]])), count=nrow(tbl) )

  } else { # SQL connection 

    sql <- paste("SELECT sum(",cols," IS NULL :: INTEGER),count(1)",
                 "FROM", tbl)

    if( sample_max != Inf ) {
      sql <- paste( sql, "TABLESAMPLE SYSTEM_ROWS(", sample_max, ")" )
    }
    res <- dbGetQuery(con, sql)
  } 

  return( res[[1]]/res[[2]] )
}
