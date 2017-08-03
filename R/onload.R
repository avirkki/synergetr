
# Initialize variables for the package

.onLoad <- function(libname,pkgname) {
  
  options("synergetr_verbose"=TRUE)
  options("synergetr_sample_max"=Inf)

  # For unknown reasons, time conversions fail on certain timestamps (e.g.
  # "1921-05-01 00:00:00") if the 'TZ' environment variable is unset. The
  # following will replace unset TZ with an empty string ""
  Sys.setenv("TZ"=Sys.getenv("TZ"))

}
