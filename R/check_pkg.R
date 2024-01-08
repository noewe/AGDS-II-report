# Define a function check_pkg that tests whether a package is already installed and hence
# only needs loading, or still needs to be installed.
# v: a char vector containing package names
check_pkg <- function(v)
{
  for(x in v){
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
}
