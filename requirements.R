# Cost Exposure paper
# R installation requirements for Docker file
# Alice Lepissier
# alice.lepissier@gmail.com

## ## ## ## ## ## ## ## ## ## ##
# REQUIREMENTS              ####
## ## ## ## ## ## ## ## ## ## ##

Sys.setenv(LIBARROW_BINARY = TRUE)
install.packages("arrow", 
                 repos = "http://cran.us.r-project.org",
                 type = "source")
install.packages(c("foreign", 
                   "here", 
                   "readstata13", 
                   "reshape2", 
                   "tidyverse"))