# Carbon-Cost-Exposure
# Alice Lepissier
# alice_lepissier@brown.edu


## ## ## ## ## ## ## ## ## ## ##
# REQUIREMENTS              ####
## ## ## ## ## ## ## ## ## ## ##

# Install regular packages
install.packages(c("corrplot",
                   "dendextend",
                   "EnvStats",
                   "foreign",
                   "gridExtra",
                   "mapproj",
                   "naniar",
                   "regclass",
                   "showtext",
                   "sysfonts",
                   "wesanderson"), 
                 repos = "http://cran.us.r-project.org",
                 type = "source")

# Install arrow package
# from https://cran.r-project.org/web/packages/arrow/vignettes/install.html
options(
  HTTPUserAgent =
    sprintf(
      "R/%s R (%s)",
      getRversion(),
      paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
    )
)

install.packages("arrow", 
                 repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")

# Install from GitHub remotes
devtools::install_github("vqv/ggbiplot@7325e880485bea4c07465a0304c470608fffb5d9")
