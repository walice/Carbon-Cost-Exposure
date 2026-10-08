# 00_setup.R
# Shared preamble: packages, helper function, graphical parameters.
# Sourced by every numbered script. Paths are resolved with the `here` package
# relative to the replication folder (marked by the empty `.here` file).

suppressPackageStartupMessages({
  library(caret)        # varImp() for random forests
  library(dendextend)
  library(ggbiplot)     # PCA biplots (GitHub: vqv/ggbiplot, commit 7325e88)
  library(kableExtra)   # codebook table for the SI
  library(lmtest)       # coeftest() for clustered standard errors
  library(maptree)      # draw.tree()
  library(margins)      # average marginal effects for the logit comparison
  library(naniar)       # missingness summaries
  library(randomForest)
  library(regclass)
  library(relaimpo)     # Shapley (LMG) variance decomposition
  library(reshape2)
  library(rpart)
  library(rpart.plot)
  library(sandwich)     # vcovCL() for clustered standard errors
  library(showtext)
  library(stargazer)    # regression tables
  library(sysfonts)
  library(tidyverse)
  library(tree)         # classification trees
  library(here)         # attach last to avoid conflicts
})

here::i_am("R/00_setup.R")
set.seed(1509)

# Send plots that the working script drew to the screen to a null device
if (!interactive()) pdf(NULL)

# Helper: coerce a 0/1 factor to numeric 0/1
bin_to_num <- function(x) {
  return(as.numeric(as.character(x)))
}

# Graphical parameters. The Google fonts need an internet connection; if they
# cannot be downloaded the figures fall back to the default sans-serif font.
theme_set(theme_minimal())
fonts_ok <- tryCatch({
  font_add_google("Montserrat", "montserrat")
  font_add_google("Lato", "lato")
  TRUE
}, error = function(e) {
  message("Google fonts unavailable (", conditionMessage(e), "); using default font.")
  FALSE
})
plot_family <- if (fonts_ok) "montserrat" else "sans"
if (fonts_ok) {
  showtext_auto()
  showtext_opts(dpi = 96)
}
theme_update(text = element_text(size = 20, family = plot_family),
             legend.spacing.x = unit("0.2", "cm"),
             title = element_text(size = 30))
my_theme <- theme(text = element_text(size = 20, family = plot_family),
                  legend.spacing.x = unit("0.2", "cm"),
                  legend.spacing.y = unit("0.2", "cm"),
                  title = element_text(size = 30))
