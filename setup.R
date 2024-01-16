# check for installed packages, install those that are missing
needed = list(
  CRAN = c("here", "yaml", "sf", "readr", "dplyr")
)
installed = rownames(installed.packages())
need_from_cran = needed$CRAN[!(needed$CRAN %in% installed)]
for (pkg in need_from_cran) install.packages(pkg, repos = "https://cloud.r-project.org/")

# load the needed packages
suppressPackageStartupMessages({
  for (pkg in needed$CRAN) library(pkg, character.only = TRUE)
})


# define the root directory
here::i_am("setup.R")

# source files found in "functions"
for (f in list.files(here::here("functions"), 
                     pattern = "^.*\\.R$", 
                     full.names = TRUE)) {
  source(f)
}

