
# Project launch file
# Source this file to run code

# Load global packages ----------------------------------------------------

# check librarian package management installed
if (!requireNamespace("librarian")) install.packages("librarian", quiet = TRUE)

# use suppress to prevent build warnings
# note can install from github
suppressWarnings(
  librarian::stock(
    DataS-DHSC/DHSClogger,
    DataS-DHSC/DHSCtools,
    # DataS-DHSC/DHSCcolours,
    yaml, tidyverse,
    tools, fs,
    curl, httr, polite, rvest,
    tidyxl, unpivotr, writexl, lubridate,
    ggrepel, scales, sf, svglite,
    quiet = TRUE
  )
)

# Setup logging -----------------------------------------------------------
logger <- DHSClogger::get_dhsc_logger()
# set threshold of console log to information and above
logger$set_threshold("log.console", "INFO")

# Call main code ----------------------------------------------------------
logger$info("[Begin]")

# add source of run script and entry point to code below
# source("./R/run_analysis.R", local = TRUE)

# run_grouped_analysis()
# run_plots()
# run_sex_filter()
# run_depravity_filter()
# run_HCage_filter()
# run_age_filter()
# run_employment_filter()
# run_limlast_filter()
# run_moderators()
# visualise_moderators()

# Working on ---------
#run_mediators()

# Tests -----------------
source("./R/run_tests.R", local = TRUE)
# run_vif()
# run_vif2()
run_hist()


# Regression -----------------------------------------
# add source of run script and entry point to code below this line
# source("./R/run_regression.R", local = TRUE)
# run_mnreg_GHQ()
# run_ordreg_GHQ()

# source("./R/run_OLS.R", local = TRUE)
# run_OLS()

logger$info("[End]")
