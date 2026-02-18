

# File paths --------------------------------------------------------------
WD <- function(...) file.path(dirname(getwd()), ...)
INPUT <- function(...) WD("Input", ...)
SCRIPTS <- function(...) WD("Scripts", ...)
OUTPUT <- function(...) WD("Output", ...)
RENV <- function(...) WD("renv", ...)

### MAKE SURE RENV IS DOWNLOADED, IF NOT INSTALL THE PACKAGE ###
# install.packages("renv)
library(renv)

# Package management ------------------------------------------------------
manage_packages <- function(snapshotDate = "2024-10-08", Rversion = "4.4.1", pkgType = c("binary", "source")) {
  
  stopifnot(Rversion == getRversion())
  pkgType <- match.arg(pkgType)
  options(pkgType = pkgType)
  
  ### MAKE SURE YOU ACTIVATE RENV FIRST TIME YOU RUN THROUGH ###
  #UNCOMMENT OUT AFTER FIRST TIME RUNNING# 
  renv::activate(WD())
  
  if (file.exists(WD("renv.lock"))) {
    
    # Retrieve status of the project environment
    status <- renv::status()
    
    # Check if there are any discrepancies
    if (!is.null(status$changed) && status$changed) {
      message("Restoring the environment...")
      
      tryCatch({
        renv::restore(lockfile = RENV("renv.lock"))
    }, error = function(e) {
      message("Failed to restore environment: ", e$message)
    })
    
      } else {
      message("Environment is already up to date.")
    }
    
  } else {
    message("No renv.lock file found. Please initialize renv.")
  }
  
  library(devtools)
  library(data.table)
  library(openxlsx)
  library(stringr)
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(tigris)
  library(shiny)
  library(leaflet)
  library(RColorBrewer)
  library(zoo)
  library(lubridate)
  
  # Enable caching
  options(tigris_use_cache = TRUE)
  
  ## Print out session information (e.g., R version, imported packages)
  sessionInfo()
}

  

