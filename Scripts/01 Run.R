
# Load necessary packages and setup environment  --------------------------

  ### SET WORKING DIRECTORY ###
  # setwd("C:/Users/azizg/Downloads/Cybersyn Case/~Production/Scripts")
  
  # Clear the global environment and run garbage collection
  rm(list = names(.GlobalEnv))
  gc()

  # Load header script
  source("00 Header.R")
  
  # Manage packages - set snapshot date, check R version, and package location
  manage_packages(snap.date <- "2024-10-08", getRversion())
  
  # Run processing and analysis
  source('02 Processing and analysis.R')
  
  ### MANUALLY SET PARAMETER FOR LIQUOR CATEGORIES ###
  ## THIS IS THE FUNCTION TO GENERATE MARKET SHARE GRAPHS FOR QUESTION #2A ##
  # The following are all the options:
    # c("WHISKY", "BRANDIES", "RUM", "VODKA", "LIQUEUR", "GIN", "TEQUILA", "COCKTAILS/RTD", "SCHNAPPS", "OTHER", "MEZCAL", "DISTILLED SPIRITS","UNKNOWN")  
  liquor_types_market_share <- calculate_market_share(DT, c("TEQUILA", "GIN", "WHISKY", "VODKA"))
  ggsave(OUTPUT("Liquor types market share.jpg"), plot = liquor_types_market_share, device = "jpeg", width = 10, height = 8)
  
  # Load Shiny tool
  source("03 R Shiny map.R")
  
   
   