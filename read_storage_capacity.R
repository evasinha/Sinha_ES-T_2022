# Author - Eva Sinha, Stanford University, esinha@stanford.edu
# Date   - 4th June, 2018

# Function details

#_______________________________________________________________________________
# Read Gross Storage Capacity information and estimate GSC per unit area and per unit annual precipitation
read_gross_storage_capacity <- function(wq_stations, precip_annual){
  
  # Read Gross Storage Capacity information
  gsc_basins                   <- read.csv(paste('~/Documents/repos/india_wq/india_WRIS/Dams/Dams_GSC_LSC.txt', sep=''), header=T, sep='\t')
  
  # Only keep select columns
  gsc_basins                   <- gsc_basins[, c('CWC_basin','GSC_MCM')]
  
  # Convert to character
  gsc_basins$CWC_basin         <- as.character(gsc_basins$CWC_basin)
  
  # Add station names
  gsc_basins                   <- merge(gsc_basins, wq_stations[, c('Station','CWC_basin','Drainage_area_km2')])
  
  # Estimate storage per unit area [km]
  gsc_basins$GSC_km            <- gsc_basins$GSC_MCM * (10^6) * (10^(-9))/gsc_basins$Drainage_area_km2
  gsc_basins$Drainage_area_km2 <- NULL # Drop area column
  
  # Add annual precip
  gsc_basins                   <- merge(gsc_basins, precip_annual)
  
  # Add column for storage capacity per unit of annual precipitation (using all 36 years of precip data 1980-2015)
  # and per unit or area and annual precipitation
  gsc_basins                   <- group_by(gsc_basins, Station) %>%
                                  mutate(GSC_MCM_mm = mean(GSC_MCM/precip),
                                         GSC        = mean(GSC_km * (10^6)/precip))
  
  gsc_basins$precip            <- NULL # Drop area column
  
  return(gsc_basins)
}

#_______________________________________________________________________________
# Read total Live Storage Capacity information and estimate LSC per unit area and per unit annual precipitation
read_live_storage_capacity <- function(wq_stations, precip_annual){
  
  # Read Gross Storage Capacity information
  lsc_basins                   <- read.csv(paste('~/Documents/repos/india_wq/india_WRIS/Reports/Basins_LSC.txt', sep=''), header=T, sep='\t')
  
  # Only keep select columns
  lsc_basins                   <- lsc_basins[, c('CWC_basin','LSC_completed_MCM')]
  
  # Rename columns
  colnames(lsc_basins)         <- c('CWC_basin', 'LSC_MCM')
  
  # Convert to character
  lsc_basins$CWC_basin         <- as.character(lsc_basins$CWC_basin)
  
  # Add station names
  lsc_basins                   <- merge(lsc_basins, wq_stations[, c('Station','CWC_basin','Drainage_area_km2')])
  
  # Estimate storage per unit area [km]
  lsc_basins$LSC_km            <- lsc_basins$LSC_MCM * (10^6) * (10^(-9))/lsc_basins$Drainage_area_km2
  lsc_basins$Drainage_area_km2 <- NULL # Drop area column
  
  # Add annual precip
  lsc_basins                   <- merge(lsc_basins, precip_annual)
  
  # Add column for storage capacity per unit of annual precipitation (using all 36 years of precip data 1980-2015)
  # and per unit or area and annual precipitation
  lsc_basins                   <- group_by(lsc_basins, Station) %>%
                                  mutate(LSC_MCM_mm = mean(LSC_MCM/precip),
                                         LSC        = mean(LSC_km * (10^6)/precip))
  
  lsc_basins$precip            <- NULL # Drop area column
  
  return(lsc_basins)
}

#_______________________________________________________________________________
# Read time series of total Live Storage Capacity information and estimate LSC per unit area and per unit annual precipitation
read_timeseries_live_storage_capacity <- function(wq_stations, precip_annual){
  
  # Read Gross Storage Capacity information 
  lsc_basins                   <- read.csv(paste('~/Documents/repos/india_wq/india_WRIS/Dams/Dams_LSC_timeseries.txt', sep=''), header=T, sep='\t')
  
  # Convert data to long format
  lsc_basins                   <- gather(lsc_basins, key=BasinId, value=LSC_MCM, -Year)
  
  # Add CWC_basin
  lsc_basins$CWC_basin         <- basin_labeller('variable', lsc_basins$BasinId)
  
  # Add station names and drainage area
  lsc_basins                   <- merge(lsc_basins, wq_stations[, c('Station','CWC_basin','Drainage_area_km2')])
  
  # Estimate storage per unit area [km]
  lsc_basins$LSC_km            <- lsc_basins$LSC_MCM * (10^6) * (10^(-9))/lsc_basins$Drainage_area_km2
  lsc_basins$Drainage_area_km2 <- NULL # Drop area column
  
  # Add annual precip
  lsc_basins                   <- merge(lsc_basins, precip_annual)
  
  # Add column for storage capacity per unit of annual precipitation (using all 36 years of precip data 1980-2015)
  # and per unit of area and annual precipitation
  lsc_basins$LSC_MCM_mm        <- lsc_basins$LSC_MCM / lsc_basins$precip
  lsc_basins$LSC               <- (lsc_basins$LSC_km * (10^6)) / lsc_basins$precip
  
  lsc_basins$precip            <- NULL # Drop area column
  
  return(lsc_basins)
}
