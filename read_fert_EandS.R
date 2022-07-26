# Author - Eva Sinha, Stanford University, esinha@stanford.edu
# Date   - 15th March, 2018

# Function details
# read_EandS_fert_India              - Read fertilizer usage within India [thousands tonnes]
# read_EandS_fert_India_states       - Read fertilizer usage rate for various states in India [kg/ha]
# read_EandS_fert_India_river_basins - Read fertilizer usage rate for various river basins in India [kg/km2]


#_______________________________________________________________________________
# Read fertilizer usage within India [thousands tonnes]
# Source: Directorate of Economics and Statistics Department of Agriculture, Cooperation and Farmers Welfare
read_EandS_fert_India <- function(){
  
  # ---------- Define file path
  in_folder           <- '~/Documents/repos/india_wq/EandS/'
  
  # Read fertilizer consumption in India based on E and S [thousands tonnes]
  EandS_fert          <- read_excel(paste(in_folder,'India_fertilizer_consumption.xlsx',sep=''), sheet='Table14.2', skip=2)
  EandS_fert          <- EandS_fert[,c('Year','Nitrogen','Phosphorus','Potassium')]
  
  # Convert to long format
  EandS_fert          <- gather(EandS_fert, key=Nutrient, value=fert_thous_ton, -Year)
  
  # Add columns to estimate fertilizer usage in kg and Terragrams
  EandS_fert$fert_kg  <- EandS_fert$fert_thous_ton * (10^6)
  EandS_fert$fert_Tg  <- EandS_fert$fert_kg/(10^9)
  
  EandS_fert$Year     <- as.numeric(substr(EandS_fert$Year,1,4))
  EandS_fert$scenario <- 'Government of India' # Directorate of Economics and Statistics
  
  return(EandS_fert)
  
}

#_______________________________________________________________________________
# Read fertilizer usage rate for various states in India [kg/km2]
# Source: Directorate of Economics and Statistics Department of Agriculture, Cooperation and Farmers Welfare
read_EandS_fert_India_states <- function(){
  
  # ---------- Define file path
  in_folder           <- '~/Documents/repos/india_wq/EandS/'
  
  # Read fertilizer consumption by states in India based on E and S [kg/ha]
  EandS_fert          <- read_excel(paste(in_folder,'India_fertilizer_consumption.xlsx',sep=''), sheet='Table14.4b_Nitrogen', skip=1)
  EandS_fert          <- EandS_fert[,-1]
  
  # Convert to long format
  EandS_fert          <- gather(EandS_fert, key=Year, value=fert_kg_ha, -State)
  EandS_fert$Year     <- as.numeric(substr(EandS_fert$Year,1,4))
  
  # Convert to kg/km2
  EandS_fert$fert_kgN_km2 <- EandS_fert$fert_kg_ha * 100
  
  # Drop column
  EandS_fert$fert_kg_ha   <- NULL
    
  return(EandS_fert)
}

#_______________________________________________________________________________
# Read fertilizer usage rate for various river basins in India [kgN/km2/yr]
# Source: Directorate of Economics and Statistics Department of Agriculture, Cooperation and Farmers Welfare
read_EandS_fert_India_river_basins <- function(){
  
  # ---------- Define file path
  in_folder                      <- '~/Documents/repos/india_wq/EandS/'
  
  # Read fertilizer consumption by CWC basins in India based on E and S [kg/km2]
  EandS_fert_CWC_basins          <- read_excel(paste(in_folder,'India_fertilizer_consumption.xlsx',sep=''), sheet='RiverBasin_Nitrogen_kg_km2', skip=1)
  
  # Convert to long format
  EandS_fert_CWC_basins          <- gather(EandS_fert_CWC_basins, key=Year, value=fert_kgN_km2, -Basin)
  EandS_fert_CWC_basins$Year     <- as.numeric(substr(EandS_fert_CWC_basins$Year,1,4))
  EandS_fert_CWC_basins$Desc     <- 'Reported values'
  
  # # Estimate values from 1980-2003 based on linear fit between data and year from 2004-2010
  # EandS_fert_CWC_basins          <- group_by(EandS_fert_CWC_basins, Basin) %>% 
  #                                   do(add_fert_rows_linear_fit_yr(., interpolate_yrs = c(1980:2003)))
  
  # Estimate fertilizer application rate for river basins from 1950-2003 based on fraction ot the total India fertilizer usage
  # 1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1986-2003
  EandS_fert_basins_1950_2003    <- estimate_basin_fert_usage()
  
  # Combine into a single data frame
  EandS_fert_CWC_basins          <- bind_rows(EandS_fert_CWC_basins, EandS_fert_basins_1950_2003)
  
  # Read fertilizer consumption in India based on E and S [kg/km2]
  EandS_fert                     <- read_excel(paste(in_folder,'India_fertilizer_consumption.xlsx',sep=''), sheet='Table14.2_kg_km2', skip=2)
  EandS_fert                     <- EandS_fert[,c('Year','Nitrogen')]
  EandS_fert$Year                <- as.numeric(substr(EandS_fert$Year,1,4))
  EandS_fert$Basin               <- 'All India average'
  EandS_fert$Desc                <- 'Reported values'
  
  # Rename column
  colnames(EandS_fert)[which(colnames(EandS_fert) == 'Nitrogen')] <- 'fert_kgN_km2'
  
  # Combine into a CWC basin and All India average into a single data frame
  EandS_fert_CWC_basins          <- bind_rows(EandS_fert_CWC_basins, EandS_fert)
  
  # Add column for source of data
  EandS_fert_CWC_basins$scenario <- 'Government of India Directorate of Economics and Statistics'
  
  return(EandS_fert_CWC_basins)
}

# ______________________________________________________________________________
# For each river basin estimate values for the given year based on linear fit between data and year from 2004-2010
add_fert_rows_linear_fit_yr <- function(df, interpolate_yrs){
  
  fit              <- lm(data=df[which(df$Year %in% c(2004:2010)),], fert_kgN_km2 ~ Year)
  newdata          <- data.frame(Year = interpolate_yrs)
  tmp              <- predict(fit, newdata) 
  tmp              <- as.data.frame(cbind(Year=interpolate_yrs, fert_kgN_km2=tmp))
  tmp$Basin        <- unique(df$Basin)
  tmp$Desc         <- 'Extrapolated values'
  
  return(rbind(tmp, df))
}

# ______________________________________________________________________________
# Estimate fertilizer application rate for river basins from 1950-2003 based on fraction ot the total India fertilizer usage
# 1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1986-2003
estimate_basin_fert_usage <- function(){
  
  # ---------- Define file path
  in_folder                      <- '~/Documents/repos/india_wq/EandS/'
  
  # Read fertilizer consumption in India based on E and S [thousands tonnes]
  EandS_fert                     <- read_excel(paste(in_folder,'India_fertilizer_consumption.xlsx',sep=''), sheet='Table14.2', skip=2)
  EandS_fert                     <- EandS_fert[,c('Year','Nitrogen')]
  EandS_fert$Year                <- as.numeric(substr(EandS_fert$Year,1,4))
  
  # Rename column
  colnames(EandS_fert)[which(colnames(EandS_fert) == 'Nitrogen')] <- 'fert_thous_ton_India'
 
  # Read fertilizer consumption by CWC basins in India based on E and S [thousands tonnes]
  EandS_fert_CWC_basins          <- read_excel(paste(in_folder,'India_fertilizer_consumption.xlsx',sep=''), sheet='RiverBasin_Nitrogen', range='A2:N12')
  
  # Convert to long format
  EandS_fert_CWC_basins          <- gather(EandS_fert_CWC_basins, key=Year, value=fert_thous_ton_Basin, -Basin, -Area_km2)
  EandS_fert_CWC_basins$Year     <- as.numeric(substr(EandS_fert_CWC_basins$Year,1,4))

  # Merge columns to add fertilizer usage for the whole of India
  EandS_fert_CWC_basins          <- merge(EandS_fert_CWC_basins, EandS_fert)
  
  # Estimate what fraction of total India fertilizer is used by various river basins
  EandS_fert_CWC_basins          <- group_by(EandS_fert_CWC_basins, Basin, Area_km2) %>%
                                    summarise(avg_frac = mean(fert_thous_ton_Basin/fert_thous_ton_India))
  
  # Estimate basins fertilizer usage as a fraction of India usage for all basins
  EandS_fert_CWC_basins          <- group_by(EandS_fert_CWC_basins, Basin) %>%
                                    do(estimate_basin_frac_nation(., EandS_fert))
  
  return(EandS_fert_CWC_basins)
}   

# ______________________________________________________________________________
estimate_basin_frac_nation  <- function(df, EandS_fert){
  
  tmp                      <- EandS_fert[which(EandS_fert$Year < 2004), ]
  tmp$Basin                <- df$Basin
  tmp$Area_km2             <- df$Area_km2
  tmp$avg_frac             <- df$avg_frac
  tmp$fert_thous_ton_Basin <- tmp$fert_thous_ton_India * tmp$avg_frac
  tmp$fert_kgN_km2         <- (tmp$fert_thous_ton_Basin * (10^6))/tmp$Area_km2
  
  # Only keep select columns
  tmp                      <- tmp[ ,c('Year','Basin','fert_kgN_km2')]
  
  tmp$Desc                 <- 'Estimated values'
  
  return(tmp)
}

#_______________________________________________________________________________
# Interpolate annual fertilizer values for yrs falling between the 1980 and 1985
interpolate_fert <- function(fertl){
  
  # Cast into wide format
  fertl               <- spread(fertl, key=Year, value=fert_kgN_km2)
  
  fertl['1981']       <- 0
  fertl['1982']       <- 0
  fertl['1983']       <- 0
  fertl['1984']       <- 0
  
  for (ind in 1:nrow(fertl)) {
    
    input_x           <- c(1980, 1985)
    input_y           <- c(fertl[ind,'1980'], fertl[ind,'1985'])
    
    fertl[ind,'1981'] <- approxExtrap(x=input_x, y=input_y, xout=1981, method='linear')$y
    fertl[ind,'1982'] <- approxExtrap(x=input_x, y=input_y, xout=1982, method='linear')$y
    fertl[ind,'1983'] <- approxExtrap(x=input_x, y=input_y, xout=1983, method='linear')$y
    fertl[ind,'1984'] <- approxExtrap(x=input_x, y=input_y, xout=1984, method='linear')$y
    
  }  # FOR ind loop ends
  
  # Melt into long format
  fertl               <- gather(fertl, key=Year, value=fert_kgN_km2, -Basin)
  fertl$Year          <- as.numeric(fertl$Year)
  
  return(fertl)
  
}