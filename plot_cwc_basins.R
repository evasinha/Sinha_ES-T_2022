# Author - Eva Sinha, Stanford University, esinha@stanford.edu
# Date   - 13th July, 2017
#
# Function details
# 

library(ggplot2)
library(ggthemes)
library(rgdal)
library(raster)   # projection
library(maptools) # unionSpatialPolygons
library(ggrepel)  # geom_text_repel

# Mean annual precip from 1980-2015
# Brahmani and Baitarni - 1429 mm/yr
# Subernarekha          - 1374 mm/yr
# Narmada               - 1146 mm/yr
# Mahanadi              - 1299 mm/yr
# Godavari              - 1106 mm/yr
# Cauvery               - 903 mm/yr
# Krishna               - 753 mm/yr

color_pal <- c('1429'    = '#2f5d9e',
               '1374'    = '#2f799e',
               '1146'    = '#77d3ec',
               '1299'    = '#43add0',
               '1106'    = '#8fdbea',
               '903'     = '#afe5ed',
               '753'     = '#d7f2f6',
               'Brahmani and Baitarni'    = '#2f5d9e',
               'Subernarekha'             = '#2f799e',
               'Narmada'                  = '#77d3ec',
               'Mahanadi'                 = '#43add0',
               'Godavari'                 = '#8fdbea',
               'Cauvery'                  = '#afe5ed',
               'Krishna'                  = '#d7f2f6',
               'Indus (Up to border)'                               = '#f1fcb3',
               'Barak and others'                                   = '#fcebd7',
               'Brahmaputra'                                        = '#b6d4fc',
               'East flowing rivers between Mahanadi and Pennar'    = '#ccfcd5',
               'East flowing rivers between Pennar and Kanyakumari' = '#b6f4fc',
               'Ganga'                                              = 'deepskyblue',
               'Ganga (within India)'                               = 'deepskyblue',
               'Mahi'                                               = '#A020F0',
               'Minor rivers draining into Myanmar and Bangladesh'  = '#f4fcc7',
               'Pennar'                                             = '#e7fcd7',
               'Sabarmati'                                          = '#fcb3df',
               'Tapi'                                               = 'darkred',
               'West flowing rivers of Kutch & Saurashtra in. Luni' = '#d7fcf1',
               'West flowing rivers from Tadri to Kanyakumari'      = '#d4d9fc',
               'West flowing rivers from Tapi to Tadri'             = '#c5c7fc')

# ______________________________________________________________________________
# Plot CWC basins and locations of selected stations
# REMAKE PLOTS USING CWC BASINS BASED ON '~/Documents/repos/India_wq/Shapefiles/Hydroshed/select_as_bas_15s_major_CWC_basins'
plot_CWC_basins <- function(filePath, fileName) {
  
  filepath         <- '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/'
  f.name           <- 'CWC_select_stations.txt'
  select.sta.loc   <- read.csv(paste(filepath,f.name,sep=''), header=T, sep='\t')
  f.name           <- 'CWC_basins.txt'
  cwc.basin.precip <- read.csv(paste(filepath,f.name,sep=''), header=T, sep='\t')
  
  
  # Read fertilizer consumption by CWC basins in India based on E and S [kg/km2]
  in_folder                      <- '~/Documents/repos/india_wq/EandS/'
  cwc.basin.fert          <- read_excel(paste(in_folder,'India_fertilizer_consumption.xlsx',sep=''), sheet='RiverBasin_Nitrogen_kg_km2', skip=1)
  
  # Convert to long format
  cwc.basin.fert          <- gather(cwc.basin.fert, key=Year, value=fert_kgN_km2, -Basin)
  cwc.basin.fert$Year     <- as.numeric(substr(cwc.basin.fert$Year,1,4))
  
  # Only keep 2015 fertilizer application rates
  cwc.basin.fert <- filter(cwc.basin.fert, Year == 2015)
  cwc.basin.fert[which(cwc.basin.fert$Basin == 'Ganga (within India)'), 'Basin'] <- 'Ganga'

  # Rename column
  colnames(cwc.basin.fert)[which(colnames(cwc.basin.fert) == 'Basin')] <- 'CWC_basin'
  
  # Read CWC shapefile
  setwd('~/Documents/repos/india_wq/Shapefiles/Hydroshed/')
  cwc.basins.hydroshed <- readOGR(dsn=getwd(),layer='select_as_bas_15s_major_CWC_basins')
  
  setwd('~/Documents/repos/india_wq/Shapefiles/Hydro1K/')
  cwc.basins.hydro1k   <- readOGR(dsn=getwd(),layer='hydro1K_as_basins')
  wq.basins            <- readOGR(dsn=getwd(),layer='basin_wq_stations')
  streamlines          <- readOGR(dsn=getwd(),layer='hydro1K_as_streamlines')
  
  # Remove Musiri station basin
  wq.basins            <- wq.basins[which(wq.basins@data$WQ_station != 'Musiri'),]
  select.sta.loc       <- select.sta.loc[which(select.sta.loc$Station != 'Musiri'),]
  
  # Only keep watersheds with CWC basin name
  cwc.basins.hydro1k   <- cwc.basins.hydro1k[which(!is.na(cwc.basins.hydro1k@data$CWC_basin)),]
  streamlines          <- streamlines[which(!is.na(streamlines@data$CWC_basin)),]

  cwc.basins.hydroshed <- cwc.basins.hydroshed[which(cwc.basins.hydroshed$CWC_basin %in% cwc.basin.precip$CWC_basin),]
  cwc.basins.hydro1k   <- cwc.basins.hydro1k[which(cwc.basins.hydro1k$CWC_basin %in% cwc.basin.precip$CWC_basin),]
  streamlines          <- streamlines[which(streamlines$CWC_basin %in% cwc.basin.precip$CWC_basin),]
  
  # Modify projection to WGS84
  proj.cwc.basins      <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  cwc.basins.hydroshed <- spTransform(cwc.basins.hydroshed, proj.cwc.basins)
  cwc.basins.hydro1k   <- spTransform(cwc.basins.hydro1k,   proj.cwc.basins)
  wq.basins            <- spTransform(wq.basins,    proj.cwc.basins)
  streamlines          <- spTransform(streamlines,  proj.cwc.basins)

  # CWC basins polygon centroids for placing labels
  cwc.basins.centroids           <- data.frame(coordinates(cwc.basins.hydroshed), CWC_basin=cwc.basins.hydroshed@data$CWC_basin)
  names(cwc.basins.centroids)    <- c('long','lat','CWC_basin')
  
  #prepare shapefiles for ploting in ggplot2
  cwc.basins.hydro1k@data$id     <- rownames(cwc.basins.hydro1k@data)
  cwc.basins.hydro1k.points      <- fortify(cwc.basins.hydro1k, region='id')
  cwc.basins.hydro1k             <- merge(cwc.basins.hydro1k.points, cwc.basins.hydro1k@data, by='id')
  
  # To ensure that id gets CWC_basin info that will be used in geom_map. 
  # A separate column of CWC_basin should also be present for merging with plotting data
  cwc.basins.hydroshed@data$id   <- cwc.basins.hydroshed@data$CWC_basin
  cwc.basins.hydroshed.points    <- fortify(cwc.basins.hydroshed, region='id')
  cwc.basins.hydroshed           <- merge(cwc.basins.hydroshed.points, cwc.basins.hydroshed@data, by='id')
  
  wq.basins@data$id              <- rownames(wq.basins@data)
  wq.basins.points               <- fortify(wq.basins, region='id')
  wq.basins                      <- merge(wq.basins.points, wq.basins@data, by='id')
  
  streamlines@data$id            <- rownames(streamlines@data)
  streamlines.points             <- fortify(streamlines, region='id')
  streamlines                    <- merge(streamlines.points, streamlines@data, by='id')
  
  # India boundary map
  world.map             <- map_data('world')
  india.map             <- world.map[which(world.map$region == 'India'),]
  
  # Breaks for annual precipitation
  breaks                <- c(250, 500, 750, 1000, 1250, 1500, 3000)
  
  color_pal_tmp         <- c('[250,500]'   = '#e7f7fa',
                             '(500,750]'   = '#bfeaf1',
                             '(750,1000]'  = '#7ed6e7',
                             '(1000,1250]' = '#2f99bc',
                             '(1250,1500]' = '#256da7', # '#2f5d9e',
                             '(1500,3000]' = '#19508f') # '#1d3a63')
  
  # Add range for annual precipitation
  cwc.basin.precip$range    <- cut(cwc.basin.precip$Avg_precip_mm_yr, dig.lab=5, breaks=breaks, include.lowest=TRUE)
  
  # Update maximum range and color-pal for maximum range
  max_precip                <- round(max(cwc.basin.precip$Avg_precip_mm_yr, na.rm=TRUE),0)
  cwc.basin.precip$range    <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), names(color_pal_tmp))
  
  # Update minimum range and color-pal for minimum range
  min_precip                <- round(min(cwc.basin.precip$Avg_precip_mm_yr, na.rm=TRUE),0)
  cwc.basin.precip$range    <- gsub(paste('\\[',breaks[max(which(breaks < min_precip))],',',sep=''), paste('\\[',min_precip,',',sep=''), cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub(paste('\\[',breaks[max(which(breaks < min_precip))],',',sep=''), paste('\\[',min_precip,',',sep=''), names(color_pal_tmp))
  
  print(table(cwc.basin.precip$range))
  
  # Replace square brackets with round brackets
  cwc.basin.precip$range    <- gsub('\\[', '(', cwc.basin.precip$range)
  cwc.basin.precip$range    <- gsub('\\]', ')', cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub('\\[', '(', names(color_pal_tmp))
  names(color_pal_tmp)      <- gsub('\\]', ')', names(color_pal_tmp))
  
  # Reorder factor levels
  cwc.basin.precip$range    <- factor(cwc.basin.precip$range, levels=names(color_pal_tmp))
  
  print(table(cwc.basin.precip$range))
  
  # Only keep streamlines for main rivers
  maj_streamlines       <- na.omit(streamlines)
  
  # Drop unused levels
  cwc.basins.hydroshed  <- droplevels(cwc.basins.hydroshed)

  # --- Make spatial plot of mean annual precipitation ---
  # Change directory for saving plot
  setwd(filePath)
  
  # Delete existing file
  fileName    <- 'CWC_basins_mean_precip.png'
  unlink(fileName)
  # Start pdf device driver for saving plots
  png(fileName, width = 700, height = 800) 
  
  p1 <- ggplot() +
    geom_map(data=cwc.basin.precip[,c('CWC_basin','range')],
             aes(map_id=CWC_basin, fill=range), map=cwc.basins.hydroshed) +
    geom_line(data=maj_streamlines,       aes(x=long, y=lat, group=group), color='white', size=1.2) +
    # geom_line(data=streamlines,     aes(x=long,y=lat,group=group),color='dodgerblue', size=0.3) +
    geom_path(data=cwc.basins.hydroshed,  aes(x=long, y=lat, group=group), color='black',      size=0.75) +
    geom_path(data=india.map,             aes(x=long, y=lat, group=group), color='grey15',     size=1.2) +
    geom_text_repel(data=subset(cwc.basins.centroids, CWC_basin %in% c('Indus (Up to border)','Godavari','Krishna','Cauvery','Narmada','Sabarmati','Mahi','Tapi','Pennar')), 
                    aes(x=long, y=lat, label=CWC_basin),
                    nudge_x       = 70-subset(cwc.basins.centroids, CWC_basin %in% c('Godavari','Krishna','Cauvery','Narmada','Sabarmati','Mahi','Tapi','Pennar'))$long,
                    nudge_y       = -0.5,
                    size          = 6,
                    segment.size  = 0.75,
                    segment.color = 'black',
                    direction     = 'y',
                    hjust         = 1) +
    geom_text_repel(data=subset(cwc.basins.centroids, CWC_basin %in% c('Brahmani and Baitarni','Subernarekha','Mahanadi')), 
                    aes(x=long, y=lat, label=CWC_basin),
                    nudge_x       = 90-subset(cwc.basins.centroids, CWC_basin %in% c('Brahmani and Baitarni','Subernarekha','Mahanadi'))$long,
                    nudge_y       = -1.5,
                    size          = 6,
                    segment.size  = 0.75,
                    segment.color = 'black',
                    direction     = 'y',
                    hjust         = 0) +
    geom_text_repel(data=subset(cwc.basins.centroids, CWC_basin %in% c('Ganga','Brahmaputra')), 
                    aes(x=long, y=lat, label=CWC_basin),
                    nudge_x       = 1.0,
                    nudge_y       = 31.0-subset(cwc.basins.centroids, CWC_basin %in% c('Ganga','Brahmaputra'))$lat,
                    size          = 6,
                    segment.size  = 0.75,
                    segment.color = 'black',
                    direction     = 'x',
                    vjust         = 0) +
    scale_fill_manual(values=color_pal_tmp) +
    scale_x_continuous(limits=c(min(cwc.basins.hydroshed$long),max(cwc.basins.hydroshed$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(cwc.basins.hydroshed$lat), max(cwc.basins.hydroshed$lat)),  expand = c(0, 0)) +
    guides(fill=guide_legend(ncol=1, title=expression(paste(Precipitation,' [',mm~yr^-1,']')))) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=18,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.85,0.25),
          legend.justification = 'center',
          legend.text          = element_text(size=18,family='Helvetica'))
  
  print(p1)
  
  dev.off()
 
  # ---
  
  # Delete existing file
  fileName    <- 'CWC_basins_mean_precip_2.png'
  unlink(fileName)
  # Start pdf device driver for saving plots
  png(fileName, width = 750, height = 800) 
  
  # Keep major streamlines and station locations for the final basins only
  final_basins         <- c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi','Godavari','Cauvery','Krishna')
  maj_streamlines      <- maj_streamlines[which(maj_streamlines$CWC_basin %in% final_basins),]
  plot.select.sta.loc  <- select.sta.loc[which(select.sta.loc$CWC_basin %in% final_basins),]
  
  p2 <- ggplot() +
    geom_map(data=cwc.basin.precip[,c('CWC_basin','range')], 
             aes(map_id=CWC_basin, fill=range), map=cwc.basins.hydroshed) +
    geom_line(data=maj_streamlines,       aes(x=long, y=lat, group=group), color='white', size=1.2) +
    # geom_line(data=streamlines,     aes(x=long, y=lat, group=group), color='dodgerblue', size=0.3) +
    geom_path(data=subset(cwc.basins.hydroshed, !CWC_basin %in% final_basins), aes(x=long, y=lat, group=group), color='black',  size=0.75) +
    geom_path(data=subset(cwc.basins.hydroshed, CWC_basin %in% final_basins),  aes(x=long, y=lat, group=group), color='#ff9000',size=0.75) +
    geom_path(data=india.map,             aes(x=long, y=lat, group=group), color='grey15',     size=1.2) +
    geom_point(data=plot.select.sta.loc, aes(x=Longitude, y=Latitude), fill='red', color='black', shape=21, size=4) +
    geom_text_repel(data=subset(plot.select.sta.loc, !Station %in% c('Mandleshwar')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = 2.6,
                    size          = 6.5,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    direction     = 'y',
                    hjust         = 0) +
    geom_text_repel(data=subset(plot.select.sta.loc, Station %in% c('Mandleshwar')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = -3.5,
                    nudge_y       = -1.8,
                    size          = 6.5,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    hjust         = 1) +
    scale_fill_manual(values=color_pal_tmp) +
    scale_x_continuous(limits=c(min(cwc.basins.hydroshed$long),max(cwc.basins.hydroshed$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(cwc.basins.hydroshed$lat), max(cwc.basins.hydroshed$lat)),  expand = c(0, 0)) +
    # guides(fill=guide_legend(ncol=1, title=expression(paste(Mean~precipitation,' [',mm~yr^-1,']')))) +
    guides(fill=guide_legend(ncol=1, title=expression(atop(Mean~precipitation, paste('[',mm~yr^-1,']'))))) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=20,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.84,0.25),
          legend.justification = 'center',
          legend.title.align   = 1,
          legend.text          = element_text(size=20,family='Helvetica'))
  
  print(p2)
  
  dev.off()
  
  # --- Make spatial plot of interannual variability of annual precipitation ---
  
  # Breaks for annual precipitation
  breaks                <- c(50, 100, 150, 200, 250, 500)
  
  color_pal_tmp         <- c('[50,100]'  = '#e7f7fa',
                             '(100,150]' = '#bfeaf1',
                             '(150,200]' = '#7ed6e7',
                             '(200,250]' = '#2f99bc',
                             '(250,500]' = '#256da7')
  
  # Add range for annual precipitation
  cwc.basin.precip$range    <- cut(cwc.basin.precip$sd_precip_mm_yr, dig.lab=5, breaks=breaks, include.lowest=TRUE)
  
  # Update maximum range and color-pal for maximum range
  max_precip                <- round(max(cwc.basin.precip$sd_precip_mm_yr, na.rm=TRUE),0)
  cwc.basin.precip$range    <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), names(color_pal_tmp))
  
  # Update minimum range and color-pal for minimum range
  min_precip                <- round(min(cwc.basin.precip$sd_precip_mm_yr, na.rm=TRUE),0)
  cwc.basin.precip$range    <- gsub(paste('\\[',breaks[max(which(breaks < min_precip))],',',sep=''), paste('\\[',min_precip,',',sep=''), cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub(paste('\\[',breaks[max(which(breaks < min_precip))],',',sep=''), paste('\\[',min_precip,',',sep=''), names(color_pal_tmp))
  
  # Reorder factor levels
  cwc.basin.precip$range    <- factor(cwc.basin.precip$range, levels=names(color_pal_tmp))

  # Replace square brackets with round brackets
  cwc.basin.precip$range    <- gsub('\\[', '(', cwc.basin.precip$range)
  cwc.basin.precip$range    <- gsub('\\]', ')', cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub('\\[', '(', names(color_pal_tmp))
  names(color_pal_tmp)      <- gsub('\\]', ')', names(color_pal_tmp))
  
  # Delete existing file
  fileName    <- 'CWC_basins_mean_sd_2.png'
  unlink(fileName)
  # Start pdf device driver for saving plots
  png(fileName, width = 750, height = 850) 
  
  p3 <- ggplot() +
    geom_map(data=cwc.basin.precip[,c('CWC_basin','range')], 
             aes(map_id=CWC_basin, fill=range), map=cwc.basins.hydroshed) +
    geom_line(data=maj_streamlines,       aes(x=long, y=lat, group=group), color='white', size=1.2) +
    # geom_line(data=streamlines,     aes(x=long, y=lat, group=group), color='dodgerblue', size=0.3) +
    geom_path(data=subset(cwc.basins.hydroshed, !CWC_basin %in% final_basins), aes(x=long, y=lat, group=group), color='black',  size=0.75) +
    geom_path(data=subset(cwc.basins.hydroshed, CWC_basin %in% final_basins),  aes(x=long, y=lat, group=group), color='#ff9000',size=0.75) +
    geom_path(data=india.map,             aes(x=long, y=lat, group=group), color='grey15',     size=1.2) +
    geom_point(data=plot.select.sta.loc, aes(x=Longitude, y=Latitude),fill='red',color='black',shape=21,size=4) +
    geom_text_repel(data=subset(plot.select.sta.loc, !Station %in% c('Mandleshwar')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = 2.6,
                    size          = 6,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    direction     = 'y',
                    hjust         = 0) +
    geom_text_repel(data=subset(plot.select.sta.loc, Station %in% c('Mandleshwar')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = -4,
                    nudge_y       = -1.8,
                    size          = 6,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    hjust         = 1) +
    scale_fill_manual(values=color_pal_tmp) +
    scale_x_continuous(limits=c(min(cwc.basins.hydroshed$long),max(cwc.basins.hydroshed$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(cwc.basins.hydroshed$lat), max(cwc.basins.hydroshed$lat)),  expand = c(0, 0)) +
    guides(fill=guide_legend(ncol=1, title=expression(paste(Interannual~variability,' [',mm~yr^-1,']')))) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=20,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.835,0.25),
          legend.justification = 'center',
          legend.text          = element_text(size=20,family='Helvetica'))
  
  print(p3)
  
  dev.off()
  
  # --- Make spatial plot of coefficient of variation of annual precipitation ---
  
  # Add coefficient of variation
  cwc.basin.precip$cv_precip <- cwc.basin.precip$sd_precip_mm_yr/cwc.basin.precip$Avg_precip_mm_yr
  
  # Breaks for annual precipitation
  breaks                <- c(0, 0.15, 0.2, 0.25, 0.3, 0.4)
  
  color_pal_tmp         <- c('[0,0.15]'    = '#ffffd4', # 'gray90',
                             '(0.15,0.2]'  = '#fed98e', # 'gray70',
                             '(0.2,0.25]'  = '#fe9929', # 'gray50',
                             '(0.25,0.3]'  = '#d95f0e', # 'gray30',
                             '(0.3,0.4]'   = '#993404') # 'gray20')
  
  # Add range for annual precipitation
  cwc.basin.precip$range    <- cut(cwc.basin.precip$cv_precip, dig.lab=5, breaks=breaks, include.lowest=TRUE)
  
  # Update maximum range and color-pal for maximum range
  max_precip                <- round(max(cwc.basin.precip$cv_precip, na.rm=TRUE),2)
  cwc.basin.precip$range    <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), names(color_pal_tmp))
  
  # Update minimum range and color-pal for minimum range
  min_precip                <- round(min(cwc.basin.precip$cv_precip, na.rm=TRUE),2)
  cwc.basin.precip$range    <- gsub(paste('\\[',breaks[max(which(breaks < min_precip))],',',sep=''), paste('\\[',min_precip,',',sep=''), cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub(paste('\\[',breaks[max(which(breaks < min_precip))],',',sep=''), paste('\\[',min_precip,',',sep=''), names(color_pal_tmp))
  
  # Reorder factor levels
  cwc.basin.precip$range    <- factor(cwc.basin.precip$range, levels=names(color_pal_tmp))
  
  # Replace square brackets with round brackets
  cwc.basin.precip$range    <- gsub('\\[', '(', cwc.basin.precip$range)
  cwc.basin.precip$range    <- gsub('\\]', ')', cwc.basin.precip$range)
  names(color_pal_tmp)      <- gsub('\\[', '(', names(color_pal_tmp))
  names(color_pal_tmp)      <- gsub('\\]', ')', names(color_pal_tmp))
  
  # Delete existing file
  fileName    <- 'CWC_basins_cv_2.png'
  unlink(fileName)
  # Start pdf device driver for saving plots
  png(fileName, width = 750, height = 800) 
  
  p4 <- ggplot() +
    geom_map(data=cwc.basin.precip[,c('CWC_basin','range')], 
             aes(map_id=CWC_basin, fill=range), map=cwc.basins.hydroshed) +
    geom_line(data=maj_streamlines,       aes(x=long, y=lat, group=group), color='blue', size=1.2) +
    # geom_line(data=streamlines,     aes(x=long, y=lat, group=group), color='dodgerblue', size=0.3) +
    geom_path(data=subset(cwc.basins.hydroshed, !CWC_basin %in% final_basins), aes(x=long, y=lat, group=group), color='black',  size=0.75) +
    geom_path(data=subset(cwc.basins.hydroshed, CWC_basin %in% final_basins),  aes(x=long, y=lat, group=group), color='#ff9000',size=0.75) +
    geom_path(data=india.map,             aes(x=long, y=lat, group=group), color='grey15',     size=1.2) +
    geom_point(data=plot.select.sta.loc, aes(x=Longitude, y=Latitude),fill='red',color='black',shape=21,size=4) +
    geom_text_repel(data=subset(plot.select.sta.loc, !Station %in% c('Mandleshwar')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = 2.6,
                    size          = 6.5,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    direction     = 'y',
                    hjust         = 0) +
    geom_text_repel(data=subset(plot.select.sta.loc, Station %in% c('Mandleshwar')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = -3.0,
                    nudge_y       = -1.8,
                    size          = 6.5,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    hjust         = 1) +
    scale_fill_manual(values=color_pal_tmp) +
    scale_x_continuous(limits=c(min(cwc.basins.hydroshed$long),max(cwc.basins.hydroshed$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(cwc.basins.hydroshed$lat), max(cwc.basins.hydroshed$lat)),  expand = c(0, 0)) +
    guides(fill=guide_legend(ncol=1, title=expression(paste(Coefficient~of~variation)))) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=20,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.84,0.25),
          legend.justification = 'center',
          legend.text          = element_text(size=20,family='Helvetica'))
  
  print(p4)
  
  dev.off()
  
  
  # --- Make spatial plot of fertilizer application rates ---
  
  # Breaks for annual precipitation
  breaks                <- seq(1500, 8500, 500)
  
  color_pal_tmp         <- c('[1500,2000]' = '#edf8e9',
                             '(2000,2500]'  = '#c7e9c0',
                             '(3500,4000]'  = '#a1d99b',
                             '(4500,5000]'  = '#74c476',
                             '(5000,5500]'  = '#31a354',
                             '(8000,8500]'  = '#006d2c')
  
  # Add range for annual precipitation
  cwc.basin.fert$range    <- cut(cwc.basin.fert$fert_kgN_km2, dig.lab=5, breaks=breaks, include.lowest=TRUE)
  
  print(table(cwc.basin.fert$range))
  
  # Update maximum range and color-pal for maximum range
  max_fert              <- round(max(cwc.basin.fert$fert_kgN_km2, na.rm=TRUE),0)
  cwc.basin.fert$range    <- gsub(paste(',',breaks[min(which(breaks > max_fert))],sep=''), paste(',',max_fert,sep=''), cwc.basin.fert$range)
  names(color_pal_tmp)    <- gsub(paste(',',breaks[min(which(breaks > max_fert))],sep=''), paste(',',max_fert,sep=''), names(color_pal_tmp))
  
  # Update minimum range and color-pal for minimum range
  min_fert              <- round(min(cwc.basin.fert$fert_kgN_km2, na.rm=TRUE),0)
  cwc.basin.fert$range    <- gsub(paste('\\[',breaks[max(which(breaks < min_fert))],',',sep=''), paste('\\[',min_fert,',',sep=''), cwc.basin.fert$range)
  names(color_pal_tmp)    <- gsub(paste('\\[',breaks[max(which(breaks < min_fert))],',',sep=''), paste('\\[',min_fert,',',sep=''), names(color_pal_tmp))
  
  # Reorder factor levels
  cwc.basin.fert$range    <- factor(cwc.basin.fert$range, levels=names(color_pal_tmp))
  
  
  # Replace square brackets with round brackets
  cwc.basin.fert$range    <- gsub('\\[', '(', cwc.basin.fert$range)
  cwc.basin.fert$range    <- gsub('\\]', ')', cwc.basin.fert$range)
  names(color_pal_tmp)      <- gsub('\\[', '(', names(color_pal_tmp))
  names(color_pal_tmp)      <- gsub('\\]', ')', names(color_pal_tmp))
  
  # Reorder factor levels
  cwc.basin.fert$range    <- factor(cwc.basin.fert$range, levels=names(color_pal_tmp))
  
  print(table(cwc.basin.fert$range))
  
  # Delete existing file
  fileName    <- 'CWC_basins_fert_2.png'
  unlink(fileName)
  # Start pdf device driver for saving plots
  png(fileName, width = 750, height = 800) 
  
  # Keep major streamlines and station locations for the final basins only
  final_basins         <- c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi','Godavari','Cauvery','Krishna')
  maj_streamlines      <- maj_streamlines[which(maj_streamlines$CWC_basin %in% final_basins),]
  plot.select.sta.loc  <- select.sta.loc[which(select.sta.loc$CWC_basin %in% final_basins),]
  
  p5 <- ggplot() +
    geom_map(data=cwc.basin.fert[,c('CWC_basin','range')], 
             aes(map_id=CWC_basin, fill=range), map=cwc.basins.hydroshed) +
    geom_line(data=maj_streamlines,       aes(x=long, y=lat, group=group), color='white', size=1.2) +
    # geom_line(data=streamlines,     aes(x=long, y=lat, group=group), color='dodgerblue', size=0.3) +
    geom_path(data=subset(cwc.basins.hydroshed, !CWC_basin %in% final_basins), aes(x=long, y=lat, group=group), color='black',  size=0.75) +
    geom_path(data=subset(cwc.basins.hydroshed, CWC_basin %in% final_basins),  aes(x=long, y=lat, group=group), color='#ff9000',size=0.75) +
    geom_path(data=india.map,             aes(x=long, y=lat, group=group), color='grey15',     size=1.2) +
    geom_point(data=plot.select.sta.loc, aes(x=Longitude, y=Latitude), fill='red', color='black', shape=21, size=4) +
    geom_text_repel(data=subset(plot.select.sta.loc, !Station %in% c('Mandleshwar')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = 2.6,
                    size          = 6.5,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    direction     = 'y',
                    hjust         = 0) +
    geom_text_repel(data=subset(plot.select.sta.loc, Station %in% c('Mandleshwar')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = -3.5,
                    nudge_y       = -1.8,
                    size          = 6.5,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    hjust         = 1) +
    scale_fill_manual(values=color_pal_tmp) +
    scale_x_continuous(limits=c(min(cwc.basins.hydroshed$long),max(cwc.basins.hydroshed$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(cwc.basins.hydroshed$lat), max(cwc.basins.hydroshed$lat)),  expand = c(0, 0)) +
    # guides(fill=guide_legend(ncol=1, title=expression(paste(Mean~precipitation,' [',mm~yr^-1,']')))) +
    guides(fill=guide_legend(ncol=1, title=expression(atop(Fert~application~rate, paste('[',kgN~km^-2,']'))))) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=20,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.84,0.25),
          legend.justification = 'center',
          legend.title.align   = 1,
          legend.text          = element_text(size=20,family='Helvetica'))
  
  print(p5)
  
  dev.off()
  
  # ---
  # Delete existing file
  unlink('CWC_basins_3.png')
  # Start pdf device driver for saving plots
  png('CWC_basins_3.png', width = 1000, height = 800) 
  
  plot.cwc.basins     <- cwc.basins.hydro1k[which(cwc.basins.hydro1k$CWC_basin %in% wq.basins$CWC_basin),]
  
  p3 <- ggplot() +
    geom_polygon(data=plot.cwc.basins, aes(x=long, y=lat, group=group, fill=CWC_basin, color='Hydro1K basins'), size=0.5) +
    geom_polygon(data=wq.basins,       aes(x=long, y=lat, group=group, color='Watershed upstream of station'), fill='transparent',size=1.05) +
    geom_path(data=india.map,          aes(x=long, y=lat, group=group),color='grey50',size=0.5) +
    geom_point(data=select.sta.loc,    aes(x=Longitude, y=Latitude), fill='red', color='black', shape=21, size=4) +
    geom_text(data=select.sta.loc,     aes(x=Longitude, y=Latitude, label=Station), color='black', size=6, nudge_y=0.5) +
    scale_fill_manual(values=color_pal) +
    scale_color_manual(name='',values=c('Hydro1K basins'='grey50','Watershed upstream of station'='black')) +
    scale_x_continuous(limits=c(min(india.map$long),max(india.map$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(india.map$lat), max(india.map$lat)),  expand = c(0, 0)) +
    guides(fill =guide_legend(ncol=1, title='CWC basins'),
           color=guide_legend(nrow=2, title=NULL, override.aes=list(fill=NA))) + # Override.aes for cleaning legends
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=15,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = 'right',
          legend.justification = 'center')
  
  print(p3)
  
  dev.off()
  
  # ---
  # Delete existing file
  unlink('CWC_basins_4.png')
  # Start pdf device driver for saving plots
  png('CWC_basins_4.png', width = 1000, height = 800) 
  
  final_basins        <- c('Brahmani and Baitarni','Subernarekha','Narmada',
                           'Mahanadi','Godavari','Cauvery','Krishna')
  
  plot.cwc.basins     <- plot.cwc.basins[which(plot.cwc.basins$CWC_basin %in% final_basins),]
  plot.wq.basins      <- wq.basins[which(wq.basins$CWC_basin %in% final_basins),]
  plot.select.sta.loc <- select.sta.loc[which(select.sta.loc$CWC_basin %in% final_basins),]
  
  # Reorder factor levels
  plot.cwc.basins$CWC_basin   <- factor(plot.cwc.basins$CWC_basin, levels=final_basins)
  plot.wq.basins$CWC_basin    <- factor(plot.wq.basins$CWC_basin, levels=final_basins)

  p4 <- ggplot() +
    geom_polygon(data=plot.cwc.basins,    aes(x=long,y=lat,fill=CWC_basin,color='Hydro1K basins',group=group), size=0.5) +
    geom_polygon(data=plot.wq.basins,     aes(x=long,y=lat,color='Watershed upstream of station',group=group),fill='transparent',size=1.05) +
    geom_path(data=india.map,             aes(x=long,y=lat,group=group),color='grey50',size=0.5) +
    geom_point(data=plot.select.sta.loc,  aes(x=Longitude, y=Latitude),fill='red',color='black',shape=21,size=4) +
    geom_text(data=plot.select.sta.loc,   aes(x=Longitude, y=Latitude,label=Station),color='black',size=6,nudge_y = 0.5) +
    scale_fill_manual(name='CWC basin', values=color_pal) +
    scale_color_manual(name='',values=c('Hydro1K basins'='grey50','Watershed upstream of station'='black')) +
    scale_x_continuous(limits=c(min(india.map$long),max(india.map$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(india.map$lat), max(india.map$lat)),  expand = c(0, 0)) +
    guides(fill =guide_legend(ncol=1, title='CWC basins'), 
           color=guide_legend(nrow=2, title=NULL, override.aes=list(fill=NA))) + # Override.aes for cleaning legends
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=15,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = 'right',
          legend.justification = 'center')
  
  print(p4)
  
  dev.off()
  
  # ------- Make individual plot for each station ---
  
  for (ind in 1:nrow(select.sta.loc)){
    
    plot.station    <- select.sta.loc[ind,]
    plot.cwc.basins <- cwc.basins.hydro1k[which(cwc.basins.hydro1k$BasinId == as.character(plot.station$BasinId)),]
    
    # Delete existing file
    fileName  <- paste(plot.station$Station, '.png', sep='')
    
    unlink(fileName)
    # Start pdf device driver for saving plots
    png(fileName, width = 700, height = 800) 
    
    p5 <- ggplot() +
      geom_polygon(data=plot.cwc.basins,  aes(x=long,y=lat,group=group, fill=CWC_basin)) +
      geom_path(data=india.map,           aes(x=long,y=lat,group=group),color='grey15',size=1.2) +
      geom_point(data=plot.station,       aes(x=Longitude, y=Latitude),fill='red',color='black',shape=21,size=8) +
      geom_text(data=plot.station,        aes(x=Longitude, y=Latitude,label=Station),color='black',size=8,nudge_y = 0.6) +
      scale_fill_manual(values=color_pal) +
      scale_x_continuous(limits=c(min(india.map$long),max(india.map$long)), expand = c(0, 0)) +
      scale_y_continuous(limits=c(min(india.map$lat), max(india.map$lat)),  expand = c(0, 0)) +
      guides(fill=guide_legend(ncol=1, title='CWC basins')) +
      theme_map() +            # clean theme for displaying maps
      theme(text                 = element_text(size=20,family='Helvetica'), 
            legend.key           = element_blank(),
            legend.key.size      = unit(2, 'lines'),
            legend.text          = element_text(size=20,family='Helvetica'),
            legend.direction     ='vertical',
            legend.position      = c(0.5,0.2))
    
    print(p5)
    
    dev.off()
    
  }
}

# ______________________________________________________________________________
# Plot CWC basins in Southern India and locations of selected stations
plot_southern_CWC_basins <- function(filePath, fileName) {
  
  filepath         <- '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/'
  f.name           <- 'CWC_select_stations.txt'
  select.sta.loc   <- read.csv(paste(filepath,f.name,sep=''), header=T, sep='\t')
  
  # Read CWC shapefile
  setwd('~/Documents/repos/india_wq/Shapefiles/Hydro1K/')
  cwc.basins       <- readOGR(dsn=getwd(),layer='hydro1K_as_basins')
  wq.basins        <- readOGR(dsn=getwd(),layer='basin_wq_stations')
  streamlines      <- readOGR(dsn=getwd(),layer='hydro1K_as_streamlines')
  
  # Remove Musiri station basin
  wq.basins        <- wq.basins[which(wq.basins@data$WQ_station != 'Musiri'),]
  select.sta.loc   <- select.sta.loc[which(select.sta.loc$Station != 'Musiri'),]
  
  # Only keep watersheds with CWC basin name
  cwc.basins       <- cwc.basins[which(!is.na(cwc.basins@data$CWC_basin)),]
  streamlines      <- streamlines[which(!is.na(streamlines@data$CWC_basin)),]
  
  # Remove northen basins and smaller east and west flowing basins
  north_basins     <- c('Brahmaputra',
                        'Barak and others',
                        'Ganga',
                        'Indus (Up to border)',
                        'Area of inland drainage in Rajasthan',
                        'Minor rivers draining into Myanmar and Bangladesh',
                        'West flowing rivers of Kutch & Saurashtra in. Luni')
  small_basins     <- c('East flowing rivers between Mahanadi and Pennar',
                        'East flowing rivers between Pennar and Kanyakumari',
                        'West flowing rivers from Tadri to Kanyakumari',
                        'West flowing rivers from Tapi to Tadri')
  cwc.basins       <- cwc.basins[-which(cwc.basins@data$CWC_basin %in% c(north_basins,small_basins)),]
  streamlines      <- streamlines[-which(streamlines@data$CWC_basin %in% c(north_basins,small_basins)),]
  
  # Read shapefile for Indian states
  setwd('~/Documents/repos/india_wq/Shapefiles/GADM/gadm36_IND_shp/')
  india.states     <- readOGR(dsn=getwd(),layer='gadm36_IND_1')
  
  # Modify projection to WGS84
  proj.cwc.basins  <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  cwc.basins       <- spTransform(cwc.basins,   proj.cwc.basins)
  wq.basins        <- spTransform(wq.basins,    proj.cwc.basins)
  streamlines      <- spTransform(streamlines,  proj.cwc.basins)
  india.states     <- spTransform(india.states, proj.cwc.basins)
  
  # Aggregate smaller watersheds within CWC basins
  # https://rud.is/projects/dissolving_polygons.html
  cwc.basins <- unionSpatialPolygons(cwc.basins, cwc.basins@data$CWC_basin)
  
  # Convert back to SpatialPolygonsDataFrame
  cwc.basins <- SpatialPolygonsDataFrame(spTransform(cwc.basins,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')),
                                         data.frame(CWC_basin=names(cwc.basins),
                                                    row.names=names(cwc.basins),
                                                    stringsAsFactors=FALSE))
  
  # CWC basins polygon centroids for placing labels
  cwc.basins.centroids        <- data.frame(coordinates(cwc.basins), CWC_basin=cwc.basins@data$CWC_basin)
  names(cwc.basins.centroids) <- c('long','lat','CWC_basin')
  
  #prepare shapefiles for ploting in ggplot2
  cwc.basins@data$id       <- rownames(cwc.basins@data)
  cwc.basins.points        <- fortify(cwc.basins, region='id')
  cwc.basins               <- merge(cwc.basins.points, cwc.basins@data, by='id')
  
  wq.basins@data$id        <- rownames(wq.basins@data)
  wq.basins.points         <- fortify(wq.basins, region='id')
  wq.basins                <- merge(wq.basins.points, wq.basins@data, by='id')
  
  streamlines@data$id      <- rownames(streamlines@data)
  streamlines.points       <- fortify(streamlines, region='id')
  streamlines              <- merge(streamlines.points, streamlines@data, by='id')
  
  india.states@data$id     <- rownames(india.states@data)
  india.states.points      <- fortify(india.states, region='id')
  india.states             <- merge(india.states.points, india.states@data, by='id')
  
  # India boundary map
  world.map                <- map_data('world')
  india.map                <- world.map[which(world.map$region == 'India'),]
  
  # Breaks for annual precipitation
  breaks                <- c(500, 750, 1000, 1250, 1500, 3000)
  
  color_pal_tmp         <- c('[500,750]'   = '#bfeaf1',
                             '(750,1000]'  = '#7ed6e7',
                             '(1000,1250]' = '#2f99bc',
                             '(1250,1500]' = '#2f5d9e',
                             '(1500,3000]' = '#1d3a63')
  
  # Add range for annual precipitation
  select.sta.loc$range      <- cut(select.sta.loc$Avg_precip_mm_yr, dig.lab=5, breaks=breaks, include.lowest=TRUE)
  
  # Update maximum range and color-pal for maximum range
  max_precip                <- round(max(select.sta.loc$Avg_precip_mm_yr, na.rm=TRUE),0)
  select.sta.loc$range      <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), select.sta.loc$range)
  names(color_pal_tmp)      <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), names(color_pal_tmp))
  
  # Update minimum range and color-pal for minimum range
  min_precip                <- round(min(select.sta.loc$Avg_precip_mm_yr, na.rm=TRUE),0)
  select.sta.loc$range      <- gsub(paste(breaks[max(which(breaks < min_precip))],',',sep=''), paste(min_precip,',',sep=''), select.sta.loc$range)
  names(color_pal_tmp)      <- gsub(paste(breaks[max(which(breaks < min_precip))],',',sep=''), paste(min_precip,',',sep=''), names(color_pal_tmp))
  
  # Reorder factor levels
  select.sta.loc$range      <- factor(select.sta.loc$range, levels=names(color_pal_tmp))
  
  # Only keep streamlines for main rivers
  maj_streamlines       <- na.omit(streamlines)
  
  # ---
  # Change directory for saving plot
  setwd(filePath)
  
  # Delete existing file
  unlink(fileName)
  # Start pdf device driver for saving plots
  png(fileName, width = 700, height = 800) 
  
  p1 <- ggplot() +
    geom_map(data=select.sta.loc[,c('CWC_basin','range')], 
             aes(map_id=CWC_basin, fill=range), map=cwc.basins) +
    geom_line(data=streamlines,     aes(x=long,y=lat,group=group),color='dodgerblue', size=0.3) +
    geom_line(data=maj_streamlines, aes(x=long,y=lat,group=group),color='blueviolet', size=1.2) +
    geom_path(data=cwc.basins,      aes(x=long,y=lat,group=group),color='black',      size=0.75) +
    geom_path(data=india.map,       aes(x=long,y=lat,group=group),color='grey15',     size=1.2) +
    geom_point(data=select.sta.loc, aes(x=Longitude, y=Latitude),fill='red',color='black',shape=21,size=4) +
    geom_text_repel(data=subset(select.sta.loc, !Station %in% c('Mandleshwar','Khanpur','Vautha','Sarangkheda')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = 2.6,
                    size          = 6,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    direction     = 'y',
                    hjust         = 0) +
    geom_text_repel(data=subset(select.sta.loc, Station %in% c('Mandleshwar','Khanpur','Vautha','Sarangkheda')), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = -4.0,
                    nudge_y       = -1.8,
                    size          = 6,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    hjust         = 1) +
    scale_fill_manual(values=color_pal_tmp) +
    scale_x_continuous(limits=c(min(india.map$long),max(india.map$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(india.map$lat), max(india.map$lat)),  expand = c(0, 0)) +
    guides(fill=guide_legend(ncol=1, title=expression(paste(Mean~precipitation,' [',mm~yr^-1,']')))) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=18,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.84,0.25),
          legend.justification = 'center',
          legend.text          = element_text(size=18,family='Helvetica'))
  
  print(p1)
  
  dev.off()
  
  # ---
  final_basins         <- c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi','Godavari','Cauvery','Krishna')
  
  plot.select.sta.loc  <- select.sta.loc[which(select.sta.loc$CWC_basin %in% final_basins),]
  cwc.basins           <- cwc.basins[which(cwc.basins$CWC_basin %in% final_basins),]
  cwc.basins.centroids <- cwc.basins.centroids[which(cwc.basins.centroids$CWC_basin %in% final_basins),]
  streamlines          <- streamlines[which(streamlines$CWC_basin %in% final_basins),]
  
  # Reorder factor levels
  plot.select.sta.loc$CWC_basin   <- factor(plot.select.sta.loc$CWC_basin,  levels=final_basins)
  cwc.basins$CWC_basin            <- factor(cwc.basins$CWC_basin,           levels=final_basins)
  cwc.basins.centroids$CWC_basin  <- factor(cwc.basins.centroids$CWC_basin, levels=final_basins)
  streamlines$CWC_basin           <- factor(streamlines$CWC_basin,          levels=final_basins)
  
  # Only keep streamlines for main rivers
  maj_streamlines       <- na.omit(streamlines)

  # Breaks for annual precipitation
  breaks                <- c(500, 750, 1000, 1250, 1500, 3000)
  
  color_pal_tmp         <- c('[500,750]'   = '#e7f7fa',
                             '(750,1000]'  = '#bfeaf1',  # '#d7f2f6',
                             '(1000,1250]' = '#7ed6e7',  # '#8fdbea',
                             '(1250,1500]' = '#2f99bc',  # '#2f5d9e'
                             '(1500,3000]' = '#2f5d9e')
  
  # color_pal_tmp         <- c('[750,900]'    = '#d7f2f6',
  #                            '(900,1050]'   = '#afe5ed',
  #                            '(1050,1200]'  = '#8fdbea',
  #                            '(1200,1350]'  = '#43add0',
  #                            '(1350,1500]'  = '#2f5d9e')
  
  # Add range for annual precipitation
  plot.select.sta.loc$range <- cut(plot.select.sta.loc$Avg_precip_mm_yr, dig.lab=5, breaks=breaks, include.lowest=TRUE)

  # Update maximum range and color-pal for maximum range
  max_precip                <- round(max(plot.select.sta.loc$Avg_precip_mm_yr),0)
  plot.select.sta.loc$range <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), plot.select.sta.loc$range)
  names(color_pal_tmp)      <- gsub(paste(',',breaks[min(which(breaks > max_precip))],sep=''), paste(',',max_precip,sep=''), names(color_pal_tmp))
  
  # Update minimum range and color-pal for minimum range
  min_precip                <- round(min(plot.select.sta.loc$Avg_precip_mm_yr),0)
  plot.select.sta.loc$range <- gsub(paste(breaks[max(which(breaks < min_precip))],',',sep=''), paste(min_precip,',',sep=''), plot.select.sta.loc$range)
  names(color_pal_tmp)      <- gsub(paste(breaks[max(which(breaks < min_precip))],',',sep=''), paste(min_precip,',',sep=''), names(color_pal_tmp))
  
  # Reorder factor levels
  plot.select.sta.loc$range <- factor(plot.select.sta.loc$range, levels=names(color_pal_tmp))
  
  # ---------- Delete existing file ---
  unlink('CWC_basins_South_India_2.png')
  # Start pdf device driver for saving plots
  png('CWC_basins_South_India_2.png', width = 700, height = 800) 

  p2 <- ggplot() +
    geom_map(data=plot.select.sta.loc[,c('CWC_basin','range')], 
             aes(map_id=CWC_basin, fill=range), map=cwc.basins) +
    geom_line(data=streamlines,     aes(x=long,y=lat,group=group),color='dodgerblue',  size=0.3) +
    geom_line(data=maj_streamlines, aes(x=long,y=lat,group=group),color='blueviolet',  size=1.2) +
    geom_path(data=cwc.basins,      aes(x=long,y=lat,group=group),color='black',       size=0.75) +
    geom_path(data=india.map,       aes(x=long,y=lat,group=group),color='grey15',      size=1.2) +
    geom_point(data=plot.select.sta.loc,   aes(x=Longitude, y=Latitude),fill='red',color='black',shape=21,size=4) +
    geom_text_repel(data=subset(cwc.basins.centroids, CWC_basin %in% c('Godavari','Krishna','Cauvery')), 
                    aes(x=long, y=lat, label=CWC_basin),
                    nudge_x       = 70-subset(cwc.basins.centroids, CWC_basin %in% c('Godavari','Krishna','Cauvery'))$long,
                    nudge_y       = -0.5,
                    size          = 6,
                    segment.size  = 0.75,
                    segment.color = 'black',
                    direction     = 'y',
                    hjust         = 1) +
    geom_text_repel(data=subset(cwc.basins.centroids, CWC_basin %in% c('Subernarekha','Narmada','Mahanadi')), 
                    aes(x=long, y=lat, label=CWC_basin),
                    nudge_x       = -1.5,
                    nudge_y       = 24.5-subset(cwc.basins.centroids, CWC_basin %in% c('Subernarekha','Narmada','Mahanadi'))$lat,
                    size          = 6,
                    segment.size  = 0.75,
                    segment.color = 'black',
                    direction     = 'x',
                    box.padding   = 1.0,
                    vjust         = 0) +
    geom_text_repel(data=subset(cwc.basins.centroids, CWC_basin %in% c('Brahmani and Baitarni')), 
                    aes(x=long, y=lat, label=CWC_basin),
                    nudge_x       = -3.3,
                    nudge_y       = 25.5-subset(cwc.basins.centroids, CWC_basin %in% c('Brahmani and Baitarni'))$lat,
                    size          = 6,
                    segment.size  = 0.75,
                    segment.color = 'black',
                    direction     = 'x',
                    box.padding   = 1.0,
                    vjust         = 0) +
    geom_text_repel(data=subset(plot.select.sta.loc, Station != 'Mandleshwar'), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = 2.6,
                    size          = 6,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    direction     = 'y',
                    hjust         = 0) +
    geom_text_repel(data=subset(plot.select.sta.loc, Station == 'Mandleshwar'), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = -4.0,
                    nudge_y       = -1.8,
                    size          = 6,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    hjust         = 1) +
    scale_fill_manual(values=color_pal_tmp) +
    scale_x_continuous(limits=c(min(india.map$long),max(india.map$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(india.map$lat), max(india.map$lat)),  expand = c(0, 0)) +
    guides(fill=guide_legend(ncol=1, title=expression(paste(Mean~precipitation,' [',mm~yr^-1,']')))) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=18,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.84,0.25),
          legend.justification = 'center',
          legend.text          = element_text(size=18,family='Helvetica'))
  
  print(p2)
  
  dev.off()

  # ---------- Delete existing file ---
  unlink('CWC_basins_South_India_3.png')
  # Start pdf device driver for saving plots
  png('CWC_basins_South_India_3.png', width = 700, height = 800) 
  
  p3 <- ggplot() +
    geom_map(data=plot.select.sta.loc[,c('CWC_basin','range')], 
             aes(map_id=CWC_basin, fill=range), map=cwc.basins) +
    geom_line(data=streamlines,     aes(x=long,y=lat,group=group),color='dodgerblue',  size=0.3) +
    geom_line(data=maj_streamlines, aes(x=long,y=lat,group=group),color='blueviolet',  size=1.2) +
    geom_path(data=cwc.basins,      aes(x=long,y=lat,group=group),color='black',       size=0.75) +
    geom_path(data=india.map,       aes(x=long,y=lat,group=group),color='grey15',      size=1.2) +
    geom_point(data=plot.select.sta.loc,   aes(x=Longitude, y=Latitude),fill='red',color='black',shape=21,size=4) +
    geom_text_repel(data=subset(plot.select.sta.loc, Station != 'Mandleshwar'), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = 2.6,
                    size          = 6,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    direction     = 'y',
                    hjust         = 0) +
    geom_text_repel(data=subset(plot.select.sta.loc, Station == 'Mandleshwar'), 
                    aes(x=Longitude, y=Latitude, label=Station),
                    nudge_x       = -4.0,
                    nudge_y       = -1.8,
                    size          = 6,
                    color         = 'red',
                    segment.size  = 0.75,
                    segment.color = 'red',
                    hjust         = 1) +
    scale_fill_manual(values=color_pal_tmp) +
    scale_x_continuous(limits=c(min(india.map$long),max(india.map$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(india.map$lat), max(india.map$lat)),  expand = c(0, 0)) +
    guides(fill=guide_legend(ncol=1, title=expression(paste(Mean~precipitation,' [',mm~yr^-1,']')))) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=18,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.84,0.25),
          legend.justification = 'center',
          legend.text          = element_text(size=18,family='Helvetica'))
  
  print(p3)
  
  dev.off()
  
}

# ______________________________________________________________________________
# Plot all CWC basins and color by whether basins drain to the east and west
plot_CWC_east_west_basins <- function(filePath, fileName) {
  
  filepath         <- '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/'
  f.name           <- 'CWC_select_stations.txt'
  select.sta.loc   <- read.csv(paste(filepath,f.name,sep=''), header=T, sep='\t')
  
  # Select stations to add to map
  select.sta.loc   <- select.sta.loc[which(select.sta.loc$CWC_basin %in% c('Brahmani and Baitarni','Cauvery',
                                                                         'Godavari','Krishna','Mahanadi',
                                                                         'Narmada','Subernarekha')),]
  
  # Read CWC shapefile
  setwd('~/Documents/repos/india_wq/Shapefiles/Hydroshed/')
  hydroshed.cwc.basins   <- readOGR(dsn=getwd(),layer='select_as_bas_15s_major_CWC_basins')
  
  # Modify projection to WGS84
  proj.cwc.basins      <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  hydroshed.cwc.basins <- spTransform(hydroshed.cwc.basins, proj.cwc.basins)
  
  #prepare shapefiles for ploting in ggplot2
  hydroshed.cwc.basins@data$id <- rownames(hydroshed.cwc.basins@data)
  hydroshed.cwc.basins.points  <- fortify(hydroshed.cwc.basins, region='id')
  hydroshed.cwc.basins         <- merge(hydroshed.cwc.basins.points, hydroshed.cwc.basins@data, by='id')
  
  # India boundary map
  world.map          <- map_data('world')
  india.map          <- world.map[which(world.map$region == 'India'),]
  
  # ---
  # Change directory for saving plot
  setwd(filePath)
  
  # Delete existing file
  unlink(fileName)
  # Start pdf device driver for saving plots
  png(fileName, width = 1000, height = 1200) 
  
  p1 <- ggplot() +
    geom_polygon(data=hydroshed.cwc.basins, aes(x=long,y=lat,group=group, fill=Flow_dir),color='black') +
    geom_path(data=india.map,               aes(x=long,y=lat,group=group),color='grey15',size=1.2) +
    geom_point(data=select.sta.loc,         aes(x=Longitude, y=Latitude),fill='red',color='black',shape=21,size=6) +
    geom_text(data=select.sta.loc,          aes(x=Longitude, y=Latitude,label=Station),color='black',size=8, fontface='bold', nudge_x=0.25, nudge_y=0.25) +
    scale_fill_manual(name='', values=c('East flowing basins'='#d1fdcc','West flowing basins'='#ffe2ff')) +
    scale_x_continuous(limits=c(min(india.map$long),max(india.map$long)), expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(india.map$lat), max(india.map$lat)),  expand = c(0, 0)) +
    guides(fill=guide_legend(ncol=1)) +
    theme_map() +            # clean theme for displaying maps
    theme(text                 = element_text(size=30,family='Helvetica'), 
          legend.key           = element_blank(),
          legend.key.size      = unit(2, 'lines'),
          legend.position      = c(0.8,0.4),
          legend.justification = 'center',
          legend.text          = element_text(size=25,family='Helvetica'))
  
  print(p1)
  
  dev.off()

}

# ______________________________________________________________________________
# Plot CWC basins and locations of selected stations
plot_select_CWC_basins <- function(filePath) {
  
  filepath         <- '~/Documents/repos/india_wq/India_WRIS/WQ_ShinyApp/'
  f.name           <- 'CWC_stations_for_smooth_curves.txt'
  # f.name           <- 'CWC_stations_lat_long_discharge_nutrient_count.txt'
  select.sta.loc   <- read.csv(paste(filepath,f.name,sep=''), header=T, sep='\t')
  
  # Read CWC shapefile
  setwd('~/Documents/repos/india_wq/Shapefiles/Hydro1K/')
  cwc.basins       <- readOGR(dsn=getwd(),layer='hydro1K_as_basins')
  wq.basins        <- readOGR(dsn=getwd(),layer='basin_wq_stations')
  streamlines      <- readOGR(dsn=getwd(),layer='hydro1K_as_streamlines')
  
  # Only keep watersheds with CWC basin name
  cwc.basins       <- cwc.basins[which(!is.na(cwc.basins@data$CWC_basin)),]
  streamlines      <- streamlines[which(!is.na(streamlines@data$CWC_basin)),]
  
  # Modify projection to WGS84
  proj.cwc.basins  <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  cwc.basins       <- spTransform(cwc.basins,  proj.cwc.basins)
  wq.basins        <- spTransform(wq.basins,   proj.cwc.basins)
  streamlines      <- spTransform(streamlines, proj.cwc.basins)
  
  #prepare shapefiles for ploting in ggplot2
  cwc.basins@data$id       <- rownames(cwc.basins@data)
  cwc.basins.points        <- fortify(cwc.basins, region='id')
  cwc.basins               <- merge(cwc.basins.points, cwc.basins@data, by='id')
  
  wq.basins@data$id        <- rownames(wq.basins@data)
  wq.basins.points         <- fortify(wq.basins, region='id')
  wq.basins                <- merge(wq.basins.points, wq.basins@data, by='id')
  
  streamlines@data$id      <- rownames(streamlines@data)
  streamlines.points       <- fortify(streamlines, region='id')
  streamlines              <- merge(streamlines.points, streamlines@data, by='id')
  
  # Change directory for saving plot
  setwd(filePath)
  
  # ------- Make individual plot for each station ---
  uniq_basins   <- unique(select.sta.loc$CWC_basin)
  # Only keep select basins for plotting
  uniq_basins   <- uniq_basins[which(uniq_basins %in% c('Brahmani and Baitarni','Cauvery',
                                                        'Godavari','Krishna','Mahanadi',
                                                        'Narmada','Pennar','Sabarmati','Subernarekha'))]
  
  for (ind in 1:length(uniq_basins)){
    
    plot.station        <- select.sta.loc[which(as.character(select.sta.loc$CWC_basin) == uniq_basins[ind]), ]
    plot.cwc.basins     <- cwc.basins[which(cwc.basins$CWC_basin   == as.character(uniq_basins[ind])),]
    plot.wq.basins      <- wq.basins[which(wq.basins$CWC_basin     == as.character(uniq_basins[ind])),]    
    plot.streamlines    <- streamlines[which(streamlines$CWC_basin == as.character(uniq_basins[ind])),]
    
    # Delete existing file
    fileName  <- paste(uniq_basins[ind], '.png', sep='')
    
    unlink(fileName)
    # Start pdf device driver for saving plots
    png(fileName, width = 800, height = 800) 
    
    p1 <- ggplot() +
      geom_polygon(data=plot.cwc.basins,  aes(x=long,y=lat,fill=CWC_basin,color='Hydro1K basins',group=group), size=0.5) +
      geom_polygon(data=plot.wq.basins,   aes(x=long,y=lat,color='Watershed upstream of station',group=group),fill='transparent',size=1.1) +
      geom_line(data=plot.streamlines,    aes(x=long,y=lat,color='Hydro1K Streamlines',group=group),size=0.75) +
      geom_point(data=plot.station,       aes(x=Longitude, y=Latitude),fill='green',color='black',shape=21,size=6) +
      geom_text(data=plot.station,        aes(x=Longitude, y=Latitude,label=Station),color='red',size=7,nudge_x=0.1,nudge_y=0.05, fontface='bold') +
      scale_fill_manual(values=color_pal) +
      scale_color_manual(name='',values=c('Hydro1K basins'='grey50','Watershed upstream of station'='black','Hydro1K Streamlines'='blue')) +
      scale_x_continuous(limits=c(min(plot.cwc.basins$long),max(plot.cwc.basins$long)), expand = c(0, 0)) +
      scale_y_continuous(limits=c(min(plot.cwc.basins$lat), max(plot.cwc.basins$lat)),  expand = c(0, 0)) +
      guides(fill=FALSE, color=guide_legend(title=NULL, nrow=2, override.aes=list(fill=NA))) + # Override.aes for cleaning legends
      labs(title = uniq_basins[ind]) + 
      coord_map() +            # Mercator projection
      theme_map() +            # clean theme for displaying maps
      theme(text                 = element_text(size=30,family='Helvetica'), 
            legend.key           = element_rect(color=NA, fill=NA),
            legend.background    = element_blank(), # no legend background
            legend.key.size      = unit(2, 'lines'),
            legend.text          = element_text(size=25,family='Helvetica'),
            legend.position      = 'bottom',
            legend.justification = 'center')
    
    print(p1)
    
    dev.off()
    
  }
}
# ______________________________________________________________________________
# Plot CWC basins and locations of selected stations
plot_CWC_basins(filePath = '~/Documents/repos/india_wq/Figures/CWC_basins/',
                fileName = 'CWC_basins.png')

# # Plot CWC basins in Southern India and locations of selected stations
# plot_southern_CWC_basins(filePath = '~/Documents/repos/india_wq/Figures/CWC_basins/',
#                          fileName = 'CWC_basins_South_India.png')

# # Plot all CWC basins and color by whether basins drain to the east and west
# plot_CWC_east_west_basins(filePath = '~/Documents/repos/india_wq/Figures/CWC_basins/',
#                           fileName = 'CWC_east_west_basins.png')
# 
# # Plot CWC basins and locations of selected stations
# plot_select_CWC_basins(filePath = '~/Documents/repos/india_wq/Figures/CWC_basins/')
