# Author - Eva Sinha, Stanford University, esinha@stanford.edu
# Date   - 16th May, 2018
#
# Function details
# param_labeller                              - Long labels for basin charateristics
# compile_basin_charac_wq_stations            - Compile basin characteristics data
# plot_DIN_flux_vs_basin_charac_wq_stations   - Make plots of comparing DIN flux to basin characteristics such as
#                                               annual precip, storage capacity, and fertilizer usage rate
# plot_DIN_flux_vs_basin_charac_wq_stations_2 - Make plots of comparing DIN flux to basin characteristics such as
#                                               annual precip, storage capacity, and fertilizer usage rate
# plot_DIN_flux_vs_precip_fert                - Make plots of comparing DIN flux to basin characteristics such as
#                                               annual precip and fertilizer usage rate

source('~/Documents/repos/india_wq/plot_annual_timeseries.R')
source('~/Documents/repos/india_wq/read_storage_capacity.R')
source('~/Documents/repos/india_wq/read_fert_EandS.R')
source('~/Documents/repos/india_wq/india_color_pal.R')

#_______________________________________________________________________________
# Long labels for basin charateristics
param_labeller <- function(var, value, LSC_variable_label){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='fert_kgN_km2']         <- 'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)'  
    value[value=='LSC_MCM_mm']           <- 'Live~storage~capacity~(MCM~mm^-1)'
    value[value=='precip']               <- 'Total~annual~precipitation~(mm~yr^-1)'
    value[value=='precip_JJAS']          <- 'JJAS~precipitation~(mm~yr^-1)'
    value[value=='Discharge_m3s']        <- 'Annual~discharge~(m^3~sec^-1)'
    value[value=='Disch_mm_yr']          <- 'Annual~discharge~(mm~yr^-1)'
    value[value=='Discharge_m3s_JJAS']   <- 'JJAS~discharge~(m^3~sec^-1)'
    value[value=='Disch_mm_yr_JJAS']     <- 'JJAS~discharge~(mm~yr^-1)'
    value[value=='LSC_var']              <- LSC_variable_label
    
    value[value=='mean_fert']            <- 'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)'  
    value[value=='mean_precip']          <- 'Total~annual~precipitation~(mm~yr^-1)'
    value[value=='mean_precip_JJAS']     <- 'JJAS~precipitation~(mm~yr^-1)'
    value[value=='mean_disch']           <- 'Annual~discharge~(mm~yr^-1)' # 'Annual~discharge~(m^3~sec^-1)'
    value[value=='mean_disch_JJAS']      <- 'JJAS~discharge~(mm~yr^-1)'   # 'JJAS~discharge~(m^3~sec^-1)'
  }
  
  return(value)
}

#_______________________________________________________________________________
# Long labels for basin charateristics
param_labeller_2 <- function(var, value, LSC_variable_label){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='fert_kgN_km2']         <- 'italic(N)[Fert]~(kg~N~km^-2~yr^-1)'  
    value[value=='LSC_MCM_mm']           <- 'italic(LSC)~(MCM~mm^-1)'
    value[value=='precip']               <- 'italic(P)[Annual]~(mm~yr^-1)'
    value[value=='precip_JJAS']          <- 'italic(P)[JJAS]~(mm~yr^-1)'
    value[value=='Discharge_m3s']        <- 'italic(Q)[Annual]~(m^3~sec^-1)'
    value[value=='Disch_mm_yr']          <- 'italic(Q)[Annual]~(mm~yr^-1)'
    value[value=='Discharge_m3s_JJAS']   <- 'italic(Q)[JJAS]~(m^3~sec^-1)'
    value[value=='Disch_mm_yr_JJAS']     <- 'italic(Q)[JJAS]~(mm~yr^-1)'
    value[value=='LSC_var']              <- LSC_variable_label
    
    value[value=='mean_fert']            <- 'italic(N)[Fert]~(kg~N~km^-2~yr^-1)'  
    value[value=='mean_precip']          <- 'italic(P)[Annual]~(mm~yr^-1)'
    value[value=='mean_precip_JJAS']     <- 'italic(P)[JJAS]~(mm~yr^-1)'
    value[value=='mean_disch']           <- 'italic(Q)[Annual]~(mm~yr^-1)' # 'Annual~discharge~(m^3~sec^-1)'
    value[value=='mean_disch_JJAS']      <- 'italic(Q)[JJAS]~(mm~yr^-1)'   # 'JJAS~discharge~(m^3~sec^-1)'
  }
  
  return(value)
}
#_______________________________________________________________________________
# Compile basin characteristics data
compile_basin_charac_wq_stations <- function(){
  filepath              <- '~/Documents/repos/india_wq/'
  
  # Read station information
  wq_stations           <- read.csv(paste(filepath,'WQ_stations_DIN_model.txt',sep=''),header=T,sep='\t')
  
  # Remove Musiri from station list since WRTDS could not be applied to obtain annual DIN flux
  wq_stations           <- wq_stations[-which(wq_stations$Station == 'Musiri'), ]
  
  # Convert to character
  wq_stations$Station   <- as.character(wq_stations$Station)
  
  # ---------- Read fertilizer usage rate for various river basins in India [kg/km2/yr]
  # Source: Directorate of Economics and Statistics Department of Agriculture, Cooperation and Farmers Welfare
  fertl_basins            <- read_EandS_fert_India_river_basins()
  fertl_basins$Desc       <- NULL
  fertl_basins$scenario   <- NULL
  # Only keep values after 1980
  fertl_basins      <- fertl_basins[which(fertl_basins$Year >= 1980), ]
  
  # Interpolate annual fertilizer values for yrs falling between the 1980 and 1985
  fertl_basins      <- interpolate_fert(fertl_basins)
  
  # Rename column
  colnames(fertl_basins)[which(colnames(fertl_basins) == 'Basin')] <- 'CWC_basin' 
  
  # Add station names
  fertl_basins      <- merge(fertl_basins, wq_stations[, c('Station','CWC_basin')])
  
  #  # ---------- Estimate average fertilizer usage across years
  #  fertl_basins      <- group_by(fertl_basins, CWC_basin, Station) %>%
  #                       summarise(fert_kgN_km2 = mean(fert_kgN_km2, na.rm=TRUE))
  
  # # ---------- Read annual and monthly precipitation based on CPC data
  # precip          <- read_basin_annual_precip(in.folder = '~/Documents/repos/india_wq/daily_precip/CPC/Basin_wq_stations/',
  #                                             filename  = 'WQ_Station_Basin_CPC_precip_daily.precip',
  #                                             out_fname = 'WQ_Station_CPC_average_annual_precip.txt',
  #                                             min_yr    = 1980,
  #                                             max_yr    = 2015)
  
  # ---------- Read annual and monthly precipitation based on IMD data
  precip          <- read_basin_annual_precip(in.folder = '~/Documents/repos/india_wq/daily_precip/IMD/0.25deg/Basin_wq_stations/',
                                              filename  = 'WQ_Station_Basin_IMD_precip_daily.precip',
                                              out_fname = 'WQ_Station_IMD_average_annual_precip.txt',
                                              min_yr    = 1980,
                                              max_yr    = 2015)
  
  precip_annual   <- precip[['Annual']]
  precip_monthly  <- precip[['Monthly']]
  
  # ---------- Read total annual discharge for various CWC basins based on Hydro observation station data
  hydro.mean          <- read_hydro_sta_basin_annual_discharge(in.folder = '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/',
                                                               wq_stations, 
                                                               min_yr    = 1980,
                                                               max_yr    = 2015)
  
  hydro.annual.tot    <- hydro.mean[['Annual']]
  hydro.monthly.tot   <- hydro.mean[['Monthly']]
  
  # Merge to get drainage area
  hydro.annual.tot    <- merge(hydro.annual.tot,  wq_stations[,c('Station','Drainage_area_km2')])
  
  # Convert discharge from m3/sec to mm/year by dividing by drainage area and converting unit for time
  # Since the discharge value in m3/sec was summed for all days in the year, we don't have to multiply by number of days in the year
  hydro.annual.tot$Disch_mm_yr      <- hydro.annual.tot$Discharge_m3s*(60*60*24)*1000/(hydro.annual.tot$Drainage_area_km2*(10^6))
  hydro.annual.tot$Disch_mm_yr_JJAS <- hydro.annual.tot$Discharge_m3s_JJAS*(60*60*24)*1000/(hydro.annual.tot$Drainage_area_km2*(10^6))

  # Only keep select columns
  hydro.annual.tot               <- hydro.annual.tot[,c('Year','Station','CWC_basin','Disch_mm_yr','Disch_mm_yr_JJAS')]
  
  
  
  # Read Gross Storage Capacity information and estimate GSC per unit area and per unit annual precipitation
  gsc_basins      <- read_gross_storage_capacity(wq_stations, precip_annual[,c('Year','CWC_basin','Station','precip')])
  
  # Read total Live Storage Capacity information and estimate LSC per unit area and per unit annual precipitation
  lsc_basins      <- read_live_storage_capacity(wq_stations, precip_annual[,c('Year','CWC_basin','Station','precip')])
  
  in.folder       <- '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/'
  f.name          <- 'CWC_select_stations.txt'
  select.sta.loc  <- read.csv(paste(in.folder,f.name,sep=''), header=T, sep='\t')
  
  # Read total annual DIN load [kg N/km2/day] for various CWC basins based on WRTDS model application
  DIN.flux        <- read_annual_DIN_flux(in.folder='~/Documents/repos/india_wq/EGRET/WRTDS_analysis/', 
                                          select.sta.loc)
  # Merge data frames
  plot_data       <- reduce(list(precip_annual, hydro.annual.tot, DIN.flux, fertl_basins, gsc_basins, lsc_basins), 
                            left_join, by = c('Year', 'Station', 'CWC_basin'))
  
  print(head(as.data.frame(plot_data)))
  print(table(plot_data$CWC_basin))
  
  # Remove rows with NA values for DIN flux
  # This will ensure we use only years with DIN flux values for precip and fertilizer distribution. 
  # Did this to ensure that ellipsoid center matches with the mean of precip
  plot_data       <- na.omit(plot_data)
  
  #plot_data$fert_kgN_km2 <- round(plot_data$fert_kgN_km2, 0)
  #plot_data$fert_kgN_km2 <- as.factor(plot_data$fert_kgN_km2)
  plot_data$GSC_MCM_mm        <- round(plot_data$GSC_MCM_mm, 0)
  plot_data$GSC_MCM_mm        <- as.factor(plot_data$GSC_MCM_mm)
  
  plot_data$LSC_MCM_mm        <- round(plot_data$LSC_MCM_mm, 0)
  plot_data$LSC_MCM_mm        <- as.factor(plot_data$LSC_MCM_mm)
  
  # Only keep select stations
  plot_data              <- plot_data[which(plot_data$Station %in% wq_stations$Station),]
  
  # Reorder factor levels
  # plot_data$Station      <- factor(plot_data$Station, levels=c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
  #                                                              'Polavaram','Urachikottai','Vijayawada'))
  # plot_data$CWC_basin    <- factor(plot_data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
  #                                                                'Godavari','Cauvery','Krishna'))
  plot_data$Station      <- factor(plot_data$Station, levels=c('Vijayawada','Urachikottai','Polavaram',
                                                               'Tikarapara','Mandleshwar','Ghatsila','Jenapur'))
  plot_data$CWC_basin    <- factor(plot_data$CWC_basin, levels=c('Krishna','Cauvery','Godavari',
                                                                 'Mahanadi','Narmada','Subernarekha','Brahmani and Baitarni'))
  
  # fert_GSC              <- reduce(list(fertl_basins, gsc_basins), left_join, by = c('Station', 'CWC_basin'))
  # fert_GSC$CWC_basin    <- NULL
  # fert_GSC$fert_kgN_km2 <- format(round(fert_GSC$fert_kgN_km2, 0), big.mark=',')
  # fert_GSC$GSC_MCM      <- format(round(fert_GSC$GSC_MCM, 0), big.mark=',')
  # 
  # # Reorder factor levels
  # fert_GSC$Station      <- factor(fert_GSC$Station, levels=c('Ghatsila','Jenapur','Tikarapara','Polavaram',
  #                                                            'Vijayawada','Urachikottai','Mandleshwar'))
  # 
  # # Sort table by factors
  # fert_GSC              <- fert_GSC[order(fert_GSC$Station),]
  
  # Estimate mean, min, and max for DIN flux and basin characteristics for each Station
  sum_plot_data      <- group_by(plot_data, CWC_basin, Station, GSC_MCM, GSC_km, GSC_MCM_mm, GSC, LSC_MCM, LSC_km, LSC_MCM_mm, LSC) %>%
                        summarise(mean_precip      = mean(precip,             na.rm=TRUE),
                                  mean_precip_JJAS = mean(precip_JJAS,        na.rm=TRUE),
                                  # mean_disch       = mean(Discharge_m3s,      na.rm=TRUE),
                                  # mean_disch_JJAS  = mean(Discharge_m3s_JJAS, na.rm=TRUE),
                                  mean_disch       = mean(Disch_mm_yr,      na.rm=TRUE),
                                  mean_disch_JJAS  = mean(Disch_mm_yr_JJAS, na.rm=TRUE),
                                  mean_DIN         = mean(WRTDS_DIN_flux,     na.rm=TRUE),
                                  sd_DIN           = sd(WRTDS_DIN_flux,       na.rm=TRUE),
                                  mean_fert        = mean(fert_kgN_km2,       na.rm=TRUE))
  
  print(as.data.frame(sum_plot_data))
  
  return(list(plot_data, sum_plot_data))
  
}

#_______________________________________________________________________________
# Make plots of comparing DIN flux to basin characteristics such as
# annual precip, storage capacity, and fertilizer usage rate
plot_DIN_flux_vs_basin_charac_wq_stations <- function(plot_data, sum_plot_data){
  
  # Define path to output file
  out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/DIN_flux_vs_basin_characteristics_WQ_stations.pdf',sep='')
  
  # Delete existing file
  unlink(out.f.name)
  # Start pdf device driver for saving plots
  pdf(out.f.name, height=8.5,width=11)
  
  print(paste('R2 between DIN flux and precip', signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ precip))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and fert rate', signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ fert_kgN_km2))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and total live storage capacity (MCM)', signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ as.numeric(as.character(LSC_MCM))))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and total live storage capacity per unit area (km)', signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ as.numeric(as.character(LSC_km))))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and total live storage capacity per unit precip (MCM mm-1)', signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ as.numeric(as.character(LSC_MCM_mm))))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and total live storage capacity per unit area and per unit precip (unitless)', signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ as.numeric(as.character(LSC))))$r.squared,2), sep=' '))
  
  print(paste('R2 between average DIN flux and average precip', signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_precip))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and average fert rate', signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_fert))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and gross storage capacity (MCM)', signif(summary(lm(data=sum_plot_data, mean_DIN ~ as.numeric(as.character(GSC_MCM))))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and gross storage capacity per unit area (km)', signif(summary(lm(data=sum_plot_data, mean_DIN ~ as.numeric(as.character(GSC_km))))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and gross storage capacity per unit precip (MCM mm-1)', signif(summary(lm(data=sum_plot_data, mean_DIN ~ as.numeric(as.character(GSC_MCM_mm))))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and gross storage capacity per unit area and per unit precip (unitless)', signif(summary(lm(data=sum_plot_data, mean_DIN ~ as.numeric(as.character(GSC))))$r.squared,2), sep=' '))

  print(paste('R2 between average DIN flux and total live storage capacity (MCM)', signif(summary(lm(data=sum_plot_data, mean_DIN ~ as.numeric(as.character(LSC_MCM))))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and total live storage capacity per unit area (km)', signif(summary(lm(data=sum_plot_data, mean_DIN ~ as.numeric(as.character(LSC_km))))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and total live storage capacity per unit precip (MCM mm-1)', signif(summary(lm(data=sum_plot_data, mean_DIN ~ as.numeric(as.character(LSC_MCM_mm))))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and total live storage capacity per unit area and per unit precip (unitless)', signif(summary(lm(data=sum_plot_data, mean_DIN ~ as.numeric(as.character(LSC))))$r.squared,2), sep=' '))
  
  
  # ---------- Making plot showing ellipsoid in both directions (annual DIN load vs. annual precip) ---------- 
  
  # Center of ellipse when using normal distribution
  # Note that this method and stat_ellipse ONLY USE VALUES WITH NON NA for both x and y columns for estimating center
  print(stats::cov.wt(na.omit(plot_data[which(plot_data$Station == 'Jenapur'),     c('precip','WRTDS_DIN_flux')]))$center)
  print(stats::cov.wt(na.omit(plot_data[which(plot_data$Station == 'Ghatsila'),    c('precip','WRTDS_DIN_flux')]))$center)
  print(stats::cov.wt(na.omit(plot_data[which(plot_data$Station == 'Mandleshwar'), c('precip','WRTDS_DIN_flux')]))$center)
  print(stats::cov.wt(na.omit(plot_data[which(plot_data$Station == 'Tikarapara'),  c('precip','WRTDS_DIN_flux')]))$center)
  print(stats::cov.wt(na.omit(plot_data[which(plot_data$Station == 'Polavaram'),   c('precip','WRTDS_DIN_flux')]))$center)
  print(stats::cov.wt(na.omit(plot_data[which(plot_data$Station == 'Urachikottai'),c('precip','WRTDS_DIN_flux')]))$center)
  print(stats::cov.wt(na.omit(plot_data[which(plot_data$Station == 'Vijayawada'),  c('precip','WRTDS_DIN_flux')]))$center)
  
  p1 <- ggplot(data=plot_data, aes(x=precip, y=WRTDS_DIN_flux, color=CWC_basin)) + 
    geom_point(alpha=0.25, shape=16, size=3) +
    stat_ellipse(level=0.68, type='norm', lwd=1.2, show.legend=FALSE) +  # level=0.68 corresponds to approximately 1 sigma
    geom_point(data=sum_plot_data, aes(x=mean_precip, y=mean_DIN, color=CWC_basin), shape=0, size=7, stroke=1.2) +
    geom_point(data=sum_plot_data, aes(x=mean_precip, y=mean_DIN, size=mean_fert, fill=LSC_MCM_mm), color='transparent', shape=21, stroke=1.2) +
    scale_color_manual(values=color_pal) +
    scale_fill_manual(values = c('grey70','grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10')) +
    guides(color=guide_legend(order=0, title='CWC Basins'),
           fill =guide_legend(order=1, title=expression(paste(Live~sto~cap~'[',MCM~mm^-1,']')),     override.aes=list(size=7)),
           size =guide_legend(order=2, title=expression(paste(Annual~fertilizer~application~'[',kg~N~km^-2~yr^-1,']')), override.aes=list(color='black'))) +
    labs(x    = expression(paste(Annual~precipitation~'[',mm~yr^-1,']')),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),  
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          legend.position   = 'right',
          legend.key.size   = unit(1.5, 'lines'),
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p1)
  
  p2 <- ggplot(data=plot_data, aes(x=precip, y=WRTDS_DIN_flux, color=CWC_basin)) + 
    stat_ellipse(level=0.68, type='norm', lwd=1.2, show.legend=FALSE) +  # level=0.68 corresponds to approximately 1 sigma
    geom_point(data=sum_plot_data, aes(x=mean_precip, y=mean_DIN, color=CWC_basin), shape=0, size=7, stroke=1.2) +
    geom_point(data=sum_plot_data, aes(x=mean_precip, y=mean_DIN, size=mean_fert, fill=LSC_MCM_mm), color='transparent', shape=21, stroke=1.2) +
    scale_color_manual(values=color_pal) +
    scale_fill_manual(values = c('grey70','grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10')) +
    guides(color=guide_legend(order=0, title='CWC Basins'),
           fill =guide_legend(order=1, title=expression(paste(Live~sto~cap~'[',MCM~mm^-1,']')),     override.aes=list(size=7)),
           size =guide_legend(order=2, title=expression(paste(Annual~fertilizer~application~'[',kg~N~km^-2~yr^-1,']')), override.aes=list(color='black'))) +
    labs(x    = expression(paste(Annual~precipitation~'[',mm~yr^-1,']')),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),  
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          legend.position   = 'right',
          legend.key.size   = unit(1.5, 'lines'),
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p2)
  
  # ---------- Making plot showing histogram value in both directions (annual DIN load vs. annual precip) ----------
  
  # library(boxplotdbl)
  # boxplotdou(precip~Station, plot_data, WRTDS_DIN_flux~Station, plot_data)
  
  # https://stackoverflow.com/questions/46068074/double-box-plots-in-ggplot2
  plot.x           <- layer_data(ggplot(data=plot_data) + geom_boxplot(aes(x=CWC_basin, y=precip)))[,1:6]
  plot.y           <- layer_data(ggplot(data=plot_data) + geom_boxplot(aes(x=CWC_basin, y=WRTDS_DIN_flux)))[,1:6]
  colnames(plot.x) <- paste0('x.', gsub('y', '', colnames(plot.x)))
  colnames(plot.y) <- paste0('y.', gsub('y', '', colnames(plot.y)))
  df               <- cbind(plot.x, plot.y)
  rm(plot.x, plot.y)
  df$CWC_basin     <- sort(unique(plot_data$CWC_basin))
  
  df.outliers      <- df %>%
                      select(CWC_basin, x.middle, x.outliers, y.middle, y.outliers) %>%
                      data.table::data.table()
  df.outliers      <- df.outliers[, list(x.outliers=unlist(x.outliers), y.outliers=unlist(y.outliers)), 
                                  by = list(CWC_basin, x.middle, y.middle)]
  
  # Add value for errorbar width 
  bar_width          <- 0.05*ceiling(max(sum_plot_data$mean_precip, sum_plot_data$mean_DIN))
  
  p3 <- ggplot(data=df) +
    geom_errorbarh(aes(x=x.middle, y=y.middle,   xmin=x.lower, xmax=x.upper,  color=CWC_basin), height=bar_width, lwd=0.75) +  # lower and upper end of box plot in x-axis dimension
    geom_errorbar(aes( x=x.middle, ymin=y.lower, ymax=y.upper,                color=CWC_basin), width=bar_width,  lwd=0.75) +  # lower and upper end of box plot in y-axis dimension
    geom_segment(aes(x=x.min,      y=y.middle,   xend=x.max,   yend=y.middle, color=CWC_basin)) +                              # min and max whiskers for x-axis dimension
    geom_segment(aes(x=x.middle,   y=y.min,      xend=x.middle,yend=y.max,    color=CWC_basin)) +                              # min and max whiskers for y-axis dimension
    geom_point(data=df.outliers, aes(x=x.outliers, y=y.middle,                color=CWC_basin), shape=19) +                    # outiers in x-direction
    geom_point(data=df.outliers, aes(x=x.middle,   y=y.outliers,              color=CWC_basin), shape=19) +                    # outiers in y-direction
    # geom_point(data=sum_plot_data, aes(x=mean_precip, y=mean_DIN, color=CWC_basin, size=mean_fert, fill=GSC_MCM_mm), shape=21, stroke=1.2) +
    geom_point(data=sum_plot_data, aes(x=mean_precip, y=mean_DIN, color=CWC_basin, size=mean_fert, fill=LSC_MCM_mm), shape=21, stroke=1.2) +
    scale_color_manual(values=color_pal) +
    scale_fill_manual(values = c('grey70','grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10')) +
    guides(color=guide_legend(order=0, title='CWC Basins', override.aes=list(lwd=2, shape=NA)),
           # fill =guide_legend(order=1, title=expression(paste(Gross~sto~cap~'[',MCM~mm^-1,']')), override.aes=list(size=7)),
           fill =guide_legend(order=1, title=expression(paste(Live~sto~cap~'[',MCM~mm^-1,']')), override.aes=list(size=7)),
           size =guide_legend(order=2, title=expression(paste(Annual~fertilizer~application~'[',kg~N~km^-2~yr^-1,']')))) +
    labs(x    = expression(paste(Annual~precipitation~'[',mm~yr^-1,']')),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          legend.position   = 'right',
          legend.key.size   = unit(1.5, 'lines'),
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p3)
  
  # ---------- Making plot showing histogram value in both directions (annual DIN load vs. annual precip) ---------- 
  p4 <- ggplot(data=df) +
    geom_rect(aes(xmin=x.lower, xmax=x.upper, ymin=(y.middle-bar_width/4), ymax=(y.middle+bar_width/4), color=CWC_basin), fill=NA, show.legend=FALSE) + # lower and upper end of box plot in x-axis dimension
    geom_rect(aes(xmin=(x.middle-bar_width/4), xmax=(x.middle+bar_width/4), ymin=y.lower, ymax=y.upper, color=CWC_basin), fill=NA, show.legend=FALSE) + # lower and upper end of box plot in x-axis dimension
    # geom_segment(aes(x=x.min,      y=y.middle,   xend=x.max,   yend=y.middle, color=CWC_basin)) +             # min and max whiskers for x-axis dimension
    # geom_segment(aes(x=x.middle,   y=y.min,      xend=x.middle,yend=y.max,    color=CWC_basin)) +             # min and max whiskers for y-axis dimension
    geom_segment(aes(x=x.min,      y=y.middle,   xend=x.lower,  yend=y.middle, color=CWC_basin)) +             # min whiskers for x-axis dimension
    geom_segment(aes(x=x.upper,    y=y.middle,   xend=x.max,    yend=y.middle, color=CWC_basin)) +             # max whiskers for x-axis dimension
    geom_segment(aes(x=x.middle,   y=y.min,      xend=x.middle, yend=y.lower,  color=CWC_basin)) +             # min whiskers for y-axis dimension
    geom_segment(aes(x=x.middle,   y=y.upper,    xend=x.middle, yend=y.max,    color=CWC_basin)) +             # max whiskers for y-axis dimension
    geom_point(data=df.outliers, aes(x=x.outliers, y=y.middle,                color=CWC_basin), shape=19) +    # outiers in x-direction
    geom_point(data=df.outliers, aes(x=x.middle,   y=y.outliers,              color=CWC_basin), shape=19) +    # outiers in y-direction
    # geom_point(data=sum_plot_data, aes(x=mean_precip, y=mean_DIN, color=CWC_basin, size=mean_fert, fill=GSC_MCM_mm), shape=21, stroke=1.2) +
    geom_point(data=sum_plot_data, aes(x=mean_precip, y=mean_DIN, color=CWC_basin, size=mean_fert, fill=LSC_MCM_mm), shape=21, stroke=1.2) +
    scale_color_manual(values=color_pal) +
    scale_fill_manual(values = c('grey70','grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10')) +
    guides(color=guide_legend(order=0, title='CWC Basins', override.aes=list(shape=NA, lwd=1.25)),
           # fill =guide_legend(order=1, title=expression(paste(Gross~sto~cap~'[',MCM~mm^-1,']')), override.aes=list(size=7)),
           fill =guide_legend(order=1, title=expression(paste(Live~sto~cap~'[',MCM~mm^-1,']')), override.aes=list(size=7)),
           size =guide_legend(order=2, title=expression(paste(Annual~fertilizer~application~'[',kg~N~km^-2~yr^-1,']')))) +
    labs(x    = expression(paste(Annual~precipitation~'[',mm~yr^-1,']')),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),  
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          legend.position   = 'right',
          legend.key.size   = unit(1.5, 'lines'),
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p4)
    
  # ---------- Making plot showing ellipsoid in both directions (annual DIN load vs. annual fertilizer usage rate) ---------- 
  
  p5 <- ggplot(data=plot_data, aes(x=fert_kgN_km2, y=WRTDS_DIN_flux, color=CWC_basin)) + 
    geom_point(alpha=0.25, shape=16, size=3) +
    stat_ellipse(level=0.68, type='norm', lwd=1.2, show.legend=FALSE) +  # level=0.68 corresponds to approximately 1 sigma
    geom_point(data=sum_plot_data, aes(x=mean_fert, y=mean_DIN, color=CWC_basin), shape=0, size=7, stroke=1.2) +
    geom_point(data=sum_plot_data, aes(x=mean_fert, y=mean_DIN, size=mean_precip, fill=LSC_MCM_mm), color='transparent', shape=21, stroke=1.2) +
    scale_color_manual(values=color_pal) +
    scale_fill_manual(values = c('grey70','grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10')) +
    guides(color=guide_legend(order=0, title='CWC Basins'),
           fill =guide_legend(order=1, title=expression(paste(Live~sto~cap~'[',MCM~mm^-1,']')), override.aes=list(size=7)),
           size =guide_legend(order=2, title=expression(paste(Annual~precip~'[',mm~yr^-1,']')), override.aes=list(color='black'))) +
    labs(x    = expression(paste(Annual~fertilizer~application~'[',kg~N~km^-2~yr^-1,']')),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),  
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          legend.position   = 'right',
          legend.key.size   = unit(1.5, 'lines'),
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p5)
  
  # # https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
  # # Summary table plot
  # table_fert_GSC        <- tableGrob(fert_GSC, rows=NULL, cols=c('CWC_basin','italic(Fert)~(kg~N~km^-2~yr^-1)','GSC~(MCM)') ,
  #                                    theme=ttheme_minimal(base_size=11.5, padding=unit(c(1, 1), 'mm'), parse=TRUE,
  #                                                         core   =list(fg_params=list(col=c('magenta','#00FF00','#FF0000','#0000FF','#FFA500','brown','darkgreen'))),
  #                                                         colhead=list(fg_params=list(fontface='bold'))))
  # 
  # p2 <- p2 + annotation_custom(table_fert_GSC, xmin=550, xmax=850, ymin=1400, ymax=1800) 
  
  # https://stackoverflow.com/questions/46068074/double-box-plots-in-ggplot2
  plot.x           <- layer_data(ggplot(data=plot_data) + geom_boxplot(aes(x=CWC_basin, y=fert_kgN_km2)))[,1:6]
  plot.y           <- layer_data(ggplot(data=plot_data) + geom_boxplot(aes(x=CWC_basin, y=WRTDS_DIN_flux)))[,1:6]
  colnames(plot.x) <- paste0('x.', gsub('y', '', colnames(plot.x)))
  colnames(plot.y) <- paste0('y.', gsub('y', '', colnames(plot.y)))
  df               <- cbind(plot.x, plot.y)
  rm(plot.x, plot.y)
  df$CWC_basin     <- sort(unique(plot_data$CWC_basin))
  
  df.outliers      <- df %>%
                      select(CWC_basin, x.middle, x.outliers, y.middle, y.outliers) %>%
                      data.table::data.table()
  df.outliers      <- df.outliers[, list(x.outliers=unlist(x.outliers), y.outliers=unlist(y.outliers)), 
                                  by = list(CWC_basin, x.middle, y.middle)]
  
  # Add value for errorbar width 
  bar_x_width          <- 0.015*ceiling(max(sum_plot_data$mean_fert))
  bar_y_width          <- 0.25*ceiling(max(sum_plot_data$mean_DIN))
  
  # ---------- Making plot showing histogram value in both directions (annual DIN load vs. annual fertilizer usage rate) ---------- 
  p6 <- ggplot(data=df) +
    geom_errorbarh(aes(x=x.middle, y=y.middle,   xmin=x.lower, xmax=x.upper,  color=CWC_basin), height=bar_x_width, lwd=0.75) +  # lower and upper end of box plot in x-axis dimension
    geom_errorbar(aes( x=x.middle, ymin=y.lower, ymax=y.upper,                color=CWC_basin), width=bar_y_width,  lwd=0.75) +  # lower and upper end of box plot in y-axis dimension
    geom_segment(aes(x=x.min,      y=y.middle,   xend=x.max,   yend=y.middle, color=CWC_basin)) +                              # min and max whiskers for x-axis dimension
    geom_segment(aes(x=x.middle,   y=y.min,      xend=x.middle,yend=y.max,    color=CWC_basin)) +                              # min and max whiskers for y-axis dimension
    geom_point(data=df.outliers, aes(x=x.outliers, y=y.middle,                color=CWC_basin), shape=19) +                    # outiers in x-direction
    geom_point(data=df.outliers, aes(x=x.middle,   y=y.outliers,              color=CWC_basin), shape=19) +                    # outiers in y-direction
    # geom_point(data=sum_plot_data, aes(x=mean_fert, y=mean_DIN, color=CWC_basin, size=mean_precip, fill=GSC_MCM_mm), shape=21, stroke=1.2) +
    geom_point(data=sum_plot_data, aes(x=mean_fert, y=mean_DIN, color=CWC_basin, size=mean_precip, fill=LSC_MCM_mm), shape=21, stroke=1.2) +
    scale_color_manual(values=color_pal) +
    scale_fill_manual(values = c('grey70','grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10')) +
    guides(color=guide_legend(order=0, title='CWC Basins', override.aes=list(lwd=2, shape=NA)),
           # fill =guide_legend(order=1, title=expression(paste(Gross~sto~cap~'[',MCM~mm^-1,']')), override.aes=list(size=7)),
           fill =guide_legend(order=1, title=expression(paste(Live~sto~cap~'[',MCM~mm^-1,']')), override.aes=list(size=7)),
           size =guide_legend(order=2, title=expression(paste(Annual~precip~'[',mm~yr^-1,']')))) +
    labs(x    = expression(paste(Annual~fertilizer~application~'[',kg~N~km^-2~yr^-1,']')),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          legend.position   = 'right',
          legend.key.size   = unit(1.5, 'lines'),
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p6)
  
  # ---------- Making plot showing scatterplot of mean annual DIN load against mean Live Storage Capacity ----------
  
  p7 <- ggplot(data=sum_plot_data, aes(x=as.numeric(as.character(LSC_MCM_mm)), y=mean_DIN, color=CWC_basin)) +
    geom_errorbar(aes(ymin=mean_DIN - sd_DIN, ymax=mean_DIN + sd_DIN, color=CWC_basin),  lwd=0.75, show.legend=FALSE) +  # mean +/- one sigma in y-axis dimension
    geom_point(data=plot_data, aes(x=as.numeric(as.character(LSC_MCM_mm)), y=WRTDS_DIN_flux), alpha=0.25, shape=16, size=3) +
    geom_point(aes(x=as.numeric(as.character(LSC_MCM_mm)), y=mean_DIN, color=CWC_basin), shape=0, size=7, stroke=1.2) +
    geom_point(aes(x=as.numeric(as.character(LSC_MCM_mm)), y=mean_DIN, size=mean_precip, fill=as.factor(round(mean_fert,0))), color='transparent', shape=21, stroke=1.2) +
    scale_color_manual(values=color_pal) +
    scale_fill_manual(values = c('grey70','grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10')) +
    guides(color=guide_legend(order=0, title='CWC Basins'),
           fill =guide_legend(order=1, title=expression(paste(Annual~fertilizer~application~'[',kg~N~km^-2~yr^-1,']')), override.aes=list(size=7)),
           size =guide_legend(order=2, title=expression(paste(Annual~precip~'[',mm~yr^-1,']')),     override.aes=list(color='black'))) +
    labs(x    = expression(paste(Live~storage~capacity~'[',MCM~mm^-1,']')),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          legend.position   = 'right',
          legend.key.size   = unit(1.5, 'lines'),
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)

  print(p7)
  
  
  # ---------- Making plot showing histogram of mean annual DIN load against mean Live Storage Capacity ---------- 
  # p8 <- ggplot(data=plot_data, aes(x=as.numeric(as.character(GSC_MCM_mm)), y=WRTDS_DIN_flux)) + 
  p8 <- ggplot(data=plot_data, aes(x=as.numeric(as.character(LSC_MCM_mm)), y=WRTDS_DIN_flux)) + 
    geom_boxplot(aes(color=CWC_basin)) + 
    # geom_point(data=sum_plot_data, aes(x=as.numeric(as.character(GSC_MCM_mm)), y=mean_DIN, size=mean_precip, fill=as.factor(round(mean_fert,0))), shape=21) +
    geom_point(data=sum_plot_data, aes(x=as.numeric(as.character(LSC_MCM_mm)), y=mean_DIN, size=mean_precip, fill=as.factor(round(mean_fert,0))), shape=21) +
    scale_color_manual(values=color_pal) + 
    scale_fill_manual(values = c('grey70','grey60', 'grey50', 'grey40', 'grey30', 'grey20', 'grey10')) +
    guides(color=guide_legend(order=0, title='CWC Basins'),
           fill =guide_legend(order=1, title=expression(paste(Annual~fertilizer~application~'[',kg~N~km^-2~yr^-1,']')), override.aes=list(size=7)),
           size =guide_legend(order=2, title=expression(paste(Annual~precip~'[',mm~yr^-1,']')))) +
    # labs(x    = expression(paste(Gross~storage~capacity~'[',MCM~mm^-1,']')),
    labs(x    = expression(paste(Live~storage~capacity~'[',MCM~mm^-1,']')),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          legend.position   = 'right',
          legend.key.size   = unit(1.5, 'lines'),
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p8)
  
  dev.off()
  
}

#_______________________________________________________________________________
# Make plots of comparing DIN flux to basin characteristics such as
# annual precip, storage capacity, and fertilizer usage rate
plot_DIN_flux_vs_basin_charac_wq_stations_2 <- function(plot_data, sum_plot_data, LSC_variable, LSC_variable_label){
  
  # Update column name for LSC variable
  colnames(plot_data)[which(colnames(plot_data)==LSC_variable)]         <- 'LSC_var'
  colnames(sum_plot_data)[which(colnames(sum_plot_data)==LSC_variable)] <- 'LSC_var'
  
  # Only keep select column for plotting
  plot_data     <- plot_data[,c('Year','Station','CWC_basin','precip','WRTDS_DIN_flux','fert_kgN_km2','LSC_var')]
  sum_plot_data <- sum_plot_data[,c('Station','CWC_basin','mean_precip','mean_DIN','sd_DIN','mean_fert','LSC_var')]
  
  # Convert data to long format for plotting
  plot_data                <- gather(plot_data,     key=variable, value=value, -Year, -Station, -CWC_basin, -WRTDS_DIN_flux)
  sum_plot_data            <- gather(sum_plot_data, key=variable, value=value, -Station, -CWC_basin, -mean_DIN, -sd_DIN)
  
  # Add long label
  plot_data$variableLab     <- param_labeller('variable', plot_data$variable,     LSC_variable_label)
  sum_plot_data$variableLab <- param_labeller('variable', sum_plot_data$variable, LSC_variable_label)
  
  variable.labels           <- c('Total~annual~precipitation~(mm~yr^-1)',
                                 'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)',
                                 LSC_variable_label)
  # variable.labels           <- c('italic(P)[Annual]~(mm~yr^-1)',
  #                                'italic(N)[Fert]~(kg~N~km^-2~yr^-1)',
  #                                LSC_variable_label)
  
  # dummy data for fixing axis scales
  dummy                     <- tibble(Year           = 2000,
                                      Station        = 'Polavaram',
                                      CWC_basin      = 'Godavari',
                                      variableLab    = 'Total~annual~precipitation~(mm~yr^-1)', # 'italic(P)[Annual]~(mm~yr^-1)'
                                      WRTDS_DIN_flux = 1000,
                                      value          = c(500,2000))
  
  print(dummy)
  
  print(group_by(plot_data, variable)%>% summarise(max_value=max(value)))
  # Reorder factor levels
  plot_data$variableLab     <- factor(plot_data$variableLab,     levels=variable.labels)
  sum_plot_data$variableLab <- factor(sum_plot_data$variableLab, levels=variable.labels)
  dummy$variableLab         <- factor(dummy$variableLab,         levels=variable.labels)
  
  
  # ---------- Making plot showing ellipsoid in both directions (annual DIN load vs. annual precip) ---------- 
  p1 <- ggplot(data=plot_data, aes(x=value, y=WRTDS_DIN_flux, color=CWC_basin)) + 
    geom_point( data=subset(plot_data, variable!='LSC_var'), alpha=0.3, shape=16, size=3) +
    geom_jitter(data=subset(plot_data, variable=='LSC_var'), alpha=0.3, shape=16, size=3, height=0) +
    stat_ellipse(data=subset(plot_data, variable!='LSC_var'), level=0.68, type='norm', lwd=1.2) +  # level=0.68 corresponds to approximately 1 sigma
    geom_segment(data=subset(sum_plot_data, variable=='LSC_var'), aes(x=value, y=mean_DIN - sd_DIN, xend=value, yend=mean_DIN + sd_DIN, color=CWC_basin),  lwd=0.75) + # mean +/- one sigma in y-axis dimension
    geom_point(  data=sum_plot_data,aes(x=value, y=mean_DIN, color=CWC_basin), shape=18, size=7) +
    geom_blank(data=dummy, aes(x=value, y=WRTDS_DIN_flux))   + # Adding blank geom to ensure required axis
    facet_wrap(~ variableLab, scales='free_x', nrow=1,  labeller=label_parsed, strip.position='bottom') +
    scale_color_manual(values=color_pal) +
    scale_x_continuous(expand=c(0.05,0.05)) +
    guides(color=guide_legend(order=0, nrow=1, title=NULL)) +
    labs(x    = NULL,
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'(',kg~N~km^-2~yr^-1,')')),
         # y    = expression(paste(italic(Q)[DIN]~'(',kg~N~km^-2~yr^-1,')')), 
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          panel.border      = element_rect(colour ='black',size=1.2),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          text              = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.placement   = 'outside',
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_blank(),   # Remove background color and black border
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p1)
  
}

#_______________________________________________________________________________
# Make plots of comparing DIN flux to basin characteristics: annual precip and fertilizer usage rate
plot_DIN_flux_vs_precip_fert <- function(plot_data, sum_plot_data){
  
  print(paste('R2 between DIN flux and precip',         signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ precip))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and fert rate',      signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ fert_kgN_km2))$r.squared,2), sep=' '))
  
  print(paste('R2 between average DIN flux and average precip',         signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_precip))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and average fert rate',      signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_fert))$r.squared,2), sep=' '))
  
  # Only keep select column for plotting
  plot_data     <- plot_data[,c('Year','Station','CWC_basin','precip','WRTDS_DIN_flux','fert_kgN_km2')]
  sum_plot_data <- sum_plot_data[,c('Station','CWC_basin','mean_precip','mean_DIN','sd_DIN','mean_fert')]
  
  # Convert data to long format for plotting
  plot_data                <- gather(plot_data,     key=variable, value=value, -Year, -Station, -CWC_basin, -WRTDS_DIN_flux)
  sum_plot_data            <- gather(sum_plot_data, key=variable, value=value, -Station, -CWC_basin, -mean_DIN, -sd_DIN)
  
  # Add long label
  plot_data$variableLab     <- param_labeller('variable', plot_data$variable,     '')
  sum_plot_data$variableLab <- param_labeller('variable', sum_plot_data$variable, '')
  
  variable.labels           <- c('Total~annual~precipitation~(mm~yr^-1)',
                                 'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)')
  # variable.labels           <- c('italic(P)[Annual]~(mm~yr^-1)',
  #                                'italic(N)[Fert]~(kg~N~km^-2~yr^-1)')
  
  # ---------- Adding plot numbering ---
  fig_labelling             <- tibble(variableLab    = c('Total~annual~precipitation~(mm~yr^-1)',
                                                         'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)'),
                                      fig_label      = c('A)','B)'))
  # fig_labelling             <- tibble(variableLab    = c('italic(P)[Annual]~(mm~yr^-1)',
  #                                                        'italic(N)[Fert]~(kg~N~km^-2~yr^-1)'),
  #                                     fig_label      = c('A)','B)'))
  
  # dummy data for fixing axis scales
  dummy                     <- tibble(Year           = 2000,
                                      Station        = 'Polavaram',
                                      CWC_basin      = 'Godavari',
                                      variableLab    = 'Total~annual~precipitation~(mm~yr^-1)', # 'italic(P)[Annual]~(mm~yr^-1)'
                                      WRTDS_DIN_flux = 1000,
                                      value          = c(500,2000))
  
  print(dummy)
  
  print(group_by(plot_data, variable)%>% summarise(max_value=max(value)))
  # Reorder factor levels
  plot_data$variableLab     <- factor(plot_data$variableLab,     levels=variable.labels)
  sum_plot_data$variableLab <- factor(sum_plot_data$variableLab, levels=variable.labels)
  dummy$variableLab         <- factor(dummy$variableLab,         levels=variable.labels)
  fig_labelling$variableLab <- factor(fig_labelling$variableLab,         levels=variable.labels)
  
  # ---------- Making plot showing ellipsoid in both directions (annual DIN load vs. annual precip) ---------- 
  p1 <- ggplot(data=plot_data, aes(x=value, y=WRTDS_DIN_flux, color=CWC_basin)) + 
    geom_point(alpha=0.3, shape=16, size=3) +
    stat_ellipse(level=0.68, type='norm', lwd=1.2) +            # level=0.68 corresponds to approximately 1 sigma
    geom_point(data=sum_plot_data,aes(x=value, y=mean_DIN, color=CWC_basin), shape=18, size=7) +
    geom_blank(data=dummy, aes(x=value, y=WRTDS_DIN_flux)) +    # Adding blank geom to ensure required axis
    geom_text(data=fig_labelling,aes(x=-Inf, y=Inf, label=fig_label), vjust=1.4, hjust=-0.4, col='black', size=6) +
    facet_wrap(~ variableLab, scales='free_x', nrow=1,  labeller=label_parsed, strip.position='bottom') +
    scale_color_manual(values=color_pal) +
    scale_x_continuous(expand=c(0.05,0.05)) +
    guides(color=guide_legend(order=0, nrow=2, byrow=FALSE, title=NULL)) +
    labs(x    = NULL,
         # y    = expression(atop(Dissolved~inorganic~nitrogen, paste(flux~'(',kg~N~km^-2~yr^-1,')'))),
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'(',kg~N~km^-2~yr^-1,')')),
         # y    = expression(paste(italic(Q)[DIN]~'(',kg~N~km^-2~yr^-1,')')),
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          panel.border      = element_rect(colour ='black',size=1.2),
          legend.position   = 'bottom',
          legend.text       = element_text(size=15,family='Helvetica',color='black'),
          text              = element_text(size=15,family='Helvetica',color='black'),
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          strip.placement   = 'outside',
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background  = element_blank(),   # Remove background color and black border
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p1)
  
}

#_______________________________________________________________________________
# Make plots of comparing DIN flux to basin characteristics: annual precip and fertilizer usage rate
plot_DIN_flux_vs_precip_fert_TOC_1 <- function(plot_data, sum_plot_data){
  
  print(paste('R2 between DIN flux and precip',         signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ precip))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and fert rate',      signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ fert_kgN_km2))$r.squared,2), sep=' '))
  
  print(paste('R2 between average DIN flux and average precip',         signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_precip))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and average fert rate',      signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_fert))$r.squared,2), sep=' '))
  
  # Only keep select column for plotting
  plot_data     <- plot_data[,c('Year','Station','CWC_basin','precip','WRTDS_DIN_flux','fert_kgN_km2')]
  sum_plot_data <- sum_plot_data[,c('Station','CWC_basin','mean_precip','mean_DIN','sd_DIN','mean_fert')]
  
  # Convert data to long format for plotting
  plot_data                <- gather(plot_data,     key=variable, value=value, -Year, -Station, -CWC_basin, -WRTDS_DIN_flux)
  sum_plot_data            <- gather(sum_plot_data, key=variable, value=value, -Station, -CWC_basin, -mean_DIN, -sd_DIN)
  
  # Add long label
  plot_data$variableLab     <- param_labeller('variable', plot_data$variable,     '')
  sum_plot_data$variableLab <- param_labeller('variable', sum_plot_data$variable, '')
  
  variable.labels           <- c('Total~annual~precipitation~(mm~yr^-1)',
                                 'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)')
  
  # ---------- Adding plot numbering ---
  fig_labelling             <- tibble(variableLab    = c('Total~annual~precipitation~(mm~yr^-1)',
                                                         'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)'),
                                      fig_label      = c('A)','B)'))
  
  # dummy data for fixing axis scales
  dummy                     <- tibble(Year           = 2000,
                                      Station        = 'Polavaram',
                                      CWC_basin      = 'Godavari',
                                      variableLab    = 'Total~annual~precipitation~(mm~yr^-1)', # 'italic(P)[Annual]~(mm~yr^-1)'
                                      WRTDS_DIN_flux = 1000,
                                      value          = c(500,2000))
  
  print(dummy)
  
  print(group_by(plot_data, variable)%>% summarise(max_value=max(value)))
  # Reorder factor levels
  plot_data$variableLab     <- factor(plot_data$variableLab,     levels=variable.labels)
  sum_plot_data$variableLab <- factor(sum_plot_data$variableLab, levels=variable.labels)
  dummy$variableLab         <- factor(dummy$variableLab,         levels=variable.labels)
  fig_labelling$variableLab <- factor(fig_labelling$variableLab,         levels=variable.labels)
  
  # ---------- Making plot showing ellipsoid in both directions (annual DIN load vs. annual precip) ---------- 
  p1 <- ggplot(data=plot_data, aes(x=value, y=WRTDS_DIN_flux, color=CWC_basin), show.legend = FALSE) + 
    geom_point(alpha=0.3, shape=16, size=1) +
    stat_ellipse(level=0.68, type='norm', lwd=0.5) +            # level=0.68 corresponds to approximately 1 sigma
    #geom_point(data=sum_plot_data,aes(x=value, y=mean_DIN, color=CWC_basin), shape=18, size=4) +
    geom_blank(data=dummy, aes(x=value, y=WRTDS_DIN_flux)) +    # Adding blank geom to ensure required axis
    geom_text(data=fig_labelling,aes(x=-Inf, y=Inf, label=fig_label), vjust=1.4, hjust=-0.4, col='black', size=1.9) +
    facet_wrap(~ variableLab, scales='free_x', nrow=1,  labeller=label_parsed, strip.position='bottom') +
    scale_color_manual(values=color_pal) +
    scale_x_continuous(expand=c(0.05,0.05)) +
    #guides(color=guide_legend(order=0, nrow=2, byrow=FALSE, title=NULL)) +
    labs(x    = NULL,
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'(',kg~N~km^-2~yr^-1,')')),
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          panel.border      = element_rect(colour ='black',size=0.5),
          legend.position   = 'none',
          #legend.text       = element_text(size=5,family='Helvetica',color='black'),
          text              = element_text(size=5,family='Helvetica',color='black'),
          plot.title        = element_text(size=5,family='Helvetica',color='black'),
          strip.placement   = 'outside',
          strip.text        = element_text(size=5,family='Helvetica',color='black'),
          strip.background  = element_blank(),   # Remove background color and black border
          axis.title.y      = element_text(size=5,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=5,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=5,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=5,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p1)
  
}

#_______________________________________________________________________________
# Make plots of comparing DIN flux to basin characteristics: annual precip and fertilizer usage rate
plot_DIN_flux_vs_precip_fert_TOC_2 <- function(plot_data, sum_plot_data){
  
  print(paste('R2 between DIN flux and precip',         signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ precip))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and fert rate',      signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ fert_kgN_km2))$r.squared,2), sep=' '))
  
  print(paste('R2 between average DIN flux and average precip',         signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_precip))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and average fert rate',      signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_fert))$r.squared,2), sep=' '))
  
  # Only keep select column for plotting
  plot_data     <- plot_data[,c('Year','Station','CWC_basin','precip','WRTDS_DIN_flux','fert_kgN_km2')]
  sum_plot_data <- sum_plot_data[,c('Station','CWC_basin','mean_precip','mean_DIN','sd_DIN','mean_fert')]
  
  # Convert data to long format for plotting
  plot_data                <- gather(plot_data,     key=variable, value=value, -Year, -Station, -CWC_basin, -WRTDS_DIN_flux)
  sum_plot_data            <- gather(sum_plot_data, key=variable, value=value, -Station, -CWC_basin, -mean_DIN, -sd_DIN)
  
  # Add long label
  plot_data$variableLab     <- param_labeller('variable', plot_data$variable,     '')
  sum_plot_data$variableLab <- param_labeller('variable', sum_plot_data$variable, '')
  
  variable.labels           <- c('Total~annual~precipitation~(mm~yr^-1)',
                                 'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)')
  
  # ---------- Adding plot numbering ---
  fig_labelling             <- tibble(variableLab    = c('Total~annual~precipitation~(mm~yr^-1)',
                                                         'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)'),
                                      fig_label      = c('A)','B)'))
  
  # dummy data for fixing axis scales
  dummy                     <- tibble(Year           = 2000,
                                      Station        = 'Polavaram',
                                      CWC_basin      = 'Godavari',
                                      variableLab    = 'Total~annual~precipitation~(mm~yr^-1)', # 'italic(P)[Annual]~(mm~yr^-1)'
                                      WRTDS_DIN_flux = 1000,
                                      value          = c(500,2000))
  
  print(dummy)
  
  print(group_by(plot_data, variable)%>% summarise(max_value=max(value)))
  # Reorder factor levels
  plot_data$variableLab     <- factor(plot_data$variableLab,     levels=variable.labels)
  sum_plot_data$variableLab <- factor(sum_plot_data$variableLab, levels=variable.labels)
  dummy$variableLab         <- factor(dummy$variableLab,         levels=variable.labels)
  fig_labelling$variableLab <- factor(fig_labelling$variableLab,         levels=variable.labels)
  
  # ---------- Making plot showing ellipsoid in both directions (annual DIN load vs. annual precip) ---------- 
  p1 <- ggplot(data=plot_data, aes(x=value, y=WRTDS_DIN_flux, color=CWC_basin), show.legend = FALSE) + 
    geom_point(alpha=0.3, shape=16, size=1) +
    stat_ellipse(level=0.68, type='norm', lwd=1.0) +            # level=0.68 corresponds to approximately 1 sigma
    #geom_point(data=sum_plot_data,aes(x=value, y=mean_DIN, color=CWC_basin), shape=18, size=4) +
    geom_blank(data=dummy, aes(x=value, y=WRTDS_DIN_flux)) +    # Adding blank geom to ensure required axis
    geom_text(data=fig_labelling,aes(x=-Inf, y=Inf, label=fig_label), vjust=1.4, hjust=-0.4, col='black', size=4) +
    facet_wrap(~ variableLab, scales='free_x', nrow=1,  labeller=label_parsed, strip.position='bottom') +
    scale_color_manual(values=color_pal) +
    scale_x_continuous(expand=c(0.05,0.05)) +
    #guides(color=guide_legend(order=0, nrow=2, byrow=FALSE, title=NULL)) +
    labs(x    = NULL,
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'(',kg~N~km^-2~yr^-1,')')),
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          panel.border      = element_rect(colour ='black',size=1.0),
          legend.position   = 'none',
          #legend.text       = element_text(size=5,family='Helvetica',color='black'),
          text              = element_text(size=10,family='Helvetica',color='black'),
          plot.title        = element_text(size=10,family='Helvetica',color='black'),
          strip.placement   = 'outside',
          strip.text        = element_text(size=10,family='Helvetica',color='black'),
          strip.background  = element_blank(),   # Remove background color and black border
          axis.title.y      = element_text(size=10,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=10,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=10,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=10,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p1)
  
}
#_______________________________________________________________________________
# Make plots of comparing DIN flux to basin characteristics such as
# annual precip and fertilizer usage rate
plot_DIN_flux_vs_precip_disch_fert <- function(plot_data, sum_plot_data){

  print(paste('R2 between DIN flux and precip',         signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ precip))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and JJAS precip',    signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ precip_JJAS))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and discharge',      signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ Disch_mm_yr))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and JJAS discharge', signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ Disch_mm_yr_JJAS))$r.squared,2), sep=' '))
  print(paste('R2 between DIN flux and fert rate',      signif(summary(lm(data=plot_data, WRTDS_DIN_flux ~ fert_kgN_km2))$r.squared,2), sep=' '))
   
  print(paste('R2 between average DIN flux and average precip',         signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_precip))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and average JJAS precip',    signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_precip_JJAS))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and average discharge',      signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_disch))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and average JJAS discharge', signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_disch_JJAS))$r.squared,2), sep=' '))
  print(paste('R2 between average DIN flux and average fert rate',      signif(summary(lm(data=sum_plot_data, mean_DIN ~ mean_fert))$r.squared,2), sep=' '))
  
  # Only keep select column for plotting
  plot_data     <- plot_data[,c('Year','Station','CWC_basin','precip','precip_JJAS','Disch_mm_yr',
                                'Disch_mm_yr_JJAS','WRTDS_DIN_flux','fert_kgN_km2')]
  sum_plot_data <- sum_plot_data[,c('Station','CWC_basin','mean_precip','mean_precip_JJAS','mean_disch',
                                    'mean_disch_JJAS','mean_DIN','sd_DIN','mean_fert')]
  
  # Convert data to long format for plotting
  plot_data                <- gather(plot_data,     key=variable, value=value, -Year, -Station, -CWC_basin, -WRTDS_DIN_flux)
  sum_plot_data            <- gather(sum_plot_data, key=variable, value=value, -Station, -CWC_basin, -mean_DIN, -sd_DIN)
  
  # Add long label
  plot_data$variableLab     <- param_labeller('variable', plot_data$variable,     '')
  sum_plot_data$variableLab <- param_labeller('variable', sum_plot_data$variable, '')
  
  variable.labels           <- c('Total~annual~precipitation~(mm~yr^-1)',
                                 'JJAS~precipitation~(mm~yr^-1)',
                                 'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)',
                                 'Annual~discharge~(mm~yr^-1)',
                                 'JJAS~discharge~(mm~yr^-1)')
  # variable.labels           <- c('italic(P)[Annual]~(mm~yr^-1)',
  #                                'italic(P)[JJAS]~(mm~yr^-1)',
  #                                'italic(N)[Fert]~(kg~N~km^-2~yr^-1)',
  #                                'italic(Q)[Annual]~(mm~yr^-1)',
  #                                'italic(Q)[JJAS]~(mm~yr^-1)')
  
  # ---------- Adding plot numbering ---
  fig_labelling             <- tibble(variableLab    = c('Total~annual~precipitation~(mm~yr^-1)',
                                                         'JJAS~precipitation~(mm~yr^-1)',
                                                         'Fertilizer~application~rate~(kg~N~km^-2~yr^-1)',
                                                         'Annual~discharge~(mm~yr^-1)',
                                                         'JJAS~discharge~(mm~yr^-1)'),
                                     fig_label      = c('A)','B)','C)','D)','E)'))
  # fig_labelling             <- tibble(variableLab    = c('italic(P)[Annual]~(mm~yr^-1)',
  #                                                        'italic(P)[JJAS]~(mm~yr^-1)',
  #                                                        'italic(N)[Fert]~(kg~N~km^-2~yr^-1)',
  #                                                        'italic(Q)[Annual]~(mm~yr^-1)',
  #                                                        'italic(Q)[JJAS]~(mm~yr^-1)'),
  #                                     fig_label      = c('A)','B)','C)','D)','E)'))
  
  # dummy data for fixing axis scales
  dummy                     <- tibble(Year           = 2000,
                                      Station        = 'Polavaram',
                                      CWC_basin      = 'Godavari',
                                      variableLab    = 'Total~annual~precipitation~(mm~yr^-1)', # 'italic(P)[Annual]~(mm~yr^-1)', 
                                      WRTDS_DIN_flux = 1000,
                                      value          = c(500,2000))
  
  print(dummy)
  
  print(group_by(plot_data, variable)%>% summarise(max_value=max(value)))
  # Reorder factor levels
  plot_data$variableLab     <- factor(plot_data$variableLab,     levels=variable.labels)
  sum_plot_data$variableLab <- factor(sum_plot_data$variableLab, levels=variable.labels)
  dummy$variableLab         <- factor(dummy$variableLab,         levels=variable.labels)
  fig_labelling$variableLab <- factor(fig_labelling$variableLab,         levels=variable.labels)
  
  # ---------- Making plot showing ellipsoid in both directions (annual DIN load vs. annual precip) ---------- 
  p1 <- ggplot(data=plot_data, aes(x=value, y=WRTDS_DIN_flux, color=CWC_basin)) + 
    geom_point(alpha=0.3, shape=16, size=3) +
    stat_ellipse(level=0.68, type='norm', lwd=1.2) +            # level=0.68 corresponds to approximately 1 sigma
    geom_point(data=sum_plot_data,aes(x=value, y=mean_DIN, color=CWC_basin), shape=18, size=7) +
    geom_blank(data=dummy, aes(x=value, y=WRTDS_DIN_flux)) +    # Adding blank geom to ensure required axis
    geom_text(data=fig_labelling,aes(x=-Inf, y=Inf, label=fig_label), vjust=1.4, hjust=-0.4, col='black', size=7) +
    facet_wrap(~ variableLab, scales='free_x', nrow=2,  labeller=label_parsed, strip.position='bottom') +
    scale_color_manual(values=color_pal) +
    scale_x_continuous(expand=c(0.05,0.05)) +
    guides(color=guide_legend(order=0, nrow=1, title=NULL)) +
    labs(x    = NULL,
         y    = expression(paste(Dissolved~inorganic~nitrogen~flux~'(',kg~N~km^-2~yr^-1,')')),
         # y    = expression(paste(italic(Q)[DIN]~'(',kg~N~km^-2~yr^-1,')')), 
         title= NULL) +
    theme_bw() +                                        # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          panel.border      = element_rect(colour ='black',size=1.2),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          text              = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.placement   = 'outside',
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_blank(),   # Remove background color and black border
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'), 
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'),
          aspect.ratio      = 1)
  
  print(p1)
  
}

#_______________________________________________________________________________
# Make plots of comparing DIN flux to basin characteristics such as
# annual precip, storage capacity, and fertilizer usage rate
data <- compile_basin_charac_wq_stations()

plot_data      <- data[[1]]
sum_plot_data  <- data[[2]]

# Make plots of comparing DIN flux to basin characteristics such as
# annual precip, storage capacity, and fertilizer usage rate
plot_DIN_flux_vs_basin_charac_wq_stations(plot_data, sum_plot_data)

# Make plots of comparing DIN flux to basin characteristics such as
# annual precip, storage capacity, and fertilizer usage rate
# Define path to output file
out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/DIN_flux_vs_basin_characteristics_WQ_stations_2.pdf',sep='')

# Delete existing file
unlink(out.f.name)
# Start pdf device driver for saving plots
pdf(out.f.name, height=7.8,width=17)

# Convert character to numeric
plot_data$LSC_MCM_mm     <- as.numeric(as.character(plot_data$LSC_MCM_mm))
sum_plot_data$LSC_MCM_mm <- as.numeric(as.character(sum_plot_data$LSC_MCM_mm))

plot_DIN_flux_vs_basin_charac_wq_stations_2(plot_data, sum_plot_data, 
                                            LSC_variable='LSC_MCM_mm', 
                                            LSC_variable_label='Live~storage~capacity~(MCM~mm^-1)')

plot_DIN_flux_vs_basin_charac_wq_stations_2(plot_data, sum_plot_data, 
                                            LSC_variable='LSC_MCM', 
                                            LSC_variable_label='Live~storage~capacity~(MCM)')

plot_DIN_flux_vs_basin_charac_wq_stations_2(plot_data, sum_plot_data, 
                                            LSC_variable='LSC', 
                                            LSC_variable_label='Live~storage~capacity~(unitless)')

dev.off()

# Make plots of comparing DIN flux to basin characteristics such as
# annual precip, storage capacity, and fertilizer usage rate
# Define path to output file
out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/DIN_flux_vs_basin_characteristics_WQ_stations_3.pdf',sep='')

# Delete existing file
unlink(out.f.name)
# Start pdf device driver for saving plots
pdf(out.f.name, height=6.5,width=11)

# Make plots of comparing DIN flux to basin characteristics: annual precip and fertilizer usage rate
plot_DIN_flux_vs_precip_fert(plot_data, sum_plot_data)

dev.off()

# ----------
out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/DIN_flux_vs_basin_characteristics_WQ_stations_3_TOC_1.pdf',sep='')

# Delete existing file
unlink(out.f.name)
# Start pdf device driver for saving plots
pdf(out.f.name, height=1.875,width=3.33)

# Make plots of comparing DIN flux to basin characteristics: annual precip and fertilizer usage rate
plot_DIN_flux_vs_precip_fert_TOC_1(plot_data, sum_plot_data)

dev.off()
# ----------

out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/DIN_flux_vs_basin_characteristics_WQ_stations_3_TOC_2.pdf',sep='')

# Delete existing file
unlink(out.f.name)
# Start pdf device driver for saving plots
pdf(out.f.name, height=3.75,width=6.66)

# Make plots of comparing DIN flux to basin characteristics: annual precip and fertilizer usage rate
plot_DIN_flux_vs_precip_fert_TOC_2(plot_data, sum_plot_data)

dev.off()
# ----------

# Make plots of comparing DIN flux to basin characteristics such as
# annual precip, storage capacity, and fertilizer usage rate
# Define path to output file
out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/DIN_flux_vs_basin_characteristics_WQ_stations_4.pdf',sep='')

# Delete existing file
unlink(out.f.name)
# Start pdf device driver for saving plots
pdf(out.f.name, height=14.0,width=17)

# Make plots of comparing DIN flux to basin characteristics such as
# annual precip and fertilizer usage rate
plot_DIN_flux_vs_precip_disch_fert(plot_data, sum_plot_data)

dev.off()
