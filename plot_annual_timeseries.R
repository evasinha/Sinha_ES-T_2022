# Author - Eva Sinha, Stanford University, esinha@stanford.edu
# Date   - 16th June, 2017
#
# Function details
# plot_annual_time_series                       - Make plots of annual time series of fertilizer usage and annual precip
# read_basin_annual_fertilizer_LUH2_per_ha      - Read annual fertilizer usage for various CWC basins [kg N/ha/yr]
# read_basin_annual_precip                      - Read annual and monthly precipitation
# read_basin_mean_annual_temp                   - Read mean annual temperature
# read_CWC_sta_basin_annual_discharge_conc_flux - Read annual discharge, DIN conc, and DIN flux for various CWC basins
# read_hydro_sta_basin_annual_discharge         - Read total annual discharge for various CWC basins based on Hydro observation station data
# read_annual_DIN_flux                          - Read total annual DIN flux [kg N/km2/yr] for various CWC basins based on WRTDS model application


library(tidyr)
library(dplyr)
library(purrr)         # reduce
library(ggplot2)
library(ggthemes)
library(zoo)
library(gridExtra)     # grid.arrange
library(grid)          # for textGrob
library(readxl)        # read_excel
library(Hmisc)         # approxExtrap	
library(broom)

source('~/Documents/repos/india_wq/plot_vector.R')
source('~/Documents/repos/india_wq/india_color_pal.R')
source('~/Documents/repos/india_wq/labels.R')
source('~/Documents/repos/india_wq/read_fert_EandS.R')

#_______________________________________________________________________________
# Make plots of annual time series of fertilizer usage and annual precip
plot_annual_time_series <- function(){
  
  filepath         <- '~/Documents/repos/india_wq/'
  
  # Read station information
  wq_stations      <- read.csv(paste(filepath,'WQ_stations_DIN_model.txt',sep=''),header=T,sep='\t')
  
  min_yr           <- 1980
  max_yr           <- 2015
  
  # # Read annual fertilizer usage for various CWC basins [kg N/ha/yr]
  # fertl_crpf     <- read_basin_annual_fertilizer_LUH2_per_ha(in.folder='~/Documents/repos/india_wq/Fertilizer/CMIP6_LUH2/LUH2_v2h/CWC_basins/', min_yr, max_yr=2015)
  
  # Read fertilizer usage rate for various river basins in India [kg/km2/yr]
  # Source: Directorate of Economics and Statistics Department of Agriculture, Cooperation and Farmers Welfare
  fertl_crpf                                                   <- read_EandS_fert_India_river_basins()
  fertl_crpf$Desc                                              <- NULL
  fertl_crpf$scenario                                          <- NULL
  # Only keep values after 1980
  fertl_crpf   <- fertl_crpf[which(fertl_crpf$Year >= 1980), ]
  
  # Interpolate annual fertilizer values for yrs falling between the 1980 and 1985
  fertl_crpf   <- interpolate_fert(fertl_crpf)
  
  # Rename column
  colnames(fertl_crpf)[which(colnames(fertl_crpf) == 'Basin')] <- 'CWC_basin' 
  
  # Add station names
  fertl_crpf   <- merge(fertl_crpf, wq_stations[, c('Station','CWC_basin')]) 
  
  # # Read annual and monthly precipitation based on APHRODITE data
  # precip         <- read_basin_annual_precip(in.folder = '~/Documents/repos/india_wq/daily_precip/APHRODITE/0.25deg/CWC_basins/',
  #                                            filename  = 'CWC_Basin_APHRODITE_precip_daily.precip',
  #                                            out_fname = 'WQ_Station_APHRODITE_average_annual_precip.txt',
  #                                            min_yr, max_yr)
  
  # # Read annual and monthly precipitation based on CPC data
  # precip         <- read_basin_annual_precip(in.folder = '~/Documents/repos/india_wq/daily_precip/CPC/Basin_wq_stations/',
  #                                            filename  = 'WQ_Station_Basin_CPC_precip_daily.precip',
  #                                            out_fname = 'WQ_Station_CPC_average_annual_precip.txt',
  #                                            min_yr, max_yr)
  
  # Read annual and monthly precipitation based on IMD data
  precip         <- read_basin_annual_precip(in.folder = '~/Documents/repos/india_wq/daily_precip/IMD/0.25deg/Basin_wq_stations/',
                                             filename  = 'WQ_Station_Basin_IMD_precip_daily.precip',
                                             out_fname = 'WQ_Station_IMD_average_annual_precip.txt',
                                             min_yr, max_yr)
  # 
  # precip         <- read_basin_annual_precip(in.folder = '~/Documents/repos/india_wq/daily_precip/IMD/0.25deg/CWC_basins/',
  #                                            filename  = 'CWC_Basin_IMD_precip_daily.precip',
  #                                            out_fname = 'CWC_Basin_IMD_average_annual_precip.txt',
  #                                            min_yr, max_yr)
  
  precip_annual  <- precip[['Annual']]
  precip_monthly <- precip[['Monthly']]
  
  tmp      <-  group_by(precip_annual, Year) %>% summarise(precip=sum(precip))
  tmp$rank <- rank(-tmp$precip)
  tmp      <- tmp[which(tmp$rank <=3 | tmp$rank >=34),]
  print('Three highest and three lowest annual precip years across all stations')
  print(tmp)
                       
  ranks <- group_by(precip_annual, Station) %>% mutate(rank=rank(-precip)) # Descending order
  ranks <- ranks[which(ranks$rank <=3 & ranks$Station %in% c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                             'Polavaram','Urachikottai','Vijayawada')), ]
  print('Three highest annual precip years for various stations')
  print(spread(ranks[c('Year','Station','rank')], key=rank, value=Year))
  print(table(ranks$Year))
  
  ranks <- group_by(precip_annual, Station) %>% mutate(rank=rank(-precip_JJAS)) # Descending order
  ranks <- ranks[which(ranks$rank <=3 & ranks$Station %in% c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                             'Polavaram','Urachikottai','Vijayawada')), ]
  print('Three highest JJAS precip years for various stations')
  print(spread(ranks[c('Year','Station','rank')], key=rank, value=Year))
  
  # # Read mean annual temperature based on APHRODITE data
  # temp           <- read_basin_mean_annual_temp(in.folder = '~/Documents/repos/india_wq/daily_temp/APHRODITE/0.25deg/CWC_basins/',
  #                                               filename  = 'CWC_Basin_APHRODITE_tmean_daily.tmean',
  #                                               min_yr)
  
  # # Read mean annual temperature based on CRU data
  # temp           <- read_basin_mean_annual_temp(in.folder = '~/Documents/repos/india_wq/monthly_temp/CRU/Basin_wq_stations/',
  #                                               filename  = 'WQ_Station_Basin_CRU_temp_monthly.temp',
  #                                               min_yr)
  
  # Read mean annual temperature based on IMD data
  temp           <- read_basin_mean_annual_temp(in.folder = '~/Documents/repos/india_wq/daily_temp/IMD/1deg/Basin_wq_stations/',
                                                filename  = 'WQ_Station_Basin_IMD_tmean_daily.tmean',
                                                min_yr)
  
  temp_annual    <- temp[['Annual']]
  temp_monthly   <- temp[['Monthly']]
  
  # Estimate mean summer (June-July-August-September) temperature 
  temp_seasonal  <- filter(temp_monthly, Month %in% c(6,7,8,9)) %>%   # keep months c(6,7,8,9), then ...
                    group_by(Year, Station, CWC_basin) %>%            # group by Year and basins, then ...
                    summarise(temp_JJAS = mean(temp, na.rm=TRUE))     # summarise each column
  
  in.folder        <- '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/'
  f.name           <- 'CWC_select_stations.txt'
  select.sta.loc   <- read.csv(paste(in.folder,f.name,sep=''), header=T, sep='\t')
  
  # # Read annual discharge, DIN conc, and DIN flux for various CWC basins based on CWC WQ station data
  # wq.annual.mean <- read_CWC_sta_basin_annual_discharge_conc_flux(in.folder = '~/Documents/repos/india_wq/India_WRIS/WQ_ShinyApp/',
  #                                                                 f.name    = 'CWC_select_stations.txt')
    
  # Read total annual discharge for various CWC basins based on Hydro observation station data
  hydro.mean       <- read_hydro_sta_basin_annual_discharge(in.folder = '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/',
                                                            select.sta.loc, min_yr, max_yr)
  
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

  # Read total annual DIN load [kgN/km2/yr] for various CWC basins based on WRTDS model application
  DIN.flux        <- read_annual_DIN_flux(in.folder='~/Documents/repos/india_wq/EGRET/WRTDS_analysis/', 
                                          select.sta.loc)
    
  ranks <- group_by(DIN.flux, Station) %>% mutate(rank=rank(-WRTDS_DIN_flux)) # Descending order
  ranks <- ranks[which(ranks$rank <=3 & ranks$Station %in% c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                             'Polavaram','Urachikottai','Vijayawada')), ]
  print('Three highest annual DIN flux year for various stations')
  print(spread(ranks[c('Year','Station','rank')], key=rank, value=Year))
  
  # Merge data frames
  plot_data <- reduce(list(fertl_crpf, precip_annual, temp_annual, temp_seasonal, hydro.annual.tot, DIN.flux), 
                left_join, by = c('Year', 'Station', 'CWC_basin'))
  
  # ---------- Scatter plot of DIN flux and annual precipitation & DIN flux and annual discharge ----------
  
  # Define path to output file
  out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/CWC_basins_DIN_precip_disch.pdf',sep='')
  
  # Delete existing file
  unlink(out.f.name)
  # Start pdf device driver for saving plots
  pdf(out.f.name, height=8.5,width=11)

  # Create the R2 labels for the two groups
  tmp_plot_data <- na.omit(plot_data)
  
  tmp_plot_data$CWC_basin  <- factor(tmp_plot_data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                       'Godavari','Cauvery','Krishna'))
  
  print('R2 betwen DIN flux and precip for All basins')
  print(summarise(tmp_plot_data, R2   = summary(lm(WRTDS_DIN_flux ~ precip))$r.squared,
                                 p_val= signif(glance(lm(WRTDS_DIN_flux ~ precip))$p.value, digits=2)))
  
  eq         <- group_by(tmp_plot_data, Station, CWC_basin) %>%    # group by variableLab then ...
    summarise(label= as.character(as.expression(substitute(italic(R)^2~'='~R2, 
                                                           list(R2 = signif(summary(lm(WRTDS_DIN_flux ~ precip))$r.squared, digits=2))))),
              R2   = summary(lm(WRTDS_DIN_flux ~ precip))$r.squared,
              n    = as.character(as.expression(substitute(italic(n)~"="~len, list(len=length(precip))))),
              p_val= as.character(as.expression(substitute(italic(p)~"="~p_val, 
                                                           list(p_val= signif(glance(lm(WRTDS_DIN_flux ~ precip))$p.value, digits=2))))),
              y   = max(precip, na.rm=TRUE))

  eq$x <- -Inf
  
  p1 <- ggplot(data=tmp_plot_data, aes(x=WRTDS_DIN_flux, y=precip)) + 
    geom_point(color='gray50', size=2) +
    geom_smooth(method='lm', se=FALSE, color='red', linetype='dashed') +
    facet_wrap(~ CWC_basin, ncol=3, scale='free') +
    geom_text(data=eq, aes(x=x, y=y,      label=label), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    geom_text(data=eq, aes(x=x, y=0.93*y, label=p_val), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    geom_text(data=eq, aes(x=x, y=0.86*y,  label=n),     hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    labs(title = NULL,
         x     = expression(paste(WRTDS~estimated~DIN~flux,' [',kg~N~km^-2~yr^-1,']')),
         y     = expression(paste(Annual~precipitation,' [',mm~yr^-1,']'))) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),   # Remove background color of facet label but keep black border
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=12,family='Helvetica',color='black'), 
          axis.text.y       = element_text(size=12,family='Helvetica',color='black'))
  
  print(p1)
  
  print('R2 betwen DIN flux and discharge  for All basins')
  print(summarise(tmp_plot_data, R2   = summary(lm(WRTDS_DIN_flux ~ Disch_mm_yr))$r.squared,
                                 p_val= signif(glance(lm(WRTDS_DIN_flux ~ Disch_mm_yr))$p.value, digits=2)))
  
  
  eq         <- group_by(tmp_plot_data, Station, CWC_basin) %>%    # group by variableLab then ...
                summarise(label= as.character(as.expression(substitute(italic(R)^2~'='~R2, 
                                                           list(R2 = signif(summary(lm(WRTDS_DIN_flux ~ Disch_mm_yr))$r.squared, digits=2))))),
                          R2   = summary(lm(WRTDS_DIN_flux ~ Disch_mm_yr))$r.squared,
                          n    = as.character(as.expression(substitute(italic(n)~"="~len, list(len=length(Disch_mm_yr))))),
                          p_val= as.character(as.expression(substitute(italic(p)~"="~p_val, 
                                                                       list(p_val= signif(glance(lm(WRTDS_DIN_flux ~ Disch_mm_yr))$p.value, digits=2))))),
                          y   = max(Disch_mm_yr, na.rm=TRUE))
  
  eq$x <- -Inf
  
  p2 <- ggplot(data=tmp_plot_data, aes(x=WRTDS_DIN_flux, y=Disch_mm_yr)) + 
    geom_point(color='gray50', size=2) +
    geom_smooth(method='lm', se=FALSE, color='red', linetype='dashed') +
    facet_wrap(~ CWC_basin, ncol=3, scale='free') +
    geom_text(data=eq, aes(x=x, y=y,     label=label), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    geom_text(data=eq, aes(x=x, y=0.9*y, label=p_val), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    geom_text(data=eq, aes(x=x, y=0.8*y, label=n),     hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    labs(title = NULL,
         x     = expression(paste(WRTDS~estimated~DIN~flux,' [',kg~N~km^-2~yr^-1,']')),
         y     = expression(paste(Annual~discharge,' [',mm~yr^-1,']'))) +
    # scale_y_continuous(labels=scales::scientific) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),   # Remove background color of facet label but keep black border
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=12,family='Helvetica',color='black'), 
          axis.text.y       = element_text(size=12,family='Helvetica',color='black'))
  
  print(p2)
  
  eq         <- group_by(tmp_plot_data, Station, CWC_basin) %>%    # group by variableLab then ...
    summarise(label= as.character(as.expression(substitute(italic(R)^2~'='~R2, 
                                                           list(R2 = signif(summary(lm(precip ~ Disch_mm_yr))$r.squared, digits=2))))),
              R2   = summary(lm(precip ~ Disch_mm_yr))$r.squared,
              n    = as.character(as.expression(substitute(italic(n)~"="~len, list(len=length(Disch_mm_yr))))),
              p_val= as.character(as.expression(substitute(italic(p)~"="~p_val, 
                                                           list(p_val= signif(glance(lm(precip ~ Disch_mm_yr))$p.value, digits=2))))),
              y   = max(Disch_mm_yr, na.rm=TRUE))
  
  eq$x <- -Inf
  
  p3 <- ggplot(data=tmp_plot_data, aes(x=precip, y=Disch_mm_yr)) + 
    geom_point(color='gray50', size=2) +
    geom_smooth(method='lm', se=FALSE, color='red', linetype='dashed') +
    facet_wrap(~ CWC_basin, ncol=3, scale='free') +
    geom_text(data=eq, aes(x=x, y=y,     label=label), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    geom_text(data=eq, aes(x=x, y=0.9*y, label=p_val), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    geom_text(data=eq, aes(x=x, y=0.8*y, label=n),     hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, col='red', size=5) +
    labs(title = NULL,
         x     = expression(paste(Annual~precipitation,' [',mm~yr^-1,']')),
         y     = expression(paste(Annual~discharge,' [',mm~yr^-1,']'))) +
    # scale_y_continuous(labels=scales::scientific) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),   # Remove background color of facet label but keep black border
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=12,family='Helvetica',color='black'), 
          axis.text.y       = element_text(size=12,family='Helvetica',color='black'))
  
  print(p3)
  
  dev.off()
  # ____________________
  
  
  # Melt data
  plot_data     <- gather(plot_data, key=variable, value=value, -Year, -Station, -CWC_basin)
  
  # Add long label
  plot_data$variableLab <- param_labeller('variable',plot_data$variable)
  
  # Reorder factor levels
  plot_data$variableLab <- factor(plot_data$variableLab, levels=c('italic(N)[Fert]',
                                                                  'italic(P)[annual]','italic(P)[JJAS]',
                                                                  'italic(T)[annual]','italic(T)[JJAS]',
                                                                  'Discharge~(mm/yr)','Discharge~JJAS~(mm/yr)',
                                                                  'WRTDS~estimated~annual~DIN~flux'))

  tmp_plot_data          <- plot_data[which(plot_data$Station %in% c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                                     'Polavaram','Urachikottai','Vijayawada')
                                            & plot_data$variableLab %in% c('italic(P)[annual]','WRTDS~estimated~annual~DIN~flux','Discharge~(mm/yr)')), ]
  # Reorder factor levels
  tmp_plot_data$Station  <- factor(tmp_plot_data$Station, levels=c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                                   'Polavaram','Urachikottai','Vijayawada'))
  tmp_plot_data$CWC_basin  <- factor(tmp_plot_data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                       'Godavari','Cauvery','Krishna'))
  
  mean_sd <-  group_by(tmp_plot_data, Station, CWC_basin, variableLab) %>%
              summarise('mean'= paste('mean==',round(mean(value, na.rm=TRUE),0),sep=''),
                        'sd'  = paste('sd==',  round(sd(value, na.rm=TRUE),0),  sep=''),
                        'CV'  = paste('CV==',  round(sd(value, na.rm=TRUE)/mean(value, na.rm=TRUE),2),  sep=''),
                        'y'   = max(value, na.rm=TRUE))
  mean_sd$x <- 1980
  print(as.data.frame(mean_sd[which(mean_sd$variableLab == 'WRTDS~estimated~annual~DIN~flux'),c('Station','CWC_basin','variableLab','y')]))
  print(as.data.frame(mean_sd[which(mean_sd$variableLab == 'Discharge~(mm/yr)'),c('Station','CWC_basin','variableLab','y')]))
  print(as.data.frame(mean_sd[which(mean_sd$variableLab == 'italic(P)[annual]'),c('Station','CWC_basin','variableLab','y')]))

  # precip, disch, and flux y-axis limits
  mean_sd$y <-  c(2000, 2000, 2000,   # Brahmani and Baitarni
                  2000, 2000, 2000,   # Subernarekha
                  2000, 2000, 2000,   # Narmada
                  2000, 1000, 1000,   # Mahanadi
                  2000, 1000, 1000,   # Godavari
                  1500, 750,  750,    # Cauvery
                  1500, 750,  750)    # Krishna
  
  # Define path to output file
  out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/CWC_basins_timeseries.pdf',sep='')
  
  # Delete existing file
  unlink(out.f.name)
  # Start pdf device driver for saving plots
  pdf(out.f.name, height=17,width=11) 
  
  # ---------- Bar plot of DIN flux and annual precipitation for all stations ----------
  
  mean_sd_tmp <- mean_sd[which(mean_sd$variableLab == 'WRTDS~estimated~annual~DIN~flux'),]
  
  p1 <- ggplot(data=tmp_plot_data[which(tmp_plot_data$variableLab == 'WRTDS~estimated~annual~DIN~flux'),], 
               aes(x=Year, y=value, fill=variableLab)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75, alpha=0.15) + 
    geom_text(data=mean_sd_tmp, aes(x=x, y=y,       label=mean), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    geom_text(data=mean_sd_tmp, aes(x=x, y=0.85*y,  label=sd),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    geom_text(data=mean_sd_tmp, aes(x=x, y=0.70*y,  label=CV),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    facet_wrap(~ CWC_basin, ncol=1, scales='free_y', strip.position='right', labeller=label_wrap_gen(width=15)) +  # line wrapping for label
    guides(fill=guide_legend(title=NULL)) + 
    labs(x    = NULL,
         y    = expression(paste(WRTDS~estimated~DIN~flux,' [',kg~N~km^-2~yr^-1,']')),
         title= NULL) +
    scale_fill_manual(values=color_pal, labels=function(x) parse(text=x)) +
    scale_x_continuous(expand=c(0.01,0.01), breaks=seq(1980,2015,3)) +
    scale_y_continuous(expand=c(0.01,0.01)) +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p1)
  
  mean_sd_tmp <- mean_sd[which(mean_sd$variableLab == 'Discharge~(mm/yr)'),]
  p2 <- ggplot(data=tmp_plot_data[which(tmp_plot_data$variableLab == 'Discharge~(mm/yr)'),], 
               aes(x=Year, y=value, fill=variableLab)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75) + 
    geom_text(data=mean_sd_tmp, aes(x=x, y=y,       label=mean), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    geom_text(data=mean_sd_tmp, aes(x=x, y=0.85*y,  label=sd),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    geom_text(data=mean_sd_tmp, aes(x=x, y=0.70*y,  label=CV),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    facet_wrap(~ CWC_basin, ncol=1, scales='free_y', strip.position='right', labeller=label_wrap_gen(width=15)) +  # line wrapping for label
    guides(fill=guide_legend(title=NULL)) + 
    labs(x    = NULL,
         y    = expression(paste(Annual~discharge,' [',mm~yr^-1,']')),
         title= NULL) +
    scale_fill_manual(values=color_pal, labels=function(x) parse(text=x)) +
    scale_x_continuous(expand=c(0.01,0.01), breaks=seq(1980,2015,3)) +
    scale_y_continuous(expand=c(0.01,0.01)) + # , labels=scales::scientific
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p2)
  
  mean_sd_tmp <- mean_sd[which(mean_sd$variableLab == 'italic(P)[annual]'),]
  p3 <- ggplot(data=tmp_plot_data[which(tmp_plot_data$variableLab == 'italic(P)[annual]'),], aes(x=Year, y=value, fill=variableLab)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75) + 
    geom_text(data=mean_sd_tmp, aes(x=x, y=y,       label=mean), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    geom_text(data=mean_sd_tmp, aes(x=x, y=0.85*y,  label=sd),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    geom_text(data=mean_sd_tmp, aes(x=x, y=0.70*y,  label=CV),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
    facet_wrap(~ CWC_basin, ncol=1, scales='free_y', strip.position='right', labeller=label_wrap_gen(width=15)) +  # line wrapping for label
    guides(fill=guide_legend(title=NULL)) + 
    labs(x    = NULL,
         y    = expression(paste(Annual~precipitation,' [',mm~yr^-1,']')),
         title= NULL) +
    scale_fill_manual(values=color_pal, labels=function(x) parse(text=x)) +
    scale_x_continuous(expand=c(0.01,0.01), breaks=seq(1980,2015,3)) +
    scale_y_continuous(expand=c(0.01,0.01)) +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p3)
  
  # ---------- For each station bar plot of DIN flux and various predictor variables ----------
  uniq_stations <- unique(plot_data$Station)
  
  for (ind in 1:length(uniq_stations)) {
    
    # Make bar plot of annual fertilizer usage
    p3 <- plot_facetted_barplot(plot_data[which(plot_data$Station == uniq_stations[ind]),],
                                x_col      = 'Year',
                                y_col      = 'value',
                                fill_col   = 'variable',
                                facet_var  = 'variableLab',
                                y_lab      = NULL,
                                min_yr,
                                plot_title = uniq_stations[ind])
    
    print(p3)
    
  }
  
  dev.off()
}

#_______________________________________________________________________________
# Read annual fertilizer usage for various CWC basins [kg N/ha/yr]
read_basin_annual_fertilizer_LUH2_per_ha <- function(in.folder, min_yr, max_yr){
  
  # Read CMIP6 LUH2 fertilizer usage [kg N/ha/yr]
  fertl_crpf   <- read.csv(paste(in.folder,'CWC_basins_fertilizer_historical.fertl',sep=''),header=T,sep='\t',na.strings='NA')
  
  # Melt data
  fertl_crpf   <- gather(fertl_crpf, key=BasinId, fert_kgN_ha, -Year)

  # Only keep values since min_yr
  fertl_crpf   <- fertl_crpf[which(fertl_crpf$Year >= min_yr & fertl_crpf$Year <= max_yr),]
  
  # Add CWC_basin
  fertl_crpf$CWC_basin <- basin_labeller('variable', fertl_crpf$BasinId)
  
  # # Add column for station type
  # fertl_crpf$Station <- 'Full basin'
  
  return(fertl_crpf)
  
}

#_______________________________________________________________________________
# Read annual and monthly precipitation
read_basin_annual_precip <- function(in.folder, filename, out_fname, min_yr, max_yr){
  
  # Read dail precip [mmd/day]
  precip_daily        <- read.csv(paste(in.folder, filename, sep=''), header=T, sep='\t')
  
  # Melt data to long form
  precip_daily        <- gather(precip_daily, key=Station, value=Precip, -Date)
  
  # Added column for Year, Month and Day
  precip_daily$Date   <- as.Date(as.character(precip_daily$Date), '%Y-%m-%d')
  precip_daily$Year   <- as.numeric(format(precip_daily$Date,'%Y'))
  precip_daily$Month  <- as.numeric(format(precip_daily$Date,'%m'))
  precip_daily$Day    <- as.numeric(format(precip_daily$Date,'%d'))

  # Only keep values since min_yr
  precip_daily        <- precip_daily[which(precip_daily$Year >= min_yr & precip_daily$Year <=max_yr),]
  
  # Add CWC_basin
  precip_daily$CWC_basin <- basin_labeller('variable', precip_daily$Station)
  
  # Calculate monthly precipitation
  precip_monthly      <- group_by(precip_daily, Year, Month, CWC_basin, Station) %>%  # group by first
                         summarise(precip = sum(Precip, na.rm=TRUE))                  # then summarise using fun sum
  
  # Calculate annual precipitation
  precip_annual       <- group_by(precip_daily, Year, CWC_basin, Station) %>%   # group by first
                         summarise(precip      = sum(Precip, na.rm=TRUE),       # then summarise using fun sum
                                   precip_JJAS = sum(Precip[which(Month %in% c(6,7,8,9))], na.rm=TRUE))
  # print(spread(precip_annual[,c('Year','Station','precip')], key=Station, value=precip))
  precip_avg_sd_annual <- group_by(precip_annual, CWC_basin, Station) %>%   # group by first
                          summarise(avg_annual_precip = round(mean(precip),0),
                                    sd_annual_precip  = round(sd(precip),0))
  
  # Write information in text file
  out_folder          <- '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/'
  write.table(precip_avg_sd_annual,file=paste(out_folder,out_fname,sep=''), row.names=F ,col.names=T, quote=F, sep='\t')
  
  # Make a list
  precip              <- list(precip_monthly, precip_annual)
  names(precip)       <- c('Monthly', 'Annual')
  
  return(precip) 
}

#_______________________________________________________________________________
# Read mean annual temperature
read_basin_mean_annual_temp <- function(in.folder, filename, min_yr){
  
  # Read daily temperature [deg C]
  temp_daily        <- read.csv(paste(in.folder, filename, sep=''),header=T,sep='\t')
  
  # Melt data to long form
  temp_daily        <- gather(temp_daily, key=Station, value=temp, -Date)
  
  # Added column for Year, Month and Day
  temp_daily$Date   <- as.Date(as.character(temp_daily$Date), '%Y-%m-%d')
  temp_daily$Year   <- as.numeric(format(temp_daily$Date,'%Y'))
  temp_daily$Month  <- as.numeric(format(temp_daily$Date,'%m'))
  temp_daily$Day    <- as.numeric(format(temp_daily$Date,'%d'))
  
  # Only keep values since min_yr
  temp_daily        <- temp_daily[which(temp_daily$Year >= min_yr),]
  
  # Add CWC_basin
  temp_daily$CWC_basin <- basin_labeller('variable', temp_daily$Station)
  
  # # Add column for station type
  # temp_daily$Station <- 'Full basin'
  
  # Calculate monthly temperature
  temp_monthly      <- group_by(temp_daily, Year, Month, CWC_basin, Station) %>%  # group by first
                       summarise(temp = mean(temp, na.rm=TRUE))                   # then summarise using fun mean
  
  # Calculate annual temperature
  temp_annual       <- group_by(temp_daily, Year, CWC_basin, Station) %>%  # group by first
                       summarise(temp = mean(temp, na.rm=TRUE))            # then summarise using fun mean
  
  # Temperature data is NaN from 1963-1972 for East_Krishna_Pennar, East_Mahanadi_Pennar, Godavari, Mahi,
  # Sabaramati, Tapi, West_Kutch_Saurashtra, and West_Tapi_Tadri
  # print(spread(temp_annual[,c('Year','Station','temp')], key=Station, value=temp))
  
  # Make a list
  temp              <- list(temp_monthly, temp_annual)
  names(temp)       <- c('Monthly', 'Annual')
  
  return(temp) 
}

#_______________________________________________________________________________
# Read annual discharge, DIN conc, and DIN flux for various CWC basins based on CWC WQ station data
read_CWC_sta_basin_annual_discharge_conc_flux <- function(in.folder, f.name){
  
  select.sta.loc   <- read.csv(paste(in.folder,f.name,sep=''), header=T, sep='\t')
  
  # Load R object containing list of CWC water quality data
  load(paste(in.folder,'WRIS_waterquality_data.RData',sep=''))
  
  # Collpase list into a single data frame
  wris.wq.list      <- bind_rows(wris.wq.list)
  
  # Only keep water quality data for the selected stations
  wris.wq.list      <- wris.wq.list[which(wris.wq.list$Site_code %in% select.sta.loc$Site_code),]
  
  # Only keep few columns for plotting
  wris.wq.list      <- select(wris.wq.list, Date, Year, Month, JulianDay, Station, Site_code, CWC_basin, Discharge_m3s, DIN_mgNl, DIN_kgNday)
  
  # Estimate mean discharge, DIN conc, and DIN flux for various months
  wris.wq.monthly.mean <- group_by(wris.wq.list, Year, Month, Station, CWC_basin) %>%       # group by first
                          summarise(Discharge_m3s = mean(Discharge_m3s, na.rm=TRUE),  # then summarise using fun mean
                                    DIN_mgNl      = mean(DIN_mgNl,      na.rm=TRUE),
                                    DIN_kgNday    = mean(DIN_kgNday,    na.rm=TRUE))        
  
  # Estimate mean annual discharge, DIN conc, and DIN flux 
  wris.wq.annual.mean <- group_by(wris.wq.monthly.mean, Year, Station, CWC_basin) %>%  # group by first
                         summarise(Discharge_m3s = mean(Discharge_m3s, na.rm=TRUE),   # then summarise using fun mean
                                   DIN_mgNl      = mean(DIN_mgNl,      na.rm=TRUE),
                                   DIN_kgNday    = mean(DIN_kgNday,    na.rm=TRUE)) 
  
  return(wris.wq.annual.mean)
  
}

#_______________________________________________________________________________
# Read total annual discharge for various CWC basins based on Hydro observation station data
read_hydro_sta_basin_annual_discharge <- function(in.folder, select.sta.loc, min_yr, max_yr){
  
  # Load R object containing list of CWC water quality data
  load(paste(in.folder,'WRIS_hydro_data.RData',sep=''))
  
  # Collpase list into a single data frame
  wris.hydro.list   <- bind_rows(wris.hydro.list)
  
  # Only keep water quality data for the selected stations
  wris.hydro.list   <- wris.hydro.list[which(wris.hydro.list$Site_code %in% select.sta.loc$Site_code),]
  
  # Only keep few columns for plotting
  wris.hydro.list   <- select(wris.hydro.list, Date, Year, Month, JulianDay, Station, Site_code, CWC_basin, Discharge_m3s)
  
  # Estimate total discharge for various months
  wris.hydro.monthly.tot <- group_by(wris.hydro.list, Year, Month, Station, CWC_basin) %>%  # group by first
                            summarise(Discharge_m3s = sum(Discharge_m3s, na.rm=TRUE))       # then summarise using fun sum
  
  # Only keep values since min_yr
  wris.hydro.monthly.tot    <- wris.hydro.monthly.tot[which(wris.hydro.monthly.tot$Year >= min_yr & wris.hydro.monthly.tot$Year <= max_yr),]
  
  # Estimate total annual discharge, DIN conc, and DIN flux 
  wris.hydro.annual.tot <- group_by(wris.hydro.monthly.tot, Year, Station, CWC_basin) %>%  # group by first
                           summarise(Discharge_m3s_JJAS = sum(Discharge_m3s[which(Month %in% c(6,7,8,9))], na.rm=TRUE),
                                     Discharge_m3s      = sum(Discharge_m3s, na.rm=TRUE))       # then summarise using fun sum
  # Make a list
  wris.hydro              <- list(wris.hydro.monthly.tot, wris.hydro.annual.tot)
  names(wris.hydro)       <- c('Monthly', 'Annual')
  
  return(wris.hydro)
  
}

#_______________________________________________________________________________
# Read total annual DIN flux [kg N/km2/yr] for various CWC basins based on WRTDS model application
read_annual_DIN_flux <- function(in.folder, select.sta.loc){
  
  # Remove Garudeshwar from station list since WRTDS could not be applied to obtain annual DIN flux
  select.sta.loc <- select.sta.loc[-which(select.sta.loc$Station %in% c('Khanpur','Nellore','Vautha','Sarangkheda')),]
  
  # Empty list for storing annual DIN flux
  DIN.flux       <- list()
  
  for (ind in 1:nrow(select.sta.loc)){
      
    filename     <- paste(in.folder,select.sta.loc$Station[ind],'_Annual_DIN_Estimated_load.txt',sep='')
    
    # Read total annual load [kg N/yr]
    x.inv <- try(read.csv(filename,header=T,sep='\t'), silent=TRUE)
    if (!'try-error' %in% class(x.inv)) {
      DIN.flux[[ind]]                    <- x.inv
      DIN.flux[[ind]]['WRTDS_DIN_flux'] <- DIN.flux[[ind]]['Load']/select.sta.loc[ind, 'Drainage_area_km2']  # DIN flux [kg N/km2/yr]
      DIN.flux[[ind]]['Station']         <- as.character(select.sta.loc[ind, 'Station'])
      DIN.flux[[ind]]['CWC_basin']       <- as.character(select.sta.loc[ind, 'CWC_basin'])
    }
  }  # for loop ends

  # Collpase list into a single data frame
  DIN.flux      <- bind_rows(DIN.flux)

  # Only keep rows with more than 6 water quality samples in a year
  DIN.flux      <- DIN.flux[which(DIN.flux$Count_obs >=6),]
  
  print(table(DIN.flux$Year, DIN.flux$Station))

  # Remove Musiri from station list since WRTDS could not be applied to obtain annual DIN flux
  DIN.flux             <- DIN.flux[-which(DIN.flux$Station %in% c('Musiri')),]
  
  # Reorder factor levels
  DIN.flux$Station     <- factor(DIN.flux$Station, levels=c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                            'Polavaram','Urachikottai','Vijayawada'))
  DIN.flux$CWC_basin   <- factor(DIN.flux$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                              'Godavari','Cauvery','Krishna'))
  
  
  # Define path to output file
  out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/CWC_basins_annual_DIN_load.pdf',sep='')
  
  # Delete existing file
  unlink(out.f.name)
  # Start pdf device driver for saving plots
  pdf(out.f.name, height=8.5,width=11) 
  
  # Make histogram of annual DIN load
  p1 <- ggplot(data=DIN.flux, aes(WRTDS_DIN_flux)) + 
    geom_histogram() +
    facet_wrap(~ CWC_basin, nrow=3) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=15,family='Helvetica',color='black'),
          strip.text        = element_text(size=15,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),   # Remove background color of facet label but keep black border
          plot.title        = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=15,family='Helvetica',color='black'), 
          axis.title.x      = element_blank(),
          axis.text.x       = element_text(size=15,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black')) +
    labs(y    =expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title=NULL)
  
  print(p1)
  
  uniq_stations <- unique(DIN.flux$Station)
  
  for (ind in 1:length(uniq_stations)) {
    
    plot_data <- DIN.flux[which(DIN.flux$Station == uniq_stations[ind]),]
    
    # ---------- mean and sd value for labelling ---
    mean_sd   <-  data.frame('mean'= paste('mean==',round(mean(plot_data$Load/(10^3)),0),sep=''),
                             'sd'  = paste('sd==',  round(sd(plot_data$Load/(10^3)),0),  sep=''),
                             'CV'  = paste('CV==',  round(sd(plot_data$Load/(10^3))/mean(plot_data$Load/(10^3)),2),  sep=''),
                             'min' = paste('min==', round(min(plot_data$Load/(10^3)),0),sep=''),
                             'max' = paste('max==', round(max(plot_data$Load/(10^3)),0),sep=''))
    mean_sd$x <- -Inf
    mean_sd$y <- max(plot_data$Load/(10^3))
    
    # Make bar plot of annual DIN load
    p2 <- ggplot(data=plot_data, aes(x=Year, y=Load/(10^3))) + 
      geom_bar(stat='identity', position=position_dodge(), width=0.5, fill='#f8766d') + 
      geom_text(data=mean_sd, aes(x=x, y=y,      label=mean), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      geom_text(data=mean_sd, aes(x=x, y=0.95*y, label=sd),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      geom_text(data=mean_sd, aes(x=x, y=0.9*y,  label=CV),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      geom_text(data=mean_sd, aes(x=x, y=0.85*y, label=min),  hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      geom_text(data=mean_sd, aes(x=x, y=0.8*y,  label=max),  hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      scale_x_continuous(breaks=seq(from=1980, to=2015, by=3), expand=c(0.01,0.01)) +
      scale_y_continuous(expand=c(0.01,0.01)) +
      theme_bw() +  # remove background
      theme(panel.grid        = element_blank(),          # Remove all grid lines
            text              = element_text(size=20,family='Helvetica',color='black'),
            plot.title        = element_text(size=20,family='Helvetica',color='black'),
            axis.title.y      = element_text(size=20,family='Helvetica',color='black'), 
            axis.title.x      = element_blank(),
            axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
            axis.text.y       = element_text(size=20,family='Helvetica',color='black')) +
      labs(y    =expression(paste(Dissolved~inorganic~nitrogen~load~'[',Mg~N~yr^-1,']')),
           title=paste(uniq_stations[ind], unique(plot_data$CWC_basin),sep=' - '))
    
    print(p2)
    
    # ---------- mean and sd value for labelling ---
    mean_sd <-  data.frame('mean'= paste('mean==',round(mean(plot_data$WRTDS_DIN_flux),0),sep=''),
                           'sd'  = paste('sd==',  round(sd(plot_data$WRTDS_DIN_flux),0),  sep=''),
                           'CV'  = paste('CV==',  round(sd(plot_data$WRTDS_DIN_flux)/mean(plot_data$WRTDS_DIN_flux),2),  sep=''),
                           'min' = paste('min==', round(min(plot_data$WRTDS_DIN_flux),0),sep=''),
                           'max' = paste('max==', round(max(plot_data$WRTDS_DIN_flux),0),sep=''))
    mean_sd$x <- -Inf
    mean_sd$y <- max(plot_data$WRTDS_DIN_flux)
    
    p3 <- ggplot(data=plot_data, aes(x=Year, y=WRTDS_DIN_flux)) + 
      geom_bar(stat='identity', position=position_dodge(), width=0.5, fill='#f8766d') + 
      geom_text(data=mean_sd, aes(x=x, y=y,      label=mean), hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      geom_text(data=mean_sd, aes(x=x, y=0.95*y, label=sd),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      geom_text(data=mean_sd, aes(x=x, y=0.9*y,  label=CV),   hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      geom_text(data=mean_sd, aes(x=x, y=0.85*y, label=min),  hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      geom_text(data=mean_sd, aes(x=x, y=0.8*y,  label=max),  hjust=0, vjust=1, parse=TRUE, inherit.aes=FALSE, size=7) +
      scale_x_continuous(breaks=seq(from=1980, to=2015, by=3), expand=c(0.01,0.01)) +
      scale_y_continuous(expand=c(0.01,0.01)) +
      theme_bw() +  # remove background
      theme(panel.grid        = element_blank(),          # Remove all grid lines
            text              = element_text(size=20,family='Helvetica',color='black'),
            plot.title        = element_text(size=20,family='Helvetica',color='black'),
            axis.title.y      = element_text(size=20,family='Helvetica',color='black'), 
            axis.title.x      = element_blank(),
            axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
            axis.text.y       = element_text(size=20,family='Helvetica',color='black')) +
      labs(y=expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),title=paste(uniq_stations[ind], unique(plot_data$CWC_basin),sep=' - '))
    
    print(p3)
    
  }
  
  dev.off()
  

  # Identify which combination of year and CWC basins are missing
  dummy                <- expand(DIN.flux, Year, CWC_basin) %>% anti_join(DIN.flux[,c('Year','Station','CWC_basin')])
  dummy$WRTDS_DIN_flux <- 2000
  dummy[which(dummy$CWC_basin %in% c('Mahanadi','Godavari','Cauvery')),'WRTDS_DIN_flux'] <- 800
  dummy[which(dummy$CWC_basin %in% c('Krishna')),'WRTDS_DIN_flux'] <- 800
  
  dummy$Load           <- 50
  dummy[which(dummy$CWC_basin %in% c('Narmada','Godavari')),'Load'] <- 150
  dummy[which(dummy$CWC_basin %in% c('Mahanadi')),'Load'] <- 100
  
  basin_labels         <-  group_by(DIN.flux, Station) %>% 
                           summarise(CWC_basin = unique(CWC_basin),
                                     'CV'      = paste('~~~~~CV==', round(sd(WRTDS_DIN_flux)/mean(WRTDS_DIN_flux),2), sep=''))
  
  # # Merge basin_labels and dummy data to add the y limits to basin_labels
  basin_labels         <- left_join(basin_labels, unique(dummy[,c('CWC_basin','WRTDS_DIN_flux','Load')]))
  
  # Reorder factor levels
  basin_labels$CWC_basin   <- factor(basin_labels$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                      'Godavari','Cauvery','Krishna'))
  
  # Define path to output file
  out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/CWC_basins_annual_DIN_load_2_basins.pdf',sep='')
  
  # Delete existing file
  unlink(out.f.name)
  # Start pdf device driver for saving plots
  pdf(out.f.name, height=5,width=8.5) 
  
  p4 <- ggplot(data=DIN.flux[which(DIN.flux$CWC_basin %in% c('Brahmani and Baitarni','Krishna')),], 
               aes(x=Year, y=WRTDS_DIN_flux)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75, fill='grey50') + 
    geom_blank(data=dummy[which(dummy$CWC_basin %in% c('Brahmani and Baitarni','Krishna')),], 
               aes(x=Year, y=WRTDS_DIN_flux))   + # Adding blank geom to ensure required axis
    geom_bar(data=dummy[which(dummy$CWC_basin %in% c('Brahmani and Baitarni','Krishna')),], 
             aes(x=Year, y=WRTDS_DIN_flux), stat='identity', position=position_dodge(), width=0.5, fill='grey90') + # Add dummy data for ensuring axis and marking year with NA values
    geom_text(data=basin_labels[which(basin_labels$CWC_basin %in% c('Brahmani and Baitarni','Krishna')),],
              aes(x=1982, y=Inf, label=CWC_basin), vjust=1.4, hjust=0, col='black', size=7) +
    # geom_text(data=basin_labels[which(basin_labels$CWC_basin %in% c('Brahmani and Baitarni','Krishna')),], 
    #           aes(x=1981, y=WRTDS_DIN_flux,     label=CWC_basin), hjust=0, vjust=1.2, size=7) +
    # geom_text(data=basin_labels[which(basin_labels$CWC_basin %in% c('Brahmani and Baitarni','Krishna')),], 
    #           aes(x=1981, y=0.9*WRTDS_DIN_flux, label=CV),        hjust=0, vjust=1.2, parse=TRUE, inherit.aes=FALSE, size=7) +
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller=label_wrap_gen(width=15)) +  # line wrapping for label
    geom_vline(xintercept=c(1989.5, 1999.5, 2009.5), color='black') + 
    labs(y    =expression(atop(Dissolved~inorganic~nitrogen, paste(flux,' [',kg~N~km^-2~yr^-1,']'))),
         # y    =expression(paste(DIN~flux~'[',kg~N~km^-2~yr^-1,']')),
         x    ='Year',
         title=NULL) +
    scale_x_continuous(expand=c(0,0), breaks=seq(1980,2015,5)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          panel.spacing     = unit(1.2,'lines'),
          text              = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_blank(),  # Remove strip background
          strip.text        = element_blank(),  # Remove strip text
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'), 
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'),
          plot.margin       = unit(c(8, 20, 8, 8), 'pt'))
  
  print(p4)
  
  dev.off()
  
  # Write information in text file
  out.folder              <- '~/Documents/repos/india_wq/Figures/'
  out.fname               <- 'CWC_basins_annual_DIN_load_2.txt'
  out.data                <- DIN.flux[,c('Year','Station','CWC_basin','WRTDS_DIN_flux')]
  out.data$WRTDS_DIN_flux <- round(out.data$WRTDS_DIN_flux, 0)
  write.table(out.data, file=paste(out.folder, out.fname,sep=''), row.names=F, col.names=T, quote=F, sep='\t')
  
  # Define path to output file
  out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/CWC_basins_annual_DIN_load_2.pdf',sep='')
  
  # Delete existing file
  unlink(out.f.name)
  # Start pdf device driver for saving plots
  pdf(out.f.name, height=17,width=11) 
  
  # Read DIN flux values available in literature for different CWC basins
  DIN.flux.lit.values             <- read.csv('~/Documents/repos/india_wq/DIN_flux_lit_review.txt',header=T,sep='\t')
  
  # Convert from factor to character
  DIN.flux.lit.values$CWC_basin   <- as.character(DIN.flux.lit.values$CWC_basin)
  DIN.flux.lit.values$Source      <- as.character(DIN.flux.lit.values$Source)
  DIN.flux.lit.values$Station     <- NA
  DIN.flux.lit.values[which(DIN.flux.lit.values$CWC_basin == 'Subernarekha'),'Station']          <- 'Ghatsila'
  DIN.flux.lit.values[which(DIN.flux.lit.values$CWC_basin == 'Brahmani and Baitarni'),'Station'] <- 'Jenapur'
  DIN.flux.lit.values[which(DIN.flux.lit.values$CWC_basin == 'Mahanadi'),'Station']              <- 'Tikarapara'
  DIN.flux.lit.values[which(DIN.flux.lit.values$CWC_basin == 'Godavari'),'Station']              <- 'Polavaram'
  DIN.flux.lit.values[which(DIN.flux.lit.values$CWC_basin == 'Krishna'), 'Station']              <- 'Vijayawada'
  DIN.flux.lit.values[which(DIN.flux.lit.values$CWC_basin == 'Cauvery'), 'Station']              <- 'Urachikottai'
  DIN.flux.lit.values[which(DIN.flux.lit.values$CWC_basin == 'Narmada'), 'Station']              <- 'Mandleshwar'
  
  # Add source column
  DIN.flux$Source          <- 'This study'
  
  # Merge the two tibbles
  plot_data                       <- bind_rows(DIN.flux, DIN.flux.lit.values)
  
  # Reorder factor levels
  plot_data$Source                <- factor(plot_data$Source, levels=c('This study','Subramanian et al., 2010',
                                                                       'Seitzinger et al., 2014','Lambs et al., 2005','Krishna et al., 2016'))
  plot_data$CWC_basin             <- factor(plot_data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                          'Godavari','Cauvery','Krishna'))
  
  
  print('Values not plotted to ensure scale is not exceeded')
  print(plot_data[which(plot_data$Source != 'This study' & plot_data$DIN_flux_kgN_km2_yr > 2000),])
  
  basin_labels$labels  <- c('A) Brahmani and Baitarni', 'B) Subernarekha', 'C) Narmada',
                            'D) Mahanadi', 'E) Godavari', 'F) Cauvery', 'G) Krishna')
  
  p5 <- ggplot(data=plot_data[which(plot_data$Source == 'This study'),], aes(x=Year, y=WRTDS_DIN_flux)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75, fill='grey50') + 
    geom_blank(data=dummy, aes(x=Year, y=WRTDS_DIN_flux))   + # Adding blank geom to ensure required axis
    geom_bar(data=dummy, aes(x=Year, y=WRTDS_DIN_flux), stat='identity', position=position_dodge(), width=0.5, fill='grey90') + # Add dummy data for ensuring axis and marking year with NA values
    geom_text(data=basin_labels, aes(x=1981, y=WRTDS_DIN_flux,      label=labels), hjust=0, vjust=1.4, size=7) +
    geom_text(data=basin_labels, aes(x=1981, y=0.85*WRTDS_DIN_flux, label=CV),     hjust=0, vjust=1.4, parse=TRUE, inherit.aes=FALSE, size=7) +
    geom_point(data=plot_data[which(plot_data$Source != 'This study' & plot_data$DIN_flux_kgN_km2_yr < 2000),], aes(x=Year, y=DIN_flux_kgN_km2_yr, shape=Source), size=7, fill='black', color='black') +
    # facet_wrap(~ Station, scales='free_y', ncol=1, strip.position='right') +
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller=label_wrap_gen(width=15)) +  # line wrapping for label
    geom_vline(xintercept=c(1989.5, 1999.5, 2009.5), color='black') + 
    guides(shape=guide_legend(title=NULL, nrow=2)) +
    labs(y    =expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         x    ='Year',
         title=NULL) +
    scale_shape_manual(values=c(15,19,17,23)) +
    scale_x_continuous(expand=c(0,0), breaks=seq(1980,2015,5)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          panel.spacing     = unit(1.2,'lines'),
          text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_blank(),  # Remove strip background
          strip.text        = element_blank(),  # Remove strip text
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'), 
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p5)
  
  p6 <- ggplot(data=DIN.flux, aes(x=Year, y=Load/(10^6))) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75, fill='grey50') + 
    geom_blank(data=dummy, aes(x=Year, y=Load))   + # Adding blank geom to ensure required axis
    geom_bar(data=dummy, aes(x=Year, y=Load), stat='identity', position=position_dodge(), width=0.5, fill='grey90') + # Add dummy data for ensuring axis and marking year with NA values
    # geom_text(data=basin_labels, aes(x=1981, y=Inf,       label=labels), hjust=0, vjust=1.2, col='black', size=7) +
    geom_text(data=basin_labels, aes(x=1981, y=Load,      label=labels), hjust=0, vjust=1.4, size=7) +
    geom_text(data=basin_labels, aes(x=1981, y=0.85*Load, label=CV),     hjust=0, vjust=1.4, parse=TRUE, inherit.aes=FALSE, size=7) +
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller=label_wrap_gen(width=15)) +  # line wrapping for label
    geom_vline(xintercept=c(1989.5, 1999.5, 2009.5), color='black') + 
    labs(y    =expression(paste(Dissolved~inorganic~nitrogen~load~'[',Gg~N~yr^-1,']')),
         x    ='Year',
         title=NULL) +
    scale_x_continuous(expand=c(0,0), breaks=seq(1980,2015,5)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          panel.spacing     = unit(1.2,'lines'),
          text              = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_blank(),  # Remove strip background
          strip.text        = element_blank(),  # Remove strip text
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'), 
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'), 
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p6)
  
  dev.off()
  
  # Define path to output file
  out.f.name    <- paste(in.folder= '~/Documents/repos/india_wq/','Figures/CWC_basins_annual_DIN_load_lit_comp.pdf',sep='')
  
  # Delete existing file
  unlink(out.f.name)
  # Start pdf device driver for saving plots
  pdf(out.f.name, height=24,width=11) 
  
  # Remove Musiri from station list since WRTDS could not be applied to obtain annual DIN flux
  DIN.flux.select                 <- DIN.flux[-which(DIN.flux$Station %in% c('Musiri', 'Ghatsila')), c('Year','Station','CWC_basin','WRTDS_DIN_flux')]
  
  # Add average values
  DIN.flux.select <- bind_rows(DIN.flux.select, 
                           group_by(DIN.flux.select, Station, CWC_basin) %>% 
                             summarise(Year = 2015,  # 2015 stores the average across all years
                                       WRTDS_DIN_flux = mean(WRTDS_DIN_flux, na.rm=TRUE)) )
  # Rename column
  colnames(DIN.flux.select)[which(colnames(DIN.flux.select) == 'WRTDS_DIN_flux')] <- 'DIN_flux_kgN_km2_yr'
  
  # Add source column
  DIN.flux.select$Source          <- 'This study'
  
  # Merge the two tibbles
  plot_data                       <- bind_rows(DIN.flux.select, DIN.flux.lit.values)
  
  # Reorder factor levels
  plot_data$Station               <- factor(plot_data$Station, levels=c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                                        'Polavaram','Urachikottai','Vijayawada'))
  plot_data$Source                <- factor(plot_data$Source, levels=c('This study','Subramanian et al., 2010',
                                                                       'Seitzinger et al., 2014','Lambs et al., 2005','Krishna et al., 2016'))
  plot_data$CWC_basin             <- factor(plot_data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                          'Godavari','Cauvery','Krishna'))
  
  # For splitting the y-axis into large and small values
  plot_data$break_y_axis                                                   <- 'No'
  plot_data[which(plot_data$DIN_flux_kgN_km2_yr > 2000), 'break_y_axis']   <- 'Yes'
  
  dummy$Source <- 'This study'
  dummy$Source <- factor(dummy$Source, levels=c('This study','Subramanian et al., 2010',
                                                'Seitzinger et al., 2014','Lambs et al., 2005','Krishna et al., 2016'))
  
  p7 <- ggplot(data=plot_data, aes(x=Year, y=DIN_flux_kgN_km2_yr, fill=Source)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75) + 
    geom_text(data=plot_data[which(plot_data$break_y_axis == 'Yes'),], aes(x=Year+0.5, y=1750, label=round(DIN_flux_kgN_km2_yr,0)), hjust=0, vjust=1, size=7, color='blue') +
    # facet_wrap(~ Station, ncol=1, strip.position='right') +
    facet_wrap(~ CWC_basin, ncol=1, strip.position='right') +
    guides(fill=guide_legend(title=NULL, nrow=2)) + 
    labs(y    =expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title=NULL) +
    scale_fill_manual(values=c('grey75','red','blue','darkgreen','orange')) +
    scale_x_continuous(expand=c(0,0), breaks=seq(1980,2015,5)) +
    scale_y_continuous(expand=c(0,0), breaks=seq(0,1750,250)) +
    coord_cartesian(ylim=c(0,1750)) +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          panel.spacing     = unit(1.2,'lines'),
          text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),   # Remove background color of facet label but keep black border
          axis.title.x      = element_blank(),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'), 
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p7)
  
  tmp_plot_data                                                                   <- plot_data
  tmp_plot_data[which(tmp_plot_data$break_y_axis == 'Yes'), 'DIN_flux_kgN_km2_yr'] <- NA
  
  p8 <- ggplot(data=tmp_plot_data, aes(x=Year, y=DIN_flux_kgN_km2_yr, fill=Source)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75) + 
    # facet_grid(Station ~ .) +
    facet_grid(CWC_basin ~ .) +
    guides(fill=guide_legend(title=NULL, nrow=2)) + 
    labs(y    =expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title=NULL) +
    scale_fill_manual(values=c('grey75','red','blue','darkgreen','orange')) +
    scale_x_continuous(expand=c(0,0), breaks=seq(1980,2015,5)) +
    scale_y_continuous(expand=c(0,0)) +
    coord_flip() +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_blank(),
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'), 
          axis.title.y      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=15,family='Helvetica',color='black'))
  
  tmp_plot_data                                                                   <- plot_data
  tmp_plot_data[which(tmp_plot_data$break_y_axis == 'No'), 'DIN_flux_kgN_km2_yr'] <- NA
  
  p9 <- ggplot(data=tmp_plot_data, aes(x=Year, y=DIN_flux_kgN_km2_yr, fill=Source)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75) + 
    # facet_grid(Station ~ .) +
    facet_grid(CWC_basin ~ .) +
    guides(fill=guide_legend(title=NULL)) + 
    labs(y    =expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title=NULL) +
    scale_fill_manual(values=c('grey75','red','blue','darkgreen','orange')) +
    scale_x_continuous(expand=c(0,0), breaks=seq(1980,2015,5)) +
    scale_y_continuous(expand=c(0,0), breaks=c(3000, 5000, 7000)) +
    coord_flip(ylim=c(3000,7250)) +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1),
          panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),   # Remove background color of facet label but keep black border
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'), 
          axis.title.y      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_blank(),
          axis.ticks.y      = element_blank())
  
  grid.arrange(p8, p9, nrow = 1, widths=c(2,1))

  # Summarise mean and sd of DIN flux for various wq stations
  DIN.flux.select                 <- group_by(DIN.flux.select, Station, CWC_basin) %>%
                                     summarise(sd_DIN_flux         = sd(DIN_flux_kgN_km2_yr),
                                               DIN_flux_kgN_km2_yr = mean(DIN_flux_kgN_km2_yr),
                                               Source              = 'This study')
  DIN.flux.lit.values$sd_DIN_flux <- NA
  DIN.flux.lit.values$Year        <- NULL

  # Merge the two tibbles
  plot_data                       <- bind_rows(DIN.flux.select, DIN.flux.lit.values)
  
  # Reorder factor levels
  plot_data$CWC_basin             <- factor(plot_data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                          'Godavari','Cauvery','Krishna'))
  
  
  p10 <- ggplot(data=plot_data, aes(x=Source, y=DIN_flux_kgN_km2_yr, fill=Source)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75) + 
    geom_errorbar(aes(ymin = DIN_flux_kgN_km2_yr - sd_DIN_flux, ymax = DIN_flux_kgN_km2_yr + sd_DIN_flux), width=0.75) + 
    geom_text(aes(label=round(DIN_flux_kgN_km2_yr,0)), hjust=1, vjust=0.5, size=5) +
    # facet_grid(Station ~ .) +
    facet_grid(CWC_basin ~ .) +
    labs(y    =expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~yr^-1,']')),
         title=NULL) +
    coord_flip() +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'none',
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),   # Remove background color of facet label but keep black border
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'), 
          axis.title.y      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p10)
  
  dev.off()
  
  # Drop columns that are not needed for plotting
  DIN.flux$Load          <- NULL
  DIN.flux$Count_obs     <- NULL
  DIN.flux$Flux_per_err  <- NULL
  
  return(DIN.flux)
  
}