# Author - Eva Sinha, Stanford University, esinha@stanford.edu
# Date   - 7th June, 2018
#
# Function details
# read_monthly_DIN_flux    - Read monthly DIN flux [kg N/km2/month] for various CWC basins based on WRTDS model application
# plot_monthly_data        - Plot monthly DIN flux, precip, and discharge for the seven water quality stations


library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)

source('~/Documents/repos/india_wq/plot_annual_timeseries.R')
source('~/Documents/repos/india_wq/india_color_pal.R')

#_______________________________________________________________________________
# Read monthly DIN flux [kg N/km2/month] for various CWC basins based on WRTDS model application
read_monthly_DIN_flux <- function(in.folder, wq_stations){
  
  # Empty list for storing monthly DIN flux
  DIN.mon.flux <- list()
  
  for (ind in 1:nrow(wq_stations)){
    
    filename   <- paste(in.folder,wq_stations$Station[ind],'_Monthly_DIN_Estimated_load.txt',sep='')
    
    # Read total monthly load [kg N/month]
    x.inv      <- try(read.csv(filename,header=T,sep='\t'), silent=TRUE)
    if (!'try-error' %in% class(x.inv)) {
      DIN.mon.flux[[ind]]                          <- x.inv
      DIN.mon.flux[[ind]]['DIN_flux_kgN_km2_mon']  <- DIN.mon.flux[[ind]]['Load']/wq_stations[ind, 'Drainage_area_km2']  # DIN flux [kg N/km2/month]
      DIN.mon.flux[[ind]]['Station']               <- as.character(wq_stations[ind, 'Station'])
      DIN.mon.flux[[ind]]['CWC_basin']             <- as.character(wq_stations[ind, 'CWC_basin'])
    }
  }  # for loop ends
  
  # Collpase list into a single data frame
  DIN.mon.flux <- bind_rows(DIN.mon.flux)
  
  print(table(DIN.mon.flux$Year, DIN.mon.flux$Station))
  
  return(DIN.mon.flux)
 
}

# ______________________________________________________________________________
param_labeller_monthly <- function(var, value){
  
  value <- as.character(value)
  
  if (var=='variable') {

    value[value=='DIN_flux_kgN_km2_mon']  <- 'italic(Q)[DIN]~(kg~N~km^-2~month^-1)'
    value[value=='Disch_mm_mon']          <- 'italic(Q)~(mm~month^-1)'
    value[value=='precip']                <- 'italic(P)~(mm~month^-1)'
  }
  return(value)
}

#_______________________________________________________________________________
# Plot monthly DIN flux, precip, and discharge for the seven water quality stations
plot_monthly_data <- function(monthly.data, monthly_variable, y_label, y_axis_breaks){
  
  # Update column name for LSC variable
  colnames(monthly.data)[which(colnames(monthly.data)==monthly_variable)]   <- 'monthly_var'
  
  # Reorder factor levels
  monthly.data$Month         <- factor(monthly.data$Month,     levels=month.abb)
  monthly.data$Station       <- factor(monthly.data$Station,   levels=c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                                        'Polavaram','Urachikottai','Vijayawada'))
  monthly.data$CWC_basin     <- factor(monthly.data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                        'Godavari','Cauvery','Krishna'))
  
  print('Percentage in June, July, August, and September')
  tmp                        <- group_by(monthly.data, Station, CWC_basin) %>% 
                                summarise(per_JJAS = 100*sum(monthly_var[which(Month %in% c('Jun','Jul','Aug','Sep'))])/sum(monthly_var))
  print(tmp)
  
  print('Percentage in June, July, August, September, and October')
  tmp                        <- group_by(monthly.data, Station, CWC_basin) %>% 
                                summarise(per_JJAS = 100*sum(monthly_var[which(Month %in% c('Jun','Jul','Aug','Sep','Oct'))])/sum(monthly_var))
  print(tmp)
  
  print('Monthly average, minimum, and maximum for various stations')
  tmp                        <- group_by(monthly.data, Station, CWC_basin) %>%
                                summarise(var_mean = mean(monthly_var),
                                          var_min  = min(monthly_var),
                                          var_max  = max(monthly_var))
  print(tmp)
        
  print('Jun, July, Aug, Sep,and Oct monthly average, minimum, and maximum for various stations')
  tmp                        <- group_by(monthly.data[which(monthly.data$Month %in% c('Jun','Jul','Aug','Sep','Oct')),], Station, CWC_basin) %>%
                                summarise(var_mean = mean(monthly_var),
                                          var_min  = min(monthly_var),
                                          var_max  = max(monthly_var))
  print(tmp)
  
  print('Month with highest monthly')
  tmp                        <- group_by(monthly.data, Station, CWC_basin) %>%
                                summarise(max_month = Month[which.max(monthly_var)])
  print(tmp)
  
  print('Average monthly for various stations')
  tmp                        <- group_by(monthly.data, Station, CWC_basin, Month) %>%
                                summarise(var_avg = mean(monthly_var))
  print(as.data.frame(spread(tmp, key=Month, value=var_avg)))
  
  print('Month with highest average monthly')
  tmp2                       <- group_by(tmp, Station, CWC_basin) %>%
                                summarise(max_month = Month[which.max(var_avg)])
  print(tmp2)
  
  print('Median monthly for various stations')
  tmp                        <- group_by(monthly.data, Station, CWC_basin, Month) %>%
                                summarise(var_median = median(monthly_var))
  print(as.data.frame(spread(tmp, key=Month, value=var_median)))
  
  print('Month with highest median monthly')
  tmp2                       <- group_by(tmp, Station, CWC_basin) %>%
                                summarise(max_median_month = Month[which.max(var_median)])
  print(tmp2)
  
  dummy                      <- group_by(monthly.data, Station, CWC_basin) %>%
                                summarise(monthly_var = max(monthly_var),
                                          Month                = 'Jun')
  # print(dummy)
  dummy$monthly_var <- y_axis_breaks
  # print(dummy)
  
  basin_labels         <-  group_by(monthly.data, Station) %>% summarise(CWC_basin = unique(CWC_basin))
  basin_labels$labels  <- c('A) Brahmani and Baitarni', 'B) Subernarekha', 'C) Narmada',
                            'D) Mahanadi', 'E) Godavari', 'F) Cauvery', 'G) Krishna')
  
  p1 <- ggplot(data=monthly.data, aes(x=Month, y=monthly_var)) + 
    geom_boxplot() +
    geom_blank(data=dummy, aes(x=Month, y=monthly_var))   + # Adding blank geom to ensure required axis
    geom_text(data=basin_labels,aes(x=-Inf, y=Inf, label=labels), vjust=1.4, hjust=0.001, col='black', size=7) +
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) + # line wrapping for label
    labs(y     = y_label,
         x     = 'Month',
         title = NULL) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +  # remove background
    theme(panel.border      = element_rect(colour ='black',size=1.0),
          panel.grid        = element_blank(),          # Remove all grid lines
          panel.spacing     = unit(1.2,'lines'),
          text              = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_blank(),  # Remove strip background
          strip.text        = element_blank(),  # Remove strip text
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_text(size=20,family='Helvetica',color='black'),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p1)
  
  plot_data  <- monthly.data[which(monthly.data$Month %in% c('Jun','Jul','Aug','Sep')),]
  
  dummy      <- group_by(plot_data, Station, CWC_basin) %>%
                summarise(monthly_var = max(monthly_var),
                          Year        = 2005,
                          Month       = 'Jun')
  dummy$monthly_var <- y_axis_breaks
  
  p2 <- ggplot(data=plot_data, aes(x=Year, y=monthly_var, fill=Month)) + 
    geom_bar(stat='identity', position=position_dodge(), width=0.75) + 
    geom_blank(data=dummy, aes(x=Year, y=monthly_var))   + # Adding blank geom to ensure required axis
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) + # line wrapping for label
    guides(fill=guide_legend(title=NULL)) + 
    labs(y     = y_label,
         title = NULL) +
    scale_fill_manual(values=c('red','blue','orange','darkgreen')) +
    scale_x_continuous(expand=c(0,0), breaks=seq(1980,2015,3)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),   # Remove background color of facet label but keep black border
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p2)
  
}


#_______________________________________________________________________________
# Plot monthly DIN flux, precip, and discharge for the two water quality stations
plot_monthly_data_2_basins <- function(monthly.data, monthly_variable, y_label, y_axis_breaks){
  
  # Update column name for LSC variable
  colnames(monthly.data)[which(colnames(monthly.data)==monthly_variable)]   <- 'monthly_var'
  
  # Reorder factor levels
  monthly.data$Month         <- factor(monthly.data$Month,     levels=month.abb)
  monthly.data$Station       <- factor(monthly.data$Station,   levels=c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                                        'Polavaram','Urachikottai','Vijayawada'))
  monthly.data$CWC_basin     <- factor(monthly.data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                        'Godavari','Cauvery','Krishna'))
  
  # Only keep data for Brahmani and Baitarni and Krishna basin
  monthly.data               <- monthly.data[which(monthly.data$CWC_basin %in% c('Brahmani and Baitarni','Krishna')),]
  
  dummy                      <- group_by(monthly.data, Station, CWC_basin) %>%
                                summarise(monthly_var = max(monthly_var),
                                          Month       = 'Jun')
  
  dummy$monthly_var    <- y_axis_breaks
  
  basin_labels         <-  group_by(monthly.data, Station) %>% summarise(CWC_basin = unique(CWC_basin))

  p1 <- ggplot(data=monthly.data, aes(x=Month, y=monthly_var)) + 
    geom_boxplot() +
    geom_blank(data=dummy, aes(x=Month, y=monthly_var))   + # Adding blank geom to ensure required axis
    geom_text(data=basin_labels,aes(x='Jan', y=Inf, label=CWC_basin), vjust=1.2, hjust=0, col='black', size=7) +
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) + # line wrapping for label
    labs(y     = y_label,
         title = NULL) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_blank(),  # Remove strip background
          strip.text        = element_blank(),  # Remove strip text
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p1)
  
}

#_______________________________________________________________________________
# Summarise months with maximum DIN flux, discharge, and precipitation and make summary plot 
max_month_analysis <- function(DIN.mon.flux, hydro.monthly.tot, precip_monthly){
  
  # Combine into a single tibble using full join
  DIN_hydro_precip_monthly <- reduce(list(DIN.mon.flux, hydro.monthly.tot, precip_monthly),
                                     full_join, by = c('Year', 'Month', 'Station', 'CWC_basin'))
  
  print('Maximum monthly DIN flux, discharge and precip for each station')
  print(group_by(DIN_hydro_precip_monthly, Station, CWC_basin) %>% 
        summarise(max_flux   = max(DIN_flux_kgN_km2_mon, na.rm=TRUE),
                  max_disch  = max(Disch_mm_mon,         na.rm=TRUE), 
                  max_precip = max(precip,               na.rm=TRUE)))
  
  max_month       <- group_by(DIN_hydro_precip_monthly, Year, Station, CWC_basin) %>% 
                     summarise(max_flux_mon   = ifelse(length(which.max(DIN_flux_kgN_km2_mon)) == 0, 'NA', Month[which.max(DIN_flux_kgN_km2_mon)]),
                               max_disch_mon  = Month[which.max(Disch_mm_mon)], 
                               max_precip_mon = Month[which.max(precip)])
  
  # Replace 'NA' with NA
  max_month[max_month == 'NA'] <- NA
  
  count_max_month <- group_by(max_month, Station, CWC_basin) %>%
                     summarise(count_yrs              = length(na.omit(max_flux_mon)),
                               count_same_flux_disch  = sum(max_flux_mon==max_disch_mon, na.rm=TRUE),
                               count_same_flux_precip = sum(max_flux_mon==max_precip_mon,na.rm=TRUE))
  
  print('Summary of the number of times the month with maximum DIN flux matches
        month with maximum discharge and month with maximum precipitation')
  print(as.data.frame(count_max_month))
  
  print('Percentage of time month with maximum DIN flux matches month with maximum discharge')
  print(100*sum(count_max_month$count_same_flux_disch) / sum(count_max_month$count_yrs))
  
  print('Percentage of time month with maximum DIN flux matches month with maximum precip')
  print(100*sum(count_max_month$count_same_flux_precip) / sum(count_max_month$count_yrs))
  
  month_abb.name = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  max_month_num = max_month
  max_month_num$max_flux_mon   = as.numeric(match(max_month_num$max_flux_mon, month_abb.name))
  max_month_num$max_disch_mon  = as.numeric(match(max_month_num$max_disch_mon, month_abb.name))
  max_month_num$max_precip_mon = as.numeric(match(max_month_num$max_precip_mon, month_abb.name))
  max_month_num$lag_flux_disch   = max_month_num$max_flux_mon - max_month_num$max_disch_mon
  max_month_num$lag_flux_precip  = max_month_num$max_flux_mon - max_month_num$max_precip_mon
  max_month_num$lag_disch_precip = max_month_num$max_disch_mon - max_month_num$max_precip_mon

  
  count_max_month_num <- group_by(max_month_num, Station, CWC_basin) %>%
    summarise(count_DIN_yrs              = length(na.omit(max_flux_mon)),
              count_disch_yrs            = length(na.omit(max_disch_mon)),
              count_minus1mon_lag_flux_precip = sum(ifelse((lag_flux_precip == -1), 1, NA), na.rm=TRUE),
              count_0mon_lag_flux_precip      = sum(ifelse((lag_flux_precip == 0), 1, NA), na.rm=TRUE),
              count_1mon_lag_flux_precip      = sum(ifelse((lag_flux_precip == 1), 1, NA), na.rm=TRUE),
              count_2mon_lag_flux_precip      = sum(ifelse((lag_flux_precip == 2), 1, NA), na.rm=TRUE),
              count_minus1mon_lag_flux_disch  = sum(ifelse((lag_flux_disch == -1), 1, NA), na.rm=TRUE),
              count_0mon_flux_disch           = sum(ifelse((lag_flux_disch == 0), 1, NA), na.rm=TRUE),
              count_1mon_flux_disch           = sum(ifelse((lag_flux_disch == 1), 1, NA), na.rm=TRUE),
              count_2mon_flux_disch           = sum(ifelse((lag_flux_disch == 2), 1, NA), na.rm=TRUE),
              count_minus1mon_lag_disch_precip  = sum(ifelse((lag_disch_precip == -1), 1, NA), na.rm=TRUE),
              count_0mon_disch_precip = sum(ifelse((lag_disch_precip == 0), 1, NA), na.rm=TRUE),
              count_1mon_disch_precip = sum(ifelse((lag_disch_precip == 1), 1, NA), na.rm=TRUE),
              count_2mon_disch_precip = sum(ifelse((lag_disch_precip == 2), 1, NA), na.rm=TRUE))
  
  # Write information in text file
  out_fname          <- '~/Documents/repos/india_wq/Figures/max_month_analysis.txt'
  write.table(count_max_month_num,file=out_fname, row.names=F ,col.names=T, quote=F, sep='\t')
  
  # Convert data to long format
  max_month                <- gather(max_month,  key=variable, value=Month, -Year, -Station, -CWC_basin)
  
  # Add long label
  max_month$variableLab    <- param_labeller('variable', max_month$variable)
  
  # Reorder factor level
  max_month$Station        <- factor(max_month$Station,   levels=c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                                   'Polavaram','Urachikottai','Vijayawada'))
  max_month$CWC_basin      <- factor(max_month$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                   'Godavari','Cauvery','Krishna'))
  max_month$Month          <- factor(max_month$Month, levels = month.abb)
  max_month$variableLab    <- factor(max_month$variableLab, levels = c('Maximum DIN flux month',
                                                                       'Maximum discharge month',
                                                                       'Maximum precipitation month'))
  
  # Only keep value for Jun, July, Aug, Sep, and Oct
  max_month                <- max_month[which(max_month$Month %in% c('Jun','Jul','Aug','Sep','Oct')),]
  
  basin_labels             <-  group_by(max_month, Station) %>% summarise(CWC_basin = unique(CWC_basin))
  print(basin_labels)
  basin_labels$labels      <- c('A) Brahmani and Baitarni', 'B) Subernarekha', 'C) Narmada',
                                'D) Mahanadi', 'E) Godavari', 'F) Cauvery', 'G) Krishna')
  
  p1 <- ggplot(data=max_month, aes(x=Year, y=Month, color=variableLab)) + 
    geom_jitter(size=3, width=0.25, height=0.1) + 
    geom_text(data=basin_labels,aes(x=1981, y=Inf, label=labels), vjust=1.2, hjust=0, col='black', size=7) +
    facet_wrap(~ CWC_basin,  ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) + # line wrapping for label
    guides(color=guide_legend(title=NULL, nrow=2, override.aes = list(size=4))) + 
    labs(y     = 'Month with the maximum value',
         title = NULL) +
    scale_color_manual(values=c('red','blue','green')) +
    scale_x_continuous(expand=c(0,0), breaks=seq(1980, 2015, 3)) +
    theme_bw() +  # remove background
    theme(text              = element_text(size=20,family='Helvetica',color='black'),
          legend.position   = 'bottom',
          legend.text       = element_text(size=20,family='Helvetica',color='black'),
          plot.title        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_blank(),  # Remove strip background
          strip.text        = element_blank(),  # Remove strip text
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p1)
  
}

#_______________________________________________________________________________
# Plot timeseries of monthly DIN flux, precip, and discharge for the seven water quality stations
plot_monthly_timeseries <- function(DIN.mon.flux, hydro.monthly.tot, precip_monthly){
  
  # Combine into a single tibble using full join
  DIN_hydro_precip_monthly             <- reduce(list(DIN.mon.flux, hydro.monthly.tot, precip_monthly),
                                               full_join, by = c('Year', 'Month', 'Station', 'CWC_basin'))
  
  # Add column for yearmon
  DIN_hydro_precip_monthly$yearmon     <- as.yearmon(paste(DIN_hydro_precip_monthly$Year, 
                                                         DIN_hydro_precip_monthly$Month, sep='-'), '%Y-%b')
  
  # Only keep select columns
  DIN_hydro_precip_monthly             <- DIN_hydro_precip_monthly[ ,c('Month','yearmon','Station','CWC_basin',
                                                                     'DIN_flux_kgN_km2_mon','Disch_mm_mon','precip')]
  
  # Convert data to long format
  DIN_hydro_precip_monthly             <- gather(DIN_hydro_precip_monthly,  key=variable, value=value, -Month, -yearmon, -Station, -CWC_basin)
  
  # Add column for normalized values (only dividing by variance)
  DIN_hydro_precip_monthly             <- group_by(DIN_hydro_precip_monthly, Station, CWC_basin, variable) %>%
                                          mutate(nor_value = value/var(value, na.rm=TRUE))
  
  # Add long label
  DIN_hydro_precip_monthly$variableLab <- param_labeller_monthly('variable', DIN_hydro_precip_monthly$variable)
  
  # Reorder factor levels
  DIN_hydro_precip_monthly$Month       <- factor(DIN_hydro_precip_monthly$Month, levels = month.abb)
  DIN_hydro_precip_monthly$variableLab <- factor(DIN_hydro_precip_monthly$variableLab, levels=c('italic(P)~(mm~month^-1)',
                                                                                                'italic(Q)[DIN]~(kg~N~km^-2~month^-1)',
                                                                                                'italic(Q)~(mm~month^-1)'))
  DIN_hydro_precip_monthly$Station     <- factor(DIN_hydro_precip_monthly$Station,     levels=c('Jenapur','Ghatsila','Mandleshwar','Tikarapara',
                                                                                                'Polavaram','Urachikottai','Vijayawada'))
  DIN_hydro_precip_monthly$CWC_basin   <- factor(DIN_hydro_precip_monthly$CWC_basin,   levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                                                                'Godavari','Cauvery','Krishna'))
  
  p1 <- ggplot(data=DIN_hydro_precip_monthly,  aes(x=yearmon, y=value, color=variableLab)) + 
    geom_point() + 
    geom_line() +
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) + # line wrapping for label
    scale_x_yearmon(format='%b %Y', n=10) + 
    scale_color_manual(values=color_pal, labels=function(x) parse(text=x)) +
    labs(y     = NULL,
         title = NULL) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),    # Remove background color of facet label but keep black border
          legend.position   ='bottom',
          legend.key.size   = unit(2, 'lines'),
          legend.title      = element_blank(),
          legend.text       = element_text(size=20,family='Helvetica', color='black'),
          legend.text.align = 0,
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p1) 
  
  
  # Estimate monthly average
  DIN_hydro_precip_avg_monthly <- group_by(DIN_hydro_precip_monthly, Month, Station, CWC_basin, variableLab) %>%
                                  summarise(value     = mean(value,     na.rm=TRUE),
                                            nor_value = mean(nor_value, na.rm=TRUE))
  
  print('Average monthly DIN flux for various stations')
  tmp    <- DIN_hydro_precip_avg_monthly[which(DIN_hydro_precip_avg_monthly$variableLab == 'italic(Q)[DIN]~(kg~N~km^-2~month^-1)'),
                                         c('Month','CWC_basin','value')]
  tmp$value = round(tmp$value, 2)
  print(as.data.frame(spread(tmp, key=Month, value=value)))
  
  print('Average monthly discharge for various stations')
  tmp    <- DIN_hydro_precip_avg_monthly[which(DIN_hydro_precip_avg_monthly$variableLab == 'italic(Q)~(mm~month^-1)'),
                                         c('Month','CWC_basin','value')]
  tmp$value = round(tmp$value, 2)
  print(as.data.frame(spread(tmp, key=Month, value=value)))
  
  print('Average monthly precip for various stations')
  tmp    <- DIN_hydro_precip_avg_monthly[which(DIN_hydro_precip_avg_monthly$variableLab == 'italic(P)~(mm~month^-1)'),
                                         c('Month','CWC_basin','value')]
  tmp$value = round(tmp$value, 2)
  print(as.data.frame(spread(tmp, key=Month, value=value)))
  
  p2 <- ggplot(data=DIN_hydro_precip_avg_monthly,  aes(x=Month, y=value, color=variableLab)) + 
    geom_point(size=3) + 
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) + # line wrapping for label
    scale_color_manual(values=color_pal, labels=function(x) parse(text=x)) +
    guides(color=guide_legend(title=NULL, override.aes = list(size=4))) + 
    labs(y     = NULL,
         title = NULL) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),    # Remove background color of facet label but keep black border
          legend.position   ='bottom',
          legend.key.size   = unit(2, 'lines'),
          legend.title      = element_blank(),
          legend.text       = element_text(size=20,family='Helvetica', color='black'),
          legend.text.align = 0,
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p2) 
  
  p3 <- ggplot(data=DIN_hydro_precip_monthly,  aes(x=yearmon, y=nor_value, color=variableLab)) + 
    geom_point() + 
    geom_line() +
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) + # line wrapping for label
    scale_x_yearmon(format='%b %Y', n=10) + 
    scale_color_manual(values=color_pal, labels=function(x) parse(text=x)) +
    labs(y     = 'Values divided by variance',
         title = NULL) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),    # Remove background color of facet label but keep black border
          legend.position   ='bottom',
          legend.key.size   = unit(2, 'lines'),
          legend.title      = element_blank(),
          legend.text       = element_text(size=20,family='Helvetica', color='black'),
          legend.text.align = 0,
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p3) 

  
  p4 <- ggplot(data=DIN_hydro_precip_avg_monthly,  aes(x=Month, y=nor_value, color=variableLab)) + 
    geom_point(size=3) + 
    facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) + # line wrapping for label
    scale_color_manual(values=color_pal, labels=function(x) parse(text=x)) +
    guides(color=guide_legend(title=NULL, override.aes = list(size=4))) + 
    labs(y     = 'Values divided by variance',
         title = NULL) +
    theme_bw() +  # remove background
    theme(panel.grid        = element_blank(),          # Remove all grid lines
          text              = element_text(size=20,family='Helvetica',color='black'),
          strip.text        = element_text(size=20,family='Helvetica',color='black'),
          strip.background  = element_rect(fill=NA),    # Remove background color of facet label but keep black border
          legend.position   ='bottom',
          legend.key.size   = unit(2, 'lines'),
          legend.title      = element_blank(),
          legend.text       = element_text(size=20,family='Helvetica', color='black'),
          legend.text.align = 0,
          axis.title.y      = element_text(size=20,family='Helvetica',color='black'),
          axis.title.x      = element_blank(),
          axis.text.x       = element_text(size=20,family='Helvetica',color='black'),
          axis.text.y       = element_text(size=20,family='Helvetica',color='black'))
  
  print(p4) 
  
}

#_______________________________________________________________________________
# Read station information
filepath         <- '~/Documents/repos/india_wq/'
wq_stations      <- read.csv(paste(filepath,'WQ_stations_DIN_model.txt',sep=''), header=T, sep='\t')

# Remove Musiri from station list since WRTDS could not be applied to obtain monthly DIN flux
wq_stations      <- wq_stations[-which(wq_stations$Station == 'Musiri'), ]

# ---------- Read monthly DIN load [kg N/km2/month] for various CWC basins based on WRTDS model application
DIN.mon.flux     <- read_monthly_DIN_flux(in.folder='~/Documents/repos/india_wq/EGRET/WRTDS_analysis/', 
                                          wq_stations)

# ---------- Read total annual and monthly discharge for various CWC basins based on Hydro observation station data
hydro.mean          <- read_hydro_sta_basin_annual_discharge(in.folder = '~/Documents/repos/india_wq/India_WRIS/Hydro_ShinyApp/',
                                                             wq_stations, 
                                                             min_yr    = 1980,
                                                             max_yr    = 2015)

hydro.monthly.tot   <- hydro.mean[['Monthly']]

# Convert month to monthly abbreviation
hydro.monthly.tot$Month  <- month.abb[hydro.monthly.tot$Month]

# Merge to get drainage area
hydro.monthly.tot        <- merge(hydro.monthly.tot, wq_stations[,c('Station','Drainage_area_km2')])

# Convert discharge from m3/sec to mm/month by dividing by drainage area and converting unit for time
# Since the discharge value in m3/sec was summed for all days in the month, we don't have to multiply by number of days in the month
hydro.monthly.tot$Disch_mm_mon  <- hydro.monthly.tot$Discharge_m3s*(60*60*24)*1000/(hydro.monthly.tot$Drainage_area_km2* (10^6))

# Only keep select columns
hydro.monthly.tot        <- hydro.monthly.tot[,c('Year','Month','Station','CWC_basin','Disch_mm_mon')]

# # ---------- Read annual and monthly precipitation based on CPC data
# precip         <- read_basin_annual_precip(in.folder = '~/Documents/repos/india_wq/daily_precip/CPC/Basin_wq_stations/',
#                                            filename  = 'WQ_Station_Basin_CPC_precip_daily.precip',
#                                            min_yr    = 1980,
#                                            max_yr    = 2015)

# ---------- Read annual and monthly precipitation based on IMD data
precip                   <- read_basin_annual_precip(in.folder = '~/Documents/repos/india_wq/daily_precip/IMD/0.25deg/Basin_wq_stations/',
                                                     filename  = 'WQ_Station_Basin_IMD_precip_daily.precip',
                                                     out_fname = 'WQ_Station_IMD_average_annual_precip.txt',
                                                     min_yr    = 1980,
                                                     max_yr    = 2015)

precip_monthly <- precip[['Monthly']]

# Only keep data for select stations
precip_monthly <- precip_monthly[which(precip_monthly$Station %in% unique(DIN.mon.flux$Station)),]

# Convert month to monthly abbreviation
precip_monthly$Month  <- month.abb[precip_monthly$Month]

# tmp <- group_by(precip_monthly, Station, CWC_basin, Year) %>% 
#        summarise(per_JJAS = round(sum(precip[which(Month %in% c('Jun','Jul','Aug','Sep'))])/sum(precip),2))
# print(as.data.frame(spread(tmp, key=Year, value=per_JJAS)))      

# Define path to output file
out.f.name    <- '~/Documents/repos/india_wq/Figures/CWC_basins_monthly_DIN_load_precip_disch.pdf'

# Delete existing file
unlink(out.f.name)
# Start pdf device driver for saving plots
pdf(out.f.name, height=17,width=11) 

# Summarise months with maximum DIN flux, discharge, and precipitation and make summary plot 
max_month_analysis(DIN.mon.flux, hydro.monthly.tot, precip_monthly)
  
# Plot timeseries of monthly DIN flux, precip, and discharge for the seven water quality stations
plot_monthly_timeseries(DIN.mon.flux, hydro.monthly.tot, precip_monthly)
  
# Plot monthly DIN flux, precip, and discharge for the seven water quality stations
print('MONTHLY DIN FLUX SUMMARIES')
plot_monthly_data(DIN.mon.flux,
                  monthly_variable= 'DIN_flux_kgN_km2_mon',
                  y_label         = expression(paste(Dissolved~inorganic~nitrogen~flux~'[',kg~N~km^-2~month^-1,']')),
                  # y_axis_breaks   = c(800, 800, 1000, 400, 250, 250, 150))
                  y_axis_breaks   = c(1000, 1000, 1000, 250, 250, 250, 250))

print('MONTHLY DISCHARGE SUMMARIES')
plot_monthly_data(hydro.monthly.tot,
                  monthly_variable='Disch_mm_mon',
                  y_label         =expression(paste(Monthly~discharge~'[',mm~month^-1,']')),
                  y_axis_breaks   = c(800, 800, 1000, 400, 250, 250, 150))

print('MONTHLY PRECIPITATION SUMMARIES')
plot_monthly_data(precip_monthly,
                  monthly_variable='precip',
                  y_label         =expression(paste(Monthly~precipitation~'[',mm~month^-1,']')),
                  y_axis_breaks   = c(800, 800, 1000, 800, 500, 500, 300))

dev.off()


# Define path to output file
out.f.name    <- '~/Documents/repos/india_wq/Figures/CWC_basins_monthly_DIN_load_2_basins.pdf'

# Delete existing file
unlink(out.f.name)
# Start pdf device driver for saving plots
pdf(out.f.name, height=5,width=8.5) 

# Plot monthly DIN flux, precip, and discharge for the two water quality stations
plot_monthly_data_2_basins(DIN.mon.flux,
                           monthly_variable= 'DIN_flux_kgN_km2_mon',
                           y_label         = expression(atop(Dissolved~inorganic~nitrogen, paste(flux,' [',kg~N~km^-2~month^-1,']'))),
                           y_axis_breaks   = c(1000, 100))

dev.off()