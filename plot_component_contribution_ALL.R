
library(broom)
library(tidyr)
library(dplyr)
library(ggplot2)

source('~/Documents/repos/india_wq/labels.R')
source('~/Documents/repos/india_wq/india_color_pal.R')

#_______________________________________________________________________________
# Read WRTDS estimated and model predicted DIN flux and component contribution for various predictor variables
# Intercept term is added to the component contribution of first variable
read_WRTDS_and_predicted_DIN_flux_and_comp_contri <- function(wq_stations, in_folder){
  
  uniq_station   <- wq_stations$Station
  cwc_basin      <- wq_stations$CWC_basin
  
  # Empty list for storing annual DIN flux
  lm_data        <- list()
  
  for (ind in 1:length(uniq_station)){
    
    # Read station information for all years from 1980-2015
    filepath        <- paste(in_folder, uniq_station[ind], '_WRTDS_DIN_flux_and_predictors.txt',sep='')
    lm_data[[ind]]  <- read.csv(filepath, header=T, sep='\t')
    
    # Load saved model
    load(file=paste(in_folder, uniq_station[ind], '_model_fit.rda',sep=''))
    
    # Only keep data for select columns
    lm_data[[ind]]  <- lm_data[[ind]][ ,c('Year','Station','Annual_DIN_flux',names(best.fit$coef[-1]))]
    
    # Add column containing predicted DIN flux
    lm_data[[ind]]$fitted  <- predict(best.fit, newdata=lm_data[[ind]][,c('Annual_DIN_flux',names(best.fit$coef[-1]))]) 
    
    # number of predictors including intercept
    num.pred.var    <- length(best.fit$coef)
    
    # Add separate column for intercept
    lm_data[[ind]]$Intercept <- best.fit$coef['(Intercept)']
    
    for (ind.var in 2:num.pred.var) {

      # Identify column number in lm_data which contains values for parameter
      columnid                  <- which(colnames(lm_data[[ind]]) == names(best.fit$coef)[ind.var])
      
      # # Value for minimum deviation only from years for which we have WRTDS estimates of DIN flux
      # deviate_value             <- min(lm_data[[ind]][-which(is.na(lm_data[[ind]]$Annual_DIN_flux)),columnid])
      
      # Value for minimum deviation only from years for which we have WRTDS estimates of DIN flux
      deviate_value             <- min(lm_data[[ind]][,columnid])
      
      # Minimum deviate variables i.e., X = X - min(X) and
      # Multiply the minimum deviated variable value with corresponding beta to get component contribution
      lm_data[[ind]][,columnid] <- (lm_data[[ind]][,columnid] - deviate_value)*best.fit$coef[ind.var]
      
      # Update the intercept to include the subtracted term
      lm_data[[ind]][,'Intercept'] <- lm_data[[ind]][,'Intercept'] + deviate_value*best.fit$coef[ind.var]
      
      # # Add the value of intercept to the component contribution for the first variable
      # if(num.pred.var == 2) if(ind.var == 2) lm_data[[ind]][,columnid] <- lm_data[[ind]][,columnid] + best.fit$coef['(Intercept)']
      # if(num.pred.var > 2)  if(ind.var == 3) lm_data[[ind]][,columnid] <- lm_data[[ind]][,columnid] + best.fit$coef['(Intercept)']
      
    }

    # Add the updated value of intercept to the component contribution for the first or second variable
    if(num.pred.var == 2){
      print(paste(cwc_basin[ind], ' - intercept and deviation value added to variable ',names(best.fit$coef)[2]))
      columnid                  <- which(colnames(lm_data[[ind]]) == names(best.fit$coef)[2])
      lm_data[[ind]][,columnid] <- lm_data[[ind]][,columnid] + lm_data[[ind]][,'Intercept']
    }
    if(num.pred.var > 2){
      print(paste(cwc_basin[ind], ' - intercept and deviation value added to variable ',names(best.fit$coef)[3]))
      columnid                  <- which(colnames(lm_data[[ind]]) == names(best.fit$coef)[3])
      lm_data[[ind]][,columnid] <- lm_data[[ind]][,columnid] + lm_data[[ind]][,'Intercept']
    } 
    
    # Drop the intercept column
    lm_data[[ind]]$Intercept  <- NULL
    
    # Convert to character
    lm_data[[ind]]$Station    <- as.character(lm_data[[ind]]$Station)
    
    # Add CWC basin
    lm_data[[ind]]$CWC_basin  <- cwc_basin[ind]
    
  }  # ind loop ends
  
  # Collpase list into a single data frame
  lm_data                  <- bind_rows(lm_data)
  
  # Read DIN flux prediction based on all basin model
  # Read station information for all years from 1980-2015
  filepath                 <- paste(in_folder, 'All_basins_WRTDS_DIN_flux_and_predictors.txt',sep='')
  all_basins_lm_data       <- read.csv(filepath, header=T, sep='\t')
  
  # Load saved model
  load(file=paste(in_folder, 'All_basins_model_fit.rda',sep=''))
  
  # Only keep data for select columns
  all_basins_lm_data       <- all_basins_lm_data[ ,c('Year','Station','Annual_DIN_flux',names(best.fit$coef[-1]))]
  
  # Add column containing predicted DIN flux
  all_basins_lm_data$all_basins_fitted  <- predict(best.fit, newdata=all_basins_lm_data[,c('Annual_DIN_flux',names(best.fit$coef[-1]))]) 

  # Merge with prediction based on individual models
  lm_data                  <- merge(lm_data, all_basins_lm_data[, c('Year','Station','all_basins_fitted')])
  
  return(lm_data)
  
}

#_______________________________________________________________________________
# Read station information
filepath        <- '~/Documents/repos/india_wq/'
wq_stations     <- read.csv(paste(filepath,'WQ_stations_DIN_model.txt',sep=''),header=T,sep='\t')

wq_stations     <- wq_stations[which(wq_stations$Station != 'Musiri'),]

# in_folder <- '~/Documents/repos/india_wq/Best_predictors/CPC_CRU/'
# in_folder <- '~/Documents/repos/india_wq/Best_predictors/CPC_CRU/With_LSC/'
in_folder <- '~/Documents/repos/india_wq/Best_predictors/IMD_Lamarque/'
# in_folder <- '~/Documents/repos/india_wq/Best_predictors/IMD_Lamarque/With_LSC/'

# Read WRTDS estimated and model predicted DIN flux and component contribution for various predictor variables
# Intercept term is added to the component contribution of first variable
lm_data         <- read_WRTDS_and_predicted_DIN_flux_and_comp_contri(wq_stations, in_folder)

# Remove rows for 1980 and 2015 as none of the CWC basins have WRTDS estimated annual DIN flux for these years
lm_data         <- lm_data[-which(lm_data$Year %in% c(1980, 2015)),]

# ---------- R2 for labelling ---
eq <-  group_by(lm_data, Station, CWC_basin) %>%
       summarise('label'= as.character(as.expression(substitute(italic(R)^2~"="~R2,
                                                                list(R2 = round(summary(lm(Annual_DIN_flux ~ fitted))$r.squared, digits=2))))),
                 n    = as.character(as.expression(substitute(italic(n)~"="~len, list(len=length(Annual_DIN_flux))))))

eq$x           <- 2013.5 # Inf
eq$y           <- c(2000, 2000, 2000, 800, 800, 800, 200)
# eq$variableLab <- 'WRTDS~estimated~DIN~flux'

# Reorder factor levels
select_stations     <- c('Jenapur','Ghatsila','Mandleshwar','Tikarapara','Polavaram','Urachikottai','Vijayawada')

lm_data$Station     <- factor(lm_data$Station, levels=select_stations)
eq$Station          <- factor(eq$Station,      levels=select_stations)

lm_data$CWC_basin   <- factor(lm_data$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                          'Godavari','Cauvery','Krishna'))
eq$CWC_basin        <- factor(eq$CWC_basin, levels=c('Brahmani and Baitarni','Subernarekha','Narmada','Mahanadi',
                                                     'Godavari','Cauvery','Krishna'))
  
# Add label for figures
eq                  <- eq[order(eq$Station),]
# eq$fig_label        <- c('A','B','C','D','E','F','G')
eq$fig_label        <- c('A) Brahmani and Baitarni', 'B) Subernarekha', 'C) Narmada',
                         'D) Mahanadi', 'E) Godavari', 'F) Cauvery', 'G) Krishna')

# Convert data to long format
lm_data             <- gather(lm_data, key=variable, value=value, -Year, -Station, -CWC_basin, -Annual_DIN_flux, -fitted, -all_basins_fitted)

# Add long label
lm_data$variableLab <- param_labeller('variable', lm_data$variable)

# Reorder factor levels
final_variables     <- c('italic(N)[Fert~`,`~Dep]',#'italic(N)[Dep]',
                         'italic(T)[JAS]',
                         'italic(n)[P>10~mm]','italic(P)[italic(p)>0.90]',
                         'italic(P)[JJAS]'   ,'italic(P)[Annual]')
lm_data$variableLab <- factor(lm_data$variableLab, levels=final_variables)

# Remove rows in which value is NA
lm_data <- lm_data[-which(is.na(lm_data$value)),]

# Subset Narmada basin data
narmada_basin                <- subset(lm_data, CWC_basin == 'Narmada')
narmada_basin$ymin           <- 0
narmada_basin$ymax           <- 0

# Add value of ymin and ymax for Narmada
rowid                        <- which(narmada_basin$variable == 'fert_dep_kgN_km2' & narmada_basin$value < 0)
narmada_basin[rowid, 'ymin'] <- narmada_basin[rowid, 'value']
narmada_basin[rowid, 'ymax'] <- 0

rowid                        <- which(narmada_basin$variable == 'fert_dep_kgN_km2' & narmada_basin$value < 0 & narmada_basin$fitted < 0)
narmada_basin[rowid, 'ymax'] <- narmada_basin[rowid, 'fitted']

rowid                        <- which(narmada_basin$variable == 'fert_dep_kgN_km2' & narmada_basin$value >= 0)
narmada_basin[rowid, 'ymin'] <- 0
narmada_basin[rowid, 'ymax'] <- narmada_basin[rowid, 'value']

narmada_basin                <- group_by(narmada_basin, Year) %>%
                                mutate(ymin = ifelse(variable=='precip_JJAS', value[variable=='fert_dep_kgN_km2'], ymin),
                                       ymax = ifelse(variable=='precip_JJAS', sum(value), ymax))

# Subset Mahandi basin data
mahanadi_basin                <- subset(lm_data, CWC_basin == 'Mahanadi')
mahanadi_basin$ymin           <- 0
mahanadi_basin$ymax           <- 0

# Add value of ymin and ymax for Mahanadi
rowid                         <- which(mahanadi_basin$variable == 'fert_dep_kgN_km2' & mahanadi_basin$value < 0)
mahanadi_basin[rowid, 'ymin'] <- mahanadi_basin[rowid, 'value']
mahanadi_basin[rowid, 'ymax'] <- 0

rowid                         <- which(mahanadi_basin$variable == 'fert_dep_kgN_km2' & mahanadi_basin$value < 0 & mahanadi_basin$fitted < 0)
mahanadi_basin[rowid, 'ymax'] <- mahanadi_basin[rowid, 'fitted']

rowid                         <- which(mahanadi_basin$variable == 'fert_dep_kgN_km2' & mahanadi_basin$value >= 0)
mahanadi_basin[rowid, 'ymin'] <- 0
mahanadi_basin[rowid, 'ymax'] <- mahanadi_basin[rowid, 'value']

# mahanadi_basin                <- group_by(mahanadi_basin, Year) %>%
#                                  mutate(ymin = ifelse(variable=='dep_kgN_km2', value[variable=='temp_JJAS'], ymin),
#                                         ymax = ifelse(variable=='dep_kgN_km2', sum(value[variable!='R99pTOT_JAS_1']), ymax),
#                                         ymin = ifelse(variable=='R99pTOT_JAS_1', sum(value[variable!='R99pTOT_JAS_1']), ymin),
#                                         ymax = ifelse(variable=='R99pTOT_JAS_1', sum(value), ymax))
mahanadi_basin                <- group_by(mahanadi_basin, Year) %>%
                                 mutate(ymin = ifelse(variable=='precip_JJAS', value[variable=='fert_dep_kgN_km2'], ymin),
                                        ymax = ifelse(variable=='precip_JJAS', sum(value), ymax))

# yr_value <- mahanadi_basin[which(mahanadi_basin$ymax < 0 ),'Year']
# for (i in 1:nrow(yr_value)){
#   mahanadi_basin[which(mahanadi_basin$Year == yr_value$Year[i] & mahanadi_basin$variable=='temp_JJAS'),'ymax'] = mahanadi_basin[which(mahanadi_basin$Year == yr_value$Year[i] & mahanadi_basin$variable=='dep_kgN_km2'),'ymax']
# }

# Subset Krishna basin data
krishna_basin                <- subset(lm_data, CWC_basin == 'Krishna')
krishna_basin$ymin           <- 0
krishna_basin$ymax           <- 0

# Add value of ymin and ymax for Krishna
rowid                        <- which(krishna_basin$variable == 'temp_JAS' & krishna_basin$value < 0)
krishna_basin[rowid, 'ymin'] <- krishna_basin[rowid, 'value']
krishna_basin[rowid, 'ymax'] <- 0

rowid                        <- which(krishna_basin$variable == 'temp_JAS' & krishna_basin$value < 0 & krishna_basin$fitted < 0)
krishna_basin[rowid, 'ymax'] <- krishna_basin[rowid, 'fitted']

rowid                        <- which(krishna_basin$variable == 'temp_JAS' & krishna_basin$value >= 0)
krishna_basin[rowid, 'ymin'] <- 0
krishna_basin[rowid, 'ymax'] <- krishna_basin[rowid, 'value']

krishna_basin                <- group_by(krishna_basin, Year) %>%
                                mutate(ymin = ifelse(variable=='precip', value[variable=='temp_JAS'], ymin),
                                       ymax = ifelse(variable=='precip', sum(value), ymax))

# Drop empty factors
lm_data$variableLab        <- droplevels(lm_data$variableLab)
narmada_basin$variableLab  <- droplevels(narmada_basin$variableLab)
mahanadi_basin$variableLab <- droplevels(mahanadi_basin$variableLab)
krishna_basin$variableLab  <- droplevels(krishna_basin$variableLab)

# ---------- Define path to output file ---------- 
out.f.name    <- paste(in_folder, 'Predicted_DIN_flux_component_contribution.pdf', sep='')

# Delete existing file
unlink(out.f.name)
# Start pdf device driver for saving plots
pdf(out.f.name, width=11, height=17) 

# Make bar plot of contribution for various predictor variables
p1 <- ggplot(data=lm_data,  aes(x=Year, y=Annual_DIN_flux)) + 
  geom_area(data=subset(lm_data,!(CWC_basin %in% c('Narmada','Mahanadi','Krishna'))), aes(x=Year, y=value, fill=variableLab), position=position_stack(reverse=T), alpha=0.5) +
  geom_ribbon(data=narmada_basin, aes(x=Year, ymin=ymin, ymax=ymax, fill=variableLab), alpha=0.5) +
  geom_ribbon(data=mahanadi_basin,aes(x=Year, ymin=ymin, ymax=ymax, fill=variableLab), alpha=0.5) +
  geom_ribbon(data=krishna_basin, aes(x=Year, ymin=ymin, ymax=ymax, fill=variableLab), alpha=0.5) +
  geom_point(aes(color='WRTDS estimated DIN flux'), size=3) + 
  geom_point(aes(x=Year, y=fitted, color='Predicted DIN flux'), size=2) +
  geom_line(aes( x=Year, y=fitted, color='Predicted DIN flux'), show.legend=FALSE) +
  geom_text(data=eq,aes(x=x, y=y, label=label), vjust=1.2, hjust=1.0, parse=TRUE, inherit.aes=TRUE, col='black', size=7) +
  geom_text(data=eq,aes(x=1981, y=Inf, label=fig_label), vjust=1.4, hjust=0, col='black', size=7) +
  facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) +  # line wrapping for label
  scale_color_manual(name='',values=c('WRTDS estimated DIN flux'='red',
                                      'Predicted DIN flux'='black')) +
  scale_fill_manual(values=color_pal, 
                    breaks=rev(final_variables),
                    labels=function(x) parse(text=x)) +
  scale_x_continuous(breaks=seq(from=1980, to=2015, by=5), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  labs(y     = expression(paste(Dissolved~inorganic~nitrogen~flux,' [',kg~N~km^-2~yr^-1,']')),
       x     = 'Year',
       title = NULL) +
  guides(color=guide_legend(override.aes=list(size=5))) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2))) +
  theme_bw() +  # remove background
  theme(panel.border      = element_rect(colour ='black',size=1.0),
        panel.grid        = element_blank(),          # Remove all grid lines
        plot.title        = element_text(size=20, family='Helvetica', color='black'),
        text              = element_text(size=20, family='Helvetica', color='black'),
        panel.spacing     = unit(1.2, 'lines'),                # Increase spacing between facet panels
        strip.background  = element_blank(),  # Remove strip background
        strip.text        = element_blank(),  # Remove strip text
        legend.box        = 'vertical',
        legend.position   ='bottom',
        legend.key.size   = unit(2, 'lines'),
        legend.spacing.x  = unit(0.3, 'cm'),  # Increase spacing between legend items
        legend.title      = element_blank(),
        legend.text       = element_text(size=20,family='Helvetica', color='black'),
        legend.text.align = 0,                       # align legend text to left
        axis.title.x      = element_text(size=20, family='Helvetica', color='black'),
        axis.title.y      = element_text(size=20, family='Helvetica', color='black'),
        axis.text.x       = element_text(size=20, family='Helvetica', color='black'), 
        axis.text.y       = element_text(size=20, family='Helvetica', color='black'),
        plot.margin       = unit(c(8, 20, 8, 8), 'pt'))

print(p1) 

# Make bar plot of contribution for various predictor variables
p2 <- ggplot(data=lm_data,  aes(x=Year, y=Annual_DIN_flux)) + 
  geom_area(data=subset(lm_data,!(CWC_basin %in% c('Narmada','Mahanadi','Krishna'))), aes(x=Year, y=value, fill=variableLab), position=position_stack(reverse=T), alpha=0.5) +
  geom_ribbon(data=narmada_basin, aes(x=Year, ymin=ymin, ymax=ymax, fill=variableLab), alpha=0.5) +
  geom_ribbon(data=mahanadi_basin,aes(x=Year, ymin=ymin, ymax=ymax, fill=variableLab), alpha=0.5) +
  geom_ribbon(data=krishna_basin, aes(x=Year, ymin=ymin, ymax=ymax, fill=variableLab), alpha=0.5) +
  geom_point(aes(color='WRTDS estimated DIN flux'), size=3) + 
  geom_point(aes(x=Year, y=fitted, color='Predicted DIN flux'), size=2) +
  geom_line(aes( x=Year, y=fitted, color='Predicted DIN flux'), show.legend=FALSE) +
  geom_point(aes(x=Year, y=all_basins_fitted, color='Predicted DIN flux (all basins model)'), size=2) +
  geom_line(aes( x=Year, y=all_basins_fitted, color='Predicted DIN flux (all basins model)'), show.legend=FALSE) +
  geom_text(data=eq,aes(x=x, y=y, label=label), vjust=1.2, hjust=1, parse=TRUE, inherit.aes=TRUE, col='black', size=7) +
  geom_text(data=eq,aes(x=1981, y=Inf, label=fig_label), vjust=1.4, hjust=0, col='black', size=7) +
  facet_wrap(~ CWC_basin, scales='free_y', ncol=1, strip.position='right', labeller = label_wrap_gen(width=15)) +  # line wrapping for label
  scale_color_manual(name='',values=c('WRTDS estimated DIN flux'='red',
                                      'Predicted DIN flux'='black',
                                      'Predicted DIN flux (all basins model)'='magenta')) +
  scale_fill_manual(values=color_pal, 
                    breaks=rev(final_variables),
                    labels=function(x) parse(text=x)) +
  scale_x_continuous(breaks=seq(from=1980, to=2015, by=5), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  labs(y     = expression(paste(Dissolved~inorganic~nitrogen~flux,' [',kg~N~km^-2~yr^-1,']')),
       x     = 'Year',
       title = NULL) +
  guides(fill=guide_legend(nrow=1),
         color=guide_legend(nrow=2, override.aes=list(size=5))) +
  theme_bw() +  # remove background
  theme(panel.border      = element_rect(colour ='black',size=1.0),
        panel.grid        = element_blank(),          # Remove all grid lines
        plot.title        = element_text(size=20, family='Helvetica', color='black'),
        text              = element_text(size=20, family='Helvetica', color='black'),
        panel.spacing     = unit(1.2, 'lines'),                # Increase spacing between facet panels
        strip.background  = element_blank(),  # Remove strip background
        strip.text        = element_blank(),  # Remove strip text
        legend.box        = 'vertical',
        legend.position   ='bottom',
        legend.key.size   = unit(2, 'lines'),
        legend.spacing.x  = unit(0.3, 'cm'),  # Increase spacing between legend items
        legend.title      = element_blank(),
        legend.text       = element_text(size=20,family='Helvetica', color='black'),
        legend.text.align = 0,                       # align legend text to left
        axis.title.x      = element_text(size=20, family='Helvetica', color='black'),
        axis.title.y      = element_text(size=20, family='Helvetica', color='black'),
        axis.text.x       = element_text(size=20, family='Helvetica', color='black'), 
        axis.text.y       = element_text(size=20, family='Helvetica', color='black'),
        plot.margin       = unit(c(8, 20, 8, 8), 'pt'))

print(p2) 


dev.off()