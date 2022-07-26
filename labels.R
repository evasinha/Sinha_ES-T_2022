

# ______________________________________________________________________________
param_labeller <- function(var, value){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='Level_m']                  <- 'Reservoir~level~(m)'
    value[value=='Storage_BCM']              <- 'Reservoir~storage~(BCM)'
    
    value[value=='Discharge_m3s']            <- 'Discharge~(m^3/sec)'
    value[value=='Discharge_m3s_JJAS']       <- 'Discharge~JJAS~(m^3/sec)'
    value[value=='Disch_mm_yr']              <- 'Discharge~(mm/yr)'
    value[value=='Disch_mm_yr_JJAS']         <- 'Discharge~JJAS~(mm/yr)'
    
    value[value=='NH3_mgNl']                 <- 'NH[3]~(mg~N/l)'
    value[value=='NO2_NO3_mgNl']             <- 'NO[2]~`+`~NO[3]~(mg~N/l)'
    value[value=='Total_nutrients_mgPl']     <- 'Total~nutrients~(mg~P/l)'
    value[value=='DIN_mgNl']                 <- 'Dissolved~Inorganic~Nitrogen~(mgN/l)'
    value[value=='DIN_kgNday']               <- 'Dissolved~Inorganic~Nitrogen~(kgN/day)'
    value[value=='Mean_gauge_m']             <- 'Mean~gauge~(m)'
    value[value=='fert_kgN_ha']              <- 'Annual~fertilizer' # 'Annual~fertilizer~(kg~N~ha^-1~yr^-1)'
    value[value=='fert_kgN_km2']             <- 'italic(N)[Fert]'        # 'italic(FU)~(kg~N~km^-2~yr^-1)'
    value[value=='ln_fert']                  <- 'ln(italic(N)[Fert])'     # 'italic(f)[FU]~(kg~N~km^-2~yr^-1)'
    value[value=='dep_kgN_km2']              <- 'italic(N)[Dep]'        # 'italic(ND)~(kg~N~km^-2~yr^-1)'
    value[value=='ln_dep']                   <- 'ln(italic(N)[Dep])'     # 'italic(f)[ND]~(kg~N~km^-2~yr^-1)'
    value[value=='fert_dep_kgN_km2']         <- 'italic(N)[Fert~`,`~Dep]'     # 'italic(FU~ND)~(kg~N~km^-2~yr^-1)'
    value[value=='ln_fert_dep']              <- 'ln(italic(N)[Fert~`,`~Dep])'  # 'italic(f)[FU~ND]~(kg~N~km^-2~yr^-1)'
    value[value=='GSC_MCM_mm']               <- 'italic(GSC)'       # 'italic(GSC)[MCM/mm]'
    value[value=='LSC_MCM']                  <- 'italic(LSC)~(MCM)' # 'italic(LSC)[MCM/mm]'
    value[value=='LSC_MCM_mm']               <- 'italic(LSC)~(MCM~mm^-1)'       # 'italic(LSC)[MCM/mm]'
    value[value=='LSC']                      <- 'italic(LSC)'       # 'italic(LSC)'
    value[value=='precip']                   <- 'italic(P)[Annual]' # 'italic(P)[Annual]~(mm~yr^-1)'
    value[value=='precip_JJAS']              <- 'italic(P)[JJAS]'   # 'italic(P)[JJAS]~(mm~yr^-1)'
    value[value=='precip_JAS']               <- 'italic(P)[JAS]'
    value[value=='precip_JASO']              <- 'italic(P)[JASO]'
    value[value=='precip_ASO']               <- 'italic(P)[ASO]'
    
    value[value=='pre_gt_10']                <- 'italic(n)[P>10~mm]'
    value[value=='pre_gt_20']                <- 'italic(n)[P>20~mm]'
    value[value=='pre_gt_10_JJAS']           <- 'italic(n)[JJAS~P>10~mm]'
    value[value=='pre_gt_20_JJAS']           <- 'italic(n)[JJAS~P>20~mm]'
    value[value=='pre_gt_10_JAS']            <- 'italic(n)[JAS~P>10~mm]'
    value[value=='pre_gt_20_JAS']            <- 'italic(n)[JAS~P>20~mm]'
    value[value=='pre_gt_10_JASO']           <- 'italic(n)[JASO~P>10~mm]'
    value[value=='pre_gt_20_JASO']           <- 'italic(n)[JASO~P>20~mm]'
    value[value=='pre_gt_10_ASO']            <- 'italic(n)[ASO~P>10~mm]'
    value[value=='pre_gt_20_ASO']            <- 'italic(n)[ASO~P>20~mm]'
    
    value[value=='R90pTOT']                  <- 'italic(P)[italic(p)>0.90]'            # 'italic(P)[italic(p)>0.90]~(mm~yr^-1)'
    value[value=='R95pTOT']                  <- 'italic(P)[italic(p)>0.95]'            # 'italic(P)[italic(p)>0.95]~(mm~yr^-1)'
    value[value=='R99pTOT']                  <- 'italic(P)[italic(p)>0.99]'            # 'italic(P)[italic(p)>0.99]~(mm~yr^-1)'
    
    value[value=='R90pTOT_JJAS_1']           <- 'italic(P)[JJAS~italic(p)>0.90]'       # 'italic(P)[JJAS~italic(p)>0.90]~(mm~yr^-1)'
    value[value=='R95pTOT_JJAS_1']           <- 'italic(P)[JJAS~italic(p)>0.95]'       # 'italic(P)[JJAS~italic(p)>0.95]~(mm~yr^-1)'
    value[value=='R99pTOT_JJAS_1']           <- 'italic(P)[JJAS~italic(p)>0.99]'       # 'italic(P)[JJAS~italic(p)>0.99]~(mm~yr^-1)'
    value[value=='R90pTOT_JAS_1']            <- 'italic(P)[JAS~italic(p)>0.90]'
    value[value=='R95pTOT_JAS_1']            <- 'italic(P)[JAS~italic(p)>0.95]'
    value[value=='R99pTOT_JAS_1']            <- 'italic(P)[JAS~italic(p)>0.99]'
    value[value=='R90pTOT_JASO_1']           <- 'italic(P)[JASO~italic(p)>0.90]'
    value[value=='R95pTOT_JASO_1']           <- 'italic(P)[JASO~italic(p)>0.95]'
    value[value=='R99pTOT_JASO_1']           <- 'italic(P)[JASO~italic(p)>0.99]'
    value[value=='R90pTOT_ASO_1']            <- 'italic(P)[ASO~italic(p)>0.90]'
    value[value=='R95pTOT_ASO_1']            <- 'italic(P)[ASO~italic(p)>0.95]'
    value[value=='R99pTOT_ASO_1']            <- 'italic(P)[ASO~italic(p)>0.99]'
    
    value[value=='R90pTOT_JJAS_2']           <- 'italic(P)[JJAS~italic(p)(JJAS)>0.90]' # 'italic(P)[JJAS~italic(p)(JJAS)>0.90]~(mm~yr^-1)'
    value[value=='R95pTOT_JJAS_2']           <- 'italic(P)[JJAS~italic(p)(JJAS)>0.95]' # 'italic(P)[JJAS~italic(p)(JJAS)>0.95]~(mm~yr^-1)'
    value[value=='R99pTOT_JJAS_2']           <- 'italic(P)[JJAS~italic(p)(JJAS)>0.99]' # 'italic(P)[JJAS~italic(p)(JJAS)>0.99]~(mm~yr^-1)'
    value[value=='R90pTOT_JAS_2']            <- 'italic(P)[JAS~italic(p)(JAS)>0.90]'
    value[value=='R95pTOT_JAS_2']            <- 'italic(P)[JAS~italic(p)(JAS)>0.95]'
    value[value=='R99pTOT_JAS_2']            <- 'italic(P)[JAS~italic(p)(JAS)>0.99]'
    value[value=='R90pTOT_JASO_2']           <- 'italic(P)[JASO~italic(p)(JASO)>0.90]'
    value[value=='R95pTOT_JASO_2']           <- 'italic(P)[JASO~italic(p)(JASO)>0.95]'
    value[value=='R99pTOT_JASO_2']           <- 'italic(P)[JASO~italic(p)(JASO)>0.99]'
    value[value=='R90pTOT_ASO_2']            <- 'italic(P)[ASO~italic(p)(ASO)>0.90]'
    value[value=='R95pTOT_ASO_2']            <- 'italic(P)[ASO~italic(p)(ASO)>0.95]'
    value[value=='R99pTOT_ASO_2']            <- 'italic(P)[ASO~italic(p)(ASO)>0.99]'
    
    
    value[value=='temp_annual']              <- 'italic(T)[Annual]'                    # 'italic(T)[Annual]~(deg~C)'
    value[value=='temp']                     <- 'italic(T)[Annual]'                    # 'italic(T)[Annual]~(deg~C)'
    value[value=='temp_JJAS']                <- 'italic(T)[JJAS]'                      # 'italic(T)[JJAS]~(deg~C)'
    value[value=='temp_JAS']                 <- 'italic(T)[JAS]'                       # 'italic(T)[JJAS]~(deg~C)'
    value[value=='temp_JASO']                <- 'italic(T)[JASO]'                      # 'italic(T)[JJAS]~(deg~C)'
    value[value=='temp_ASO']                 <- 'italic(T)[ASO]'                       # 'italic(T)[JJAS]~(deg~C)'
    
    value[value=='urban']                    <- 'italic(LU)[Urban]'
    value[value=='agriculture']              <- 'italic(LU)[Agriculture]'
    value[value=='forest']                   <- 'italic(LU)[Forest]'
    value[value=='shrubland']                <- 'italic(LU)[Shrub]'
    value[value=='grassland']                <- 'italic(LU)[Grass]'
    value[value=='barren']                   <- 'italic(LU)[Barren]'
    value[value=='agriculture_forest']       <- 'italic(LU)[A,F]'
    value[value=='urban_agriculture_forest'] <- 'italic(LU)[U,A,F]'
    value[value=='Intercept']                <- 'Intercept'
    value[value=='Annual_DIN_load']          <- 'WRTDS~estimated~DIN~load~(kg~N~yr^-1)'
    value[value=='Annual_DIN_flux']          <- 'WRTDS~estimated~DIN~flux'             # 'WRTDS~estimated~DIN~flux~(kg~N~km^-2~yr^-1)'
    value[value=='WRTDS_DIN_flux']           <- 'WRTDS~estimated~annual~DIN~flux'
    value[value=='fitted']                   <- 'Predicted~DIN~flux'                   # 'Predicted~DIN~flux~(kg~N~km^-2~yr^-1)'
    value[value=='All_basin_model_fitted']   <- 'Predicted~DIN~flux~(all~basin~model)'
    value[value=='ln_DIN_flux']              <- 'Log~transformed~DIN~flux~(kg~N~km^-2~yr^-1)'
    value[value=='Conc']                     <- 'Conc~(mg~N/l)'
    value[value=='Flux']                     <- 'Flux~(kg/day)'
    value[value=='Q']                        <- 'Discharge~(m^3/sec)'
    value[value=='min_bic']                  <- 'Bayesian~Information~Criterion'
    value[value=='min_rsq']                  <- 'r^2'
    value[value=='min_adjr2']                <- 'Adjusted~r^2'
    value[value=='ConcAve']                  <- 'Observed~conc.'
    value[value=='ConcHat']                  <- 'WRTDS~estimated~conc.'
    value[value=='resid']                    <- 'Residual~conc.'
    
    value[value=='Conc_mgL']                 <- 'Concentration~(mg~L^-1)'
    value[value=='Load']                     <- 'Load~(10^6~kg~yr^-1)'
    # value[value=='FNConc_mgL']               <- 'Flow-normalized~concentration~(mg~L^-1)'
    # value[value=='FNLoad']                   <- 'Flow-normalized~load~(10^6~kg~yr^-1)'
    
    value[value=='FNConc_mgL']               <- 'Flow-normalized\nconcentration (mg/L)'
    value[value=='FNLoad']                   <- 'Flow-normalized\nload (10^6 kg/yr)'
    
    value[value=='Model_R2']                       <- 'R2 between estimated and predicted annual flux'
    value[value=='R2_observed_estimated_log_conc'] <- 'Observed vs. estimated ln(concentration)'
    value[value=='R2_observed_estimated_conc']     <- 'Observed vs. estimated concentration'
    value[value=='R2_observed_estimated_load']     <- 'Observed vs. estimated load'
    value[value=='R2_observed_load_discharge']     <- 'Observed load vs. discharge'
    value[value=='R2_estimated_load_discharge_sampled'] <- 'Estimated load vs. discharge (sampled days)'
    value[value=='R2_estimated_load_discharge_all']     <- 'Estimated load vs. discharge (all days)'

    value[value=='max_flux_mon']                        <- 'Maximum DIN flux month'
    value[value=='max_disch_mon']                       <- 'Maximum discharge month'
    value[value=='max_precip_mon']                      <- 'Maximum precipitation month'

    value[value=='CPC_CRU']             <- 'CPC~CRU~data'
    value[value=='CPC_CRU_LSC']         <- 'CPC~CRU~data~with~LSC~timeseries'
    value[value=='IMD']                 <- 'IMD~data'
    value[value=='IMD_LSC']             <- 'IMD~data~with~LSC~timeseries'
}
  
  return(value)
}

# ______________________________________________________________________________
param_labeller_2 <- function(var, value){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='Annual_DIN_flux']          <- '$Q_{DIN}$ (kg N $km^{-2}$ $yr^{-1}$)'
    value[value=='precip']                   <- '$P_{Annual}$ (mm $yr^{-1}$)'
    value[value=='precip_annual']            <- '$P_{Annual}$ (mm $yr^{-1}$)'
    value[value=='precip_JJAS']              <- '$P_{JJAS}$ (mm $yr^{-1}$)'
    value[value=='precip_JAS']               <- '$P_{JAS}$ (mm $yr^{-1}$)'
    value[value=='precip_JASO']              <- '$P_{JASO}$ (mm $yr^{-1}$)'
    value[value=='precip_ASO']               <- '$P_{ASO}$ (mm $yr^{-1}$)'
    
    value[value=='pre_gt_10']                <- '$n_{P>10 mm}$'
    value[value=='pre_gt_20']                <- '$n_{P>20 mm}$'
    value[value=='pre_gt_10_JJAS']           <- '$n_{JJAS \\; P>10 mm}$'
    value[value=='pre_gt_20_JJAS']           <- '$n_{JJAS \\; P>20 mm}$'
    value[value=='pre_gt_10_JAS']            <- '$n_{JAS  \\; P>10 mm}$'
    value[value=='pre_gt_20_JAS']            <- '$n_{JAS  \\; P>20 mm}$'
    value[value=='pre_gt_10_JASO']           <- '$n_{JASO \\; P>10 mm}$'
    value[value=='pre_gt_20_JASO']           <- '$n_{JASO \\; P>20 mm}$'
    value[value=='pre_gt_10_ASO']            <- '$n_{ASO  \\; P>10 mm}$'
    value[value=='pre_gt_20_ASO']            <- '$n_{ASO  \\; P>20 mm}$'
    
    value[value=='R90pTOT']                  <- '$P_{p>0.90}$ (mm $yr^{-1}$)'
    value[value=='R95pTOT']                  <- '$P_{p>0.95}$ (mm $yr^{-1}$)$'
    value[value=='R99pTOT']                  <- '$P_{p>0.99}$ (mm $yr^{-1}$)'
    
    value[value=='R90pTOT_JJAS_1']           <- '$P_{JJAS \\; p>0.90}$ (mm $yr^{-1}$)$' 
    value[value=='R95pTOT_JJAS_1']           <- '$P_{JJAS \\; p>0.95}$ (mm $yr^{-1}$)'
    value[value=='R99pTOT_JJAS_1']           <- '$P_{JJAS \\; p>0.99}$ (mm $yr^{-1}$)'
    value[value=='R90pTOT_JAS_1']            <- '$P_{JAS  \\; p>0.90}$ (mm $yr^{-1}$)$' 
    value[value=='R95pTOT_JAS_1']            <- '$P_{JAS  \\; p>0.95}$ (mm $yr^{-1}$)'
    value[value=='R99pTOT_JAS_1']            <- '$P_{JAS  \\; p>0.99}$ (mm $yr^{-1}$)'
    value[value=='R90pTOT_JASO_1']           <- '$P_{JASO \\; p>0.90}$ (mm $yr^{-1}$)$' 
    value[value=='R95pTOT_JASO_1']           <- '$P_{JASO \\; p>0.95}$ (mm $yr^{-1}$)'
    value[value=='R99pTOT_JASO_1']           <- '$P_{JASO \\; p>0.99}$ (mm $yr^{-1}$)'
    value[value=='R90pTOT_ASO_1']            <- '$P_{ASO  \\; p>0.90}$ (mm $yr^{-1}$)$' 
    value[value=='R95pTOT_ASO_1']            <- '$P_{ASO  \\; p>0.95}$ (mm $yr^{-1}$)'
    value[value=='R99pTOT_ASO_1']            <- '$P_{ASO  \\; p>0.99}$ (mm $yr^{-1}$)'
    
    value[value=='R90pTOT_JJAS_2']           <- '$P_{JJAS \\; p(JJAS)>0.90}$ (mm $yr^{-1}$)'
    value[value=='R95pTOT_JJAS_2']           <- '$P_{JJAS \\; p(JJAS)>0.95}$ (mm $yr^{-1}$)$'
    value[value=='R99pTOT_JJAS_2']           <- '$P_{JJAS \\; p(JJAS)>0.99}$ (mm $yr^{-1}$)'
    value[value=='R90pTOT_JAS_2']            <- '$P_{JAS  \\; p(JAS)>0.90}$ (mm $yr^{-1}$)'
    value[value=='R95pTOT_JAS_2']            <- '$P_{JAS  \\; p(JAS)>0.95}$ (mm $yr^{-1}$)$'
    value[value=='R99pTOT_JAS_2']            <- '$P_{JAS  \\; p(JAS)>0.99}$ (mm $yr^{-1}$)'
    value[value=='R90pTOT_JASO_2']           <- '$P_{JASO \\; p(JASO)>0.90}$ (mm $yr^{-1}$)'
    value[value=='R95pTOT_JASO_2']           <- '$P_{JASO \\; p(JASO)>0.95}$ (mm $yr^{-1}$)$'
    value[value=='R99pTOT_JASO_2']           <- '$P_{JASO \\; p(JASO)>0.99}$ (mm $yr^{-1}$)'
    value[value=='R90pTOT_ASO_2']            <- '$P_{ASO  \\; p(ASO)>0.90}$ (mm $yr^{-1}$)'
    value[value=='R95pTOT_ASO_2']            <- '$P_{ASO  \\; p(ASO)>0.95}$ (mm $yr^{-1}$)$'
    value[value=='R99pTOT_ASO_2']            <- '$P_{ASO  \\; p(ASO)>0.99}$ (mm $yr^{-1}$)'
    
    value[value=='temp_annual']              <- '$T_{Annual}$ (deg C)'
    value[value=='temp_JJAS']                <- '$T_{JJAS}$ (deg C)'
    value[value=='temp_JAS']                 <- '$T_{JAS}$ (deg C)'
    value[value=='temp_JASO']                <- '$T_{JASO}$ (deg C)'
    value[value=='temp_ASO']                 <- '$T_{ASO}$ (deg C)'
    
    value[value=='fert_kgN_km2']             <- '$N_{Fert}$ (kg N $km^{-2}$ $yr^{-1}$)'
    value[value=='dep_kgN_km2']              <- '$N_{Dep}$ (kg N $km^{-2}$ $yr^{-1}$)'
    value[value=='fert_dep_kgN_km2']         <- '$N_{Fert Dep}$ (kg N $km^{-2}$ $yr^{-1}$)'
    value[value=='urban']                    <- '$LU_{Urban}$ (%)'
    value[value=='agriculture']              <- '$LU_{Agriculture}$ (%)'
    value[value=='forest']                   <- '$LU_{Forest}$ (%)'
    value[value=='shrubland']                <- '$LU_{Shrub}$ (%)'
    value[value=='grassland']                <- '$LU_{Grass}$ (%)'
    value[value=='barren']                   <- '$LU_{Barren}$ (%)'

  }
  
  return(value)
}

# ______________________________________________________________________________
gauge_labeller <- function(var, value){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='HZS']            <- 'Absolute Gauge'
    value[value=='HHS']            <- 'With M.S.L.'
  }
  
  return(value)
}

# ______________________________________________________________________________
discharge_type_labeller <- function(var, value){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='O']            <- 'Observed discharge'
    value[value=='C']            <- 'Computed discharge'
  }
  
  return(value)
}

# ______________________________________________________________________________
basin_labeller <- function(var, value){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='Barak']                    <- 'Barak and others'
    value[value=='Brahmani_Baitarni']        <- 'Brahmani and Baitarni'
    value[value=='Brahmaputra']              <- 'Brahmaputra'
    value[value=='Cauvery']                  <- 'Cauvery'
    value[value=='East_Mahanadi_Pennar']     <- 'East flowing rivers between Mahanadi and Pennar'
    value[value=='East_Pennar_Kanyakumari']  <- 'East flowing rivers between Pennar and Kanyakumari'
    value[value=='Ganga']                    <- 'Ganga (within India)'
    value[value=='Godavari']                 <- 'Godavari'
    value[value=='Krishna']                  <- 'Krishna'
    value[value=='Mahanadi']                 <- 'Mahanadi'
    value[value=='Mahi']                     <- 'Mahi'
    value[value=='Minor_Myanmar_Bangladesh'] <- 'Minor rivers draining into Myanmar and Bangladesh'
    value[value=='Narmada']                  <- 'Narmada'
    value[value=='Pennar']                   <- 'Pennar'
    value[value=='Sabarmati']                <- 'Sabarmati'
    value[value=='Subernarekha']             <- 'Subernarekha'
    value[value=='Tapi']                     <- 'Tapi'
    value[value=='West_Kutch_Saurashtra']    <- 'West flowing rivers of Kutch and Saurashtra in. Luni'
    value[value=='West_Tadri_Kanyakumari']   <- 'West flowing rivers from Tadri to Kanyakumari'
    value[value=='West_Tapi_Tadri']          <- 'West flowing rivers from Tapi to Tadri'
    
    value[value=='Ghatsila']                 <- 'Subernarekha'
    value[value=='Jenapur']                  <- 'Brahmani and Baitarni'
    value[value=='Tikarapara']               <- 'Mahanadi'
    value[value=='Polavaram']                <- 'Godavari'
    value[value=='Vijayawada']               <- 'Krishna'
    value[value=='Musiri']                   <- 'Cauvery'
    value[value=='Urachikottai']             <- 'Cauvery'
    value[value=='Mandleshwar']              <- 'Narmada'
    value[value=='Khanpur']                  <- 'Mahi'
    value[value=='Nellore']                  <- 'Pennar'
    value[value=='Sarangkheda']              <- 'Tapi'
    value[value=='Vautha']                   <- 'Sabarmati'

  }
  
  return(value)
}

# ______________________________________________________________________________
USGS_basin_names <- function(var, value){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='01491000']       <-	'Choptank River near Greensboro, MD'
    value[value=='01576754']       <-	'Conestoga River at Conestoga, PA'
    value[value=='01578310']       <- 'Susquehanna River at Conowingo, MD'
    value[value=='01594440']       <-	'Patuxent River near Bowie, MD'
    value[value=='01668000']       <- 'Rappahannock River near Fredericksburg, VA'
    value[value=='01673000']       <-	'Pamunkey River near Hanover, VA'
    value[value=='01674500']       <-	'Mattaponi River near Beulahville, VA'
    value[value=='02035000']	     <- 'James River at Cartersville, VA'
    value[value=='02041650']	     <- 'Appomattox River at Matoaca, VA'
    value[value=='05420500']       <- 'Mississippi River at Clinton, IA'
    value[value=='06610000']       <- 'Missouri River at Omaha, NE'
    value[value=='06934500']       <- 'Missouri River at Hermann, MO'
    value[value=='07022000']       <- 'Mississippi River at Thebes, IL'
    value[value=='03303280']       <- 'Ohio River at Cannelton Dam at Cannelton, IN'
    value[value=='07373420']       <- 'Mississippi River near St. Francisville, LA'
    value[value=='07381495']       <- 'Lower Atchafalaya River at Melville, LA'
  }
  
  return(value)
}