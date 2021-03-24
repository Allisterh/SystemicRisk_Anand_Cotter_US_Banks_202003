################################################################################
########### US Bank Int: Bank stability forecasting [202101] with Z scores #####
################################################################################

### Load libraries ###
library(tidyverse)
library(moments)
library(lmtest)
library(sandwich)
library(plm)
library(tseries)
library(Matrix)

### Source prior script for computing systematic risk exposures ### 
name_script_file <- "US_Bank_Int_Stability.R"
source(name_script_file, echo = F) 

########################################################################
############ Quarterly Z score calculations for each bank ##############
########################################################################

# Computing Z score numerator = net income/total assets (=ROA) + 
# common equity/total assets (common equity ratio/100)
panel_Z <- panel_SRE_full_2 %>%
  dplyr::mutate('Z_num' = com_eq_ratio/100 + roa)

# Minimum quarterly observations needed for computing Z denominator
num_obs_Z <- 20

panel_Z_nest <- panel_Z %>%
  dplyr::select(cusip_8, Q_num, roa) %>%
  dplyr::group_by(cusip_8) %>%
  tidyr::nest() %>%
  dplyr::mutate('nobs' = purrr::map_dbl(data, nrow)) %>%
  dplyr::filter(nobs >= num_obs_Z)

# Function to calculate rolling standard deviation of ROA
func_sd_roll <- function(df, window = 8)
{
  # This function accepts a dataframe with first column as Quarter Numbers
  # and second column as ROA; and returns a vector with rolling standard 
  # deviations for ROA of length = window parameter
  
  df_2 <- df %>% dplyr::arrange(., Q_num) #Arrange in order of Q_num
  z_den <- c(rep(NA, nrow(df_2))) #Initialize the denominator as vector of NA
  q <- df_2$Q_num #Isolate quarter numbers
  
  for (i in 1:(nrow(df_2)-window+1))
  {
    # Isolate the part relevant for computing the rolling sd
    df_3 <- dplyr::filter(df_2, Q_num %in% q[i]:q[i+window-1])
    
    z_den[i+window-1] = sd(df_3$roa, na.rm = T) #Compute rolling sd for ROA
  }
  
  return(z_den)
}

# Append the Z score denominator as a new column
panel_Z_nest <- panel_Z_nest %>%
  dplyr::mutate('Q_num' = purrr::map(data, function(df){return(df$Q_num)}), 
                'Z_den' = purrr::map(data, func_sd_roll))

# Z score denominator
panel_Z_den <- panel_Z_nest %>%
  dplyr::select(cusip_8, Q_num, Z_den) %>%
  tidyr::unnest(., cols = c('Q_num', 'Z_den'))

# Joining with the panel with Z numerator
panel_Z <- panel_Z %>%
  dplyr::left_join(., panel_Z_den, by = c('cusip_8', 'Q_num')) %>%
  dplyr::mutate('Z_score' = Z_num/Z_den)

#######################################################################
##### Ready for panel regression of Z scores on integration lags ######
#######################################################################

# Select relevant variables
panel_data_Z_int <- panel_Z %>%
  dplyr::select(., cusip_8, Q_num, Z_score, bank_size, t1_t2_ratio, npa_ratio,
                loss_prov_ratio, com_eq_ratio) 

# Join panel data with the lags of quarterly bank integration
panel_data_Z_int <- panel_data_Z_int %>%
  dplyr::left_join(., panel_data_vol, by = c('cusip_8', 'Q_num', 'bank_size', 
                                             't1_t2_ratio', 'npa_ratio', 
                                             'loss_prov_ratio', 'com_eq_ratio'))

# Formula for regression
formula_Z_int <- Z_score ~ int_lag1 + int_lag2 + int_lag3 + int_lag4 + int_lag5 + 
  bank_size + t1_t2_ratio + npa_ratio + loss_prov_ratio

### All banks, full duration ###

Z_int_panel <- func_panel_est(formula_Z_int, panel_data_Z_int)

### All banks, pre Dodd-Frank ###

Z_int_panel_pre_DF <- func_panel_est(formula_Z_int,
                                     dplyr::filter(panel_data_Z_int, Q_num < 71))

### All banks, post Dodd-Frank ###

Z_int_panel_post_DF <- func_panel_est(formula_Z_int,
                                     dplyr::filter(panel_data_Z_int, Q_num >= 71))


### Large banks, full duration ###

Z_int_panel_large <- func_panel_est(formula_Z_int, 
                                    dplyr::filter(panel_data_Z_int, 
                                                  cusip_8 %in% cusip_large_2019$cusip_8))


### Large banks, pre Dodd-Frank ###

Z_int_panel_large_pre_DF <- func_panel_est(formula_Z_int, 
                                    dplyr::filter(panel_data_Z_int, 
                                                  cusip_8 %in% cusip_large_2019$cusip_8 &
                                                    Q_num < 71))

### Large banks, post Dodd-Frank ###

Z_int_panel_large_post_DF <- func_panel_est(formula_Z_int, 
                                    dplyr::filter(panel_data_Z_int, 
                                                  cusip_8 %in% cusip_large_2019$cusip_8 &
                                                    Q_num >= 71))

#####################################################
##### Behavior During Crises ########################
#####################################################

panel_data_Z_int_crises <- panel_data_Z_int %>%
  dplyr::mutate('GR' = dplyr::case_when(Q_num %in% seq(59, 65) ~ 1,
                                        TRUE ~ 0),
                'EZ' = dplyr::case_when(Q_num %in% seq(69, 77) ~ 1,
                                        TRUE ~ 0),
                'LTCM' = dplyr::case_when(Q_num %in% seq(21, 23) ~ 1,
                                          TRUE ~ 0), 
                'Dotcom' = dplyr::case_when(Q_num %in% seq(37, 39) ~ 1, 
                                            TRUE ~ 0),
                'Crises' = dplyr::case_when(GR == 1 | EZ == 1 | LTCM == 1 | Dotcom == 1 ~ 1,
                                            TRUE ~ 0))

### All crises ###

Z_int_panel_cries_agg <- func_panel_est(formula_Z_int,
                                        dplyr::filter(panel_data_Z_int_crises, 
                                                      Crises == 1))

### GR ###

Z_int_panel_cries_GR <- func_panel_est(formula_Z_int,
                                        dplyr::filter(panel_data_Z_int_crises, 
                                                      GR == 1))

### EZ ###

Z_int_panel_cries_EZ <- func_panel_est(formula_Z_int,
                                        dplyr::filter(panel_data_Z_int_crises, 
                                                      EZ == 1))

### LTCM ###

Z_int_panel_cries_LTCM <- func_panel_est(formula_Z_int,
                                        dplyr::filter(panel_data_Z_int_crises, 
                                                      LTCM == 1))

### Dotcom ###
Z_int_panel_cries_dotcom <- func_panel_est(formula_Z_int,
                                        dplyr::filter(panel_data_Z_int_crises, 
                                                      Dotcom == 1))

##############################################
######## Large banks subsample ###############
##############################################

### All crises ###

Z_int_panel_cries_agg_large <- func_panel_est(formula_Z_int,
                                        dplyr::filter(panel_data_Z_int_crises, 
                                                      Crises == 1 & 
                                                        cusip_8 %in% cusip_large_2019$cusip_8))

### GR ###

Z_int_panel_cries_GR_large <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_crises, 
                                                     GR == 1 & 
                                                       cusip_8 %in% cusip_large_2019$cusip_8))

### EZ ###

Z_int_panel_cries_EZ_large <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_crises, 
                                                     EZ == 1 & 
                                                       cusip_8 %in% cusip_large_2019$cusip_8))

### LTCM ###

Z_int_panel_cries_LTCM_large <- func_panel_est(formula_Z_int,
                                         dplyr::filter(panel_data_Z_int_crises, 
                                                       LTCM == 1 & 
                                                         cusip_8 %in% cusip_large_2019$cusip_8))

### Dotcom ###
Z_int_panel_cries_dotcom_large <- func_panel_est(formula_Z_int,
                                           dplyr::filter(panel_data_Z_int_crises, 
                                                         Dotcom == 1 & 
                                                           cusip_8 %in% cusip_large_2019$cusip_8))




#################################
##### Bull and Bear TED VIX #####
#################################

panel_data_Z_int_bear_bull <- panel_data_Z_int %>%
  dplyr::left_join(., bull_bear_TED_VIX, by = 'Q_num')

### TED bear ###

Z_int_panel_TED_bear <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_bear_bull,
                                                     bull_bear_TED == 'L'))

### TED bull ###

Z_int_panel_TED_bull <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_bear_bull,
                                                     bull_bear_TED == 'H'))

### VIX bear ###

Z_int_panel_VIX_bear <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_bear_bull,
                                                     bull_bear_VIX == 'L'))

### VIX bull ###

Z_int_panel_VIX_bull <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_bear_bull,
                                                     bull_bear_VIX == 'H'))

#################################################################
################# Large Banks subsample #########################
#################################################################

### TED bear ###

Z_int_panel_TED_bear_large <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_bear_bull,
                                                     bull_bear_TED == 'L' &
                                                       cusip_8 %in% cusip_large_2019$cusip_8))

### TED bull ###

Z_int_panel_TED_bull_large <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_bear_bull,
                                                     bull_bear_TED == 'H' &
                                                       cusip_8 %in% cusip_large_2019$cusip_8))

### VIX bear ###

Z_int_panel_VIX_bear_large <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_bear_bull,
                                                     bull_bear_VIX == 'L' &
                                                       cusip_8 %in% cusip_large_2019$cusip_8))

### VIX bull ###

Z_int_panel_VIX_bull_large <- func_panel_est(formula_Z_int,
                                       dplyr::filter(panel_data_Z_int_bear_bull,
                                                     bull_bear_VIX == 'H'&
                                                       cusip_8 %in% cusip_large_2019$cusip_8))
