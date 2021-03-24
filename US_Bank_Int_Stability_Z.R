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
num_obs_Z <- 30

panel_Z_nest <- panel_Z %>%
  dplyr::select(cusip_8, Q_num, roa) %>%
  dplyr::group_by(cusip_8) %>%
  tidyr::nest() %>%
  dplyr::mutate('nobs' = purrr::map_dbl(data, nrow)) %>%
  dplyr::filter(nobs >= num_obs_Z)

# Function to calculate rolling standard deviation of ROA
func_sd_roll <- function(df, window = 12)
{
  # This function accepts a dataframe with first column as Quarter Numbers
  # and second column as ROA; and returns a vector with rolling standard 
  # deviations for ROA of length = window parameter
  
  df_2 <- df %>% dplyr::arrange(., Q_num) #Arrange in order of Q_num
  z_num <- c(rep(NA, nrow(df_2))) #Initialize the numerator as vector of NA
  q <- df_2$Q_num #Isolate quarter numbers
  
  for (i in 1:(nrow(df_2)-window+1))
  {
    # Isolate the part relevant for computing the rolling sd
    df_3 <- dplyr::filter(df_2, Q_num %in% q[i]:q[i+window-1])
    
    z_num[i+window-1] = sd(df_3$roa, na.rm = T) #Compute rolling sd for ROA
  }
  
  return(z_num)
}

# Append the Z score denominator as a new column
panel_Z_nest <- panel_Z_nest %>%
  dplyr::mutate('Z_den' = purrr::map(data, func_sd_roll))

# Z score denominator
panel_Z_den <- panel_Z_nest %>%
  dplyr::select(cusip_8, Z_den) %>%
  tidyr::unnest(., cols = Z_den)

# Joining with the panel with Z numerator
panel_Z <- panel_Z %>%
  dplyr::left_join(., panel_Z_den, by = 'cusip_8') %>%
  dplyr::mutate('Z_score' = Z_num/Z_den)

formula_Z_int <- Z_score ~ int_lag1 + int_lag2 + int_lag3 + int_lag4 + int_lag5 + 
  bank_size + t1_t2_ratio + npa_ratio + loss_prov_ratio