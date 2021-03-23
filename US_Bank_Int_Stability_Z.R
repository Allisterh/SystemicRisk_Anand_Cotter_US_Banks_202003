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

panel_Z <- panel_SRE_full_2 %>%
  dplyr::mutate('Z_num' = com_eq_ratio/100 + roa)

num_obs_Z <- 30

panel_Z_nest <- panel_Z %>%
  dplyr::select(cusip_8, Q_num, roa) %>%
  dplyr::group_by(cusip_8) %>%
  tidyr::nest() %>%
  dplyr::mutate('nobs' = purrr::map_dbl(data, nrow)) %>%
  dplyr::filter(nobs >= num_obs_Z)

func_sd_roll <- function(df, window = 12)
{
  df_2 <- df %>% dplyr::arrange(., Q_num)
  z_num <- c(rep(NA, nrow(df_2)))
  q <- df_2$Q_num
  for (i in 1:(nrow(df_2)-window+1))
  {
    df_3 <- dplyr::filter(df_2, Q_num %in% q[i]:q[i+window-1])
    z_num[i+window-1] = sd(df_3$roa, na.rm = T)
  }
  
  return(z_num)
}

panel_Z_nest <- panel_Z_nest %>%
  dplyr::mutate('Z_den' = purrr:map(data, func_sd_roll))