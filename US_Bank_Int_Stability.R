################################################################################
########### US Bank Int: Bank stability forecasting [202101] ###################
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
name_script_file <- "US_Bank_SRE_panel_est_policy.R"
source(name_script_file, echo = F) #Compute systematic risk exposure using daily price


###############################################################################
############### Adding quarterly volatility calculation #######################
###############################################################################

func_vol_qtr <- function(df)
{
  #This function accepts a dataframe whose columns are
  #daily bank returns and rows correspond to quarters
  #and returns a vector whose entries are quarterly 
  #volatilities computed as sd(daily ret)*sqrt(days in quarter)
  
  vol_qtr <- (apply(df, 2, sd))*sqrt(nrow(df))
  return(vol_qtr)
}

# Compute quarterly volatilities
nest_quarter_vol_reg <- nest_quarter_pc_regression %>%
  dplyr::mutate('vol_qtr' = purrr::map(data_qtr_clean_2, func_vol_qtr))


###############################################################################
########## Transforming variables to long format ##############################
###############################################################################

vol_int_bank_long <- nest_quarter_vol_reg %>%
  dplyr::mutate('Bank' = purrr::map(SRE_2, func_pick_name)) %>%
  dplyr::select(Bank, Q_num, SRE_2, vol_qtr) %>%
  tidyr::unnest(., cols = c(Bank, SRE_2, vol_qtr)) %>% #unnest to long format
  dplyr::left_join(., bank_cusip_file, by = 'Bank') %>% #attach cusip identifier
  dplyr::distinct(cusip_8, Q_num, .keep_all = T) %>% #remove duplicates
  dplyr::select(cusip_8, Bank, Q_num, vol_qtr, SRE_2)

### Introducing lags in bank integration values ###

func_lag_vec <- function(vec)
{
  # This function accepts a vector and returns the same vector with one lag
  # after appending an NA to the first entry
  lag_vec <- c(NA, vec[1:(length(vec)-1)])
  return(lag_vec)
}

# Produce lag 1 SRE in wide format
lag1_int_df <- apply(SRE_US_banks_wide[, -1], 2, func_lag_vec) %>%
  as_tibble() %>%
  dplyr::mutate('Q_num' = SRE_US_banks_wide$Q_num) %>%
  dplyr::select(Q_num, everything())
  

lag1_int_long <- lag1_int_df %>%
  tidyr::gather(-Q_num, key = 'Bank', value = 'int_lag1')

# Arranging in a panel format
panel_int_vol_full <- vol_int_bank_long %>%
  dplyr::left_join(., lag1_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::left_join(., panel_SRE_full_2, by = c('cusip_8', 'Bank', 
                                               'Q_num', 'SRE_2')) %>%
  dplyr::arrange(., cusip_8)

######################################
### OLS = Pooling Panel Regression ###
######################################

formula_vol_int <- vol_qtr ~ int_lag1

# vol_int_ols <- plm::plm(formula_vol_int,
#                         panel_int_vol_full,
#                         model = 'pooling') %>%
#   summary(.)

vol_int_ols <- summary(lm(formula_vol_int, panel_int_vol_full))















