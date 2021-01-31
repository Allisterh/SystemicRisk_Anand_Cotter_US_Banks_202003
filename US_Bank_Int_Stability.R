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
source(name_script_file, echo = F) 


###############################################################################
############### Adding quarterly volatility calculation #######################
###############################################################################

func_vol_qtr <- function(df)
{
  #This function accepts a quarterly dataframe whose columns are
  #daily bank returns and returns a vector whose entries are quarterly 
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

func_lag_tib_wide <- function(df)
{
  # This function accepts a dataframe in wide format of bank integration 
  # and returns a tibble in wide format where each column vector is lagged
  # by one entry
  lag1_df <- apply(df[, -1], 2, func_lag_vec) %>%
    as_tibble() %>%
    dplyr::mutate('Q_num' = SRE_US_banks_wide$Q_num) %>%
    dplyr::select(Q_num, everything())
  
  return(lag1_df)
}

# Produce lag 1 SRE in wide format
lag1_int_df <- func_lag_tib_wide(SRE_US_banks_wide)  

# Produce lag 2 SRE in wide format
lag2_int_df <- func_lag_tib_wide(lag1_int_df)  

# Produce lag 3 SRE in wide format
lag3_int_df <- func_lag_tib_wide(lag2_int_df)  

# Produce lag 4 SRE in wide format
lag4_int_df <- func_lag_tib_wide(lag3_int_df)  


# Converting the wide formatted lags to long format
lag1_int_long <- lag1_int_df %>%
  tidyr::gather(-Q_num, key = 'Bank', value = 'int_lag1')

lag2_int_long <- lag2_int_df %>%
  tidyr::gather(-Q_num, key = 'Bank', value = 'int_lag2')

lag3_int_long <- lag3_int_df %>%
  tidyr::gather(-Q_num, key = 'Bank', value = 'int_lag3')

lag4_int_long <- lag4_int_df %>%
  tidyr::gather(-Q_num, key = 'Bank', value = 'int_lag4')

# Arranging in a panel format
panel_int_vol_full <- vol_int_bank_long %>%
  dplyr::left_join(., lag1_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::left_join(., lag2_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::left_join(., lag3_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::left_join(., lag4_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::left_join(., panel_SRE_full_2, by = c('cusip_8', 'Bank', 
                                               'Q_num', 'SRE_2')) %>%
  dplyr::arrange(., cusip_8)


# Regression formula
formula_vol_int <- vol_qtr ~ int_lag1 + int_lag2 + int_lag3 + int_lag4 + 
  bank_size + t1_t2_ratio + npa_ratio + loss_prov_ratio

###########################
### All banks full time ###
###########################

panel_data_vol <- panel_int_vol_full %>%
  dplyr::select(cusip_8, Q_num, vol_qtr, int_lag1, int_lag2, int_lag3, 
                int_lag4, bank_size, t1_t2_ratio, npa_ratio, loss_prov_ratio)

### OLS = Pooling Panel Regression ###
vol_int_ols <- plm::plm(formula_vol_int,
                        panel_data_vol,
                        model = 'pooling') %>%
  summary(.)

## Panel regression with bank and quarter fixed effects and double clustering ##
vol_int_panel <- func_panel_est(formula = formula_vol_int, 
                                panel_data = panel_data_vol)

###########################
### All banks: Pre 2006 ###
###########################

vol_int_panel_H1 <- func_panel_est(formula = formula_vol_int,
                                   panel_data = dplyr::filter(panel_data_vol, 
                                                              Q_num < 108/2))

############################
### All banks: Post 2006 ###
############################

vol_int_panel_H2 <- func_panel_est(formula = formula_vol_int,
                                   panel_data = dplyr::filter(panel_data_vol, 
                                                              Q_num >= 108/2))

################################
### Systemic banks full time ###
################################

# Redoing panel estimation for systemic banks
panel_data_vol_sys_full <- vol_int_bank_long %>%
  dplyr::left_join(., lag1_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::left_join(., lag2_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::left_join(., lag3_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::left_join(., lag4_int_long, by = c('Bank', 'Q_num')) %>%
  dplyr::right_join(., panel_SRE_large_2019, by = c('cusip_8', 'Bank', 
                                                   'Q_num', 'SRE_2')) %>%
  dplyr::arrange(., cusip_8)

panel_data_vol_sys <- panel_data_vol_sys_full %>%
  dplyr::select(cusip_8, Q_num, vol_qtr, int_lag1, int_lag2, int_lag3, 
                int_lag4, bank_size, t1_t2_ratio, npa_ratio, loss_prov_ratio)

## Panel regression with bank and quarter fixed effects and double clustering ##
vol_int_panel_sys <- func_panel_est(formula = formula_vol_int, 
                                    panel_data = panel_data_vol_sys)

###########################
### Sys banks: Pre 2006 ###
###########################

vol_int_panel_H1_sys <- func_panel_est(formula = formula_vol_int,
                                   panel_data = dplyr::filter(panel_data_vol_sys, 
                                                              Q_num < 108/2))

############################
### Sys banks: Post 2006 ###
############################

vol_int_panel_H2_sys <- func_panel_est(formula = formula_vol_int,
                                   panel_data = dplyr::filter(panel_data_vol_sys, 
                                                              Q_num >= 108/2))












