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
name_script_file <- "US_Bank_Int_Stability_Z.R"
source(name_script_file, echo = F) 

###################################################
############ CAPM beta for each bank ##############
###################################################

# Read beta data for US banks
name_data_file <- 'beta_suite_wrds.dta'
beta_banks_monthly <- haven::read_dta(name_data_file)
# Converting names to lowercase for uniformity (in merging etc.)
names(beta_banks_monthly) <- names(beta_banks_monthly) %>% tolower()

# Extracting relevant subsample based on permno link with cusip
beta_banks_monthly_2 <- beta_banks_monthly %>%
  dplyr::left_join(., link_permno_ncusip_cusip, by = 'permno') %>%
  dplyr::select(-c('n', 'ticker', 'permco')) %>%
  dplyr::mutate('Year' = lubridate::year(date),
                'Month' = lubridate::month(date),) %>%
  dplyr::filter(Month %in% c(3, 6, 9, 12)) %>%
  dplyr::mutate('Quarter' = dplyr::case_when(Month == 3 ~ 'Q1',
                                             Month == 6 ~ 'Q2',
                                             Month == 9 ~ 'Q3',
                                             Month == 12 ~ 'Q4')) %>%
  dplyr::mutate('datacqtr' = paste0(Year, Quarter)) %>%
  dplyr::left_join(., tibble_year_quarter, by = 'datacqtr')

# Arranging in panel data format
panel_data_beta <- panel_data_vol %>%
  dplyr::left_join(., beta_banks_monthly_2, by = c('cusip_8', 'Q_num')) %>%
  dplyr::select(-c(date, siccd, ncusip, comnam, Year, Month, Quarter, datacqtr)) %>%
  dplyr::distinct(cusip_8, Q_num, .keep_all = TRUE)

# Regression equation: 
formula_beta_int <- b_mkt ~ int_lag1 + int_lag2 + int_lag3 + int_lag4 + int_lag5 +
  bank_size + t1_t2_ratio + npa_ratio + loss_prov_ratio
formula_ivol_int <- ivol ~ int_lag1 + int_lag2 + int_lag3 + int_lag4 + int_lag5 +
  bank_size + t1_t2_ratio + npa_ratio + loss_prov_ratio
formula_tvol_int <- tvol ~ int_lag1 + int_lag2 + int_lag3 + int_lag4 + int_lag5 +
  bank_size + t1_t2_ratio + npa_ratio + loss_prov_ratio

# Beta: all banks all time
beta_int_panel_full <- func_panel_est(formula_beta_int, panel_data_beta)
# Beta: large banks all time
beta_int_panel_large <- func_panel_est(formula_beta_int, 
                                       dplyr::filter(panel_data_beta, 
                                                     cusip_8 %in% cusip_large_2019$cusip_8))
# Beta: all banks pre DF
beta_int_panel_pre_DF <- func_panel_est(formula_beta_int, dplyr::filter(panel_data_beta,
                                                                      Q_num < 71))
# Beta: all banks post DF
beta_int_panel_post_DF <- func_panel_est(formula_beta_int, dplyr::filter(panel_data_beta,
                                                                        Q_num >= 71))

#########################################

# Ivol: all banks all time
ivol_int_panel_full <- func_panel_est(formula_ivol_int, panel_data_beta)
# Ivol: large banks all time
ivol_int_panel_large <- func_panel_est(formula_ivol_int, 
                                       dplyr::filter(panel_data_beta, 
                                                     cusip_8 %in% cusip_large_2019$cusip_8))
# Ivol: all banks pre DF
ivol_int_panel_pre_DF <- func_panel_est(formula_ivol_int, dplyr::filter(panel_data_beta,
                                                                        Q_num < 71))
# Ivol: all banks post DF
ivol_int_panel_post_DF <- func_panel_est(formula_ivol_int, dplyr::filter(panel_data_beta,
                                                                         Q_num >= 71))

#######################################

# Tvol: all banks all time
tvol_int_panel_full <- func_panel_est(formula_tvol_int, panel_data_beta)
# Tvol: large banks all time
tvol_int_panel_large <- func_panel_est(formula_tvol_int, 
                                       dplyr::filter(panel_data_beta, 
                                                     cusip_8 %in% cusip_large_2019$cusip_8))
# Tvol: all banks pre DF
tvol_int_panel_pre_DF <- func_panel_est(formula_tvol_int, dplyr::filter(panel_data_beta,
                                                                        Q_num < 71))
# Tvol: all banks post DF
tvol_int_panel_post_DF <- func_panel_est(formula_tvol_int, dplyr::filter(panel_data_beta,
                                                                         Q_num >= 71))