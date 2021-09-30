############################################################
########### US Bank Int Robustness #########################
############################################################

### Load libraries ###
library(tidyverse)
library(moments)
library(lmtest)
library(sandwich)
library(plm)
library(tseries)
library(Matrix)

### Source prior script for computing bank integration ### 
name_script_file <- "US_Bank_Int_Stability_Z.R"
source(name_script_file, echo = F) 

#########################################################
### Clark-West (2007) test for out of sample forecast ###
#########################################################

func_clark_west <- function(y_hat_big, y_hat_small, y)
{
  # This function accepts two nested model predictions y_hat_big
  # subset y_hat_small and compares out of sample forecasts 
  # generated from them
  # H0: y1 <= y2 
  # H1: y1 > y2
  
  e1     <- (y-y_hat_big)^2
  e2     <- (y-y_hat_small)^2
  e3     <- (y_hat_big - y_hat_small)^2
  
  f_hat <- (e1 - e2 + e3)
  
  P <- length(f_hat)
  f_mean <- mean(f_hat)
  t.stat <- sqrt(P)*f_mean/(var(f_hat-f_mean))^0.5
  #p.val <- 1 - pt(t.stat, df = df)
  p.val <- 1 - pt(t.stat, df = P-1)
  
  result <- data.frame(t.stat, p.val)
  colnames(result) <- c("t.stat","p.val")
  
  return(result)
}

########################################################
### Function to calculate CW07 p value for each bank ###
########################################################

func_p_value_cw07_bank <- function(df)
{
  form_small <- vol_qtr ~ bank_size + t1_t2_ratio + npa_ratio + loss_prov_ratio
  form_big <- vol_qtr ~ bank_size + t1_t2_ratio + npa_ratio + loss_prov_ratio +
    int_lag1 + int_lag2 + int_lag3 + int_lag4 + int_lag5
  
  y_hat_big <- predict(lm(form_big, df))
  y_hat_small <- predict(lm(form_small, df))
  y <- df$vol_qtr
    
  #cw_07 <- func_clark_west(y_hat_big, y_hat_small, y)
  cw_07 <- func_clark_west(y_hat_small, y_hat_big, y) 
   
  return(cw_07$p.val)
}


### Nesting panel data for vol regression for each bank and computing
### CW 2007 p values

nest_vol_CW07 <- panel_data_vol %>%
  tidyr::nest(data = !cusip_8) %>%
  dplyr::mutate('no_missing_data' = purrr::map(data, na.omit),
                'num_rows' = purrr::map_dbl(no_missing_data, nrow)) %>%
  dplyr::filter(num_rows >= 5) %>%
  dplyr::mutate('cw_07_p_value' = purrr::map_dbl(no_missing_data, 
                                                 func_p_value_cw07_bank))
