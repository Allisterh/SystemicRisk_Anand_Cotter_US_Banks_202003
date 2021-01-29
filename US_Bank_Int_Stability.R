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

nest_quarter_vol_reg <- nest_quarter_pc_regression %>%
  dplyr::mutate('vol_qtr' = purrr::map(data_qtr_clean_2, func_vol_qtr))

