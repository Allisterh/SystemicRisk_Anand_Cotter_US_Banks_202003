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

