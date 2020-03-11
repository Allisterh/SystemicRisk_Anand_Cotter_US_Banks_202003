########################################################################
########### Redoing US Bank project for journal submission #############
########################################################################

### Load libraries ###

library(tidyverse)
library(moments)



############################
### Directory_Management ###
############################

# For reproduction of this script change the address
# of the directories to conform to their location in 
# the host machine.

### Read file ### 

dir_working <- getwd() #address of present directory
setwd("..") #move control to parent folder (where data file lives)
file_name_US_banks_daily <- "SIC_6000_6799_20200306.dta" #CRSP daily return file

# Calculate time needed to read whole ~9 GB file, not suitable for small RAMs
time_pre_read_CRSP <- Sys.time()

data_US_banks_daily <- haven::read_dta(file_name_US_banks_daily)

time_post_read_CRSP <- Sys.time()

message("Read CRSP file. Time taken to read file = ", 
        round(time_post_read_CRSP - time_pre_read_CRSP, 2), " min")

setwd(dir_working) #move back control to child directory

#############################################
### Data Filtration, Cleaning and Tidying ###
#############################################

### Admissible SIC codes
ind_comm_banks <- c(6020:6029) #commercial banks
ind_saving_inst <- c(6030:6039) #saving institutions
ind_credit_union <- c(6060:6069) #credit unions
ind_bank_hold <- c(6710:6712) #bank holding companies

ind_bank_use <- c(ind_comm_banks, ind_saving_inst,
                  ind_credit_union, ind_bank_hold) #use only these

### Admissible share codes (source: http://www.crsp.com/products/documentation/data-definitions-1)

ind_share_code_common <- c(10, 11) #only common shares included

year_min <- 1993

### Filter CRSP data ###


# Calculate time needed to filter whole ~9 GB file
time_pre_filter_CRSP <- Sys.time()

# Filter banks with common shares post 1994 and nominal price > 1
data_US_banks_daily_2 <- data_US_banks_daily %>% 
  dplyr::filter(siccd %in% ind_bank_use | hsiccd %in% ind_bank_use) %>% #ignore non-banks
  dplyr::filter(shrcd %in% ind_share_code_common) %>% #include common shares only
  dplyr::filter(lubridate::year(date) >= year_min) %>% #only banks post '94
  dplyr::filter(prc > 1) #ignore banks with nominal price <= $1

time_post_filter_CRSP <- Sys.time()

message("Filtered CRSP file. Time taken to filter file = ", 
        round(time_post_filter_CRSP - time_pre_filter_CRSP, 2), " min")

# Remove from workspace, the (very) heavy original data file
rm(data_US_banks_daily)

# Select bank names, codes and daily stock returns
returns_daily_banks_US <- data_US_banks_daily_2 %>%
  dplyr::select(date, comnam, siccd, ret, hsiccd, shrcd, ncusip, cusip)

# Remove the (heavy) derived data file
# rm(data_US_banks_daily_2)

# Label banks according to type
# returns_daily_banks_US <- returns_daily_banks_US %>%
#   dplyr::mutate('bank_type' = dplyr::case_when(siccd %in% ind_bank_hold ~ "HC", #holding company
#                                                siccd %in% ind_comm_banks ~ "CB", #commercial banks
#                                                siccd %in% ind_credit_union ~ "CU", #credit unions
#                                                siccd %in% ind_saving_inst ~ "SI")) #savings institutions

# Name of banks in the sample
name_banks_US <- returns_daily_banks_US %>% 
  dplyr::select(comnam) %>%
  dplyr::distinct()

returns_daily_banks_US <- returns_daily_banks_US %>%
  dplyr::mutate('Year' = lubridate::year(date),
                'Quarter' = lubridate::quarter(date)) %>%
  dplyr::select(date, Year, Quarter, everything())

returns_daily_banks_US <- returns_daily_banks_US %>%
  dplyr::mutate('Q_num' = 4*(Year - (year_min - 1) - 1) + Quarter) %>% #this formula generates quarter sequence
  dplyr::select(date, Year, Quarter, Q_num, everything())

# Nest quarterly 
nest_quarter_banks_US <- returns_daily_banks_US %>%
  dplyr::group_by(Q_num) %>%
  tidyr::nest()


func_rm_date_dupli <- function(df)
{
  # This function accepts a dataframe
  # and returns non-duplicated date rows 
  temp_df <- df %>%
    dplyr::group_by(date, comnam) %>%
    dplyr::distinct(., date, .keep_all = T)
  
  return(temp_df)
}

# Remove duplicated dates
nest_quarter_banks_US <- nest_quarter_banks_US %>%
  dplyr::mutate('data_no_duplic' = purrr::map(data, func_rm_date_dupli)) %>%
  dplyr::select(-data)

func_ret_wide <- function(df)
{
  # This function accepts the full quarterly
  # dataframe and returns date, bank and daily return
  # in wide format
  temp_l <- df %>% dplyr::select(date, comnam, ret)
  # In wide format
  temp_w <- tidyr::spread(temp_l, key = comnam, value = ret)
  
  return(temp_w)
}

func_med_NA_df <- function(df)
{
  # This function accepts a data frame with missing values and
  # returns a data frame where missing values are replaced with
  # column medians
  func_med_NA_vec <- function(vec)
  {
    vec[is.na(vec) | is.infinite(vec) | is.nan(vec)] <- median(vec, na.rm = T)
    return(vec)
  }
  
  df_2 <- apply(df, 2, func_med_NA_vec)
  
  return(df_2)
}

func_cov <- function(df)
{
  # This function accepts a dataframe
  # ignores the date column and computes
  # the covariance matrix of non-missing
  # data entries
  df_2 <- na.omit(df[, -1])
  return(cov(df_2))
}

nest_quarter_banks_US <- nest_quarter_banks_US %>%
 dplyr::mutate('data_qtr_wide' = purrr::map(data_no_duplic, func_ret_wide))
               , 
               'data_qtr_clean' = purrr::map(data_qtr_wide, func_med_NA_df),
               'cov_matrix' = purrr::map(data_qtr_wide, func_cov),
               'eig_val' = purrr::map(cov_matrix, function(df){return(eigen(df)$values)}),
               'eig_vec' = purrr::map(cov_matrix, function(df){return(eigen(df)$vectors)}),
               'share' = purrr::map(eig_val, function(vec){return(cumsum(vec)/sum(vec))}))

