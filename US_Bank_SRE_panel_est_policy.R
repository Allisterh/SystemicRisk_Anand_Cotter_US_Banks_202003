
################################################################################
########### US Bank SRE: Panel estimation & policy analysis ####################
################################################################################

### Load libraries ###
library(tidyverse)
library(moments)
library(lmtest)
library(sandwich)
library(plm)
library(poweRlaw)

### Source prior script for computing systematic risk exposures ### 
name_script_file <- "US_Bank_SRE.R"
source(name_script_file, echo = F) #Compute systematic risk exposure using daily price

name_data_file <- 'CStat_Bank.dta'
data_US_banks_quarterly <- haven::read_dta(name_data_file)

# Old explantory variables
data_Cstat_expl <- data_US_banks_quarterly %>%
  dplyr::select(gvkey, datacqtr, fyearq, datadate,
                cusip, conm, conml, atq,
                capr1q, capr2q, capr3q,
                ceqq, cshoq, cstkq, curcdq, 
                curncdq, dvcq, fic, glaq,
                lseq, ltq, nimq, tniiq,
                piq, seqq, stboq, tbq, teqq, 
                tfdq, niinty, piy, tcoey, tcory,
                dlcq, dlttq, 
                dptcq, dpdcq, dpscq, fdfrq, ffsq,
                fhlbq, loq, mbshsq, mtgiq, niintq, 
                npatq, tdomdq, teqq,
                dd1q, dibq, ireoq,
                olbmq, ltmibq)


data_Cstat_expl_2 <- data_Cstat_expl %>%
  dplyr::rename('total_assets' = atq, 
                'total_borrowing' = tbq,
                'total_liab' = ltq,
                'net_interest_margin' = nimq,
                'gain_post_tax' = glaq,
                'income_pre_tax' = piq,
                'T1_ratio' = capr1q,
                'T2_ratio' = capr2q,
                'T1_T2_comb_ratio' = capr3q,
                'tot_liab_shareholder_equity' = lseq,
                'common_equity' = ceqq,
                'common_stock' = cstkq,
                'cash_div_common_stock' = dvcq,
                'total_deposits' = dptcq,
                'total_foreign_deposits' = tfdq,
                'debt_in_curr_liab' = dlcq,
                'total_long_term_debt' = dlttq,
                'total_shareholder_equity' = seqq,
                'total_stockholder_equity' = teqq,
                'total_noninterest_income' = tniiq,
                'net_interest_income' = niintq,
                'total_non_performing_assets'= npatq,
                'long_term_debt_due_1_yr' = dd1q)

table_summary_expl <- apply(data_Cstat_expl_2[, -c(1:7, 15:16, 18)], 
                            2, summary) %>% t()

# Based on how many non-missing values there are we select the following
# variables for panel estimation exercise

data_Cstat_expl_3 <- data_Cstat_expl_2 %>%
  dplyr::select(gvkey, datadate, cusip, conm,
                total_assets, total_deposits, common_equity,
                total_shareholder_equity, total_borrowing,
                total_long_term_debt, net_interest_income,
                debt_in_curr_liab, common_stock,
                total_noninterest_income, cash_div_common_stock,
                total_non_performing_assets, net_interest_margin,
                T1_T2_comb_ratio)


func_log10 <- function(vec)
{
  # This function returns log after ignoring
  # all entries with subzero values
  vec[vec <= 0] <- NA
  return(log10(vec))
}


data_Cstat_panel <- data_Cstat_expl_3 %>%
  dplyr::mutate('bank_size' = func_log10(total_assets),
                'deposit_ratio' = 100*(total_deposits/total_assets),
                'com_eq_ratio' = 100*(common_equity/total_assets),
                'shareholder_eq_ratio' = 100*(total_shareholder_equity/total_assets),
                'debt_ratio' = 100*(total_borrowing/total_assets),
                'debt_ratio_long_term' = 100*(total_long_term_debt/total_assets),
                'profit_ratio_net_int' = 100*(net_interest_income/total_assets),
                'debt_ratio_current' = 100*(debt_in_curr_liab/total_assets),
                'com_stock_ratio' = 100*(common_stock/total_assets),
                'profit_ratio_non_int' = 100*(total_noninterest_income/total_assets),
                'profit_ratio_cash_div' = 100*(cash_div_common_stock/total_assets),
                'npa_ratio' = 100*(total_non_performing_assets/total_assets),
                'net_int_margin' = net_interest_margin,
                't1_t2_ratio' = T1_T2_comb_ratio)