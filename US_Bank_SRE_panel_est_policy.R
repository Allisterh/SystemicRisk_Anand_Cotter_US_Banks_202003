
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
name_script_file <- "US_Bank_SRE_results.R"
source(name_script_file, echo = F) #Compute systematic risk exposure using daily price

name_data_file <- 'CStat_Bank.dta'
data_US_banks_quarterly <- haven::read_dta(name_data_file)

# Explantory variables
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

# Renaming for clarity
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

# Summarizing 
table_summary_expl <- apply(data_Cstat_expl_2[, -c(1:7, 15:16, 18)], 
                            2, summary) %>% t()

### Based on how many non-missing values there are we select the following ###
### variables for panel estimation exercise ##################################

data_Cstat_expl_3 <- data_Cstat_expl_2 %>%
  dplyr::select(gvkey, fyearq, datadate, datacqtr, cusip, fic, conm,
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


data_Cstat_expl_4 <- data_Cstat_expl_3 %>%
  dplyr::mutate('bank_size' = func_log10(total_assets),
                'deposit_ratio' = 100*(total_deposits/total_assets),
                'com_eq_ratio' = 100*(common_equity/total_assets),
                'shareholder_eq_ratio' = 100*(total_shareholder_equity/total_assets),
                'debt_ratio' = 100*(total_borrowing/total_assets),
                'debt_ratio_long_term' = 100*(total_long_term_debt/total_assets),
                'net_int_income_ratio' = 100*(net_interest_income/total_assets),
                'debt_ratio_current' = 100*(debt_in_curr_liab/total_assets),
                'com_stock_ratio' = 100*(common_stock/total_assets),
                'non_int_income_ratio' = 100*(total_noninterest_income/total_assets),
                'cash_div_ratio' = 100*(cash_div_common_stock/total_assets),
                'npa_ratio' = 100*(total_non_performing_assets/total_assets),
                'net_int_margin' = net_interest_margin,
                't1_t2_ratio' = T1_T2_comb_ratio)

data_Cstat_panel <- data_Cstat_expl_4 %>%
  dplyr::select(gvkey:conm, bank_size:t1_t2_ratio)

#### 8-digit cusip column for matching ####

data_Cstat_panel <- data_Cstat_panel %>%
  dplyr::mutate('cusip_8' = substr(cusip, 1, 8)) %>%
  dplyr::select(conm, datacqtr, cusip_8, 
                everything()) %>%
  dplyr::select(-c(gvkey, datadate, cusip))

### Matching bank name to 8-digit cusip ###
bank_cusip_file <- returns_daily_banks_US %>%
  dplyr::select(comnam, ncusip, cusip) %>%
  dplyr::distinct() %>%
  dplyr::rename('Bank' = comnam, 'cusip_8' = cusip)

### Attaching cusip code to bank SRE calculation ###

SRE_US_banks_long_2 <- SRE_US_banks_long %>%
  dplyr::left_join(., bank_cusip_file, by = 'Bank') %>%
  dplyr::arrange(Bank)

# Remove duplicate cusip-Q_num entries
SRE_US_banks_long_3 <- SRE_US_banks_long_2 %>%
  dplyr::distinct(cusip_8, Q_num, .keep_all = T)

## Attaching year-quarter to SRE long data ##

year_min <- min(returns_daily_banks_US$Year)
year_max <- max(returns_daily_banks_US$Year)

year_quarter <- paste0(rep(1993:2019, each = 4),
                       rep(c('Q1', 'Q2', 'Q3', 'Q4'), 
                           (year_max-year_min+1)))
quarter_numbers <- 1:108

tibble_year_quarter <- tibble::tibble('Q_num' = quarter_numbers,
                                      'datacqtr' = year_quarter)

data_Cstat_panel_2 <- data_Cstat_panel %>%
  dplyr::left_join(., tibble_year_quarter, by = 'datacqtr') %>%
  dplyr::select(conm, datacqtr, Q_num, fyearq, cusip_8, everything())  

# Remove duplicate cusip-Q_num entries
data_Cstat_panel_3 <- data_Cstat_panel_2 %>%
  dplyr::distinct(cusip_8, Q_num, .keep_all = T)

func_cusip_check <- function(cusip_8)
{
  # This function accepts an 8 digit cusip
  # and returns 1 if the last two digits
  # indicate common stocks (10 or 11) else
  # returns 0
  last_2_char <- substr(cusip_8, 7, 8)
  if (last_2_char == '10' |
      last_2_char == '11')
  {
    return(1)
  } else
  {
    return(0)
  }
}


#### Arranging the final panel data form ####

panel_SRE_full_left <- SRE_US_banks_long_3 %>%
  dplyr::left_join(., data_Cstat_panel_3, by = c('Q_num', 'cusip_8')) %>%
  dplyr::select(cusip_8, Q_num, datacqtr, ncusip, 
                Bank, conm, fyearq, fic, everything()) %>%
  dplyr::mutate('cusip_status' = purrr::map_dbl(cusip_8, func_cusip_check))


#############################################################################
################# Panel estimations begin here ##############################
#############################################################################

panel_SRE_full <- panel_SRE_full_left %>%
  dplyr::select(-c(ncusip, fyearq, fic)) %>%
  dplyr::select(cusip_8, Q_num, cusip_status, 
                datacqtr, Bank, conm, everything()) %>%
  dplyr::filter(cusip_status == 1) %>%
  dplyr::ungroup() 

panel_SRE_full_2 <- panel_SRE_full %>%
  dplyr::distinct(cusip_8, Q_num, .keep_all = T)


### Main model specification ###

formula_main <- SRE_2 ~ bank_size + deposit_ratio + com_eq_ratio +
  net_int_margin + npa_ratio + t1_t2_ratio + non_int_income_ratio +
  cash_div_ratio + debt_ratio_current 

func_panel_est <- function(formula = formula_main, 
                           panel_data = panel_SRE_full_2, 
                           model_spec = 'within',
                           fixed_effect = 'twoways')
{
  # This function accepts a formula, panel data matrix and model spec
  # and returns the summary of an unbalanced, fixed-effects panel regression
  # with clustered robust standard errors with clustering at both 
  # Country and Year levels
  
  # Panel estimation with fixed effects
  plm_fixed <- plm::plm(formula, 
                        data = panel_data, 
                        model = model_spec,
                        type = "HC0", 
                        effect = fixed_effect)
  
  # Robust, clustered standard errors
  vcov_err <- plm::vcovDC(plm_fixed) #Double clustering
  
  plm_fixed_robust <- lmtest::coeftest(plm_fixed, vcov. = vcov_err)
  
  plm_out <- summary(plm_fixed)
  
  # Include robust clustered errors
  plm_out$coefficients <- unclass(plm_fixed_robust) 
  
  return(plm_out)
}

###########################################################################
################# TABLE 6: Descriptive Stats ##############################
###########################################################################

func_summary <- function(vec)
{
  return(data.frame('Min' = min(vec),
                    'Max' = max(vec),
                    'Mean' = mean(vec),
                    'Med' = median(vec),
                    'SD' = sd(vec),
                    'IQR' = IQR(vec)))
}

panel_variables <- panel_SRE_full_2 %>%
  dplyr::select(SRE_2, bank_size, deposit_ratio, com_eq_ratio, 
                net_int_margin, npa_ratio, t1_t2_ratio, 
                non_int_income_ratio, cash_div_ratio, debt_ratio_current)

print_table_6 <- apply(na.omit(panel_variables), 2, func_summary) %>% 
  sapply(., rbind)

###########################################################################
################# TABLE 7: Correlation Matrix #############################
###########################################################################

print_table_7 <- cor(panel_variables, use = 'complete.obs')

################################
### Panel estimation results ###
################################

## All banks full time
panel_est_main_model_full <- func_panel_est(formula = formula_main,
                                       panel_data = panel_SRE_full_2,
                                       fixed_effect = 'twoways')

# Pooled
panel_est_main_model_pool <- func_panel_est(model_spec = 'pooling')

## All banks pre 2006
panel_est_main_model_H1 <- func_panel_est(formula = formula_main,
                                          panel_data = panel_SRE_full_2 %>%
                                            dplyr::filter(Q_num < max(Q_num)/2),
                                          fixed_effect = 'twoways')
# Pooled pre 2006
panel_est_main_model_H1_pool <- func_panel_est(panel_data = panel_SRE_full_2 %>%
                                              dplyr::filter(Q_num < max(Q_num)/2),
                                            model_spec = 'pooling')

## All banks post 2006
panel_est_main_model_H2 <- func_panel_est(formula = formula_main,
                                       panel_data = panel_SRE_full_2 %>%
                                         dplyr::filter(Q_num >= max(Q_num)/2),
                                       fixed_effect = 'twoways')
# Pooled post 2006
panel_est_main_model_H2_pool <- func_panel_est(panel_data = panel_SRE_full_2 %>%
                                                 dplyr::filter(Q_num >= max(Q_num)/2),
                                               model_spec = 'pooling')


### Subsample: Large banks (>$1B in 2019Q1) ###

cusip_large_2019 <- panel_SRE_full_2 %>%
  dplyr::filter(Q_num == 105) %>%
  dplyr::filter(bank_size >= 3) %>%
  dplyr::select(cusip_8, Bank, conm)

panel_SRE_large_2019 <- panel_SRE_full_2 %>%
  dplyr::filter(cusip_8 %in% cusip_large_2019$cusip_8)

# Large banks full time
panel_est_main_model_large <- func_panel_est(formula = formula_main,
                                             panel_data = panel_SRE_large_2019,
                                             fixed_effect = 'twoways')
# Large banks pooled
panel_est_main_model_large_pool <- func_panel_est(panel_data = panel_SRE_large_2019,
                                                  model_spec = 'pooling')

# Large banks pre 2006
panel_est_main_model_large_H1 <- func_panel_est(formula = formula_main,
                                             panel_data = panel_SRE_large_2019 %>%
                                               dplyr::filter(Q_num < max(Q_num)/2),
                                             fixed_effect = 'twoways')
# Large banks pre 2006 pooled
panel_est_main_model_large_H1_pool <- func_panel_est(panel_data = panel_SRE_large_2019 %>%
                                                  dplyr::filter(Q_num < max(Q_num)/2),
                                                  model_spec = 'pooling')

# Large banks post 2006
panel_est_main_model_large_H2 <- func_panel_est(formula = formula_main,
                                                panel_data = panel_SRE_large_2019 %>%
                                                  dplyr::filter(Q_num >= max(Q_num)/2),
                                                fixed_effect = 'twoways')
# Large banks post 2006 pooled
panel_est_main_model_large_H2_pool <- func_panel_est(panel_data = panel_SRE_large_2019 %>%
                                                       dplyr::filter(Q_num >= max(Q_num)/2),
                                                     model_spec = 'pooling')


#########################################################################
############## Dodd-Frank Act Policy Effects ############################
#########################################################################

bank_cusip_sys <- bank_cusip_file %>%
  dplyr::filter(Bank %in% banks_systemic)

### Tier 1 and 2 combined capital ratio ###

# nest_SRE_t1t2 <- panel_SRE_full_2 %>%
#   dplyr::select(Q_num, t1_t2_ratio) %>%
#   dplyr::group_by(Q_num) %>%
#   dplyr::arrange(Q_num) %>%
#   tidyr::nest()

nest_SRE_sys <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, cusip_8, t1_t2_ratio, com_eq_ratio) %>%
  dplyr::filter(cusip_8 %in% bank_cusip_sys$cusip_8) %>%
  dplyr::group_by(Q_num) %>%
  dplyr::arrange(Q_num) %>%
  tidyr::nest()


nest_SRE_sys <- nest_SRE_sys %>%
  dplyr::mutate('med_t1t2_sys' = purrr::map_dbl(data, 
                                                function(tib){return(median(tib$t1_t2_ratio, 
                                                                            na.rm = T))}),
                'med_com_eq_sys' = purrr::map_dbl(data, 
                                                  function(tib){return(median(tib$com_eq_ratio, 
                                                                              na.rm = T))}))


### Plotting the median systemic bank's ratios (Fig 6+7) ###
x_breaks_sys <- seq(1, 108, by = 4)
x_labels_sys <- paste0(seq(1993, 2019), "Q1")

## T1 T2 combined
plot_med_t1t2_sys <- ggplot(nest_SRE_sys, aes(Q_num, med_t1t2_sys)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 71, linetype = 'dashed') +
  geom_vline(xintercept = 63, linetype = 'dotdash') +
#  ylim(11.5, 16) +
  scale_x_continuous(breaks = x_breaks_sys,
                     labels = x_labels_sys) +
  labs(x = "", y = "Median systemic bank's Tier 1 and 2 ratio (combined, in %)") +
  theme_bw() +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

## Common equity ratio
plot_med_com_eq_sys <- ggplot(nest_SRE_sys, aes(Q_num, med_com_eq_sys)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 71, linetype = 'dashed') +
  geom_vline(xintercept = 65, linetype = 'dotdash') +
  scale_x_continuous(breaks = x_breaks_sys,
                     labels = x_labels_sys) +
  labs(x = "", y = "Median systemic bank's common equity ratio (in %)") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

###############################################################################
######## TABLE 5: Mean variables before and after Dodd-Frank ##################
###############################################################################

SRE_sys_Dodd_Frank_pre <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, cusip_8, SRE_2, t1_t2_ratio, com_eq_ratio) %>%
  dplyr::filter(cusip_8 %in% bank_cusip_sys$cusip_8) %>%
  dplyr::filter(Q_num < 71)
#  dplyr::filter(Q_num < 63)

SRE_sys_Dodd_Frank_post <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, cusip_8, SRE_2, t1_t2_ratio, com_eq_ratio) %>%
  dplyr::filter(cusip_8 %in% bank_cusip_sys$cusip_8) %>%
  dplyr::filter(Q_num >= 71)
#  dplyr::filter(Q_num >= 63)

### Test of equality of pooled means ###

## Tier 1 and 2 ratio
# T test (parametric)
mean_test_t1t2 <- t.test(SRE_sys_Dodd_Frank_pre$t1_t2_ratio,
                         SRE_sys_Dodd_Frank_post$t1_t2_ratio,
                         alternative = "less")
# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_t1t2_wilcox <- wilcox.test(SRE_sys_Dodd_Frank_pre$t1_t2_ratio, 
                                SRE_sys_Dodd_Frank_post$t1_t2_ratio, 
                                alternative = "less")
# KS test
mean_test_t1t2_ks <- ks.test(SRE_sys_Dodd_Frank_pre$t1_t2_ratio, 
                        SRE_sys_Dodd_Frank_post$t1_t2_ratio,
                        alternative = "greater")

## Common equity ratio
# T test (parametric)
mean_test_com_eq <- t.test(SRE_sys_Dodd_Frank_pre$com_eq_ratio,
                         SRE_sys_Dodd_Frank_post$com_eq_ratio,
                         alternative = "less")
# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_com_eq_wilcox <- wilcox.test(SRE_sys_Dodd_Frank_pre$com_eq_ratio, 
                                     SRE_sys_Dodd_Frank_post$com_eq_ratio, 
                                     alternative = "less")
# KS test
mean_test_com_eq_ks <- ks.test(SRE_sys_Dodd_Frank_pre$com_eq_ratio, 
                        SRE_sys_Dodd_Frank_post$com_eq_ratio,
                        alternative = "greater")

## SRE
# T test (parametric)
mean_test_SRE <- t.test(SRE_sys_Dodd_Frank_pre$SRE_2,
                           SRE_sys_Dodd_Frank_post$SRE_2,
                           alternative = "less")
# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_SRE_wilcox <- wilcox.test(SRE_sys_Dodd_Frank_pre$SRE_2, 
                                       SRE_sys_Dodd_Frank_post$SRE_2, 
                                       alternative = "less")
# KS test
mean_test_SRE_ks <- ks.test(SRE_sys_Dodd_Frank_pre$SRE_2, 
                               SRE_sys_Dodd_Frank_post$SRE_2,
                               alternative = "greater")

############################################################################
############ Mean and variance tests during crises #########################
############################################################################

####### SRE ########

panel_SRE_crises <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, SRE_2) %>%
  dplyr::arrange(Q_num) %>%
  dplyr::mutate('GR' = dplyr::case_when(Q_num %in% seq(60, 66) ~ 1,
                                        TRUE ~ 0),
                'EZ' = dplyr::case_when(Q_num %in% seq(70, 78) ~ 1,
                                        TRUE ~ 0),
                'Crises' = dplyr::case_when(GR == 1 | EZ == 1 ~ 1,
                                            TRUE ~ 0))

### The Great Recession ###

SRE_GR <- panel_SRE_crises %>%
  dplyr::filter(GR == 1)
SRE_no_GR <- panel_SRE_crises %>%
  dplyr::filter(GR == 0)

# T test (parametric)
mean_test_SRE_GR <- t.test(SRE_GR$SRE_2,
                        SRE_no_GR$SRE_2,
                        alternative = "greater")

# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_SRE_GR_wilcox <- wilcox.test(SRE_GR$SRE_2,
                                       SRE_no_GR$SRE_2,
                                       alternative = "greater")
# KS test
mean_test_SRE_GR_ks <- ks.test(SRE_GR$SRE_2,
                               SRE_no_GR$SRE_2,
                               alternative = "less")

### The Eurozone Crisis ###

SRE_EZ <- panel_SRE_crises %>%
  dplyr::filter(EZ == 1)
SRE_no_EZ <- panel_SRE_crises %>%
  dplyr::filter(EZ == 0)

# T test (parametric)
mean_test_SRE_EZ <- t.test(SRE_EZ$SRE_2,
                           SRE_no_EZ$SRE_2,
                           alternative = "greater")

# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_SRE_EZ_wilcox <- wilcox.test(SRE_EZ$SRE_2,
                                       SRE_no_EZ$SRE_2,
                                       alternative = "greater")
# KS test
mean_test_SRE_EZ_ks <- ks.test(SRE_EZ$SRE_2,
                               SRE_no_EZ$SRE_2,
                               alternative = "less")


### Crises ###

SRE_GREZ <- panel_SRE_crises %>%
  dplyr::filter(Crises == 1)
SRE_no_GREZ <- panel_SRE_crises %>%
  dplyr::filter(Crises == 0)

# T test (parametric)
mean_test_SRE_GREZ <- t.test(SRE_GREZ$SRE_2,
                           SRE_no_GREZ$SRE_2,
                           alternative = "greater")

# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_SRE_GREZ_wilcox <- wilcox.test(SRE_GREZ$SRE_2,
                                       SRE_no_GREZ$SRE_2,
                                       alternative = "greater")
# KS test
mean_test_SRE_GREZ_ks <- ks.test(SRE_GREZ$SRE_2,
                               SRE_no_GREZ$SRE_2,
                               alternative = "less")


####### Mean PC1 contribution ########


######################################
############ TABLE 4 #################
######################################

nest_share_crises <- nest_share %>%
  dplyr::mutate('GR' = dplyr::case_when(Q_num %in% seq(60, 66) ~ 1,
                                        TRUE ~ 0),
                'EZ' = dplyr::case_when(Q_num %in% seq(70, 78) ~ 1,
                                        TRUE ~ 0),
                'Crises' = dplyr::case_when(GR == 1 | EZ == 1 ~ 1,
                                            TRUE ~ 0))

nest_share_GREZ <- nest_share_crises %>%
  dplyr::filter(Crises == 1)
nest_share_no_GREZ <- nest_share_crises %>%
  dplyr::filter(Crises == 0)

# T test (parametric)
mean_test_share_GREZ <- t.test(nest_share_GREZ$eig_vec_1,
                             nest_share_no_GREZ$eig_vec_1,
                             alternative = "greater")

# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_share_GREZ_wilcox <- wilcox.test(nest_share_GREZ$eig_vec_1,
                                           nest_share_no_GREZ$eig_vec_1,
                                           alternative = "greater")
# KS test
mean_test_share_GREZ_ks <- ks.test(nest_share_GREZ$eig_vec_1,
                                   nest_share_no_GREZ$eig_vec_1,
                                   alternative = "less")

