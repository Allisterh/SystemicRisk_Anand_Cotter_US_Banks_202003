################################################################################
########### US Bank systematic risk exposure: results and analysis #############
################################################################################

### Load libraries ###

library(tidyverse)
library(moments)
library(lmtest)
library(sandwich)
library(plm)

### Source prior script for computing systematic risk exposures ### 
name_script_file <- "US_Bank_SRE.R"
source(name_script_file, echo = F) #Compute systematic risk exposure using daily price

######################################################
### Descriptive SRE statistics #######################
######################################################

#########################
### Bank-wise summary ###
#########################

func_summ_vec <- function(vec)
{
  min <- min(vec, na.rm = T)
  max <- max(vec, na.rm = T)
  med <- median(vec, na.rm = T)
  mean <- mean(vec, na.rm = T)
  std <- sd(vec, na.rm = T)
  iqr <- IQR(vec, na.rm = T)
  skew <- moments::skewness(vec, na.rm = T)
  kurt <- moments::kurtosis(vec, na.rm = T)
  frac_NA <- sum(is.na(vec))/length(vec)
  
  summ_vec <- return(tibble::tibble('Min' = min, 
                                    'Max' = max, 
                                    'Med' = med, 
                                    'Mean' = mean, 
                                    'Std_Dev' = std,
                                    'Skew' = skew,
                                    'Kurt' = kurt,
                                    'IQR' = iqr,
                                    'SRE_%_Missing' = frac_NA))
  
  return(summ_vec)
}

# Summary for each bank separately
SRE_summ_bank_list <- apply(SRE_US_banks_wide[, -1], 2, func_summ_vec)
SRE_summ_bank_df <- dplyr::bind_rows(SRE_summ_bank_list) %>%
  tibble::add_column('Banks' = names(SRE_summ_bank_list)) %>%
  dplyr::select(Banks, everything())


# Summary of the whole US banking sector: Summary of summary of banks #
SRE_summ_US_list <- apply(SRE_summ_bank_df[, -1], 2, func_summ_vec)
SRE_summ_US_df <- dplyr::bind_rows(SRE_summ_US_list) %>%
  tibble::add_column('Parameters' = names(SRE_summ_US_list)) %>%
  dplyr::select(Parameters, everything())

#########################
### Quarterly summary ###
#########################

# Summary for each quarter separately
SRE_summ_quarterly_list <- apply(SRE_US_banks_wide[, -1], 1, func_summ_vec)
SRE_summ_quarterly_df <- dplyr::bind_rows(SRE_summ_quarterly_list) %>%
  tibble::add_column('Quarters' = nest_quarter_pc_regression$Q_num) %>%
  dplyr::select(Quarters, everything()) 


###################################################
#### Explanatory share of top eigenvectors ########
############ Figure 1 #############################
###################################################

nest_share <- nest_quarter_banks_US %>%
  dplyr::select(Q_num, share) %>%
  dplyr::mutate('top_eig_vec' = purrr::map(share, function(vec){vec[1:10]})) %>%
  dplyr::mutate('eig_vec_1' = purrr::map_dbl(top_eig_vec, function(vec){vec[1]}))

share_long <- nest_share %>%
  dplyr::select(Q_num, top_eig_vec) %>%
  tidyr::unnest(., cols = top_eig_vec)

### Boxplots of top 10 eigenvector explanatory power ###

x_breaks_share <- seq(1, 108, by = 4)
x_labels_share <- paste0(seq(1993, 2019), "Q1")


plot_box_share <- ggplot(data = share_long %>% 
                           dplyr::filter(Q_num %in% x_breaks_share),
                         mapping = aes(x = Q_num, y = top_eig_vec, group = Q_num)) +
  geom_boxplot() +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Explanatory power of top 10 eigenvectors") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


### Topmost eigenvector's explanatory power share ###

plot_share_eig_vec_1 <- ggplot(data = nest_share, 
                               mapping = aes(x = Q_num, y = eig_vec_1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Explanatory power of the 1st (topmost) eigenvector") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

###################################################
######### Trend plot for median bank ##############
############ Figure 2 #############################
###################################################

# Median bank's quarterly behavior
qtr_grid <- SRE_summ_quarterly_df$Quarters
x_breaks <- seq(qtr_grid[1], qtr_grid[105], by = 8)
x_labels <- paste0(seq(1993, 2019, by = 2), "Q1")

# All quarters
plot_trend_median <- ggplot(SRE_summ_quarterly_df,
                            aes(Quarters, Med)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Median US bank's systematic risk exposure") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


###################################
###### SRE Boxplots ###############
####### Figure 3 ##################
###################################

# Yearly

x_breaks_y <- seq(qtr_grid[1], qtr_grid[105], by = 4)
x_labels_y <- paste0(seq(1993, 2019), "Q1")

data_boxplot <- SRE_US_banks_long %>%
  dplyr::filter(Q_num %in% x_breaks_y)

plot_box_yearly <- ggplot(data = data_boxplot, 
                          mapping = aes(x = Q_num, y = SRE_2, group = Q_num)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_continuous(breaks = x_breaks_y,
                     labels = x_labels_y) +
  # scale_x_continuous(breaks = x_breaks_share,
  #                    labels = x_labels_share) +
  labs(x = "", y = "US banks' systematic risk exposure") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


###############################################################
################### GSIB+DSIB US Banks ########################
###############################################################

name_banks_US <- name_banks_US %>%
  dplyr::arrange(comnam)

### GSIBs ###
# Banks have different names
bank_jpm <- c("J P MORGAN CHASE & CO", 
              "JPMORGAN CHASE & CO", 
              "MORGAN J P & CO INC")
bank_citi <- c("CITICORP","CITIGROUP INC")
bank_bofa <- c("BANK OF AMERICA CORP",
               "BANKAMERICA CORP",
               "BANKAMERICA CORP NEW")
bank_goldman <- c("GOLDMAN SACHS GROUP INC")
# bank_wells_fargo <- c("WELLS FARGO & CO NEW", 
#                       "NORWEST CORP", 
#                       "WACHOVIA CORP NEW",
#                       "WACHOVIA CORP 2ND NEW")
bank_wells_fargo <- c("WELLS FARGO & CO NEW")
bank_bny <- c("BANK OF NEW YORK MELLON CORP",
              "MELLON BANK CORP", 
              "MELLON FINANCIAL CORP")
bank_morgan_stanley <- c("MORGAN STANLEY DEAN WITTER & CO")
bank_state_street <- c("STATE STREET CORP",
                       "STATE STREET BOSTON CORP")

banks_gsib <- c(bank_jpm, bank_citi, bank_bofa,
                bank_goldman, bank_wells_fargo,
                bank_bny, bank_morgan_stanley,
                bank_state_street)

### DSIBs ###

bank_ally <- "ALLY FINANCIAL INC"
bank_amex <- "AMERICAN EXPRESS CO"
bank_truist <- "TRUIST FINANCIAL CORP"
bank_capital_one <- "CAPITAL ONE FINANCIAL CORP"
bank_comerica <- "COMERICA INC"
bank_discover <- "DISCOVER FINANCIAL SERVICES"
bank_fifth_third <- "FIFTH THIRD BANCORP"
bank_huntington <- "HUNTINGTON BANCSHARES INC"
bank_keycorp <- "KEYCORP NEW"
bank_mt <- "M & T BANK CORP"
bank_north_trust <- "NORTHERN TRUST CORP"
bank_pnc <- c("P N C BANK CORP",
              "P N C FINANCIAL CORP",
              "P N C FINANCIAL SERVICES GRP INC")
bank_regions <- c("REGIONS FINANCIAL CORP",
                  "REGIONS FINANCIAL CORP NEW")
bank_suntrust <- "SUNTRUST BANKS INC"
bank_us_bancorp <- "UNITED STATES BANCORP"
bank_union_bankcal <- "UNION BANK SAN FRANCISCO CA"
bank_zion <- c("ZIONS BANCORPORATION",
               "ZIONS BANCORPORATION N A")

banks_dsib <- c(bank_ally, bank_amex, bank_truist,
                bank_capital_one, bank_comerica,
                bank_discover, bank_fifth_third, 
                bank_huntington, bank_keycorp,
                bank_mt, bank_north_trust, bank_pnc,
                bank_regions, bank_suntrust, 
                bank_us_bancorp, bank_union_bankcal,
                bank_zion)

banks_systemic <- c(banks_gsib, banks_dsib)

SRE_US_systemic <- SRE_US_banks_long %>%
  dplyr::filter(Bank %in% banks_systemic) %>%
  dplyr::mutate("Type" = dplyr::case_when(Bank %in% banks_gsib ~ "GSIB",
                                          Bank %in% banks_dsib ~ "DSIB"))

median_quarter_full <- SRE_US_banks_long %>%
  dplyr::group_by(Q_num) %>%
  dplyr::summarise('med_full' = median(SRE_2, na.rm = T))
median_quarter_systemic <- SRE_US_systemic %>%
  dplyr::group_by(Q_num) %>%
  dplyr::summarise('med_sys' = median(SRE_2, na.rm = T))
median_quarter_GSIB <- SRE_US_systemic %>%
  dplyr::filter(Type == 'GSIB') %>%
  dplyr::group_by(Q_num) %>%
  dplyr::summarise('med_gsib' = median(SRE_2, na.rm = T))
median_quarter_DSIB <- SRE_US_systemic %>%
  dplyr::filter(Type == 'DSIB') %>%
  dplyr::group_by(Q_num) %>%
  dplyr::summarise('med_dsib' = median(SRE_2, na.rm = T))

median_quarter <- median_quarter_full %>%
  dplyr::full_join(., median_quarter_systemic, by = 'Q_num') %>%
  dplyr::full_join(., median_quarter_GSIB, by = 'Q_num') %>%
  dplyr::full_join(., median_quarter_DSIB, by = 'Q_num') 

median_quarter_long <- median_quarter %>%
  tidyr::gather(c(med_full, med_sys, med_gsib, med_dsib),
                key = "Medians", value = "SRE")

##############################################################
################## Figure 4 ##################################
##############################################################

df_rect_crises <- data.frame(x_1 = c(60, 70),
                             x_2 = c(66, 78),
                             y_1 = c(0, 0),
                             y_2 = c(1, 1))

data_plot_med_sys <- median_quarter_long %>%
  dplyr::filter(Medians %in% c('med_full', 'med_sys'))

plot_med_systemic <- ggplot(data_plot_med_sys,
                            aes(x = Q_num, y = SRE)) +
  geom_point() +
  geom_line(mapping = aes(linetype = Medians)) +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Median systemic and median US bank's SRE") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

### With crises shaded in grey ###
plot_med_systemic_crises <- ggplot() +
  geom_rect(data = df_rect_crises,
            mapping = aes(xmin = x_1, 
                          xmax = x_2, 
                          ymin = y_1, 
                          ymax = y_2),
            color = "grey",
            alpha = 0.1) +
  geom_point(data = data_plot_med_sys,
            mapping = aes(x = Q_num, y = SRE)) +
  geom_line(data = data_plot_med_sys, 
            mapping = aes(x = Q_num, y = SRE, linetype = Medians)) +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Median systemic and median US bank's SRE") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#######################################################################
############## First and second sample halves #########################
#######################################################################

median_quarter <- median_quarter %>% 
  dplyr::mutate('Period' = dplyr::case_when(Q_num < max(Q_num)/2 ~ 'H1', 
                                            Q_num >= max(Q_num)/2 ~ 'H2'))

plot_med_H1H2 <- ggplot(data = median_quarter, 
                        mapping = aes(x = Q_num, y = med_full)) +
  geom_point() +
  geom_line() +
  geom_smooth(mapping = aes(group = Period),
              method = 'lm',
              linetype = 'dotdash',
              color = 'black') +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Median US bank's SRE") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
