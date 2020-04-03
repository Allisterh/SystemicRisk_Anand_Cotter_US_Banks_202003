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


################# Median US bank's results ##########################

###################################################
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
  scale_x_continuous(breaks = x_breaks,
                     labels = x_labels) +
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

bank_systemic <- c(banks_gsib, banks_dsib)