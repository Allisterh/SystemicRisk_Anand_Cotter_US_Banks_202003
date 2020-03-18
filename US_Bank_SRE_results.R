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
source(name_script_file, echo = F) #Compute diversification using original daily index data

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
  labs(x = "", y = 'Systematic risk exposure') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 



###################################
###### SRE Boxplots ###############
###################################

# Yearly
year_seq <- qtr_grid[seq(1, 105, 4)]

data_boxplot <- SRE_US_banks_long %>%
  dplyr::filter(Q_num %in% year_seq)
