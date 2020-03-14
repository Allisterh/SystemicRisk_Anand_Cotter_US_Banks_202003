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

# Boxplot of missing values for SRE (out of 107) #
# boxplot(SRE_summ_df$`SRE_%_Missing`)

# Summary of banks with most numerous observations 
SRE_summ_bank_most <- SRE_summ_bank_df %>%
  dplyr::filter(`SRE_%_Missing` < 40) %>%
  dplyr::arrange(`SRE_%_Missing`)

name_banks_US_most <- dplyr::select(SRE_summ_bank_most, Banks)

# Write out as .csv file #
# readr::write_csv(SRE_summ_bank_most, 'Syst_Risk_Expos_summary_most.csv')
# readr::write_csv(SRE_summ_df, 'Syst_Risk_Expos_summary_full.csv')


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

# Write out as .csv file #
# readr::write_csv(SRE_summ_df, 'Syst_Risk_Expos_summary_full.csv')


############### Proportion of variance explained by eigenvectors #####################

### Figure 1 ###

# Plot the top eigenvector's share
# plot(eig_share_top, type = 'l', xlab = 'Quarters', ylab = 'Top eigenvector share')
# grid()

################# Median US bank's results ##########################

# Median bank's quarterly behavior
qtr_grid <- SRE_summ_quarterly_df$Quarters
x_breaks <- seq(qtr_grid[1], qtr_grid[105], by = 8)
x_labels <- paste0(seq(1993, 2019, by = 2), "Q4")

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


# Median US bank with most numerous data [core]

SRE_US_quarterly_most_wide <- SRE_US_banks_wide %>%
  dplyr::select(dplyr::intersect(name_banks_US_most$Banks, names(SRE_US_banks_wide)))

# Summary for each quarter separately
SRE_summ_quarterly_most_list <- apply(SRE_US_quarterly_most_wide[, -1], 1, func_summ_vec)
SRE_summ_quarterly_most_df <- dplyr::bind_rows(SRE_summ_quarterly_most_list) %>%
  tibble::add_column('Quarters' = nest_quarter_pc_regression$Q_num) %>%
  dplyr::select(Quarters, everything()) 

# 
# 
# 
# plot_trend_median_most <- ggplot(SRE_summ_quarterly_df, aes(Quarters, Med)) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(method = "lm", 
#               linetype = "dashed", 
#               color = "black") +
#   scale_x_continuous(breaks = x_breaks,
#                      labels = x_labels) +
#   labs(x = "", y = 'Systematic risk exposure') +
#   theme_bw() +
#   theme(text = element_text(size = 20)) +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

###################################
###### SRE Boxplots ###############
###################################

# Yearly
year_seq <- qtr_grid[seq(1, 105, 4)]

data_boxplot <- SRE_US_banks_long %>%
  dplyr::filter(Q_num %in% year_seq)

# boxplot_SRE_yearly <- ggplot(data = data_boxplot %>% dplyr::group_by(Q_num),
#                              mapping = aes(x = Q_num, y = SRE_2)) +
#   stat_boxplot(geom ='errorbar', width = 0.3) + 
#   # geom_boxplot(na.rm = T, outlier.shape = NA) +
#   geom_boxplot(na.rm = T) +
#   theme_bw() + 
#   labs(x = NULL) +
#   theme(axis.text.x=element_text(angle=60, hjust=1)) +
#   theme(text = element_text(size = 20))
