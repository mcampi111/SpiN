library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(reshape2)
library(viridis)
library(ggpubr)
library(demography)
library(StMoMo)
library(fds)
library(tidyr)
library(viridis)
library(transport)
library(viridis)
library(gridExtra)
library(kableExtra)
library(lmtest)
library(tseries)

###############################
# SET DIRECTORY AND READ FILE #
###############################

source("/Users/mcampi/Desktop/Hung_Joanna/code/utils.R")

mydir <- "/Users/mcampi/Desktop/Hung_Joanna/data/"
mydir2 <- "/Users/mcampi/Desktop/Hung_Joanna/code/"
mydir_figs <- "/Users/mcampi/Desktop/Hung_Joanna/code/figs/"

result <- prepare_data(mydir, mydir2, mydir_figs)


list2env(result, envir = .GlobalEnv)

###########################################
# CHOOSE AGE STEP TO CONDUCT THE ANALYSIS #
###########################################
#Select both of them below i.e; age_var and age_group_var

age_var = "age_group2" #"age_group" #"age_group2"   "age_group3"
age_group_var = age_groups2 #all_age_groups #age_groups2  age_groups
n_age = length(age_group_var)


#########################################
# DATA Prep.& Hearing Rates Construction#
#########################################

hr_counts = process_data(data_lee_carter, 
                         freq_tresh, 
                         age_group_var = age_var,
                         n_tresh, n_age,
                         n_hlclasses,
                         process_func = process_matrix_countsonly)


totals_count = process_data(data_lee_carter, 
                            freq_tresh, 
                            age_group_var = age_var,
                            n_tresh, n_age,
                            n_hlclasses,
                            process_func = process_matrix_totals)


hr_rates <- process_data(data_lee_carter, 
                         freq_tresh, 
                         age_group_var = age_var,
                         n_tresh, n_age,
                         n_hlclasses,
                         process_func = process_matrix2)

hr_rates_interp <- recursive_interpolation(hr_rates)


spiq_rates = process_data(spiq, 
                          treshold_SPIQ, 
                          age_group_var = age_var,
                          n_tresh, n_age,
                          n_hlclasses)

spin_rates = process_data(spin, 
                          treshold_SPIN, 
                          age_group_var = age_var,
                          n_tresh, n_age,
                          n_hlclasses)


#######################
#######################
# STEP 1: CPBMT MODEL #
#######################
#######################

######################
# CPBMT model by age #
######################

#MODEL ON THE Proportions
demodata_byhl = lapply(1:n_tresh, function(i)
  demogdata(data = hr_rates_interp[[1]][[i]],
            pop = matrix(rep(totals_count[[1]][[i]], each = 11), ncol = 11, byrow = TRUE),
            ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),
            years = as.numeric(freq_vector),
            type = 'mortality',
            label = 'hearing',
            name = 'total') )

model_hl = lapply(1:n_tresh, function(i)
  lca(data = demodata_byhl[[i]], restype = "rates"))



#########################################
# CPBMT model by degree of hearing loss # 
#########################################


#MODEL ON THE Proportions
demodata_by_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    demogdata(data = hr_rates_interp[[2]][[i]][[j]],
              pop = matrix(rep(totals_count[[2]][[i]][[j]], each = 11), ncol = 11, byrow = TRUE),
              ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),
              years = as.numeric(freq_vector),
              type = 'mortality',
              label = 'hearing',
              name = 'total') ) )


model_by_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    lca(data = demodata_by_age_hl[[i]][[j]], restype = "rates",
        interpolate = TRUE)) )


###################
# CPBMT MODEL Sex #
###################

demodata_by_age_sex = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j)
    demogdata(data = hr_rates_interp[[3]][[i]][[j]],
              pop = matrix(rep(totals_count[[3]][[i]][[j]], each = 11), ncol = 11, byrow = TRUE),
              ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),
              years = as.numeric(freq_vector),
              type = 'mortality',
              label = 'hearing',
              name = 'total') ) )



model_by_age_sex = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j)
    lca(data = demodata_by_age_sex[[i]][[j]] , restype = "rates",
        interpolate = TRUE)) )


###########################################################################
###########################################################################
#   Statistical Tests for Differences between reference population and HL #
###########################################################################
###########################################################################

set.seed(0)

#######################
# CPBMT model OVERALL #
#######################

ar1_hl_model = lapply(1:n_tresh, function(i)
  arima(as.numeric(model_hl[[i]]$kt), order = c(1, 0, 0)) )

loglike_base_hl <- lapply(1:n_tresh, function(i) {
  Y <- model_hl[[i]]$total
  alpha_a <- model_hl[[i]]$ax
  beta_a <- model_hl[[i]]$bx
  theta_f <- coef(ar1_hl_model[[i]])[[2]]
  phi_1 <- coef(ar1_hl_model[[i]])[[1]]
  sigma2_epsilon <- apply(model_hl[[i]]$residuals$y, 2, var) #0.1 #Discuss --> sigma_epsilon
  sigma2_omega_f <- var(ar1_hl_model[[i]]$residuals) #0.1 #Discuss --> sigma_omega
  kappa_0 <- as.numeric(model_hl[[i]]$kt) #modify the function cause you have k and you change sigma and P
  P_0 <- 1.0 #Discuss
  
  result <- baseline_kalman_filter_log_likelihood(Y, alpha_a, beta_a, 
                                                  theta_f, phi_1, sigma2_epsilon,
                                                  sigma2_omega_f, 
                                                  my_kappa = kappa_0, 
                                                  P_0)
  return(result)
})




loglike_extended_hl <- lapply(1:n_tresh, function(i) {
  Y <- model_hl[[i]]$total
  alpha_a <- model_hl[[i]]$ax
  beta_a <- model_hl[[i]]$bx
  theta_f <- coef(ar1_hl_model[[i]])[[2]]
  phi_1 <- coef(ar1_hl_model[[i]])[[1]]
  sigma2_epsilon <- apply(model_hl[[i]]$residuals$y, 2, var) #0.1 #Discuss --> sigma_epsilon
  sigma2_omega_f <- var(ar1_hl_model[[i]]$residuals) #0.1 #Discuss --> sigma_omega
  kappa_0 <- as.numeric(model_hl[[i]]$kt) #modify the function cause you have k and you change sigma and P
  P_0 <- 1.0 #Discuss
  
  gamma_v = coef(partial_step_FWL_hl[[i]])[2,]
  gamma_w = NULL#coef(partial_step_FWL_hl[[i]])[3,]
  v = spiq_rates[[1]][[i]]
  w = NULL#spin_rates[[1]][[i]]
  
  
  result <- extended_kalman_filter_log_likelihood(Y, 
                                                  alpha_a,
                                                  beta_a, 
                                                  theta_f,
                                                  phi_1,
                                                  sigma2_epsilon, 
                                                  sigma2_omega_f, 
                                                  gamma_v, v,
                                                  gamma_w, w,
                                                  my_kappa = kappa_0,#kappa_0, 
                                                  #v_0, w_0, 
                                                  P_0)
  return(result)
})




#####################
# CPBMT model by HL # 
#####################


ar1_age_hl_model = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j) 
    arima(as.numeric(model_by_age_hl[[i]][[j]]$kt), order = c(1, 0, 0)) ))

loglike_base_age_hl <- lapply(1:n_tresh, function(i) {
  lapply(1:n_hlclasses, function(j) {
    Y <- model_by_age_hl[[i]][[j]]$total
    alpha_a <- model_by_age_hl[[i]][[j]]$ax
    beta_a <- model_by_age_hl[[i]][[j]]$bx
    theta_f <- coef(ar1_age_hl_model[[i]][[j]])[[2]]
    phi_1 <- coef(ar1_age_hl_model[[i]][[j]])[[1]]
    sigma2_epsilon <- apply(model_by_age_hl[[i]][[j]]$residuals$y, 2, var) #0.1 #Discuss --> sigma_epsilon
    sigma2_omega_f <- var(ar1_age_hl_model[[i]][[j]]$residuals) #0.1 #Discuss --> sigma_omega
    kappa_0 <- as.numeric(model_by_age_hl[[i]][[j]]$kt) #modify the function cause you have k and you change sigma and P
    P_0 <- 1.0 #Discuss
    
    result <- baseline_kalman_filter_log_likelihood(Y, alpha_a, beta_a, 
                                                    theta_f, phi_1, sigma2_epsilon,
                                                    sigma2_omega_f, 
                                                    my_kappa = kappa_0, 
                                                    P_0)
    return(result)
  })
})




loglike_extended_age_hl <- lapply(1:n_tresh, function(i) {
  lapply(1:n_hlclasses, function(j) {
    Y <- model_by_age_hl[[i]][[j]]$total
    alpha_a <- model_by_age_hl[[i]][[j]]$ax
    beta_a <- model_by_age_hl[[i]][[j]]$bx
    theta_f <- coef(ar1_age_hl_model[[i]][[j]])[[2]]
    phi_1 <- coef(ar1_age_hl_model[[i]][[j]])[[1]]
    sigma2_epsilon <- apply(model_by_age_hl[[i]][[j]]$residuals$y, 2, var) #0.1 #Discuss --> sigma_epsilon
    sigma2_omega_f <- var(ar1_age_hl_model[[i]][[j]]$residuals) #0.1 #Discuss --> sigma_omega
    kappa_0 <- as.numeric(model_by_age_hl[[i]][[j]]$kt) #modify the function cause you have k and you change sigma and P
    P_0 <- 1.0 #Discuss
    
    gamma_v = coef(partial_step_FWL_age_hl[[i]][[j]])[2,]
    gamma_w = coef(partial_step_FWL_age_hl[[i]][[j]])[3,] #NULL
    v = spiq_rates[[2]][[i]][[j]]
    w = spin_rates[[2]][[i]][[j]] #NULL
    
    
    result <- extended_kalman_filter_log_likelihood(Y, 
                                                    alpha_a,
                                                    beta_a, 
                                                    theta_f,
                                                    phi_1,
                                                    sigma2_epsilon, 
                                                    sigma2_omega_f, 
                                                    gamma_v, v,
                                                    gamma_w, w,
                                                    my_kappa = kappa_0,#kappa_0, 
                                                    #v_0, w_0, 
                                                    P_0)
    return(result)
  })
})



######################
# CPBMT model by SEX # 
######################

ar1_sex_model = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j) 
    arima(as.numeric(model_by_age_sex[[i]][[j]]$kt), order = c(1, 0, 0)) ))

loglike_base_sex <- lapply(1:n_tresh, function(i) {
  lapply(1:2, function(j) {
    Y <- model_by_age_sex[[i]][[j]]$total
    alpha_a <- model_by_age_sex[[i]][[j]]$ax
    beta_a <- model_by_age_sex[[i]][[j]]$bx
    theta_f <- coef(ar1_sex_model[[i]][[j]])[[2]]
    phi_1 <- coef(ar1_sex_model[[i]][[j]])[[1]]
    sigma2_epsilon <- apply(model_by_age_sex[[i]][[j]]$residuals$y, 2, var) #0.1 #Discuss --> sigma_epsilon
    sigma2_omega_f <- var(ar1_sex_model[[i]][[j]]$residuals) #0.1 #Discuss --> sigma_omega
    kappa_0 <- as.numeric(model_by_age_sex[[i]][[j]]$kt) #modify the function cause you have k and you change sigma and P
    P_0 <- 1.0 #Discuss
    
    result <- baseline_kalman_filter_log_likelihood(Y, alpha_a, beta_a, 
                                                    theta_f, phi_1, sigma2_epsilon,
                                                    sigma2_omega_f, 
                                                    my_kappa = kappa_0, 
                                                    P_0)
    return(result)
  })
})




loglike_extended_sex <- lapply(1:n_tresh, function(i) {
  lapply(1:2, function(j) {
    Y <- model_by_age_sex[[i]][[j]]$total
    alpha_a <- model_by_age_sex[[i]][[j]]$ax
    beta_a <- model_by_age_sex[[i]][[j]]$bx
    theta_f <- coef(ar1_sex_model[[i]][[j]])[[2]]
    phi_1 <- coef(ar1_sex_model[[i]][[j]])[[1]]
    sigma2_epsilon <- apply(model_by_age_sex[[i]][[j]]$residuals$y, 2, var) #0.1 #Discuss --> sigma_epsilon
    sigma2_omega_f <- var(ar1_sex_model[[i]][[j]]$residuals) #0.1 #Discuss --> sigma_omega
    kappa_0 <- as.numeric(model_by_age_sex[[i]][[j]]$kt) #modify the function cause you have k and you change sigma and P
    P_0 <- 1.0 #Discuss
    
    gamma_v = coef(partial_step_FWL_age_sex[[i]][[j]])[2,]
    gamma_w = coef(partial_step_FWL_age_sex[[i]][[j]])[3,] #NULL
    v = spiq_rates[[3]][[i]][[j]]
    w = spin_rates[[3]][[i]][[j]] #NULL
    
    
    result <- extended_kalman_filter_log_likelihood(Y, 
                                                    alpha_a,
                                                    beta_a, 
                                                    theta_f,
                                                    phi_1,
                                                    sigma2_epsilon, 
                                                    sigma2_omega_f, 
                                                    gamma_v, v,
                                                    gamma_w, w,
                                                    my_kappa = kappa_0,#kappa_0, 
                                                    #v_0, w_0, 
                                                    P_0)
    return(result)
  })
})



##################################################
##################################################
################# Contrast tests #################
##################################################
##################################################

#########
#EXTENDED vs BASELINE CONTRASTs for overall and by Category of Hearing Loss and by Sex
#########

LR_hl <- lapply(1:n_tresh, function(i)
  2 * (loglike_extended_hl[[i]]$log_likelihood - loglike_base_hl[[i]]$log_likelihood) )

LR_age_hl <- lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    2 * (loglike_extended_age_hl[[i]][[j]]$log_likelihood - loglike_base_age_hl[[i]][[j]]$log_likelihood) ))

LR_age_sex <- lapply(1:n_tresh, function(i)
  lapply(1:2, function(j)
    2 * (loglike_extended_sex[[i]][[j]]$log_likelihood - loglike_base_sex[[i]][[j]]$log_likelihood) ))


# Calculate degrees of freedom
df <- 22

# Calculate p-value
lrt_hl_p_values <- lapply(1:n_tresh, function(i)
  pchisq(LR_hl[[i]], df, lower.tail = FALSE))


lrt_age_hl_p_values <- lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    pchisq(LR_age_hl[[i]][[j]], df, lower.tail = FALSE)))

lrt_sex_p_values <- lapply(1:n_tresh, function(i)
  lapply(1:2, function(j)
    pchisq(LR_age_sex[[i]][[j]], df, lower.tail = FALSE)))


lrt_age_hl_p_values2 = lapply(1:n_tresh, function(i) do.call(rbind, lrt_age_hl_p_values[[i]] ))
lrt_age_hl_p_values2 = lapply(1:n_tresh, function(i) data.frame( HL = hl_classes,
                                                                 Quantile = freq_tresh_plot[i],
                                                                 p = lrt_age_hl_p_values2[[i]]))

lrt_age_hl_p_values2 = do.call(rbind, lrt_age_hl_p_values2 )
lrt_age_hl_p_values2$HL =  factor(lrt_age_hl_p_values2$HL, levels = hl_classes)

ggplot(lrt_age_hl_p_values2, aes(x = HL, y = p,  color = Quantile )) +
  geom_boxplot(alpha = 0.5,color = "black")  +
  geom_point(size = 2,
             aes(color =  as.factor(Quantile) ))  +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = paste("Age"), 
       y = "P-value LRT Test", 
       color = "Quantile") +
  theme_bw()  +
  scale_color_manual(values = thresh_colors) +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom") 



lrt_sex_p_values2 = lapply(1:n_tresh, function(i) do.call(rbind, lrt_sex_p_values[[i]] ))
lrt_sex_p_values2 = lapply(1:n_tresh, function(i) data.frame( Sex = c("Female", "Male"),
                                                              Quantile = freq_tresh_plot[i],
                                                              p = lrt_sex_p_values2[[i]]))

lrt_sex_p_values2 = do.call(rbind, lrt_sex_p_values2 )
lrt_sex_p_values2$Sex =  factor(c("Female", "Male"), levels = c("Female", "Male"))

ggplot(lrt_sex_p_values2, aes(x = Sex, y = p,  color = Quantile )) +
  geom_boxplot(alpha = 0.5,color = "black")  +
  geom_point(size = 2,
             aes(color =  as.factor(Quantile) ))  +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = paste("Age"), 
       y = "P-value LRT Test", 
       color = "Quantile") +
  theme_bw()  +
  scale_color_manual(values = thresh_colors) +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom") 



##################
#Overall vs HL models for both BASELINE & EXTENDED CONTRASTs for fixed quantile levels
##################

overall_hlage_baseline = do.call(rbind, lapply(1:n_tresh, function(j)
  sapply(1:n_hlclasses, function(i)
    round(vuong_test_with_loglike(loglike_base_hl[[j]]$log_like_vector,
                                  loglike_base_age_hl[[j]][[i]]$log_like_vector)$vuong_pvalue,3)) ))


overall_hlage_extended = do.call(rbind, lapply(1:n_tresh, function(j)
  sapply(1:n_hlclasses, function(i)
    round(vuong_test_with_loglike(loglike_extended_hl[[j]]$log_like_vector,
                                  loglike_extended_age_hl[[j]][[i]]$log_like_vector)$vuong_pvalue,3)) ))


colnames(overall_hlage_baseline) = hl_classes
overall_hlage_baseline = as.data.frame(overall_hlage_baseline)
overall_hlage_baseline


kable(overall_hlage_baseline, format = 'latex', linesep = "")

colnames(overall_hlage_extended) = hl_classes
overall_hlage_extended = as.data.frame(overall_hlage_extended)
overall_hlage_extended


kable(overall_hlage_extended, format = 'latex', linesep = "")



# Add a column to identify the source
overall_hlage_baseline$Model <- "Baseline"
overall_hlage_extended$Model <- "Extended"

overall_hlage_baseline$Prob_level <- factor(freq_tresh_plot, levels = freq_tresh_plot)
overall_hlage_extended$Prob_level <-  factor(freq_tresh_plot, levels = freq_tresh_plot)


# Combine the data frames
combined_df <- rbind(overall_hlage_baseline, overall_hlage_extended)

# Reshape the data to long format
long_df <- pivot_longer(combined_df, cols = c("Slight", "Mild", "Moderate",
                                              "Moderately severe", "Severe"),
                        names_to = "HL", 
                        values_to = "p_value")

# Create the box plots
ggbox0 = ggplot(long_df, aes(x = Prob_level, y = p_value, fill = Model)) +
  geom_boxplot(aes(fill = Model), color = "blue") +
  geom_point(aes(color = HL), size = 3,
             position = position_jitter(width = 0.2)) +
  labs(title = "",
       x = "Quantile",
       y = "P-value",
       fill = "Model") +
  theme_minimal() +  # Use jitter for better visualization
  facet_grid(~ Model, scales = "free_x") +
  labs(x = "Quantile", y = "P-value", 
       color = "Hearing Loss", 
       fill = "Hearing Loss",
       title = "") +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(color = NA),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.9),  # Add grid around the plots
    axis.text.x = element_text(size = 13, angle = 45, vjust = 0.5),  # Adjust vjust to move labels closer to the axis
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 15),  # Make facet grid text bigger
    legend.box = "horizontal", 
    legend.box.just = "top"
  ) +
  guides(fill = "none") +  # Remove fill legend for boxplot
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red")  # Add dashed line at y = 0.05

ggbox0

ggsave(filename = paste0(mydir_figs,"box_overall_hl_base_extended", ".pdf"), 
       plot = ggbox0,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)



#BY SEX
overall_sexage_baseline = do.call(rbind, lapply(1:n_tresh, function(j)
  sapply(1:2, function(i)
    round(vuong_test_with_loglike(loglike_base_hl[[j]]$log_like_vector,
                                  loglike_base_sex[[j]][[i]]$log_like_vector)$vuong_pvalue,3)) ))


overall_sexage_extended = do.call(rbind, lapply(1:n_tresh, function(j)
  sapply(1:2, function(i)
    round(vuong_test_with_loglike(loglike_extended_hl[[j]]$log_like_vector,
                                  loglike_extended_sex[[j]][[i]]$log_like_vector)$vuong_pvalue,3)) ))


colnames(overall_sexage_baseline) = c("Female", "Male")
overall_sexage_baseline = as.data.frame(overall_sexage_baseline)
overall_sexage_baseline


kable(overall_sexage_baseline, format = 'latex', linesep = "")

colnames(overall_sexage_extended) = c("Female", "Male")
overall_sexage_extended = as.data.frame(overall_sexage_extended)
overall_sexage_extended

kable(overall_sexage_extended, format = 'latex', linesep = "")



# Add a column to identify the source
overall_sexage_baseline$Model <- "Baseline"
overall_sexage_extended$Model <- "Extended"

overall_sexage_baseline$Prob_level <- factor(freq_tresh_plot, levels = freq_tresh_plot)
overall_sexage_extended$Prob_level <-  factor(freq_tresh_plot, levels = freq_tresh_plot)


# Combine the data frames
combined_df_sex <- rbind(overall_sexage_baseline, overall_sexage_extended)

# Reshape the data to long format
long_df_sex <- pivot_longer(combined_df_sex, cols = c("Female", "Male"),
                            names_to = "Sex", 
                            values_to = "p_value")

# Create the box plots
ggbox1 = ggplot(long_df_sex, aes(x = Prob_level, y = p_value, fill = Model)) +
  geom_boxplot(aes(fill = Model), color = "blue") +
  geom_point(aes(color = Sex), size = 3,
             position = position_jitter(width = 0.2)) +
  labs(title = "",
       x = "Quantile",
       y = "P-value",
       fill = "Model") +
  theme_minimal() +  # Use jitter for better visualization
  facet_grid(~ Model, scales = "free_x") +
  labs(x = "Quantile", y = "P-value", 
       color = "Hearing Loss", 
       fill = "Hearing Loss",
       title = "") +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(color = NA),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.9),  # Add grid around the plots
    axis.text.x = element_text(size = 13, angle = 45, vjust = 0.5),  # Adjust vjust to move labels closer to the axis
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 15),  # Make facet grid text bigger
    legend.box = "horizontal", 
    legend.box.just = "top"
  ) +
  guides(fill = "none") +  # Remove fill legend for boxplot
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red")  # Add dashed line at y = 0.05

ggbox1

ggsave(filename = paste0(mydir_figs,"box_overall_sex_base_extended", ".pdf"), 
       plot = ggbox1,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)



#####################
# REFERENCE CONTRASTs i.e. slight vs all, then mild vs all, etc. for BASELINE and then EXTENDED
###################

############################
# Contrast by Hearing loss #
###########################

#Baseline
contrast_base_hl = data.frame( contrast = t(cbind(t(as.vector(rep(paste(hl_classes[1], "vs", 
                                                                        hl_classes[-1]), 5))),
                                                  t(as.vector(rep(paste(hl_classes[-1][1], "vs", 
                                                                        hl_classes[-1][-1]), 5))),
                                                  t(as.vector(rep(paste(hl_classes[-c(1:2)][1], "vs", 
                                                                        hl_classes[-c(1:2)][-1]), 5))),
                                                  t(as.vector(rep(paste(hl_classes[-c(1:3)][1], "vs", 
                                                                        hl_classes[-c(1:3)][-1]), 5))))),
                               
                               quantile = c(rep(quant_tresh, each = 4),
                                            rep(quant_tresh, each = 3),
                                            rep(quant_tresh, each = 2),
                                            rep(quant_tresh, each = 1) ),
                               
                               p_value_base = do.call(rbind,lapply(1:(n_hlclasses-1), function(k) {
                                 t(do.call(cbind, lapply(1:n_tresh, function(i) {
                                   t(as.numeric(sapply((k+1):n_hlclasses, function(j) {
                                     round(vuong_test_with_loglike(loglike_base_age_hl[[i]][[k]]$log_like_vector,
                                                                   loglike_base_age_hl[[i]][[j]]$log_like_vector)$vuong_pvalue,3)
                                   })))
                                 })))
                               }))
                               
)


######################## to prove that the test is actually symmetric  ##################

vuong_results <- list()

# Loop over all pairs of thresholds (i, j)
for (i in 1:(n_tresh - 1)) {
  for (j in (i + 1):n_tresh) {
    # Initialize an empty matrix to store results for the pair (i, j)
    vuong_matrix <- matrix(NA, n_hlclasses, n_hlclasses)
    
    # Loop over all combinations of classes (k, l)
    for (k in 1:n_hlclasses) {
      for (l in 1:n_hlclasses) {
        if (k != l) {
          # Calculate Vuong test for (i, k) vs (j, l)
          vuong_stat <- vuong_test_with_loglike(
            loglike_base_age_hl[[i]][[k]]$log_like_vector,
            loglike_base_age_hl[[j]][[l]]$log_like_vector
          )
          
          # Store p-value in matrix
          vuong_matrix[k, l] <- round(vuong_stat$vuong_pvalue, 3)
          
          # For bidirectional comparison, also calculate (j, l) vs (i, k)
          vuong_matrix[l, k] <- round(vuong_stat$vuong_pvalue, 3)
        }
      }
    }
    
    # Store results for pair (i, j) in vuong_results list
    vuong_results[[paste("Comparison", i, "vs", j)]] <- vuong_matrix
  }
}

print(vuong_results)

#############################################################################


contrast_base_hl$reference_variable <- sapply(contrast_base_hl$contrast, function(x) {
  unlist(strsplit(x, " vs "))[1]
})

contrast_base_hl$contrast_variable <- sapply(contrast_base_hl$contrast, function(x) {
  unlist(strsplit(x, " vs "))[2]
})

contrast_base_hl$reference_variable <- factor(contrast_base_hl$reference_variable, 
                                              levels = c("Slight", "Mild",  "Moderate", "Moderately severe"))
# Convert quantile to factor
contrast_base_hl$quantile <- factor(contrast_base_hl$quantile)


# Plot with facets and colored points
ggbox1 = ggplot(contrast_base_hl, aes(x = contrast_variable,
                                      y = p_value_base)) +
  geom_boxplot(aes(fill = contrast_variable), color = "blue") +  # Set boxplot outline color to blue
  geom_point(aes(color = quantile), size = 3,
             position = position_jitter(width = 0.2)) +  # Use jitter for better visualization
  facet_grid(~ reference_variable, scales = "free_x") +
  labs(x = "Contrast", y = "P-value", 
       color = "Quantile", 
       fill = "Contrast",
       title = "") + #P-value Distribution for Baseline Model
  scale_color_manual(values = setNames(thresh_colors, quant_tresh)) +  # Use manual color scale for points
  scale_fill_manual(values = hearing_loss_col) +  # Use manual color scale for fill
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(color = NA),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.9),  # Add grid around the plots
    axis.text.x = element_text(size = 13, angle = 45, vjust = 0.5),  # Adjust vjust to move labels closer to the axis
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 15),  # Make facet grid text bigger
    legend.box = "horizontal", 
    legend.box.just = "top"
  ) +
  guides(fill = "none") +  # Remove fill legend for boxplot
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red")  # Add dashed line at y = 0.05


ggsave(filename = paste0(mydir_figs,"box_hl_baseline", ".pdf"), 
       plot = ggbox1,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)

#Extended
contrast_extended_hl = data.frame( contrast = t(cbind(t(as.vector(rep(paste(hl_classes[1], "vs", 
                                                                            hl_classes[-1]), 5))),
                                                      t(as.vector(rep(paste(hl_classes[-1][1], "vs", 
                                                                            hl_classes[-1][-1]), 5))),
                                                      t(as.vector(rep(paste(hl_classes[-c(1:2)][1], "vs", 
                                                                            hl_classes[-c(1:2)][-1]), 5))),
                                                      t(as.vector(rep(paste(hl_classes[-c(1:3)][1], "vs", 
                                                                            hl_classes[-c(1:3)][-1]), 5))))),
                                   
                                   quantile = c(rep(quant_tresh, each = 4),
                                                rep(quant_tresh, each = 3),
                                                rep(quant_tresh, each = 2),
                                                rep(quant_tresh, each = 1) ),
                                   
                                   p_value_extended = do.call(rbind,lapply(1:(n_hlclasses-1), function(k) {
                                     t(do.call(cbind, lapply(1:n_tresh, function(i) {
                                       t(as.numeric(sapply((k+1):n_hlclasses, function(j) {
                                         round(vuong_test_with_loglike(loglike_extended_age_hl[[i]][[k]]$log_like_vector,
                                                                       loglike_extended_age_hl[[i]][[j]]$log_like_vector)$vuong_pvalue,3)
                                       })))
                                     })))
                                   }))
)


contrast_extended_hl$reference_variable <- sapply(contrast_extended_hl$contrast, function(x) {
  unlist(strsplit(x, " vs "))[1]
})

contrast_extended_hl$contrast_variable <- sapply(contrast_extended_hl$contrast, function(x) {
  unlist(strsplit(x, " vs "))[2]
})

contrast_extended_hl$reference_variable <- factor(contrast_extended_hl$reference_variable, 
                                                  levels = c("Slight", "Mild",  "Moderate", "Moderately severe"))
# Convert quantile to factor
contrast_extended_hl$quantile <- factor(contrast_extended_hl$quantile)


# Plot with facets and colored points
ggbox2 = ggplot(contrast_extended_hl, aes(x = contrast_variable,
                                          y = p_value_extended)) +
  geom_boxplot(aes(fill = contrast_variable), color = "blue") +  # Set boxplot outline color to blue
  geom_point(aes(color = quantile), size = 3, position = position_jitter(width = 0.2)) +  # Use jitter for better visualization
  facet_grid(~ reference_variable, scales = "free_x") +
  labs(x = "Contrast", y = "P-value", 
       color = "Quantile", 
       fill = "Contrast",
       title = "") +#P-value Distribution for Extended Model
  scale_color_manual(values = setNames(thresh_colors, quant_tresh)) +  # Use manual color scale for points
  scale_fill_manual(values = hearing_loss_col) +  # Use manual color scale for fill
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.9),  # Add grid around the plots
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),  # Adjust vjust to move labels closer to the axis
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    strip.text = element_text(size = 15),  # Make facet grid text bigger
    legend.box = "horizontal", 
    legend.box.just = "top"
  ) +
  guides(fill = "none") +  # Remove fill legend for boxplot
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red")  # Add dashed line at y = 0.05

ggsave(filename = paste0(mydir_figs,"box_hl_extended", ".pdf"), 
       plot = ggbox2,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)

final_contrast_hl = cbind(contrast_base_hl, contrast_extended_hl["p_value_extended"])
final_contrast_hl


kable(final_contrast_hl, format = 'latex', linesep = "")


#########################
# Contrast by Quantiles #
#########################


comparisons <- c()

for (i in 1:(length(freq_tresh_plot) - 1)) {
  for (j in (i + 1):length(freq_tresh_plot)) {
    comparisons <- c(comparisons, paste(freq_tresh_plot[i], "vs", freq_tresh_plot[j]))
  }
}

#Baseline

contrast_base_quant = data.frame( contrast = rep(comparisons, 5),
                                  
                                  hearing_loss = rep(hl_classes, each = 10),
                                  
                                  p_value_base = do.call(rbind,lapply(1:n_hlclasses, function(k) {
                                    # For each hearing loss class k
                                    t(do.call(cbind,lapply(1:(n_tresh - 1), function(i) {
                                      # For each threshold i
                                      t(as.numeric(sapply((i + 1):n_tresh, function(j) {
                                        # Compare with all subsequent thresholds j > i
                                        round(vuong_test_with_loglike(loglike_base_age_hl[[i]][[k]]$log_like_vector,
                                                                      loglike_base_age_hl[[j]][[k]]$log_like_vector)$vuong_pvalue ,3)
                                      })))
                                    })))
                                  })
                                  )
)


contrast_base_quant$reference_quantile <- sapply(contrast_base_quant$contrast, function(x) {
  unlist(strsplit(x, " vs "))[1]
})

contrast_base_quant$contrast_quantile <- sapply(contrast_base_quant$contrast, function(x) {
  unlist(strsplit(x, " vs "))[2]
})

contrast_base_quant$reference_quantile <- factor(contrast_base_quant$reference_quantile, 
                                                 levels = c("20%", "40%",  "60%", "80%"))
# Convert hearing loss to factor
contrast_base_quant$hearing_loss <- factor(contrast_base_quant$hearing_loss,
                                           levels = c("Slight", "Mild", "Moderate",  
                                                      "Moderately severe", "Severe"))


# Plot with facets and colored points
ggbox3 = ggplot(contrast_base_quant, aes(x = factor(contrast_quantile, 
                                                    levels = c("20%", "40%", "60%", "80%", "90%")),
                                         y = p_value_base)) +
  geom_boxplot(aes(fill = factor(contrast_quantile, 
                                 levels = c("20%", "40%", "60%", "80%", "90%"))),
               color = "blue") +  # Set boxplot outline color to blue
  geom_point(aes(color = hearing_loss), size = 3, position = position_jitter(width = 0.2)) +
  facet_grid(~ reference_quantile, scales = "free_x") +
  labs(x = "Contrast", y = "P-value", 
       color = "Hearing Loss", 
       fill = "Contrast",
       title = "") +#P-value Distribution for Baseline Model
  scale_color_manual(values = hearing_loss_col) +  # Set point colors based on hearing_loss_col
  scale_fill_manual(values = setNames(thresh_colors[-1], 
                                      unique(contrast_base_quant$contrast_quantile))) +  # Set fill colors
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.9),  # Add grid around the plots
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),  # Adjust vjust to move labels closer to the axis
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    strip.text = element_text(size = 15),  # Make facet grid text bigger
    legend.box = "horizontal", 
    legend.box.just = "top"
  ) +
  guides(fill = "none") +  # Remove fill legend for boxplot
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red")  # Add dashed line at y = 0.05


ggsave(filename = paste0(mydir_figs,"box_quant_baseline", ".pdf"), 
       plot = ggbox3,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)

#Extended
contrast_extended_quant = data.frame( contrast = rep(comparisons, 5),
                                      
                                      hearing_loss = rep(hl_classes, each = 10),
                                      
                                      p_value_extended = do.call(rbind,lapply(1:n_hlclasses, function(k) {
                                        # For each hearing loss class k
                                        t(do.call(cbind,lapply(1:(n_tresh - 1), function(i) {
                                          # For each threshold i
                                          t(as.numeric(sapply((i + 1):n_tresh, function(j) {
                                            # Compare with all subsequent thresholds j > i
                                            round(vuong_test_with_loglike(loglike_extended_age_hl[[i]][[k]]$log_like_vector,
                                                                          loglike_extended_age_hl[[j]][[k]]$log_like_vector)$vuong_pvalue ,3)
                                          })))
                                        })))
                                      })
                                      )
)


contrast_extended_quant$reference_quantile <- sapply(contrast_extended_quant$contrast, function(x) {
  unlist(strsplit(x, " vs "))[1]
})

contrast_extended_quant$contrast_quantile <- sapply(contrast_extended_quant$contrast, function(x) {
  unlist(strsplit(x, " vs "))[2]
})

contrast_extended_quant$reference_quantile <- factor(contrast_extended_quant$reference_quantile, 
                                                     levels = c("20%", "40%",  "60%", "80%"))
# Convert quantile to factor
contrast_extended_quant$hearing_loss <- factor(contrast_extended_quant$hearing_loss,
                                               levels = c("Slight", "Mild", "Moderate",  
                                                          "Moderately severe", "Severe"))


# Plot with facets and colored points
ggbox4 = ggplot(contrast_extended_quant, aes(x = factor(contrast_quantile, 
                                                        levels = c("20%", "40%", "60%", "80%", "90%")),
                                             y = p_value_extended)) +
  geom_boxplot(aes(fill = factor(contrast_quantile, 
                                 levels = c("20%", "40%", "60%", "80%", "90%"))),
               color = "blue") +  # Set boxplot outline color to blue
  geom_point(aes(color = hearing_loss), size = 3, position = position_jitter(width = 0.2)) +
  facet_grid(~ reference_quantile, scales = "free_x") +
  labs(x = "Contrast", y = "P-value", 
       color = "Hearing Loss", 
       fill = "Contrast",
       title = "") +#P-value Distribution for Extended Model
  scale_color_manual(values = hearing_loss_col) +  # Set point colors based on hearing_loss_col
  scale_fill_manual(values = setNames(thresh_colors[-1], 
                                      unique(contrast_extended_quant$contrast_quantile))) +  # Set fill colors
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(color = NA),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.9),  # Add grid around the plots
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),  # Adjust vjust to move labels closer to the axis
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    strip.text = element_text(size = 15),  # Make facet grid text bigger
    legend.box = "horizontal", 
    legend.box.just = "top"
  ) +
  guides(fill = "none") +  # Remove fill legend for boxplot
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red")  # Add dashed line at y = 0.05

ggsave(filename = paste0(mydir_figs,"box_quant_extended", ".pdf"), 
       plot = ggbox4,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)




final_contrast_quant = cbind(contrast_base_quant, contrast_extended_quant["p_value_extended"])
final_contrast_quant


kable(final_contrast_quant, format = 'latex', linesep = "")


###################
# Contrast by SEX #
###################

#Baseline
contrast_base_sex = data.frame( contrast = t(cbind(t(as.vector(rep(paste("Female", "vs", 
                                                                         "Male"), 5))))),
                                
                                quantile = quant_tresh,
                                
                                p_value_base = do.call(rbind,lapply(1:(2-1), function(k) {
                                  t(do.call(cbind, lapply(1:n_tresh, function(i) {
                                    t(as.numeric(sapply((k+1):2, function(j) {
                                      round(vuong_test_with_loglike(loglike_base_sex[[i]][[k]]$log_like_vector,
                                                                    loglike_base_sex[[i]][[j]]$log_like_vector)$vuong_pvalue,3)
                                    })))
                                  })))
                                }))
                                
)


contrast_base_sex$reference_variable <- sapply(contrast_base_sex$contrast, function(x) {
  unlist(strsplit(x, " vs "))[1]
})

contrast_base_sex$contrast_variable <- sapply(contrast_base_sex$contrast, function(x) {
  unlist(strsplit(x, " vs "))[2]
})

contrast_base_sex$reference_variable <- factor(contrast_base_sex$reference_variable, 
                                               levels = c("Female", "Male"))
# Convert quantile to factor
contrast_base_sex$quantile <- factor(contrast_base_sex$quantile)


#Extended
contrast_extended_sex = data.frame( contrast = t(cbind(t(as.vector(rep(paste("Female", "vs", 
                                                                             "Male"), 5))))),
                                    
                                    quantile = quant_tresh,
                                    
                                    p_value_extended = do.call(rbind,lapply(1:(2-1), function(k) {
                                      t(do.call(cbind, lapply(1:n_tresh, function(i) {
                                        t(as.numeric(sapply((k+1):2, function(j) {
                                          round(vuong_test_with_loglike(loglike_extended_sex[[i]][[k]]$log_like_vector,
                                                                        loglike_extended_sex[[i]][[j]]$log_like_vector)$vuong_pvalue,3)
                                        })))
                                      })))
                                    }))
)


contrast_extended_sex$reference_variable <- sapply(contrast_extended_sex$contrast, function(x) {
  unlist(strsplit(x, " vs "))[1]
})

contrast_extended_sex$contrast_variable <- sapply(contrast_extended_sex$contrast, function(x) {
  unlist(strsplit(x, " vs "))[2]
})

contrast_extended_sex$reference_variable <- factor(contrast_extended_sex$reference_variable, 
                                                   levels = c("Female", "Male"))
# Convert quantile to factor
contrast_extended_sex$quantile <- factor(contrast_extended_sex$quantile)



#########################
# Contrast by Quantiles #
#########################

comparisons <- c()

for (i in 1:(length(freq_tresh_plot) - 1)) {
  for (j in (i + 1):length(freq_tresh_plot)) {
    comparisons <- c(comparisons, paste(freq_tresh_plot[i], "vs", freq_tresh_plot[j]))
  }
}

#Baseline
contrast_base_quant_sex = data.frame( contrast = rep(comparisons, 2),
                                      
                                      sex = rep(c("Female", "Male"), each = 10),
                                      
                                      p_value_base = do.call(rbind,lapply(1:2, function(k) {
                                        # For each hearing loss class k
                                        t(do.call(cbind,lapply(1:(n_tresh - 1), function(i) {
                                          # For each threshold i
                                          t(as.numeric(sapply((i + 1):n_tresh, function(j) {
                                            # Compare with all subsequent thresholds j > i
                                            round(vuong_test_with_loglike(loglike_base_sex[[i]][[k]]$log_like_vector,
                                                                          loglike_base_sex[[j]][[k]]$log_like_vector)$vuong_pvalue ,3)
                                          })))
                                        })))
                                      })
                                      )
)


contrast_base_quant_sex$reference_quantile <- sapply(contrast_base_quant_sex$contrast, function(x) {
  unlist(strsplit(x, " vs "))[1]
})

contrast_base_quant_sex$contrast_quantile <- sapply(contrast_base_quant_sex$contrast, function(x) {
  unlist(strsplit(x, " vs "))[2]
})

contrast_base_quant_sex$reference_quantile <- factor(contrast_base_quant_sex$reference_quantile, 
                                                     levels = c("20%", "40%",  "60%", "80%"))
# Convert hearing loss to factor
contrast_base_quant_sex$sex <- factor(contrast_base_quant_sex$sex,
                                      levels = c("Female",  
                                                 "Male"))
contrast_base_quant_sex


#Extended
contrast_extended_quant_sex = data.frame( contrast = rep(comparisons, 2),
                                          
                                          sex = rep(c("Female", "Male"), each = 10),
                                          
                                          p_value_extended = do.call(rbind,lapply(1:2, function(k) {
                                            # For each hearing loss class k
                                            t(do.call(cbind,lapply(1:(n_tresh - 1), function(i) {
                                              # For each threshold i
                                              t(as.numeric(sapply((i + 1):n_tresh, function(j) {
                                                # Compare with all subsequent thresholds j > i
                                                round(vuong_test_with_loglike(loglike_extended_sex[[i]][[k]]$log_like_vector,
                                                                              loglike_extended_sex[[j]][[k]]$log_like_vector)$vuong_pvalue ,3)
                                              })))
                                            })))
                                          })
                                          )
)


contrast_extended_quant_sex$reference_quantile <- sapply(contrast_extended_quant_sex$contrast, function(x) {
  unlist(strsplit(x, " vs "))[1]
})

contrast_extended_quant_sex$contrast_quantile <- sapply(contrast_extended_quant_sex$contrast, function(x) {
  unlist(strsplit(x, " vs "))[2]
})

contrast_extended_quant_sex$reference_quantile <- factor(contrast_extended_quant_sex$reference_quantile, 
                                                         levels = c("20%", "40%",  "60%", "80%"))
# Convert quantile to factor
contrast_extended_quant_sex$sex <- factor(contrast_extended_quant_sex$sex,
                                          levels = c("Female",  
                                                     "Male"))
contrast_extended_quant_sex


