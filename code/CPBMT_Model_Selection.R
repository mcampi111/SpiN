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

age_var = "age_group3" #"age_group" #"age_group2"   "age_group3"
age_group_var = all_age_groups #age_groups  #age_groups2  all_age_groups
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



###################
###################
# MSE COMPUTATION #
###################
###################

mse_hl = do.call(cbind,
                 lapply(1:n_tresh, function(i)
                   apply( (hr_rates_interp[[1]][[i]] - model_hl[[i]]$fitted$y)^2, 
                          1, sum) ))
colnames(mse_hl) = freq_tresh_plot
mse_hl = as.data.frame(mse_hl)
mse_hl$age_group = age_group_var


melt_mse_hl = melt(mse_hl, id.vars = 'age_group')

mse_age_hl = lapply(1:n_tresh, function(i)
  do.call(cbind,lapply(1:n_hlclasses, function(j) 
    apply((hr_rates_interp[[2]][[i]][[j]] - model_by_age_hl[[i]][[j]]$fitted$y )^2, 1, sum) )))

for (i in 1:n_hlclasses) {
  colnames(mse_age_hl[[i]]) = hl_classes
  mse_age_hl[[i]] = as.data.frame(mse_age_hl[[i]])
  mse_age_hl[[i]]$age_group = age_group_var
  mse_age_hl[[i]]$Threshold = freq_tresh_plot[i]
}


mse_age_hl = do.call(rbind, mse_age_hl)
mse_age_hl_melted = melt(mse_age_hl, id.vars = c('age_group','Threshold'))
mse_age_hl_melted$Threshold <- factor(mse_age_hl_melted$Threshold, 
                                      levels = freq_tresh_plot)
mse_age_hl_melted <- mse_age_hl_melted %>%
  rename(Hearing_Loss = variable) %>%
  mutate(Hearing_Loss = factor(Hearing_Loss, levels = hl_classes))



mse_age_sex = lapply(1:n_tresh, function(i)
  do.call(cbind,lapply(1:2, function(j) 
    apply((hr_rates_interp[[3]][[i]][[j]] - model_by_age_sex[[i]][[j]]$fitted$y )^2, 1, sum) )))

for (i in 1:n_tresh) {
  colnames(mse_age_sex[[i]]) = c("Female", "Male") 
  mse_age_sex[[i]] = as.data.frame(mse_age_sex[[i]])
  mse_age_sex[[i]]$age_group = age_group_var
  mse_age_sex[[i]]$Threshold = freq_tresh_plot[i]
}


mse_age_sex = do.call(rbind, mse_age_sex)
mse_age_sex_melted = melt(mse_age_sex, id.vars = c('age_group','Threshold'))
mse_age_sex_melted$Threshold = factor(mse_age_sex_melted$Threshold,
                                      levels =  freq_tresh_plot)

mse_age_sex_melted <- mse_age_sex_melted %>%
  rename(Sex = variable) %>%
  mutate(Sex = factor(Sex, levels = c("Female", "Male") ))


#Boxplots
ggplot(melt_mse_hl, aes(x = as.factor(age_group), 
                                   y = value)) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  geom_point(size = 2,
             aes(color = as.factor(variable))) +
  scale_color_manual(values = thresh_colors ) +
  labs(x = "Age", 
       y = "MSE", 
       color = paste("Threshold")) +
  theme_bw()  +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white')
  ) 


ggplot(mse_age_hl_melted, aes(x =  as.factor(age_group), 
                                        y = value)) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  geom_point(size = 2,
             aes(color = as.factor(Threshold))) +
  scale_color_manual(values = thresh_colors ) + 
  facet_wrap(~Hearing_Loss, scales = "free_y") +
  labs(x = "Age", 
       y = "MSE", 
       color = paste("Threshold")) +
  theme_bw()  +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white')
  ) 



melt_mse_hl <- melt_mse_hl %>%
  add_column(Hearing_Loss = "Overall", .after = "variable")

melt_mse_hl <- melt_mse_hl %>%
  rename(Threshold = variable) %>%
  mutate(Threshold = factor(Threshold, levels = freq_tresh_plot))



final_mse = rbind(melt_mse_hl, 
                  mse_age_hl_melted)


ggplot(final_mse, aes(x =  as.factor(age_group), 
                      y = value)) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  geom_point(size = 2,
             aes(color = Threshold)) +
  facet_wrap(~factor(Hearing_Loss, 
                     levels = c(hl_classes,"Overall") ), 
             scales = "free_y") +
  scale_color_manual(values = thresh_colors) +
  labs(x = "Age", 
       y = "MSE", 
       color = "Hearing Loss") +
  theme_bw() +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white')
  )


#Lines


col_mse_plot <- c("Overall" = "grey",
                  "Severe" = "#004529",          
                  "Moderately severe" = "#006837",
                  "Moderate" = "#31A354",         
                  "Mild" = "#A1D99B",            
                  "Slight" = "#D0F0C0")   


ggplot(final_mse, aes(x = as.factor(age_group), 
                      y = value, 
                      group = factor(Hearing_Loss, levels = c(hl_classes,"Overall")  ), 
                      color = factor(Hearing_Loss, levels = c(hl_classes,"Overall")))) +
  geom_line(linewidth = 1, alpha = 0.7) + 
  geom_point(size = 2) +
  facet_wrap(~Threshold, scales = "free_y") +
  scale_color_manual(values = col_mse_plot) +
  labs(x = "Age", 
       y = "MSE", 
       color = "Hearing Loss") +
  theme_bw() +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white')
  )

gg_finalmse = ggplot(final_mse, aes(x = as.factor(age_group), 
                      y = value, 
                      group = as.factor(Threshold), 
                      color = as.factor(Threshold))) +
  geom_line(linewidth = 1, alpha = 0.7) + 
  geom_point(size = 2) +
  facet_wrap(~factor(Hearing_Loss, 
                     levels = c(hl_classes,"Overall")), scales = "free_y") +
  scale_color_manual(values = thresh_colors) +
  labs(x = "Age", 
       y = "MSE", 
       color = "Empirical Quantile") +
  theme_bw() +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 13, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 15),   # Increased legend title size
    legend.text = element_text(size = 15),    # Increased legend text size
    strip.text = element_text(color = 'black', size = 18),
    strip.background = element_rect(fill = 'white')
  )

gg_finalmse

ggsave(filename = paste0(mydir_figs,"mse_lines_", age_var, ".pdf"), 
       plot = gg_finalmse,
       width = 22,
       height = 12)


gg_finalmse_sex = ggplot(mse_age_sex_melted, 
       aes(x = as.factor(age_group), 
                      y = value, 
                      group = as.factor(Threshold), 
                      color = as.factor(Threshold))) +
  geom_line(linewidth = 1, alpha = 0.7) + 
  geom_point(size = 2) +
  facet_wrap(~Sex, scales = "free_y") +
  scale_color_manual(values = thresh_colors) +
  labs(x = "Age", 
       y = "MSE", 
       color = "Empirical Quantile") +
  theme_bw() +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 13, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 15),   # Increased legend title size
    legend.text = element_text(size = 15),    # Increased legend text size
    strip.text = element_text(color = 'black', size = 18),
    strip.background = element_rect(fill = 'white')
  )

gg_finalmse_sex

ggsave(filename = paste0(mydir_figs,"mse_lines_sex_", age_var, ".pdf"), 
       plot = gg_finalmse_sex,
       width = 22,
       height = 12)


