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
library(geomtextpath)

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

saveRDS(totals_count, file = paste(mydir, "totals_count_5y.rds" , sep ="") )
saveRDS(hr_rates_interp, file = paste(mydir, "hr_rates_interp_5y.rds" , sep ="") )
saveRDS(spiq_rates, file = paste(mydir, "spiq_rates_5y.rds" , sep ="") )
saveRDS(spin_rates, file = paste(mydir, "spin_rates_5y.rds" , sep ="") )


####################
# Plot Proportions #
####################

#Hearing Loss Proportions
plots_hr_rates = plot_hr_rates(hr_rates_interp,
                               hl_classes, 
                               n_tresh,
                               age_group_var,
                               freq_tresh, 
                               sex_classes) 
plots_hr_rates[[1]]
plots_hr_rates[[2]]
plots_hr_rates[[3]]


plots_hr_rates2 = plot_hr_rates2(hr_rates_interp,
                                 hl_classes, 
                                 n_tresh,
                                 age_group_var,
                                 freq_tresh, 
                                 sex_classes) 

plots_hr_rates2[[1]]
plots_hr_rates2[[2]]
plots_hr_rates2[[3]]


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"bar_r_age_", i, ".pdf"), 
         plot = plots_hr_rates[[1]][[i]],
         width = 12,
         height = 9)

  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"bar_r_agehl_", i, ".pdf"), 
         plot = plots_hr_rates[[2]][[i]],
         width = 12,
         height = 9)
  
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"bar_r_agesex_", i, ".pdf"), 
         plot = plots_hr_rates[[3]][[i]],
         width = 12,
         height = 9)

  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"hear_r_age_", i, ".pdf"), 
         plot = plots_hr_rates2[[1]][[i]],
         width = 12,
         height = 9)

  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"hear_r_agehl_", i, ".pdf"),
         plot = plots_hr_rates2[[2]][[i]],
         width = 12,
         height = 9)
  
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"hear_r_agesex_", i, ".pdf"),
         plot = plots_hr_rates2[[3]][[i]],
         width = 12,
         height = 9)
  
  
}



#Spiq Proportions
plots_spiq_rates = plot_speech_rates(spiq_rates,
                                     hl_classes, 
                                     n_tresh,
                                     age_group_var,
                                     treshold_SPIQ,
                                     "SpiQ",
                                     sex_classes ) 

plots_spiq_rates[[1]]
plots_spiq_rates[[2]]
plots_spiq_rates[[3]]




for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spiq_bar_age_", i, ".pdf"), 
         plot = plots_spiq_rates[[1]][[i]],
         width = 8,
         height = 6)

  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spiq_bar_agehl_", i, ".pdf"),
         plot = plots_spiq_rates[[2]][[i]],
         width = 8,
         height = 6)
  
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spiq_bar_agesex_", i, ".pdf"),
         plot = plots_spiq_rates[[3]][[i]],
         width = 8,
         height = 6)
}


plots_spiq_rates2 = plot_speech_rates2(spiq_rates,
                                       hl_classes, 
                                       n_tresh,
                                       age_group_var,
                                       treshold_SPIQ,
                                       "SpiQ",
                                       sex_classes) 

plots_spiq_rates2[[1]]
plots_spiq_rates2[[2]]
plots_spiq_rates2[[3]]



for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spiq_r_age_", i, ".pdf"), 
         plot = plots_spiq_rates2[[1]][[i]],
         width = 8,
         height = 6)

  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spiq_r_agehl_", i, ".pdf"),
         plot = plots_spiq_rates2[[2]][[i]],
         width = 8,
         height = 6)
  
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spiq_r_agesex_", i, ".pdf"),
         plot = plots_spiq_rates2[[3]][[i]],
         width = 8,
         height = 6)
}


#Spin Proportions
plots_spin_rates = plot_speech_rates(spin_rates,
                                     hl_classes, 
                                     n_tresh,
                                     age_group_var,
                                     treshold_SPIN,
                                     "SpiN",
                                     sex_classes ) 

plots_spin_rates[[1]]
plots_spin_rates[[2]]
plots_spin_rates[[3]]


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spin_bar_age_", i, ".pdf"), 
         plot = plots_spin_rates[[1]][[i]],
         width = 8,
         height = 6)

  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spin_bar_agehl_", i, ".pdf"),
         plot = plots_spin_rates[[2]][[i]],
         width = 8,
         height = 6)
  
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spin_bar_agesex_", i, ".pdf"),
         plot = plots_spin_rates[[3]][[i]],
         width = 8,
         height = 6)
}



plots_spin_rates2 = plot_speech_rates2(spin_rates,
                                       hl_classes, 
                                       n_tresh,
                                       age_group_var,
                                       treshold_SPIN,
                                       "SpiN",
                                       sex_classes ) 

plots_spin_rates2[[1]]
plots_spin_rates2[[2]]
plots_spin_rates2[[3]]


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spin_r_age_", i, ".pdf"), 
         plot = plots_spin_rates2[[1]][[i]],
         width = 8,
         height = 6)

  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spin_r_agehl_", i, ".pdf"),
         plot = plots_spin_rates2[[2]][[i]],
         width = 8,
         height = 6)
  
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spin_r_agesex_", i, ".pdf"),
         plot = plots_spin_rates2[[3]][[i]],
         width = 8,
         height = 6)
}


heatmaps_rates = plot_heat_rates(hr_rates_interp,spiq_rates, spin_rates,
                                 hl_classes, freq_vector, n_tresh,
                                 age_group_var, freq_tresh,
                                 freq_tresh_plot, sex_classes  )
                                 #treshold_SPIQ, treshold_SPIN)


heatmaps_rates[[1]]
heatmaps_rates[[2]]
heatmaps_rates[[3]]



ggsave(filename = paste0(mydir_figs,"heat_overall", ".pdf"), 
       plot = heatmaps_rates[[1]],
       width = 12,
       height = 4,
       device = "pdf",
       units = "in", dpi = 300)

ggsave(filename = paste0(mydir_figs,"heat_hl", ".pdf"), 
         plot = heatmaps_rates[[2]],
         width = 12,
         height = 9)

ggsave(filename = paste0(mydir_figs,"heat_sex", ".pdf"), 
       plot = heatmaps_rates[[3]],
       width = 12,
       height = 6)




polar_heatmaps_rates = plot_polar_heatmaps(hr_rates_interp,spiq_rates, spin_rates,
                    hl_classes, freq_vector, n_tresh,
                    age_group_var, freq_tresh,
                    freq_tresh_plot, sex_classes  )



ggsave(filename = paste0(mydir_figs,"heatpolar_overall", ".pdf"), 
       plot = polar_heatmaps_rates[[1]],
       width = 12,
       height = 4,
       device = "pdf",
       units = "in", dpi = 300)

ggsave(filename = paste0(mydir_figs,"heatpolar_hl", ".pdf"), 
       plot = polar_heatmaps_rates[[2]],
       width = 12,
       height = 11)

ggsave(filename = paste0(mydir_figs,"heatpolar_sex", ".pdf"), 
       plot = polar_heatmaps_rates[[3]],
       width = 12,
       height = 6)

ggsave(filename = paste0(mydir_figs,"heatpolar_altogether", ".pdf"), 
       plot = polar_heatmaps_rates[[4]],
       width = 13,
       height = 15)



###############
###############
# CPBMT MODEL #
###############
###############

#######################
# CPBMT MODEL overall #
#######################

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

saveRDS(model_hl,
        file = paste0(mydir,"model_hl", ".rds"), )


#MODEL ON THE counts --> use this for getting the alphas
demodata_byhl_counts = lapply(1:n_tresh, function(i)
  demogdata(data = hr_counts[[1]][[i]],
            pop = matrix(rep(totals_count[[1]][[i]], each = 11), ncol = 11, byrow = TRUE),
            ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),
            years = as.numeric(freq_vector),
           type = 'mortality',
            label = 'hearing',
            name = 'total') )

model_hl_counts = lapply(1:n_tresh, function(i)
  lca(data = demodata_byhl_counts[[i]],
      restype = "deaths"))

saveRDS(model_hl_counts,
        file = paste0(mydir,"model_hl_counts", ".rds"), )


#################################
# Plots for CPBMT model results #
#################################



plots_model_age <- plot_coeff_by_age(model_hl, freq_tresh_plot,
                                     age_group_var, freq_vector,
                                     thresh_colors)
plots_model_age[[1]]
plots_model_age[[2]]
plots_model_age[[3]]



plots_model_age_counts <- plot_coeff_by_age(model_hl_counts, freq_tresh_plot,
                                     age_group_var, freq_vector,
                                     thresh_colors)

plots_model_age_counts[[1]]
plots_model_age_counts[[2]]
plots_model_age_counts[[3]]


gg_by_tresh_age = plot_coeff_kb_age(model_hl, freq_tresh_plot,
                                    age_group_var, freq_vector)




ggsave(filename = paste0(mydir_figs,"BK_age", ".pdf"), 
       plot = gg_by_tresh_age,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"kappa_age", ".pdf"), 
       plot = plots_model_age[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"a_age", ".pdf"), 
       plot = plots_model_age[[2]],#plots_model_age_counts
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"b_age", ".pdf"), 
       plot = plots_model_age[[3]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)



#change the colors for the threshodls like one color per thresholds with all the
#scales for age + invert them like the dark one is the one old
bikappaplot = plot_coeff_bikappa_age(model_hl, freq_tresh_plot,
                       age_group_var, freq_vector,
                       rev(viridis(10, option = "C")))



ggsave(filename = paste0(mydir_figs,"bikappa_age", ".pdf"), 
       plot = bikappaplot,
       width = 10,
       height = 8,
       device = "pdf",
       units = "in", dpi = 300)




###############################################
# CPBMT model by degree of hearing loss & Age # 
###############################################

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
    lca(data = demodata_by_age_hl[[i]][[j]] , restype = "rates",
        interpolate = TRUE)) )


saveRDS(model_by_age_hl,
        file = paste0(mydir,"model_by_age_hl", ".rds") )



#MODEL ON THE counts --> use this for getting the alphas and move to the plot files
hr_counts2 = hr_counts

demodata_by_age_hl_counts = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    demogdata(data = hr_counts2[[2]][[i]][[j]],
             pop = matrix(rep(totals_count[[2]][[i]][[j]], each = 11), ncol = 11, byrow = TRUE),
              ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),
              years = as.numeric(freq_vector),
             type = 'mortality',
              label = 'hearing',
              name = 'total') ) )


model_by_age_hl_counts = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    lca(data = demodata_by_age_hl_counts[[i]][[j]], restype = "rates",
        interpolate = TRUE)) )

saveRDS(model_by_age_hl_counts,
        file = paste0(mydir,"model_by_age_hl_counts", ".rds") )




####################################
# Plots for SSM model resultson HL #
####################################


plots_model_agehl <- plot_coeff_by_age_hl(model_by_age_hl, 
                                          n_tresh, n_hlclasses, hl_classes,
                                          age_group_var ,
                                          freq_tresh_plot, freq_vector,
                                          thresh_colors)


plots_model_agehl[[1]]
plots_model_agehl[[2]]
plots_model_agehl[[3]]


plots_model_agehl_counts <- plot_coeff_by_age_hl(model_by_age_hl_counts, 
                                                 n_tresh, n_hlclasses, hl_classes,
                                                 age_group_var ,
                                                 freq_tresh_plot, freq_vector,
                                                 thresh_colors)

plots_model_agehl_counts[[1]]
plots_model_agehl_counts[[2]]
plots_model_agehl_counts[[3]]




gg_by_tresh_age_hl = plot_coeff_kb_age_hl(model_by_age_hl, 
                                          n_tresh, n_hlclasses, hl_classes,
                                          age_group_var ,
                                          freq_tresh_plot, freq_vector)[[1]]


ggsave(filename = paste0(mydir_figs,"BK_age_hl", ".pdf"), 
       plot = gg_by_tresh_age_hl,
       width = 10,
       height = 10,
       device = "pdf",
       units = "in", dpi = 300)




ggsave(filename = paste0(mydir_figs,"kappa_age_hl", ".pdf"), 
       plot = plots_model_agehl[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)



ggsave(filename = paste0(mydir_figs,"kappa_age_hl", ".pdf"), 
       plot = plots_model_agehl[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"a_age_hl", ".pdf"), 
       plot = plots_model_agehl[[2]],#plots_model_agehl_counts
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"b_age_hl", ".pdf"), 
       plot = plots_model_agehl[[3]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)



#same as above + ordering of the plots by HL level
bikappa_age_hl = plot_coeff_bikappa_age_hl(model_by_age_hl, 
                          n_tresh, n_hlclasses, hl_classes,
                          age_group_var ,
                          freq_tresh_plot, freq_vector,
                          rev(viridis(10, option = "C")))[[1]]

ggsave(filename = paste0(mydir_figs,"bikappa_age_hl", ".pdf"), 
       plot = bikappa_age_hl,
       width = 12,
       height = 10,
       device = "pdf",
       units = "in", dpi = 300)



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


saveRDS(model_by_age_sex,
        file = paste0(mydir,"model_by_age_sex", ".rds") )


#MODEL ON THE counts --> use this for getting the alphas
demodata_by_age_sex_counts = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j)
    demogdata(data = hr_counts[[3]][[i]][[j]],
              pop = matrix(rep(totals_count[[3]][[i]][[j]], each = 11),
                           ncol = 11, byrow = TRUE),
              ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),
              years = as.numeric(freq_vector),
              type = 'mortality',
              label = 'hearing',
              name = 'total') ) )


model_by_age_sex_counts = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j)
    lca(data = demodata_by_age_sex_counts[[i]][[j]], restype = "rates",
        interpolate = TRUE)) )

saveRDS(model_by_age_sex_counts,
        file = paste0(mydir,"model_by_age_sex_counts", ".rds") )


####################################
# Plots for SSM model resultson Sex #
####################################

plots_model_agesex <- plot_coeff_by_age_hl(model_by_age_sex, 
                                          n_tresh, 2, c("Female", "Male"),
                                          age_group_var ,
                                          freq_tresh_plot, freq_vector,
                                          thresh_colors)


plots_model_agesex[[1]]
plots_model_agesex[[2]]
plots_model_agesex[[3]]

plots_model_agesex_counts <- plot_coeff_by_age_hl(model_by_age_sex_counts, 
                                                n_tresh, 2, c("Female", "Male"),
                                                age_group_var ,
                                                freq_tresh_plot, freq_vector,
                                                thresh_colors)

plots_model_agesex_counts[[1]]
plots_model_agesex_counts[[2]]
plots_model_agesex_counts[[3]]

gg_by_tresh_age_sex = plot_coeff_kb_age_hl(model_by_age_sex, 
                                          n_tresh, 2, c("Female", "Male"),
                                          age_group_var ,
                                          freq_tresh_plot, freq_vector)[[1]]


ggsave(filename = paste0(mydir_figs,"BK_age_sex", ".pdf"), 
       plot = gg_by_tresh_age_sex,
       width = 10,
       height = 10,
       device = "pdf",
       units = "in", dpi = 300)




ggsave(filename = paste0(mydir_figs,"kappa_age_sex", ".pdf"), 
       plot = plots_model_agesex[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)



ggsave(filename = paste0(mydir_figs,"kappa_age_sex", ".pdf"), 
       plot = plots_model_agesex[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"a_age_sex", ".pdf"), 
       plot = plots_model_agesex[[2]], #plots_model_agesex_counts[[2]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"b_age_sex", ".pdf"), 
       plot = plots_model_agesex[[3]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)



#same as above + ordering of the plots by HL level
bikappa_age_sex = plot_coeff_bikappa_age_hl(model_by_age_sex, 
                                           n_tresh, 2, c("Female", "Male"),
                                           age_group_var ,
                                           freq_tresh_plot, freq_vector,
                                           rev(viridis(10, option = "C")))[[1]]

ggsave(filename = paste0(mydir_figs,"bikappa_age_sex", ".pdf"), 
       plot = bikappa_age_sex,
       width = 12,
       height = 10,
       device = "pdf",
       units = "in", dpi = 300)




################################################################
################################################################
# By Hearing Loss with the overall and split by quantile level #
################################################################
################################################################

#By HL

gg_risk_profiles = plot_coeff_all(model_hl, model_by_age_hl, 
                                  n_tresh, n_hlclasses, hl_classes,
                                  age_group_var ,
                                  freq_tresh_plot, freq_vector,
                                  hearing_loss_col,
                                  "Hearing Loss")

gg_risk_profiles_count = plot_coeff_all(model_hl_counts, model_by_age_hl_counts, 
                                        n_tresh, n_hlclasses, hl_classes,
                                        age_group_var ,
                                        freq_tresh_plot, freq_vector,
                                        hearing_loss_col,
                                        "Hearing Loss")


ggsave(filename = paste0(mydir_figs,"b_risk", ".pdf"), 
       plot = gg_risk_profiles[[3]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"a_risk", ".pdf"), 
       plot = gg_risk_profiles[[2]], #gg_risk_profiles_count
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"k_risk", ".pdf"), 
       plot = gg_risk_profiles[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)

bg_color <- "#f0f0f0"

combined_risk <- (gg_risk_profiles[[2]] / gg_risk_profiles[[3]] / gg_risk_profiles[[1]]) + 
  plot_layout(heights = c(1, 1, 1),
              guides = 'collect') &
  theme(legend.position = "bottom")

ggsave(filename = paste0(mydir_figs,"risk_profiles", ".pdf"), 
       plot = combined_risk,
       width = 10,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300,
       bg = bg_color)



combined_risk_unique <- (gg_risk_profiles[[8]] + gg_risk_profiles[[7]]) +
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")


ggsave(filename = paste0(mydir_figs,"risk_profiles_unique_hl", ".pdf"), 
       plot = combined_risk_unique,
       width = 10,
       height = 5,
       device = "pdf",
       units = "in", dpi = 300,
       bg = bg_color)




#By Sex
gg_risk_profiles_sex = plot_coeff_all(model_hl, model_by_age_sex, 
                                  n_tresh, 2, c("Female", "Male"),
                                  age_group_var ,
                                  freq_tresh_plot, freq_vector,
                                  c("pink","blue"),
                                  "Sex")

gg_risk_profiles_count_sex = plot_coeff_all(model_hl_counts, model_by_age_sex_counts, 
                                        n_tresh, 2, c("Female", "Male"),
                                        age_group_var ,
                                        freq_tresh_plot, freq_vector,
                                        c("pink","blue"),
                                        "Sex")

ggsave(filename = paste0(mydir_figs,"b_risk_sex", ".pdf"), 
       plot = gg_risk_profiles_sex[[3]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"a_risk_sex", ".pdf"), 
       plot = gg_risk_profiles_sex[[2]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"k_risk_sex", ".pdf"), 
       plot = gg_risk_profiles_sex[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)

bg_color <- "#f0f0f0"

combined_risk_sex <- (gg_risk_profiles_sex[[2]] / gg_risk_profiles_sex[[3]] / gg_risk_profiles_sex[[1]]) + 
  plot_layout(heights = c(1, 1, 1),
              guides = 'collect') &
  theme(legend.position = "bottom")

ggsave(filename = paste0(mydir_figs,"risk_profiles_sex", ".pdf"), 
       plot = combined_risk_sex,
       width = 10,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300,
       bg = bg_color)



combined_risk_unique_sex <- (gg_risk_profiles_sex[[8]] + gg_risk_profiles_sex[[9]] + gg_risk_profiles_sex[[7]]) +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom")


ggsave(filename = paste0(mydir_figs,"risk_profiles_unique_sex", ".pdf"), 
       plot = combined_risk_unique_sex,
       width = 10,
       height = 5,
       device = "pdf",
       units = "in", dpi = 300,
       bg = bg_color)




#############################
# ALL RESULTS TOGETHER PLOT #
#############################

#OVERALL

# Define the background color
bg_color <- "#f0f0f0"  # Super light gray

# Combine the first three plots and share a common legend at the bottom
combined_row1_0 <- plots_model_age[[2]] + 
  plots_model_age[[3]] + 
  plots_model_age[[1]] + 
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

# Combine plot6 and plot7 into a single row
combined_row3_0 <- gg4 + gg5 + 
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')


# Arrange the combined row and the fourth plot, then add the fifth plot as a third row
combined_plot_0 <- (combined_row1_0 / bikappaplot / gg_by_tresh_age / combined_row3_0) + 
  plot_layout(heights = c(1, 1, 1, 1)) 

# Print the combined plot with the light gray background
print(combined_plot_0 & theme(plot.background = element_rect(fill = bg_color, 
                                                             color = NA)))

combined_plot_0_with_bg <- combined_plot_0 & theme(plot.background = element_rect(fill = bg_color, color = NA))



ggsave(filename = paste0(mydir_figs,"overall_overall", ".pdf"), 
       plot = combined_plot_0,#combined_plot_0_with_bg
       width = 9,
       height = 12,
       device = "pdf",
       units = "in", dpi = 300,
       bg = bg_color)

#BY HEARING LOSS SEPARATION


#taka alpha which makes sense here
# Define the function to create the plots
create_plots <- function(hl_type, hl_index) {
  
  plot1 <- ggplot(filter(plots_model_agehl_counts[[5]], 
                         `HL degree` == hl_type), 
                  aes(x = age, y = scale(value),
                      group = treshold, color = treshold)) +
    geom_line(size = 1.5) +
    labs(title = "", x = "Age", y = "As") +
    theme_bw() +  
    theme(legend.position = "none",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white')) +
    scale_color_manual(values = thresh_colors, name = "Quantile")
  
  plot2 <- ggplot(filter(plots_model_agehl[[4]],
                         `HL degree` == hl_type), 
                  aes(x = age, y = scale(value), 
                      group = treshold, color = treshold)) +
    geom_line(size = 1.5) +
    labs(title = "", x = "Age", y = "Bs") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white')) +
    scale_color_manual(values = thresh_colors, name = "Quantile")
  
  plot3 <- ggplot(filter(plots_model_agehl[[6]], 
                         `HL degree` == hl_type),
                  aes(x = frequency, y = scale(value), 
                      group = treshold, color = treshold)) +
    geom_line(size = 1.5) + 
    labs(title = "", x = "Frequency", y = "Kappas") +
    theme_bw() +  
    theme(legend.position = "none",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white')) +
    scale_color_manual(values = thresh_colors, name = "Quantile")
  
  plot4 <- ggplot(filter(plot_coeff_bikappa_age_hl(model_by_age_hl, 
                                                   n_tresh, n_hlclasses, 
                                                   hl_classes,
                                                   age_group_var,
                                                   freq_tresh_plot, 
                                                   freq_vector,
                                                   rev(viridis(10, 
                                                               option = "C")))[[2]], 
                         `hl_loss` == hl_type),
                  aes(x = as.factor(variable), y = value, 
                      color = as.factor(age), group = as.factor(age))) +
    geom_line()  +
    geom_point() +  
    facet_grid(~threshold, scales = "free" ) +
    scale_color_manual(values = rev(viridis(10, option = "C"))) +
    labs(title = "", x = "Frequency", y = "b*k", color = "Age") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 0.9),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12)) +
    guides(color = guide_legend(ncol = 10))
  
  filtered_data_for_ggplot <- filter(plot_coeff_kb_age_hl(model_by_age_hl, 
                                                          n_tresh, n_hlclasses,
                                                          hl_classes,
                                                          age_group_var,
                                                          freq_tresh_plot, 
                                                          freq_vector)[[2]],
                                     `hl_loss` == hl_type)
  
  plot5 <- ggplot(filtered_data_for_ggplot, 
                  aes(x = age, y = value,  color = variable, group = variable)) +
    geom_line(size = 1)  + 
    facet_grid(~threshold,scales = "free" ) +
    scale_color_discrete() +
    labs(title = "",
         x = "Age",
         y = "B*K",
         color = "Frequency") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.text.x = element_text(size = 10, angle = 0),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12)) +
    scale_x_continuous(breaks = seq(min(filtered_data_for_ggplot$age), 
                                    max(filtered_data_for_ggplot$age), by = 10)) +
    guides(color = guide_legend(ncol = 11))  # Adjust legend to 11 columns
  
  # Extract plots 6 and 7 for the given hearing loss type (index 3 for Moderate)
  plot6 <- plots_list_spiq_FWL[[hl_index]]
  plot7 <- plots_list_spin_FWL[[hl_index]]
  
  # Combine the first three plots and share a common legend at the bottom
  combined_row1 <- plot1 + 
    plot2 + 
    plot3 + 
    plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
  
  # Combine plot6 and plot7 into a single row
  combined_row3 <- plot6 + plot7 + 
    plot_layout(guides = 'collect') & theme(legend.position = 'bottom') 
  
  # Arrange the combined row and the fourth plot, then add the fifth plot as a third row
  combined_plot <- (combined_row1 / plot4 / plot5 / combined_row3) + plot_layout(heights = c(1, 1, 1, 1))
  
  return(combined_plot)
}


gg_overall_slight = create_plots("Slight",1)
gg_overall_mild = create_plots("Mild",2)
gg_overall_moderate = create_plots("Moderate",3)
gg_overall_modsevere = create_plots("Moderately severe",4)
gg_overall_severe = create_plots("Severe",5)



ggsave(filename = paste0(mydir_figs,"overall_slight", ".pdf"), 
       plot = gg_overall_slight,
       width = 9,
       height = 12,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"overall_mild", ".pdf"), 
       plot = gg_overall_mild,
       width = 9,
       height = 12,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"overall_moderate", ".pdf"), 
       plot = gg_overall_moderate,
       width = 9,
       height = 12,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"overall_modsev", ".pdf"), 
       plot = gg_overall_modsevere,
       width = 9,
       height = 12,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"overall_severe", ".pdf"), 
       plot = gg_overall_severe,
       width = 9,
       height = 12,
       device = "pdf",
       units = "in", dpi = 300)















