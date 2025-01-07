library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(reshape2)
library(viridis)
library(ggpubr)
library(tidyr)
library(transport)
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


totals_count = readRDS(paste(mydir, "totals_count_5y.rds" , sep ="") )
hr_rates_interp = readRDS(paste(mydir, "hr_rates_interp_5y.rds" , sep ="") )
spiq_rates = readRDS(paste(mydir, "spiq_rates_5y.rds" , sep ="") )
spin_rates = readRDS(paste(mydir, "spin_rates_5y.rds" , sep ="") )

model_hl = readRDS(paste(mydir,"model_hl", ".rds", sep = ""))
model_hl_counts =readRDS(paste(mydir,"model_hl_counts", ".rds", sep = ""))

model_by_age_hl = readRDS(paste(mydir,"model_by_age_hl", ".rds", sep = ""))
model_by_age_hl_counts =readRDS(paste(mydir,"model_by_age_hl_counts",
                                      ".rds", sep = ""))


###########################################
# CHOOSE AGE STEP TO CONDUCT THE ANALYSIS #
###########################################

age_var = "age_group2" #"age_group" #"age_group2"   "age_group3"
age_group_var = age_groups2 #all_age_groups #age_groups2  age_groups
n_age = length(age_group_var)


#######################################################
#######################################################
#   Frisch–Waugh–Lovell theorem  & PARTIAL REGRESSION #
#######################################################
#######################################################

##################
# Model Overall  # 
##################

x1_hl = lapply(1:n_tresh, function(i) 
  cbind(as.numeric(model_hl[[i]]$ax), as.numeric(model_hl[[i]]$bx) ) )


H_x1_hl = lapply(1:n_tresh, function(j)
  x1_hl[[j]] %*% ginv(t(x1_hl[[j]]) %*% x1_hl[[j]]) %*% t(x1_hl[[j]])  )


M_x1_hl = lapply(1:n_tresh, function(j) diag(n_age) - H_x1_hl[[j]] )


y_adj_hl = lapply(1:n_tresh, function(i)  
  M_x1_hl[[i]] %*% hr_rates_interp[[1]][[i]] ) 

x2_adj_hl = lapply(1:n_tresh, function(i)
  M_x1_hl[[i]] %*%  cbind(spiq_rates[[1]][[i]],  spin_rates[[1]][[i]])  ) 

partial_step_FWL_hl = lapply(1:n_tresh, function(i) 
  lm(y_adj_hl[[i]] ~ x2_adj_hl[[i]] ))


res_FWL_hl = process_summary(partial_step_FWL_hl, 
                             n_tresh, freq_vector, freq_tresh_plot,
                             freq_tresh_plot,
                             hl_classes)

remove_hl_classes <- function(df) {
  df <- df[, !(names(df) %in% 'hl_classes'), drop = FALSE]
  return(df)
}


res_FWL_hl <- lapply(res_FWL_hl, remove_hl_classes)

melt_est_FWL_spiq_hl = melt(res_FWL_hl$est_pstep2_spiq_hl,
                            id.vars = "SPIQ_tresh")
melt_p_FWL_spiq_hl = melt(res_FWL_hl$p_pstep2_spiq_hl,
                          id.vars = "SPIQ_tresh")
melt_FWL_spiq_hl = melt_est_FWL_spiq_hl
melt_FWL_spiq_hl$color = melt_p_FWL_spiq_hl$value

melt_FWL_spiq_hl$color2 <- ifelse(melt_FWL_spiq_hl$color < 0.01, "cyan",
                                  ifelse(melt_FWL_spiq_hl$color <= 0.05,
                                         "white", "blue"))
melt_FWL_spiq_hl$color2 <- factor(melt_FWL_spiq_hl$color2, 
                                  levels = c("cyan", "white", "blue"))
melt_FWL_spiq_hl$color3 <- thresh_colors[match(melt_FWL_spiq_hl$SPIQ_tresh, 
                                               c("20%", "40%", "60%",
                                                 "80%", "90%"))]

################
# Latex Output #
################

latex_tbl_SPIQ = bind_cols(round(res_FWL_hl[[1]][-c(12:13)],3), 
                           round(res_FWL_hl[[3]][-c(12:13)],3) ) %>% 
  dplyr::select(all_of(c(matrix(names(.), ncol = 11, byrow = TRUE))))

kable(latex_tbl_SPIQ, format = 'latex')

latex_tbl_SPIN = bind_cols(round(res_FWL_hl[[2]][-c(12:13)],3), 
                           round(res_FWL_hl[[4]][-c(12:13)],3) ) %>% 
  dplyr::select(all_of(c(matrix(names(.), ncol = 11, byrow = TRUE))))

kable(latex_tbl_SPIN, format = 'latex')


############
# heatmaps #
############

# Create a new column to store the final fill color
melt_FWL_spiq_hl$final_fill_color <- ifelse(melt_FWL_spiq_hl$color2 %in%
                                              c("white", "cyan"), 
                                            "white",  
                                            melt_FWL_spiq_hl$color3)

# Set alpha value: Significant values are fully opaque, others have alpha 0.7
melt_FWL_spiq_hl$alpha_value <- ifelse(melt_FWL_spiq_hl$final_fill_color == "white",
                                       1, 0.7)


significance_colors <- c("Significant at 0.01" = "cyan",
                         "Significant at 0.05" = "white")


# Define colors for the plot
quantile_colors <- c("Significant at 0.05" = "white",   
                     "Non-Significant (20%)" = "#F0E8E0",    
                     "Non-Significant (40%)" = "#FFDDC1", 
                     "Non-Significant (60%)" = "#FF8C69", 
                     "Non-Significant (80%)" = "#FF6347", 
                     "Non-Significant (90%)" = "#FF0000")

# Create a new column for descriptive color labels
melt_FWL_spiq_hl$color_label <- ifelse(melt_FWL_spiq_hl$final_fill_color == "white", 
                                       "Significant at 0.05", 
                                       ifelse(melt_FWL_spiq_hl$final_fill_color == "#F0E8E0", "Non-Significant (20%)",
                                              ifelse(melt_FWL_spiq_hl$final_fill_color == "#FFDDC1", "Non-Significant (40%)",
                                                     ifelse(melt_FWL_spiq_hl$final_fill_color == "#FF8C69", "Non-Significant (60%)",
                                                            ifelse(melt_FWL_spiq_hl$final_fill_color == "#FF6347", "Non-Significant (80%)",
                                                                   ifelse(melt_FWL_spiq_hl$final_fill_color == "#FF0000", "Non-Significant (90%)",
                                                                          "Non-Significant"))))))


# Combine both vectors into a named list
all_colors <- c(significance_colors, quantile_colors)

all_levels =  c(
  "Significant at 0.05",
  "Non-Significant (20%)",
  "Non-Significant (40%)",
  "Non-Significant (60%)",
  "Non-Significant (80%)",
  "Non-Significant (90%)"
)


melt_FWL_spiq_hl$color_label <- factor(melt_FWL_spiq_hl$color_label, levels = all_levels)

melt_FWL_spiq_hl$final_fill_color <- factor(melt_FWL_spiq_hl$final_fill_color,
                                            levels = c("white", "#F0E8E0", 
                                                       "#FFDDC1", "#FF8C69",
                                                       "#FF6347", "#FF0000"))


##############################
#WITH RESPECT TO HEARING LOSS#
##############################

melt_FWL_spiq_hl$new_color_label <- ifelse(grepl("Non-Significant", 
                                                 melt_FWL_spiq_hl$color_label),
                                           "Not Significant", 
                                           "Significant at 0.05")

melt_FWL_spiq_hl$new_color_label <- factor(melt_FWL_spiq_hl$new_color_label,
                                           levels = c("Significant at 0.05", 
                                                      "Not Significant"))

gg4_overall = ggplot(melt_FWL_spiq_hl, aes(x = as.factor(variable), 
                                           y = as.factor(SPIQ_tresh), 
                                           fill = new_color_label,  # Use the new_color_label for fill
                                           label = round(value, 2),
                                           alpha = alpha_value)) +
  geom_tile(color = "grey50", size = 0.5, show.legend = TRUE) +  # Add borders around tiles
  geom_text(vjust = 1, color = "black", size = 3) +  # Add the text for values
  scale_fill_manual(values = c("Significant at 0.05" = "white",
                               "Not Significant" = "red"),  # Only two colors
                    drop = FALSE) +  # Do not drop unused factor levels
  scale_alpha_continuous(guide = 'none', range = c(0.7, 1)) +  # Use alpha without legend
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white"),  # Set plot background to white
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 0.9),  # Adjust x-axis text for better alignment
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    plot.margin = margin(0.1, 0.1, 0.1, 0.1),
    plot.title = element_text(size = 21)
  ) +
  labs(fill = "Estimate Significance",  
       x = "Frequencies",  
       y = "Probabilistic Level",
       title = expression(gamma[(Q)]))  +
  guides(
    fill = guide_legend(
      title = "Estimate Significance",  # Legend title
      override.aes = list(alpha = 1),  # Ensure alpha is shown in legend
      ncol = 2  # Number of columns in the legend
    )
  )

gg4_overall

melt_est_FWL_spin_hl = melt(res_FWL_hl$est_pstep2_spin_hl, id.vars = "SPIN_tresh")
melt_p_FWL_spin_hl = melt(res_FWL_hl$p_pstep2_spin_hl, id.vars = "SPIN_tresh")
melt_FWL_spin_hl = melt_est_FWL_spin_hl
melt_FWL_spin_hl$color = melt_p_FWL_spin_hl$value

melt_FWL_spin_hl$color2 <- ifelse(melt_FWL_spin_hl$color < 0.01, "cyan",
                                  ifelse(melt_FWL_spin_hl$color <= 0.05, "white", "blue"))
melt_FWL_spin_hl$color2 <- factor(melt_FWL_spin_hl$color2, levels = c("cyan", "white", "blue"))
melt_FWL_spin_hl$color3 <- thresh_colors[match(melt_FWL_spin_hl$SPIN_tresh, c("20%", "40%", "60%", "80%", "90%"))]


melt_FWL_spin_hl$color2 <- as.character(melt_FWL_spin_hl$color2)

# Create a new column to store the final fill color
melt_FWL_spin_hl$final_fill_color <- ifelse(melt_FWL_spin_hl$color2 %in% c("white", "cyan"), 
                                            "white",  # All significant values will be white
                                            melt_FWL_spin_hl$color3)

# Set alpha value: Significant values are fully opaque, others have alpha 0.7
melt_FWL_spin_hl$alpha_value <- ifelse(melt_FWL_spin_hl$final_fill_color == "white", 1, 0.7)



# Combine both vectors into a named list
all_colors <- c(significance_colors, quantile_colors)

# Create a new column for descriptive color labels
melt_FWL_spin_hl$color_label <- ifelse(melt_FWL_spin_hl$final_fill_color == "white", "Significant at 0.05",
                                       ifelse(melt_FWL_spin_hl$final_fill_color == "#F0E8E0", "Non-Significant (20%)",
                                              ifelse(melt_FWL_spin_hl$final_fill_color == "#FFDDC1", "Non-Significant (40%)",
                                                     ifelse(melt_FWL_spin_hl$final_fill_color == "#FF8C69", "Non-Significant (60%)",
                                                            ifelse(melt_FWL_spin_hl$final_fill_color == "#FF6347", "Non-Significant (80%)",
                                                                   ifelse(melt_FWL_spin_hl$final_fill_color == "#FF0000", "Non-Significant (90%)",
                                                                          "Non-Significant"))))))

melt_FWL_spin_hl$color_label <- factor(melt_FWL_spin_hl$color_label, levels = all_levels)

melt_FWL_spin_hl$final_fill_color <- factor(melt_FWL_spin_hl$final_fill_color,
                                            levels = c("white", "#F0E8E0", 
                                                       "#FFDDC1", "#FF8C69",
                                                       "#FF6347", "#FF0000"))


melt_FWL_spin_hl$new_color_label <- ifelse(grepl("Non-Significant", 
                                                 melt_FWL_spin_hl$color_label),
                                           "Not Significant", 
                                           "Significant at 0.05")

melt_FWL_spin_hl$new_color_label <- factor(melt_FWL_spin_hl$new_color_label,
                                           levels = c("Significant at 0.05", 
                                                      "Not Significant"))


gg5_overall = ggplot(melt_FWL_spin_hl, aes(x = as.factor(variable), 
                                           y = as.factor(SPIN_tresh), 
                                           fill = new_color_label,  # Use the new_color_label for fill
                                           label = round(value, 2),
                                           alpha = alpha_value)) +
  geom_tile(color = "grey50", size = 0.5, show.legend = TRUE) +  # Add borders around tiles
  geom_text(vjust = 1, color = "black", size = 3) +  # Add the text for values
  scale_fill_manual(values = c("Significant at 0.05" = "white",
                               "Not Significant" = "red"),  # Only two colors
                    drop = FALSE) +  # Do not drop unused factor levels
  scale_alpha_continuous(guide = 'none', range = c(0.7, 1)) +  # Use alpha without legend
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white"),  # Set plot background to white
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 0.9),  # Adjust x-axis text for better alignment
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    plot.margin = margin(0.1, 0.1, 0.1, 0.1),
    plot.title = element_text(size = 21)
  ) +
  labs(fill = "Estimate Significance",  
       x = "Frequencies",  
       y = "Probabilistic Level",
       title = expression(gamma[(N)]))  +
  guides(
    fill = guide_legend(
      title = "Estimate Significance",  # Legend title
      override.aes = list(alpha = 1),  # Ensure alpha is shown in legend
      ncol = 2  # Number of columns in the legend
    )
  )




###################################
# Model by degree of hearing loss # 
###################################

x1_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    cbind(as.numeric(model_by_age_hl[[i]][[j]]$ax), as.numeric(model_by_age_hl[[i]][[j]]$bx))  ))


H_x1_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j) 
    x1_age_hl[[i]][[j]] %*% ginv(t(x1_age_hl[[i]][[j]]) %*% x1_age_hl[[i]][[j]]) %*% t(x1_age_hl[[i]][[j]])  ))


M_x1_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)  diag(n_age) - H_x1_age_hl[[i]][[j]] ))


y_adj_age_hl = lapply(1:n_tresh, function(i) 
  lapply(1:n_hlclasses, function(j) 
    M_x1_age_hl[[i]][[j]] %*% hr_rates_interp[[2]][[i]][[j]] )) 

x2_adj_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j) 
    M_x1_age_hl[[i]][[j]] %*%  cbind(spiq_rates[[2]][[i]][[j]], spin_rates[[2]][[i]][[j]])  )) 


partial_step_FWL_age_hl = lapply(1:n_tresh, function(i) 
  lapply(1:n_hlclasses, function(j) 
    lm(y_adj_age_hl[[i]][[j]] ~ x2_adj_age_hl[[i]][[j]] )) )


res_FWL_age_hl = lapply(1:n_tresh, function(i)
  process_summary(partial_step_FWL_age_hl[[i]],
                  n_hlclasses, freq_vector,
                  freq_tresh_plot[i],
                  freq_tresh_plot[i],
                  hl_classes))

res_FWL_age_hl2 = lapply(1:n_hlclasses, function(i)
  list( est_pstep2_spiq_hl = rbind(res_FWL_age_hl[[1]][[1]][i,],
                                   res_FWL_age_hl[[2]][[1]][i,],
                                   res_FWL_age_hl[[3]][[1]][i,],
                                   res_FWL_age_hl[[4]][[1]][i,],
                                   res_FWL_age_hl[[5]][[1]][i,]),
        
        est_pstep2_spin_hl = rbind(res_FWL_age_hl[[1]][[2]][i,],
                                   res_FWL_age_hl[[2]][[2]][i,],
                                   res_FWL_age_hl[[3]][[2]][i,],
                                   res_FWL_age_hl[[4]][[2]][i,],
                                   res_FWL_age_hl[[5]][[2]][i,]),
        
        p_pstep2_spiq_hl =  rbind(res_FWL_age_hl[[1]][[3]][i,],
                                  res_FWL_age_hl[[2]][[3]][i,],
                                  res_FWL_age_hl[[3]][[3]][i,],
                                  res_FWL_age_hl[[4]][[3]][i,],
                                  res_FWL_age_hl[[5]][[3]][i,]),
        
        p_pstep2_spin_hl = rbind(res_FWL_age_hl[[1]][[4]][i,],
                                 res_FWL_age_hl[[2]][[4]][i,],
                                 res_FWL_age_hl[[3]][[4]][i,],
                                 res_FWL_age_hl[[4]][[4]][i,],
                                 res_FWL_age_hl[[5]][[4]][i,]) ) )



num_titles <- 5

# Define Q as a character
Q <- "Q"
N <- "N"


# Create a vector of titles with indexed subscripts
titles_spiq <- sapply(1:num_titles, function(i) {
  # Create expression using bquote
  bquote(gamma[.(Q)]^{h[.(i)]})
}, USE.NAMES = FALSE)



titles_spin <- sapply(1:num_titles, function(i) {
  # Create expression using bquote
  bquote(gamma[.(N)]^{h[.(i)]})
}, USE.NAMES = FALSE)



# Create plots for each class
plots_list_spiq_FWL <- lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list(res_FWL_age_hl2[[i]], 
                 variable_type = "SPIQ",
                 title = titles_spiq[[i]],
                 variable_type2 = 'hl')
})


plots_list_spin_FWL <- lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list(res_FWL_age_hl2[[i]], 
                 variable_type = "SPIN",
                 title = titles_spin[[i]],
                 variable_type2 = 'hl')
})

gg6 = grid.arrange(grobs = plots_list_spiq_FWL, ncol = 3)
gg6

ggsave(filename = paste0(mydir_figs,"part_reg_spiq_age_hl", ".pdf"), 
       plot = gg6,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)

gg7 = grid.arrange(grobs = plots_list_spin_FWL, ncol = 3)
gg7

ggsave(filename = paste0(mydir_figs,"part_reg_spin_age_hl", ".pdf"), 
       plot = gg7,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


################
# Latex Output #
################


spiq_latex = lapply(1:n_hlclasses, function(i)  bind_cols(round(res_FWL_age_hl2[[i]][[1]][-c(12:13)],3), 
                                                          round(res_FWL_age_hl2[[i]][[3]][-c(12:13)],3) ) %>% 
                      dplyr::select(all_of(c(matrix(names(.), ncol = 11, byrow = TRUE)))) )

for (i in 1:n_hlclasses) { spiq_latex[[i]] = as.data.frame(spiq_latex[[i]]) }
for (i in 1:n_hlclasses) { spiq_latex[[i]]$SPIQ_treshold = treshold_SPIQ }

spiq_latex2 = do.call(rbind, spiq_latex)


kable(spiq_latex2, format = 'latex')

spin_latex = spiq_latex = lapply(1:n_hlclasses, function(i)  bind_cols(round(res_FWL_age_hl2[[i]][[2]][-c(12:13)],3), 
                                                                       round(res_FWL_age_hl2[[i]][[4]][-c(12:13)],3) ) %>% 
                                   dplyr::select(all_of(c(matrix(names(.), ncol = 11, byrow = TRUE)))) )


for (i in 1:n_hlclasses) { spin_latex[[i]] = as.data.frame(spin_latex[[i]]) }
for (i in 1:n_hlclasses) { spin_latex[[i]]$SPIB_treshold = treshold_SPIN }

spin_latex2 = do.call(rbind, spin_latex)


kable(spin_latex2, format = 'latex')


##################
# PLOTs combined #
##################

#############################
# BY Degree of Hearing Loss #
#############################

plots_list_spiq_FWL_2 <- lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list2(res_FWL_age_hl2[[i]],
                  variable_type = "SPIQ", 
                  title = titles_spiq[[i]],
                  non_significant_color = hearing_loss_col[i],
                  variable_type2 = "hl")
})

plots_list_spin_FWL_2 <- lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list2(res_FWL_age_hl2[[i]], 
                  variable_type = "SPIN",
                  title = titles_spin[[i]],
                  non_significant_color = hearing_loss_col[i],
                  variable_type2 = "hl")
})




combined_part_0 <- gg4_overall + gg5_overall +
  plot_layout(guides = 'collect',
              widths = c(1, 1)) & theme(legend.position = 'bottom')


# Define the background color
bg_color <- "#f0f0f0"  # Super light gray


ggsave(filename = paste0(mydir_figs,"part_reg_all", ".pdf"), 
       plot = combined_part_0,
       width = 13,
       height = 4,
       device = "pdf",
       units = "in", dpi = 300,
       bg = bg_color)



combined_part_1 <- plots_list_spiq_FWL_2[[1]] +plots_list_spin_FWL_2[[1]] + 
  plot_layout(guides = 'collect',
              widths = c(1, 1)) & theme(legend.position = 'bottom')

combined_part_2 <- plots_list_spiq_FWL_2[[2]] +plots_list_spin_FWL_2[[2]] + 
  plot_layout(guides = 'collect',
              widths = c(1, 1)) & theme(legend.position = 'bottom')

combined_part_3 <- plots_list_spiq_FWL_2[[3]] +plots_list_spin_FWL_2[[3]] + 
  plot_layout(guides = 'collect',
              widths = c(1, 1)) & theme(legend.position = 'bottom')

combined_part_4 <- plots_list_spiq_FWL_2[[4]] +plots_list_spin_FWL_2[[4]] + 
  plot_layout(guides = 'collect',
              widths = c(1, 1)) & theme(legend.position = 'bottom')

combined_part_5 <- plots_list_spiq_FWL_2[[5]] +plots_list_spin_FWL_2[[5]] + 
  plot_layout(guides = 'collect',
              widths = c(1, 1)) & theme(legend.position = 'bottom')

# fix the distance between the 2 cols
combined_partial <- (combined_part_1 / combined_part_2 / combined_part_3/ combined_part_4 / combined_part_5) + 
  plot_layout(heights = c(1, 1, 1, 1, 1)) 


# Define the background color
bg_color <- "#f0f0f0"  # Super light gray


ggsave(filename = paste0(mydir_figs,"part_reg_all_hl", ".pdf"), 
       plot = combined_partial,
       width = 13,
       height = 18,
       device = "pdf",
       units = "in", dpi = 300,
       bg = bg_color)



prova <- lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list3(res_FWL_age_hl2[[i]],
                  variable_type = "SPIQ", 
                  title = titles_spiq[[i]],
                  non_significant_color = hearing_loss_col[i],
                  variable_type2 = "hl")
})

prova <- lapply(1:length(prova), function(i) {
  prova[[i]][[2]]$HL <- hl_classes[i]
  return(prova[[i]])
})


prova2 = rbind(prova[[1]][[2]], prova[[2]][[2]],
               prova[[3]][[2]], prova[[4]][[2]],
               prova[[5]][[2]])

prova2$HL <- factor(prova2$HL, levels = hl_classes)

ggplot(prova2, aes(x = variable, 
                   y = value)) +
  geom_boxplot(color = "black") +  # Black boxplot
  geom_point(aes(color = color_label,  # Border color based on color_label
                 fill = SPIQ_tresh),   # Fill color based on SPIQ_tresh
             size = 2, 
             shape = 21,  # Use a shape that supports both fill and border
             stroke = 1,  # Width of the contour
             position = position_jitter(width = 0.2)) +  # Jitter the points slightly
  labs(x = "Terms", 
       y = "Estimates") + 
  facet_wrap(~HL, scales = "free") +  # Facet by HL levels
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +
  scale_color_manual(values = c("Significant at 0.05" = "red", 
                                "Non-Significant" = "blue"))


ggplot(prova2, aes(x = variable, 
                   y = value)) +
  #geom_boxplot(color = "black") +  # Black boxplot
  geom_point(aes(color = color_label),   # Fill color based on SPIQ_tresh
             size = 2, 
             shape = 21,  # Use a shape that supports both fill and border
             stroke = 1,  # Width of the contour
             position = position_jitter(width = 0.2)) +  # Jitter the points slightly
  labs(x = "Terms", 
       y = "Estimates") + 
  facet_grid(SPIQ_tresh~HL, scales = "free") +  # Facet by HL levels
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +
  scale_color_manual(values = c("Significant at 0.05" = "red", 
                                "Non-Significant" = "blue"))




prova <- lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list3(res_FWL_age_hl2[[i]],
                  variable_type = "SPIN", 
                  title = titles_spiq[[i]],
                  non_significant_color = hearing_loss_col[i],
                  variable_type2 = "hl")
})

prova <- lapply(1:length(prova), function(i) {
  prova[[i]][[2]]$HL <- hl_classes[i]
  return(prova[[i]])
})


prova2 = rbind(prova[[1]][[2]], prova[[2]][[2]],
               prova[[3]][[2]], prova[[4]][[2]],
               prova[[5]][[2]])

prova2$HL <- factor(prova2$HL, levels = hl_classes)

ggplot(prova2, aes(x = variable, 
                   y = value)) +
  geom_boxplot(color = "black") +  # Black boxplot
  geom_point(aes(color = color_label,  # Border color based on color_label
                 fill = SPIN_tresh),   # Fill color based on SPIN_tresh
             size = 2, 
             shape = 21,  # Use a shape that supports both fill and border
             stroke = 1,  # Width of the contour
             position = position_jitter(width = 0.2)) +  # Jitter the points slightly
  labs(x = "Terms", 
       y = "Estimates") + 
  facet_wrap(~HL, scales = "free") +  # Facet by HL levels
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +
  scale_color_manual(values = c("Significant at 0.05" = "red", 
                                "Non-Significant" = "blue"))


ggplot(prova2, aes(x = variable, 
                   y = value)) +
  #geom_boxplot(color = "black") +  # Black boxplot
  geom_point(aes(color = color_label),   # Fill color based on SPIN_tresh
             size = 2, 
             shape = 21,  # Use a shape that supports both fill and border
             stroke = 1,  # Width of the contour
             position = position_jitter(width = 0.2)) +  # Jitter the points slightly
  labs(x = "Terms", 
       y = "Estimates") + 
  facet_grid(SPIN_tresh~HL, scales = "free") +  # Facet by HL levels
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +
  scale_color_manual(values = c("Significant at 0.05" = "red", 
                                "Non-Significant" = "blue"))



################
# Model by SEX # 
################


x1_age_sex = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j)
    cbind(as.numeric(model_by_age_sex[[i]][[j]]$ax), 
          as.numeric(model_by_age_sex[[i]][[j]]$bx))  ))


H_x1_age_sex = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j) 
    x1_age_sex[[i]][[j]] %*% ginv(t(x1_age_sex[[i]][[j]]) %*% x1_age_sex[[i]][[j]]) %*% t(x1_age_sex[[i]][[j]])  ))


M_x1_age_sex = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j)  diag(n_age) - H_x1_age_sex[[i]][[j]] ))


y_adj_age_sex = lapply(1:n_tresh, function(i) 
  lapply(1:2, function(j) 
    M_x1_age_sex[[i]][[j]] %*% hr_rates_interp[[3]][[i]][[j]] )) 

x2_adj_age_sex = lapply(1:n_tresh, function(i)
  lapply(1:2, function(j) 
    M_x1_age_sex[[i]][[j]] %*%  cbind(spiq_rates[[3]][[i]][[j]], spin_rates[[3]][[i]][[j]])  )) 


partial_step_FWL_age_sex = lapply(1:n_tresh, function(i) 
  lapply(1:2, function(j) 
    lm(y_adj_age_sex[[i]][[j]] ~ x2_adj_age_sex[[i]][[j]] )) )


res_FWL_age_sex = lapply(1:n_tresh, function(i)
  process_summary(partial_step_FWL_age_sex[[i]],
                  2, freq_vector,
                  freq_tresh_plot[i], 
                  freq_tresh_plot[i], 
                  c("Female", "Male")))


res_FWL_age_sex2 = lapply(1:2, function(i)
  list( est_pstep2_spiq_sex = rbind(res_FWL_age_sex[[1]][[1]][i,],
                                    res_FWL_age_sex[[2]][[1]][i,],
                                    res_FWL_age_sex[[3]][[1]][i,],
                                    res_FWL_age_sex[[4]][[1]][i,],
                                    res_FWL_age_sex[[5]][[1]][i,]),
        
        est_pstep2_spin_sex = rbind(res_FWL_age_sex[[1]][[2]][i,],
                                    res_FWL_age_sex[[2]][[2]][i,],
                                    res_FWL_age_sex[[3]][[2]][i,],
                                    res_FWL_age_sex[[4]][[2]][i,],
                                    res_FWL_age_sex[[5]][[2]][i,]),
        
        p_pstep2_spiq_sex =  rbind(res_FWL_age_sex[[1]][[3]][i,],
                                   res_FWL_age_sex[[2]][[3]][i,],
                                   res_FWL_age_sex[[3]][[3]][i,],
                                   res_FWL_age_sex[[4]][[3]][i,],
                                   res_FWL_age_sex[[5]][[3]][i,]),
        
        p_pstep2_spin_sex = rbind(res_FWL_age_sex[[1]][[4]][i,],
                                  res_FWL_age_sex[[2]][[4]][i,],
                                  res_FWL_age_sex[[3]][[4]][i,],
                                  res_FWL_age_sex[[4]][[4]][i,],
                                  res_FWL_age_sex[[5]][[4]][i,]) ) )



num_titles <- 2

# Define Q as a character
Q <- "Q"
N <- "N"


# Create a vector of titles with indexed subscripts
titles_spiq <- sapply(1:num_titles, function(i) {
  # Create expression using bquote
  bquote(gamma[.(Q)]^{s[.(i)]})
}, USE.NAMES = FALSE)



titles_spin <- sapply(1:num_titles, function(i) {
  # Create expression using bquote
  bquote(gamma[.(N)]^{s[.(i)]})
}, USE.NAMES = FALSE)


# Create plots for each class
plots_list_spiq_FWL_sex <- lapply(seq_along(res_FWL_age_sex2), function(i) {
  plot_from_list(res_FWL_age_sex2[[i]], 
                 variable_type = "SPIQ", 
                 title = titles_spiq[[i]],
                 variable_type2 = "sex")
})


plots_list_spin_FWL_sex <- lapply(seq_along(res_FWL_age_sex2), function(i) {
  plot_from_list(res_FWL_age_sex2[[i]], 
                 variable_type = "SPIN", 
                 title = titles_spin[[i]],
                 variable_type2 = "sex")
})

gg6_sex = grid.arrange(grobs = plots_list_spiq_FWL_sex, ncol = 2)
gg6_sex

ggsave(filename = paste0(mydir_figs,"part_reg_spiq_age_sex", ".pdf"), 
       plot = gg6_sex,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)

gg7_sex = grid.arrange(grobs = plots_list_spin_FWL_sex, ncol = 2)
gg7_sex

ggsave(filename = paste0(mydir_figs,"part_reg_spin_age_sex", ".pdf"), 
       plot = gg7_sex,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


##########
# BY Sex #
##########


plots_list_spiq_FWL_2_sex <- lapply(seq_along(res_FWL_age_sex2), function(i) {
  plot_from_list2(res_FWL_age_sex2[[i]],
                  variable_type = "SPIQ", 
                  title = titles_spiq[[i]],
                  non_significant_color = c("pink","blue")[i],
                  variable_type2 = "sex")
})

plots_list_spin_FWL_2_sex <- lapply(seq_along(res_FWL_age_sex2), function(i) {
  plot_from_list2(res_FWL_age_sex2[[i]], 
                  variable_type = "SPIN",
                  title = titles_spin[[i]],
                  non_significant_color = c("pink","blue")[i],
                  variable_type2 = "sex")
})




combined_part_1_sex <- plots_list_spiq_FWL_2_sex[[1]] + plots_list_spin_FWL_2_sex[[1]] + 
  plot_layout(guides = 'collect',
              widths = c(1, 1)) & theme(legend.position = 'bottom')

combined_part_2_sex <- plots_list_spiq_FWL_2_sex[[2]] +plots_list_spin_FWL_2_sex[[2]] + 
  plot_layout(guides = 'collect',
              widths = c(1, 1)) & theme(legend.position = 'bottom')

# fix the distance between the 2 cols
combined_partial_sex <- (combined_part_1_sex / combined_part_2_sex) + 
  plot_layout(heights = c(1, 1)) 


# Define the background color
bg_color <- "#f0f0f0"  # Super light gray


ggsave(filename = paste0(mydir_figs,"part_reg_all_sex", ".pdf"), 
       plot = combined_partial_sex,
       width = 13,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300,
       bg = bg_color)


