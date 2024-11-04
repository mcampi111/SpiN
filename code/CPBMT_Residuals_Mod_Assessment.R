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

mydir<- "./data/"
mydir2<- "./code/"

mydir_figs = "./code/figs/"


source(paste(mydir2, "utils.R", sep = "")) 


####################
# DATA PREPARATION #
####################

data_ampl2 = readRDS( file= paste(mydir, "data_ampl2bis.rds", sep = ""))
data_ampl2 <- data_ampl2[data_ampl2$AGE <= 89, ]
data_ampl2 <- data_ampl2[data_ampl2$AGE >= 45, ]

data_ampl2$age_group2 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 5), 
                             labels = FALSE,
                             include.lowest = TRUE)
data_ampl2$age_group2 <- factor(paste0((data_ampl2$age_group2 - 1) * 5, "-", 
                                       (data_ampl2$age_group2) * 5))

data_ampl2$age_group3 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 1), 
                             labels = FALSE,
                             include.lowest = TRUE)
data_ampl2$age_group3 <- factor(paste0((data_ampl2$age_group3 - 1) , "-", 
                                       (data_ampl2$age_group3) ))


#final data for analysis
data_lee_carter = data_ampl2[,c(3,4,7:17,34,35,36,29)] #Left ear
data_lee_carter$SEXE = as.factor(data_lee_carter$SEXE)

spiq =  data_ampl2[,c(3,4,30,34,35,36,29)]
spin =  data_ampl2[,c(3,4,31,34,35,36,29)]

PTA <- data_ampl2$PTA
PTA_df = data.frame(PTA)

SRT <- data_ampl2$SRT
SRT_df = data.frame(SRT)

SNR <- data_ampl2$SNR
SNR_df = data.frame(SNR)

##############
# PARAMETERS #
##############

hl_classes = levels(data_ampl2$Classification) # for age_group3 levels(data_ampl2$Classification)[-5]
n_hlclasses = length(hl_classes)

freq_vector = sapply(1:11, function(i) str_split(colnames(data_lee_carter)[2+i], "_")[[1]][2])
n_freq = length(freq_vector)

age_groups <- levels(data_lee_carter$age_group) #[-c(1,2,3,4,10:12)] #stability results
n_age_g = length(age_groups)

age_groups2 <- levels(data_lee_carter$age_group2)
n_age_g2 = length(age_groups2)

all_age_groups = levels(data_lee_carter$age_group3)
n_age_g3 = length(all_age_groups)


####################
# RATES THRESHOLDS #
####################

decision_threshold = as.data.frame(cbind(PTA,SRT,SNR))

quant_tresh <- c(0.2, 0.4, 0.6, 0.8, 0.9)


quant_PTA <- quantile(decision_threshold$PTA, probs = quant_tresh, na.rm = TRUE) 
quant_SRT <- quantile(decision_threshold$SRT, probs = quant_tresh, na.rm = TRUE) 
quant_SNR <- quantile(decision_threshold$SNR, probs = quant_tresh, na.rm = TRUE) 

tres_pta_srt = data.frame(PTA = quant_PTA, SRT = quant_SRT)
tres_pta_snr = data.frame(PTA = quant_PTA, SNR = quant_SNR)
tres_srt_snr = data.frame(SRT = quant_SRT, SNR = quant_SNR)

tres_pta_srt2 <- data.frame(X = as.numeric(tres_pta_srt[,1]), 
                            Y = as.numeric(tres_pta_srt[,2]))  
tres_pta_snr2 <- data.frame(X = as.numeric(tres_pta_snr[,1]), 
                            Y = as.numeric(tres_pta_snr[,2]))  
tres_srt_snr2 <- data.frame(X = as.numeric(tres_srt_snr[,1]), 
                            Y = as.numeric(tres_srt_snr[,2]))  


treshold_hearing_rate = as.integer(tres_pta_snr[,1])
n_tresh = length(treshold_hearing_rate)

treshold_SPIQ = as.integer(tres_pta_srt[,2])
treshold_SPIN = as.integer(tres_pta_snr[,2])


freq_tresh = do.call(rbind,lapply(data_ampl2[,7:17], quantile, quant_tresh, 2))

freq_tresh_plot = c( paste(20,"%", sep = ""), paste(40,"%", sep = ""),
                     paste(60,"%", sep = ""), paste(80,"%", sep = ""),
                     paste(90,"%", sep = "")) 


thresh_colors = c("#F5F5F5", "#FFDDC1", "#FF8C69", "#FF6347", "#FF0000")
hearing_loss_col = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529") 



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


############################
############################
# STEP 1: STRUCTURED MODEL #
############################
############################

###############
# CPBMT model #
###############


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



######################
# RESIDUALS ANALYSIS #
######################

combined_data <- lapply(1:n_tresh, function(i)
  data.frame(Fitted_Values =  melt(model_hl[[i]]$fitted$y)$value,
             Residuals = melt(model_hl[[i]]$residuals$y)$value, 
             Thresholds = freq_tresh_plot[i] ) )

combined_data = do.call(rbind, combined_data)

gg_resfit = ggplot(combined_data, aes(x = Fitted_Values,
                                      y = Residuals,
                                      color = as.factor(Thresholds) )) +
  geom_point() +
  scale_color_manual(values = thresh_colors) +
  theme_bw() +
  labs(x = "Predicted Values",
       y = "Residuals",
       title = "Residuals vs. Predicted Values",
       color = "Quantile") +
  facet_wrap(~ Thresholds, nrow = 2) +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))

gg_resfit


ggsave(filename = paste0(mydir_figs,"res_fitted_age", ".pdf"), 
       plot = gg_resfit,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


heterosked_tests = lapply(1:n_tresh,function(i)
  bptest(model_hl[[i]]$residuals$y ~ model_hl[[i]]$fitted$y))

lapply(1:n_tresh,function(i) round(heterosked_tests[[i]]$p.value, 3))

################################################################################
# Test for Autocorrelation between the Residuals of the model across Frequency #
################################################################################

#Box-Pierce (Ljung-Box) Test - fixed lag = 4
boxpierce_Lb_hl = do.call(cbind,lapply(1:n_tresh, function(j)
  round(sapply(1:n_age,function(i) 
    Box.test(model_hl[[j]]$residuals$y[i,], 
             lag = 4, type = "Ljung-Box")$p.value),3) ))

boxpierce_Lb_hl = as.data.frame(boxpierce_Lb_hl)
colnames(boxpierce_Lb_hl) = freq_tresh_plot#c(0.2, 0.4, 0.6, 0.8, 0.9)
boxpierce_Lb_hl$Age = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1]))
boxpierce_Lb_hl = boxpierce_Lb_hl[, c(ncol(boxpierce_Lb_hl), 1:(ncol(boxpierce_Lb_hl)-1))]


#Breusch-Godfrey Test (also known as Breusch-Pagan Test or Breusch-Godfrey LM Test) - fixed lag = 4
do.call(cbind, lapply(1:n_tresh, function(j)
  round(bgtest(as.vector(model_hl[[j]]$residuals$y)  ~ 1, 
               order = 4)$p.value ,3) ))  

# Augmented Dickey-Fuller (ADF) Test - fixed lag = 4 --> TOASK here
adf_hl = do.call(cbind,lapply(1:n_tresh, function(j)
  round(sapply(1:n_age,function(i)
    adf.test(model_hl[[j]]$residuals$y[i,])$p.value),3) ))

adf_hl = as.data.frame(adf_hl)
colnames(adf_hl) = freq_tresh_plot#c(0.2, 0.4, 0.6, 0.8, 0.9)
adf_hl$Age = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1]))
adf_hl = adf_hl[, c(ncol(adf_hl), 1:(ncol(adf_hl)-1))]


kable(cbind(boxpierce_Lb_hl,adf_hl[,-1]), format = "latex")


melt_box_tests_hl = melt(boxpierce_Lb_hl, id.vars = "Age")
melt_adf_hl = melt(adf_hl, id.vars = "Age")


gg_box_tests_hl = ggplot(melt_box_tests_hl, aes(x = as.factor(Age), 
                                                y = value,
                                                color = variable )) +
  geom_boxplot(alpha = 0.5,color = "black") +
  scale_color_manual(values = thresh_colors) + 
  geom_point(size = 2,
             aes(color =  as.factor(variable) ))  +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = paste("Age"), 
       y = "P-value Box-Pierce", 
       color = "Quantile") +
  theme_bw()  +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom"
  ) 


gg_box_tests_hl

ggsave(filename = paste0(mydir_figs,"box_test_hl", ".pdf"), 
       plot = gg_box_tests_hl,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)



gg_adf_tests_hl = ggplot(melt_adf_hl, aes(x = as.factor(Age), 
                                          y = value,
                                          color = variable )) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  geom_point(size = 2,
             aes(color =  as.factor(variable) ))  +
  scale_color_manual(values = thresh_colors) +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = paste("Age"), 
       y = "P-value Augmented Dickey-Fuller", 
       color = "Quantile") +
  theme_bw()  +
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom"
  ) 


gg_adf_tests_hl

ggsave(filename = paste0(mydir_figs,"adf_test_hl", ".pdf"), 
       plot = gg_adf_tests_hl,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


melt_box_tests_hl <- melt_box_tests_hl %>%
  mutate(threshold_group = ifelse(value > 0.05, "Above_Significance", "Below_Significance"))

melt_box_hl_count = melt_box_tests_hl %>%
  group_by(threshold_group, variable, Age) %>%
  summarise(count = n(), proportion = n() / nrow(melt_box_tests_hl))


melt_box_hl_count = na.omit(melt_box_hl_count)

gg_prop_box_hl = ggplot(melt_box_hl_count, aes(x = Age, y = proportion,
                                               fill = as.factor(variable))) +
  geom_bar(stat = "identity", aes(alpha = threshold_group)) +
  facet_grid(~threshold_group,
             labeller = labeller(threshold_group = c(Above_Significance = ">0.05", 
                                                     Below_Significance = "<0.05"))) +
  labs(x = "Age", 
       y = "P-value Box-Pierce Proportion") +
  scale_fill_manual(values =thresh_colors,name = "Quantile") +  # Specify your desired colors
  theme_bw() + 
  scale_alpha_manual(values = c(0.7, 1)) + 
  guides(alpha = "none") + 
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, 
                               angle = 45,
                               hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white')
  )



gg_prop_box_hl


ggsave(filename = paste0(mydir_figs,"box_prop_test_hl", ".pdf"), 
       plot = gg_prop_box_hl,
       width = 15,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)



melt_adf_hl <- melt_adf_hl %>%
  mutate(threshold_group = ifelse(value > 0.05, "Above_Significance", "Below_Significance"))

melt_adf_hl_count = melt_adf_hl %>%
  group_by(threshold_group, variable, Age) %>%
  summarise(count = n(), proportion = n() / nrow(melt_adf_hl))


melt_adf_hl_count = na.omit(melt_adf_hl_count)

gg_prop_adf_hl = ggplot(melt_adf_hl_count, aes(x = Age, y = proportion,
                                               fill = as.factor(variable))) +
  geom_bar(stat = "identity", aes(alpha = threshold_group)) + 
  facet_grid(~threshold_group,
             labeller = labeller(threshold_group = c(Above_Significance = ">0.05", 
                                                     Below_Significance = "<0.05"))) +
  labs(x = "Age", 
       y = "P-value Augmented Dickey-Fuller Proportion") +
  scale_fill_manual(values =thresh_colors,name = "Quantile") +
  theme_bw() + scale_alpha_manual(values=c(0.7,1)) + 
  guides(alpha = "none") + 
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, 
                               angle = 45,
                               hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white'))



gg_prop_adf_hl


ggsave(filename = paste0(mydir_figs,"adf_prop_test_hl", ".pdf"), 
       plot = gg_prop_adf_hl,
       width = 15,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


#############################
# MODEL ASSESSMENT MEASURES #
#############################

var_prop_age = do.call(cbind, lapply(1:n_tresh, function(i) model_hl[[i]]$varprop) )  

colnames(var_prop_age) = c('Thresholds 1', 'Thresholds 2', 'Thresholds 3', 'Thresholds 4', 'Thresholds 5')
var_prop_age = as.data.frame(var_prop_age)

var_prop_age


dev_age = do.call(rbind,lapply(1:n_tresh, function(i) model_hl[[i]]$mdev )) 

dev_age = as.data.frame(dev_age)

dev_age = t(dev_age)
colnames(dev_age) = c('Thresholds 1', 'Thresholds 2', 'Thresholds 3', 'Thresholds 4', 'Thresholds 5')

df_to_latex_age = rbind(var_prop_age,dev_age)
df_to_latex_age


round(df_to_latex_age, 3)



########################################
# CPMT model by degree of hearing loss # 
########################################

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



######################
# RESIDUALS ANALYSIS #
######################

combined_data_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    data.frame(Fitted_Values =  melt(model_by_age_hl[[i]][[j]]$fitted$y)$value,
               Residuals = melt(model_by_age_hl[[i]][[j]]$residuals$y)$value, 
               Thresholds = freq_tresh_plot[i],
               HL_Loss = hl_classes[j] ) )) 

combined_data_age_hl = do.call(rbind, lapply(1:n_tresh, function(i) 
  do.call(rbind,combined_data_age_hl[[i]]) ) )

combined_data_age_hl$HL_Loss <- factor(combined_data_age_hl$HL_Loss, levels = hl_classes)



gg_res_fitted_agehl = ggplot(combined_data_age_hl, aes(x = Fitted_Values,
                                                       y = Residuals,
                                                       color = as.factor(Thresholds) )) +
  geom_point() +
  theme_bw() +
  labs(x = "Predicted Values",
       y = "Residuals",
       title = "Residuals vs. Predicted Values",
       color = "Quantile")  +
  scale_color_manual(values = thresh_colors) +
  facet_grid(HL_Loss ~ Thresholds,  scales ='free') +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))

gg_res_fitted_agehl

ggsave(filename = paste0(mydir_figs,"res_fitted_age_hl", ".pdf"), 
       plot = gg_res_fitted_agehl,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


heterosked_tests_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    bptest(model_by_age_hl[[i]][[j]]$residuals$y ~ model_by_age_hl[[i]][[j]]$fitted$y) )) 

lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    round(heterosked_tests_age_hl[[i]][[j]]$p.value, 3)))

###############################################################
# Test for Autocorrelation between the Rates across Frequency #
###############################################################


#Box-Pierce (Ljung-Box) Test - fixed lag = 4
boxpierce_Lb_agehl = lapply(1:n_tresh, function(j)
  do.call(cbind,lapply(1:n_hlclasses, function(h)
    round(sapply(1:n_age,function(i) 
      Box.test(model_by_age_hl[[j]][[h]]$residuals$y[i,], 
               lag = 4, type = "Ljung-Box")$p.value),3) )))

for (j in 1:n_tresh) {
  
  boxpierce_Lb_agehl[[j]]<- as.data.frame(boxpierce_Lb_agehl[[j]])
  colnames(boxpierce_Lb_agehl[[j]])<-  hl_classes
  boxpierce_Lb_agehl[[j]]$Age = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1]))
  boxpierce_Lb_agehl[[j]]$Threshold = freq_tresh_plot[j]
  boxpierce_Lb_agehl[[j]] =  boxpierce_Lb_agehl[[j]][, c(ncol(boxpierce_Lb_agehl[[j]]), 
                                                         1:(ncol(boxpierce_Lb_agehl[[j]])-1))]
  boxpierce_Lb_agehl[[j]] =  boxpierce_Lb_agehl[[j]][, c(ncol(boxpierce_Lb_agehl[[j]]), 
                                                         1:(ncol(boxpierce_Lb_agehl[[j]])-1))]
  
}



#Breusch-Godfrey Test (also known as Breusch-Pagan Test or Breusch-Godfrey LM Test) - fixed lag = 4
do.call(rbind, lapply(1:n_tresh, function(j)
  do.call(cbind,lapply(1:n_hlclasses, function(h)
    round(bgtest(as.vector(model_by_age_hl[[j]][[h]]$residuals$y)  ~ 1, 
                 order = 4)$p.value ,3) ))  ))

# Augmented Dickey-Fuller (ADF) Test - fixed lag = 4 --> TOASK here
adf_agehl = lapply(1:n_tresh, function(j)
  do.call(cbind,lapply(1:n_hlclasses, function(h)
    round(sapply(1:n_age,function(i) 
      adf.test(model_by_age_hl[[j]][[h]]$residuals$y[i,])$p.value),3) )))

for (j in 1:n_tresh) {
  
  adf_agehl[[j]]<- as.data.frame(adf_agehl[[j]])
  colnames(adf_agehl[[j]])<-  hl_classes
  adf_agehl[[j]]$Age = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1]))
  adf_agehl[[j]]$Threshold = freq_tresh_plot[j]
  adf_agehl[[j]] =  adf_agehl[[j]][, c(ncol(adf_agehl[[j]]), 
                                       1:(ncol(adf_agehl[[j]])-1))]
  adf_agehl[[j]] =  adf_agehl[[j]][, c(ncol(adf_agehl[[j]]), 
                                       1:(ncol(adf_agehl[[j]])-1))]
  
}


kable(cbind(boxpierce_Lb_hl,adf_hl[,-1]), format = "latex")


melt_box_tests_agehl = melt(boxpierce_Lb_agehl, id.vars = c("Age","Threshold") )
melt_adf_agehl = melt(adf_agehl, id.vars = c("Age","Threshold") )


gg_box_tests_agehl = ggplot(melt_box_tests_agehl, aes(x = as.factor(Age), 
                                                      y = value,
                                                      color = Threshold )) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  facet_wrap(~variable) +
  geom_point(size = 2,
             aes(color =  as.factor(Threshold) ))  +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = paste("Age"), 
       y = "P-value Box-Pierce", 
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
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white')
  ) 


gg_box_tests_agehl

ggsave(filename = paste0(mydir_figs,"box_test_agehl", ".pdf"), 
       plot = gg_box_tests_agehl,
       width = 15,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)



gg_adf_tests_agehl = ggplot(melt_adf_agehl, aes(x = as.factor(Age), 
                                                y = value,
                                                color = Threshold )) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  facet_wrap(~variable) +
  geom_point(size = 2,
             aes(color =  as.factor(Threshold) ))  +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = paste("Quantile"), 
       y = "P-value Augmented Dickey-Fuller", 
       color = "Age") +
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
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white')
  ) 

gg_adf_tests_agehl

ggsave(filename = paste0(mydir_figs,"adf_test_agehl", ".pdf"), 
       plot = gg_adf_tests_agehl,
       width = 15,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)



melt_box_tests_agehl <- melt_box_tests_agehl %>%
  mutate(threshold_group = ifelse(value > 0.05, "Above_Threshold", "Below_Threshold"))

melt_box_agehl_count = melt_box_tests_agehl %>%
  group_by(threshold_group, variable, Age,Threshold) %>%
  summarise(count = n(), proportion = n() / nrow(melt_box_tests_agehl))


melt_box_agehl_count = na.omit(melt_box_agehl_count)

gg_prop_box_agehl = ggplot(melt_box_agehl_count, aes(x = Age, y = proportion,
                                                     fill = as.factor(variable))) +
  geom_bar(stat = "identity", aes(alpha = threshold_group)) + 
  facet_grid(Threshold ~ threshold_group,
             labeller = labeller(threshold_group = c(Above_Threshold = ">0.05", 
                                                     Below_Threshold = "<0.05"))) +
  labs(x = "Age", 
       y = "P-value Box-Pierce Proportion")  +  
  theme_bw() + scale_alpha_manual(values=c(0.7,1)) + 
  guides(alpha = "none") +
  scale_fill_manual(values = thresh_colors,name = "Hearing Loss") + 
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, 
                               angle = 45,
                               hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white'))



gg_prop_box_agehl


ggsave(filename = paste0(mydir_figs,"box_prop_test_agehl", ".pdf"), 
       plot = gg_prop_box_agehl,
       width = 15,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


melt_adf_agehl <- melt_adf_agehl %>%
  mutate(threshold_group = ifelse(value > 0.05, "Above_Threshold", "Below_Threshold"))

melt_adf_agehl_count = melt_adf_agehl %>%
  group_by(threshold_group, variable, Age,Threshold) %>%
  summarise(count = n(), proportion = n() / nrow(melt_adf_agehl))

melt_adf_agehl_count = na.omit(melt_adf_agehl_count)

gg_prop_adf_agehl = ggplot(melt_adf_agehl_count, aes(x = Age, y = proportion,
                                                     fill = as.factor(Threshold))) +
  geom_bar(stat = "identity", aes(alpha = threshold_group)) + 
  facet_grid(variable ~ threshold_group,
             labeller = labeller(threshold_group = c(Above_Threshold = ">0.05", 
                                                     Below_Threshold = "<0.05"))) +
  labs(x = "Age", 
       y = "P-value Augmented Dickey-Fuller Proportion") +
  theme_bw() + scale_alpha_manual(values=c(0.7,1)) +
  scale_fill_manual(values = thresh_colors,
                    name = "Quantile") +  
  guides(alpha = "none") + 
  theme(
    plot.background = element_rect(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, 
                               angle = 45,
                               hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white'))

gg_prop_adf_agehl


ggsave(filename = paste0(mydir_figs,"adf_prop_test_agehl", ".pdf"), 
       plot = gg_prop_adf_agehl,
       width = 15,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


#############################
# MODEL ASSESSMENT MEASURES #
#############################

var_prop_age_hl = do.call(cbind, lapply(1:n_tresh, function(i) do.call(rbind,
        lapply(1:n_hlclasses, function(j) model_by_age_hl[[i]][[j]]$varprop)) ) )

colnames(var_prop_age_hl) = freq_tresh_plot
var_prop_age_hl = as.data.frame(var_prop_age_hl)
var_prop_age_hl$HL = hl_classes
var_prop_age_hl$HL = factor(var_prop_age_hl$HL, levels = hl_classes)

var_prop_age_hl

kable(round(var_prop_age_hl[,c(1:5)],3), format = 'latex')

var_prop_age_hl_long <- melt(var_prop_age_hl)

gg1= ggplot(var_prop_age_hl_long, aes(x = HL,
                                      y = value, color = variable,
                                      group = variable)) +
  geom_line() +
  geom_point() +
  labs(x = "Hearing Loss", y = "Variance Proportion", color = "Quantile") +
  scale_color_manual(values = thresh_colors)+
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))

gg1

ggsave(filename = paste0(mydir_figs,"var_prop_age_hl", ".pdf"), 
       plot = gg1,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


dev_age_hl = do.call(rbind,lapply(1:n_tresh, function(i) do.call(rbind,
                                                                 lapply(1:n_hlclasses, function(j) model_by_age_hl[[i]][[j]]$mdev )) ))

dev_age_hl = as.data.frame(dev_age_hl)

dev_age_hl$HL = rep(hl_classes,5)
dev_age_hl$HL = factor(dev_age_hl$HL, levels = hl_classes)

dev_age_hl$Thresholds = rep(colnames(var_prop_age_hl)[-6], each = 5)

dev_age_hl

kable(cbind(round(dev_age_hl[,c(1,2)],3) ,dev_age_hl[,c(3,4)] ), format = "latex")

gg2 = ggplot(dev_age_hl, aes(x = HL, y = `Mean deviance base`, color = Thresholds)) +
  geom_line(aes(group = Thresholds)) +
  geom_point() +
  scale_color_manual(values = thresh_colors)+
  labs(title = "",
       x = "Hearing Loss",
       y = "Mean Deviance Base",  color = "Quantile") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))
gg2

ggsave(filename = paste0(mydir_figs,"dev_age_hl_base", ".pdf"), 
       plot = gg2,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)

gg2


gg3 = ggplot(dev_age_hl, aes(x = HL, y = `Mean deviance total`, 
                             color = Thresholds)) +
  geom_line(aes(group = Thresholds)) +
  geom_point() +
  scale_color_manual(values = thresh_colors)+
  labs(title = "",
       x = "Hearing Loss",
       y = "Mean Deviance Total",  color = "Quantile") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))
gg3


ggsave(filename = paste0(mydir_figs,"dev_age_hl_total", ".pdf"), 
       plot = gg3,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)



########################################
# CPMT model by degree of hearing loss # 
########################################

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



#############################
# MODEL ASSESSMENT MEASURES #
#############################

var_prop_age_sex = do.call(cbind, lapply(1:n_tresh, function(i) do.call(rbind,
            lapply(1:2, function(j) model_by_age_sex[[i]][[j]]$varprop)) ) )

colnames(var_prop_age_sex) = freq_tresh_plot
var_prop_age_sex = as.data.frame(var_prop_age_sex)
var_prop_age_sex$Sex = c("Female", "Male")
var_prop_age_sex$Sex = factor(var_prop_age_sex$Sex, levels = c("Female", "Male"))

var_prop_age_sex

kable(round(var_prop_age_sex[,c(1:5)],3), format = 'latex')

var_prop_age_sex_long <- melt(var_prop_age_sex)

gg1= ggplot(var_prop_age_sex_long, aes(x = Sex,
                                      y = value, color = variable,
                                      group = variable)) +
  geom_line() +
  geom_point() +
  labs(x = "Sex", y = "Variance Proportion", color = "Quantile") +
  scale_color_manual(values = thresh_colors)+
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))

gg1

ggsave(filename = paste0(mydir_figs,"var_prop_age_sex", ".pdf"), 
       plot = gg1,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


dev_age_sex = do.call(rbind,lapply(1:n_tresh, function(i) do.call(rbind,
                  lapply(1:2, function(j) model_by_age_sex[[i]][[j]]$mdev )) ))

dev_age_sex = as.data.frame(dev_age_sex)

dev_age_sex$Sex = rep(c("Female", "Male"),5)
dev_age_sex$Sex = factor(dev_age_sex$Sex, levels = c("Female", "Male"))

dev_age_sex$Thresholds = rep(colnames(var_prop_age_sex)[-6], each = 2)

dev_age_sex

kable(cbind(round(dev_age_sex[,c(1,2)],3) ,dev_age_sex[,c(3,4)] ), format = "latex")

gg2 = ggplot(dev_age_sex, aes(x = Sex, y = `Mean deviance base`, color = Thresholds)) +
  geom_line(aes(group = Thresholds)) +
  geom_point() +
  scale_color_manual(values = thresh_colors)+
  labs(title = "",
       x = "Sex",
       y = "Mean Deviance Base",  color = "Quantile") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))
gg2

ggsave(filename = paste0(mydir_figs,"dev_age_sex_base", ".pdf"), 
       plot = gg2,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)

gg2


gg3 = ggplot(dev_age_sex, aes(x = Sex, y = `Mean deviance total`, 
                             color = Thresholds)) +
  geom_line(aes(group = Thresholds)) +
  geom_point() +
  scale_color_manual(values = thresh_colors)+
  labs(title = "",
       x = "Sex",
       y = "Mean Deviance Total",  color = "Quantile") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))
gg3


ggsave(filename = paste0(mydir_figs,"dev_age_hl_total", ".pdf"), 
       plot = gg3,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)












