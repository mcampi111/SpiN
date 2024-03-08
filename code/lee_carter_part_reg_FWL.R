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


###############################
# SET DIRECTORY AND READ FILE #
###############################

mydir<- "C:\\Users\\mcampi\\Desktop\\Hung_Joanna\\data\\"
mydir2<- "C:\\Users\\mcampi\\Desktop\\Hung_Joanna\\code\\"

mydir_figs = "C:\\Users\\mcampi\\Desktop\\Hung_Joanna\\code\\figs\\"


source(paste(mydir2, "utils.R", sep = "")) 


data_ampl2 = readRDS( file= paste(mydir, "data_ampl2bis.rds", sep = ""))
data_ampl2 <- data_ampl2[data_ampl2$AGE <= 89, ]
data_ampl2 <- data_ampl2[data_ampl2$AGE >= 45, ]

ggplot(data_ampl2, aes(x = AGE)) +
  geom_histogram(binwidth = 1, 
                 fill = "#4DA1A9", 
                 color = "#2B3A42",  
                 aes(y = ..count..)) +
  labs(title = "Age Distribution", 
       x = "Age", y = "Number of Patients") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)


data_ampl2$age_group2 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 5), labels = FALSE,
                             include.lowest = TRUE)
data_ampl2$age_group2 <- factor(paste0((data_ampl2$age_group2 - 1) * 5, "-", (data_ampl2$age_group2) * 5))

data_ampl2$age_group3 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 1), labels = FALSE,
                             include.lowest = TRUE)
data_ampl2$age_group3 <- factor(paste0((data_ampl2$age_group3 - 1) , "-", (data_ampl2$age_group3) ))


#final data for analysis
data_lee_carter = data_ampl2[,c(3,4,7:17,34,35,36)] #Left ear
#data_lee_carter = data_ampl2[,c(3,4,18:28,34,35,36)] #Right ear
spiq =  data_ampl2[,c(3,4,30,34,35,36)]
spin =  data_ampl2[,c(3,4,31,34,35,36)]


apply(data_ampl2[,c(7:17)], 2, range)
apply(data_ampl2[,c(18:28)], 2, range)


apply(data_ampl2[,c(30)], 2, range)
apply(data_ampl2[,c(31)], 2, range)



##############
# SOME PLTOS #
##############


ggplot(data_ampl2, aes(x = Classification, y = SRT)) +
  geom_boxplot(aes(fill = Classification), color = "black") +
  labs(title = "SRT Distribution by Classification", 
       x = "Classification", y = "SRT") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(data_ampl2, aes(x = Classification, y = SNR)) +
  geom_boxplot(aes(fill = Classification), color = "black") +
  labs(title = "SNR Distribution by Classification", 
       x = "Classification", y = "SNR") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


PTA <- data_ampl2$PTA
SRT <- data_ampl2$SRT

PTA_df = data.frame(PTA)
SRT_df = data.frame(SRT)


ggplot(PTA_df, aes(x = PTA)) +
  geom_histogram(binwidth = 1, 
                 fill = "#4DA1A9", 
                 color = "#2B3A42",  
                 aes(y = ..count..)) +
  labs(title = "PTA Distribution",
       x = "PTA",
       y = "Nulber of Patients") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)




ggplot(SRT_df, aes(x = SRT)) +
  geom_histogram(binwidth = 1, 
                 fill = "#4DA1A9", 
                 color = "#2B3A42",  
                 aes(y = ..count..)) +
  labs(title = "SRT Distribution",
       x = "SRT",
       y = "Number of Patients") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)

SNR <- data_ampl2$SNR
SNR_df = data.frame(SNR)


ggplot(SNR_df, aes(x = SNR))  +
  geom_histogram(binwidth = 1, 
                 fill = "#4DA1A9", 
                 color = "#2B3A42",  
                 aes(y = ..count..)) +
  labs(title = "SNR Distribution",
       x = "SNR",
       y = "Number of Patients") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)


F_1500 = data_ampl2$FREQ_1500_L
F_1500_df = data.frame(F_1500)


ggplot(F_1500_df, aes(x = F_1500)) +
  geom_bar(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Barplot of F_1500",
       x = "F_1500",
       y = "# Patients") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)


ggplot(data_ampl2, aes(x=PTA, y=SRT, fill = Classification)) +
  geom_point(size=2, shape=23) +
  theme_bw() + theme(axis.line = element_line(color='black'),
                     plot.background = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.text.x = element_text( size=10, angle = 90 ),
                     axis.text.y = element_text( size=10),
                     axis.title.x = element_text(size = 15),
                     axis.title.y = element_text(size = 15),
                     legend.position = "bottom") 


ggplot(data_ampl2, aes(x=PTA, y=SNR, fill = Classification)) +
  geom_point(size=2, shape=23) +
  theme_bw() + theme(axis.line = element_line(color='black'),
                     plot.background = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.text.x = element_text( size=10, angle = 90 ),
                     axis.text.y = element_text( size=10),
                     axis.title.x = element_text(size = 15),
                     axis.title.y = element_text(size = 15),
                     legend.position = "bottom") 


ggplot(data_ampl2, aes(x=SRT, y=SNR, fill = Classification)) +
  geom_point(size=2, shape=23) +
  theme_bw() + theme(axis.line = element_line(color='black'),
                     plot.background = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.text.x = element_text( size=10, angle = 90 ),
                     axis.text.y = element_text( size=10),
                     axis.title.x = element_text(size = 15),
                     axis.title.y = element_text(size = 15),
                     legend.position = "bottom") 



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
#decision_threshold_un = unique(decision_threshold) I don t think this should be done

quant_tresh <- c(0.2, 0.4, 0.6, 0.8, 0.9)
#quant_tresh2 <- c(0.25, 0.45, 0.65, 0.85, 0.95)

quant_PTA <- quantile(decision_threshold$PTA, probs = quant_tresh, na.rm = TRUE) #decision_threshold_un$PTA
quant_SRT <- quantile(decision_threshold$SRT, probs = quant_tresh, na.rm = TRUE) #decision_threshold_un$SRT
quant_SNR <- quantile(decision_threshold$SNR, probs = quant_tresh, na.rm = TRUE) #decision_threshold_un$SNR

tres_pta_srt = data.frame(PTA = quant_PTA, SRT = quant_SRT)
tres_pta_snr = data.frame(PTA = quant_PTA, SNR = quant_SNR)
tres_srt_snr = data.frame(SRT = quant_SRT, SNR = quant_SNR)

tres_pta_srt2 <- data.frame(X = as.numeric(tres_pta_srt[,1]), Y = as.numeric(tres_pta_srt[,2]))  
tres_pta_snr2 <- data.frame(X = as.numeric(tres_pta_snr[,1]), Y = as.numeric(tres_pta_snr[,2]))  
tres_srt_snr2 <- data.frame(X = as.numeric(tres_srt_snr[,1]), Y = as.numeric(tres_srt_snr[,2]))  


treshold_hearing_rate = as.integer(tres_pta_snr[,1])
n_tresh = length(treshold_hearing_rate)

treshold_SPIQ = as.integer(tres_pta_srt[,2])
treshold_SPIN = as.integer(tres_pta_snr[,2])


freq_tresh = do.call(rbind,lapply(data_ampl2[,7:17], quantile, quant_tresh, 2))


############################
# VISUALISATION Thresholds #
############################

ggplot(data_ampl2, aes(x = PTA, y = SRT)) +
  geom_point(size = 2, shape = 23, aes(fill = Classification)) +
  geom_segment(data = tres_pta_srt2, aes(x = -Inf, xend = X, y = Y, yend = Y), 
               linetype = "dashed", color = "red", size = 1.5) +
  geom_segment(data = tres_pta_srt2, aes(x = X, xend = X, y = -Inf, yend = Y), 
               linetype = "dashed", color = "blue", size = 1.5) +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  )



ggplot(data_ampl2, aes(x = PTA, y = SNR)) +
  geom_point(size = 2, shape = 23, aes(fill = Classification)) +
  geom_segment(data = tres_pta_snr2, aes(x = -Inf, xend = X, y = Y, yend = Y), 
               linetype = "dashed", color = "red", size = 1.5) +
  geom_segment(data = tres_pta_snr2, aes(x = X, xend = X, y = -Inf, yend = Y), 
               linetype = "dashed", color = "blue", size = 1.5) +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  )


ggplot(data_ampl2, aes(x = SRT, y = SNR)) +
  geom_point(size = 2, shape = 23, aes(fill = Classification)) +
  geom_segment(data = tres_srt_snr2, aes(x = -Inf, xend = X, y = Y, yend = Y), 
               linetype = "dashed", color = "red", size = 1.5) +
  geom_segment(data = tres_srt_snr2, aes(x = X, xend = X, y = -Inf, yend = Y), 
               linetype = "dashed", color = "blue", size = 1.5) +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  )


###########################################
# CHOOSE AGE STEP TO CONDUCT THE ANALYSIS #
###########################################

age_var = "age_group3"
age_group_var = all_age_groups
n_age = length(age_group_var)


#########################################
# DATA Prep.& Hearing Rates Construction#
#########################################

# hr_rates <- process_data(data_lee_carter, 
#                          treshold_hearing_rate, 
#                          age_group_var = age_var,
#                          n_tresh, n_age,
#                          n_hlclasses,
#                          process_func = process_matrix)


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


##############
# Plot Rates #
##############

#Hearing Loss Rates
plots_hr_rates = plot_hr_rates(hr_rates_interp,
                               hl_classes, 
                               n_tresh,
                               age_group_var,
                               freq_tresh) #modify this with the right thresholds if use the second option
plots_hr_rates[[1]]
plots_hr_rates[[2]]


plots_hr_rates2 = plot_hr_rates2(hr_rates_interp,
                                 hl_classes, 
                                 n_tresh,
                                 age_group_var,
                                 freq_tresh) 

plots_hr_rates2[[1]]
plots_hr_rates2[[2]]


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"hear_r_age_", i, ".pdf"), 
         plot = plots_hr_rates2[[1]][[i]],
         width = 12,
         height = 9)
}


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"hear_r_agehl_", i, ".pdf"),
         plot = plots_hr_rates2[[2]][[i]],
         width = 12,
         height = 9)
}



#Spiq Rates
plots_spiq_rates = plot_speech_rates(spiq_rates,
                                     hl_classes, 
                                     n_tresh,
                                     age_group_var,
                                     treshold_SPIQ,
                                     "SpiQ") 

plots_spiq_rates[[1]]
plots_spiq_rates[[2]]


plots_spiq_rates2 = plot_speech_rates2(spiq_rates,
                                     hl_classes, 
                                     n_tresh,
                                     age_group_var,
                                     treshold_SPIQ,
                                     "SpiQ") 

plots_spiq_rates2[[1]]
plots_spiq_rates2[[2]]


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spiq_r_age_", i, ".pdf"), 
         plot = plots_spiq_rates2[[1]][[i]],
         width = 8,
         height = 6)
}


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spiq_r_agehl_", i, ".pdf"),
         plot = plots_spiq_rates2[[2]][[i]],
         width = 8,
         height = 6)
}


#Spin Rates
plots_spin_rates = plot_speech_rates(spin_rates,
                                     hl_classes, 
                                     n_tresh,
                                     age_group_var,
                                     treshold_SPIN,
                                     "SpiN") 

plots_spin_rates[[1]]
plots_spin_rates[[2]]


plots_spin_rates2 = plot_speech_rates2(spin_rates,
                                     hl_classes, 
                                     n_tresh,
                                     age_group_var,
                                     treshold_SPIN,
                                     "SpiN") 

plots_spin_rates2[[1]]
plots_spin_rates2[[2]]


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spin_r_age_", i, ".pdf"), 
         plot = plots_spin_rates2[[1]][[i]],
         width = 8,
         height = 6)
}


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"spin_r_agehl_", i, ".pdf"),
         plot = plots_spin_rates2[[2]][[i]],
         width = 8,
         height = 6)
}




#Need to solve this in the general case of treshold_hearing_rate still!!!!!!!
heatmaps_rates = plot_heat_rates(hr_rates_interp,spiq_rates, spin_rates,
                                 hl_classes, freq_vector, n_tresh,
                                 age_group_var, freq_tresh,#treshold_hearing_rate
                                 treshold_SPIQ, treshold_SPIN)


heatmaps_rates[[1]]
heatmaps_rates[[2]]


ggsave(filename = paste0(mydir_figs,"heat_overall", ".pdf"), 
       plot = heatmaps_rates[[1]],
       width = 12,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


for (i in 1:n_tresh) {
  # Modify the file name/path as needed
  ggsave(filename = paste0(mydir_figs,"heat_hl_", i, ".pdf"), 
         plot = heatmaps_rates[[2]][[i]],
         width = 12,
         height = 9)
}


########################
########################
# STEP 1: LEE - CARTER #
########################
########################

##################################
# LEE - CARTER model by age only #
##################################


# demodata_byhl = lapply(1:n_tresh, function(i)
#   demogdata(data = hr_rates_interp[[1]][[i]],  
#             pop = hr_rates_interp[[1]][[i]]*treshold_hearing_rate[i], 
#             ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),  
#             years = as.numeric(freq_vector), 
#             type = 'mortality',
#             label = 'hearing',
#             name = 'total') )

demodata_byhl = lapply(1:n_tresh, function(i)
  demogdata(data = hr_rates_interp[[1]][[i]],
            pop = hr_rates_interp[[1]][[i]]*freq_tresh[,i],
            ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),
            years = as.numeric(freq_vector),
            type = 'mortality',
            label = 'hearing',
            name = 'total') )

model_hl = lapply(1:n_tresh, function(i)
  lca(data = demodata_byhl[[i]]))


summary(model_hl[[2]])
plot(model_hl[[1]])
plot(model_hl[[2]])
plot(model_hl[[3]])
plot(model_hl[[4]])



plots_model_age <- plot_coeff_by_age(model_hl, treshold_hearing_rate,
                                     age_group_var, freq_vector)
plots_model_age[[1]]
plots_model_age[[2]]
plots_model_age[[3]]


ggsave(filename = paste0(mydir_figs,"kappa_age", ".pdf"), 
       plot = plots_model_age[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"a_age", ".pdf"), 
       plot = plots_model_age[[2]],
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


plot(demodata_byhl[[1]],
     years=as.numeric(freq_vector))

plot(forecast(model_hl[[1]], h = 10))
plot(forecast(model_hl[[2]], h = 10))
plot(forecast(model_hl[[3]], h = 10))
plot(forecast(model_hl[[4]], h = 10))
plot(forecast(model_hl[[5]], h = 10))


library(gplots)
#fitted
heatmap.2( model_hl[[1]]$fitted$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))


heatmap.2( model_hl[[2]]$fitted$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))

heatmap.2( model_hl[[3]]$fitted$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))

heatmap.2( model_hl[[4]]$fitted$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))

heatmap.2( model_hl[[5]]$fitted$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))

#residuals
heatmap.2( model_hl[[1]]$residuals$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))

heatmap.2( model_hl[[2]]$residuals$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))

heatmap.2( model_hl[[3]]$residuals$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))

heatmap.2( model_hl[[5]]$residuals$y,
           dendrogram = "none",
           #col =  rev(hcl.colors(20*10, "reds", rev = TRUE)), 
           Rowv=FALSE, Colv = FALSE, labRow = FALSE, labCol = FALSE,
           margins=c(3,0), 
           trace='none', 
           symkey=FALSE, 
           symbreaks=FALSE, 
           density.info='none', 
           denscol="black",
           keysize=1, 
           key.par=list(mar=c(3.5,0,3,0)),
           lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1))


######################################################
# LEE - CARTER model by degree of hearing loss & Age # 
#####################################################



# demodata_by_age_hl = lapply(1:n_tresh, function(i)
#   lapply(1:n_hlclasses, function(j)
#     demogdata(data = hr_rates_interp[[2]][[i]][[j]],    
#               pop = hr_rates_interp[[2]][[i]][[j]]*treshold_hearing_rate[i], 
#               ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])), 
#               years = as.numeric(freq_vector), 
#               type = 'mortality',
#               label = 'hearing',
#               name = 'total') ) )


demodata_by_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    demogdata(data = hr_rates_interp[[2]][[i]][[j]],
              pop = hr_rates_interp[[2]][[i]][[j]]*freq_tresh[,i],
              ages = sapply(strsplit(age_group_var, "-"), function(x) as.numeric(x[1])),
              years = as.numeric(freq_vector),
              type = 'mortality',
              label = 'hearing',
              name = 'total') ) )



model_by_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    lca(data = demodata_by_age_hl[[i]][[j]] ,
        interpolate = TRUE)) )

summary(model_by_age_hl[[1]][[1]])


plot(model_by_age_hl[[1]][[3]])


plots_model_agehl <- plot_coeff_by_age_hl(model_by_age_hl, 
                                          n_tresh, n_hlclasses, hl_classes,
                                          age_group_var ,
                                          treshold_hearing_rate, freq_vector)


plots_model_agehl[[1]]
plots_model_agehl[[2]]
plots_model_agehl[[3]]


ggsave(filename = paste0(mydir_figs,"kappa_age_hl", ".pdf"), 
       plot = plots_model_agehl[[1]],
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)


ggsave(filename = paste0(mydir_figs,"a_age_hl", ".pdf"), 
       plot = plots_model_agehl[[2]],
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




plot(forecast(model_by_age_hl[[1]][[1]]))
plot(forecast(model_by_age_hl[[1]][[2]]))
plot(forecast(model_by_age_hl[[1]][[3]]))
plot(forecast(model_by_age_hl[[1]][[4]]))
plot(forecast(model_by_age_hl[[1]][[5]]))


var_prop_age_hl = do.call(cbind, lapply(1:n_tresh, function(i) do.call(rbind,
  lapply(1:n_hlclasses, function(j) model_by_age_hl[[i]][[j]]$varprop)) ) )

colnames(var_prop_age_hl) = c('Thresholds 1', 'Thresholds 2', 'Thresholds 3', 'Thresholds 4', 'Thresholds 5')
var_prop_age_hl = as.data.frame(var_prop_age_hl)
var_prop_age_hl$HL = hl_classes
var_prop_age_hl$HL = factor(var_prop_age_hl$HL, levels = hl_classes)
  
var_prop_age_hl

kable(round(var_prop_age_hl[,c(1:5)],3), format = 'latex')

var_prop_age_hl_long <- tidyr::pivot_longer(var_prop_age_hl, 
                                            starts_with("Thresholds"),
                                            names_to = "Threshold",
                                            values_to = "Value")

gg1= ggplot(var_prop_age_hl_long, aes(x = Threshold, y = Value, color = HL, group = HL)) +
  geom_line() +
  geom_point() +
  labs(x = "Thresholds", y = "Variance Proportion", color = "HL") +
  scale_color_discrete(name = "HL")  +
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

gg2 = ggplot(dev_age_hl, aes(x = Thresholds, y = `Mean deviance base`, color = HL)) +
  geom_line(aes(group = HL)) +
  geom_point() +
  labs(title = "",
       x = "Tresholds",
       y = "Mean Deviance Base") +
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

ggsave(filename = paste0(mydir_figs,"dev_age_hl_base", ".pdf"), 
       plot = gg2,
       width = 8,
       height = 6,
       device = "pdf",
       units = "in", dpi = 300)

gg2


gg3 = ggplot(dev_age_hl, aes(x = Thresholds, y = `Mean deviance total`, color = HL)) +
  geom_line(aes(group = HL)) +
  geom_point() +
  labs(title = "",
       x = "Thresholds",
       y = "Mean Deviance Total") +
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

################################################
################################################
#   SOLUTION 2)  : Frisch–Waugh–Lovell theorem #
################################################
################################################

#####################
# Model by age only # 
#####################

x1_hl = lapply(1:n_tresh, function(i) 
  as.numeric(model_hl[[i]]$bx) %*% t(as.numeric(model_hl[[i]]$kt)))


H_x1_hl = lapply(1:n_tresh, function(j) x1_hl[[j]] %*% ginv(t(x1_hl[[j]]) %*% x1_hl[[j]]) %*% t(x1_hl[[j]])  )


M_x1_hl = lapply(1:n_tresh, function(j) diag(n_age) - H_x1_hl[[j]] )


y_adj_hl = lapply(1:n_tresh, function(i)  M_x1_hl[[i]] %*% hr_rates_interp[[1]][[i]] ) 

x2_adj_hl = lapply(1:n_tresh, function(i)  M_x1_hl[[i]] %*%  cbind(spiq_rates[[1]][[i]], 
                                                                   spin_rates[[1]][[i]])  ) 

partial_step_FWL_hl = lapply(1:n_tresh, function(i) 
                    lm(y_adj_hl[[i]] ~ x2_adj_hl[[i]] ))


res_FWL_hl = process_summary(partial_step_FWL_hl, 
                             n_tresh, freq_vector, treshold_SPIQ,
                             treshold_SPIN, hl_classes)

remove_hl_classes <- function(df) {
  df <- df[, !(names(df) %in% 'hl_classes'), drop = FALSE]
  return(df)
}


res_FWL_hl <- lapply(res_FWL_hl, remove_hl_classes)

melt_est_FWL_spiq_hl = melt(res_FWL_hl$est_pstep2_spiq_hl, id.vars = "SPIQ_tresh")
melt_p_FWL_spiq_hl = melt(res_FWL_hl$p_pstep2_spiq_hl, id.vars = "SPIQ_tresh")
melt_FWL_spiq_hl = melt_est_FWL_spiq_hl
melt_FWL_spiq_hl$color = melt_p_FWL_spiq_hl$value

melt_FWL_spiq_hl$color2 <- ifelse(melt_FWL_spiq_hl$color < 0.01, "azure3",
                                     ifelse(melt_FWL_spiq_hl$color <= 0.05, "cyan", "blue"))
melt_FWL_spiq_hl$color2 <- factor(melt_FWL_spiq_hl$color2, levels = c("azure3", "cyan", "blue"))



latex_tbl_SPIQ = bind_cols(round(res_FWL_hl[[1]][-c(12:13)],3), 
                  round(res_FWL_hl[[3]][-c(12:13)],3) ) %>% 
                    dplyr::select(all_of(c(matrix(names(.), ncol = 11, byrow = TRUE))))

kable(latex_tbl_SPIQ, format = 'latex')

latex_tbl_SPIN = bind_cols(round(res_FWL_hl[[2]][-c(12:13)],3), 
                  round(res_FWL_hl[[4]][-c(12:13)],3) ) %>% 
                    dplyr::select(all_of(c(matrix(names(.), ncol = 11, byrow = TRUE))))

kable(latex_tbl_SPIN, format = 'latex')


#heatmaps
# Plotting with manual color scale
gg4 = ggplot(melt_FWL_spiq_hl, aes(x = as.factor(variable), 
                                y = as.factor(SPIQ_tresh), 
                                fill = color2,
                                label = round(value, 3))) +
  geom_tile() + 
  geom_text(vjust = 1) +
  scale_fill_manual(values = c("azure3" = "azure3", 
                               "cyan" = "cyan", 
                               "blue" = "blue"),
                    labels = c("< 0.01", 
                               "0.01 < & < 0.05", 
                               " > 0.05")) +  # Specify legend labels
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
    plot.margin = margin(0.1, 0.1, 0.1, 0.1)
  ) +
  labs(fill = "Significance",  
       x = "Frequencies",  
       y = "SPIQ threshold")

gg4

ggsave(filename = paste0(mydir_figs,"part_reg_spiq_age", ".pdf"), 
       plot = gg4,
       width = 12,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)

#boxplot
melt_FWL_spiq_hl$SPIQ_tresh2 <- factor(melt_FWL_spiq_hl$SPIQ_tresh)

# Create the boxplot
gg4_box1 = ggplot(melt_FWL_spiq_hl, aes(x = as.factor(variable), 
                             y = color)) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  geom_point(size = 2, aes(color = SPIQ_tresh2))  +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = "Frequency", y = "P-value", color = "SPIQ Threshold") +
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

gg4_box1

ggsave(filename = paste0(mydir_figs,"box_spiq1_spiq_age", ".pdf"), 
       plot = gg4_box1,
       # width = 12,
       # height = 9,
       device = "pdf",
       units = "in", dpi = 300)

gg4_box2 = ggplot(melt_FWL_spiq_hl, aes(x = SPIQ_tresh2, 
                             y = color)) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  geom_point(size = 2, aes(color = as.factor(variable) ))  +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = "SPIQ Threshold", y = "P-value", color = "Frequency") +
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

gg4_box2

ggsave(filename = paste0(mydir_figs,"box_spiq2_spiq_age", ".pdf"), 
       plot = gg4_box2,
      # width = 12,
      # height = 9,
       device = "pdf",
       units = "in", dpi = 300)


melt_est_FWL_spin_hl = melt(res_FWL_hl$est_pstep2_spin_hl, id.vars = "SPIN_tresh")
melt_p_FWL_spin_hl = melt(res_FWL_hl$p_pstep2_spin_hl, id.vars = "SPIN_tresh")
melt_FWL_spin_hl = melt_est_FWL_spin_hl
melt_FWL_spin_hl$color = melt_p_FWL_spin_hl$value

melt_FWL_spin_hl$color2 <- ifelse(melt_FWL_spin_hl$color < 0.01, "azure3",
                                     ifelse(melt_FWL_spin_hl$color <= 0.05, "cyan", "blue"))
melt_FWL_spin_hl$color2 <- factor(melt_FWL_spin_hl$color2, levels = c("azure3", "cyan", "blue"))

# Plotting with manual color scale
gg5 = ggplot(melt_FWL_spin_hl, aes(x = as.factor(variable), 
                                y = as.factor(SPIN_tresh), 
                                fill = color2,
                                label = round(value, 3))) +
  geom_tile() + 
  geom_text(vjust = 1) +
  scale_fill_manual(values = c("azure3" = "azure3", 
                               "cyan" = "cyan", 
                               "blue" = "blue"),
                    labels = c("< 0.01", 
                               "0.01 < & < 0.05", 
                               " > 0.05")) +  # Specify legend labels
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
    plot.margin = margin(0.1, 0.1, 0.1, 0.1)  # Remove space around the plot
  ) +
  labs(fill = "Significance",  
       x = "Frequencies",  
       y = "SPIN threshold")

gg5


ggsave(filename = paste0(mydir_figs,"part_reg_spin_age", ".pdf"), 
       plot = gg5,
       width = 12,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)

#boxplot
melt_FWL_spin_hl$SPIN_tresh2 <- factor(melt_FWL_spin_hl$SPIN_tresh)

# Create the boxplot
gg5_box1 = ggplot(melt_FWL_spin_hl, aes(x = as.factor(variable), 
                             y = color)) +
  geom_boxplot(alpha = 0.5,color = "black") + 
  geom_point(size = 2, aes(color = SPIN_tresh2))  +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = "Frequency", y = "P-value", color = "SPIN Threshold") +
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

gg5_box1

ggsave(filename = paste0(mydir_figs,"box_spin1_spin_age", ".pdf"), 
       plot = gg5_box1,
       # width = 12,
       # height = 9,
       device = "pdf",
       units = "in", dpi = 300)


gg5_box2 = ggplot(melt_FWL_spin_hl, aes(x = SPIN_tresh2, 
                                        y = color)) +
  geom_boxplot(alpha = 0.5, color = "black") + 
  geom_point(size = 2, aes(color = as.factor(variable) )) +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
  labs(x = "SPIN Threshold", y = "P-value", color = "Frequency") +
  theme_bw() +
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


gg5_box2


ggsave(filename = paste0(mydir_figs,"box_spin2_spin_age", ".pdf"), 
       plot = gg5_box2,
       # width = 12,
       # height = 9,
       device = "pdf",
       units = "in", dpi = 300)


#########################################
# Model by degree of hearing loss & Age # 
#########################################


x1_age_hl = lapply(1:n_tresh, function(i)
              lapply(1:n_hlclasses, function(j)
              as.numeric(model_by_age_hl[[i]][[j]]$bx) %*% t(as.numeric(model_by_age_hl[[i]][[j]]$kt)) ))



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
                  treshold_SPIQ[i],
                  treshold_SPIN[i],
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



# Create plots for each class
plots_list_spiq_FWL <- lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list(res_FWL_age_hl2[[i]], variable_type = "SPIQ", title = hl_classes[i])
})


plots_list_spin_FWL <- lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list(res_FWL_age_hl2[[i]], variable_type = "SPIN", title = hl_classes[i])
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





box_1_SPIQ = lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list_boxplot(res_FWL_age_hl2[[i]], 
                         variable_type = "SPIQ", title = hl_classes[i])
})

plots_box_1_SPIQ = grid.arrange(grobs = box_1_SPIQ, ncol = 3)

ggsave(filename = paste0(mydir_figs,"box_1_SPIQ", ".pdf"), 
       plot = plots_box_1_SPIQ,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


box_2_SPIQ = lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list_boxplot_2(res_FWL_age_hl2[[i]], 
                         variable_type = "SPIQ", title = hl_classes[i])
})


plots_box_2_SPIQ = grid.arrange(grobs = box_2_SPIQ, ncol = 3)


ggsave(filename = paste0(mydir_figs,"box_2_SPIQ", ".pdf"), 
       plot = plots_box_2_SPIQ,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


box_1_SPIN = lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list_boxplot(res_FWL_age_hl2[[i]], 
                         variable_type = "SPIN", title = hl_classes[i])
})

plots_box_1_SPIN = grid.arrange(grobs = box_1_SPIN, ncol = 3)

ggsave(filename = paste0(mydir_figs,"box_1_SPIN", ".pdf"), 
       plot = plots_box_1_SPIN,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)


box_2_SPIN = lapply(seq_along(res_FWL_age_hl2), function(i) {
  plot_from_list_boxplot_2(res_FWL_age_hl2[[i]], 
                           variable_type = "SPIN", title = hl_classes[i])
})

plots_box_2_SPIN = grid.arrange(grobs = box_2_SPIN, ncol = 3)

ggsave(filename = paste0(mydir_figs,"box_2_SPIN", ".pdf"), 
       plot = plots_box_2_SPIN,
       width = 14,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)




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


