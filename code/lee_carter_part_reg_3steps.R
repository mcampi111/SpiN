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


###############################
# SET DIRECTORY AND READ FILE #
###############################

mydir<- "C:\\Users\\mcampi\\Desktop\\Hung_Joanna\\data\\"
mydir2<- "C:\\Users\\mcampi\\Desktop\\Hung_Joanna\\code\\"


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


data_lee_carter = data_ampl2[,c(3,4,7:17,34,35,36)] #Left ear
#data_lee_carter = data_ampl2[,c(3,4,18:28,34,35,36)] #Right ear


apply(data_ampl2[,c(7:17)], 2, range)
apply(data_ampl2[,c(18:28)], 2, range)


spiq =  data_ampl2[,c(3,4,30,34,35,36)]

apply(data_ampl2[,c(30)], 2, range)

spin =  data_ampl2[,c(3,4,31,34,35,36)]

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
decision_threshold_un = unique(decision_threshold)

quant_tresh <- c(0.2, 0.4, 0.6, 0.8, 0.9)
#quant_tresh2 <- c(0.25, 0.45, 0.65, 0.85, 0.95)

quant_PTA <- quantile(decision_threshold_un$PTA, probs = quant_tresh, na.rm = TRUE)
quant_SRT <- quantile(decision_threshold_un$SRT, probs = quant_tresh, na.rm = TRUE)
quant_SNR <- quantile(decision_threshold_un$SNR, probs = quant_tresh, na.rm = TRUE)

tres_pta_snr = data.frame(PTA = quant_PTA, SRT = quant_SRT)
tres_pta_srt = data.frame(PTA = quant_PTA, SNR = quant_SNR)
tres_srt_snr = data.frame(SRT = quant_SRT, SNR = quant_SNR)

tres_pta_snr2 <- data.frame(X = as.numeric(tres_pta_snr[,1]), Y = as.numeric(tres_pta_snr[,2]))  
tres_pta_srt2 <- data.frame(X = as.numeric(tres_pta_srt[,1]), Y = as.numeric(tres_pta_srt[,2]))  
tres_srt_snr2 <- data.frame(X = as.numeric(tres_srt_snr[,1]), Y = as.numeric(tres_srt_snr[,2]))  


treshold_hearing_rate = as.integer(tres_pta_snr[,1])
n_tresh = length(treshold_hearing_rate)

treshold_SPIQ = as.integer(tres_pta_snr[,2])
treshold_SPIN = as.integer(tres_pta_srt[,2])


############################
# VISUALISATION Thresholds #
############################

ggplot(data_ampl2, aes(x = PTA, y = SRT)) +
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



ggplot(data_ampl2, aes(x = PTA, y = SNR)) +
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

hr_rates <- process_data(data_lee_carter, 
                         treshold_hearing_rate, 
                         age_group_var = age_var,
                         n_tresh, n_age,
                         n_hlclasses)


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


plots_hr_rates = plot_hr_rates(hr_rates_interp,
                               hl_classes, 
                               n_tresh,
                               age_group_var,
                               treshold_hearing_rate) 
plots_hr_rates[[1]]
plots_hr_rates[[2]]




plots_spiq_rates = plot_speech_rates(spiq_rates,
                                     hl_classes, 
                                     n_tresh,
                                     age_group_var,
                                     treshold_SPIQ,
                                     "SpiQ") 

plots_spiq_rates[[1]]
plots_spiq_rates[[2]]



plots_spin_rates = plot_speech_rates(spin_rates,
                                     hl_classes, 
                                     n_tresh,
                                     age_group_var,
                                     treshold_SPIN,
                                     "SpiN") 

plots_spin_rates[[1]]
plots_spin_rates[[2]]




heatmaps_rates = plot_heat_rates(hr_rates_interp,spiq_rates, spin_rates,
                                 hl_classes, freq_vector, n_tresh,
                                 age_group_var, treshold_hearing_rate,
                                 treshold_SPIN, treshold_SPIQ)


heatmaps_rates[[1]]
heatmaps_rates[[2]]

########################
########################
# STEP 1: LEE - CARTER #
########################
########################

##################################
# LEE - CARTER model by age only #
##################################


demodata_byhl = lapply(1:n_tresh, function(i)
  demogdata(data = hr_rates_interp[[1]][[i]],  
            pop = hr_rates_interp[[1]][[i]]*treshold_hearing_rate[i], 
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



demodata_by_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    demogdata(data = hr_rates_interp[[2]][[i]][[j]],    
              pop = hr_rates_interp[[2]][[i]][[j]]*treshold_hearing_rate[i], 
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



plot(forecast(model_by_age_hl[[1]][[1]]))
plot(forecast(model_by_age_hl[[1]][[2]]))
plot(forecast(model_by_age_hl[[1]][[3]]))
plot(forecast(model_by_age_hl[[1]][[4]]))
plot(forecast(model_by_age_hl[[1]][[5]]))



########################################################
########################################################
#  SOLUTION 1) REGRESS X2 ON X1 + RESIDUALS REGRESSION #
########################################################
########################################################


############################
############################
# STEP 2) REGRESS X2 ON X1 #
############################
############################

#####################
# Model by age only #
#####################


x1_hl = lapply(1:n_tresh, function(i) 
  as.numeric(model_hl[[i]]$bx) %*% t(as.numeric(model_hl[[i]]$kt)))


partial_step2_hl = lapply(1:n_tresh, function(i) 
  lm(x1_hl[[i]] ~ cbind(spiq_rates[[1]][[i]], 
                        spin_rates[[1]][[i]]) ))

res_step2_hl = process_summary(partial_step2_hl, 
                               n_tresh, freq_vector, treshold_SPIQ,
                               treshold_SPIN)


melt_est_pstep2_spiq_hl = melt(res_step2_hl$est_pstep2_spiq_hl, id.vars = "SPIQ_tresh")
melt_p_pstep2_spiq_hl = melt(res_step2_hl$p_pstep2_spiq_hl, id.vars = "SPIQ_tresh")
melt_pstep2_spiq_hl = melt_est_pstep2_spiq_hl
melt_pstep2_spiq_hl$color = melt_p_pstep2_spiq_hl$value

melt_pstep2_spiq_hl$color2 <- ifelse(melt_pstep2_spiq_hl$color < 0.01, "azure3",
                                     ifelse(melt_pstep2_spiq_hl$color <= 0.05, "cyan", "blue"))
melt_pstep2_spiq_hl$color2 <- factor(melt_pstep2_spiq_hl$color2, levels = c("azure3", "cyan", "blue"))

# Plotting with manual color scale
ggplot(melt_pstep2_spiq_hl, aes(x = as.factor(variable), 
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
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    plot.margin = margin(0, 0, 0, 0)  # Remove space around the plot
  ) +
  labs(fill = "Significance",  
       x = "Frequencies",  
       y = "SPIQ threshold")


melt_est_pstep2_spin_hl = melt(res_step2_hl$est_pstep2_spin_hl, id.vars = "SPIN_tresh")
melt_p_pstep2_spin_hl = melt(res_step2_hl$p_pstep2_spin_hl, id.vars = "SPIN_tresh")
melt_pstep2_spin_hl = melt_est_pstep2_spin_hl
melt_pstep2_spin_hl$color = melt_p_pstep2_spin_hl$value

melt_pstep2_spin_hl$color2 <- ifelse(melt_pstep2_spin_hl$color < 0.01, "azure3",
                                     ifelse(melt_pstep2_spin_hl$color <= 0.05, "cyan", "blue"))
melt_pstep2_spin_hl$color2 <- factor(melt_pstep2_spin_hl$color2, levels = c("azure3", "cyan", "blue"))

# Plotting with manual color scale
ggplot(melt_pstep2_spin_hl, aes(x = as.factor(variable), 
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
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    plot.margin = margin(0, 0, 0, 0)  # Remove space around the plot
  ) +
  labs(fill = "Significance",  
       x = "Frequencies",  
       y = "SPIN threshold")




#########################################
# Model by degree of hearing loss & Age # 
#########################################

x1_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)
    as.numeric(model_by_age_hl[[i]][[j]]$bx) %*% t(as.numeric(model_by_age_hl[[i]][[j]]$kt)) ))

partial_step2_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j) 
    lm(x1_age_hl[[i]][[j]] ~ cbind(spiq_rates[[2]][[i]][[j]], 
                                   spin_rates[[2]][[i]][[j]]) )) )

res_step2_age_hl = lapply(1:n_tresh, function(i)
  process_summary(partial_step2_age_hl[[i]],
                  n_hlclasses, freq_vector,
                  treshold_SPIQ,
                  treshold_SPIN))

# Create plots for each class
plots_list_spiq <- lapply(seq_along(res_step2_age_hl), function(i) {
  plot_from_list(res_step2_age_hl[[i]], variable_type = "SPIQ", title = hl_classes[i])
})


plots_list_spin <- lapply(seq_along(res_step2_age_hl), function(i) {
  plot_from_list(res_step2_age_hl[[i]], variable_type = "SPIN", title = hl_classes[i])
})

grid.arrange(grobs = plots_list_spiq, ncol = 3)
grid.arrange(grobs = plots_list_spin, ncol = 3)


############################
############################
# STEP 3) REGRESS eF ON eX #
############################
############################

##################################################################################
#QUESTION OF HOW MANY WE SHOULD REGULARISE HERE 
#For now I regularise only one and i follow the second example in this code

# lambda <- 1/kappa.lm(lm(model_hl[[1]]$residuals$y ~ partial_step2_hl[[1]]$residuals))
# svd_result <- svd(partial_step2_hl[[1]]$residuals)
# regularized_singular_values <- svd_result$d / (svd_result$d^2 + lambda^2)
# regularized_matrix <- svd_result$u %*% diag(regularized_singular_values) %*% t(svd_result$v)
# 
# summary(lm(model_hl[[1]]$residuals$y ~ regularized_matrix))
# 
# lambda <- 1/kappa(partial_step2_hl[[1]]$residuals)
# svd_result <- svd(partial_step2_hl[[1]]$residuals)
# regularized_singular_values <- svd_result$d / (svd_result$d^2 + lambda^2)
# regularized_matrix <- svd_result$u %*% diag(regularized_singular_values) %*% t(svd_result$v)
# 
# summary(lm(model_hl[[1]]$residuals$y ~ regularized_matrix))
# 
# 
# lambda <- 1/kappa(partial_step2_hl[[1]]$residuals)
# svd_result <- svd(partial_step2_hl[[1]]$residuals)
# regularized_singular_values <- svd_result$d / (svd_result$d^2 + lambda^2)
# regularized_matrix <- svd_result$u %*% diag(regularized_singular_values) %*% t(svd_result$v)
# 
# lambda2 <- 1/kappa(model_hl[[1]]$residuals$y)
# svd_result2 <- svd(model_hl[[1]]$residuals$y)
# regularized_singular_values2 <- svd_result2$d / (svd_result2$d^2 + lambda2^2)
# regularized_matrix2 <- svd_result2$u %*% diag(regularized_singular_values2) %*% t(svd_result2$v)
# 
# summary(lm(regularized_matrix2 ~ regularized_matrix))
####################################################################################

##################################
# LEE - CARTER model by age only #
##################################

#Ex regularised only
reg_res_matrices = lapply(1:n_tresh, function(i) reg_mat_function(partial_step2_hl[[i]]$residuals))

res_reg_hl = lapply(1:n_tresh, function(i)
  lm(model_hl[[i]]$residuals$y ~ reg_res_matrices[[i]]))


#Ex and Ey regularised 
reg_leecarter_res_hl = lapply(1:n_tresh, function(i) reg_mat_function(model_hl[[i]]$residuals$y))

res_reg_hl = lapply(1:n_tresh, function(i)
  lm(reg_leecarter_res_hl[[i]] ~ reg_res_matrices[[i]]))


sum_res_hl = lapply(1:n_tresh, function(i) summary(res_reg_hl[[i]]))


est_res_hl = lapply(1:n_tresh, function(i) 
  as.data.frame(do.call(rbind,lapply(sum_res_hl[[i]], 
                                     function(model_output) coef(model_output)[, "Estimate"][2:(n_freq+1)]))) )

for(i in 1:n_tresh){colnames(est_res_hl[[i]]) = freq_vector }

for(i in 1:n_tresh){est_res_hl[[i]]$HLtresh = treshold_hearing_rate[i]
est_res_hl[[i]]$SPIQ_tresh = treshold_SPIQ[i]
est_res_hl[[i]]$SPIN_tresh = treshold_SPIN[i]
est_res_hl[[i]]$frequency = freq_vector
}

est_res_hl = do.call(rbind, est_res_hl)


p_res_hl = lapply(1:n_tresh, function(i) 
  as.data.frame(round(do.call(rbind,lapply(sum_res_hl[[i]], 
                                           function(model_output) 
                                             coef(model_output)[, "Pr(>|t|)"][2:(n_freq+1)])),3) ) )

for(i in 1:n_tresh){colnames(p_res_hl[[i]]) = freq_vector }

for(i in 1:n_tresh){p_res_hl[[i]]$HLtresh = treshold_hearing_rate[i]
p_res_hl[[i]]$SPIQ_tresh = treshold_SPIQ[i]
p_res_hl[[i]]$SPIN_tresh = treshold_SPIN[i]
p_res_hl[[i]]$frequency = freq_vector
}

p_res_hl = do.call(rbind, p_res_hl)



melt_est_res_hl = melt(est_res_hl, id.vars = c("HLtresh", "SPIQ_tresh", "SPIN_tresh", "frequency"))
melt_p_res_hl = melt(p_res_hl, id.vars = c("HLtresh", "SPIQ_tresh", "SPIN_tresh", "frequency"))
melt_res_hl = melt_est_res_hl
melt_res_hl$color = melt_p_res_hl$value

melt_res_hl$color2 <- ifelse(melt_res_hl$color < 0.01, "azure3",
                             ifelse(melt_res_hl$color <= 0.05, "cyan", "blue"))
melt_res_hl$color2 <- factor(melt_res_hl$color2, levels = c("azure3", "cyan", "blue"))

melt_res_hl$facet_label <- paste("HLtresh:", melt_res_hl$HLtresh, 
                                 "SPIQ_tresh:", melt_res_hl$SPIQ_tresh,
                                 "SPIN_tresh:", melt_res_hl$SPIN_tresh)

ggplot(melt_res_hl, aes(x = as.factor(variable), 
                        y = as.factor(as.numeric(frequency)), 
                        fill = color2,
                        label = round(value*10^14, 1))) + #round(value, 1))) +
  geom_tile()  + facet_wrap(~facet_label, ncol = 3, scales = "free") +
  geom_text(vjust = 1) +
  scale_fill_manual(values = c("azure3" = "azure3", 
                               "cyan" = "cyan", 
                               "blue" = "blue"),
                    labels = c("< 0.01", 
                               "0.01 < & < 0.05", 
                               " > 0.05")) +  # Specify legend labels
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    plot.margin = margin(0, 0, 0, 0)  # Remove space around the plot
  ) +
  labs(fill = "Significance",  
       x = "Frequencies",  
       y = "Coeff. Estimates per Frequency")



#########################################
# Model by degree of hearing loss & Age # 
#########################################

#Ex regularised only
reg_res_age_matrices = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j) 
    reg_mat_function(partial_step2_age_hl[[i]][[j]]$residuals)) )

res_reg_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j) 
    lm(model_by_age_hl[[i]][[j]]$residuals$y ~ reg_res_age_matrices[[i]][[j]])) )

#Ex and Ey regularised 
# reg_leecarter_res_age_hl = lapply(1:n_tresh, function(i)
#   lapply(1:n_hlclasses, function(j)
#     reg_mat_function(model_by_age_hl[[i]][[j]]$residuals$y)) )
# 
# 
# res_reg_age_hl = lapply(1:n_tresh, function(i)
#   lapply(1:n_hlclasses, function(j)
#     lm(reg_leecarter_res_age_hl[[i]][[j]] ~ reg_res_age_matrices[[i]][[j]])) )


sum_res_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)  
    summary(res_reg_age_hl[[i]][[j]])))

est_res_age_hl = lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j) 
    as.data.frame(do.call(rbind,lapply(sum_res_age_hl[[i]][[j]], 
                                       function(model_output) 
                                         coef(model_output)[, "Estimate"][2:(n_freq+1)]))) ))

for(i in 1:n_tresh){ for(j in 1:n_hlclasses){ colnames(est_res_age_hl[[i]][[j]]) = freq_vector }}

for(i in 1:n_tresh){ for(j in 1:n_hlclasses){ 
  est_res_age_hl[[i]][[j]]$HLtresh = treshold_hearing_rate[i]
  est_res_age_hl[[i]][[j]]$SPIQ_tresh = treshold_SPIQ[i]
  est_res_age_hl[[i]][[j]]$SPIN_tresh = treshold_SPIN[i]
  est_res_age_hl[[i]][[j]]$frequency = freq_vector
  est_res_age_hl[[i]][[j]]$HL_Loss = hl_classes[j]
  
}}

est_res_age_hl2 = list()

for(i in 1:n_tresh){
  
  est_res_age_hl2[[i]] = do.call(rbind, est_res_age_hl[[i]])
  
}


p_res_age_hl =  lapply(1:n_tresh, function(i)
  lapply(1:n_hlclasses, function(j)  
    as.data.frame(round(do.call(rbind,lapply(sum_res_age_hl[[i]][[j]], 
                                             function(model_output) 
                                               coef(model_output)[, "Pr(>|t|)"][2:(n_freq+1)])),3) ) ))

for(i in 1:n_tresh){ for(j in 1:n_hlclasses){ colnames(p_res_age_hl[[i]][[j]]) = freq_vector }}

for(i in 1:n_tresh){ for(j in 1:n_hlclasses){ 
  p_res_age_hl[[i]][[j]]$HLtresh = treshold_hearing_rate[i]
  p_res_age_hl[[i]][[j]]$SPIQ_tresh = treshold_SPIQ[i]
  p_res_age_hl[[i]][[j]]$SPIN_tresh = treshold_SPIN[i]
  p_res_age_hl[[i]][[j]]$frequency = freq_vector
  p_res_age_hl[[i]][[j]]$HL_Loss = hl_classes[j]
  
}}

p_res_age_hl2 = list()

for(i in 1:n_tresh){
  
  p_res_age_hl2[[i]] = do.call(rbind, p_res_age_hl[[i]])
  
}


melt_res_age_hl <- Map(process_est_res_age_hl2, est_res_age_hl2,
                       p_res_age_hl2)

for (i in seq_along(melt_res_age_hl)) {
  melt_res_age_hl[[i]]$HL_Loss = factor(melt_res_age_hl[[i]]$HL_Loss, levels = hl_classes)
}



plots_res_age_hl <- lapply(melt_res_age_hl, plot_melt_res_hl, Ex_regularized = T, 
                           Ey_regularized = T)

plots_res_age_hl





