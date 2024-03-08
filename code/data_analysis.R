library(dplyr)
library(sp)
library(leaflet)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(naniar)
library(reticulate)
library(proxy)
library(pracma)
library(reshape2)
library(readxl)
library(janitor)
library(tibble)
library(viridis)
library(scales)
library(blandr)
library(irr)
library(MASS)
library(emg)
library(blandr)
library(ggpubr)
library(ggpmisc)
library(smplot2)
library(e1071)   
library(ggcorrplot)
library(rgl)
library(randomForest)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(tsne)
library(Rtsne)
library(BART)

###############################
# SET DIRECTORY AND READ FILE #
###############################

mydir<- "C:\\Users\\mcampi\\Desktop\\Hung_Joanna\\data\\"

data_ampl<- read_csv2(paste(mydir, "DataBase_Amplifon.csv", sep = ""),
                     col_names = T, quote=';') 


data = data_ampl
#data = read.csv2("DataBase_Amplifon.csv", header=T,sep=";")
# row.names(data) = data[,"CODANCL"]
# data = data[,-1]


#########################################################
# Step 1 (Filter) : Age criterion (exclusion of < 18 years old people)

filter_age = data[,"AGE"] > 18
Age_criterion = filter_age * 1

#########################################################
# Step 2 (Filter) : (A) Symmetry : difference max 15dB on 500, 1000 et 2000 Hz between ears

freq_left_low = c("FREQ_500_L", "FREQ_1000_L", "FREQ_2000_L") # Frequencies on left ear
freq_right_low = c("FREQ_500_R", "FREQ_1000_R", "FREQ_2000_R") # Frequencies on right ear
Criterion_Symmetric_A = rep(TRUE, length(data[,1]))
for (j in 1:length(freq_left_low)){
  Criterion_Symmetric_A = Criterion_Symmetric_A & (abs(data[,freq_left_low[j]] - data[,freq_right_low[j]]) <= 15)
}
# (B) Symmetry : difference max 20dB on 3000, 4000 et 6000 Hz between ears

freq_left_high = c("FREQ_3000_L", "FREQ_4000_L", "FREQ_6000_L") # Frequencies on left ear
freq_right_high = c("FREQ_3000_R", "FREQ_4000_R", "FREQ_6000_R") # Frequencies on right ear
Criterion_Symmetric_B = rep(TRUE, length(data[,1]))
for (j in 1:length(freq_left_high)){
  Criterion_Symmetric_B = Criterion_Symmetric_B & (abs(data[,freq_left_high[j]] - data[,freq_right_high[j]]) <= 20)
}
Criterion_Symmetric_B = Criterion_Symmetric_B * 1

Criterion_Symmetric = ((Criterion_Symmetric_A == 1) + (Criterion_Symmetric_B == 1)) == 2

#########################################################
# Step 3 (Filter) : VAS questions for the patient about discomfort in calm and noise (1 : no discomfort, 10 : high discomfort)

vas_noise_done = (!is.na(data[,"VAS_NOISE"])) # Patient answered to VAS in noise
vas_calm_done = (!is.na(data[,"VAS_CALM"])) # Patient answered to VAS in CALM
VAS = (vas_noise_done & vas_calm_done) * 1


#####################
data_criterion = cbind(Age_criterion, Criterion_Symmetric, VAS, data) # Add of criterions

Male = data_criterion[,"SEXE"] == "M"
Female = data_criterion[,"SEXE"] == "F"


#########################################################
# Step 4 (Flowchart) 


# 1 - All the data
length(data_criterion[,1]) # 81221 patients
sum(Male) # 40785 male patients
sum(Female) # 40436 female patients

# 2 - Exclusion : asymmetrical and aged less than 18 years old
sum((Age_criterion == 0)) # 330 patients aged less than 18 years old
sum((Criterion_Symmetric == 0)) # 26320 patients with asymmetrical hearing

sum((Age_criterion == 0)) # 330 patients aged less than 18 years old
sum((Age_criterion == 0) | (Criterion_Symmetric == 0)) # 26550 patients with asymmetrical hearing and aged less than 18 years old 

# 3 - Inclusion : 

# patients with VAS
idx = (Age_criterion == 1) & (Criterion_Symmetric == 1)
idx = idx & (VAS == 1) # 35091 patients with VAS 
sum(idx & Male) # 16527 male patients with VAS 
sum(idx & Female) # 18564 female patients with VAS 

# patients without VAS
idx = (Age_criterion == 1) & (Criterion_Symmetric == 1)
idx = idx & (VAS == 0) # 19580 patients with VAS 
sum(idx & Male) # 9202 male patients with VAS 
sum(idx & Female) # 10378 female patients with VAS 


# hearing of these patients
Filtering = (Age_criterion == 1) & (Criterion_Symmetric == 1)
data_select = data_criterion[Filtering,]

table(data_select[,"Classification"])
#  Mild          Moderate    Moderately severe            Severe            Slight 
# 17924            24969                  8116               734              2928 

#################################
# rename  and delete variables  #
#################################

data_select = data_select[,-c(4,5)]
colnames(data_select)

colnames(data_select)[3]  <- "VAS"
colnames(data_select)
colnames(data_select)[33]  <- "VAS_NOISE" 
colnames(data_select)



data_select$Classification = factor(data_select$Classification, 
                                    levels = c("Slight",
                                               "Mild",
                                               "Moderate",
                                               "Moderately severe",
                                               "Severe"))

################
#Missing values#  
################

vis_miss(data_select[1:10000,])
vis_miss(data_select[10000:20000,])
vis_miss(data_select[20000:30000,])
vis_miss(data_select[30000:54000,])


data_na<- na.omit(data_ampl)


###################
#PLOTs for Numbers#  
###################

range(data_ampl$AGE)

data_ampl = data_ampl %>% mutate(age_group = dplyr::case_when(AGE <= 10 ~ "0-10",
                                                  AGE > 10 & AGE <= 20 ~ "10-20",
                                                  AGE > 20 & AGE <= 30 ~ "20-30",
                                                  AGE > 30 & AGE <= 40 ~ "30-40",
                                                  AGE > 40 & AGE <= 50 ~ "40-50",
                                                  AGE > 50 & AGE <= 60 ~ "50-60",
                                                  AGE > 60 & AGE <= 70 ~ "60-70",
                                                  AGE > 70 & AGE <= 80 ~ "70-80",
                                                  AGE > 80 & AGE <= 90 ~ "80-90",
                                                  AGE > 90 & AGE <= 100 ~ "90-100",
                                                  AGE > 100 & AGE <= 100 ~ "100-110",
                                                  AGE > 110 ~ "110-120" ),
                                                  age_group = factor(
                                                    age_group,
                                                    level = c("0-10",
                                                              "10-20",
                                                              "20-30",
                                                              "30-40",
                                                              "40-50",
                                                              "50-60",
                                                              "60-70",
                                                              "70-80",
                                                              "80-90",
                                                              "90-100",
                                                              "100-110",
                                                              "110-120")  ))



data_ampl2 = data_ampl %>%
               group_by(age_group) %>%
                filter(!any(age_group == "0-10"))

data_ampl2$Classification = factor(data_ampl2$Classification, 
                                   levels = c("Slight",
                                              "Mild",
                                              "Moderate",
                                              "Moderately severe",
                                              "Severe"))
data_ampl2 <- na.omit(data_ampl2)

saveRDS(data_ampl2,
        file= "C:\\Users\\mcampi\\Desktop\\Hung_Joanna\\data\\data_ampl2bis.rds")

########################
# Correlation matrix  #
######################

Matrice_Corr = data_ampl2[,c("PTA","SRT","SNR")]
colnames(Matrice_Corr) = c("PTA","SRT","SNR")
Matrice_Corr = cor(Matrice_Corr)
Matrice_Corr  


ggcorrplot(Matrice_Corr, outline.col = "white",
           colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 8)



Matrice_Corr_class = data_ampl2[,c("Classification","PTA","SRT","SNR")] %>% group_split(Classification)
Matrice_Corr_class = lapply(1:length(Matrice_Corr_class), function(i) cor(Matrice_Corr_class[[i]][,c(2:4)]) )

gg1 = ggcorrplot(Matrice_Corr_class[[1]], outline.col = "white",
           colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
           title = "Slight")
gg2 = ggcorrplot(Matrice_Corr_class[[2]], outline.col = "white",
           colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
           title = "Mild")
gg3 = ggcorrplot(Matrice_Corr_class[[3]], outline.col = "white",
           colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
           title = "Moderate")
gg4 = ggcorrplot(Matrice_Corr_class[[4]], outline.col = "white",
           colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
           title = "Moderately severe")
gg5 = ggcorrplot(Matrice_Corr_class[[5]], outline.col = "white",
           colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
           title = "Severe")


ggarrange(gg1, gg2, gg3, gg4, gg5,
                           nrow = 2,
                           ncol = 3)


Matrice_Corr_class_puretone = data_ampl2[,c("Classification",
                                            "FREQ_125_L" ,"FREQ_250_L",
                                            "FREQ_500_L", "FREQ_750_L", 
                                            "FREQ_1000_L", "FREQ_1500_L" ,
                                            "FREQ_2000_L", "FREQ_3000_L",   
                                            "FREQ_4000_L",  "FREQ_6000_L",
                                            "FREQ_8000_L",
                                            "SRT","SNR")] 
colnames(Matrice_Corr_class_puretone)[2:12]<- c("F125" ,"F250",
                                                "F500", "F750", 
                                                "F1000", "F1500" ,
                                                "F2000", "F3000",   
                                                "F4000",  "F6000",
                                                "F8000")

Matrice_Corr_class_puretone = Matrice_Corr_class_puretone %>% group_split(Classification)

Matrice_Corr_class_puretone = lapply(1:length(Matrice_Corr_class_puretone), 
                                     function(i) cor(Matrice_Corr_class_puretone[[i]][,c(2:14)]) )




gg11 = ggcorrplot(Matrice_Corr_class_puretone[[1]], outline.col = "white",
                 colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
                 title = "Slight")
gg22 = ggcorrplot(Matrice_Corr_class_puretone[[2]], outline.col = "white",
                 colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
                 title = "Mild")
gg33 = ggcorrplot(Matrice_Corr_class_puretone[[3]], outline.col = "white",
                 colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
                 title = "Moderate")
gg44 = ggcorrplot(Matrice_Corr_class_puretone[[4]], outline.col = "white",
                 colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
                 title = "Moderately severe")
gg55 = ggcorrplot(Matrice_Corr_class_puretone[[5]], outline.col = "white",
                 colors = c("#6D9EC1", "white", "#E32636"),lab = TRUE,lab_size = 3,
                 title = "Severe")


ggarrange(gg11, gg22, gg33, gg44, gg55,
          nrow = 2,
          ncol = 3)


                           

vec_class = c("Slight", "Mild", "Moderate","Moderately severe","Severe")


##############
# histograms #  
##############


ggplot(data_ampl2,aes(x=Classification,fill=Classification)) +
  geom_bar() +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none") + xlab("Hearing Loss")+ ylab("")



ggplot(data_ampl2,aes(x=Classification,fill=Classification)) +
  geom_bar() + facet_wrap(~SEXE) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) + xlab("Classification groups")+ ylab("")


ggplot(data_ampl2,aes(x=Classification,fill=Classification)) +
  geom_bar() + facet_wrap(~age_group) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) + xlab("Classification groups")+ ylab("")


ggplot(data_ampl2,aes(x=Classification,fill=Classification)) +
  geom_bar() + facet_wrap(~age_group + SEXE) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) + xlab("Classification groups")+ ylab("")

ggplot(data_ampl2,aes(x=best_ear ,fill=Classification)) +
  geom_bar() + facet_wrap(~Classification ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) + xlab("Best ear")+ ylab("")



ggplot(data_ampl2,aes(x= as.factor(VAS_CALM),fill=Classification)) +
  geom_bar()  +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom") + xlab("VAS CALM")+ ylab("# Patients")

ggplot(data_ampl2,aes(x= as.factor(VAS_NOISE),fill=Classification)) +
  geom_bar()  +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom") + xlab("VAS NOISE")+ ylab("# Patients")




col_CALM = "green4"
COL_BRUIT = "orange"
    
  
data_select_VAS = data_select[data_select[,"VAS"] == 1,]
data_select_VAS = data_select_VAS[(data_select_VAS[,"VAS_NOISE"] > 0),]
data_select_VAS[,"VAS_NOISE"] = as.factor(data_select_VAS[,"VAS_NOISE"])
data_select_VAS = data_select_VAS[(data_select_VAS[,"VAS_CALM"] > 0),]
data_select_VAS[,"VAS_CALM"] = as.factor(data_select_VAS[,"VAS_CALM"])
  
ggplot(data = data_select_VAS, aes(x = VAS_CALM)) +
    geom_bar(width = 0.5, fill = col_CALM) +
    scale_x_discrete("Auditory discomfort in quiet (VAS)") +
    scale_y_continuous("Number of patients", 
                       limits = c(0,9000), 
                       breaks = seq(0,9000,1000)) +
    theme_bw() +
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text( size=10, angle = 90 ),
          axis.text.y = element_text( size=10),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          legend.position = "bottom") 
  
 ggplot(data = data_select_VAS, aes(x = VAS_NOISE)) +
    geom_bar(width = 0.5, fill = COL_BRUIT) +
    scale_x_discrete("Auditory discomfort in noise (VAS)") +
    scale_y_continuous("Number of patients", 
                       limits = c(0,9000), 
                       breaks = seq(0,9000,1000)) +
    theme_bw() +
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text( size=10, angle = 90 ),
          axis.text.y = element_text( size=10),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          legend.position = "bottom") 
  
 
 
 ggplot(data = data_select_VAS, aes(x = VAS_CALM)) +
   geom_bar(width = 0.5, fill = col_CALM) +
   facet_wrap(~Classification) +
   scale_x_discrete("Auditory discomfort in quiet (VAS)") +
   scale_y_continuous("Number of patients", 
                      limits = c(0,9000), 
                      breaks = seq(0,9000,1000)) +
   theme_bw() +
   theme(axis.line = element_line(color='black'),
         plot.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text( size=10, angle = 90 ),
         axis.text.y = element_text( size=10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.position = "bottom",
         strip.text = element_text(color = 'black'),
         strip.background = element_rect(fill = 'white'))
 
 ggplot(data = data_select_VAS, aes(x = VAS_NOISE)) +
   geom_bar(width = 0.5, fill = COL_BRUIT)  +
   facet_wrap(~Classification) +
   scale_x_discrete("Auditory discomfort in noise (VAS)") +
   scale_y_continuous("Number of patients", 
                      limits = c(0,9000), 
                      breaks = seq(0,9000,1000)) +
   theme_bw() +
   theme(axis.line = element_line(color='black'),
         plot.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text( size=10, angle = 90 ),
         axis.text.y = element_text( size=10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.position = "bottom",
         strip.text = element_text(color = 'black'),
         strip.background = element_rect(fill = 'white')) 


###############
#   scatter   #  
###############

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


ggplot(data_ampl2, aes(x=SRT, y=PTA, fill = Classification)) +
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


############
#   Lines  #   
############


temp = data_ampl2[,c(4,7:17)] %>% filter(Classification == "Moderately severe")
temp$id = seq(1, nrow(temp), by = 1)
temp = melt(temp[,c(2:13)], id.vars = "id")

ggplot(temp, aes(x=variable, 
                 y=value, 
                 group=id  , color = as.factor(id)))  +
           geom_point() + geom_line()+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 45, vjust = 0.5 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none") + xlab("Frequencies")+ ylab("")

###############
###############
#   boxplots  #  
###############
###############


ggplot(data_ampl2, aes(x=Classification, y=PTA, fill=Classification)) + 
  geom_boxplot()  + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none")

ggplot(data_ampl2, aes(x=Classification, y=SNR, fill=Classification)) + 
  geom_boxplot()  + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none")

ggplot(data_ampl2, aes(x=Classification, y=SRT, fill=Classification)) + 
  geom_boxplot()  + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none")

ggplot(data_ampl2, aes(x=Classification, y=PTA, fill=Classification)) + 
  geom_boxplot()  + facet_wrap(~age_group  ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +xlab('HL Class')

ggplot(data_ampl2, aes(x=age_group, y=PTA, fill=Classification)) + 
  geom_boxplot()  + facet_wrap(~Classification  ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +xlab('Age Group')



ggplot(data_ampl2, aes(x=Classification, y=SRT, fill=Classification)) + 
  geom_boxplot()  + facet_wrap(~age_group  ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +xlab('HL Class')


ggplot(data_ampl2, aes(x=age_group, y=SRT, fill=Classification)) + 
  geom_boxplot()  + facet_wrap(~Classification  ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +xlab('Age Group')



ggplot(data_ampl2, aes(x=Classification, y=SNR, fill=Classification)) + 
  geom_boxplot()  + facet_wrap(~age_group ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +xlab('HL Class')

ggplot(data_ampl2, aes(x=age_group, y=SNR, fill=Classification)) + 
  geom_boxplot()  + facet_wrap(~Classification ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +xlab('Age Group')



ggplot(data = data_select_VAS, aes(x = VAS_CALM, y = SRT)) +
     geom_boxplot(fill = col_CALM, width = 0.2) +
     scale_x_discrete("Auditory discomfort in quiet (VAS)") +
     scale_y_continuous("SRT (dB HL)", 
                        limits = c(0,80),
                        breaks = seq(0,80,10)) +
     theme_bw() +
     theme(axis.line = element_line(color='black'),
           plot.background = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text( angle = 90 ),
           legend.position = "none")



ggplot(data = data_select_VAS, aes(x = VAS_NOISE, y = SNR)) +
  geom_boxplot(fill = COL_BRUIT, width = 0.2) +
  scale_x_discrete("Auditory discomfort in noise (VAS)") +
  scale_y_continuous("SNR (dB)", 
                     limits = c(-10,20),
                     breaks = seq(-10,20,5)) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none")


#order them according to the order of the slight etc.

ggplot(data = data_select_VAS, aes(x = VAS_CALM, y = SRT)) +
  geom_boxplot(fill = col_CALM, width = 0.2) + 
  facet_wrap(~as.factor(Classification)) +
  scale_x_discrete("Auditory discomfort in quiet (VAS)") +
  scale_y_continuous("SRT (dB HL)", 
                     limits = c(0,80),
                     breaks = seq(0,80,10)) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))



ggplot(data = data_select_VAS, aes(x = VAS_NOISE, y = SNR)) + 
  facet_wrap(~as.factor(Classification)) +
  geom_boxplot(fill = COL_BRUIT, width = 0.2) +
  scale_x_discrete("Auditory discomfort in noise (VAS)") +
  scale_y_continuous("SNR (dB)", 
                     limits = c(-10,20),
                     breaks = seq(-10,20,5)) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( angle = 90 ),
        legend.position = "none",
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'))



#########
#########
## PCA ##
#########
#########

colnames(data_ampl2)

data_ampl2_left = data_ampl2[, c(4,3,5,7:17,30:33)]
colnames(data_ampl2_left)
colnames(data_ampl2_left)[4:14]<- c("F125","F250","F500","F750","F1000","F1500","F2000","F3000","F4000","F6000","F8000")
colnames(data_ampl2_left)



data_ampl2_left = data_ampl2_left %>% 
  mutate(
    # Create categories
     color = dplyr::case_when(
      Classification == "Slight"  ~ "blue",
      Classification == "Mild"  ~ "yellow",
      Classification == "Moderate"  ~ "gray",
      Classification == "Moderately severe"  ~ "red",
      Classification == "Severe"  ~ "orange"
     ),
    # Convert to factor
    color = factor(
      color,
      level = c("blue","yellow", "gray", "red", "orange")
    )
  )





corMat_pt <- cor(data_ampl2_left[c(4:14)])
corrplot(corMat_pt, order = "hclust")

corMat_sp <- cor(data_ampl2_left[c(15:16)])
corrplot(corMat_sp, order = "hclust")

corMat_all <- cor(data_ampl2_left[c(4:16)])
corrplot(corMat_all, order = "hclust")


res.pca.pt <- PCA(data_ampl2_left[c(4:14)], scale.unit=TRUE, graph = T)
res.pca.sp <- PCA(data_ampl2_left[c(15:16)], scale.unit=TRUE, graph = T)
res.pca.all <- PCA(data_ampl2_left[c(4:16)], scale.unit=TRUE, graph = T)


#This line of code will sort the variables the most linked to each PC.
dimdesc(res.pca.pt)
dimdesc(res.pca.sp)
dimdesc(res.pca.all)



summary(res.pca.pt)
summary(res.pca.sp)
summary(res.pca.all)


######################
# Graph of variables #
######################

#Eigenvalues / Variances
fviz_eig(res.pca.pt, addlabels = TRUE)
fviz_eig(res.pca.sp, addlabels = TRUE)
fviz_eig(res.pca.all, addlabels = TRUE)


#Quality of representation
var_pt <- get_pca_var(res.pca.pt)
var_sp <- get_pca_var(res.pca.sp)
var_all <- get_pca_var(res.pca.all)


corrplot(var_pt$cos2, is.corr=FALSE) #to check with dimdesc and te one below
corrplot(var_sp$cos2, is.corr=FALSE) #to check with dimdesc and te one below
corrplot(var_all$cos2, is.corr=FALSE) #to check with dimdesc and te one below


fviz_pca_var(res.pca.pt, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

fviz_pca_var(res.pca.sp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

fviz_pca_var(res.pca.all, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)



#Contributions of variables to PCs
fviz_contrib(res.pca.pt, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca.sp, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca.all, choice = "var", axes = 1, top = 10)



#######################
# Graph of individuals
#######################


fviz_pca_biplot(res.pca.pt, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = data_ampl2_left$Classification,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
                
                legend.title = list(fill = "Classification", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors





fviz_pca_biplot(res.pca.sp, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = data_ampl2_left$Classification,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
                
                legend.title = list(fill = "Classification", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors



fviz_pca_biplot(res.pca.all, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = data_ampl2_left$Classification,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
                
                legend.title = list(fill = "Classification", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors


corMat_left_redfreq <- cor(data_ampl2_left[c(8,10,12)])
corrplot(corMat_left_redfreq, order = "hclust")


res.pca.left.redfreq <- PCA(data_ampl2_left[c(8,10,12)], scale.unit=TRUE, graph = T)


fviz_pca_biplot(res.pca.left.redfreq, 
                # Individuals
                geom.ind = "point",
                fill.ind = data_ampl2_left$Classification, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Classification", color = "Contrib",
                                    alpha = "Contrib")
)






