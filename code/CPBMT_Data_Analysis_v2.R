library(dplyr)
library(sp)
library(leaflet)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(naniar)
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
library(ggpubr)
library(ggpmisc)
library(smplot2)
library(ggcorrplot)
library(randomForest)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(tsne)
library(Rtsne)

###############################
# SET DIRECTORY AND READ FILE #
###############################

source(".code/utils.R")

mydir <- "./data/"
mydir2 <- "./code/"
mydir_figs <- "./code/figs/"

result <- prepare_data(mydir, mydir2, mydir_figs)


list2env(result, envir = .GlobalEnv)



###########################################
# CHOOSE AGE STEP TO CONDUCT THE ANALYSIS œ#
###########################################
#Select both of them below i.e; age_var and age_grup_var

age_var = "age_group2" #"age_group" #"age_group2"   "age_group3"
age_group_var = age_groups2 #all_age_groups #age_groups2  age_groups
n_age = length(age_group_var)



###################
#DESCRIPTIVE PLOTS#  
###################


##############
# barplots #  
##############


data_ampl2_hist0<- data_lee_carter %>% 
  ungroup() %>%
  count(Classification) %>% 
  mutate(percent = n/sum(n), n = n)


color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                          unique(data_ampl2_hist0$Classification))

data_ampl2_hist0 <- data_ampl2_hist0 %>%
  mutate(color = color_mapping[Classification])


#counf ot PEOPLE by age
data_ampl2_hist1<- data_lee_carter %>% 
  ungroup() %>%
  count(Classification ,age_group2) %>% 
  mutate(percent = n/sum(n), n = n)

color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                          unique(data_ampl2_hist1$Classification))

data_ampl2_hist1 <- data_ampl2_hist1 %>%
  mutate(color = color_mapping[Classification])


#counf ot PEOPLE by sex 0
data_ampl2_hist_sex_only<- data_lee_carter %>% 
  ungroup() %>%
  count(SEXE) %>% 
  mutate(percent = n/sum(n), n = n)

pdf( paste(mydir_figs, "hist_people_sex.pdf", sep = ""))  
ggplot(data_ampl2_hist_sex_only,
       aes(x = SEXE, y = n, fill = SEXE)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE),
             vjust = 0.5,show.legend = FALSE, size = 7)  +
  #scale_fill_grey() +
  xlab("Sex") + ylab("Participants")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_fill_manual(values = c("F" = "pink", "M" = "blue"))
dev.off()


#counf ot PEOPLE by sex 1
data_ampl2_hist2<- data_lee_carter %>% 
  ungroup() %>%
  count(Classification ,SEXE) %>% 
  mutate(percent = n/sum(n), n = n)

color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                          unique(data_ampl2_hist2$Classification))

data_ampl2_hist2 <- data_ampl2_hist2 %>%
  mutate(color = color_mapping[Classification])



#histrograms 
#1) by condition
pdf( paste(mydir_figs, "hist_people_hl.pdf", sep = ""))  
ggplot(data_ampl2_hist0,
       aes(x = Classification, y = n, fill = Classification)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE),
             vjust = 0.5,show.legend = FALSE, size = 7)  +
  #scale_fill_grey() +
  xlab("Hearing Loss") + ylab("Participants")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_fill_manual(values=data_ampl2_hist0$color) 

dev.off()



#2) by condition + age
pdf( paste(mydir_figs, "hist_people_hl_age.pdf", sep = ""), width = 12, height = 7) 
ggplot(data_ampl2_hist1,
       aes(x = Classification, y = n, fill = Classification)) + 
  geom_col(position = position_stack(reverse = TRUE)) + 
  facet_wrap(~age_group2, ncol = 5, scales = "free_y") +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = FALSE),
             vjust = 0.5,show.legend = FALSE, size = 3)  +
  #scale_fill_grey() +
  xlab("Hearing Loss") + ylab("Participants")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  scale_fill_manual(values=data_ampl2_hist0$color) 

dev.off()


#2) by condition + sex
pdf( paste(mydir_figs, "hist_people_hl_sex.pdf", sep = ""), width = 12, height = 7) 
ggplot(data_ampl2_hist2,
       aes(x = Classification, y = n, fill = Classification)) + 
  geom_col(position = position_stack(reverse = TRUE)) + 
  facet_wrap(~SEXE, ncol = 5, scales = "free_y") +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = FALSE),
             vjust = 0.5,show.legend = FALSE, size = 3)  +
  #scale_fill_grey() +
  xlab("Hearing Loss") + ylab("Participants")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  scale_fill_manual(values=data_ampl2_hist0$color) 

dev.off()






###############
#   scatter   #  
###############

ggplot(data_lee_carter, aes(x=PTA, y=SNR, fill = Classification)) +
  geom_point(size=2, shape=23) +
  theme_bw() +
  scale_fill_manual(values = hearing_loss_col, name = "Hearing Loss") + 
  theme(axis.line = element_line(color='black'),
                     plot.background = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.text.x = element_text( size=10, angle = 90 ),
                     axis.text.y = element_text( size=10),
                     axis.title.x = element_text(size = 15),
                     axis.title.y = element_text(size = 15),
                     legend.position = "bottom") 

ggplot(data_ampl2, aes(x=PTA, y=SRT, fill = Classification)) +
  geom_point(size=2, shape=23) +
  scale_fill_manual(values = hearing_loss_col, name = "Hearing Loss") + 
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


ggplot(data_ampl2, aes(x=SRT, y=SNR, fill = Classification)) +
  geom_point(size=2, shape=23) +
  theme_bw() +
  scale_fill_manual(values = hearing_loss_col, name = "Hearing Loss") + 
  theme(axis.line = element_line(color='black'),
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

#by degree of hearing loss only
bxpl_data_ampl2 = melt(data_ampl2[,c(4,7:28,30,31)],
                       id.vars = "Classification")

bxpl_data_ampl2 <- bxpl_data_ampl2 %>%
  mutate(Test_Type = case_when(
    grepl("_L", variable) ~ "Audiogram_L",
    grepl("_R", variable) ~ "Audiogram_R",
    TRUE ~ "Speech_Tests"
  ))

pdf( paste(mydir_figs, "box_audio_left_hl.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2, Test_Type == "Audiogram_L"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~Classification, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Left Audiogram") 
dev.off()


pdf( paste(mydir_figs, "box_audio_right_hl.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2, Test_Type == "Audiogram_R"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~Classification, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Right Audiogram") 
dev.off()

pdf( paste(mydir_figs, "box_speech_hl.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2, Test_Type == "Speech_Tests"), 
       aes(x = variable, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~Classification, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Speech Tests") 
dev.off()


#by age only

bxpl_data_ampl2_ageonly = melt(data_ampl2[,c(7:17,30,31,35)],
                               id.vars = "age_group2")

bxpl_data_ampl2_ageonly <- bxpl_data_ampl2_ageonly %>%
  mutate(Test_Type = case_when(
    grepl("_L", variable) ~ "Audiogram_L",
    TRUE ~ "Speech_Tests"
  ))



pdf( paste(mydir_figs, "box_audio_ageonly.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_ageonly, Test_Type == "Audiogram_L"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~age_group2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Left Audiogram") 
dev.off()




pdf( paste(mydir_figs, "box_speech_ageonly.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_ageonly, Test_Type == "Speech_Tests"), 
       aes(x = variable, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~age_group2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Speech Tests") 
dev.off()


#by sex only
bxpl_data_ampl2_sexonly = melt(data_ampl2[,c(7:17,30,31,29)],
                       id.vars = "SEXE")

bxpl_data_ampl2_sexonly <- bxpl_data_ampl2_sexonly %>%
  mutate(Test_Type = case_when(
    grepl("_L", variable) ~ "Audiogram_L",
    TRUE ~ "Speech_Tests"
  ))

pdf( paste(mydir_figs, "box_audio_sexonly.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_sexonly, Test_Type == "Audiogram_L"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~SEXE, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Left Audiogram") 
dev.off()




pdf( paste(mydir_figs, "box_speech_sexonly.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_sexonly, Test_Type == "Speech_Tests"), 
       aes(x = variable, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~SEXE, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Speech Tests") 
dev.off()



#by conditions and age
bxpl_data_ampl2_age = melt(data_ampl2[,c(4,7:28,30,31,35)],
                           id.vars = c("Classification", "age_group2"))

bxpl_data_ampl2_age <- bxpl_data_ampl2_age %>%
  mutate(Test_Type = case_when(
    grepl("_L", variable) ~ "Audiogram_L",
    grepl("_R", variable) ~ "Audiogram_R",
    TRUE ~ "Speech_Tests"
  ))


pdf( paste(mydir_figs, "box_audio_left_hl_age.pdf", sep = ""),
     width = 20, height = 9)  
ggplot(data = subset(bxpl_data_ampl2_age, Test_Type == "Audiogram_L"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification~age_group2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 13),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Left Audiogram") 
dev.off()




pdf( paste(mydir_figs, "box_speech_tests_hl_age.pdf", sep = ""),
     width = 20, height = 9)   
ggplot(data = subset(bxpl_data_ampl2_age, Test_Type == "Speech_Tests"), 
       aes(x = variable, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification~age_group2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Speech Tests") 
dev.off()


#by conditions and sex
bxpl_data_ampl2_sex = melt(data_ampl2[,c(4,7:17,30,31,29)],
                           id.vars = c("Classification", "SEXE"))

bxpl_data_ampl2_sex <- bxpl_data_ampl2_sex %>%
  mutate(Test_Type = case_when(
    grepl("_L", variable) ~ "Audiogram_L",
    TRUE ~ "Speech_Tests"
  ))  

pdf( paste(mydir_figs, "box_audio_left_hl_sex.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_sex, Test_Type == "Audiogram_L"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification~SEXE, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Left Audiogram") 
dev.off()


pdf( paste(mydir_figs, "box_speech_tests_hl_sex.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_sex, Test_Type == "Speech_Tests"), 
       aes(x = variable, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification~SEXE, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Speech Tests") 
dev.off()


#####################
# Descriptive Stats #
#####################


# Define the columns of interest
columns_of_interest <- c("AGE", "FREQ_125_L", "FREQ_250_L", "FREQ_500_L", "FREQ_750_L",
                         "FREQ_1000_L", "FREQ_1500_L", "FREQ_2000_L", "FREQ_3000_L",
                         "FREQ_4000_L", "FREQ_6000_L", "FREQ_8000_L", "SNR", "SRT")

# Convert columns to numeric if they are not already
data_ampl2 <- data_ampl2 %>%
  mutate(across(all_of(columns_of_interest), as.numeric))


descriptive_stats_overall <-  data_ampl2 %>%
  ungroup() %>%  # Ensure no grouping is applied
  summarise(across(all_of(columns_of_interest), list(
    Mean = ~ round(mean(. , na.rm = TRUE), 2),
    Median = ~ round(median(. , na.rm = TRUE), 2),
    SD = ~ round(sd(. , na.rm = TRUE), 2),
    Min = ~ round(min(. , na.rm = TRUE), 2),
    Max = ~ round(max(. , na.rm = TRUE), 2)
  ), .names = "{col}_{fn}"))

columns_of_interest <- c("AGE_Mean", "AGE_Median", "AGE_SD", "AGE_Min", "AGE_Max",
                         "FREQ_125_L_Mean", "FREQ_125_L_Median", "FREQ_125_L_SD", "FREQ_125_L_Min", "FREQ_125_L_Max",
                         "FREQ_250_L_Mean", "FREQ_250_L_Median", "FREQ_250_L_SD", "FREQ_250_L_Min", "FREQ_250_L_Max",
                         "FREQ_500_L_Mean", "FREQ_500_L_Median", "FREQ_500_L_SD", "FREQ_500_L_Min", "FREQ_500_L_Max",
                         "FREQ_750_L_Mean", "FREQ_750_L_Median", "FREQ_750_L_SD", "FREQ_750_L_Min", "FREQ_750_L_Max",
                         "FREQ_1000_L_Mean", "FREQ_1000_L_Median", "FREQ_1000_L_SD", "FREQ_1000_L_Min", "FREQ_1000_L_Max",
                         "FREQ_1500_L_Mean", "FREQ_1500_L_Median", "FREQ_1500_L_SD", "FREQ_1500_L_Min", "FREQ_1500_L_Max",
                         "FREQ_2000_L_Mean", "FREQ_2000_L_Median", "FREQ_2000_L_SD", "FREQ_2000_L_Min", "FREQ_2000_L_Max",
                         "FREQ_3000_L_Mean", "FREQ_3000_L_Median", "FREQ_3000_L_SD", "FREQ_3000_L_Min", "FREQ_3000_L_Max",
                         "FREQ_4000_L_Mean", "FREQ_4000_L_Median", "FREQ_4000_L_SD", "FREQ_4000_L_Min", "FREQ_4000_L_Max",
                         "FREQ_6000_L_Mean", "FREQ_6000_L_Median", "FREQ_6000_L_SD", "FREQ_6000_L_Min", "FREQ_6000_L_Max",
                         "FREQ_8000_L_Mean", "FREQ_8000_L_Median", "FREQ_8000_L_SD", "FREQ_8000_L_Min", "FREQ_8000_L_Max",
                         "SNR_Mean", "SNR_Median", "SNR_SD", "SNR_Min", "SNR_Max",
                         "SRT_Mean", "SRT_Median", "SRT_SD", "SRT_Min", "SRT_Max")

descriptive_stats_overall = descriptive_stats_overall %>%
  pivot_longer(
    cols = all_of(columns_of_interest), 
    names_to = c("frequency", "statistic"), 
    names_pattern = "(.*)_(.*)", 
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = frequency,
    values_from = value
  )

kable(descriptive_stats_overall, format = "latex", booktabs = TRUE, longtable = TRUE)


#REDefine the columns of interest for Age
columns_of_interest <- c("FREQ_125_L", "FREQ_250_L", "FREQ_500_L", "FREQ_750_L",
                         "FREQ_1000_L", "FREQ_1500_L", "FREQ_2000_L", "FREQ_3000_L",
                         "FREQ_4000_L", "FREQ_6000_L", "FREQ_8000_L", "SNR", "SRT")


# Compute descriptive statistics without grouping by age_group2
descriptive_stats_age_group2 <- data_ampl2 %>%
  dplyr::select(all_of(columns_of_interest), age_group2) %>%
  group_by(age_group2) %>%
  summarise(across(all_of(columns_of_interest), list(
    Mean = ~ round(mean(. , na.rm = TRUE), 2),
    Median = ~ round(median(. , na.rm = TRUE), 2),
    SD = ~ round(sd(. , na.rm = TRUE), 2),
    Min = ~ round(min(. , na.rm = TRUE), 2),
    Max = ~ round(max(. , na.rm = TRUE), 2)
  ), .names = "{col}_{fn}"), .groups = 'drop')


#REDefine the columns of interest for sex
columns_of_interest <- c("AGE", "FREQ_125_L", "FREQ_250_L", "FREQ_500_L", "FREQ_750_L",
                         "FREQ_1000_L", "FREQ_1500_L", "FREQ_2000_L", "FREQ_3000_L",
                         "FREQ_4000_L", "FREQ_6000_L", "FREQ_8000_L", "SNR", "SRT")


descriptive_stats_sex <- data_ampl2 %>%
  dplyr::select(all_of(columns_of_interest), SEXE) %>%
  group_by(SEXE) %>%
  summarise(across(all_of(columns_of_interest), list(
    Mean = ~ round(mean(. , na.rm = TRUE), 2),
    Median = ~ round(median(. , na.rm = TRUE), 2),
    SD = ~ round(sd(. , na.rm = TRUE), 2),
    Min = ~ round(min(. , na.rm = TRUE), 2),
    Max = ~ round(max(. , na.rm = TRUE), 2)
  ), .names = "{col}_{fn}"), .groups = 'drop')




#REDefine the columns of interest for HL
columns_of_interest <- c("AGE", "FREQ_125_L", "FREQ_250_L", "FREQ_500_L", "FREQ_750_L",
                         "FREQ_1000_L", "FREQ_1500_L", "FREQ_2000_L", "FREQ_3000_L",
                         "FREQ_4000_L", "FREQ_6000_L", "FREQ_8000_L", "SNR", "SRT")


descriptive_stats_hl <- data_ampl2 %>%
  dplyr::select(all_of(columns_of_interest), Classification) %>%
  group_by(Classification) %>%
  summarise(across(all_of(columns_of_interest), list(
    Mean = ~ round(mean(. , na.rm = TRUE), 2),
    Median = ~ round(median(. , na.rm = TRUE), 2),
    SD = ~ round(sd(. , na.rm = TRUE), 2),
    Min = ~ round(min(. , na.rm = TRUE), 2),
    Max = ~ round(max(. , na.rm = TRUE), 2)
  ), .names = "{col}_{fn}"), .groups = 'drop')




############## When you run ths part you need to change the dataset you use as 
# well as the variable for which you group them ie age group2 SEXE or Classification
# Furthermore if age group2 is considered then you need to remove age and AGE_ 
# statistics from the non frequency 

# Separate frequency-related and non-frequency-related variables
freq_vars <- descriptive_stats_hl %>% #CHANGE descriptive_stats_age_group2
  dplyr::select(Classification, starts_with("FREQ"))

non_freq_vars <- descriptive_stats_hl %>%
  dplyr::select(Classification, SNR_Mean, SNR_Median, SNR_SD, SNR_Min, SNR_Max,
         SRT_Mean, SRT_Median, SRT_SD, SRT_Min, SRT_Max,
         AGE_Mean, AGE_Median, AGE_SD, AGE_Min, AGE_Max)

# Pivot frequency-related variables to long format
long_freq <- freq_vars %>%
  pivot_longer(
    cols = -Classification,
    names_to = c(".value", "frequency"),
    names_pattern = "FREQ_(\\d+)_L_(.+)"
  )

# Rename the 'frequency' column to 'statistics'
long_freq_renamed <- long_freq %>%
  rename(statistics = frequency)


# Pivot non-frequency-related variables to long format
long_non_freq <- non_freq_vars %>%
  pivot_longer(
    cols = -Classification,
    names_to = c("variable", ".value"),
    names_sep = "_"
  )


long_non_freq_reshaped <- long_non_freq %>%
  pivot_wider(
    names_from = variable,   # This will become column names
    values_from = c(Mean, Median, SD, Min, Max)  # These will become values
  )



long_non_freq_long <-  long_non_freq_reshaped %>%
  pivot_longer(
    cols = c(Mean_SNR, Median_SNR, SD_SNR, Min_SNR, Max_SNR,
             Mean_SRT, Median_SRT, SD_SRT, Min_SRT, Max_SRT,
             Mean_AGE, Median_AGE, SD_AGE, Min_AGE, Max_AGE),
    names_to = c("statistics", ".value"),
    names_pattern = "(.*)_(.*)"
  )


# Combine the frequency-related and non-frequency-related datasets
combined_long_data <- full_join(long_freq_renamed, 
                                long_non_freq_long, 
                                by = c("Classification", 
                                       "statistics"))


kable(combined_long_data, format = "latex", booktabs = TRUE, longtable = TRUE)

########################################
# DATA Prep.& Hearing Rates Construction#
#########################################

#final data for analysis
data_lee_carter = data_ampl2[,c(3,4,7:17,34,35,36)] #Left ear


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



#######################################
# PLOT Audiogram with Rates principle#
######################################

###########
#   Lines  #   
############


temp = data_lee_carter[,c(2,3:13)] %>% filter(Classification == "Severe")
temp$id = seq(1, nrow(temp), by = 1)
temp = temp[c(1:10),]
temp = melt(temp[,c(2:13)], id.vars = "id")
  
gg_rate_0 = ggplot(temp, aes(x=variable, 
                 y=value, 
                 group=id  , 
                 color = as.factor(id)))  +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() +
  geom_point(color = hearing_loss_col[5]) + 
  geom_line(color = hearing_loss_col[5]) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 45, vjust = 0.5 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        legend.position = "none") + xlab("Frequencies")+ ylab("")

# Ensure the variable column in both data frames has the same type
temp$variable <- as.factor(temp$variable)

freq_tresh_for_plot_rate = data.frame(
  variable = c("FREQ_125_L", "FREQ_250_L", "FREQ_500_L", "FREQ_750_L", "FREQ_1000_L", 
               "FREQ_1500_L", "FREQ_2000_L", "FREQ_3000_L", "FREQ_4000_L", "FREQ_6000_L", "FREQ_8000_L"),
  value = as.numeric(freq_tresh[,1]),
  value2 = as.numeric(freq_tresh[,2]),
  value3 = as.numeric(freq_tresh[,3]),
  value4 = as.numeric(freq_tresh[,4]),
  value5 = as.numeric(freq_tresh[,5])
)

freq_tresh_for_plot_rate$variable <- factor(freq_tresh_for_plot_rate$variable,
                                            levels = levels(temp$variable))

# Create a data frame for the polygon
polygon_data <- rbind(
  freq_tresh_for_plot_rate[,-c(3:6)],
  data.frame(variable = rev(levels(freq_tresh_for_plot_rate$variable)), 
             value = rep(min(temp$value, na.rm = TRUE),
                         length(levels(freq_tresh_for_plot_rate$variable))))
)

# Ensure variable is treated as a factor in polygon_data
polygon_data$variable <- factor(polygon_data$variable,
                                levels = levels(temp$variable))



# Create a data frame for the polygon
polygon_data2 <- rbind(
  freq_tresh_for_plot_rate[,-c(2,4:6)],
  data.frame(variable = rev(levels(freq_tresh_for_plot_rate$variable)), 
             value2 = rep(min(temp$value, na.rm = TRUE),
                         length(levels(freq_tresh_for_plot_rate$variable))))
)

# Ensure variable is treated as a factor in polygon_data2
polygon_data2$variable <- factor(polygon_data2$variable,
                                 levels = levels(temp$variable))


# Create a data frame for the polygon
polygon_data3 <- rbind(
  freq_tresh_for_plot_rate[,-c(2:3,5:6)],
  data.frame(variable = rev(levels(freq_tresh_for_plot_rate$variable)), 
             value3 = rep(min(temp$value, na.rm = TRUE),
                          length(levels(freq_tresh_for_plot_rate$variable))))
)

# Ensure variable is treated as a factor in polygon_data3
polygon_data3$variable <- factor(polygon_data3$variable,
                                 levels = levels(temp$variable))


# Create a data frame for the polygon
polygon_data4 <- rbind(
  freq_tresh_for_plot_rate[,-c(2:4,6)],
  data.frame(variable = rev(levels(freq_tresh_for_plot_rate$variable)), 
             value4 = rep(min(temp$value, na.rm = TRUE),
                          length(levels(freq_tresh_for_plot_rate$variable))))
)

# Ensure variable is treated as a factor in polygon_data4
polygon_data4$variable <- factor(polygon_data4$variable,
                                 levels = levels(temp$variable))




# Create a data frame for the polygon
polygon_data5 <- rbind(
  freq_tresh_for_plot_rate[,-c(2:5)],
  data.frame(variable = rev(levels(freq_tresh_for_plot_rate$variable)), 
             value5 = rep(min(temp$value, na.rm = TRUE),
                          length(levels(freq_tresh_for_plot_rate$variable))))
)

# Ensure variable is treated as a factor in polygon_data5
polygon_data5$variable <- factor(polygon_data5$variable,
                                 levels = levels(temp$variable))



freq_tresh_values_line_plot<- data.frame(
  variable = c("FREQ_125_L", "FREQ_250_L", "FREQ_500_L", "FREQ_750_L", "FREQ_1000_L", 
               "FREQ_1500_L", "FREQ_2000_L", "FREQ_3000_L", "FREQ_4000_L", "FREQ_6000_L", "FREQ_8000_L"),
  value = as.numeric(freq_tresh[,1]),
  value2 = as.numeric(freq_tresh[,2]),
  value3 = as.numeric(freq_tresh[,3]),
  value4 = as.numeric(freq_tresh[,4]),
  value5 = as.numeric(freq_tresh[,5])
)

# Ensure variable is treated as a factor in freq_tresh_values_line_plot
freq_tresh_values_line_plot$variable <- factor(freq_tresh_values_line_plot$variable, 
                                               levels = levels(temp$variable))


# Plot
gg_rate_1 = ggplot(temp, aes(x = variable, y = value, 
                 group = id, color = as.factor(id))) +
  geom_point(data = temp %>% 
               mutate(size = ifelse(value > freq_tresh_values_line_plot$value[match(variable,
                                                                                    freq_tresh_values_line_plot$variable)], 0.3, 0.1)),
             aes(size = size), color = hearing_loss_col[5]) + 
  geom_line(color = hearing_loss_col[5]) +
  geom_polygon(data = polygon_data, aes(x = variable, y = value, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value, 
                                                    group = 1,
                                                    size = 0.1), color = "gold") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value,
                                                     group = 1,
                                                     size = 0.5), color = "gold") +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  xlab("Frequencies") + 
  ylab("")


gg_rate_2 = ggplot(temp, aes(x = variable, y = value, group = id, color = as.factor(id))) +
  geom_point(data = temp %>% 
               mutate(size = ifelse(value > freq_tresh_values_line_plot$value2[match(variable,
                                                                                     freq_tresh_values_line_plot$variable)], 0.3, 0.1)),
             aes(size = size), color = hearing_loss_col[5]) + 
  geom_line(color = hearing_loss_col[5]) +
  geom_polygon(data = polygon_data, aes(x = variable, y = value, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data2, aes(x = variable, y = value2, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value, 
                                                    group = 1,
                                                    size = 0.1), color = "gold") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value,
                                                     group = 1,
                                                     size = 0.5), color = "gold") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value2, 
                                                    group = 1,
                                                    size = 0.1), color = "#228B22") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value2,
                                                     group = 1,
                                                     size = 0.5), color = "#228B22") +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  xlab("Frequencies") + 
  ylab("")





gg_rate_3 = ggplot(temp, aes(x = variable, y = value, group = id, color = as.factor(id))) +
  geom_point(data = temp %>% 
               mutate(size = ifelse(value > freq_tresh_values_line_plot$value3[match(variable,
                                                                                     freq_tresh_values_line_plot$variable)], 0.3, 0.1)),
             aes(size = size), color = hearing_loss_col[5]) + 
  geom_line(color = hearing_loss_col[5]) +
  geom_polygon(data = polygon_data, aes(x = variable, y = value, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data2, aes(x = variable, y = value2, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data3, aes(x = variable, y = value3, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value, 
                                                    group = 1,
                                                    size = 0.1), color = "gold") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value,
                                                     group = 1,
                                                     size = 0.5), color = "gold") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value2, 
                                                    group = 1,
                                                    size = 0.1), color = "#228B22") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value2,
                                                     group = 1,
                                                     size = 0.5), color = "#228B22") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value3, 
                                                    group = 1,
                                                    size = 0.1), color = "#FFA500") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value3,
                                                     group = 1,
                                                     size = 0.5), color = "#FFA500") +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  xlab("Frequencies") + 
  ylab("")



gg_rate_4 = ggplot(temp, aes(x = variable, y = value, group = id, color = as.factor(id))) +
  geom_point(data = temp %>% 
               mutate(size = ifelse(value > freq_tresh_values_line_plot$value4[match(variable,
                                                                                     freq_tresh_values_line_plot$variable)], 0.3, 0.1)),
             aes(size = size), color = hearing_loss_col[5]) + 
  geom_line(color = hearing_loss_col[5]) +
  geom_polygon(data = polygon_data, aes(x = variable, y = value, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data2, aes(x = variable, y = value2, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data3, aes(x = variable, y = value3, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data4, aes(x = variable, y = value4, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value, 
                                                    group = 1,
                                                    size = 0.1), color = "gold") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value,
                                                     group = 1,
                                                     size = 0.5), color = "gold") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value2, 
                                                    group = 1,
                                                    size = 0.1), color = "#228B22") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value2,
                                                     group = 1,
                                                     size = 0.5), color = "#228B22") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value3, 
                                                    group = 1,
                                                    size = 0.1), color = "#FFA500") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value3,
                                                     group = 1,
                                                     size = 0.5), color = "#FFA500") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value4, 
                                                    group = 1,
                                                    size = 0.1), color = "#3366CC") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value4,
                                                     group = 1,
                                                     size = 0.5), color = "#3366CC") +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  xlab("Frequencies") + 
  ylab("")



gg_rate_5 = ggplot(temp, aes(x = variable, y = value, group = id, color = as.factor(id))) +
  geom_point(data = temp %>% 
               mutate(size = ifelse(value > freq_tresh_values_line_plot$value5[match(variable,
                                                                                     freq_tresh_values_line_plot$variable)], 0.3, 0.1)),
             aes(size = size),
             color = hearing_loss_col[5]) + 
  geom_line(color = hearing_loss_col[5]) +
  geom_polygon(data = polygon_data, aes(x = variable, y = value, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data2, aes(x = variable, y = value2, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data3, aes(x = variable, y = value3, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data4, aes(x = variable, y = value4, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_polygon(data = polygon_data5, aes(x = variable, y = value5, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value, 
                                                    group = 1,
                                                    size = 0.1), color = "gold") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value,
                                                     group = 1,
                                                     size = 0.5), color = "gold") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value2, 
                                                    group = 1,
                                                    size = 0.1), color = "#228B22") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value2,
                                                     group = 1,
                                                     size = 0.5), color = "#228B22") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value3, 
                                                    group = 1,
                                                    size = 0.1), color = "#FFA500") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value3,
                                                     group = 1,
                                                     size = 0.5), color = "#FFA500") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value4, 
                                                    group = 1,
                                                    size = 0.1), color = "#3366CC") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value4,
                                                     group = 1,
                                                     size = 0.5), color = "#3366CC") +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value5, 
                                                    group = 1,
                                                    size = 0.1), color = "#8B0000") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value5,
                                                     group = 1,
                                                     size = 0.5), color = "#8B0000") +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  xlab("Frequencies") + 
  ylab("")








combined_row1 <- gg_rate_0 + gg_rate_1  

combined_row2 <- gg_rate_2 + gg_rate_3
  
combined_row3 <- gg_rate_4 + gg_rate_5


# Arrange the combined row and the fourth plot, then add the fifth plot as a third row
combined_plot_0 <- (combined_row1 / combined_row2 / combined_row3) 


combined_plot_0

ggsave(filename = paste0(mydir_figs,"audio_rates_Sev", ".pdf"), 
       plot = combined_plot_0,
       width = 9,
       height = 12,
       device = "pdf",
       units = "in", dpi = 300)



#####################################
#   Plot Rates with Respect to Age  #   
#####################################


temp = data_lee_carter[,c(3:13, 15)] 
temp$id = seq(1, nrow(temp), by = 1)
temp = temp[c(1:300),]
temp = melt(temp, id.vars = c("id", "age_group2") )

ggplot(temp, aes(x=variable, y=value, 
                  group=id, 
                  color = as.factor(age_group2)))  +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() + facet_wrap(~age_group2, ncol = 3) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'),
        axis.text.x = element_text( size=10, angle = 45, vjust = 0.5 ),
        axis.text.y = element_text( size=10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        legend.position = "none") + xlab("Frequencies")+ ylab("") +
  scale_color_manual(values = rev(viridis(10, option = "C")))



# Create a data frame for the polygon
polygon_data <- rbind(
  freq_tresh_for_plot_rate[,-c(3:6)],
  data.frame(variable = rev(levels(freq_tresh_for_plot_rate$variable)), 
             value = rep(min(temp$value, na.rm = TRUE),
                         length(levels(freq_tresh_for_plot_rate$variable))))
)

# Ensure variable is treated as a factor in polygon_data
polygon_data$variable <- factor(polygon_data$variable,
                                levels = levels(temp$variable))



freq_tresh_values_line_plot<- data.frame(
  variable = c("FREQ_125_L", "FREQ_250_L", "FREQ_500_L", "FREQ_750_L", "FREQ_1000_L", 
               "FREQ_1500_L", "FREQ_2000_L", "FREQ_3000_L", "FREQ_4000_L", "FREQ_6000_L", "FREQ_8000_L"),
  value = as.numeric(freq_tresh[,1]),
  value2 = as.numeric(freq_tresh[,2]),
  value3 = as.numeric(freq_tresh[,3]),
  value4 = as.numeric(freq_tresh[,4]),
  value5 = as.numeric(freq_tresh[,5])
)

# Ensure variable is treated as a factor in freq_tresh_values_line_plot
freq_tresh_values_line_plot$variable <- factor(freq_tresh_values_line_plot$variable, 
                                               levels = levels(temp$variable))


# Plot
gg_rates_age = ggplot(temp, aes(x=variable, y=value, 
                 group=id, 
                 color = as.factor(age_group2))) +
  geom_point(data = temp %>% 
               mutate(size = ifelse(value > freq_tresh_values_line_plot$value[match(variable,
                                                                                    freq_tresh_values_line_plot$variable)], 0.3, 0.1)),
             ) + 
  geom_line() +
  geom_polygon(data = polygon_data, aes(x = variable, y = value, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value, 
                                                    group = 1), color = "black") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value,
                                                     group = 1), color = "black") +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() + facet_wrap(~age_group2, ncol = 3) +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    strip.text = element_text(color = 'black'),
    strip.background = element_rect(fill = 'white'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  xlab("Frequencies") + 
  ylab("") +
  scale_color_manual(values = rev(viridis(10, option = "C")))


ggsave(filename = paste0(mydir_figs,"audio_rates_age", ".pdf"), 
       plot = gg_rates_age,
       width = 12,
       height = 9,
       device = "pdf",
       units = "in", dpi = 300)

# Get the second level of age_group2
second_level <- levels(temp$age_group2)[2]

# Filter the data for only this level
filtered_temp <- temp %>% filter(age_group2 == second_level)

# Print the filtered data to verify
print(filtered_temp)

# Create the plot using the filtered data
gg_age_rate = ggplot(filtered_temp, aes(x = variable, y = value, group = id, color = as.factor(age_group2))) +
  geom_point(data = filtered_temp %>% 
               mutate(size = ifelse(value > freq_tresh_values_line_plot$value[match(variable,
                                                                                    freq_tresh_values_line_plot$variable)], 0.3, 0.1))) + 
  geom_line() +
  geom_polygon(data = polygon_data, aes(x = variable, y = value, group = 1), 
               fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
  geom_line(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                    y = value, 
                                                    group = 1), color = "black") +
  geom_point(data = freq_tresh_values_line_plot, aes(x = variable, 
                                                     y = value,
                                                     group = 1), color = "black") +
  scale_x_discrete(labels = freq_vector) +
  scale_y_reverse() + 
  facet_wrap(~age_group2, ncol = 1) +
  theme_bw() +
  theme(
    axis.line = element_line(color = 'black'),
    strip.text = element_blank(),
    strip.background = element_rect(fill = 'white'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  xlab("") + 
  ylab("") +
  scale_color_manual(values = rev(viridis(10, option = "C"))[1]) #SELECT THE COLOR HERE

gg_age_rate

ggsave(filename = paste0(mydir_figs,"audio_rates_age1", ".pdf"), 
       plot = gg_age_rate,
       device = "pdf",
       units = "in",
       width = 11,
       height = 3)



############################################
#   BOXPLOTS WITH DISTRIBUTION AND Q_F(d)  #   
############################################

freq_tresh_for_plot_rate = data.frame(
  variable = c("FREQ_125_L", "FREQ_250_L", "FREQ_500_L", "FREQ_750_L", "FREQ_1000_L", 
               "FREQ_1500_L", "FREQ_2000_L", "FREQ_3000_L", "FREQ_4000_L", "FREQ_6000_L", "FREQ_8000_L"),
  value = as.numeric(freq_tresh[,1]),
  value2 = as.numeric(freq_tresh[,2]),
  value3 = as.numeric(freq_tresh[,3]),
  value4 = as.numeric(freq_tresh[,4]),
  value5 = as.numeric(freq_tresh[,5])
)


data_lee_carter2 = data_lee_carter
# Prepare data: remove the _L suffix
colnames(data_lee_carter2) <- gsub("_L", "", colnames(data_lee_carter2))
colnames(data_lee_carter2) <- gsub("FREQ_", "", colnames(data_lee_carter2))

# Convert to long format
data_long <- melt(data_lee_carter2, id.vars = c("AGE", "Classification",
                                                "age_group", "age_group2", "age_group3"), 
                  measure.vars = c("125", "250", "500", "750", "1000",
                                   "1500", "2000", "3000", "4000", "6000", "8000"))


# Define the correct order of the levels for the factor
frequency_levels <- c("125", "250", "500", "750", "1000", 
                      "1500", "2000", "3000", "4000", "6000", "8000")


freq_tresh_for_plot_rate_long <- freq_tresh_for_plot_rate %>%
  pivot_longer(cols = starts_with("value"), names_to = "value_type", values_to = "value")

# Unique value types
value_types <- unique(freq_tresh_for_plot_rate_long$value_type)

for (value_type in value_types) {

    line_data <-  data.frame(
    variable = c("125", "250", "500", "750", "1000", "1500", "2000", 
                 "3000", "4000", "6000", "8000"),
    line_value = freq_tresh_for_plot_rate[, value_type]
  )
  
  # Create the plot
  p <- ggplot() +
    geom_boxplot(data = data_long, aes(x = factor(variable, 
                                                  levels = frequency_levels),
                                       y = value, 
                                       fill = variable), 
                 color = "black") +
    geom_line(data = line_data, aes(x = factor(variable,
                                               levels = frequency_levels), 
                                    y = line_value, group = 1), 
              color = "red", 
              size = 1,
              linetype = "dashed") +
    geom_ribbon(data = line_data, aes(x = factor(variable, 
                                                 levels = frequency_levels),
                                      ymin = -Inf, 
                                      ymax = line_value, group = 1), 
                fill = "gray46", alpha = 0.3) +
    scale_y_reverse() +
    labs(x = "Frequency (Hz)",
         y = "Value (dB)", 
         title = "") +
    theme_bw() +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25))
  
  print(p)
  
  # Save the plot
  ggsave(filename = paste0(mydir_figs, "prop_", value_type, ".pdf"),
  plot = p, width = 8, height = 6)
}





#by degree of hearing loss
for (value_type in value_types) {
  
  line_data <-  data.frame(
    variable = c("125", "250", "500", "750", "1000", "1500", "2000", 
                 "3000", "4000", "6000", "8000"),
    line_value = freq_tresh_for_plot_rate[, value_type]
  )
  
  # Create the plot
  p <- ggplot() +
    geom_boxplot(data = data_long, aes(x = factor(variable, 
                                                  levels = frequency_levels),
                                       y = value, 
                                       fill = variable), 
                 color = "black") +
    geom_line(data = line_data, aes(x = factor(variable,
                                               levels = frequency_levels), 
                                    y = line_value, group = 1), 
              color = "red", 
              size = 1,
              linetype = "dashed") +
    geom_ribbon(data = line_data, aes(x = factor(variable, 
                                                 levels = frequency_levels),
                                      ymin = -Inf, 
                                      ymax = line_value, group = 1), 
                fill = "gray46", alpha = 0.3) +
    facet_wrap(~ Classification, scales = "free_y") +
    scale_y_reverse() +
    labs(x = "Frequency (Hz)",
         y = "Value (dB)", 
         title = "") +
    theme_bw() +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          strip.background = element_rect(fill = 'white'),
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 18),
          axis.title.x = element_text(size = 23),
          axis.title.y = element_text(size = 23),
          strip.text = element_text(size = 24))
  
  print(p)
  
  # Save the plot
  ggsave(filename = paste0(mydir_figs, "prop_hl_", value_type, ".pdf"),
  plot = p, width = 13, height = 6)
}

##########
# By Sex #
##########

# Convert to long format
data_long <- melt(data_lee_carter2, id.vars = c("AGE", "Classification", "SEXE",
                                                "age_group", "age_group2", "age_group3"), 
                  measure.vars = c("125", "250", "500", "750", "1000",
                                   "1500", "2000", "3000", "4000", "6000", "8000"))


# Define the correct order of the levels for the factor
frequency_levels <- c("125", "250", "500", "750", "1000", 
                      "1500", "2000", "3000", "4000", "6000", "8000")


freq_tresh_for_plot_rate_long <- freq_tresh_for_plot_rate %>%
  pivot_longer(cols = starts_with("value"), names_to = "value_type", values_to = "value")

# Unique value types
value_types <- unique(freq_tresh_for_plot_rate_long$value_type)


for (value_type in value_types) {
  
  line_data <-  data.frame(
    variable = c("125", "250", "500", "750", "1000", "1500", "2000", 
                 "3000", "4000", "6000", "8000"),
    line_value = freq_tresh_for_plot_rate[, value_type]
  )
  
  # Create the plot
  p <- ggplot() +
    geom_boxplot(data = data_long, aes(x = factor(variable, 
                                                  levels = frequency_levels),
                                       y = value, 
                                       fill = variable), 
                 color = "black") +
    geom_line(data = line_data, aes(x = factor(variable,
                                               levels = frequency_levels), 
                                    y = line_value, group = 1), 
              color = "red", 
              size = 1,
              linetype = "dashed") +
    geom_ribbon(data = line_data, aes(x = factor(variable, 
                                                 levels = frequency_levels),
                                      ymin = -Inf, 
                                      ymax = line_value, group = 1), 
                fill = "gray46", alpha = 0.3) +
    facet_wrap(~ SEXE, scales = "free_y") +
    scale_y_reverse() +
    labs(x = "Frequency (Hz)",
         y = "Value (dB)", 
         title = "") +
    theme_bw() +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          strip.background = element_rect(fill = 'white'),
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 18),
          axis.title.x = element_text(size = 23),
          axis.title.y = element_text(size = 23),
          strip.text = element_text(size = 24))
  
  print(p)
  
  # Save the plot
  ggsave(filename = paste0(mydir_figs, "prop_sex_", value_type, ".pdf"),
         plot = p, width = 13, height = 6)
}







