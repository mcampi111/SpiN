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
library(patchwork)


#########
# Utils #
#########



prepare_data <- function(mydir, mydir2, mydir_figs) {
  source(paste0(mydir2, "utils.R"))
  
  data_ampl2 <- readRDS(file = paste0(mydir, "data_ampl2bis.rds"))
  data_ampl2 <- data_ampl2[data_ampl2$AGE <= 89 & data_ampl2$AGE >= 45, ]
  
  data_ampl2$age_group2 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 5),
                               labels = FALSE, include.lowest = TRUE)
  data_ampl2$age_group2 <- factor(paste0((data_ampl2$age_group2 - 1) * 5, "-",
                                         (data_ampl2$age_group2) * 5))
  
  data_ampl2$age_group3 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 1),
                               labels = FALSE, include.lowest = TRUE)
  data_ampl2$age_group3 <- factor(paste0((data_ampl2$age_group3 - 1), "-",
                                         (data_ampl2$age_group3)))
  
  data_lee_carter <- data_ampl2[, c(3, 4, 7:17, 34, 35, 36, 29)]
  data_lee_carter$SEXE <- as.factor(data_lee_carter$SEXE)
  
  spiq <- data_ampl2[, c(3, 4, 30, 34, 35, 36, 29)]
  spin <- data_ampl2[, c(3, 4, 31, 34, 35, 36, 29)]
  
  PTA_df <- data.frame(PTA = data_ampl2$PTA)
  SRT_df <- data.frame(SRT = data_ampl2$SRT)
  SNR_df <- data.frame(SNR = data_ampl2$SNR)
  
  hl_classes <- levels(data_ampl2$Classification)
  n_hlclasses <- length(hl_classes)
  
  freq_vector <- sapply(1:11, function(i) 
    str_split(colnames(data_lee_carter)[2 + i], "_")[[1]][2])
  n_freq <- length(freq_vector)
  
  age_groups <- levels(data_lee_carter$age_group)[5:9]
  n_age_g <- length(age_groups)
  
  age_groups2 <- levels(data_lee_carter$age_group2)
  n_age_g2 <- length(age_groups2)
  
  all_age_groups <- levels(data_lee_carter$age_group3)
  n_age_g3 <- length(all_age_groups)
  
  decision_threshold <- as.data.frame(cbind(data_ampl2$PTA, 
                                            data_ampl2$SRT, data_ampl2$SNR))
  
  quant_tresh <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  quant_PTA <- quantile(decision_threshold$PTA, 
                        probs = quant_tresh, na.rm = TRUE) 
  quant_SRT <- quantile(decision_threshold$SRT, 
                        probs = quant_tresh, na.rm = TRUE) 
  quant_SNR <- quantile(decision_threshold$SNR, 
                        probs = quant_tresh, na.rm = TRUE)
  
  tres_pta_srt <- data.frame(PTA = quant_PTA, SRT = quant_SRT)
  tres_pta_snr <- data.frame(PTA = quant_PTA, SNR = quant_SNR)
  tres_srt_snr <- data.frame(SRT = quant_SRT, SNR = quant_SNR)
  
  freq_tresh <- do.call(rbind, lapply(data_ampl2[, 7:17], quantile, 
                                      quant_tresh, 2))
  
  treshold_hearing_rate = as.integer(tres_pta_snr[,1])
  n_tresh = length(treshold_hearing_rate)
  
  treshold_SPIQ = as.integer(tres_pta_srt[,2])
  treshold_SPIN = as.integer(tres_pta_snr[,2])
  
  freq_tresh_plot = c( paste(20,"%", sep = ""), paste(40,"%", sep = ""),
                       paste(60,"%", sep = ""), paste(80,"%", sep = ""),
                       paste(90,"%", sep = "")) 
  
  
  thresh_colors = c("#F5F5F5", "#FFDDC1", "#FF8C69", "#FF6347", "#FF0000")
  hearing_loss_col = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529") 
  
  
  
  list(
    data_lee_carter = data_lee_carter,
    spiq = spiq,
    spin = spin,
    PTA_df = PTA_df,
    SRT_df = SRT_df,
    SNR_df = SNR_df,
    freq_tresh = freq_tresh,
    hl_classes = hl_classes,
    n_hlclasses = n_hlclasses,
    freq_vector = freq_vector,
    n_freq = n_freq,
    age_groups = age_groups,
    age_groups2 = age_groups2,
    all_age_groups = all_age_groups,
    n_age_g =n_age_g,
    n_age_g2 = n_age_g2,
    n_age_g3 = n_age_g3,
    tres_pta_snr = tres_pta_snr,
    tres_pta_srt = tres_pta_srt,
    treshold_hearing_rate = treshold_hearing_rate,
    treshold_SPIQ = treshold_SPIQ,
    treshold_SPIN = treshold_SPIN,
    freq_tresh_plot = freq_tresh_plot,
    thresh_colors = thresh_colors,
    hearing_loss_col = hearing_loss_col,
    n_tresh = n_tresh
  )
}


process_matrix <- function(mat, thresholds) {
  if (ncol(mat) == 17) {
    processed_list <- lapply(thresholds, function(threshold) {
      counts <- colSums(mat[, -c(1, 2, 14, 15, 16, 17)] > threshold)
      ratios <- counts / nrow(mat)
      return(ratios)
    })
  } else if (ncol(mat) == 7) {
    processed_list <- lapply(thresholds, function(threshold) {
      counts <- colSums(mat[, -c(1, 2, 4, 5, 6,7)] > threshold)
      ratios <- counts / nrow(mat)
      return(ratios)
    })
  } else {
    stop("Unexpected number of columns in 'mat'")
  }
  
  return(processed_list)
}


process_matrix2 <- function(mat, freq_tresh) {
  if (ncol(mat) == 17) {
    processed_list <- lapply(seq_len(ncol(freq_tresh)), function(thresh_col) {
      threshold <- freq_tresh[, thresh_col]
      counts <- colSums(mat[, -c(1, 2, 14, 15, 16, 17)] > threshold)
      ratios <- counts / nrow(mat)
      return(ratios)
    })
  } else if (ncol(mat) == 7) {
    processed_list <- lapply(seq_len(ncol(freq_tresh)), function(thresh_col) {
      threshold <- freq_tresh[, thresh_col]
      counts <- colSums(mat[, -c(1, 2, 4, 5, 6,7)] > threshold)
      ratios <- counts / nrow(mat)
      return(ratios)
    })
  } else {
    stop("Unexpected number of columns in 'freq_tresh'")
  }
  
  return(processed_list)
}



process_matrix_totals <- function(mat, freq_tresh) {
  if (ncol(mat) == 17) {
    processed_list <- lapply(seq_len(ncol(freq_tresh)), function(thresh_col) {
      threshold <- freq_tresh[, thresh_col]
      totals <- nrow(mat)
      return(totals)
    })
  } else if (ncol(mat) == 7) {
    processed_list <- lapply(seq_len(ncol(freq_tresh)), function(thresh_col) {
      threshold <- freq_tresh[, thresh_col]
      totals <- nrow(mat)
      return(totals)
    })
  } else {
    stop("Unexpected number of columns in 'freq_tresh'")
  }
  
  return(processed_list)
}


process_matrix_countsonly <- function(mat, freq_tresh) {
  if (ncol(mat) == 17) {
    processed_list <- lapply(seq_len(ncol(freq_tresh)), function(thresh_col) {
      threshold <- freq_tresh[, thresh_col]
      counts <- colSums(mat[, -c(1, 2, 14, 15, 16, 17)] > threshold)
      return(counts)
    })
  } else if (ncol(mat) == 7) {
    processed_list <- lapply(seq_len(ncol(freq_tresh)), function(thresh_col) {
      threshold <- freq_tresh[, thresh_col]
      counts <- colSums(mat[, -c(1, 2, 4, 5, 6,7)] > threshold)
      return(counts)
    })
  } else {
    stop("Unexpected number of columns in 'freq_tresh'")
  }
  
  return(processed_list)
}


process_data <- function(data, thresholds, age_group_var,
                         len_tresh, len_age, len_hl,
                         process_func = process_matrix) {
  
  data_by_age <- data %>%
    group_by(!!sym(age_group_var)) %>% group_split()
  
  data_by_age_hl <- data %>%
    group_by(!!sym(age_group_var)) %>% 
    group_split() %>% 
    lapply(function(subgroup) {
      subgroup %>%
        group_split(`Classification`)
    })
  
  data_by_age_sex <- data %>%
    group_by(!!sym(age_group_var)) %>% 
    group_split() %>% 
    lapply(function(subgroup) {
      subgroup %>%
        group_split(`SEXE`)
    })
  
  hrates_age <- lapply(data_by_age, process_func, thresholds)
  hrates_age_mat <- lapply(1:len_tresh, function(i) 
    do.call(rbind, lapply(hrates_age, "[[", i)))
  
  hrates_age_hl <- lapply(1:len_age, function(i) {
    lapply(data_by_age_hl[[i]], process_func, thresholds)
  })
  
  hrates_age_hl_mat <-lapply(1:len_tresh, function(h)
    lapply(1:len_hl, function(j)
      do.call(rbind,
              lapply(1:len_age, function(i) 
                hrates_age_hl[[i]][[j]][[h]]))))
  
  hrates_age_sex <- lapply(1:len_age, function(i) {
    lapply(data_by_age_sex[[i]], process_func, thresholds)
  })
  
  hrates_age_sex_mat <-lapply(1:len_tresh, function(h)
    lapply(1:2, function(j)
      do.call(rbind,
              lapply(1:len_age, function(i) 
                hrates_age_sex[[i]][[j]][[h]]))))
  
  return(list(hrates_age_mat = hrates_age_mat, 
              hrates_age_hl_mat = hrates_age_hl_mat,
              hrates_age_sexl_mat = hrates_age_sex_mat))
}



interpolate_row_linear <- function(row) {
  zero_indices <- which(row == 0)
  non_zero_indices <- which(row != 0)
  
  # If row consists entirely of zeros, perturb it with small Gaussian noise
  if (length(zero_indices) == length(row)) {
    row <- abs(row + rnorm(length(row), mean = 0, sd = 1e-5))
  }
  
  if (length(zero_indices) > 0 && length(zero_indices) != length(row)) {
    if (length(non_zero_indices) > 1) {
      row[zero_indices] <- approx(non_zero_indices, row[non_zero_indices],
                                  xout = zero_indices,
                                  method = 'linear', rule = 2)$y
    } else if (length(non_zero_indices) == 1) {
      # If only one non-zero value, replace it with the successive value if available,
      # or with this value everywhere else if it's the last position
      next_index <- non_zero_indices + 1
      last_index <- length(row)
      
      if (next_index <= last_index) {
        row[zero_indices] <- row[next_index]
      } else {
        row[zero_indices] <- row[non_zero_indices]
      }
    }
  }
  
  return(row)
}





recursive_interpolation <- function(lst) {
  if (is.matrix(lst)) {
    # If the element is a matrix, apply linear interpolation
    result <- t(apply(lst, 1, interpolate_row_linear))
    return(result)
  } else if (is.list(lst)) {
    # If the element is a list, apply the function recursively 
    #to each element of the list
    result <- lapply(lst, recursive_interpolation)
    return(result)
  } else {
    # If the element is neither a matrix nor a list, return it unchanged
    return(lst)
  }
}


plot_hr_rates <- function(df_rates, hl_classes, len_tresh, 
                          age_group_var, thresholds, sex_classes) {
  
  gg1 <- list()
  gg2 <- list()
  gg3 <- list()
  
  for (i in 1:len_tresh) {
    melted_rates0 <- melt(df_rates[[1]][[i]])
    melted_rates0$age_group <- age_group_var[melted_rates0$Var1]
    
    if (is.matrix(thresholds)) {
      threshold_value <- paste("Empirical Quantile:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Empirical Quantile:", thresholds[[i]])
    }
    
    gg1[[i]] <- ggplot(melted_rates0, aes(x = reorder(Var2, as.numeric(Var2)),
                                          y = value)) +
      geom_bar(stat = "identity") + facet_wrap(~age_group) +
      labs(title = threshold_value,
           x = "Frequency",
           y = "Hearing Rates") +
      scale_fill_discrete(name = "") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white')) 
  } 
  
  for (i in 1:len_tresh) {
    melted_rates <- melt(df_rates[[2]][[i]])
    melted_rates$age_group <- age_group_var[melted_rates$Var1]
    melted_rates$hl_classes <- hl_classes[melted_rates$L1]
    ordering <- hl_classes  
    melted_rates$hl_classes <- factor(melted_rates$hl_classes,
                                      levels = ordering)
    
    if (is.matrix(thresholds)) {
      threshold_value <- paste("Empirical Quantile:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Empirical Quantile:", thresholds[[i]])
    }
    
    hearing_loss_col = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529") 
    
    gg2[[i]] <- ggplot(melted_rates, aes(x = reorder(Var2,
                                                     as.numeric(Var2)),
                                         y = value,
                                         fill = as.factor(hl_classes))) +
      geom_bar(stat = "identity") + facet_wrap(~age_group) +
      labs(title = threshold_value,
           x = "Frequency",
           y = "Hearing Rates") +
      scale_fill_discrete(name = "") +
      scale_fill_manual(values = hearing_loss_col, name = "HL Classes") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white')) 
  } 
  
  for (i in 1:len_tresh) {
    melted_rates_sex <- melt(df_rates[[3]][[i]])
    melted_rates_sex$age_group <- age_group_var[melted_rates_sex$Var1]
    melted_rates_sex$sex_classes <- sex_classes[melted_rates_sex$L1]
    ordering_sex <- sex_classes
    melted_rates_sex$sex_classes <- factor(melted_rates_sex$sex_classes,
                                      levels = ordering_sex)
    
    if (is.matrix(thresholds)) {
      threshold_value <- paste("Empirical Quantile:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Empirical Quantile:", thresholds[[i]])
    }
    
    sex_col = c("pink","blue") 
    
    gg3[[i]] <- ggplot(melted_rates_sex, aes(x = reorder(Var2,
                                                     as.numeric(Var2)),
                                         y = value,
                                         fill = as.factor(sex_classes))) +
      geom_bar(stat = "identity") + facet_wrap(~age_group) +
      labs(title = threshold_value,
           x = "Frequency",
           y = "Hearing Rates") +
      scale_fill_discrete(name = "") +
      scale_fill_manual(values = sex_col, name = "Sex") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white')) 
  } 
  
  return(list(gg1 = gg1, gg2 = gg2, gg3 = gg3))
  
}


plot_hr_rates2 <- function(df_rates, hl_classes, len_tresh, 
                           age_group_var, thresholds, sex_classes) {
  
  gg1 <- list()
  gg2 <- list()
  gg3 <- list()
  
  
  for (i in 1:len_tresh) {
    melted_rates0 <- melt(df_rates[[1]][[i]])
    melted_rates0$age_group <- age_group_var[melted_rates0$Var1]
    melted_rates0$frequency <- as.numeric(melted_rates0$Var2)
    melted_rates0$freq_mapped = factor(freq_vector[as.numeric(melted_rates0$Var2)], 
                                       levels = unique(freq_vector))
    
    
    
    if (is.matrix(thresholds)) {
      threshold_value <- paste("Empirical Quantile:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Empirical Quantile:", thresholds[[i]])
    }
    
    gg1[[i]] <- ggplot(melted_rates0, aes(x = freq_mapped,
                                          y = value)) +
      geom_line(aes(group = 1), size = 1) + facet_wrap(~age_group) +
      labs(title = threshold_value,
           x = "Frequency",
           y = "Hearing Rates") +
      scale_fill_discrete(name = "") +  
      scale_x_discrete(breaks = unique(melted_rates0$freq_mapped)) +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white'),
            plot.title = element_text(size = 20)) 
  } 
  
  for (i in 1:len_tresh) {
    melted_rates <- melt(df_rates[[2]][[i]])
    melted_rates$age_group <- age_group_var[melted_rates$Var1]
    melted_rates$hl_classes <- hl_classes[melted_rates$L1]
    ordering <- hl_classes  
    melted_rates$hl_classes <- factor(melted_rates$hl_classes,
                                      levels = ordering)
    melted_rates$frequency <- as.numeric(melted_rates$Var2)
    melted_rates$freq_mapped = factor(freq_vector[as.numeric(melted_rates$Var2)], 
                                       levels = unique(freq_vector))
    
    
    if (is.matrix(thresholds)) {
      threshold_value <- paste("Empirical Quantile:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Empirical Quantile:", thresholds[[i]])
    }
    
    hearing_loss_col = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529") 
    
    gg2[[i]] <- ggplot(melted_rates, aes(x =  freq_mapped,
                                         y = value,
                                         color = as.factor(hl_classes))) +
      geom_line(aes(group = hl_classes),size = 1) + facet_wrap(~age_group) +
      labs(title = threshold_value,
           x = "Frequency",
           y = "Hearing Rates",
           color = "HL Classes") +
      scale_color_discrete(name = "") +
      scale_color_manual(values = hearing_loss_col, name = "") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white'),
            plot.title = element_text(size = 20)) 
  }
  
  for (i in 1:len_tresh) {
    melted_rates_sex <- melt(df_rates[[3]][[i]])
    melted_rates_sex$age_group <- age_group_var[melted_rates_sex$Var1]
    melted_rates_sex$sex_classes <- sex_classes[melted_rates_sex$L1]
    ordering_sex <- sex_classes  
    melted_rates_sex$sex_classes <- factor(melted_rates_sex$sex_classes,
                                      levels = ordering_sex)
    melted_rates_sex$frequency <- as.numeric(melted_rates_sex$Var2)
    melted_rates_sex$freq_mapped = factor(freq_vector[as.numeric(melted_rates_sex$Var2)], 
                                      levels = unique(freq_vector))
    
    
    if (is.matrix(thresholds)) {
      threshold_value <- paste("Empirical Quantile:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Empirical Quantile:", thresholds[[i]])
    }
    
    sex_col = c("pink","blue") 
    
    gg3[[i]] <- ggplot(melted_rates_sex, aes(x =  freq_mapped,
                                         y = value,
                                         color = as.factor(sex_classes))) +
      geom_line(aes(group = sex_classes),size = 1) + facet_wrap(~age_group) +
      labs(title = threshold_value,
           x = "Frequency",
           y = "Hearing Rates",
           color = "Sex") +
      scale_color_discrete(name = "") +
      scale_color_manual(values = sex_col, name = "") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white'),
            plot.title = element_text(size = 20)) 
  } 
  
  return(list(gg1 = gg1, gg2 = gg2, gg3 = gg3))
  
}

plot_speech_rates <- function(df_rates,hl_classes,len_tresh, 
                              age_group_var,tresholds, speech_type,
                              sex_classes) {
  
  gg1 = list()
  gg2 = list()
  gg3 = list()
  
  for (i in 1:len_tresh) {
    melted_rates0 = melt(df_rates[[1]][[i]])
    melted_rates0$age_group = age_group_var[melted_rates0$Var1]
    
    gg1[[i]] =  ggplot(melted_rates0, aes(x = age_group,
                                          y = value)) +
      geom_bar(stat = "identity") + 
      labs(title = paste("Empirical Quantile:", tresholds[[i]]),
           x = "Age Group",
           y = ifelse(speech_type == "SpiQ", "SpiQ Rates", "SpiN Rates")) +
      scale_fill_discrete(name = "") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white')) } 
  
  for (i in 1:len_tresh) {
    melted_rates = melt(df_rates[[2]][[i]])
    melted_rates$age_group = age_group_var[melted_rates$Var1]
    melted_rates$hl_classes = hl_classes[melted_rates$L1]
    ordering <- hl_classes  
    melted_rates$hl_classes <- factor(melted_rates$hl_classes,
                                      levels = ordering)
    
    hearing_loss_col = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529") 
    
    
    gg2[[i]] = ggplot(melted_rates, aes(x = age_group,
                                        y = value,
                                        fill = as.factor(hl_classes))) +
      geom_bar(stat = "identity") + 
      labs(title = paste("Empirical Quantile:", tresholds[[i]]),
           x = "Age Group",
           y = ifelse(speech_type == "SpiQ", "SpiQ Rates", "SpiN Rates")) +
      scale_fill_discrete(name = "") +
      scale_fill_manual(values = hearing_loss_col, name = "HL Classes") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white')) } 
  
  for (i in 1:len_tresh) {
    melted_rates_sex = melt(df_rates[[3]][[i]])
    melted_rates_sex$age_group = age_group_var[melted_rates_sex$Var1]
    melted_rates_sex$sex_classes = sex_classes[melted_rates_sex$L1]
    ordering_sex <- sex_classes  
    melted_rates_sex$sex_classes <- factor(melted_rates_sex$sex_classes,
                                      levels = ordering_sex)
    
    sex_col = c("pink","blue") 
    
    
    gg3[[i]] = ggplot(melted_rates_sex, aes(x = age_group,
                                        y = value,
                                        fill = as.factor(sex_classes))) +
      geom_bar(stat = "identity") + 
      labs(title = paste("Empirical Quantile:", tresholds[[i]]),
           x = "Age Group",
           y = ifelse(speech_type == "SpiQ", "SpiQ Rates", "SpiN Rates")) +
      scale_fill_discrete(name = "") +
      scale_fill_manual(values = sex_col, name = "Sex") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white')) } 
  
  return(list(gg1 = gg1, gg2 = gg2, gg3 = gg3))
  
}



plot_speech_rates2 <- function(df_rates,hl_classes,len_tresh, 
                              age_group_var,tresholds, speech_type,
                              sex_classes) {
  
  gg1 = list()
  gg2 = list()
  gg3 = list()
  
  for (i in 1:len_tresh) {
    melted_rates0 = melt(df_rates[[1]][[i]])
    melted_rates0$age_group = factor(age_group_var[melted_rates0$Var1], levels = unique(age_group_var))
    
    gg1[[i]] =  ggplot(melted_rates0, aes(x = age_group,
                                          y = value)) +
      geom_line(aes(group = 1),size = 1) + 
      labs(title = paste("Empirical Quantile:", tresholds[[i]]),
           x = "Age Group",
           y = ifelse(speech_type == "SpiQ", "SpiQ Rates", "SpiN Rates")) +
      scale_fill_discrete(name = "") +  
      scale_x_discrete(breaks = unique(melted_rates0$age_group)) +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white'),
            plot.title = element_text(size = 20)) } 
  
  for (i in 1:len_tresh) {
    melted_rates = melt(df_rates[[2]][[i]])
    melted_rates$age_group = factor(age_group_var[melted_rates$Var1], levels = unique(age_group_var))
    melted_rates$hl_classes = hl_classes[melted_rates$L1]
    ordering <- hl_classes  
    melted_rates$hl_classes <- factor(melted_rates$hl_classes,
                                      levels = ordering)
    
    hearing_loss_col = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529") 
    
    
    gg2[[i]] = ggplot(melted_rates, aes(x = age_group,
                                        y = value,
                                        color = as.factor(hl_classes))) +
      geom_line(aes(group = hl_classes),size = 1) + 
      labs(title = paste("Empirical Quantile:", tresholds[[i]]),
           x = "Age Group",
           y = ifelse(speech_type == "SpiQ", "SpiQ Rates", "SpiN Rates"),
           color = "HL Classes") +
      scale_fill_discrete(name = "")  +
      scale_color_manual(values = hearing_loss_col, name = "") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white'),
            plot.title = element_text(size = 20)) } 
  
  for (i in 1:len_tresh) {
    melted_rates_sex = melt(df_rates[[3]][[i]])
    melted_rates_sex$age_group = factor(age_group_var[melted_rates_sex$Var1], 
                                    levels = unique(age_group_var))
    melted_rates_sex$sex_classes = sex_classes[melted_rates_sex$L1]
    ordering_sex <- sex_classes  
    melted_rates_sex$sex_classes <- factor(melted_rates_sex$sex_classes,
                                      levels = ordering_sex)
    
    sex_col = c("pink","blue") 
    
    
    gg3[[i]] = ggplot(melted_rates_sex, aes(x = age_group,
                                        y = value,
                                        color = as.factor(sex_classes))) +
      geom_line(aes(group = sex_classes),size = 1) + 
      labs(title = paste("Empirical Quantile:", tresholds[[i]]),
           x = "Age Group",
           y = ifelse(speech_type == "SpiQ", "SpiQ Rates", "SpiN Rates"),
           color = "Sex") +
      scale_fill_discrete(name = "")  +
      scale_color_manual(values = sex_col, name = "") +
      theme_bw() +
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            strip.text = element_text(color = 'black'),
            strip.background = element_rect(fill = 'white'),
            plot.title = element_text(size = 20)) } 
  
  return(list(gg1 = gg1, gg2 = gg2, gg3 = gg3))
  
}



plot_heat_rates <- function(df_rates, spq_rqtes, spn_rates,
                            hl_classes, freq_vector, 
                            len_tresh, age_group_var, threshold_hl,
                            freq_plot, sex_classes ){ #,
                            #threshold_hl, 
                            #threshold_spq, 
                            #threshold_spn) {
  
  gg1 <- list()
  gg2 <- list()
  gg3 <- list()
  
  melt_rates <- list()
  
  for (i in 1:len_tresh) { 
    
    melt_rates[[i]] <-  as.data.frame(cbind(df_rates[[1]][[i]],
                                            spq_rqtes[[1]][[i]],
                                            spn_rates[[1]][[i]])) 
    colnames(melt_rates[[i]]) <- c(paste(freq_vector), 
                                   "SRT", "SNR")
    melt_rates[[i]]$age <- sapply(strsplit(age_group_var, "-"),
                                  function(x) as.numeric(x[1])) 
    melt_rates[[i]]$threshold <- freq_plot[i]
  }
  
  melted_rates <- melt(melt_rates, id.var = c("age", "threshold"))
  
  #melted_rates$SRT <- threshold_spq[melted_rates$L1]
  #melted_rates$SNR <- threshold_spn[melted_rates$L1]
  
  #melted_rates$facet_label <- paste("HL:", melted_rates$HL,
  #                                  " SRT:", melted_rates$SRT, 
  #                                  " SNR:", melted_rates$SNR)
  
  
  gg1 <- ggplot(melted_rates, aes(x = variable, y = age, fill = value)) +
    geom_tile() + 
    scale_fill_gradient(low = "white", high = "blue") +   
    theme_bw() + 
    facet_wrap(~threshold, ncol = 5) +
    theme(
      panel.background = element_rect(),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 10, angle = 90),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.position = "bottom",
      strip.background = element_rect(fill = "white", color = "black")
    ) +
    labs(fill = "Proportion",  
         x = "Frequency",  
         y = "Age"
    )
  
  
  
  melt_rates2 <- list()
  
  for (i in 1:len_tresh) {
    
    melt_rates2[[i]] <- list()
    
    for (j in 1:length(hl_classes)) {
      
      melt_rates2[[i]][[j]] <- as.data.frame(cbind(df_rates[[2]][[i]][[j]],
                                                   spq_rqtes[[2]][[i]][[j]],
                                                   spn_rates[[2]][[i]][[j]]))
      
      colnames(melt_rates2[[i]][[j]]) <- c(paste(freq_vector), 
                                           "SRT", "SNR")
      
      melt_rates2[[i]][[j]]$age <- sapply(strsplit(age_group_var, "-"),
                                          function(x) as.numeric(x[1]))
      melt_rates2[[i]][[j]]$hl_classes <- hl_classes[j]
      melt_rates2[[i]][[j]]$threshold <- freq_plot[i]
      
      
    } 
  }
  
  
    melted_rates2 <- melt(melt_rates2, id.var = c('age', 'hl_classes', 'threshold'))
    melted_rates2$hl_classes <- factor(melted_rates2$hl_classes, 
                                       levels = hl_classes)
    
    gg2 <- ggplot(melted_rates2, aes(x = variable, y = age, fill = value)) +
      geom_tile() + 
      scale_fill_gradient(low = "white", high = "blue") +    
      theme_bw() + 
      facet_grid(hl_classes~threshold) +
      theme(
        panel.background = element_rect(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black")
      ) +
      labs(fill = "Proportion",  
           x = "Frequency",  
           y = "Age"
      )
    
    
    
    melt_rates3 <- list()
    
    for (i in 1:len_tresh) {
      
      melt_rates3[[i]] <- list()
      
      for (j in 1:length(sex_classes)) {
        
        melt_rates3[[i]][[j]] <- as.data.frame(cbind(df_rates[[3]][[i]][[j]],
                                                     spq_rqtes[[3]][[i]][[j]],
                                                     spn_rates[[3]][[i]][[j]]))
        
        colnames(melt_rates3[[i]][[j]]) <- c(paste(freq_vector), 
                                             "SRT", "SNR")
        
        melt_rates3[[i]][[j]]$age <- sapply(strsplit(age_group_var, "-"),
                                            function(x) as.numeric(x[1]))
        melt_rates3[[i]][[j]]$sex_classes <- sex_classes[j]
        melt_rates3[[i]][[j]]$threshold <- freq_plot[i]
        
        
      } 
    }
    
    
    melted_rates3 <- melt(melt_rates3, id.var = c('age', 'sex_classes', 'threshold'))
    melted_rates3$sex_classes <- factor(melted_rates3$sex_classes, 
                                       levels = sex_classes)
    
    gg3 <- ggplot(melted_rates3, aes(x = variable, y = age, fill = value)) +
      geom_tile() + 
      scale_fill_gradient(low = "white", high = "blue") +    
      theme_bw() + 
      facet_grid(sex_classes~threshold) +
      theme(
        panel.background = element_rect(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white", color = "black")
      ) +
      labs(fill = "Proportion",  
           x = "Frequency",  
           y = "Age"
      )
  
  
  return(list(gg1 = gg1, gg2 = gg2, gg3 = gg3))
  
}



plot_polar_heatmaps <- function(df_rates, spq_rqtes, spn_rates,
                                hl_classes, freq_vector, 
                                len_tresh, age_group_var, threshold_hl,
                                freq_plot, sex_classes) {
  
  # Prepare data for gg1 (polar plot)
  melt_rates <- list()
  
  for (i in 1:len_tresh) { 
    melt_rates[[i]] <- as.data.frame(cbind(df_rates[[1]][[i]],
                                           spq_rqtes[[1]][[i]],
                                           spn_rates[[1]][[i]])) 
    colnames(melt_rates[[i]]) <- c(paste(freq_vector), "SRT", "SNR")
    melt_rates[[i]]$age <- sapply(strsplit(age_group_var, "-"),
                                  function(x) as.numeric(x[1])) 
    melt_rates[[i]]$threshold <- freq_plot[i]
  }
  
  melted_rates <- melt(melt_rates, id.vars = c("age", "threshold"))
  
  # Add angle for polar plot
  melted_rates$angle <- as.numeric(as.factor(melted_rates$variable)) * (360 / length(unique(melted_rates$variable)))
  
  # gg1 - Polar heatmap with frequencies
  gg1 <- ggplot(melted_rates, aes(x = angle, y = age, fill = value)) +
    geom_tile() +
    geom_textpath(data = melted_rates,
                  aes(x = angle, y = max(age) + 10, label = variable), 
                  color = "black", size = 3, linetype = 0) +
    geom_hline(yintercept = 0:3 * 5, color = "gray90") +
    coord_curvedpolar(theta = "x") +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    facet_wrap(~threshold, ncol = 5) +
    labs(fill = "Proportion", x = "Frequency", y = "Age") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      legend.position = "left"   #left
    )
  
  # Prepare data for gg2 (polar plot with HL classes)
  melt_rates2 <- list()
  
  for (i in 1:len_tresh) {
    melt_rates2[[i]] <- list()
    for (j in 1:length(hl_classes)) {
      melt_rates2[[i]][[j]] <- as.data.frame(cbind(df_rates[[2]][[i]][[j]],
                                                   spq_rqtes[[2]][[i]][[j]],
                                                   spn_rates[[2]][[i]][[j]]))
      colnames(melt_rates2[[i]][[j]]) <- c(paste(freq_vector), "SRT", "SNR")
      melt_rates2[[i]][[j]]$age <- sapply(strsplit(age_group_var, "-"),
                                          function(x) as.numeric(x[1]))
      melt_rates2[[i]][[j]]$hl_classes <- hl_classes[j]
      melt_rates2[[i]][[j]]$threshold <- freq_plot[i]
    } 
  }
  
  melted_rates2 <- melt(melt_rates2, id.vars = c('age', 'hl_classes', 'threshold'))
  melted_rates2$hl_classes <- factor(melted_rates2$hl_classes, levels = hl_classes)
  
  melted_rates2$angle <- as.numeric(as.factor(melted_rates2$variable)) * (360 / length(unique(melted_rates2$variable)))
  
  # gg2 - Polar heatmap with HL classes
  gg2 <- ggplot(melted_rates2, aes(x = angle, y = age, fill = value)) +
    geom_tile() +
    geom_textpath(data = melted_rates2,
                  aes(x = angle, y = max(age) + 10, label = variable), 
                  color = "black", size = 3, linetype = 0) +
    geom_hline(yintercept = 0:3 * 5, color = "gray90") +
    coord_curvedpolar(theta = "x") +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    facet_grid(hl_classes ~ threshold) +
    labs(fill = "Proportion", x = "Frequency", y = "Age") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      legend.position = "left"  #left
    )
  
  # Prepare data for gg3 (polar plot with Sex classes)
  melt_rates3 <- list()
  
  for (i in 1:len_tresh) {
    melt_rates3[[i]] <- list()
    for (j in 1:length(sex_classes)) {
      melt_rates3[[i]][[j]] <- as.data.frame(cbind(df_rates[[3]][[i]][[j]],
                                                   spq_rqtes[[3]][[i]][[j]],
                                                   spn_rates[[3]][[i]][[j]]))
      colnames(melt_rates3[[i]][[j]]) <- c(paste(freq_vector), "SRT", "SNR")
      melt_rates3[[i]][[j]]$age <- sapply(strsplit(age_group_var, "-"),
                                          function(x) as.numeric(x[1]))
      melt_rates3[[i]][[j]]$sex_classes <- sex_classes[j]
      melt_rates3[[i]][[j]]$threshold <- freq_plot[i]
    } 
  }
  
  melted_rates3 <- melt(melt_rates3, id.vars = c('age', 'sex_classes', 'threshold'))
  melted_rates3$sex_classes <- factor(melted_rates3$sex_classes, levels = sex_classes)
  
  melted_rates3$angle <- as.numeric(as.factor(melted_rates3$variable)) * (360 / length(unique(melted_rates3$variable)))
  
  # gg3 - Polar heatmap with Sex classes
  gg3 <- ggplot(melted_rates3, aes(x = angle, y = age, fill = value)) +
    geom_tile() +
    geom_textpath(data = melted_rates3,
                  aes(x = angle, y = max(age) + 10, label = variable), 
                  color = "black", size = 3, linetype = 0) +
    geom_hline(yintercept = 0:3 * 5, color = "gray90") +
    coord_curvedpolar(theta = "x") +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    facet_grid(sex_classes ~ threshold) +
    labs(fill = "Proportion", x = "Frequency", y = "Age") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      legend.position = "left"
    )
  
  
  melted_rates$label <- "Overall"
  melted_rates2$label <- melted_rates2$hl_classes  
  melted_rates3$label <- melted_rates3$sex_classes 
  
  melted_rates2 <- subset(melted_rates2, select = -hl_classes)
  melted_rates3 <- subset(melted_rates3, select = -sex_classes)
  melted_rates2 <- subset(melted_rates2, select = -L2)
  melted_rates3 <- subset(melted_rates3, select = -L2)
  
  combined_data <- rbind(melted_rates, melted_rates2, melted_rates3)
  combined_data$label <- factor(combined_data$label, 
                                levels = c("Overall", 
                                           sex_classes, 
                                           hl_classes))
  
  gg4 <- ggplot(combined_data, aes(x = angle, y = age, fill = value)) +
    geom_tile() +
    geom_textpath(data = combined_data,
                  aes(x = angle, y = max(age) + 10, label = variable), 
                  color = "black", size = 3, linetype = 0) +
    geom_hline(yintercept = 0:3 * 5, color = "gray90") +
    coord_curvedpolar(theta = "x") +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    facet_grid(label ~ threshold) +
    labs(fill = "Proportion", x = "Frequency", y = "Age") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      legend.position = "left"
    )
  
  
  # Return a list of all polar plots
  return(list(gg1 = gg1, gg2 = gg2, 
              gg3 = gg3, gg4 = gg4))
}



plot_coeff_by_age <- function(model, treshold_hearing_rate,
                              age_group_var, freq_vector,
                              color_thresh) {
  
  plot_coefficients <- function(coef_data, x_var, y_var, 
                                title, x_label, y_label) {
    ggplot(coef_data, aes(x = !!sym(x_var), y = scale(value), 
                          group = `treshold`, color = `treshold`)) +
      geom_line(size = 1.5) +
      labs(title = "",
           x = x_label,
           y = y_label,
           color = "Probabilistic Level") +
      scale_color_manual(values = color_thresh) +
      theme_bw() +  
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(size = 16, face = "bold"),      
            axis.title.y = element_text(size = 20, face = "bold"),     
            legend.title = element_text(size = 14, face = "bold"),      
            legend.text = element_text(size = 12)     )
  }
  
  # Kappas
  k_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh, function(i)
    as.numeric(model[[i]]$kt)) ))
  colnames(k_byage) <- treshold_hearing_rate
  k_byage$frequency <- as.numeric(freq_vector)
  k_byage_melted <- melt(k_byage, id.vars = "frequency"  )
  colnames(k_byage_melted) <- c ("frequency", "treshold",  "value")
  
  #plot_kappas <- plot_coefficients(k_byage_melted, "frequency", "treshold", 
  #                                 "Kappas by Frequency", "Frequency", "Kappas")
  
  plot_kappas <- plot_coefficients(k_byage_melted, "frequency", "treshold", 
                                   expression(paste(kappa, " by Frequency")), 
                                   "Frequency", 
                                   expression(kappa))
  
  # As
  a_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh,  function(i) 
    as.numeric(model[[i]]$ax)) ))
  colnames(a_byage) <- treshold_hearing_rate
  a_byage$age <-  sapply(strsplit(age_group_var, "-"), function(x)
    as.numeric(x[1]))
  a_byage_melted <- melt(a_byage, id.vars = "age"  )
  colnames(a_byage_melted) <- c ("age", "treshold",  "value")
  
  #plot_as <- plot_coefficients(a_byage_melted, "age", "treshold", 
  #                            "As by Age", "Age", "As")
  
  plot_as <- plot_coefficients(a_byage_melted, "age", "treshold", 
                               expression(paste(alpha, " by Age")), 
                               "Age", 
                               expression(alpha))
  
  # Bs
  b_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh,  function(i) 
                      as.numeric(model[[i]]$bx)) ))
  colnames(b_byage) <- treshold_hearing_rate
  b_byage$age <-  sapply(strsplit(age_group_var, "-"), function(x) 
    as.numeric(x[1]))
  b_byage_melted <- melt(b_byage, id.vars = "age"  )
  colnames(b_byage_melted) <- c ("age", "treshold",  "value")
  
  #plot_bs <- plot_coefficients(b_byage_melted, "age", "treshold", 
  #                             "Bs by Age", "Age", "Bs")
  
  plot_bs <- plot_coefficients(b_byage_melted, "age", "treshold", 
                               expression(paste(beta, " by Age")), 
                               "Age", 
                               expression(beta))
  
  return(list(plot_kappas, plot_as, plot_bs))
}


plot_coeff_kb_age <- function(model, treshold_hearing_rate,
                              age_group_var, freq_vector) {
  
  # Kappas
  k_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh, function(i)
    as.numeric(model[[i]]$kt)) ))
  colnames(k_byage) <- treshold_hearing_rate
  k_byage$frequency <- as.numeric(freq_vector)
  
  # Bs
  b_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh,  function(i) 
    as.numeric(model[[i]]$bx)) ))
  colnames(b_byage) <- treshold_hearing_rate
  b_byage$age <-  sapply(strsplit(age_group_var, "-"), function(x) 
    as.numeric(x[1]))
  
  age_plot = sapply(strsplit(age_group_var, "-"), 
                    function(x) as.numeric(x[1]))
  
  # Calculate products of coefficients
  all_prods <- lapply(1:n_tresh, function(i) {
    
    prodkb <- t(t(b_byage[, i])) %*% as.vector(k_byage[, i]) 
    colnames(prodkb) <- freq_vector
    prodkb <- as.data.frame(prodkb)
    prodkb$age <- age_plot
    prodkb$age <- factor(prodkb$age)
    prodkb$threshold <- treshold_hearing_rate[i]
    
    return(prodkb)
    
  })
  
  # Combine all results into a single dataframe
  combined_prods <- do.call(rbind, all_prods)
  
  # Reshape data for plotting
  combined_prods2 <- melt(combined_prods, id.vars = c("age", "threshold") )
  combined_prods2$age <- as.numeric(as.character(combined_prods2$age))
  
  
  # Plot
  gg =  ggplot(combined_prods2, aes(x = age, 
                                    y = value, 
                                    color = variable,
                                    group = variable)) +
    geom_line(size = 1)  + 
    facet_wrap(~threshold,scales = "free_x", ncol = 5 ) +
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
    scale_x_continuous(breaks = seq(min(combined_prods2$age), 
                                    max(combined_prods2$age), by = 10)) +
    guides(color = guide_legend(ncol = 11))
  
  return(gg)
}





plot_coeff_by_age_hl <- function(model_by_age_hl, n_tresh, n_hlclasses,
                                 hl_classes,
                                 age_group_var, treshold_hearing_rate, 
                                 freq_vector,
                                 custom_colors) {
  k_by_age_hl <- lapply(1:n_tresh, function(i) as.data.frame(
    do.call(cbind, 
            lapply(1:(n_hlclasses), 
                   function(j) as.numeric(model_by_age_hl[[i]][[j]]$kt) ) 
    )))
  
  for (i in 1:n_tresh) {
    colnames(k_by_age_hl[[i]]) = hl_classes
    k_by_age_hl[[i]]$frequency = as.numeric(freq_vector)
    k_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  k_by_age_hl = do.call(rbind, k_by_age_hl)
  
  k_by_age_hl_melted = melt(k_by_age_hl, id.vars = c("treshold","frequency" ) )
  colnames(k_by_age_hl_melted) = c ("treshold","frequency",
                                    "HL degree",  "value")
  
  gg_k <- ggplot(k_by_age_hl_melted, aes(x = frequency,
                                         y = scale(value), 
                                         group = treshold,
                                         color = treshold)) +
    geom_line(size = 1.5) + facet_wrap(~`HL degree`, scales = "free") +
    labs(title = "",
         x = "Frequency",
         y = expression(kappa),
         color = "Probabilistic Level") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12))+
    scale_color_manual(values = custom_colors)
  
  a_by_age_hl <- lapply(1:n_tresh, function(i) as.data.frame(
    do.call(cbind, 
            lapply(1:(n_hlclasses), 
                   function(j) as.numeric(model_by_age_hl[[i]][[j]]$ax) ) 
    )))
  
  for (i in 1:n_tresh) {
    colnames(a_by_age_hl[[i]]) = hl_classes
    a_by_age_hl[[i]]$age = sapply(strsplit(age_group_var, "-"),
                                  function(x) as.numeric(x[1]))
    a_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  a_by_age_hl = do.call(rbind, a_by_age_hl)
  
  a_by_age_hl_melted = melt(a_by_age_hl, id.vars = c("treshold","age" )  )
  colnames(a_by_age_hl_melted) = c ("treshold","age",  "HL degree",  
                                    "value")
  
  gg_a <- ggplot(a_by_age_hl_melted, aes(x = age, y = scale(value), 
                                         group = treshold, color = treshold)) +
    geom_line(size = 1.5) + facet_wrap(~`HL degree`, scales = "free") +
    labs(title = "",
         x = "Age",
         y = expression(alpha),
         color = "Probabilistic Level") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12)    ) +
    scale_color_manual(values = custom_colors)
  
  b_by_age_hl <- lapply(1:n_tresh, function(i) as.data.frame(
    do.call(cbind, 
            lapply(1:(n_hlclasses), 
                   function(j) as.numeric(model_by_age_hl[[i]][[j]]$bx) ) 
    )))
  
  for (i in 1:n_tresh) {
    colnames(b_by_age_hl[[i]]) = hl_classes
    b_by_age_hl[[i]]$age = sapply(strsplit(age_group_var, "-"), 
                                  function(x) as.numeric(x[1]))
    b_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  b_by_age_hl = do.call(rbind, b_by_age_hl)
  
  b_by_age_hl_melted = melt(b_by_age_hl, id.vars = c("treshold","age" )  )
  colnames(b_by_age_hl_melted) = c ("treshold","age",  "HL degree",  "value")
  
  gg_b <- ggplot(b_by_age_hl_melted, aes(x = age, y = scale(value), 
                                         group = treshold, color = treshold)) +
    geom_line(size = 1.5) + facet_wrap(~`HL degree`, scales = "free") +
    labs(title = "",
         x = "Age",
         y = expression(beta),
         color = "Probabilistic Level") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12)    ) +
    scale_color_manual(values = custom_colors)
  
  return(list(gg_k, gg_a, gg_b,
              b_by_age_hl_melted, 
              a_by_age_hl_melted,
              k_by_age_hl_melted))
}


plot_coeff_kb_age_hl <- function(model_by_age_hl, 
                                 n_tresh, n_hlclasses,
                                 hl_classes,
                                 age_group_var, 
                                 treshold_hearing_rate, 
                                 freq_vector) {
  
  # Kappas
  k_by_age_hl <- lapply(1:n_tresh, 
                        function(i) as.data.frame(
                          do.call(cbind, 
                                  lapply(1:(n_hlclasses), 
                                         function(j) as.numeric(model_by_age_hl[[i]][[j]]$kt) ) 
                          )))
  
  for (i in 1:n_tresh) {
    colnames(k_by_age_hl[[i]]) = hl_classes
    k_by_age_hl[[i]]$frequency = as.numeric(freq_vector)
    k_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  #k_by_age_hl = do.call(rbind, k_by_age_hl)
  
  # Bs
  b_by_age_hl <- lapply(1:n_tresh, function(i) as.data.frame(
    do.call(cbind, 
            lapply(1:(n_hlclasses), 
                   function(j) as.numeric(model_by_age_hl[[i]][[j]]$bx) ) 
    )))
  
  for (i in 1:n_tresh) {
    colnames(b_by_age_hl[[i]]) = hl_classes
    b_by_age_hl[[i]]$age = sapply(strsplit(age_group_var, "-"), 
                                  function(x) as.numeric(x[1]))
    b_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  #b_by_age_hl = do.call(rbind, b_by_age_hl)
  
  age_plot = sapply(strsplit(age_group_var, "-"), 
                    function(x) as.numeric(x[1]))
  
  # Calculate products of coefficients
  all_prods <- lapply(1:n_tresh, function(i) {
    
    lapply(1:n_hlclasses, function(j) {
      
      prodkb <- t(t(b_by_age_hl[[i]][, j])) %*% as.vector(k_by_age_hl[[i]][, j]) 
      colnames(prodkb) <- freq_vector
      prodkb <- as.data.frame(prodkb)
      prodkb$age <- age_plot
      prodkb$age <- factor(prodkb$age)
      prodkb$threshold <- treshold_hearing_rate[i]
      prodkb$hl_loss = hl_classes[j]
      
      return(prodkb)
    })
  })
  
  # Combine all results into a single dataframe
  combined_prods <- lapply(1:n_tresh, function(i) { do.call(rbind, all_prods[[i]]) })
  
  # Reshape data for plotting
  combined_prods2 <- melt(combined_prods, id.vars = c("age", "threshold", "hl_loss") )
  combined_prods2$hl_loss <- factor(combined_prods2$hl_loss, levels = hl_classes)
  combined_prods2$age <- as.numeric(as.character(combined_prods2$age))
  
  
  # Plot
  gg =  ggplot(combined_prods2, aes(x = age, 
                                    y = value, 
                                    color = variable,
                                    group = variable)) +
    geom_line(size = 1)  + 
    facet_grid(threshold~hl_loss,scales = "free" ) +
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
    scale_x_continuous(breaks = seq(min(combined_prods2$age), 
                                    max(combined_prods2$age), by = 10))
  
  return(list(gg,combined_prods2))
}




plot_coeff_bikappa_age <- function(model, treshold_hearing_rate,
                                   age_group_var, freq_vector,
                                   custom_colors) {
  
  # Kappas
  k_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh, function(i)
    as.numeric(model[[i]]$kt)) ))
  colnames(k_byage) <- treshold_hearing_rate
  k_byage$frequency <- as.numeric(freq_vector)
  
  # Bs
  b_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh,  function(i) 
    as.numeric(model[[i]]$bx)) ))
  colnames(b_byage) <- treshold_hearing_rate
  b_byage$age <-  sapply(strsplit(age_group_var, "-"), function(x) 
    as.numeric(x[1]))
  
  age_plot = sapply(strsplit(age_group_var, "-"), 
                    function(x) as.numeric(x[1]))
  
  # Calculate products of coefficients
  bik <- lapply(1:n_tresh, function(i) {
    
    bik_temp = do.call(rbind,lapply(1:length(age_plot),function(j)
      as.numeric(model_hl[[i]]$bx[j] * model_hl[[i]]$kt)))
    
    colnames(bik_temp) = freq_vector
    
    bik_temp = as.data.frame(bik_temp)
    
    bik_temp$age = age_plot
    
    bik_temp$threshold = treshold_hearing_rate[i]
    
    return(bik_temp)
    
  })
  
  # Combine all results into a single dataframe
  combined_bik <- do.call(rbind, bik)
  
  # Reshape data for plotting
  combined_bik2 <- melt(combined_bik, id.vars = c("age", "threshold") )
  combined_bik2$age <- as.numeric(as.character(combined_bik2$age))
  
  
  # Plot
  gg =  ggplot(combined_bik2, aes(x = as.factor(variable), 
                                  y = value, 
                                  color = as.factor(age),
                                  group = as.factor(age) )) +
    geom_line()  +
    geom_point() +  
    facet_wrap(~threshold,scales = "free_x" , ncol = 5 ) +
    scale_color_manual(values = custom_colors) +
    labs(title = "",
         x = "Frequency",
         y = "b*k",
         color = "Age") +
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
  
  return(gg)
}



plot_coeff_bikappa_age_hl <- function(model_by_age_hl, 
                                      n_tresh, n_hlclasses,
                                      hl_classes,
                                      age_group_var, 
                                      treshold_hearing_rate, 
                                      freq_vector,
                                      custom_colors) {
  
  # Kappas
  k_by_age_hl <- lapply(1:n_tresh, 
                        function(i) as.data.frame(
                          do.call(cbind, 
                                  lapply(1:(n_hlclasses), 
                                         function(j) as.numeric(model_by_age_hl[[i]][[j]]$kt) ) 
                          )))
  
  for (i in 1:n_tresh) {
    colnames(k_by_age_hl[[i]]) = hl_classes
    k_by_age_hl[[i]]$frequency = as.numeric(freq_vector)
    k_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  #k_by_age_hl = do.call(rbind, k_by_age_hl)
  
  # Bs
  b_by_age_hl <- lapply(1:n_tresh, function(i) as.data.frame(
    do.call(cbind, 
            lapply(1:(n_hlclasses), 
                   function(j) as.numeric(model_by_age_hl[[i]][[j]]$bx) ) 
    )))
  
  for (i in 1:n_tresh) {
    colnames(b_by_age_hl[[i]]) = hl_classes
    b_by_age_hl[[i]]$age = sapply(strsplit(age_group_var, "-"), 
                                  function(x) as.numeric(x[1]))
    b_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  #b_by_age_hl = do.call(rbind, b_by_age_hl)
  
  age_plot = sapply(strsplit(age_group_var, "-"), 
                    function(x) as.numeric(x[1]))
  
  # Calculate products of coefficients
  bik <- lapply(1:n_tresh, function(i) {
    
    lapply(1:n_hlclasses, function(h) {
      
      bik_temp = do.call(rbind,lapply(1:length(age_plot),function(j)
        as.numeric(b_by_age_hl[[i]][[h]][j] * k_by_age_hl[[i]][[h]])))
      
      colnames(bik_temp) = freq_vector
      bik_temp = as.data.frame(bik_temp)
      bik_temp$age = age_plot
      bik_temp$threshold = treshold_hearing_rate[i]
      bik_temp$hl_loss = hl_classes[h]
      
      
      return(bik_temp)
      
    })
  })
  
  # Combine all results into a single dataframe
  combined_bik <- lapply(1:n_tresh, function(i) { do.call(rbind, bik[[i]]) })
  
  # Reshape data for plotting
  combined_bik2 <- melt(combined_bik, id.vars = c("age", "threshold", "hl_loss") )
  combined_bik2$hl_loss <- factor(combined_bik2$hl_loss, levels = hl_classes)
  combined_bik2$age <- as.numeric(as.character(combined_bik2$age))
  
  
  # Plot
  gg =  ggplot(combined_bik2, aes(x = as.factor(variable), 
                                  y = value, 
                                  color = as.factor(age),
                                  group = as.factor(age) )) +
    geom_line()  +
    geom_point() +  
    facet_grid(threshold~hl_loss,scales = "free" ) +
    scale_color_manual(values = custom_colors) +
    labs(title = "",
         x = "Frequency",
         y = "b*k",
         color = "Age") +
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
  
  return(list(gg, combined_bik2))
}





plot_coeff_all <- function(model_hl, model_by_age_hl, 
                           n_tresh, n_hlclasses,
                           hl_classes, age_group_var, 
                           treshold_hearing_rate, 
                           freq_vector,
                           custom_colors,
                           type) {
  
  hl_classes2 <- c("Overall", hl_classes)  # Ensure hl_classes is defined
  
  k_by_age_hl <- lapply(1:n_tresh, function(i) as.data.frame(
    do.call(cbind, 
            lapply(1:(n_hlclasses), 
                   function(j) as.numeric(model_by_age_hl[[i]][[j]]$kt) ) 
    )))
  
  for (i in 1:n_tresh) {
    colnames(k_by_age_hl[[i]]) = hl_classes
    k_by_age_hl[[i]]$frequency = as.numeric(freq_vector)
    k_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  k_by_age_hl = do.call(rbind, k_by_age_hl)
  
  k_by_age_hl_melted = melt(k_by_age_hl, id.vars = c("treshold","frequency" ) )
  colnames(k_by_age_hl_melted) = c ("treshold","frequency",
                                    "HL degree",  "value")
  
  # Kappas
  k_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh, function(i)
    as.numeric(model_hl[[i]]$kt)) ))
  colnames(k_byage) <- treshold_hearing_rate
  k_byage$frequency <- as.numeric(freq_vector)
  k_byage_melted <- melt(k_byage, id.vars = "frequency"  )
  colnames(k_byage_melted) <- c ("frequency", "treshold",  "value")
  k_byage_melted$`HL degree` <- "Overall"
  k_byage_melted_reordered <- k_byage_melted %>%
    dplyr::select(treshold, frequency, `HL degree`, value)
  
  k_final = rbind(k_byage_melted_reordered,
                  k_by_age_hl_melted)
  
  k_final$`HL degree` <- factor(k_final$`HL degree`, levels = hl_classes2)
  
  k_final_20 <- k_final[k_final$treshold == "20%", ]
  
  gg_k <- ggplot(k_final, aes(x = frequency,
                                         y = scale(value), 
                                         group = `HL degree`,
                                         color = `HL degree`)) +
    geom_line(aes(linetype = ifelse(`HL degree` == "Overall",
                                    "dotdash", "solid")),
              size = 0.9) +
    facet_wrap(~treshold, scales = "free", ncol = 5) +
    labs(title = "",
         x = "Frequency",
         y = expression(kappa),
         color = paste(type)) +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12))+
    scale_color_manual(values = c("red", custom_colors))  + 
    scale_linetype_manual(values = c("dotdash" = "dotdash", "solid" = "solid")) +
    guides(color = guide_legend(ncol = 6), linetype = "none")
  
  gg_k_unique = ggplot(k_final_20, aes(x = frequency,
                                    y = scale(value), 
                                    group = `HL degree`,
                                    color = `HL degree`)) +
    geom_line(aes(linetype = ifelse(`HL degree` == "Overall",
                                    "dotdash", "solid")),
              size = 1.5) +
    labs(title = "",
         x = "Frequency",
         y = expression(kappa),
         color = paste(type)) +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12))+
    scale_color_manual(values = c("red", custom_colors))  + 
    scale_linetype_manual(values = c("dotdash" = "dotdash", "solid" = "solid")) +
    guides(color = guide_legend(ncol = 6), linetype = "none")
    
  
  a_by_age_hl <- lapply(1:n_tresh, function(i) as.data.frame(
    do.call(cbind, 
            lapply(1:(n_hlclasses), 
                   function(j) as.numeric(model_by_age_hl[[i]][[j]]$ax) ) 
    )))
  
  for (i in 1:n_tresh) {
    colnames(a_by_age_hl[[i]]) = hl_classes
    a_by_age_hl[[i]]$age = sapply(strsplit(age_group_var, "-"),
                                  function(x) as.numeric(x[1]))
    a_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  a_by_age_hl = do.call(rbind, a_by_age_hl)
  
  a_by_age_hl_melted = melt(a_by_age_hl, id.vars = c("treshold","age" )  )
  colnames(a_by_age_hl_melted) = c ("treshold","age",  "HL degree",  
                                    "value")
  
  a_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh,  function(i) 
    as.numeric(model_hl[[i]]$ax)) ))
  colnames(a_byage) <- treshold_hearing_rate
  a_byage$age <-  sapply(strsplit(age_group_var, "-"), function(x)
    as.numeric(x[1]))
  a_byage_melted <- melt(a_byage, id.vars = "age"  )
  colnames(a_byage_melted) <- c ("age", "treshold",  "value")
  a_byage_melted$`HL degree` <- "Overall"
  a_byage_melted_reordered <- a_byage_melted %>%
    dplyr::select(treshold, age, `HL degree`, value)
  
  a_final = rbind(a_byage_melted_reordered,
                  a_by_age_hl_melted)
  
  a_final$`HL degree` <- factor(a_final$`HL degree`, levels = hl_classes2)
  
  a_final_20 <- a_final[a_final$treshold == "20%", ]
  
  gg_a <- ggplot(a_final, aes(x = age, y = value, 
                                         group = `HL degree`,
                              color = `HL degree`)) +
    geom_line(aes(linetype = ifelse(`HL degree` == "Overall",
                                    "dotdash", "solid")),
              size = 0.9) + facet_wrap(~treshold, 
                                       scales = "free",
                                       ncol = 5) +
    labs(title = "",
         x = "Age",
         y = expression(alpha),
         color = paste(type)) +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12)    ) +
    scale_color_manual(values =  c("red", custom_colors)) +
    scale_linetype_manual(values = c("dotdash" = "dotdash", "solid" = "solid")) +
    guides(color = guide_legend(ncol = 6), linetype = "none")
  
  gg_a_unique = ggplot(a_final_20, aes(x = age, y = value, 
                                    group = `HL degree`,
                                    color = `HL degree`)) +
    geom_line(aes(linetype = ifelse(`HL degree` == "Overall",
                                    "dotdash", "solid")),
              size = 1.5) + 
    labs(title = "",
         x = "Age",
         y = expression(alpha),
         color = paste(type)) +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12)    ) +
    scale_color_manual(values =  c("red", custom_colors)) +
    scale_linetype_manual(values = c("dotdash" = "dotdash", "solid" = "solid")) +
    guides(color = guide_legend(ncol = 6), linetype = "none")
  
  b_by_age_hl <- lapply(1:n_tresh, function(i) as.data.frame(
    do.call(cbind, 
            lapply(1:(n_hlclasses), 
                   function(j) as.numeric(model_by_age_hl[[i]][[j]]$bx) ) 
    )))
  
  for (i in 1:n_tresh) {
    colnames(b_by_age_hl[[i]]) = hl_classes
    b_by_age_hl[[i]]$age = sapply(strsplit(age_group_var, "-"), 
                                  function(x) as.numeric(x[1]))
    b_by_age_hl[[i]]$treshold = as.factor(treshold_hearing_rate[i])
  }
  
  b_by_age_hl = do.call(rbind, b_by_age_hl)
  
  b_by_age_hl_melted = melt(b_by_age_hl, id.vars = c("treshold","age" )  )
  colnames(b_by_age_hl_melted) = c ("treshold","age",  "HL degree",  "value")
  
  
  b_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh,  function(i) 
    as.numeric(model_hl[[i]]$bx)) ))
  colnames(b_byage) <- treshold_hearing_rate
  b_byage$age <-  sapply(strsplit(age_group_var, "-"), function(x)
    as.numeric(x[1]))
  b_byage_melted <- melt(b_byage, id.vars = "age"  )
  colnames(b_byage_melted) <- c ("age", "treshold",  "value")
  b_byage_melted$`HL degree` <- "Overall"
  b_byage_melted_reordered <- b_byage_melted %>%
    dplyr::select(treshold, age, `HL degree`, value)
  
  b_final = rbind(b_byage_melted_reordered,
                  b_by_age_hl_melted)
  
  b_final$`HL degree` <- factor(b_final$`HL degree`, levels = hl_classes2)
  
  b_final_20 <- b_final[b_final$treshold == "20%", ]
  
  gg_b <- ggplot(b_final, aes(x = age, y = value, 
                              group = `HL degree`, 
                              color = `HL degree`)) +
    geom_line(aes(linetype = ifelse(`HL degree` == "Overall",
                                    "dotdash", "solid")),
              size = 0.9) + 
    facet_wrap(~treshold,  scales = "free", ncol = 5) +
    labs(title = "",
         x = "Age",
         y = expression(beta),
         color = paste(type)) +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12)    ) +
    scale_color_manual(values =  c("red", custom_colors)) +
    scale_linetype_manual(values = c("dotdash" = "dotdash", "solid" = "solid")) +
    guides(color = guide_legend(ncol = 6), linetype = "none")
  
  gg_b_unique <- ggplot(b_final_20, aes(x = age, y = value, 
                              group = `HL degree`, 
                              color = `HL degree`)) +
    geom_line(aes(linetype = ifelse(`HL degree` == "Overall",
                                    "dotdash", "solid")),
              size = 1.5) + 
    labs(title = "",
         x = "Age",
         y = expression(beta),
         color = paste(type)) +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 16, face = "bold"),      
          axis.title.y = element_text(size = 20, face = "bold"),     
          legend.title = element_text(size = 14, face = "bold"),      
          legend.text = element_text(size = 12)    ) +
    scale_color_manual(values =  c("red", custom_colors)) +
    scale_linetype_manual(values = c("dotdash" = "dotdash", "solid" = "solid")) +
    guides(color = guide_legend(ncol = 6), linetype = "none")
  
  return(list(gg_k, gg_a, gg_b,
              b_final, 
              a_final,
              k_final,
              gg_k_unique, gg_a_unique, gg_b_unique,
              a_final_20, b_final_20, k_final_20))
}




get.series <- function(data,series)
{
  if(!is.el(series,names(data)))
    stop(paste("Series",series,"not found"))
  i <- match(toupper(series),toupper(names(data)))
  return(as.matrix(data[[i]]))
}


is.el <- function(el,set)
{
  is.element(toupper(el),toupper(set))
}

fitmx <- function (kt,ax,bx,transform=FALSE)
{
  # Derives mortality rates from kt mortality index,
  # per Lee-Carter method
  clogratesfit <- outer(kt, bx)
  logratesfit <- sweep(clogratesfit,2,ax,"+")
  if(transform)
    return(logratesfit)
  else
    return(exp(logratesfit))
}


my_bms <- function (data, series = names(data$rate)[1], years = data$year, 
                    ages = data$age, max.age = 100, minperiod = 20, 
                    breakmethod = c("bms", "bai"), scale = FALSE, 
                    restype = c("logrates", "rates", "deaths"), 
                    interpolate = FALSE) 
{
  restype <- match.arg(restype)
  breakmethod <- match.arg(breakmethod)
  out <- my_lca(data, series = series, years = years, ages = ages, 
             max.age = max.age, adjust = "dxt", chooseperiod = TRUE, 
             minperiod = minperiod, scale = scale, restype = restype, 
             breakmethod = breakmethod, interpolate = interpolate)
  out$call <- match.call()
  return(out)
}


my_lca<- function (data, series = names(data$rate)[1], 
                   years = data$year, 
                   ages = data$age, max.age = 100, adjust = c("dt", "dxt", 
                                                              "e0", "none"), 
                   chooseperiod = FALSE, minperiod = 20, 
                   breakmethod = c("bai", "bms"), scale = FALSE,
                   restype = c("logrates", "rates", "deaths"), interpolate = FALSE) 
{
  if (!inherits(data, "demogdata")) {
    stop("Not demography data")
  }
  if (!any(data$type == c("mortality", "fertility"))) {
    stop("Neither mortality nor fertility data")
  }
  adjust <- match.arg(adjust)
  restype <- match.arg(restype)
  breakmethod <- match.arg(breakmethod)
  data <- extract.ages(data, ages, combine.upper = FALSE)
  if (max.age < max(ages)) 
    data <- extract.ages(data, min(ages):max.age, combine.upper = TRUE)
  startage <- min(data$age)
  mx <- get.series(data$rate, series)
  pop <- get.series(data$pop, series)
  startyear <- min(years)
  stopyear <- max(years)
  if (startyear > max(data$year) | stopyear < min(data$year)) 
    stop("Year not found")
  startyear <- max(startyear, min(data$year))
  if (!is.null(stopyear)) 
    stopyear <- min(stopyear, max(data$year))
  else stopyear <- max(data$year)
  id2 <- stats::na.omit(match(startyear:stopyear, data$year))
  mx <- mx[, id2]
  pop <- pop[, id2]
  year <- data$year[id2]
  deltat <- year[2] - year[1]
  ages <- data$age
  n <- length(ages)
  m <- length(year)
  mx <- matrix(mx, nrow = n, ncol = m)
  colnames(mx) <- year
  rownames(mx) <- ages
  if (interpolate) {
    mx[is.na(mx)] <- 0
    if (sum(abs(mx) < 1e-09, na.rm = TRUE) > 0) {
      warning("Replacing zero values with estimates")
      for (i in 1:n) mx[i, ] <- fill.zero(mx[i, ])
    }
  }
  mx <- t(mx)
  mx[mx == 0] <- NA
  logrates <- log(mx)
  pop <- t(pop)
  deaths <- pop * mx
  ax <- apply(logrates, 2, mean, na.rm = TRUE)
  if (sum(ax < -1e+09) > 0) 
    stop(sprintf("Some %s rates are zero.\n Try reducing the maximum age or setting interpolate=TRUE.", 
                 data$type))
  clogrates <- sweep(logrates, 2, ax)
  svd.mx <- svd(clogrates)
  sumv <- sum(svd.mx$v[, 1])
  bx <- svd.mx$v[, 1]/sumv
  kt <- svd.mx$d[1] * svd.mx$u[, 1] * sumv
  ktadj <- rep(0, m)
  logdeathsadj <- matrix(NA, n, m)
  z <- log(t(pop)) + ax
  x <- 1:m
  ktse <- stats::predict(stats::lm(kt ~ x), se.fit = TRUE)$se.fit
  ktse[is.na(ktse)] <- 1
  agegroup = ages[4] - ages[3]
  if (adjust == "dxt") {
    options(warn = -1)
    for (i in 1:m) {
      y <- as.numeric(deaths[i, ])
      zi <- as.numeric(z[, i])
      weight <- as.numeric(zi > -1e-08)
      
      # Check if weight contains only zeros
      if(all(weight == 0)) {
        weight <- NULL
      }
      
      yearglm <- stats::glm(y ~ offset(zi) - 1 + bx, family = stats::poisson, 
                            weights = weight)
      ktadj[i] <- yearglm$coef[1]
      logdeathsadj[, i] <- z[, i] + bx * ktadj[i]
    }
    options(warn = 0)
  }
  else if (adjust == "dt") {
    FUN <- function(p, Dt, bx, ax, popi) {
      Dt - sum(exp(p * bx + ax) * popi)
    }
    for (i in 1:m) {
      if (i == 1) 
        guess <- kt[1]
      else guess <- mean(c(ktadj[i - 1], kt[i]))
      ktadj[i] <- findroot(FUN, guess = guess, margin = 3 * 
                             ktse[i], ax = ax, bx = bx, popi = pop[i, ], 
                           Dt = sum(as.numeric(deaths[i, ])))
      logdeathsadj[, i] <- z[, i] + bx * ktadj[i]
    }
  }
  else if (adjust == "e0") {
    e0 <- apply(mx, 1, get.e0, agegroup = agegroup, sex = series, 
                startage = startage)
    FUN2 <- function(p, e0i, ax, bx, agegroup, series, startage) {
      e0i - estimate.e0(p, ax, bx, agegroup, series, startage)
    }
    for (i in 1:m) {
      if (i == 1) 
        guess <- kt[1]
      else guess <- mean(c(ktadj[i - 1], kt[i]))
      ktadj[i] <- findroot(FUN2, guess = guess, margin = 3 * 
                             ktse[i], e0i = e0[i], ax = ax, bx = bx, 
                           agegroup = agegroup, 
                           series = series, startage = startage)
    }
  }
  else if (adjust == "none") 
    ktadj <- kt
  else stop("Unknown adjustment method")
  kt <- ktadj
  if (chooseperiod) {
    if (breakmethod == "bai") {
      x <- 1:m
      bp <- strucchange::breakpoints(kt ~ x)$breakpoints
      bp <- bp[bp <= (m - minperiod)]
      bestbreak <- max(bp)
      return(my_lca(data, series, year[(bestbreak + 1):m], 
                 ages = ages, max.age = max.age, adjust = adjust, 
                 interpolate = interpolate, chooseperiod = FALSE, 
                 scale = scale))
    }
    else {
      RS <- devlin <- devadd <- numeric(m - 2)
      for (i in 1:(m - 2)) {
        tmp <- my_lca(data, series, year[i:m], ages = ages, 
                   max.age = max.age, adjust = adjust, chooseperiod = FALSE, 
                   interpolate = interpolate, scale = scale)
        devlin[i] <- tmp$mdev[2]
        devadd[i] <- tmp$mdev[1]
        RS[i] <- (tmp$mdev[2]/tmp$mdev[1])
      }
      bestbreak <- order(RS[1:(m - minperiod)])[1] - 1
      out <- my_lca(data, series, year[(bestbreak + 1):m], 
                 ages = ages, max.age = max.age, adjust = adjust, 
                 chooseperiod = FALSE, interpolate = interpolate, 
                 scale = scale)
      out$mdevs <- ts(cbind(devlin, devadd, RS), start = startyear, 
                      deltat = deltat)
      dimnames(out$mdevs)[[2]] <- c("Mean deviance total", 
                                    "Mean deviance base", "Mean deviance ratio")
      return(out)
    }
  }
  logfit <- fitmx(kt, ax, bx, transform = TRUE)
  colnames(logfit) <- ages
  rownames(logfit) <- year
  if (restype == "logrates") {
    fit <- logfit
    res <- logrates - fit
  }
  else if (restype == "rates") {
    fit <- exp(logfit)
    res <- exp(logrates) - fit
  }
  else if (restype == "deaths") {
    fit <- exp(logfit) * pop
    res <- deaths - fit
  }
  residuals <- fts(ages, t(res), frequency = 1/deltat, start = years[1], 
                   xname = "Age", yname = paste("Residuals", data$type, 
                                                "rate"))
  fitted <- fts(ages, t(fit), frequency = 1/deltat, start = years[1], 
                xname = "Age", yname = paste("Fitted", data$type, "rate"))
  names(ax) <- names(bx) <- ages
  if (scale) {
    avdiffk <- -mean(diff(kt))
    bx <- bx * avdiffk
    kt <- kt/avdiffk
  }
  deathsadjfit <- exp(logfit) * pop
  drift <- mean(diff(kt))
  ktlinfit <- mean(kt) + drift * (1:m - (m + 1)/2)
  deathslinfit <- fitmx(ktlinfit, ax, bx, transform = FALSE) * 
    pop
  dflogadd <- (m - 2) * (n - 1)
  mdevlogadd <- 2/dflogadd * sum(deaths * log(deaths/deathsadjfit) - 
                                   (deaths - deathsadjfit))
  dfloglin <- (m - 2) * n
  mdevloglin <- 2/dfloglin * sum(deaths * log(deaths/deathslinfit) - 
                                   (deaths - deathslinfit))
  mdev <- c(mdevlogadd, mdevloglin)
  output <- list(label = data$label, age = ages, year = year, 
                 mx = t(mx), ax = ax, bx = bx, kt = ts(kt, start = startyear, 
                                                       deltat = deltat), 
                 residuals = residuals, fitted = fitted, 
                 varprop = svd.mx$d[1]^2/sum(svd.mx$d^2),
                 y = fts(ages,  t(mx), start = years[1],
                         frequency = 1/deltat,  xname = "Age", 
                         yname = ifelse(data$type == "mortality", "Mortality", 
                                        "Fertility")), mdev = mdev)
  names(output)[4] <- series
  output$call <- match.call()
  names(output$mdev) <- c("Mean deviance base", "Mean deviance total")
  output$adjust <- adjust
  output$type <- data$type
  return(structure(output, class = "lca"))
}




process_summary <- function(partial_step2_hl, n_loop, freq_vector,
                            treshold_SPIQ, treshold_SPIN, hl_classes) {
  # Step 1: Compute summary for each model in partial_step2_hl
  sum_part_step2_hl <- lapply(1:n_loop, function(i) summary(partial_step2_hl[[i]]))
  
  # Step 2: Extract estimates 
  extract_estimates <- function(summary_output, column_index) {
    estimates <- lapply(summary_output, function(model_output) 
      coef(model_output)[, "Estimate"][column_index])
  }
  
  est_pstep2_spiq_hl <- lapply(1:n_loop, function(i) as.numeric(do.call(rbind,
                                                                        extract_estimates(sum_part_step2_hl[[i]], 2))) )
  est_pstep2_spin_hl <-  lapply(1:n_loop, function(i) as.numeric(do.call(rbind,
                                                                         extract_estimates(sum_part_step2_hl[[i]], 3))) )
  
  # Step 3: Extract p-values 
  extract_pvalues <- function(summary_output, column_index) {
    p_values <- lapply(summary_output, function(model_output) {
      round(as.numeric(coef(model_output)[, "Pr(>|t|)"][column_index]), 3)
    })
  }
  
  p_pstep2_spiq_hl <- lapply(1:n_loop, function(i) as.numeric(do.call(rbind,
                                                                      extract_pvalues(sum_part_step2_hl[[i]], 2))) )
  p_pstep2_spin_hl <- lapply(1:n_loop, function(i) as.numeric(do.call(rbind,
                                                                      extract_pvalues(sum_part_step2_hl[[i]], 3))) )
  
  est_pstep2_spiq_hl = do.call(rbind, est_pstep2_spiq_hl)
  est_pstep2_spin_hl = do.call(rbind, est_pstep2_spin_hl)
  p_pstep2_spiq_hl = do.call(rbind, p_pstep2_spiq_hl)
  p_pstep2_spin_hl = do.call(rbind, p_pstep2_spin_hl)
  
  
  # Step 4: Create data frames for estimates and p-values
  est_pstep2_spiq_hl <- as.data.frame(est_pstep2_spiq_hl)
  est_pstep2_spin_hl <- as.data.frame(est_pstep2_spin_hl)
  p_pstep2_spiq_hl <- as.data.frame(p_pstep2_spiq_hl)
  p_pstep2_spin_hl <- as.data.frame(p_pstep2_spin_hl)
  
  spiq_t = treshold_SPIQ
  spin_t = treshold_SPIN
  
  # Step 5: Add column names and nh_classe, SPIQ/SPIN thresholds
  colnames(est_pstep2_spiq_hl) <- colnames(est_pstep2_spin_hl) <- colnames(p_pstep2_spiq_hl) <- colnames(p_pstep2_spin_hl) <- freq_vector
  est_pstep2_spiq_hl$hl_classes <- p_pstep2_spiq_hl$hl_classes <- hl_classes
  est_pstep2_spin_hl$hl_classes <- p_pstep2_spin_hl$hl_classes  <- hl_classes
  
  est_pstep2_spiq_hl$SPIQ_tresh <- p_pstep2_spiq_hl$SPIQ_tresh <- spiq_t
  est_pstep2_spin_hl$SPIN_tresh <- p_pstep2_spin_hl$SPIN_tresh  <- spin_t
  
  
  # Return the results
  list(est_pstep2_spiq_hl = est_pstep2_spiq_hl,
       est_pstep2_spin_hl = est_pstep2_spin_hl,
       p_pstep2_spiq_hl = p_pstep2_spiq_hl,
       p_pstep2_spin_hl = p_pstep2_spin_hl)
}






plot_from_list <- function(res_list, variable_type, title, variable_type2) {
  
  remove_hl_classes <- function(df) {
    df <- df[, !(names(df) %in% 'hl_classes'), drop = FALSE]
    return(df)
  }
  thresh_colors = c("#F0E8E0", "#FFDDC1", "#FF8C69", "#FF6347", "#FF0000")
  
  
  # Remove 'hl_classes' column from each list element using lapply
  res_list <- lapply(res_list, remove_hl_classes)
  
  # Determine the suffix based on variable_type2
  suffix <- if (variable_type2 == "hl") {
    "_hl"
  } else if (variable_type2 == "sex") {
    "_sex"
  } else {
    stop("Invalid variable_type2. Please specify either 'HL' or 'SEX'.")
  }
  
  # Construct the data frame names dynamically
  if (variable_type == "SPIQ") {
    melt_est <- melt(res_list[[paste0("est_pstep2_spiq", suffix)]], id.vars = "SPIQ_tresh")
    melt_p <- melt(res_list[[paste0("p_pstep2_spiq", suffix)]], id.vars = "SPIQ_tresh")
  } else if (variable_type == "SPIN") {
    melt_est <- melt(res_list[[paste0("est_pstep2_spin", suffix)]], id.vars = "SPIN_tresh")
    melt_p <- melt(res_list[[paste0("p_pstep2_spin", suffix)]], id.vars = "SPIN_tresh")
  } else {
    stop("Invalid variable type. Please specify either 'SPIQ' or 'SPIN'.")
  }
  

  # Assign melted data frames
  melt_pstep2 <- melt_est
  melt_pstep2$color <- melt_p$value
  
  # Determine colors based on p-values
  melt_pstep2$color2 <- ifelse(melt_pstep2$color < 0.01, "cyan",
                               ifelse(melt_pstep2$color <= 0.05, "white", "blue"))
  melt_pstep2$color2 <- factor(melt_pstep2$color2, levels = c("cyan", "white", "blue"))
  melt_pstep2$color3 <- thresh_colors[match(melt_pstep2[[paste0(variable_type, "_tresh")]],
                                            c("20%", "40%", "60%", "80%", "90%"))]
  
  melt_pstep2$color2 <- as.character(melt_pstep2$color2)
  
  # Create a new column to store the final fill color
  melt_pstep2$final_fill_color <- ifelse(melt_pstep2$color2 %in% c("white", "cyan"), 
                                              "white",  # All significant values will be white
                                         melt_pstep2$color3)
  
  # Set alpha value: Significant values are fully opaque, others have alpha 0.7
  melt_pstep2$alpha_value <- ifelse(melt_pstep2$final_fill_color == "white", 1, 0.7)
  
  significance_colors <- c("Significant at 0.01" = "cyan",
                           "Significant at 0.05" = "white")
  
  # Define colors for the plot
  quantile_colors <- c("Significant at 0.05" = "white",   
                       "Non-Significant (20%)" = "#F0E8E0",    
                       "Non-Significant (40%)" = "#FFDDC1", 
                       "Non-Significant (60%)" = "#FF8C69", 
                       "Non-Significant (80%)" = "#FF6347", 
                       "Non-Significant (90%)" = "#FF0000")
  
  
  # Combine both vectors into a named list
  all_colors <- c(significance_colors, quantile_colors)
  
  # Create a new column for descriptive color labels
  melt_pstep2$color_label <- ifelse(melt_pstep2$final_fill_color == "white", "Significant at 0.05",
                 ifelse(melt_pstep2$final_fill_color == "#F0E8E0", "Non-Significant (20%)",
                 ifelse(melt_pstep2$final_fill_color == "#FFDDC1", "Non-Significant (40%)",
                 ifelse(melt_pstep2$final_fill_color == "#FF8C69", "Non-Significant (60%)",
                 ifelse(melt_pstep2$final_fill_color == "#FF6347", "Non-Significant (80%)",
                 ifelse(melt_pstep2$final_fill_color == "#FF0000", "Non-Significant (90%)",
                 "Non-Significant"))))))
  
  all_levels =  c(
    "Significant at 0.05",
    "Non-Significant (20%)",
    "Non-Significant (40%)",
    "Non-Significant (60%)",
    "Non-Significant (80%)",
    "Non-Significant (90%)"
  )
  
    
  melt_pstep2$color_label <- factor(melt_pstep2$color_label, levels = all_levels)
  
  melt_pstep2$final_fill_color <- factor(melt_pstep2$final_fill_color,
                                         levels = c("white", "#F0E8E0", 
                                                    "#FFDDC1", "#FF8C69",
                                                    "#FF6347", "#FF0000"))
  
  
  # Plotting with manual color scale
  p <- ggplot(melt_pstep2, aes(x = as.factor(variable), 
                               y = as.factor(get(paste0(variable_type, "_tresh"))), 
                               fill = color_label,
                               label = round(value, 2),
                               alpha = alpha_value)) +
    geom_tile(color = "grey50", size = 0.5, show.legend = TRUE) +  # Add borders around tiles
    geom_text(vjust = 1, color = "black", size = 3)  +  # Adjust text size and color for better readability
    scale_fill_manual( values = c("Significant at 0.05" = "white",
                                  "Non-Significant (20%)" = "#F0E8E0",
                                  "Non-Significant (40%)" = "#FFDDC1",
                                  "Non-Significant (60%)" = "#FF8C69",
                                  "Non-Significant (80%)" = "#FF6347",
                                  "Non-Significant (90%)" = "#FF0000"),
                      drop = FALSE) +  # Use the combined vector for fill colors
    scale_alpha_continuous(guide = 'none', range = c(0.7, 1)) + 
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
      plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(size = 18)
    ) +
    labs(fill = "Estimate Significance",  
         x = "Frequencies",  
         y = paste("Probabilistic Level")) +
    ggtitle(title) +
    guides(
      fill = guide_legend(
        title = "Estimate Significance",  # Legend title
        override.aes = list(alpha = 1)  # Ensure alpha is shown in legend
      )
    )
  
  return(p)
}



plot_from_list2 <- function(res_list, variable_type, title,
                            non_significant_color,
                            variable_type2) {
  
  # Function to remove the 'hl_classes' column from a data frame
  remove_hl_classes <- function(df) {
    df <- df[, !(names(df) %in% 'hl_classes'), drop = FALSE]
    return(df)
  }
  
  # Remove 'hl_classes' column from each list element using lapply
  res_list <- lapply(res_list, remove_hl_classes)
  
  # Determine the suffix based on variable_type2
  suffix <- if (variable_type2 == "hl") {
    "_hl"
  } else if (variable_type2 == "sex") {
    "_sex"
  } else {
    stop("Invalid variable_type2. Please specify either 'HL' or 'SEX'.")
  }
  
  # Construct the data frame names dynamically
  if (variable_type == "SPIQ") {
    melt_est <- melt(res_list[[paste0("est_pstep2_spiq", suffix)]], id.vars = "SPIQ_tresh")
    melt_p <- melt(res_list[[paste0("p_pstep2_spiq", suffix)]], id.vars = "SPIQ_tresh")
  } else if (variable_type == "SPIN") {
    melt_est <- melt(res_list[[paste0("est_pstep2_spin", suffix)]], id.vars = "SPIN_tresh")
    melt_p <- melt(res_list[[paste0("p_pstep2_spin", suffix)]], id.vars = "SPIN_tresh")
  } else {
    stop("Invalid variable type. Please specify either 'SPIQ' or 'SPIN'.")
  }
  
  
  # Assign melted data frames
  melt_pstep2 <- melt_est
  melt_pstep2$color <- melt_p$value
  
  # Determine colors based on p-values
  melt_pstep2$final_fill_color <- ifelse(melt_pstep2$color < 0.05, 
                                         "white", 
                                         non_significant_color)
  
  # Set alpha value: Significant values are fully opaque, others have alpha 0.7
  melt_pstep2$alpha_value <- ifelse(melt_pstep2$final_fill_color == "white", 1, 0.7)
  
  # Define color labels based on significance and threshold levels
  melt_pstep2$color_label <- ifelse(melt_pstep2$final_fill_color == "white", 
                                    "Significant at 0.05",
                                    paste("Non-Significant", sep = ""))
  
  # Convert the final fill color to a factor for plotting
  melt_pstep2$final_fill_color <- factor(melt_pstep2$final_fill_color,
                                         levels = c("white", non_significant_color))
  
  # Plotting with manual color scale
  p <- ggplot(melt_pstep2, aes(x = as.factor(variable), 
                               y = as.factor(get(paste0(variable_type, "_tresh"))), 
                               fill = color_label,
                               label = round(value, 2),
                               alpha = alpha_value)) +
    geom_tile(color = "grey50", size = 0.5, show.legend = TRUE) +  # Add borders around tiles
    geom_text(vjust = 1, color = "black", size = 3) +  # Adjust text size and color for better readability
    scale_fill_manual(values = c("Significant at 0.05" = "white", 
                                 "Non-Significant" = non_significant_color),  # Use the specified color for non-significant values
                      drop = FALSE) +  # Use the combined vector for fill colors
    scale_alpha_continuous(guide = 'none', range = c(0.7, 1)) + 
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
      plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(size = 18)
    ) +
    labs(fill = "Estimate Significance",  
         x = "Frequencies",  
         y = paste("Probabilistic Level")) +
    ggtitle(title) +
    guides(
      fill = guide_legend(
        title = "Estimate Significance",  # Legend title
        override.aes = list(alpha = 1)  # Ensure alpha is shown in legend
      )
    )
  
  return(p)
}


plot_from_list3 <- function(res_list, variable_type, title,
                            non_significant_color,
                            variable_type2) {
  
  # Function to remove the 'hl_classes' column from a data frame
  remove_hl_classes <- function(df) {
    df <- df[, !(names(df) %in% 'hl_classes'), drop = FALSE]
    return(df)
  }
  
  # Remove 'hl_classes' column from each list element using lapply
  res_list <- lapply(res_list, remove_hl_classes)
  
  # Determine the suffix based on variable_type2
  suffix <- if (variable_type2 == "hl") {
    "_hl"
  } else if (variable_type2 == "sex") {
    "_sex"
  } else {
    stop("Invalid variable_type2. Please specify either 'HL' or 'SEX'.")
  }
  
  # Construct the data frame names dynamically
  if (variable_type == "SPIQ") {
    melt_est <- melt(res_list[[paste0("est_pstep2_spiq", suffix)]], id.vars = "SPIQ_tresh")
    melt_p <- melt(res_list[[paste0("p_pstep2_spiq", suffix)]], id.vars = "SPIQ_tresh")
  } else if (variable_type == "SPIN") {
    melt_est <- melt(res_list[[paste0("est_pstep2_spin", suffix)]], id.vars = "SPIN_tresh")
    melt_p <- melt(res_list[[paste0("p_pstep2_spin", suffix)]], id.vars = "SPIN_tresh")
  } else {
    stop("Invalid variable type. Please specify either 'SPIQ' or 'SPIN'.")
  }
  
  
  # Assign melted data frames
  melt_pstep2 <- melt_est
  melt_pstep2$color <- melt_p$value
  
  # Determine colors based on p-values
  melt_pstep2$final_fill_color <- ifelse(melt_pstep2$color < 0.05, 
                                         "white", 
                                         non_significant_color)
  
  # Set alpha value: Significant values are fully opaque, others have alpha 0.7
  melt_pstep2$alpha_value <- ifelse(melt_pstep2$final_fill_color == "white", 1, 0.7)
  
  # Define color labels based on significance and threshold levels
  melt_pstep2$color_label <- ifelse(melt_pstep2$final_fill_color == "white", 
                                    "Significant at 0.05",
                                    paste("Non-Significant", sep = ""))
  
  # Convert the final fill color to a factor for plotting
  melt_pstep2$final_fill_color <- factor(melt_pstep2$final_fill_color,
                                         levels = c("white", non_significant_color))
  
  # Plotting with manual color scale
  p <- ggplot(melt_pstep2, 
              aes(x = variable, 
                  y = value)) +
    geom_boxplot(color = "black") +  # Black boxplot
    geom_point(aes(color = color_label,
                   shape = SPIQ_tresh,
                   fill = SPIQ_tresh), 
               size = 3, 
               position = position_jitter(width = 0.2), # Points with color, shape, and fill
               stroke = 1,  # Width of the contour
               ) +  
    labs(x = "Terms", 
         y = "Estimates") + 
    facet_wrap(~SPIQ_tresh) +
    theme_bw() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "bottom") +
    scale_fill_identity() + 
    scale_color_manual(values = c("Significant at 0.05" = "blue", 
                                  "Non-Significant" = "red")) +
    scale_shape_manual(values = c(21, 22, 23, 24, 25)) 
    
  return(list(p,melt_pstep2))
}



reg_mat_function <- function(matrix_res){
  
  lambda <- 1/kappa(matrix_res)
  svd_result <- svd(matrix_res)
  regularized_singular_values <- svd_result$d / (svd_result$d^2 + lambda^2)
  regularized_matrix <- svd_result$u %*% diag(regularized_singular_values) %*% t(svd_result$v)
  
  return(regularized_matrix)
}


process_est_res_age_hl2 <- function(est_res_age_hl2_element, p_res_age_hl2_element) {
  # Melt the 'est_res_age_hl2_element' and 'p_res_age_hl2_element'
  melted_est_res_age_hl <- melt(est_res_age_hl2_element, id.vars = c("HLtresh", "SPIQ_tresh",
                                                                     "SPIN_tresh", "frequency",
                                                                     "HL_Loss"))
  melted_p_res_age_hl <- melt(p_res_age_hl2_element, id.vars = c("HLtresh", "SPIQ_tresh", 
                                                                 "SPIN_tresh", "frequency",
                                                                 "HL_Loss"))
  
  # Combine melted data and assign to 'melt_res_hl'
  melted_res_hl <- melted_est_res_age_hl
  melted_res_hl$color <- melted_p_res_age_hl$value
  
  # Create 'color2' based on 'color'
  melted_res_hl$color2 <- ifelse(melted_res_hl$color < 0.01, "azure3",
                                 ifelse(melted_res_hl$color <= 0.05, "cyan", "blue"))
  melted_res_hl$color2 <- factor(melted_res_hl$color2, levels = c("azure3", "cyan", "blue"))
  
  # Create 'facet_label'
  melted_res_hl$facet_label <- paste("HLtresh:", melted_res_hl$HLtresh, 
                                     "SPIQ_tresh:", melted_res_hl$SPIQ_tresh,
                                     "SPIN_tresh:", melted_res_hl$SPIN_tresh)
  
  return(melted_res_hl)
}


plot_melt_res_hl <- function(melt_res_hl_element, Ex_regularized, Ey_regularized) {
  facet_labels <- unique(melt_res_hl_element$facet_label)
  
  ggplot(melt_res_hl_element, aes(x = as.factor(variable), 
                                  y = as.factor(as.numeric(frequency)), 
                                  fill = color2,
                                  label = if (Ex_regularized && Ey_regularized) {
                                    round(value, 1)
                                  } else {
                                    round(value*10^14, 1)
                                  })) +
    geom_tile()  + facet_wrap(~HL_Loss , ncol = 3, scales = "free") +
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
         y = "Coeff. Estimates per Frequency") +
    ggtitle(paste("", paste(facet_labels, collapse = ", ")))
}



plot_from_list_boxplot <- function(res_list, variable_type, 
                                   title, thresh_col) {
  
  remove_hl_classes <- function(df) {
    df <- df[, !(names(df) %in% 'hl_classes'), drop = FALSE]
    return(df)
  }
  
  # Remove 'hl_classes' column from each list element using lapply
  res_list <- lapply(res_list, remove_hl_classes)
  
  # Melt the necessary data frames based on the variable type
  if (variable_type == "SPIQ") {
    melt_est <- melt(res_list$est_pstep2_spiq_hl, id.vars = "SPIQ_tresh")
    melt_p <- melt(res_list$p_pstep2_spiq_hl, id.vars = "SPIQ_tresh")
    melt_est$SPIQ_tresh2 <- factor(melt_est$SPIQ_tresh)
  } else if (variable_type == "SPIN") {
    melt_est <- melt(res_list$est_pstep2_spin_hl, id.vars = "SPIN_tresh")
    melt_p <- melt(res_list$p_pstep2_spin_hl, id.vars = "SPIN_tresh")
    melt_est$SPIN_tresh2 <- factor(melt_est$SPIN_tresh)
  } else {
    stop("Invalid variable type. Please specify either 'SPIQ' or 'SPIN'.")
  }
  
  # Assign melted data frames
  melt_pstep2 <- melt_est
  melt_pstep2$color <- melt_p$value
  
  # Determine colors based on p-values
  melt_pstep2$color2 <- ifelse(melt_pstep2$color < 0.01, "azure3",
                               ifelse(melt_pstep2$color <= 0.05, "cyan", "blue"))
  melt_pstep2$color2 <- factor(melt_pstep2$color2, levels = c("azure3", "cyan", "blue"))
  
  
  # Plotting with manual color scale
  p <- ggplot(melt_pstep2, aes(x = as.factor(variable), 
                               y = color)) +
    geom_boxplot(alpha = 0.5,color = "black") + 
    geom_point(size = 2,
               aes(color = as.factor(get(paste0(variable_type, "_tresh")))))  +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
    scale_color_manual(values = thresh_colors ) +
    labs(x = "Frequency", 
         y = "P-value", 
         color = paste("Probabilistic Level")) +
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
    ) +
    ggtitle(title)
  
  return(p)
}



plot_from_list_boxplot_2 <- function(res_list, variable_type, title) {
  
  remove_hl_classes <- function(df) {
    df <- df[, !(names(df) %in% 'hl_classes'), drop = FALSE]
    return(df)
  }
  
  # Remove 'hl_classes' column from each list element using lapply
  res_list <- lapply(res_list, remove_hl_classes)
  # Melt the necessary data frames based on the variable type
  if (variable_type == "SPIQ") {
    melt_est <- melt(res_list$est_pstep2_spiq_hl, id.vars = "SPIQ_tresh")
    melt_p <- melt(res_list$p_pstep2_spiq_hl, id.vars = "SPIQ_tresh")
    melt_est$SPIQ_tresh2 <- factor(melt_est$SPIQ_tresh)
  } else if (variable_type == "SPIN") {
    melt_est <- melt(res_list$est_pstep2_spin_hl, id.vars = "SPIN_tresh")
    melt_p <- melt(res_list$p_pstep2_spin_hl, id.vars = "SPIN_tresh")
    melt_est$SPIN_tresh2 <- factor(melt_est$SPIN_tresh)
  } else {
    stop("Invalid variable type. Please specify either 'SPIQ' or 'SPIN'.")
  }
  
  # Assign melted data frames
  melt_pstep2 <- melt_est
  melt_pstep2$color <- melt_p$value
  
  # Determine colors based on p-values
  melt_pstep2$color2 <- ifelse(melt_pstep2$color < 0.01, "azure3",
                               ifelse(melt_pstep2$color <= 0.05, "cyan", "blue"))
  melt_pstep2$color2 <- factor(melt_pstep2$color2, levels = c("azure3", "cyan", "blue"))
  
  
  # Plotting with manual color scale
  p <- ggplot(melt_pstep2, aes(x = as.factor(get(paste0(variable_type, "_tresh2"))), 
                               y = color)) +
    geom_boxplot(alpha = 0.5,color = "black") + 
    geom_point(size = 2,
               aes(color =  as.factor(variable) ))  +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "red",size = 1) +
    labs(x = paste("Probabilistic Level"), 
         y = "P-value", 
         color = "Frequency") +
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
    ) +
    ggtitle(title)
  
  return(p)
}




baseline_kalman_filter_log_likelihood <- function(Y,
                                                  alpha_a,
                                                  beta_a, 
                                                  theta_f, 
                                                  phi_1, 
                                                  sigma2_epsilon,
                                                  sigma2_omega_f,
                                                  my_kappa,#kappa_0, 
                                                  P_0) {
  P_var <- nrow(Y)
  F_var <- ncol(Y)
  
  # Initialize state and covariance estimates
  #kappa_estimates <- matrix(0, nrow = F_var, ncol = 1)
  P_estimates <- array(0, dim = c(F_var, 1, 1))
  kappa_f <- my_kappa#kappa_0
  P_f <- P_0
  
  # Initialize log likelihood
  log_likelihood <- 0
  
  # Initialize log likelihood vector
  log_like_vector <- numeric(F_var)
  
  for (f in 1:F_var) {
    # Prediction step
    kappa_f_pred <- kappa_f[f]#theta_f + phi_1 * kappa_f
    P_f_pred <- phi_1^2 * P_f + sigma2_omega_f
    
    # Innovation (residual)
    y_f <- matrix(Y[, f], nrow = P_var, ncol = 1)
    v_f <- y_f - alpha_a - beta_a * kappa_f_pred
    
    # Innovation covariance
    S_f <- (beta_a %*% t(beta_a) * as.numeric(P_f_pred))  + (sigma2_epsilon[f] * diag(P_var)) #instead of sigma2_epsilon only without [f]
    
    # Kalman gain
    K_f <- P_f_pred %*% t(beta_a) %*% solve(S_f)
    
    # Update step
    #kappa_f <- kappa_f_pred + K_f %*% v_f
    P_f <- P_f_pred - K_f %*% S_f %*% t(K_f)
    
    # Store estimates
    #kappa_estimates[f, ] <- kappa_f
    P_estimates[f, , ] <- P_f
    
    # Update log likelihood
    log_like <- -0.5 * (P_var * log(2 * pi) + log(det(S_f)) + t(v_f) %*% solve(S_f) %*% v_f)
    log_likelihood <- log_likelihood + log_like
    
    # Store log likelihood for this time point
    log_like_vector[f] <- log_like
  }
  
  return(list(P_estimates = P_estimates, 
              log_likelihood = as.numeric(log_likelihood),
              log_like_vector = as.numeric(log_like_vector)  ))
}









extended_kalman_filter_log_likelihood <- function(Y, 
                                                  alpha_a,
                                                  beta_a, 
                                                  theta_f,
                                                  phi_1,
                                                  sigma2_epsilon, 
                                                  sigma2_omega_f, 
                                                  gamma_v = NULL, v_f_coeff = NULL,
                                                  gamma_w = NULL, w_f_coeff = NULL,
                                                  my_kappa,
                                                  P_0) {
  P_var <- nrow(Y)
  F_var <- ncol(Y)
  
  P_estimates <- array(0, dim = c(F_var, 1, 1))
  kappa_f <- my_kappa
  P_f <- P_0
  
  log_likelihood <- 0
  
  # Initialize log likelihood vector
  log_like_vector <- numeric(F_var)
  
  for (f in 1:F_var) {
    kappa_f_pred <- kappa_f[f]
    P_f_pred <- phi_1^2 * P_f + sigma2_omega_f
    
    y_f <- matrix(Y[, f], nrow = P_var, ncol = 1)
    
    if (is.null(gamma_v) && is.null(v_f_coeff) && is.null(gamma_w) && is.null(w_f_coeff)) {
      v_f <- y_f - alpha_a - beta_a * kappa_f_pred
    } else if (is.null(gamma_v) && is.null(v_f_coeff)) {
      v_f <- y_f - alpha_a - beta_a * kappa_f_pred - gamma_w[f] * w_f_coeff
    } else if (is.null(gamma_w) && is.null(w_f_coeff)) {
      v_f <- y_f - alpha_a - beta_a * kappa_f_pred - gamma_v[f] * v_f_coeff
    } else {
      v_f <- y_f - alpha_a - beta_a * kappa_f_pred - gamma_v[f] * v_f_coeff - gamma_w[f] * w_f_coeff
    }
    
    S_f <- (beta_a %*% t(beta_a) * as.numeric(P_f_pred))  + (sigma2_epsilon[f] * diag(P_var))
    
    K_f <- P_f_pred %*% t(beta_a) %*% solve(S_f)
    
    P_f <- P_f_pred - K_f %*% S_f %*% t(K_f)
    
    P_estimates[f, , ] <- P_f
    
    log_like <- -0.5 * (P_var * log(2 * pi) + log(det(S_f)) + t(v_f) %*% solve(S_f) %*% v_f)
    log_likelihood <- log_likelihood + log_like
    
    # Store log likelihood for this time point
    log_like_vector[f] <- log_like
  }
  
  return(list(P_estimates = P_estimates,
              log_likelihood = as.numeric(log_likelihood),
              log_like_vector = as.numeric(log_like_vector) ))
}




vuong_test_with_loglike <- function(loglike1, loglike2, size = 0.05, 
                                    ndraws = 10000, seed = 1) {
  set.seed(seed)
  
  # Compute the length of log-likelihood vectors
  N <- length(loglike1)
  
  # Calculate log-likelihood ratios and mean difference
  LR <- sum(loglike1 - loglike2) / N
  w2 <- sum((loglike1 - loglike2 - LR)^2) / N
  
  # Standard Vuong test statistic
  Tvuong <- sqrt(N) * LR / sqrt(w2)
  
  # p-value for the standard Vuong test
  pval_vuong <- 2 * (1 - pnorm(abs(Tvuong)))
  
  # Generate standard normal draws for the non-degenerate test
  Z <- matrix(rnorm(ndraws * 2), ndraws, 2)
  
  # Calculate the critical value for the non-degenerate test
  c.value <- 0
  quantile_value <- function(size) as.numeric(quantile(abs(Tvuong + Z[, 1] / sqrt(N)), 1 - size))
  cv.value <- quantile_value(size)
  
  # Calculate the non-degenerate test statistic
  LRmod <- LR + mean(loglike1 - loglike2) / (2 * N)
  Tnd <- sqrt(N) * LRmod / sqrt(w2)
  
  # Calculate p-value for the non-degenerate Vuong test
  pvalue <- 2 * (1 - ecdf(abs(Tvuong + Z[, 1] / sqrt(N)))(abs(Tnd)))
  
  list(
    vuong_statistic = Tvuong,
    vuong_pvalue = pval_vuong,
    non_degenerate_statistic = Tnd,
    non_degenerate_pvalue = pvalue,
    critical_value = cv.value
  )
}




prepare_model_data_hl <- function(model, 
                                  model_index, 
                                  frequencies,
                                  freq_t,
                                  hr_rates) {
  ax <- model$ax
  kt <- model$kt
  bx <- model$bx
  
  ages <- as.numeric(names(ax))
  
  hearing_loss_rates <- list()
  
  for (age_pos in seq_along(ages)) {
    
    age <- ages[age_pos]
    alpha <- ax[as.character(age)]
    beta <- bx[as.character(age)]
    
    proba = hr_rates[age_pos, ]
    
    rates <- exp(alpha + beta * kt) * proba
    
    hearing_loss_rates[[as.character(age)]] <- data.frame(
      Frequency = frequencies, 
      Rate = as.numeric(rates),
      Age = age,
      Model = freq_t[model_index]
    )
  }
  
  return(do.call(rbind, hearing_loss_rates))
}



prepare_model_data_age_hl <- function(model, model_index,
                                      hl_index, 
                                      frequencies,
                                      freq_t,
                                      hr_rates,
                                      hl_class) {
  ax <- model$ax
  kt <- model$kt
  bx <- model$bx
  
  ages <- as.numeric(names(ax))
  
  hearing_loss_rates <- list()
  
  for (age_pos in seq_along(ages)) {
    
    age <- ages[age_pos]
    alpha <- ax[as.character(age)]
    beta <- bx[as.character(age)]
    
    proba = hr_rates[age_pos, ] 
    
    rates <- exp(alpha + beta * kt)  * proba
    
    hearing_loss_rates[[as.character(age)]] <- data.frame(
      Frequency = frequencies, 
      Rate = as.numeric(rates),
      Age = age,
      Model = freq_t[model_index],
      hl_index = hl_class[[hl_index]]
    )
  }
  
  return(do.call(rbind, hearing_loss_rates))
}



#THESE 2 are old version where I incorporated only the residuals of the
#SPIN and SPIQ partial regression 1 and 2 here I also forgot the proba
# so I can delete them

# prepare_model_data_hl_spin_spiq <- function(model, model_index, frequencies,
#                                             freq_t, model_res) {
#   ax <- model$ax
#   kt <- model$kt
#   bx <- model$bx
#   
#   ages <- as.numeric(names(ax))
#   
#   hearing_loss_rates <- list()
#   
#   for (age_pos in seq_along(ages)) {
#     age <- ages[age_pos]
#     alpha <- ax[as.character(age)]
#     beta <- bx[as.character(age)]
#     
#     # Extract residuals for the current age group (assuming they correspond to age `age`)
#     residuals_age <- model_res[[model_index]]$residuals[age_pos, ]
#     
#     # Compute rates using the model parameters and residuals
#     rates <- exp(alpha + beta * kt + residuals_age)
#     
#     # Create data frame for each age group
#     df_age <- data.frame(
#       Frequency = frequencies,
#       Rate = as.numeric(rates),
#       Age = age,
#       Model = freq_t[model_index]
#     )
#     
#     # Store in the list
#     hearing_loss_rates[[as.character(age)]] <- df_age
#   }
#   
#   # Combine all data frames into a single data frame
#   return(do.call(rbind, hearing_loss_rates))
# }
# 
# 
# prepare_model_data_age_hl_spin_spiq  <- function(model, model_index, hl_index, 
#                                                  frequencies, freq_t, hl_class,
#                                                  model_res) {
#   ax <- model$ax
#   kt <- model$kt
#   bx <- model$bx
#   
#   ages <- as.numeric(names(ax))
#   
#   hearing_loss_rates <- list()
#   
#   for (age_pos in seq_along(ages)) {
#     
#     age <- ages[age_pos]
#     alpha <- ax[as.character(age)]
#     beta <- bx[as.character(age)]
#     
#     residuals_age <- model_res[[model_index]][[hl_index]]$residuals[age_pos, ]
#     
#     rates <- exp(alpha + beta * kt + residuals_age)
#     
#     hearing_loss_rates[[as.character(age)]] <- data.frame(
#       Frequency = frequencies, 
#       Rate = as.numeric(rates),
#       Age = age,
#       Model = freq_t[model_index],
#       hl_index = hl_class[[hl_index]]
#     )
#   }
#   
#   return(do.call(rbind, hearing_loss_rates))
# }
# 
# 

#CHECK THE FOLLOWING 2 with Gareth:

#1)is it okay if I used the same alpha and beta and kappa plus the spiq,spin and res?
#2) is it okay if I use the sigma for the rates so they are [0,1]?
prepare_model_data_hl_spin_spiq <- function(model, model_index, 
                                            frequencies, 
                                            freq_t,
                                            hr_rates,
                                            model_res, 
                                            srt_rates, snr_rates) {
  ax <- model$ax
  kt <- model$kt
  bx <- model$bx
  
  ages <- as.numeric(names(ax))
  
  hearing_loss_rates <- list()
  
  for (age_pos in seq_along(ages)) {
    
    age <- ages[age_pos]
    alpha <- ax[as.character(age)]
    beta <- bx[as.character(age)]
    
    proba = hr_rates[age_pos, ]
    
    # Extract residuals for the current age group (assuming they correspond to age `age`)
    residuals_age <- model_res$residuals[age_pos, ]
    
    # Extract SRT and SNR rates for the current age group
    srt_rate <- srt_rates[age_pos, 1]
    snr_rate <- snr_rates[age_pos, 1]
    
    # Extract coefficients for SRT and SNR for the current frequency
    srt_coeff <- coef(model_res)[1, ]
    snr_coeff <- coef(model_res)[2, ]
    
    # Compute rates using the model parameters, SRT, SNR, and residuals
    rates <- exp(alpha + beta * kt + srt_coeff * srt_rate + snr_coeff * snr_rate + residuals_age) * proba
    rates <-  1 / (1 + exp(-rates))
    
    # Create data frame for each age group
    df_age <- data.frame(
      Frequency = frequencies,
      Rate = as.numeric(rates),
      Age = age,
      Model = freq_t[model_index]
    )
    
    # Store in the list
    hearing_loss_rates[[as.character(age)]] <- df_age
  }
  
  # Combine all data frames into a single data frame
  return(do.call(rbind, hearing_loss_rates))
}



prepare_model_data_age_hl_spin_spiq <- function(model, 
                                                model_index,
                                                hl_index, 
                                                frequencies,
                                                freq_t,
                                                hl_class,
                                                hr_rates,
                                                model_res, 
                                                srt_rates,
                                                snr_rates) {
  ax <- model$ax
  kt <- model$kt
  bx <- model$bx
  
  ages <- as.numeric(names(ax))
  
  hearing_loss_rates <- list()
  
  for (age_pos in seq_along(ages)) {
    
    age <- ages[age_pos]
    alpha <- ax[as.character(age)]
    beta <- bx[as.character(age)]
    
    proba = hr_rates[age_pos, ]
    
    # Extract residuals for the current age group (assuming they correspond to age `age`)
    residuals_age <- model_res$residuals[age_pos, ]
    
    # Extract SRT and SNR rates for the current age group
    srt_rate <- srt_rates[age_pos, 1]
    snr_rate <- snr_rates[age_pos, 1]
    
    # Extract coefficients for SRT and SNR for the current frequency
    srt_coeff <- coef(model_res)[1, ]
    snr_coeff <- coef(model_res)[2, ]
    
    # Compute rates using the model parameters, SRT, SNR, and residuals
    rates <- exp(alpha + beta * kt + srt_coeff * srt_rate + snr_coeff * snr_rate + residuals_age) * proba
    rates <-  1 / (1 + exp(-rates))
    
    
    hearing_loss_rates[[as.character(age)]] <- data.frame(
      Frequency = frequencies,
      Rate = as.numeric(rates),
      Age = age,
      Model = freq_t[model_index],
      hl_index = hl_class[[hl_index]]
    )
  }
  
  return(do.call(rbind, hearing_loss_rates))
}









