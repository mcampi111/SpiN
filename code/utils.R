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

#########
# Utils #
#########

process_matrix <- function(mat, thresholds) {
  if (ncol(mat) == 16) {
    processed_list <- lapply(thresholds, function(threshold) {
      counts <- colSums(mat[, -c(1, 2, 14, 15, 16)] > threshold)
      ratios <- counts / nrow(mat)
      return(ratios)
    })
  } else if (ncol(mat) == 6) {
    processed_list <- lapply(thresholds, function(threshold) {
      counts <- colSums(mat[, -c(1, 2, 4, 5, 6)] > threshold)
      ratios <- counts / nrow(mat)
      return(ratios)
    })
  } else {
    stop("Unexpected number of columns in 'mat'")
  }
  
  return(processed_list)
}


process_matrix2 <- function(mat, freq_tresh) {
  if (ncol(mat) == 16) {
    processed_list <- lapply(seq_len(ncol(freq_tresh)), function(thresh_col) {
      threshold <- freq_tresh[, thresh_col]
      counts <- colSums(mat[, -c(1, 2, 14, 15, 16)] > threshold)
      ratios <- counts / nrow(mat)
      return(ratios)
    })
  } else if (ncol(mat) == 6) {
    processed_list <- lapply(seq_len(ncol(freq_tresh)), function(thresh_col) {
      threshold <- freq_tresh[, thresh_col]
      counts <- colSums(mat[, -c(1, 2, 4, 5, 6)] > threshold)
      ratios <- counts / nrow(mat)
      return(ratios)
    })
  } else {
    stop("Unexpected number of columns in 'freq_tresh'")
  }
  
  return(processed_list)
}




# process_matrix <- function(mat, thresholds) {
#   processed_list <- lapply(thresholds, function(threshold) {
#     counts <- colSums(mat[, -c(1, 2, 14, 15, 16)] > threshold)
#     ratios <- counts / nrow(mat)
#     return(ratios)
#   })
#   return(processed_list)
# }
# 
# process_matrix2 <- function(mat, thresholds) {
#   processed_list <- lapply(thresholds, function(threshold) {
#     counts <- colSums(mat[, -c(1, 2, 4, 5, 6)] > threshold)
#     ratios <- counts / nrow(mat)
#     return(ratios)
#   })
#   return(processed_list)
# }


# process_data <- function(data, thresholds, age_group_var,
#                          len_tresh, len_age, len_hl) {
#   
#   data_by_age <- data %>%
#     group_by(!!sym(age_group_var)) %>% group_split()
#   
#   data_by_age_hl <- data %>%
#     group_by(!!sym(age_group_var)) %>% 
#     group_split() %>% 
#     lapply(function(subgroup) {
#       subgroup %>%
#         group_split(`Classification`)
#     })
#   
#   hrates_age <- lapply(data_by_age, process_matrix, thresholds)
#   hrates_age_mat <- lapply(1:len_tresh, function(i) 
#     do.call(rbind, lapply(hrates_age, "[[", i)))
#   
#   hrates_age_hl <- lapply(1:len_age, function(i) {
#     lapply(data_by_age_hl[[i]], process_matrix, thresholds)
#   })
#   
#   hrates_age_hl_mat <-lapply(1:len_tresh, function(h)
#     lapply(1:len_hl, function(j)
#       do.call(rbind,
#               lapply(1:len_age, function(i) 
#                 hrates_age_hl[[i]][[j]][[h]]))))
#   
#   return(list(hrates_age_mat = hrates_age_mat, 
#               hrates_age_hl_mat = hrates_age_hl_mat))
# }



process_data <- function(data, thresholds, age_group_var,
                         len_tresh, len_age, len_hl, process_func = process_matrix) {
  
  data_by_age <- data %>%
    group_by(!!sym(age_group_var)) %>% group_split()
  
  data_by_age_hl <- data %>%
    group_by(!!sym(age_group_var)) %>% 
    group_split() %>% 
    lapply(function(subgroup) {
      subgroup %>%
        group_split(`Classification`)
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
  
  return(list(hrates_age_mat = hrates_age_mat, 
              hrates_age_hl_mat = hrates_age_hl_mat))
}



# interpolate_row_linear <- function(row) {
#   zero_indices <- which(row == 0)
#   non_zero_indices <- which(row != 0)
#   
#   if (length(zero_indices) > 0) {
#     
#     row[zero_indices] = approx(non_zero_indices, row[non_zero_indices],
#                                xout = zero_indices,
#                                method = 'linear', rule = 2)$y
#     
#   }
#   
#   return(row)
#   
# }

# interpolate_row_linear <- function(row) {
#   zero_indices <- which(row == 0)
#   non_zero_indices <- which(row != 0)
#   
#   if (length(zero_indices) > 0) {
#     if (length(non_zero_indices) > 1) {
#       row[zero_indices] <- approx(non_zero_indices, row[non_zero_indices],
#                                   xout = zero_indices,
#                                   method = 'linear', rule = 2)$y
#     } else {
#       #TOBEDISCUSSED
#       row[zero_indices] <- row[zero_indices + 1]
#     }
#   }
#   
#   return(row)
# }

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
                          age_group_var, thresholds) {
  
  gg1 <- list()
  gg2 <- list()
  
  for (i in 1:len_tresh) {
    melted_rates0 <- melt(df_rates[[1]][[i]])
    melted_rates0$age_group <- age_group_var[melted_rates0$Var1]
    
    if (is.matrix(thresholds)) {
      threshold_value <- paste("Threshold:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Threshold:", thresholds[[i]])
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
      threshold_value <- paste("Threshold:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Threshold:", thresholds[[i]])
    }
    
    gg2[[i]] <- ggplot(melted_rates, aes(x = reorder(Var2,
                                                     as.numeric(Var2)),
                                         y = value,
                                         fill = as.factor(hl_classes))) +
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
  
  return(list(gg1 = gg1, gg2 = gg2))
  
}


plot_hr_rates2 <- function(df_rates, hl_classes, len_tresh, 
                           age_group_var, thresholds) {
  
  gg1 <- list()
  gg2 <- list()
  
  for (i in 1:len_tresh) {
    melted_rates0 <- melt(df_rates[[1]][[i]])
    melted_rates0$age_group <- age_group_var[melted_rates0$Var1]
    melted_rates0$frequency <- as.numeric(melted_rates0$Var2)
    melted_rates0$freq_mapped = factor(freq_vector[as.numeric(melted_rates0$Var2)], 
                                       levels = unique(freq_vector))
    
    
    
    if (is.matrix(thresholds)) {
      threshold_value <- paste("Threshold:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Threshold:", thresholds[[i]])
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
      threshold_value <- paste("Threshold:", paste(thresholds[, i], collapse = ", "))
    } else {
      threshold_value <- paste("Threshold:", thresholds[[i]])
    }
    
    gg2[[i]] <- ggplot(melted_rates, aes(x =  freq_mapped,
                                         y = value,
                                         color = as.factor(hl_classes))) +
      geom_line(aes(group = hl_classes),size = 1) + facet_wrap(~age_group) +
      labs(title = threshold_value,
           x = "Frequency",
           y = "Hearing Rates",
           color = "HL Classes") +
      scale_color_discrete(name = "") +
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
  
  return(list(gg1 = gg1, gg2 = gg2))
  
}

plot_speech_rates <- function(df_rates,hl_classes,len_tresh, 
                              age_group_var,tresholds, speech_type) {
  
  gg1 = list()
  gg2 = list()
  
  for (i in 1:len_tresh) {
    melted_rates0 = melt(df_rates[[1]][[i]])
    melted_rates0$age_group = age_group_var[melted_rates0$Var1]
    
    gg1[[i]] =  ggplot(melted_rates0, aes(x = age_group,
                                          y = value)) +
      geom_bar(stat = "identity") + 
      labs(title = paste("Threshold:", tresholds[[i]]),
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
    
    gg2[[i]] = ggplot(melted_rates, aes(x = age_group,
                                        y = value,
                                        fill = as.factor(hl_classes))) +
      geom_bar(stat = "identity") + 
      labs(title = paste("Threshold:", tresholds[[i]]),
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
  
  return(list(gg1 = gg1, gg2 = gg2))
  
}



plot_speech_rates2 <- function(df_rates,hl_classes,len_tresh, 
                              age_group_var,tresholds, speech_type) {
  
  gg1 = list()
  gg2 = list()
  
  for (i in 1:len_tresh) {
    melted_rates0 = melt(df_rates[[1]][[i]])
    melted_rates0$age_group = factor(age_group_var[melted_rates0$Var1], levels = unique(age_group_var))
    
    gg1[[i]] =  ggplot(melted_rates0, aes(x = age_group,
                                          y = value)) +
      geom_line(aes(group = 1),size = 1) + 
      labs(title = paste("Threshold:", tresholds[[i]]),
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
    
    gg2[[i]] = ggplot(melted_rates, aes(x = age_group,
                                        y = value,
                                        color = as.factor(hl_classes))) +
      geom_line(aes(group = hl_classes),size = 1) + 
      labs(title = paste("Threshold:", tresholds[[i]]),
           x = "Age Group",
           y = ifelse(speech_type == "SpiQ", "SpiQ Rates", "SpiN Rates"),
           color = "HL Classes") +
      scale_fill_discrete(name = "")  +
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
  
  return(list(gg1 = gg1, gg2 = gg2))
  
}




# plot_heat_rates <- function(df_rates,spq_rqtes, spn_rates,
#                             hl_classes, freq_vector, len_tresh, age_group_var,
#                             threshold_hl, threshold_spq, threshold_spn) {
#   
#   gg1 = list()
#   gg2 = list()
#   
#   melt_rates = list()
#   
#   for (i in 1:len_tresh) { 
#     
#     melt_rates[[i]] =  as.data.frame(cbind(df_rates[[1]][[i]],
#                                            spq_rqtes[[1]][[i]],
#                                            spn_rates[[1]][[i]])) 
#     colnames(melt_rates[[i]]) = c(paste(freq_vector), 
#                                   "SRT", "SNR")
#     melt_rates[[i]]$age = sapply(strsplit(age_group_var, "-"),
#                                  function(x) as.numeric(x[1])) }
#   
#   melted_rates = melt(melt_rates, id.var = "age")
#   melted_rates$HL = threshold_hl[melted_rates$L1]
#   melted_rates$SRT = threshold_spq[melted_rates$L1]
#   melted_rates$SNR = threshold_spn[melted_rates$L1]
#   
#   melted_rates$facet_label <- paste("HL:", melted_rates$HL,
#                                     " SRT:", melted_rates$SRT, 
#                                     " SNR:", melted_rates$SNR)
#   
#   
#   gg1 = ggplot(melted_rates, aes(x = variable, y = age, fill = value)) +
#     geom_tile() + 
#     scale_fill_viridis_c(direction = -1, option = "plasma") +  
#     theme_minimal() + 
#     facet_wrap(~facet_label) +
#     theme(
#       plot.background = element_rect(fill = "white"),  
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.text.x = element_text(size = 10, angle = 90),
#       axis.text.y = element_text(size = 10),
#       axis.title.x = element_text(size = 12),
#       axis.title.y = element_text(size = 12),
#       legend.position = "bottom"
#     ) +
#     labs(fill = "Rate",  
#          x = "Frequency",  
#          y = "Age"
#     )
#   
#   
#   
#   melt_rates2 = list()
#   
#   for (i in 1:len_tresh) {
#     
#     melt_rates2[[i]] = list()
#     
#     for (j in 1:length(hl_classes)) {
#       
#       melt_rates2[[i]][[j]] = as.data.frame(cbind(df_rates[[2]][[i]][[j]],
#                                                   spq_rqtes[[2]][[i]][[j]],
#                                                   spn_rates[[2]][[i]][[j]]))
#       
#       colnames(melt_rates2[[i]][[j]]) = c(paste(freq_vector), 
#                                           "SRT", "SNR")
#       
#       melt_rates2[[i]][[j]]$age = sapply(strsplit(age_group_var, "-"),
#                                          function(x) as.numeric(x[1]))
#       
#     } }
#   
#   
#   for (i in 1:len_tresh) {
#     melted_rates2 = melt(melt_rates2[[i]], id.var = 'age')
#     melted_rates2$hl_classes = factor(hl_classes[melted_rates2$L1], 
#                                       levels = hl_classes)
#     
#     gg2[[i]] = ggplot(melted_rates2, aes(x = variable, y = age, fill = value)) +
#       geom_tile() + 
#       scale_fill_viridis_c(direction = -1, option = "plasma") +  
#       theme_minimal() + 
#       facet_wrap(~hl_classes) +
#       theme(
#         plot.background = element_rect(fill = "white"),  
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text.x = element_text(size = 10, angle = 90),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position = "bottom"
#       ) +
#       labs(title = paste("Thresholds HL:", threshold_hl[i], 
#                          " SRT:", threshold_spq[i], 
#                          " SNR:", threshold_spn[i]),
#            fill = "Rate",  
#            x = "Frequency",  
#            y = "Age"
#       )
#   }
#   
#   return(list(gg1 = gg1, gg2 = gg2))
#   
# }


plot_heat_rates <- function(df_rates, spq_rqtes, spn_rates,
                            hl_classes, freq_vector, len_tresh, age_group_var,
                            threshold_hl, threshold_spq, threshold_spn) {
  
  gg1 <- list()
  gg2 <- list()
  
  melt_rates <- list()
  
  for (i in 1:len_tresh) { 
    
    melt_rates[[i]] <-  as.data.frame(cbind(df_rates[[1]][[i]],
                                            spq_rqtes[[1]][[i]],
                                            spn_rates[[1]][[i]])) 
    colnames(melt_rates[[i]]) <- c(paste(freq_vector), 
                                   "SRT", "SNR")
    melt_rates[[i]]$age <- sapply(strsplit(age_group_var, "-"),
                                  function(x) as.numeric(x[1])) 
  }
  
  melted_rates <- melt(melt_rates, id.var = "age")
  
  if (is.matrix(threshold_hl)) {
    melted_rates$HL <- paste(paste(threshold_hl[, i], collapse = ", "))
  } else {
    melted_rates$HL <- paste(threshold_hl)
  }
  
  melted_rates$SRT <- threshold_spq[melted_rates$L1]
  melted_rates$SNR <- threshold_spn[melted_rates$L1]
  
  melted_rates$facet_label <- paste("HL:", melted_rates$HL,
                                    " SRT:", melted_rates$SRT, 
                                    " SNR:", melted_rates$SNR)
  
  
  gg1 <- ggplot(melted_rates, aes(x = variable, y = age, fill = value)) +
    geom_tile() + 
    scale_fill_viridis_c(direction = -1, option = "plasma") +  
    theme_bw() + 
    facet_wrap(~facet_label) +
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
    labs(fill = "Rate",  
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
      
    } 
  }
  
  
  for (i in 1:len_tresh) {
    melted_rates2 <- melt(melt_rates2[[i]], id.var = 'age')
    melted_rates2$hl_classes <- factor(hl_classes[melted_rates2$L1], 
                                       levels = hl_classes)
    
    gg2[[i]] <- ggplot(melted_rates2, aes(x = variable, y = age, fill = value)) +
      geom_tile() + 
      scale_fill_viridis_c(direction = -1, option = "plasma") +  
      theme_bw() + 
      facet_wrap(~hl_classes) +
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
      labs(title = paste("HL:", 
                         if(is.matrix(threshold_hl)) paste(threshold_hl[,i], collapse = ", ") 
                         else threshold_hl[i], 
                         " SRT:", threshold_spq[i], 
                         " SNR:", threshold_spn[i]),
           fill = "Rate",  
           x = "Frequency",  
           y = "Age"
      )
  }
  
  return(list(gg1 = gg1, gg2 = gg2))
  
}





plot_coeff_by_age <- function(model, treshold_hearing_rate,
                              age_group_var, freq_vector) {
  
  plot_coefficients <- function(coef_data, x_var, y_var, 
                                title, x_label, y_label) {
    ggplot(coef_data, aes(x = !!sym(x_var), y = scale(value), 
                          group = `treshold`, color = `treshold`)) +
      geom_line(size = 1.5) +
      labs(title = title,
           x = x_label,
           y = y_label) +
      theme_bw() +  
      theme(legend.position = "bottom",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  }
  
  # Kappas
  k_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh, function(i)
    as.numeric(model[[i]]$kt)) ))
  colnames(k_byage) <- treshold_hearing_rate
  k_byage$frequency <- as.numeric(freq_vector)
  k_byage_melted <- melt(k_byage, id.vars = "frequency"  )
  colnames(k_byage_melted) <- c ("frequency", "treshold",  "value")
  
  plot_kappas <- plot_coefficients(k_byage_melted, "frequency", "treshold", 
                                   "Kappas by Frequency", "Frequency", "Kappas")
  
  # As
  a_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh,  function(i) 
    as.numeric(model[[i]]$ax)) ))
  colnames(a_byage) <- treshold_hearing_rate
  a_byage$age <-  sapply(strsplit(age_group_var, "-"), function(x)
    as.numeric(x[1]))
  a_byage_melted <- melt(a_byage, id.vars = "age"  )
  colnames(a_byage_melted) <- c ("age", "treshold",  "value")
  
  plot_as <- plot_coefficients(a_byage_melted, "age", "treshold", 
                               "As by Age", "Age", "As")
  
  # Bs
  b_byage <- as.data.frame(do.call(cbind, lapply(1:n_tresh,  function(i) 
                      as.numeric(model[[i]]$bx)) ))
  colnames(b_byage) <- treshold_hearing_rate
  b_byage$age <-  sapply(strsplit(age_group_var, "-"), function(x) 
    as.numeric(x[1]))
  b_byage_melted <- melt(b_byage, id.vars = "age"  )
  colnames(b_byage_melted) <- c ("age", "treshold",  "value")
  
  plot_bs <- plot_coefficients(b_byage_melted, "age", "treshold", 
                               "Bs by Age", "Age", "Bs")
  
  return(list(plot_kappas, plot_as, plot_bs))
}



plot_coeff_by_age_hl <- function(model_by_age_hl, n_tresh, n_hlclasses,
                                 hl_classes,
                                 age_group_var, treshold_hearing_rate, 
                                 freq_vector) {
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
  
  gg_k <- ggplot(k_by_age_hl_melted, aes(x = frequency, y = scale(value), 
                                         group = treshold, color = treshold)) +
    geom_line(size = 1.5) + facet_wrap(~`HL degree`, scales = "free") +
    labs(title = "",
         x = "Frequency",
         y = "Kappas") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'))
  
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
         y = "As") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'))
  
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
         y = "Bs") +
    theme_bw() +  
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(fill = 'white'))
  
  return(list(gg_k, gg_a, gg_b))
}




# process_summary <- function(partial_step2_hl, n_loop, freq_vector, treshold_SPIQ, treshold_SPIN) {
#   # Step 1: Compute summary for each model in partial_step2_hl
#   sum_part_step2_hl <- lapply(1:n_loop, function(i) summary(partial_step2_hl[[i]]))
#   
#   # Step 2: Extract estimates for SPIQ and SPIN thresholds
#   extract_estimates <- function(summary_output, column_index) {
#     estimates <- lapply(summary_output, function(model_output) 
#       coef(model_output)[, "Estimate"][column_index])
#   }
#   
#   est_pstep2_spiq_hl <- lapply(1:n_loop, function(i) as.numeric(do.call(rbind,
#                                                                          extract_estimates(sum_part_step2_hl[[i]], 2))) )
#   est_pstep2_spin_hl <-  lapply(1:n_loop, function(i) as.numeric(do.call(rbind,
#                                                                           extract_estimates(sum_part_step2_hl[[i]], 3))) )
#   
#   # Step 3: Extract p-values for SPIQ and SPIN thresholds
#   extract_pvalues <- function(summary_output, column_index) {
#     p_values <- lapply(summary_output, function(model_output) {
#       round(as.numeric(coef(model_output)[, "Pr(>|t|)"][column_index]), 3)
#     })
#   }
#   
#   p_pstep2_spiq_hl <- lapply(1:n_loop, function(i) as.numeric(do.call(rbind,
#                                                                        extract_pvalues(sum_part_step2_hl[[i]], 2))) )
#   p_pstep2_spin_hl <- lapply(1:n_loop, function(i) as.numeric(do.call(rbind,
#                                                                        extract_pvalues(sum_part_step2_hl[[i]], 3))) )
#   
#   est_pstep2_spiq_hl = do.call(rbind, est_pstep2_spiq_hl)
#   est_pstep2_spin_hl = do.call(rbind, est_pstep2_spin_hl)
#   p_pstep2_spiq_hl = do.call(rbind, p_pstep2_spiq_hl)
#   p_pstep2_spin_hl = do.call(rbind, p_pstep2_spin_hl)
#   
#   
#   # Step 4: Create data frames for estimates and p-values
#   est_pstep2_spiq_hl <- as.data.frame(est_pstep2_spiq_hl)
#   est_pstep2_spin_hl <- as.data.frame(est_pstep2_spin_hl)
#   p_pstep2_spiq_hl <- as.data.frame(p_pstep2_spiq_hl)
#   p_pstep2_spin_hl <- as.data.frame(p_pstep2_spin_hl)
#   
#   # Step 5: Add column names and SPIQ/SPIN thresholds
#   colnames(est_pstep2_spiq_hl) <- colnames(est_pstep2_spin_hl) <- colnames(p_pstep2_spiq_hl) <- colnames(p_pstep2_spin_hl) <- freq_vector
#   est_pstep2_spiq_hl$SPIQ_tresh <- p_pstep2_spiq_hl$SPIQ_tresh <- treshold_SPIQ
#   est_pstep2_spin_hl$SPIN_tresh <- p_pstep2_spin_hl$SPIN_tresh  <- treshold_SPIN
#   
#   # Return the results
#   list(est_pstep2_spiq_hl = est_pstep2_spiq_hl,
#        est_pstep2_spin_hl = est_pstep2_spin_hl,
#        p_pstep2_spiq_hl = p_pstep2_spiq_hl,
#        p_pstep2_spin_hl = p_pstep2_spin_hl)
# }





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






plot_from_list <- function(res_list, variable_type, title) {
  
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
  } else if (variable_type == "SPIN") {
    melt_est <- melt(res_list$est_pstep2_spin_hl, id.vars = "SPIN_tresh")
    melt_p <- melt(res_list$p_pstep2_spin_hl, id.vars = "SPIN_tresh")
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
                               y = as.factor(get(paste0(variable_type, "_tresh"))), 
                               fill = color2,
                               label = round(value, 2))) +
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
         y = paste(variable_type, "threshold")) +
    ggtitle(title)
  
  return(p)
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



plot_from_list_boxplot <- function(res_list, variable_type, title) {
  
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
    labs(x = "Frequency", 
         y = "P-value", 
         color = paste(variable_type, "threshold")) +
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
    labs(x = paste(variable_type, "threshold"), 
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








