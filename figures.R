# Load packages ####
library(rethinking)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(posterior)
library(suncalc)
library(cmdstanr)
library(ggplot2)
library(cowplot)

# Species ####
key_sp <- c("baboon",
            "vervetmonkey",
            "elephant",
            "buffalo",
            "dikdik",
            "impala",
            "kudu",
            "giraffe",
            "zebragrevys",
            "zebraplains",
            "hyenaspotted",
            "leopard")

key_sp_alphabetical <- key_sp[order(key_sp)]

# Dataframe to store output ####
effect_probs_df <- as.data.frame(key_sp)

effect_probs_df$occ_fine_season1 <- NA
effect_probs_df$occ_fine_season2 <- NA
effect_probs_df$occ_grid_season1 <- NA
effect_probs_df$occ_grid_season2 <- NA

effect_probs_df$totdet_fine_season1 <- NA
effect_probs_df$totdet_fine_season2 <- NA
effect_probs_df$totdet_grid_season1 <- NA
effect_probs_df$totdet_grid_season2 <- NA

# Load cmdstan output files ####
# Parameters to load
pars <- c("lp__", 
          "k_bar",
          "beta_opuntia")

# Occupancy ####
# Load fine-scale no vegetation pathway
setwd("C:/temp/camera_trap_models/occupancy_post")
file_list <- list.files(pattern = ".fine_scale_total_no_veg_path")
post_list_fine1 <- list()
for(i in seq(1, length(file_list), by = 4)){
  mod <- read_cmdstan_csv(files = file_list[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  post_list_fine1[[i]] <- post
  print(i)
}
post_list_fine1 <- post_list_fine1[lengths(post_list_fine1) != 0]
names(post_list_fine1) <- key_sp_alphabetical

# Load grid square with no vegetation pathway
setwd("C:/temp/camera_trap_models/occupancy_post")
file_list <- list.files(pattern = ".grid_square_total_no_veg_path")
post_list_grid1 <- list()
for(i in seq(1, length(file_list), by = 4)){
  mod <- read_cmdstan_csv(files = file_list[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  post_list_grid1[[i]] <- post
  print(i)
}
post_list_grid1 <- post_list_grid1[lengths(post_list_grid1) != 0]
names(post_list_grid1) <- key_sp_alphabetical


# Calculate effect probabilities and save to dataframe

for(i in 1:length(key_sp)){
  p1 <- post_list_fine1[[key_sp[i]]]
  p2 <- post_list_grid1[[key_sp[i]]]
  
  effect_probs_df$occ_fine_season1[i] <- sum(p1$`beta_opuntia[1]` > 0) / length(p1$`beta_opuntia[1]`)
  effect_probs_df$occ_fine_season2[i] <- sum(p1$`beta_opuntia[2]` > 0) / length(p1$`beta_opuntia[2]`)
  
  effect_probs_df$occ_grid_season1[i] <- sum(p2$`beta_opuntia[1]` > 0) / length(p2$`beta_opuntia[1]`)
  effect_probs_df$occ_grid_season2[i] <- sum(p2$`beta_opuntia[2]` > 0) / length(p2$`beta_opuntia[2]`)
}

# Total number of detections #### 

pars <- c("lp__", 
          "alpha_bar",
          "beta_opuntia")

# Load fine scale with no veg pathway
setwd("C:/temp/camera_trap_models/total_activity/output")
file_list2 <- list.files(pattern = ".fine_scale_total_no_veg_path.")
total_activity_post_list_fine2 <- list()

for(i in seq(1, length(file_list2), by = 4)){
  mod <- read_cmdstan_csv(files = file_list2[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  total_activity_post_list_fine2[[i]] <- post
  print(i)
}
total_activity_post_list_fine2 <- total_activity_post_list_fine2[lengths(total_activity_post_list_fine2) != 0]
names(total_activity_post_list_fine2) <- key_sp_alphabetical

# Load grid square with no veg pathway
setwd("C:/temp/camera_trap_models/total_activity/output")
file_list2 <- list.files(pattern = ".grid_square_total_no_veg_path.")
total_activity_post_list_grid2 <- list()

for(i in seq(1, length(file_list2), by = 4)){
  mod <- read_cmdstan_csv(files = file_list2[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  total_activity_post_list_grid2[[i]] <- post
  print(i)
}
total_activity_post_list_grid2 <- total_activity_post_list_grid2[lengths(total_activity_post_list_grid2) != 0]
names(total_activity_post_list_grid2) <- key_sp_alphabetical

# Calculate effect probabilities and save to dataframe
for(i in 1:length(key_sp)){
  p1 <- total_activity_post_list_fine2[[key_sp[i]]]
  p2 <- total_activity_post_list_grid2[[key_sp[i]]]
  
  effect_probs_df$totdet_fine_season1[i] <- sum(p1$`beta_opuntia[1]` > 0) / length(p1$`beta_opuntia[1]`)
  effect_probs_df$totdet_fine_season2[i] <- sum(p1$`beta_opuntia[2]` > 0) / length(p1$`beta_opuntia[2]`)
  
  effect_probs_df$totdet_grid_season1[i] <- sum(p2$`beta_opuntia[1]` > 0) / length(p2$`beta_opuntia[1]`)
  effect_probs_df$totdet_grid_season2[i] <- sum(p2$`beta_opuntia[2]` > 0) / length(p2$`beta_opuntia[2]`)
}

# Make heatmap ####
effect_probs_df_long <- effect_probs_df %>% pivot_longer(!key_sp, names_to = "effect")

p_heatmap <- ggplot(effect_probs_df_long, aes(x = effect, y = key_sp, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "grey90", high = "blue", midpoint = 0.5, limits = c(0,1)) +
  scale_y_discrete(limits = rev(key_sp)) +
  ylab("") + xlab("") + 
  guides(fill=guide_legend(title="P(+ve effect)", reverse = TRUE)) +
  theme_classic() +
  theme(axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))

setwd("C:/temp/figures")
ggsave2("fig3.svg", 
        plot = p_heatmap,
        bg = "white",
        width = 110,
        height = 100,
        units = "mm")


# Make ribbon plots ####
xseq_fine <- seq(-0.7663, 4.0190, by = 0.1) # Use real min/max Opuntia cover (standardised) values
xseq_grid <- seq(-1.52959, 3.06667, by = 0.1) # Use real min/max Opuntia volume (standardised) values

mycols <- layer_data(p_heatmap) %>% filter(x < 5) %>% select(fill)
mycols <- as.vector(mycols$fill)

mycols2 <- layer_data(p_heatmap) %>% filter(x > 4) %>% select(fill)
mycols2 <- as.vector(mycols2$fill)

colouralpha <- 0.5

y_vjust <- 0.5

scale_fun_custom <- function(x){
  tmp <- sprintf("%.1f", x)

  for(q in 1:length(tmp)){
    if(nchar(tmp[q]) < 4){
      tmp[q] <- paste0(" ", tmp[q])
    }
  }
  return(tmp)
}

# Occupancy ####
p_list <- list() # List to store plots
for(i in 1:length(key_sp)){
  print(i)
  # Subset to current species
  p1 <- post_list_fine1[[key_sp[i]]]
  p2 <- post_list_grid1[[key_sp[i]]]
  
  
  # Calculate marginal effects
  p1_season1 <- matrix(NA, nrow=nrow(p1), ncol=length(xseq_fine))
  p1_season2 <- matrix(NA, nrow=nrow(p1), ncol=length(xseq_fine))
  p2_season1 <- matrix(NA, nrow=nrow(p2), ncol=length(xseq_grid))
  p2_season2 <- matrix(NA, nrow=nrow(p2), ncol=length(xseq_grid))
  
  for(x in 1:length(xseq_fine)){
    p1_season1[,x] <- inv_logit(p1$`k_bar[1]` + p1$`beta_opuntia[1]`*xseq_fine[x])
    p1_season2[,x] <- inv_logit(p1$`k_bar[2]` + p1$`beta_opuntia[2]`*xseq_fine[x])
  }
  for(x in 1:length(xseq_grid)){
    p2_season1[,x] <- inv_logit(p2$`k_bar[1]` + p2$`beta_opuntia[1]`*xseq_grid[x])
    p2_season2[,x] <- inv_logit(p2$`k_bar[2]` + p2$`beta_opuntia[2]`*xseq_grid[x])
  }
  
  p1_mu1 <- apply(p1_season1, 2, median)
  p1_PI95_1 <- apply(p1_season1, 2, HPDI, prob=0.95)
  p1_PI89_1 <- apply(p1_season1, 2, HPDI, prob=0.89)
  p1_PI80_1 <- apply(p1_season1, 2, HPDI, prob=0.80)
  p1_PI70_1 <- apply(p1_season1, 2, HPDI, prob=0.70)
  p1_PI60_1 <- apply(p1_season1, 2, HPDI, prob=0.60)
  p1_PI50_1 <- apply(p1_season1, 2, HPDI, prob=0.50)
  
  p1_mu2 <- apply(p1_season2, 2, median)
  p1_PI95_2 <- apply(p1_season2, 2, HPDI, prob=0.95)
  p1_PI89_2 <- apply(p1_season2, 2, HPDI, prob=0.89)
  p1_PI80_2 <- apply(p1_season2, 2, HPDI, prob=0.80)
  p1_PI70_2 <- apply(p1_season2, 2, HPDI, prob=0.70)
  p1_PI60_2 <- apply(p1_season2, 2, HPDI, prob=0.60)
  p1_PI50_2 <- apply(p1_season2, 2, HPDI, prob=0.50)
  
  p2_mu1 <- apply(p2_season1, 2, median)
  p2_PI95_1 <- apply(p2_season1, 2, HPDI, prob=0.95)
  p2_PI89_1 <- apply(p2_season1, 2, HPDI, prob=0.89)
  p2_PI80_1 <- apply(p2_season1, 2, HPDI, prob=0.80)
  p2_PI70_1 <- apply(p2_season1, 2, HPDI, prob=0.70)
  p2_PI60_1 <- apply(p2_season1, 2, HPDI, prob=0.60)
  p2_PI50_1 <- apply(p2_season1, 2, HPDI, prob=0.50)
  
  p2_mu2 <- apply(p2_season2, 2, median)
  p2_PI95_2 <- apply(p2_season2, 2, HPDI, prob=0.95)
  p2_PI89_2 <- apply(p2_season2, 2, HPDI, prob=0.89)
  p2_PI80_2 <- apply(p2_season2, 2, HPDI, prob=0.80)
  p2_PI70_2 <- apply(p2_season2, 2, HPDI, prob=0.70)
  p2_PI60_2 <- apply(p2_season2, 2, HPDI, prob=0.60)
  p2_PI50_2 <- apply(p2_season2, 2, HPDI, prob=0.50)
  
  
  p1_df1 <- as.data.frame(
    cbind(
      xseq_fine,
      p1_mu1,
      t(p1_PI95_1),
      t(p1_PI89_1),
      t(p1_PI80_1),
      t(p1_PI70_1),
      t(p1_PI60_1),
      t(p1_PI50_1)
    )
  )
  
  p1_df2 <- as.data.frame(
    cbind(
      xseq_fine,
      p1_mu2,
      t(p1_PI95_2),
      t(p1_PI89_2),
      t(p1_PI80_2),
      t(p1_PI70_2),
      t(p1_PI60_2),
      t(p1_PI50_2)
    )
  )
  
  p2_df1 <- as.data.frame(
    cbind(
      xseq_grid,
      p2_mu1,
      t(p2_PI95_1),
      t(p2_PI89_1),
      t(p2_PI80_1),
      t(p2_PI70_1),
      t(p2_PI60_1),
      t(p2_PI50_1)
    )
  )
  
  p2_df2 <- as.data.frame(
    cbind(
      xseq_grid,
      p2_mu2,
      t(p2_PI95_2),
      t(p2_PI89_2),
      t(p2_PI80_2),
      t(p2_PI70_2),
      t(p2_PI60_2),
      t(p2_PI50_2)
    )
  )
  
  # Make the plots
  if(i != length(key_sp)){
    # Plot 1
    plot1 <- ggplot(p1_df1,
                    aes(x = xseq_fine, y = p1_mu1)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_line() +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), lim = c(0, 1)) +
      ylab(expression(psi)) +
      xlab("Opuntia cover (%)") + 
      scale_x_continuous(breaks = c(-0.7663, 1.703541, 4.17335), labels = c("0", "20", "40"),
                         lim = c(-0.7663, 4.17335)) +
      theme_classic() +
      theme(
        axis.title.y = element_text(angle = 0, vjust = y_vjust),
        axis.text.x = element_blank(),   
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.x = element_blank())
    
    # Plot 2
    plot2 <- ggplot(p1_df2,
                    aes(x = xseq_fine, y = p1_mu2)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_line() +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), lim = c(0, 1)) +
      ylab(expression(psi)) +
      xlab("Opuntia cover (%)") + 
      scale_x_continuous(breaks = c(-0.7663, 1.703541, 4.17335), labels = c("0", "20", "40"),
                         lim = c(-0.7663, 4.17335)) +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),   
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
    
    
    # Plot 3
    plot3 <- ggplot(p2_df1,
                    aes(x = xseq_grid, y = p2_mu1)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_line() +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), lim = c(0, 1)) +
      xlab(expression(paste("Grid square Opuntia (m" ^ "3", ")"))) +
      ylab(expression(psi)) +
      scale_x_continuous(breaks = c(-1.650739, 0.7262536, 3.103246), labels = c(4.5, 10.5, 16.5),
                         lim = c(-1.650739, 3.103246)) +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),   
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
    
    # Plot 4
    plot4 <- ggplot(p2_df2,
                    aes(x = xseq_grid, y = p2_mu2)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_line() +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), lim = c(0, 1)) +
      xlab(expression(paste("Grid square Opuntia (m" ^ "3", ")"))) +
      ylab(expression(psi)) +
      scale_x_continuous(breaks = c(-1.650739, 0.7262536, 3.103246), labels = c(4.5, 10.5, 16.5),
                         lim = c(-1.650739, 3.103246)) +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),   
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
  }else{
    # Plot 1
    plot1 <- ggplot(p1_df1,
                    aes(x = xseq_fine, y = p1_mu1)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols[(i-1)*4 + 1], alpha = colouralpha) +
      geom_line() +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), lim = c(0, 1)) +
      ylab(expression(psi)) +
      xlab("") + 
      scale_x_continuous(breaks = c(-0.7663, 1.703541, 4.17335), labels = c("0", "20", "40"),
                         lim = c(-0.7663, 4.17335)) +
      theme_classic() +
      theme(
        axis.title.y = element_text(angle = 0, vjust = y_vjust),
      )
    
    # Plot 2
    plot2 <- ggplot(p1_df2,
                    aes(x = xseq_fine, y = p1_mu2)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols[(i-1)*4 + 2], alpha = colouralpha) +
      geom_line() +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), lim = c(0, 1)) +
      ylab(expression(psi)) +
      xlab("") + 
      scale_x_continuous(breaks = c(-0.7663, 1.703541, 4.17335), labels = c("0", "20", "40"),
                         lim = c(-0.7663, 4.17335)) +
      theme_classic() +
      theme(
        axis.text.y = element_blank(),   
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
    
    
    # Plot 3
    plot3 <- ggplot(p2_df1,
                    aes(x = xseq_grid, y = p2_mu1)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols[(i-1)*4 + 3], alpha = colouralpha) +
      geom_line() +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), lim = c(0, 1)) +
      xlab("") +
      ylab(expression(psi)) +
      scale_x_continuous(breaks = c(-1.650739, 0.7262536, 3.103246), labels = c(4.5, 10.5, 16.5),
                         lim = c(-1.650739, 3.103246)) +
      theme_classic() +
      theme(
        axis.text.y = element_blank(),   
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
    
    # Plot 4
    plot4 <- ggplot(p2_df2,
                    aes(x = xseq_grid, y = p2_mu2)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols[(i-1)*4 + 4], alpha = colouralpha) +
      geom_line() +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1), lim = c(0, 1)) +
      xlab("") +
      ylab(expression(psi)) +
      scale_x_continuous(breaks = c(-1.650739, 0.7262536, 3.103246), labels = c(4.5, 10.5, 16.5),
                         lim = c(-1.650739, 3.103246)) +
      theme_classic() +
      theme(
        axis.text.y = element_blank(),   
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
  }
  
  
  p_list[[(i-1)*4 + 1]] <- plot1
  p_list[[(i-1)*4 + 2]] <- plot2
  p_list[[(i-1)*4 + 3]] <- plot3
  p_list[[(i-1)*4 + 4]] <- plot4
}

grid1 <- plot_grid(plotlist = p_list, 
                   nrow = 12,
                   rel_widths = c(1.25,1,1,1),
                   rel_heights = c(rep(1,11), 1.5),
                   greedy = FALSE)

setwd("C:/temp/figures")
ggsave2("fig4.svg", 
        plot = grid1,
        bg = "white",
        width = 155,
        height = 210,
        units = "mm")


# Total detections ####
p_list2 <- list() # List to store plots
for(i in 1:length(key_sp)){
  print(i)
  # Subset to current species
  p1 <- total_activity_post_list_fine2[[key_sp[i]]]
  p2 <- total_activity_post_list_grid2[[key_sp[i]]]
  
  
  # Calculate marginal effects
  p1_season1 <- matrix(NA, nrow=nrow(p1), ncol=length(xseq_fine))
  p1_season2 <- matrix(NA, nrow=nrow(p1), ncol=length(xseq_fine))
  p2_season1 <- matrix(NA, nrow=nrow(p2), ncol=length(xseq_grid))
  p2_season2 <- matrix(NA, nrow=nrow(p2), ncol=length(xseq_grid))
  
  for(x in 1:length(xseq_fine)){
    p1_season1[,x] <- exp(p1$`alpha_bar[1]` + p1$`beta_opuntia[1]`*xseq_fine[x])
    p1_season2[,x] <- exp(p1$`alpha_bar[2]` + p1$`beta_opuntia[2]`*xseq_fine[x])
  }
  for(x in 1:length(xseq_grid)){
    p2_season1[,x] <- exp(p2$`alpha_bar[1]` + p2$`beta_opuntia[1]`*xseq_grid[x])
    p2_season2[,x] <- exp(p2$`alpha_bar[2]` + p2$`beta_opuntia[2]`*xseq_grid[x])
  }
  
  p1_mu1 <- apply(p1_season1, 2, median)
  p1_PI95_1 <- apply(p1_season1, 2, HPDI, prob=0.95)
  p1_PI89_1 <- apply(p1_season1, 2, HPDI, prob=0.89)
  p1_PI80_1 <- apply(p1_season1, 2, HPDI, prob=0.80)
  p1_PI70_1 <- apply(p1_season1, 2, HPDI, prob=0.70)
  p1_PI60_1 <- apply(p1_season1, 2, HPDI, prob=0.60)
  p1_PI50_1 <- apply(p1_season1, 2, HPDI, prob=0.50)
  
  p1_mu2 <- apply(p1_season2, 2, median)
  p1_PI95_2 <- apply(p1_season2, 2, HPDI, prob=0.95)
  p1_PI89_2 <- apply(p1_season2, 2, HPDI, prob=0.89)
  p1_PI80_2 <- apply(p1_season2, 2, HPDI, prob=0.80)
  p1_PI70_2 <- apply(p1_season2, 2, HPDI, prob=0.70)
  p1_PI60_2 <- apply(p1_season2, 2, HPDI, prob=0.60)
  p1_PI50_2 <- apply(p1_season2, 2, HPDI, prob=0.50)
  
  p2_mu1 <- apply(p2_season1, 2, median)
  p2_PI95_1 <- apply(p2_season1, 2, HPDI, prob=0.95)
  p2_PI89_1 <- apply(p2_season1, 2, HPDI, prob=0.89)
  p2_PI80_1 <- apply(p2_season1, 2, HPDI, prob=0.80)
  p2_PI70_1 <- apply(p2_season1, 2, HPDI, prob=0.70)
  p2_PI60_1 <- apply(p2_season1, 2, HPDI, prob=0.60)
  p2_PI50_1 <- apply(p2_season1, 2, HPDI, prob=0.50)
  
  p2_mu2 <- apply(p2_season2, 2, median)
  p2_PI95_2 <- apply(p2_season2, 2, HPDI, prob=0.95)
  p2_PI89_2 <- apply(p2_season2, 2, HPDI, prob=0.89)
  p2_PI80_2 <- apply(p2_season2, 2, HPDI, prob=0.80)
  p2_PI70_2 <- apply(p2_season2, 2, HPDI, prob=0.70)
  p2_PI60_2 <- apply(p2_season2, 2, HPDI, prob=0.60)
  p2_PI50_2 <- apply(p2_season2, 2, HPDI, prob=0.50)
  
  
  p1_df1 <- as.data.frame(
    cbind(
      xseq_fine,
      p1_mu1,
      t(p1_PI95_1),
      t(p1_PI89_1),
      t(p1_PI80_1),
      t(p1_PI70_1),
      t(p1_PI60_1),
      t(p1_PI50_1)
    )
  )
  
  p1_df2 <- as.data.frame(
    cbind(
      xseq_fine,
      p1_mu2,
      t(p1_PI95_2),
      t(p1_PI89_2),
      t(p1_PI80_2),
      t(p1_PI70_2),
      t(p1_PI60_2),
      t(p1_PI50_2)
    )
  )
  
  p2_df1 <- as.data.frame(
    cbind(
      xseq_grid,
      p2_mu1,
      t(p2_PI95_1),
      t(p2_PI89_1),
      t(p2_PI80_1),
      t(p2_PI70_1),
      t(p2_PI60_1),
      t(p2_PI50_1)
    )
  )
  
  p2_df2 <- as.data.frame(
    cbind(
      xseq_grid,
      p2_mu2,
      t(p2_PI95_2),
      t(p2_PI89_2),
      t(p2_PI80_2),
      t(p2_PI70_2),
      t(p2_PI60_2),
      t(p2_PI50_2)
    )
  )
  
  # Make the plots
  if(i != length(key_sp)){
    # Plot 1
    plot1 <- ggplot(p1_df1,
                    aes(x = xseq_fine, y = p1_mu1)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_line() +
      #ylim(0, 1) +
      ylab(expression(lambda)) +
      xlab("Opuntia cover (%)") + 
      scale_x_continuous(breaks = c(-0.7663, 1.703541, 4.17335), labels = c("0", "20", "40"),
                         lim = c(-0.7663, 4.17335)) +
      scale_y_continuous(labels = scale_fun_custom,
                         limits = c(0,max(
                           p1_df1$`0.95|`,
                           p1_df2$`0.95|`,
                           p2_df1$`0.95|`,
                           p2_df2$`0.95|`
                         ))) +
      theme_classic() +
      theme(
        axis.title.y = element_text(angle = 0, vjust = y_vjust),
        axis.text.x = element_blank(),   
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.x = element_blank())
    
    # Plot 2
    plot2 <- ggplot(p1_df2,
                    aes(x = xseq_fine, y = p1_mu2)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_line() +
      #ylim(0, 1) +
      ylab(expression(lambda)) +
      xlab("Opuntia cover (%)") + 
      scale_x_continuous(breaks = c(-0.7663, 1.703541, 4.17335), labels = c("0", "20", "40"),
                         lim = c(-0.7663, 4.17335)) +
      scale_y_continuous(labels = scale_fun_custom,
                         limits = c(0,max(
                           p1_df1$`0.95|`,
                           p1_df2$`0.95|`,
                           p2_df1$`0.95|`,
                           p2_df2$`0.95|`
                         ))) +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),   
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
    
    
    # Plot 3
    plot3 <- ggplot(p2_df1,
                    aes(x = xseq_grid, y = p2_mu1)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_line() +
      #ylim(0, 1) +
      xlab(expression(paste("Grid square Opuntia (m" ^ "3", ")"))) +
      ylab(expression(lambda)) +
      scale_x_continuous(breaks = c(-1.650739, 0.7262536, 3.103246), labels = c(4.5, 10.5, 16.5),
                         lim = c(-1.650739, 3.103246)) +
      scale_y_continuous(labels = scale_fun_custom,
                         limits = c(0,max(
                           p1_df1$`0.95|`,
                           p1_df2$`0.95|`,
                           p2_df1$`0.95|`,
                           p2_df2$`0.95|`
                         ))) +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),   
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
    
    # Plot 4
    plot4 <- ggplot(p2_df2,
                    aes(x = xseq_grid, y = p2_mu2)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_line() +
      #ylim(0, 1) +
      xlab(expression(paste("Grid square Opuntia (m" ^ "3", ")"))) +
      ylab(expression(lambda)) +
      scale_x_continuous(breaks = c(-1.650739, 0.7262536, 3.103246), labels = c(4.5, 10.5, 16.5),
                         lim = c(-1.650739, 3.103246)) +
      scale_y_continuous(labels = scale_fun_custom,
                         limits = c(0,max(
                           p1_df1$`0.95|`,
                           p1_df2$`0.95|`,
                           p2_df1$`0.95|`,
                           p2_df2$`0.95|`
                         ))) +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),   
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
  }else{
    # Plot 1
    plot1 <- ggplot(p1_df1,
                    aes(x = xseq_fine, y = p1_mu1)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols2[(i-1)*4 + 1], alpha = colouralpha) +
      geom_line() +
      ylim(0, 1) +
      ylab(expression(lambda)) +
      xlab("") + 
      scale_x_continuous(breaks = c(-0.7663, 1.703541, 4.17335), labels = c("0", "20", "40"),
                         lim = c(-0.7663, 4.17335)) +
      scale_y_continuous(labels = scale_fun_custom,
                         limits = c(0,max(
                           p1_df1$`0.95|`,
                           p1_df2$`0.95|`,
                           p2_df1$`0.95|`,
                           p2_df2$`0.95|`
                         ))) +
      theme_classic() +
      theme(
        axis.title.y = element_text(angle = 0, vjust = y_vjust)
      )
    
    # Plot 2
    plot2 <- ggplot(p1_df2,
                    aes(x = xseq_fine, y = p1_mu2)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols2[(i-1)*4 + 2], alpha = colouralpha) +
      geom_line() +
      #ylim(0, 1) +
      ylab(expression(lambda)) +
      xlab("") + 
      scale_x_continuous(breaks = c(-0.7663, 1.703541, 4.17335), labels = c("0", "20", "40"),
                         lim = c(-0.7663, 4.17335)) +
      scale_y_continuous(labels = scale_fun_custom,
                         limits = c(0,max(
                           p1_df1$`0.95|`,
                           p1_df2$`0.95|`,
                           p2_df1$`0.95|`,
                           p2_df2$`0.95|`
                         ))) +
      theme_classic() +
      theme(
        axis.text.y = element_blank(),   
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
    
    
    # Plot 3
    plot3 <- ggplot(p2_df1,
                    aes(x = xseq_grid, y = p2_mu1)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols2[(i-1)*4 + 3], alpha = colouralpha) +
      geom_line() +
      #ylim(0, 1) +
      xlab("") +
      ylab(expression(lambda)) +
      scale_x_continuous(breaks = c(-1.650739, 0.7262536, 3.103246), labels = c(4.5, 10.5, 16.5),
                         lim = c(-1.650739, 3.103246)) +
      scale_y_continuous(labels = scale_fun_custom,
                         limits = c(0,max(
                           p1_df1$`0.95|`,
                           p1_df2$`0.95|`,
                           p2_df1$`0.95|`,
                           p2_df2$`0.95|`
                         ))) +
      theme_classic() +
      theme(
        axis.text.y = element_blank(),   
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
    
    # Plot 4
    plot4 <- ggplot(p2_df2,
                    aes(x = xseq_grid, y = p2_mu2)) +
      geom_ribbon(aes(ymin = `|0.95`, ymax = `0.95|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.89`, ymax = `0.89|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.8`, ymax =  `0.8|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.7`, ymax =  `0.7|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.6`, ymax =  `0.6|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_ribbon(aes(ymin = `|0.5`, ymax =  `0.5|`), fill = mycols2[(i-1)*4 + 4], alpha = colouralpha) +
      geom_line() +
      #ylim(0, 1) +
      xlab("") +
      ylab(expression(lambda)) +
      scale_x_continuous(breaks = c(-1.650739, 0.7262536, 3.103246), labels = c(4.5, 10.5, 16.5),
                         lim = c(-1.650739, 3.103246)) +
      scale_y_continuous(labels = scale_fun_custom,
                         limits = c(0,max(
                           p1_df1$`0.95|`,
                           p1_df2$`0.95|`,
                           p2_df1$`0.95|`,
                           p2_df2$`0.95|`
                         ))) +
      theme_classic() +
      theme(
        axis.text.y = element_blank(),   
        #axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        axis.title.y = element_blank())
  }
  
  
  p_list2[[(i-1)*4 + 1]] <- plot1
  p_list2[[(i-1)*4 + 2]] <- plot2
  p_list2[[(i-1)*4 + 3]] <- plot3
  p_list2[[(i-1)*4 + 4]] <- plot4
}


grid2 <- plot_grid(plotlist = p_list2, 
                   nrow = 12,
                   rel_widths = c(1.25,1,1,1),
                   rel_heights = c(rep(1,11), 1.5),
                   greedy = FALSE)
#grid2

setwd("C:/temp/figures")
ggsave2("fig5.svg", 
        plot = grid2,
        bg = "white",
        width = 155,
        height = 210,
        units = "mm")
