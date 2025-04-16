# Load packages ####
#library(rethinking)
library(dplyr)
#library(tidyr)
#library(viridis)
#library(lubridate)
library(posterior)
#library(suncalc)
library(cmdstanr)
library(ggplot2)
library(cowplot)
library(scales)
library(svglite)
library(ggdensity)

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


# Load cmdstan output files ####
# Occupancy ####
pars <- c("lp__", 
          "k_bar",
          "beta_opuntia")

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

# Load fine-scale with vegetation pathway 
setwd("C:/temp/camera_trap_models/occupancy_post")
file_list <- list.files(pattern = ".fine_scale_total_veg_path")
post_list_fine2 <- list()
for(i in seq(1, length(file_list), by = 4)){
  mod <- read_cmdstan_csv(files = file_list[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  post_list_fine2[[i]] <- post
  print(i)
}
post_list_fine2 <- post_list_fine2[lengths(post_list_fine2) != 0]
names(post_list_fine2) <- key_sp_alphabetical

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

# Load grid square with vegetation pathway
setwd("C:/temp/camera_trap_models/occupancy_post")
file_list <- list.files(pattern = ".grid_square_total_veg_path")
post_list_grid2 <- list()
for(i in seq(1, length(file_list), by = 4)){
  mod <- read_cmdstan_csv(files = file_list[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  post_list_grid2[[i]] <- post
  print(i)
}
post_list_grid2 <- post_list_grid2[lengths(post_list_grid2) != 0]
names(post_list_grid2) <- key_sp_alphabetical

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

# Load fine scale with veg pathway
setwd("C:/temp/camera_trap_models/total_activity/output")
file_list1 <- list.files(pattern = ".fine_scale_total_vegpath.")
total_activity_post_list_fine1 <- list()

for(i in seq(1, length(file_list1), by = 4)){
  mod <- read_cmdstan_csv(files = file_list1[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  total_activity_post_list_fine1[[i]] <- post
  print(i)
}
total_activity_post_list_fine1 <- total_activity_post_list_fine1[lengths(total_activity_post_list_fine1) != 0]
names(total_activity_post_list_fine1) <- key_sp_alphabetical

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

# Load grid square with veg pathway
setwd("C:/temp/camera_trap_models/total_activity/output")
file_list1 <- list.files(pattern = ".grid_square_total_vegpath.")
total_activity_post_list_grid1 <- list()

for(i in seq(1, length(file_list1), by = 4)){
  mod <- read_cmdstan_csv(files = file_list1[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  total_activity_post_list_grid1[[i]] <- post
  print(i)
}
total_activity_post_list_grid1 <- total_activity_post_list_grid1[lengths(total_activity_post_list_grid1) != 0]
names(total_activity_post_list_grid1) <- key_sp_alphabetical

# Visualise difference between k_bar and beta_Opuntia distributions ####
p_list <- list()
for(i in 1:length(key_sp)){
  beta_fine_noveg1 <- post_list_fine2[[key_sp[i]]]$`beta_opuntia[1]`
  beta_fine_noveg2 <- post_list_fine2[[key_sp[i]]]$`beta_opuntia[2]`
  beta_fine_veg1 <- post_list_fine1[[key_sp[i]]]$`beta_opuntia[1]`
  beta_fine_veg2 <- post_list_fine1[[key_sp[i]]]$`beta_opuntia[2]`
  beta_grid_noveg1 <- post_list_grid2[[key_sp[i]]]$`beta_opuntia[1]`
  beta_grid_noveg2 <- post_list_grid2[[key_sp[i]]]$`beta_opuntia[2]`
  beta_grid_veg1 <- post_list_grid1[[key_sp[i]]]$`beta_opuntia[1]`
  beta_grid_veg2 <- post_list_grid1[[key_sp[i]]]$`beta_opuntia[2]`
  
  k_fine_noveg1 <- post_list_fine2[[key_sp[i]]]$`k_bar[1]`
  k_fine_noveg2 <- post_list_fine2[[key_sp[i]]]$`k_bar[2]`
  k_fine_veg1 <- post_list_fine1[[key_sp[i]]]$`k_bar[1]`
  k_fine_veg2 <- post_list_fine1[[key_sp[i]]]$`k_bar[2]`
  k_grid_noveg1 <- post_list_grid2[[key_sp[i]]]$`k_bar[1]`
  k_grid_noveg2 <- post_list_grid2[[key_sp[i]]]$`k_bar[2]`
  k_grid_veg1 <- post_list_grid1[[key_sp[i]]]$`k_bar[1]`
  k_grid_veg2 <- post_list_grid1[[key_sp[i]]]$`k_bar[2]`
  
  tmpdf <- as.data.frame(c(
    beta_fine_noveg1,
    beta_fine_noveg2,
    beta_fine_veg1,
    beta_fine_veg2,
    beta_grid_noveg1,
    beta_grid_noveg2,
    beta_grid_veg1,
    beta_grid_veg2
  ))
  colnames(tmpdf) <- "beta"
  
  tmpdf_k <- as.data.frame(c(
    k_fine_noveg1,
    k_fine_noveg2,
    k_fine_veg1,
    k_fine_veg2,
    k_grid_noveg1,
    k_grid_noveg2,
    k_grid_veg1,
    k_grid_veg2
  ))
  colnames(tmpdf_k) <- "k"
  
  tmpdf <- cbind(tmpdf, tmpdf_k)
  
  tmpdf$veg <- rep(c("noveg", "veg"), each = 8000*2)
  tmpdf$scale <- rep(c("fine", "grid"), each = 8000*4)
  tmpdf$season <- rep(c(1,2), each = 8000)
  
  tmpdf1a <- tmpdf %>% filter(scale == "fine") %>% filter(season == 1)
  tmpdf1b <- tmpdf %>% filter(scale == "fine") %>% filter(season == 2)
  
  tmpdf2a <- tmpdf %>% filter(scale == "grid") %>% filter(season == 1)
  tmpdf2b <- tmpdf %>% filter(scale == "grid") %>% filter(season == 2)
  
  ptmp1a <- ggplot(tmpdf1a, aes(x = beta, y = k, fill = veg, colour = veg)) +
    geom_hdr(probs = c(0.95, 0.89, 0.80, 0.70, 0.60, 0.50)) +
    scale_fill_manual(values=c("red", "blue")) +
    scale_colour_manual(values=c("red","blue")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    ylab(expression(paste(bar("k")))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.x = element_text(size = rel(0.8)))
  
  ptmp1b <- ggplot(tmpdf1b, aes(x = beta, y = k, fill = veg, colour = veg)) +
    geom_hdr(probs = c(0.95, 0.89, 0.80, 0.70, 0.60, 0.50)) +
    scale_fill_manual(values=c("red", "blue")) +
    scale_colour_manual(values=c("red","blue")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    ylab(expression(paste(bar("k")))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.x = element_text(size = rel(0.8)))
  
  ptmp2a <- ggplot(tmpdf2a, aes(x = beta, y = k, fill = veg, colour = veg)) +
    geom_hdr(probs = c(0.95, 0.89, 0.80, 0.70, 0.60, 0.50)) +
    scale_fill_manual(values=c("red", "blue")) +
    scale_colour_manual(values=c("red","blue")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    ylab(expression(paste(bar("k")))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.x = element_text(size = rel(0.8)))
  
  ptmp2b <- ggplot(tmpdf2b, aes(x = beta, y = k, fill = veg, colour = veg)) +
    geom_hdr(probs = c(0.95, 0.89, 0.80, 0.70, 0.60, 0.50)) +
    scale_fill_manual(values=c("red", "blue")) +
    scale_colour_manual(values=c("red","blue")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    ylab(expression(paste(bar("k")))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.x = element_text(size = rel(0.8)))
  
  p_list[[(i-1)*4 + 1]] <- ptmp1a
  p_list[[(i-1)*4 + 2]] <- ptmp1b
  p_list[[(i-1)*4 + 3]] <- ptmp2a
  p_list[[(i-1)*4 + 4]] <- ptmp2b
}

grid1a <- plot_grid(plotlist = p_list[1:24],
                    nrow = 6)
grid1b <- plot_grid(plotlist = p_list[25:48],
                    nrow = 6)


setwd("C:/temp/figures")
ggsave2("occ_dens1a.svg", 
        plot = grid1a,
        bg = "white",
        width = 155,
        height = 210,
        units = "mm")

ggsave2("occ_dens1b.svg", 
        plot = grid1b,
        bg = "white",
        width = 155,
        height = 210,
        units = "mm")

# Visualise difference between alpha_bar and beta_Opuntia distributions ####
p_list <- list()
for(i in 1:length(key_sp)){
  beta_fine_noveg1 <- total_activity_post_list_fine2[[key_sp[i]]]$`beta_opuntia[1]`
  beta_fine_noveg2 <- total_activity_post_list_fine2[[key_sp[i]]]$`beta_opuntia[2]`
  beta_fine_veg1 <- total_activity_post_list_fine1[[key_sp[i]]]$`beta_opuntia[1]`
  beta_fine_veg2 <- total_activity_post_list_fine1[[key_sp[i]]]$`beta_opuntia[2]`
  beta_grid_noveg1 <- total_activity_post_list_grid2[[key_sp[i]]]$`beta_opuntia[1]`
  beta_grid_noveg2 <- total_activity_post_list_grid2[[key_sp[i]]]$`beta_opuntia[2]`
  beta_grid_veg1 <- total_activity_post_list_grid1[[key_sp[i]]]$`beta_opuntia[1]`
  beta_grid_veg2 <- total_activity_post_list_grid1[[key_sp[i]]]$`beta_opuntia[2]`

  alpha_fine_noveg1 <- total_activity_post_list_fine2[[key_sp[i]]]$`alpha_bar[1]`
  alpha_fine_noveg2 <- total_activity_post_list_fine2[[key_sp[i]]]$`alpha_bar[2]`
  alpha_fine_veg1 <- total_activity_post_list_fine1[[key_sp[i]]]$`alpha_bar[1]`
  alpha_fine_veg2 <- total_activity_post_list_fine1[[key_sp[i]]]$`alpha_bar[2]`
  alpha_grid_noveg1 <- total_activity_post_list_grid2[[key_sp[i]]]$`alpha_bar[1]`
  alpha_grid_noveg2 <- total_activity_post_list_grid2[[key_sp[i]]]$`alpha_bar[2]`
  alpha_grid_veg1 <- total_activity_post_list_grid1[[key_sp[i]]]$`alpha_bar[1]`
  alpha_grid_veg2 <- total_activity_post_list_grid1[[key_sp[i]]]$`alpha_bar[2]`
  
  tmpdf <- as.data.frame(c(
    beta_fine_noveg1,
    beta_fine_noveg2,
    beta_fine_veg1,
    beta_fine_veg2,
    beta_grid_noveg1,
    beta_grid_noveg2,
    beta_grid_veg1,
    beta_grid_veg2
  ))
  colnames(tmpdf) <- "beta"
  
  tmpdf_alpha <- as.data.frame(c(
    alpha_fine_noveg1,
    alpha_fine_noveg2,
    alpha_fine_veg1,
    alpha_fine_veg2,
    alpha_grid_noveg1,
    alpha_grid_noveg2,
    alpha_grid_veg1,
    alpha_grid_veg2
  ))
  colnames(tmpdf_alpha) <- "alpha"
  
  tmpdf <- cbind(tmpdf, tmpdf_alpha)
  
  tmpdf$veg <- rep(c("noveg", "veg"), each = 8000*2)
  tmpdf$scale <- rep(c("fine", "grid"), each = 8000*4)
  tmpdf$season <- rep(c(1,2), each = 8000)
  
  tmpdf1a <- tmpdf %>% filter(scale == "fine") %>% filter(season == 1)
  tmpdf1b <- tmpdf %>% filter(scale == "fine") %>% filter(season == 2)
  
  tmpdf2a <- tmpdf %>% filter(scale == "grid") %>% filter(season == 1)
  tmpdf2b <- tmpdf %>% filter(scale == "grid") %>% filter(season == 2)
  
  ptmp1a <- ggplot(tmpdf1a, aes(x = beta, y = alpha, fill = veg, colour = veg)) +
    geom_hdr(probs = c(0.95, 0.89, 0.80, 0.70, 0.60, 0.50)) +
    scale_fill_manual(values=c("red", "blue")) +
    scale_colour_manual(values=c("red","blue")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    ylab(expression(alpha)) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.x = element_text(size = rel(0.8)))
  
  ptmp1b <- ggplot(tmpdf1b, aes(x = beta, y = alpha, fill = veg, colour = veg)) +
    geom_hdr(probs = c(0.95, 0.89, 0.80, 0.70, 0.60, 0.50)) +
    scale_fill_manual(values=c("red", "blue")) +
    scale_colour_manual(values=c("red","blue")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    ylab(expression(alpha)) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.x = element_text(size = rel(0.8)))
  
  ptmp2a <- ggplot(tmpdf2a, aes(x = beta, y = alpha, fill = veg, colour = veg)) +
    geom_hdr(probs = c(0.95, 0.89, 0.80, 0.70, 0.60, 0.50)) +
    scale_fill_manual(values=c("red", "blue")) +
    scale_colour_manual(values=c("red","blue")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    ylab(expression(alpha)) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.x = element_text(size = rel(0.8)))
  
  ptmp2b <- ggplot(tmpdf2b, aes(x = beta, y = alpha, fill = veg, colour = veg)) +
    geom_hdr(probs = c(0.95, 0.89, 0.80, 0.70, 0.60, 0.50)) +
    scale_fill_manual(values=c("red", "blue")) +
    scale_colour_manual(values=c("red","blue")) +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    xlab(expression(beta)) +
    ylab(expression(alpha)) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.text.x = element_text(size = rel(0.8)))
  
  p_list[[(i-1)*4 + 1]] <- ptmp1a
  p_list[[(i-1)*4 + 2]] <- ptmp1b
  p_list[[(i-1)*4 + 3]] <- ptmp2a
  p_list[[(i-1)*4 + 4]] <- ptmp2b
}

grid1a <- plot_grid(plotlist = p_list[1:24],
                   nrow = 6)
grid1b <- plot_grid(plotlist = p_list[25:48],
                    nrow = 6)


setwd("C:/temp/figures")
ggsave2("total_dens1a.svg", 
        plot = grid1a,
        bg = "white",
        width = 155,
        height = 210,
        units = "mm")

ggsave2("total_dens1b.svg", 
        plot = grid1b,
        bg = "white",
        width = 155,
        height = 210,
        units = "mm")

