# Load packages ####
library(rethinking)
library(dplyr)
library(tidyr)
library(bayesplot)
library(viridis)
library(lubridate)
library(posterior)
library(suncalc)

# Source helper functions ####
source("C:/temp/Zooniverse/Final/scripts/helper_functions_new_v1.R", echo = FALSE)

# Load data ####
setwd("C:/temp/Zooniverse/Final/processed")
consensus_classifications <- get(load("consensus_classifications.Rdata"))
detmats <- get(load("detmats.Rdata"))
startends <- get(load("startends.Rdata"))
sitedays <- get(load("sitedays.Rdata"))
validation_set <- get(load("validation_set.Rdata"))

setwd("C:/temp/Zooniverse/Final/site_data")
site_data1 <- read.csv("Fieldseason1_site_data_main.csv", header = TRUE)
site_data2 <- read.csv("Fieldseason2_site_data_main.csv", header = TRUE)

tree_data <- read.csv("Cameras_tree_data_main.csv", header = TRUE)

df_mu_t <- get(load("grid_square_total.Rdata"))

dist_river <- read.csv("distance_to_river.csv", header = TRUE)
dist_road <- read.csv("distance_to_road.csv", header = TRUE)

weather <- read.csv("Mpala_weather.csv")
weather <- weather %>% separate(TIMESTAMP, into = c("Date", "Time"), sep = " ", remove = FALSE, convert = FALSE)

# Process data into required format ####
# Merge distance to river/road into the main sites dataframes
# Site location does not change between seasons
dist_river <- dist_river %>% select(Site_ID, HubDist) %>% rename(dist_river = HubDist)
dist_road <- dist_road %>% select(Site_ID, HubDist) %>% rename(dist_road = HubDist)

site_data1 <- merge(site_data1, dist_river, by="Site_ID")
site_data1 <- merge(site_data1, dist_road, by="Site_ID")

site_data2 <- merge(site_data2, dist_river, by="Site_ID")
site_data2 <- merge(site_data2, dist_road, by="Site_ID")

# Calculate total cover
site_data1$opuntia_total_cover <- (site_data1$Opuntia_stricta_FOV + site_data1$Opuntia_other_FOV +
                                    (3*site_data1$Opuntia_stricta_area) + (3*site_data1$Opuntia_other_area))/8

site_data2$opuntia_total_cover <- (site_data2$Opuntia_stricta_FOV + site_data2$Opuntia_other_FOV +
                                     (3*site_data2$Opuntia_stricta_area) + (3*site_data2$Opuntia_other_area))/8


# Count trees at each site
tree_data <- tree_data %>% filter(!grepl("Fallen", Species)) %>% filter(!grepl("Dead", Species)) # Not counting fallen or dead trees
tree_sites <- tree_data %>% select(Site_ID, FOV, Area) %>%
  group_by(Site_ID) %>%
  reframe(n_trees = FOV+Area) %>%
  group_by(Site_ID) %>%
  reframe(n_trees = sum(n_trees)) 

site_data1 <- merge(site_data1, tree_sites, by="Site_ID", all.x = TRUE)
site_data2 <- merge(site_data2, tree_sites, by="Site_ID", all.x = TRUE)

site_data1$n_trees[is.na(site_data1$n_trees)] <- 0 # Sites with no tree data have zero trees
site_data2$n_trees[is.na(site_data2$n_trees)] <- 0 # Sites with no tree data have zero trees

# Average % covers across FOV and area
site_data1$grass_total <- (site_data1$Grass_FOV + (3*site_data1$Grass_area)) / 4
site_data1$forb_total <- (site_data1$Forb_FOV + (3*site_data1$Forb_area)) / 4
site_data1$shrub_total <- (site_data1$Shrub_FOV + (3*site_data1$Shrub_area)) / 4
site_data1$succulent_total <- (site_data1$Succulent_FOV + (3*site_data1$Succulent_area)) / 4
site_data1$tree_total <- (site_data1$Tree_FOV + (3*site_data1$Tree_area)) / 4

site_data1$grass_total[is.na(site_data1$grass_total)] <- 0
site_data1$forb_total[is.na(site_data1$forb_total)] <- 0
site_data1$shrub_total[is.na(site_data1$shrub_total)] <- 0
site_data1$succulent_total[is.na(site_data1$succulent_total)] <- 0
site_data1$tree_total[is.na(site_data1$tree_total)] <- 0

site_data2$grass_total <- (site_data2$Grass_FOV + (3*site_data2$Grass_area)) / 4
site_data2$forb_total <- (site_data2$Forb_FOV + (3*site_data2$Forb_area)) / 4
site_data2$shrub_total <- (site_data2$Shrub_FOV + (3*site_data2$Shrub_area)) / 4
site_data2$succulent_total <- (site_data2$Succulent_FOV + (3*site_data2$Succulent_area)) / 4
site_data2$tree_total <- (site_data2$Tree_FOV + (3*site_data2$Tree_area)) / 4

site_data2$grass_total[is.na(site_data2$grass_total)] <- 0
site_data2$forb_total[is.na(site_data2$forb_total)] <- 0
site_data2$shrub_total[is.na(site_data2$shrub_total)] <- 0
site_data2$succulent_total[is.na(site_data2$succulent_total)] <- 0
site_data2$tree_total[is.na(site_data2$tree_total)] <- 0

# Calculate how often site is used by livestock (inc. camels)
livestock <- detmats$livestock[,-1] + detmats$camel[,-1]
livestock_proportion <- matrix(NA, ncol = 1, nrow = nrow(livestock))
for(i in 1:nrow(livestock)){
  livestock_proportion[i] <- sum(livestock[i,], na.rm = TRUE) / (ncol(livestock) - sum(is.na(livestock[i,])))
}
livestock_proportion <- as.data.frame(livestock_proportion)
livestock_proportion$Site_ID <- detmats$livestock[,1]
colnames(livestock_proportion) <- c("livestock_proportion", "Site_ID")

livestock_proportion1 <- livestock_proportion %>% filter(Site_ID < 2000)
livestock_proportion2 <- livestock_proportion %>% filter(Site_ID >= 2000)

livestock_proportion1$Site_ID <- livestock_proportion1$Site_ID - 1000L
livestock_proportion2$Site_ID <- livestock_proportion2$Site_ID - 2000L

site_data1 <- merge(site_data1, livestock_proportion1, by="Site_ID")
site_data2 <- merge(site_data2, livestock_proportion2, by="Site_ID")

# Merge in grid square-level Opuntia densities
colnames(df_mu_t) <- c("grid_square", "volume_total")
df_mu_t$volume_total <- as.numeric(df_mu_t$volume_total)

site_data1$grid_square <- as.factor(site_data1$Grid_square)
site_data2$grid_square <- as.factor(site_data2$Grid_square)

site_data1 <- merge(site_data1, df_mu_t, by="grid_square", all.x = TRUE)
site_data2 <- merge(site_data2, df_mu_t, by="grid_square", all.x = TRUE)


# Detection covariates 
# Camera model
site_data1$Cam_model <- as.factor(site_data1$Cam_model)
site_data2$Cam_model <- as.factor(site_data2$Cam_model)

# Weather
temperature <- bind_daily_covs(startends = startends,
                               day_data = weather, 
                               day_cov = "AirTC_1_Avg",
                               summary_type = "mean",
                               standardise = TRUE,
                               date_format = "dmy")
temperature[is.na(temperature)] <- -9999 # Replace NA with ridiculous number so it's obvious if these values are used accidentally
temperature <- temperature[order(temperature$Site),] # Ensure temperature matrix is ordered by site ID
temperature_mat <- as.matrix(temperature[,-1])

# Change format of site_data site ID's by adding 1000/2000 to season1/2 data respectively
site_data1$Site_ID <- site_data1$Site_ID + 1000L
site_data2$Site_ID <- site_data2$Site_ID + 2000L

# Combine site data
site_data <- rbind(site_data1, site_data2)

# Ensure that site_data is ordered by site_ID
site_data <- site_data[order(site_data$Site_ID),]

# Generate distance matrix
dmat <- generate_distance_matrix(site_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

# List key species
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

sitedays <- sitedays %>% group_by(Site) %>% summarise(Days = sum(Days)) # Group by site to get total days


# Occupancy ####
# Create dlist for each species and save in Stan JSON format
setwd("C:/temp/Zooniverse/Final/processed/species_detection_jsons")
# Fine-scale
for(sp in 1:length(key_sp)){
  # Select data for the species
  dd <- detmats[key_sp[sp]]
  dd <- dd[[1]]
  dd <- as.matrix(dd[,-1])
  dd[is.na(dd)] <- -9999
  mode(dd) <- "integer"
  
  # Prepare data list for Stan
  dlist <- list(
    # Number of sites and visits
    nsites = as.integer(nrow(dmat)),
    N_maxvisits = as.integer(max(sitedays$Days)),
    V = as.integer(sitedays$Days),
    # Observed data
    y = dd,
    # Occupancy covariates
    opuntia = standardize(site_data$opuntia_total_cover),
    d_water = standardize(site_data$dist_river),
    d_road = standardize(site_data$dist_road),
    livestock = standardize(site_data$livestock_proportion),
    grass = standardize(site_data$grass_total),
    forb = standardize(site_data$forb_total),
    shrub = standardize(site_data$shrub_total),
    succulent = standardize(site_data$succulent_total),
    tree = standardize(site_data$n_trees),
    # Detection covariates
    cam_model = as.integer(site_data$Cam_model),
    temp = temperature_mat,
    
    # Distance matrix
    dmat = dmat,
    
    # Season
    season = as.integer(ifelse(site_data$Site_ID >= 2000, 2, 1)),
    nseasons = 2L)
  
  # Save in Stan JSON format
  write_stan_json(data = dlist, file = paste0(key_sp[sp],"_dlist_fine_scale.json"))
}

# Broad-scale
# Prepare grid square-level data
grid_data <- site_data %>% filter(!is.na(volume_total))
sitedays_grid <- sitedays %>% filter(Site %in% grid_data$Site_ID) #%>% 
#  group_by(Site) %>% 
#  summarise(Days = sum(Days))

# Prepare distance matrix and temperature data for subset of sites with grid-square data
dmat <- generate_distance_matrix(grid_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

temperature <- temperature %>% filter(Site %in% grid_data$Site_ID)
temperature_mat <- as.matrix(temperature[,-1])

for(sp in 1:length(key_sp)){
  # Select data for the species
  dd <- (detmats[key_sp[sp]])
  dd <- as.data.frame(dd[[1]])
  dd <- dd %>% filter(dd$site %in% grid_data$Site_ID)
  dd <- as.matrix(dd[,-1])
  dd[is.na(dd)] <- -9999
  mode(dd) <- "integer"
  
  # Prepare data list for Stan
  dlist <- list(
    # Number of sites and visits
    nsites = as.integer(nrow(dmat)),
    N_maxvisits = as.integer(max(sitedays_grid$Days)),
    V = as.integer(sitedays_grid$Days),
    # Observed data
    y = dd,
    # Occupancy covariates
    opuntia = standardize(grid_data$volume_total),
    d_water = standardize(grid_data$dist_river),
    d_road = standardize(grid_data$dist_road),
    livestock = standardize(grid_data$livestock_proportion),
    grass = standardize(grid_data$grass_total),
    forb = standardize(grid_data$forb_total),
    shrub = standardize(grid_data$shrub_total),
    succulent = standardize(grid_data$succulent_total),
    tree = standardize(grid_data$n_trees),
    # Detection covariates
    cam_model = as.integer(grid_data$Cam_model),
    temp = temperature_mat,
    
    # Distance matrix
    dmat = dmat,
    
    # Season
    season = as.integer(ifelse(grid_data$Site_ID >= 2000, 2, 1)),
    nseasons = 2L)
  
  # Save in Stan JSON format
  write_stan_json(data = dlist, file = paste0(key_sp[sp],"_dlist_grid_square.json"))
}

# Load cmdstan output files 
# Parameters to load
pars <- c("lp__", 
          "k_bar",
          "beta_opuntia")

key_sp_alphabetical <- key_sp[order(key_sp)]

# Load fine-scale no vegetation pathway
setwd("F:/JASMIN_outputs/occupancy_post")
file_list <- list.files(pattern = ".fine_scale_total_no_veg_path")
post_list_fine1 <- list()
diagnostics_list_fine1 <- list()
for(i in seq(1, length(file_list), by = 4)){
  mod <- read_cmdstan_csv(files = file_list[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  post_list_fine1[[i]] <- post
  diagnostics_list_fine1[[i]] <- diagnostics
  print(i)
}
post_list_fine1 <- post_list_fine1[lengths(post_list_fine1) != 0]
diagnostics_list_fine1 <- diagnostics_list_fine1[lengths(diagnostics_list_fine1) != 0]
names(post_list_fine1) <- key_sp_alphabetical
names(diagnostics_list_fine1) <- key_sp_alphabetical

# Load fine-scale with vegetation pathway
setwd("F:/JASMIN_outputs/occupancy_post")
file_list <- list.files(pattern = ".fine_scale_total_veg_path")
post_list_fine2 <- list()
diagnostics_list_fine2 <- list()
for(i in seq(1, length(file_list), by = 4)){
  mod <- read_cmdstan_csv(files = file_list[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  post_list_fine2[[i]] <- post
  diagnostics_list_fine2[[i]] <- diagnostics
  print(i)
}
post_list_fine2 <- post_list_fine2[lengths(post_list_fine2) != 0]
diagnostics_list_fine2 <- diagnostics_list_fine2[lengths(diagnostics_list_fine2) != 0]
names(post_list_fine2) <- key_sp_alphabetical
names(diagnostics_list_fine2) <- key_sp_alphabetical

# Load grid square with no vegetation pathway
setwd("F:/JASMIN_outputs/occupancy_post")
file_list <- list.files(pattern = ".grid_square_total_no_veg_path")
post_list_grid1 <- list()
diagnostics_list_grid1 <- list()
for(i in seq(1, length(file_list), by = 4)){
  mod <- read_cmdstan_csv(files = file_list[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  post_list_grid1[[i]] <- post
  diagnostics_list_grid1[[i]] <- diagnostics
  print(i)
}
post_list_grid1 <- post_list_grid1[lengths(post_list_grid1) != 0]
diagnostics_list_grid1 <- diagnostics_list_grid1[lengths(diagnostics_list_grid1) != 0]
names(post_list_grid1) <- key_sp_alphabetical
names(diagnostics_list_grid1) <- key_sp_alphabetical

# Load grid square with vegetation pathway
setwd("F:/JASMIN_outputs/occupancy_post")
file_list <- list.files(pattern = ".grid_square_total_veg_path")
post_list_grid2 <- list()
diagnostics_list_grid2 <- list()
for(i in seq(1, length(file_list), by = 4)){
  mod <- read_cmdstan_csv(files = file_list[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  post_list_grid2[[i]] <- post
  diagnostics_list_grid2[[i]] <- diagnostics
  print(i)
}
post_list_grid2 <- post_list_grid2[lengths(post_list_grid2) != 0]
diagnostics_list_grid2 <- diagnostics_list_grid2[lengths(diagnostics_list_grid2) != 0]
names(post_list_grid2) <- key_sp_alphabetical
names(diagnostics_list_grid2) <- key_sp_alphabetical

# Reload saved post_lists
# setwd("C:/temp/Zooniverse/Final/processed/models_post")
#post_list_fine1 <- get(load("post_list_fine1.Rdata"))
#post_list_fine2 <- get(load("post_list_fine2.Rdata"))
#post_list_grid1 <- get(load("post_list_grid1.Rdata"))
#post_list_grid2 <- get(load("post_list_grid2.Rdata"))

# Marginal effect plots
# Load good plot settings
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

# Set directory to save images
setwd("F:/JASMIN_outputs/occupancy_figures")

# Fine scale - total effect no vegetation pathway
# Open new graphics device to save as TIFF
tiff("fine_scale_total_novegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-0.7575, 4.2039, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- post_list_fine1[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- inv_logit(s$`k_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$`k_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = expression(psi),
       xlab="Opuntia cover", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Fine scale - total effect with vegetation pathway
# Open new graphics device to save as TIFF
tiff("fine_scale_total_vegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-0.7575, 4.2039, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- post_list_fine2[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- inv_logit(s$`k_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$`k_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = expression(psi),
       xlab="Opuntia cover", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Grid square - total effect no vegetation pathway
# Open new graphics device to save as TIFF
tiff("grid_square_total_novegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-1.554, 3.585, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- post_list_grid1[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- inv_logit(s$`k_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$`k_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = expression(psi),
       xlab="Opuntia grid square vol.", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Grid square - total effect with vegetation pathway
# Open new graphics device to save as TIFF
tiff("grid_square_total_vegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-1.554, 3.585, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- post_list_grid2[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- inv_logit(s$`k_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$`k_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = expression(psi),
       xlab="Opuntia grid square vol.", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device


# Total number of detections ####
setwd("C:/temp/Zooniverse/Final/processed/total_activity_jsons")
for(sp in 1:length(key_sp)){
  # Subset to focal key species
  dat <- consensus_classifications %>% filter(species == key_sp[sp])
  
  # Indicate observation
  dat$detections <- 1L
  
  # Group by site and calculate total number of obs
  dat_grouped <- dat %>% group_by(site) %>%
    summarise(across(c(detections), sum))
  dat_grouped$site <- as.integer(gsub("Site_", "", dat_grouped$site))
  
  # Sites with no detections are missing - add them back in
  sites_all <- as.data.frame(sitedays$Site)
  colnames(sites_all) <- "site"
  dat_grouped <- merge(sites_all, dat_grouped, by = "site", all.x = TRUE)
  dat_grouped$detections[is.na(dat_grouped$detections)] <- 0
  
  # Bind to site/grid covariates 
  dat_grouped <- merge(dat_grouped, site_data, by.x = "site", by.y = "Site_ID", all.x = TRUE)
  
  # Overwrite dat to avoid re-writing dlist code
  dat <- dat_grouped 
  
  # Generate distance matrix
  dmat <- generate_distance_matrix(dat, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)
  
  # Site data dlist
  dlist <- list(
    # Observation and season indexes
    n_obs = as.integer(nrow(dat)),
    
    season = as.integer(ifelse(dat$site >= 2000, 2, 1)),
    nseasons = 2L,
    
    # Observed data
    detections = as.integer(dat$detections),
    
    # Covariates
    opuntia = standardize(dat$opuntia_total_cover),
    d_water = standardize(dat$dist_river),
    d_road = standardize(dat$dist_road),
    livestock = standardize(dat$livestock_proportion),
    grass = standardize(dat$grass_total),
    forb = standardize(dat$forb_total),
    shrub = standardize(dat$shrub_total),
    succulent = standardize(dat$succulent_total),
    tree = standardize(dat$n_trees),
    
    # Distance matrix
    dmat = dmat
  )
  
  # In some cases, all succulent values are 0 so standardize returns NaN
  dlist$succulent[is.nan(dlist$succulent)] <- 0
  
  write_stan_json(data = dlist, file = paste0(key_sp[sp],"_total_activity_dlist_fine_scale.json"))
  rm(dlist)
  
  # Grid square dlist
  dat <- dat %>% filter(!is.na(volume_total))
  
  # Generate distance matrix
  dmat <- generate_distance_matrix(dat, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)
  
  
  dlist <- list(
    # Observation and season indexes
    n_obs = as.integer(nrow(dat)),
    season = as.integer(ifelse(dat$site >= 2000, 2, 1)),
    nseasons = 2L,
    
    # Observed data
    detections = as.integer(dat$detections),
    
    # Covariates
    opuntia = standardize(dat$opuntia_total_cover),
    d_water = standardize(dat$dist_river),
    d_road = standardize(dat$dist_road),
    livestock = standardize(dat$livestock_proportion),
    grass = standardize(dat$grass_total),
    forb = standardize(dat$forb_total),
    shrub = standardize(dat$shrub_total),
    succulent = standardize(dat$succulent_total),
    tree = standardize(dat$n_trees),
    
    # Distance matrix
    dmat = dmat
  )
  # In some cases, all succulent values are 0 so standardize returns NaN
  dlist$succulent[is.nan(dlist$succulent)] <- 0
  
  write_stan_json(data = dlist, file = paste0(key_sp[sp],"_total_activity_dlist_grid_square.json"))
}

# Load output from JASMIN
setwd("F:/JASMIN_outputs/total_activity_gp")
key_sp_alphabetical <- key_sp[order(key_sp)]
pars <- c("lp__", 
          "alpha_bar",
          "beta_opuntia")

# Grid square total vegpath
file_list1 <- list.files(pattern = ".grid_square_total_vegpath.")
total_activity_post_list_grid1 <- list()
total_activity_diagnostics_list_grid1 <- list()

for(i in seq(1, length(file_list1), by = 4)){
  mod <- read_cmdstan_csv(files = file_list1[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  total_activity_post_list_grid1[[i]] <- post
  total_activity_diagnostics_list_grid1[[i]] <- diagnostics
  print(i)
}
total_activity_post_list_grid1 <- total_activity_post_list_grid1[lengths(total_activity_post_list_grid1) != 0]
total_activity_diagnostics_list_grid1 <- total_activity_diagnostics_list_grid1[lengths(total_activity_diagnostics_list_grid1) != 0]
names(total_activity_post_list_grid1) <- key_sp_alphabetical
names(total_activity_diagnostics_list_grid1) <- key_sp_alphabetical

# Plot results
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

setwd("C:/temp/Zooniverse/Final/figures/total_activity_figures")
tiff("total_activity_grid_square_total_vegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-1.554, 3.585, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- total_activity_post_list_grid1[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- exp(s$`alpha_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- exp(s$`alpha_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  #PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  #PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = "P(night)",
       xlab="Opuntia grid square vol.", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Grid square total novegpath
setwd("F:/JASMIN_outputs/total_activity_gp")
file_list2 <- list.files(pattern = ".grid_square_total_novegpath.")
total_activity_post_list_grid2 <- list()
total_activity_diagnostics_list_grid2 <- list()

for(i in seq(1, length(file_list2), by = 4)){
  mod <- read_cmdstan_csv(files = file_list2[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  total_activity_post_list_grid2[[i]] <- post
  total_activity_diagnostics_list_grid2[[i]] <- diagnostics
  print(i)
}
total_activity_post_list_grid2 <- total_activity_post_list_grid2[lengths(total_activity_post_list_grid2) != 0]
total_activity_diagnostics_list_grid2 <- total_activity_diagnostics_list_grid2[lengths(total_activity_diagnostics_list_grid2) != 0]
names(total_activity_post_list_grid2) <- key_sp_alphabetical
names(total_activity_diagnostics_list_grid2) <- key_sp_alphabetical

# Plot results
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

setwd("C:/temp/Zooniverse/Final/figures/total_activity_figures")
tiff("total_activity_grid_square_total_novegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-1.554, 3.585, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- total_activity_post_list_grid2[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- exp(s$`alpha_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- exp(s$`alpha_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  #PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  #PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = "P(night)",
       xlab="Opuntia grid square vol.", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Fine scale total vegpath
setwd("F:/JASMIN_outputs/total_activity_gp")
file_list1 <- list.files(pattern = ".fine_scale_total_vegpath.")
total_activity_post_list_fine1 <- list()
total_activity_diagnostics_list_fine1 <- list()

for(i in seq(1, length(file_list1), by = 4)){
  mod <- read_cmdstan_csv(files = file_list1[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  total_activity_post_list_fine1[[i]] <- post
  total_activity_diagnostics_list_fine1[[i]] <- diagnostics
  print(i)
}
total_activity_post_list_fine1 <- total_activity_post_list_fine1[lengths(total_activity_post_list_fine1) != 0]
total_activity_diagnostics_list_fine1 <- total_activity_diagnostics_list_fine1[lengths(total_activity_diagnostics_list_fine1) != 0]
names(total_activity_post_list_fine1) <- key_sp_alphabetical
names(total_activity_diagnostics_list_fine1) <- key_sp_alphabetical

# Plot results
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

setwd("C:/temp/Zooniverse/Final/figures/total_activity_figures")
tiff("total_activity_fine_scale_total_vegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-0.7575, 4.2039, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- total_activity_post_list_fine1[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- exp(s$`alpha_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- exp(s$`alpha_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  #PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  #PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = "P(night)",
       xlab="Opuntia cover", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Fine scale total novegpath
setwd("F:/JASMIN_outputs/total_activity_gp")
file_list2 <- list.files(pattern = ".fine_scale_total_novegpath.")
total_activity_post_list_fine2 <- list()
total_activity_diagnostics_list_fine2 <- list()

for(i in seq(1, length(file_list2), by = 4)){
  mod <- read_cmdstan_csv(files = file_list2[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  total_activity_post_list_fine2[[i]] <- post
  total_activity_diagnostics_list_fine2[[i]] <- diagnostics
  print(i)
}
total_activity_post_list_fine2 <- total_activity_post_list_fine2[lengths(total_activity_post_list_fine2) != 0]
total_activity_diagnostics_list_fine2 <- total_activity_diagnostics_list_fine2[lengths(total_activity_diagnostics_list_fine2) != 0]
names(total_activity_post_list_fine2) <- key_sp_alphabetical
names(total_activity_diagnostics_list_fine2) <- key_sp_alphabetical

# Plot results
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

setwd("C:/temp/Zooniverse/Final/figures/total_activity_figures")
tiff("total_activity_fine_scale_total_novegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-0.7575, 4.2039, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- total_activity_post_list_fine2[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- exp(s$`alpha_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- exp(s$`alpha_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  #PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  #PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = "P(night)",
       xlab="Opuntia cover", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device


# Night/day detections ####
setwd("C:/temp/Zooniverse/Final/processed/day_night_detection_jsons/gp")
for(sp in 1:length(key_sp)){
  # Subset to focal key species
  dat <- consensus_classifications %>% filter(species == key_sp[sp])
  
  # Obtain solar noon times for each observation
  duskdawn <- getSunlightTimes(date = date(dat$DateTimeLub),
                                 lat = 0.293061,
                                 lon = 36.899246,
                                 keep = c("dusk", "dawn"),
                                 tz = "Africa/Nairobi")
  
  # Force times to UTC as camera trap datetimes are (incorrectly) stored as UTC
  duskdawn$dusk <- force_tz(duskdawn$dusk, "UTC")
  duskdawn$dawn <- force_tz(duskdawn$dawn, "UTC")
  
  
  # Indicate whether observation is at night
  dat$night <- ifelse(dat$DateTimeLub < duskdawn$dawn | dat$DateTimeLub > duskdawn$dusk, 1, 0)
  dat$surveyed <- 1L
  
  
  # Group by site and calculate total number of obs. at night, and in total
  dat_grouped <- dat %>% group_by(site) %>%
    summarise(across(c(night, surveyed), sum))
  
  # Bind to site/grid covariates 
  dat_grouped$site <- as.integer(gsub("Site_", "", dat_grouped$site))
  dat_grouped <- merge(dat_grouped, site_data, by.x = "site", by.y = "Site_ID", all.x = TRUE)
  
  # Overwrite dat to avoid re-writing dlist code
  dat <- dat_grouped 
  
  # Generate distance matrix
  dmat <- generate_distance_matrix(dat, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

  # Site data dlist
  dlist <- list(
    # Observation and season indexes
    n_obs = as.integer(nrow(dat)),

    season = as.integer(ifelse(dat$site >= 2000, 2, 1)),
    nseasons = 2L,
    
    # Observed data
    night = as.integer(dat$night),
    surveyed = as.integer(dat$surveyed),
    
    # Covariates
    opuntia = standardize(dat$opuntia_total_cover),
    d_water = standardize(dat$dist_river),
    d_road = standardize(dat$dist_road),
    livestock = standardize(dat$livestock_proportion),
    grass = standardize(dat$grass_total),
    forb = standardize(dat$forb_total),
    shrub = standardize(dat$shrub_total),
    succulent = standardize(dat$succulent_total),
    tree = standardize(dat$n_trees),
    
    # Distance matrix
    dmat = dmat
  )
  
  # In some cases, all succulent values are 0 so standardize returns NaN
  dlist$succulent[is.nan(dlist$succulent)] <- 0
  
  write_stan_json(data = dlist, file = paste0(key_sp[sp],"_day_night_detection_dlist_fine_scale.json"))
  rm(dlist)
  
  # Grid square dlist
  dat <- dat %>% filter(!is.na(volume_total))
  
  # Generate distance matrix
  dmat <- generate_distance_matrix(dat, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)
  
  
  dlist <- list(
    # Observation and season indexes
    n_obs = as.integer(nrow(dat)),
    season = as.integer(ifelse(dat$site >= 2000, 2, 1)),
    nseasons = 2L,
    
    # Observed data
    night = as.integer(dat$night),
    surveyed = as.integer(dat$surveyed),
    
    # Covariates
    opuntia = standardize(dat$opuntia_total_cover),
    d_water = standardize(dat$dist_river),
    d_road = standardize(dat$dist_road),
    livestock = standardize(dat$livestock_proportion),
    grass = standardize(dat$grass_total),
    forb = standardize(dat$forb_total),
    shrub = standardize(dat$shrub_total),
    succulent = standardize(dat$succulent_total),
    tree = standardize(dat$n_trees),
    
    # Distance matrix
    dmat = dmat
  )
  # In some cases, all succulent values are 0 so standardize returns NaN
  dlist$succulent[is.nan(dlist$succulent)] <- 0
  
  write_stan_json(data = dlist, file = paste0(key_sp[sp],"_day_night_detection_dlist_grid_square.json"))
}


# Load output from JASMIN
setwd("F:/JASMIN_outputs/day_night_detection_gp")
key_sp_alphabetical <- key_sp[order(key_sp)]
pars <- c("lp__", 
          "alpha_bar",
          "beta_opuntia")

# Grid square total vegpath
file_list1 <- list.files(pattern = ".grid_square_total_vegpath.")
day_night_post_list_grid1 <- list()
day_night_diagnostics_list_grid1 <- list()

for(i in seq(1, length(file_list1), by = 4)){
  mod <- read_cmdstan_csv(files = file_list1[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  day_night_post_list_grid1[[i]] <- post
  day_night_diagnostics_list_grid1[[i]] <- diagnostics
  print(i)
}
day_night_post_list_grid1 <- day_night_post_list_grid1[lengths(day_night_post_list_grid1) != 0]
day_night_diagnostics_list_grid1 <- day_night_diagnostics_list_grid1[lengths(day_night_diagnostics_list_grid1) != 0]
names(day_night_post_list_grid1) <- key_sp_alphabetical
names(day_night_diagnostics_list_grid1) <- key_sp_alphabetical

# Plot results
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

setwd("C:/temp/Zooniverse/Final/figures/day_night_figures")
tiff("day_night_grid_square_total_vegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-1.554, 3.585, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- day_night_post_list_grid1[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- inv_logit(s$`alpha_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$`alpha_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  #PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  #PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = "P(night)",
       xlab="Opuntia grid square vol.", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Grid square total novegpath
setwd("F:/JASMIN_outputs/day_night_detection_gp")
file_list2 <- list.files(pattern = ".grid_square_total_novegpath.")
day_night_post_list_grid2 <- list()
day_night_diagnostics_list_grid2 <- list()

for(i in seq(1, length(file_list2), by = 4)){
  mod <- read_cmdstan_csv(files = file_list2[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  day_night_post_list_grid2[[i]] <- post
  day_night_diagnostics_list_grid2[[i]] <- diagnostics
  print(i)
}
day_night_post_list_grid2 <- day_night_post_list_grid2[lengths(day_night_post_list_grid2) != 0]
day_night_diagnostics_list_grid2 <- day_night_diagnostics_list_grid2[lengths(day_night_diagnostics_list_grid2) != 0]
names(day_night_post_list_grid2) <- key_sp_alphabetical
names(day_night_diagnostics_list_grid2) <- key_sp_alphabetical

# Plot results
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

setwd("C:/temp/Zooniverse/Final/figures/day_night_figures")
tiff("day_night_grid_square_total_novegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-1.554, 3.585, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- day_night_post_list_grid2[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- inv_logit(s$`alpha_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$`alpha_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  #PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  #PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = "P(night)",
       xlab="Opuntia grid square vol.", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Fine scale total vegpath
setwd("F:/JASMIN_outputs/day_night_detection_gp")
file_list1 <- list.files(pattern = ".fine_scale_total_vegpath.")
day_night_post_list_fine1 <- list()
day_night_diagnostics_list_fine1 <- list()

for(i in seq(1, length(file_list1), by = 4)){
  mod <- read_cmdstan_csv(files = file_list1[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  day_night_post_list_fine1[[i]] <- post
  day_night_diagnostics_list_fine1[[i]] <- diagnostics
  print(i)
}
day_night_post_list_fine1 <- day_night_post_list_fine1[lengths(day_night_post_list_fine1) != 0]
day_night_diagnostics_list_fine1 <- day_night_diagnostics_list_fine1[lengths(day_night_diagnostics_list_fine1) != 0]
names(day_night_post_list_fine1) <- key_sp_alphabetical
names(day_night_diagnostics_list_fine1) <- key_sp_alphabetical

# Plot results
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

setwd("C:/temp/Zooniverse/Final/figures/day_night_figures")
tiff("day_night_fine_scale_total_vegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-0.7575, 4.2039, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- day_night_post_list_fine1[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- inv_logit(s$`alpha_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$`alpha_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  #PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  #PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = "P(night)",
       xlab="Opuntia cover", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

# Fine scale total novegpath
setwd("F:/JASMIN_outputs/day_night_detection_gp")
file_list2 <- list.files(pattern = ".fine_scale_total_novegpath.")
day_night_post_list_fine2 <- list()
day_night_diagnostics_list_fine2 <- list()

for(i in seq(1, length(file_list2), by = 4)){
  mod <- read_cmdstan_csv(files = file_list2[i:(i+3)], variables = pars)
  post <- as_draws_df(mod$post_warmup_draws)
  diagnostics <- as_draws_df(mod$post_warmup_sampler_diagnostics)
  day_night_post_list_fine2[[i]] <- post
  day_night_diagnostics_list_fine2[[i]] <- diagnostics
  print(i)
}
day_night_post_list_fine2 <- day_night_post_list_fine2[lengths(day_night_post_list_fine2) != 0]
day_night_diagnostics_list_fine2 <- day_night_diagnostics_list_fine2[lengths(day_night_diagnostics_list_fine2) != 0]
names(day_night_post_list_fine2) <- key_sp_alphabetical
names(day_night_diagnostics_list_fine2) <- key_sp_alphabetical

# Plot results
pr <- get(load("C:/temp/Zooniverse/Final/scripts/good_plot_par.Rdata")) # Good settings for 8-panel plots

# Figure letters
plot_titles <- c("A)", "B)", "C)", "D)", 
                 "E)", "F)", "G)", "H)",
                 "I)", "J)", "K)", "L)")

# Colours for shading CI's
species_colours <- c("#35B779FF","#440154FF")
colouralpha <- 0.4

setwd("C:/temp/Zooniverse/Final/figures/day_night_figures")
tiff("day_night_fine_scale_total_novegpath.tiff", width = 15.83, height = 12.69, units = 'cm', res = 300)
par(pr)
par(mfrow=c(3,4))

xseq <- seq(-0.7575, 4.2039, by = 0.01) # Use real min/max Opuntia cover (standardised) values

# Loop over each species
for(i in 1:length(key_sp)){
  s <- day_night_post_list_fine2[[key_sp[i]]]
  
  # Calculate marginal effects
  p_season1 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  p_season2 <- matrix(NA, nrow=nrow(s), ncol=length(xseq))
  
  for(x in 1:length(xseq)){
    p_season1[,x] <- inv_logit(s$`alpha_bar[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$`alpha_bar[2]` + s$`beta_opuntia[2]`*xseq[x])
  }
  
  mu1 <- apply(p_season1, 2, median)
  #PI95_1 <- apply(p_season1, 2, HPDI, prob=0.95)
  PI89_1 <- apply(p_season1, 2, HPDI, prob=0.89)
  #PI80_1 <- apply(p_season1, 2, HPDI, prob=0.80)
  #PI70_1 <- apply(p_season1, 2, HPDI, prob=0.70)
  #PI60_1 <- apply(p_season1, 2, HPDI, prob=0.60)
  #PI50_1 <- apply(p_season1, 2, HPDI, prob=0.50)
  #PI_all_1 <- rbind(PI95_1, PI89_1, PI80_1, PI70_1, PI60_1, PI50_1)
  
  mu2 <- apply(p_season2, 2, median)
  #PI95_2 <- apply(p_season2, 2, HPDI, prob=0.95)
  PI89_2 <- apply(p_season2, 2, HPDI, prob=0.89)
  #PI80_2 <- apply(p_season2, 2, HPDI, prob=0.80)
  #PI70_2 <- apply(p_season2, 2, HPDI, prob=0.70)
  #PI60_2 <- apply(p_season2, 2, HPDI, prob=0.60)
  #PI50_2 <- apply(p_season2, 2, HPDI, prob=0.50)
  #PI_all_2 <- rbind(PI95_2, PI89_2, PI80_2, PI70_2, PI60_2, PI50_2)
  
  # Make the plots
  plot(NULL, xlim=c(min(xseq),max(xseq)), ylim=c(0,1), main="", 
       ylab = "P(night)",
       xlab="Opuntia cover", 
       yaxt = "n")
  title(paste(plot_titles[i]), adj=0, line = 0.7)
  axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  
  #shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  #shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  #shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  #shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device

