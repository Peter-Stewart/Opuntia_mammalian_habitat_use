# Load packages ####
library(rethinking)
library(dplyr)
library(tidyr)
library(bayesplot)
library(viridis)
library(lubridate)
library(posterior)

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
#site_data1 <- site_data1 %>% filter(Site_ID %in% sitedays$Site) # Remove the two damaged sites

#opuntia_data <- read.csv("Cameras_opuntia_data_main.csv", header = TRUE)
#opuntia_data <- opuntia_data %>% filter(Site_ID %in% sitedays$Site) # Remove the two damaged sites

tree_data <- read.csv("Cameras_tree_data_main.csv", header = TRUE)
#tree_data <- tree_data %>% filter(Site_ID %in% sitedays$Site) # Remove the two damaged sites

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


# Prepare data lists for upload to cluster
key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "dikdik",
            "giraffe",
            "hyenaspotted",
            "leopard",
            "zebraplains",
            "kudu",
            "buffalo")

indexes <- list()
for(i in 1:length(detmats)){
  if(names(detmats[i]) %in% key_sp){
    indexes[i] <- i
  }else{
    indexes[i] <- NULL}
}
indexes <- do.call(rbind, indexes)

sitedays <- sitedays %>% group_by(Site) %>% summarise(Days = sum(Days)) # Group by site to get total days

# Create dlist for each species and save in Stan JSON format
setwd("C:/temp/Zooniverse/Final/processed/species_detection_jsons")
# Fine-scale
for(sp in 1:length(key_sp)){
  # Select data for the species
  dd <- detmats[indexes[sp,]]
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
  group_by(Site) %>% 
  summarise(Days = sum(Days))

# Prepare distance matrix and temperature data for subset of sites with grid-square data
dmat <- generate_distance_matrix(grid_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

temperature <- temperature %>% filter(Site %in% grid_data$Site_ID)
temperature_mat <- as.matrix(temperature[,-1])

for(sp in 1:length(key_sp)){
  # Select data for the species
  dd <- (detmats[indexes[sp,]])
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

# Load cmdstan output files ####
# Parameters to load
pars <- c("lp__", 
          "k_bar",
          "k_season",
          "beta_opuntia")

# Load fine-scale no vegetation pathway
setwd("F:/JASMIN_outputs/occupancy_post/fine_scale/total_no_veg_path")
file_list <- list.files(pattern = ".csv")
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
names(post_list_fine1) <- key_sp
names(diagnostics_list_fine1) <- key_sp

# Load fine-scale with vegetation pathway
setwd("F:/JASMIN_outputs/occupancy_post/fine_scale/total_veg_path")
file_list <- list.files(pattern = ".csv")
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
names(post_list_fine2) <- key_sp
names(diagnostics_list_fine2) <- key_sp

# Load grid square with no vegetation pathway
setwd("F:/JASMIN_outputs/occupancy_post/grid_square/total_no_veg_path")
file_list <- list.files(pattern = ".csv")
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
names(post_list_grid1) <- key_sp
names(diagnostics_list_grid1) <- key_sp

# Load grid square with vegetation pathway
setwd("F:/JASMIN_outputs/occupancy_post/grid_square/total_veg_path")
file_list <- list.files(pattern = ".csv")
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
names(post_list_grid2) <- key_sp
names(diagnostics_list_grid2) <- key_sp


# Marginal effect plots ####
# Load good plot settings
pr <- get(load("C:/Users/PeteS/OneDrive/Durham/Occupancy chapter/good_plot_par.Rdata")) # Good settings for 8-panel plots

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
    p_season1[,x] <- inv_logit(s$k_bar + s$`k_season[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$k_bar + s$`k_season[2]` + s$`beta_opuntia[2]`*xseq[x])
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
  
  shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
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
    p_season1[,x] <- inv_logit(s$k_bar + s$`k_season[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$k_bar + s$`k_season[2]` + s$`beta_opuntia[2]`*xseq[x])
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
  
  shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
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
    p_season1[,x] <- inv_logit(s$k_bar + s$`k_season[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$k_bar + s$`k_season[2]` + s$`beta_opuntia[2]`*xseq[x])
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
  
  shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
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
    p_season1[,x] <- inv_logit(s$k_bar + s$`k_season[1]` + s$`beta_opuntia[1]`*xseq[x])
    p_season2[,x] <- inv_logit(s$k_bar + s$`k_season[2]` + s$`beta_opuntia[2]`*xseq[x])
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
  
  shade(PI95_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI89_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI80_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI70_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI60_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  shade(PI50_1, xseq, col=col.alpha(species_colours[1], colouralpha))
  
  shade(PI95_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI89_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI80_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI70_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI60_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  shade(PI50_2, xseq, col=col.alpha(species_colours[2], colouralpha))
  
  points(x = xseq, y = mu2, type="l", lwd=2, lty = 2)
  points(x = xseq, y = mu1, type="l", lwd=2)
  
  # Optional dashed lines at psi = 0.5 and x = 0
  #abline(h = 0.5, lty = 2)
  #abline(v = 0, lty = 2)
}
dev.off() # Close graphics device



















# Activity analysis ####
# For all key species, plot activity kernel for each and make 1 plot to compare overall activity levels
site_data$Pair <- as.factor(site_data$Pair)
dd <- site_data %>% select(Pair, grid_square, Site_ID)
high_sites <- dd %>% filter(Pair == "high")
low_sites <- dd %>% filter(Pair == "low")
high_sitenames <- matrix(NA, nrow = nrow(high_sites), 1)
low_sitenames <- matrix(NA, nrow = nrow(low_sites), 1)

for(i in 1:nrow(high_sites)){
  site_prefix <- NULL
  if(high_sites[i,3] < 10){
    site_prefix <- "Site_0"
  }else{
    site_prefix <- "Site_"
  }
  high_sitenames[i,1] <- paste0(site_prefix, high_sites[i,3])
}

for(i in 1:nrow(low_sites)){
  site_prefix <- NULL
  if(low_sites[i,3] < 10){
    site_prefix <- "Site_0"
  }else{
    site_prefix <- "Site_"
  }
  low_sitenames[i,1] <- paste0(site_prefix, low_sites[i,3])
}

# Baboon 
df1 <- consensus_classifications %>% filter(species == "baboon") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "baboon") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m1 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m2 <- fitact(dat = t_rad2,
             sample = "data",
             reps = 1000,
             show = TRUE)

# Elephant 
df1 <- consensus_classifications %>% filter(species == "elephant") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "elephant") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m3 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m4 <- fitact(dat = t_rad2,
             sample = "data",
             reps = 1000,
             show = TRUE)

# Vervet monkey
df1 <- consensus_classifications %>% filter(species == "vervetmonkey") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "vervetmonkey") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m5 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m6 <- fitact(dat = t_rad2,
             sample = "data",
             reps = 1000,
             show = TRUE)

# Grevy's zebra
df1 <- consensus_classifications %>% filter(species == "zebragrevys") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "zebragrevys") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m7 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m8 <- fitact(dat = t_rad2,
             sample = "data",
             reps = 1000,
             show = TRUE)

# Impala
df1 <- consensus_classifications %>% filter(species == "impala") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "impala") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m9 <- fitact(dat = t_rad1,
             sample = "data",
             reps = 1000,
             show = TRUE)
m10 <- fitact(dat = t_rad2,
              sample = "data",
              reps = 1000,
              show = TRUE)

# Giraffe
df1 <- consensus_classifications %>% filter(species == "giraffe") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "giraffe") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m11 <- fitact(dat = t_rad1,
              sample = "data",
              reps = 1000,
              show = TRUE)
m12 <- fitact(dat = t_rad2,
              sample = "data",
              reps = 1000,
              show = TRUE)


# Spotted hyena
df1 <- consensus_classifications %>% filter(species == "hyenaspotted") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "hyenaspotted") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m13 <- fitact(dat = t_rad1,
              sample = "data",
              reps = 1000,
              show = TRUE)
m14 <- fitact(dat = t_rad2,
              sample = "data",
              reps = 1000,
              show = TRUE)

# Dik-dik
df1 <- consensus_classifications %>% filter(species == "dikdik") %>% filter(site %in% high_sitenames)
df2 <- consensus_classifications %>% filter(species == "dikdik") %>% filter(site %in% low_sitenames)
times1 <- df1$DateTimeLub
times2 <- df2$DateTimeLub
t_rad1 <- gettime(x = times1,
                  scale = "radian")
t_rad2 <- gettime(x = times2,
                  scale = "radian")
m15 <- fitact(dat = t_rad1,
              sample = "data",
              reps = 1000,
              show = TRUE)
m16 <- fitact(dat = t_rad2,
              sample = "data",
              reps = 1000,
              show = TRUE)

# Save results
setwd("E:/ch3_post_new/activity_analysis/activity_analysis_output")
save(m1, file = "baboon_high.Rdata")
save(m2, file = "baboon_low.Rdata")
save(m3, file = "elephant_high.Rdata")
save(m4, file = "elephant_low.Rdata")
save(m5, file = "vervet_high.Rdata")
save(m6, file = "vervet_low.Rdata")
save(m7, file = "zebragrevys_high.Rdata")
save(m8, file = "zebragrevys_low.Rdata")
save(m9, file = "impala_high.Rdata")
save(m10, file = "impala_low.Rdata")
save(m11, file = "giraffe_high.Rdata")
save(m12, file = "giraffe_low.Rdata")
save(m13, file = "hyenaspotted_high.Rdata")
save(m14, file = "hyenaspotted_low.Rdata")
save(m15, file = "dikdik_high.Rdata")
save(m16, file = "dikdik_low.Rdata")

# Loading saved data 
setwd("E:/ch3_post_new/activity_analysis/activity_analysis_output")
m1 <- get(load("baboon_high.Rdata"))
m2 <- get(load("baboon_low.Rdata"))
m3 <- get(load("elephant_high.Rdata"))
m4 <- get(load("elephant_low.Rdata"))
m5 <- get(load("vervet_high.Rdata"))
m6 <- get(load("vervet_low.Rdata"))
m7 <- get(load("zebragrevys_high.Rdata"))
m8 <- get(load("zebragrevys_low.Rdata"))
m9 <- get(load("impala_high.Rdata"))
m10 <- get(load("impala_low.Rdata"))
m11 <- get(load("giraffe_high.Rdata"))
m12 <- get(load("giraffe_low.Rdata"))
m13 <- get(load("hyenaspotted_high.Rdata"))
m14 <- get(load("hyenaspotted_low.Rdata"))
m15 <- get(load("dikdik_high.Rdata"))
m16 <- get(load("dikdik_low.Rdata"))

# Activity plots for each species
#par(mfrow=c(2,4))
#pr <- par()
pr <- get(load("C:/Users/PeteS/OneDrive/Durham/Occupancy chapter/good_plot_par.Rdata")) # Good settings for 8-panel plots
col_high <- "#440154FF"
col_low <- "#4AC16DFF"
col_alpha <- 0.5

setwd("E:/ch3_post_new/activity_analysis/activity_analysis_output")
tiff("activity_plots_new.tiff", width = 15.83, height = 8.46, units = 'cm', res = 300)
par(pr)
# Baboon
clean_activity_plot(m1, 
                    species_title = "",
                    colour = col_high,
                    alpha = col_alpha)
title("A)", adj=0, line = 0.7)
clean_activity_plot(m2, 
                    colour = col_low,
                    alpha = col_alpha, 
                    add = TRUE)

# Elephant 
clean_activity_plot(m4, 
                    species_title = "",
                    colour = col_low,
                    alpha = col_alpha)
title("B)", adj=0, line = 0.7)
clean_activity_plot(m3, 
                    colour = col_high,
                    alpha = col_alpha, 
                    add = TRUE)

# Vervet monkey 
clean_activity_plot(m5, 
                    species_title = "",
                    colour = col_high,
                    alpha = col_alpha)
title("C)", adj=0, line = 0.7)
clean_activity_plot(m6, 
                    colour = col_low,
                    alpha = col_alpha, 
                    add = TRUE)

# Grevy's zebra
clean_activity_plot(m7, 
                    species_title = "",
                    colour = col_high,
                    alpha = col_alpha)
title("D)", adj=0, line = 0.7)
clean_activity_plot(m8, 
                    colour = col_low,
                    alpha = col_alpha, 
                    add = TRUE)

# Impala
clean_activity_plot(m9, 
                    species_title = "",
                    colour = col_high,
                    alpha = col_alpha)
title("E)", adj=0, line = 0.7)
clean_activity_plot(m10, 
                    colour = col_low,
                    alpha = col_alpha, 
                    add = TRUE)

# Dik-dik
clean_activity_plot(m15, 
                    species_title = "",
                    colour = col_high,
                    alpha = col_alpha)
title("F)", adj=0, line = 0.7)
clean_activity_plot(m16, 
                    colour = col_low,
                    alpha = col_alpha, 
                    add = TRUE)

# Giraffe
clean_activity_plot(m11, 
                    species_title = "",
                    colour = col_high,
                    alpha = col_alpha)
title("G)", adj=0, line = 0.7)
clean_activity_plot(m12, 
                    colour = col_low,
                    alpha = col_alpha, 
                    add = TRUE)

# Spotted hyena
clean_activity_plot(m13, 
                    species_title = "",
                    colour = col_high,
                    alpha = col_alpha)
title("H)", adj=0, line = 0.7)
clean_activity_plot(m14, 
                    colour = col_low,
                    alpha = col_alpha, 
                    add = TRUE)

dev.off()

# Activity analysis split by site ####
site_list <- unique(consensus_classifications$site)

key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "dikdik",
            "giraffe",
            "hyenaspotted")

results_list <- list()

for(s in 1:length(key_sp)){
  act_results <- matrix(NA, nrow=length(site_list), ncol = 4)
  for(i in 1:length(site_list)){
    df <- consensus_classifications %>% filter(species == key_sp[s])
    df_sub <- df %>% filter(site == site_list[i])
    if(nrow(df_sub) == 0){
      act_results[i,] <- 0
    }else{
      times1 <- df_sub$DateTimeLub
      t_rad1 <- gettime(x = times1,
                        scale = "radian")
      m1 <- fitact(dat = t_rad1,
                   sample = "data",
                   reps = 1000,
                   show = TRUE)
      #save(m1, file = paste0(key_sp[s],"_",site_list[i],".Rdata"))
      act_results[i,] <- m1@act
      rm(m1); rm(t_rad1); rm(times1)
    }
    results_list[[s]] <- act_results
  }
}

names(results_list) <- key_sp

# Save activity results for each site
setwd("E:/ch3_post_new/activity_analysis/activity_analysis_output")
save(results_list, file = "activity_sitelevel_all.Rdata")

# Load activity results for each site
setwd("E:/ch3_post_new/activity_analysis/activity_analysis_output")
results_list <- get(load("activity_sitelevel_all.Rdata"))

# Parameters for running model
key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "dikdik",
            "giraffe",
            "hyenaspotted")

model_list <- c("total_novegpath", "total_vegpath") # List of models to run
n_chains <- 4 # Number of chains
n_cores <- 4 # Number of computer cores
n_warmup <- 3000 # Number of warmup iterations per chain
n_iter <- 4000 # Total number of iterations (warmup + sample) per chain

# Fit hurdle model for each species - fine-scale
dmat <- generate_distance_matrix(site_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

for(m in 1:length(model_list)){
  setwd(paste0("E:/ch3_post_new/activity_analysis/activity_analysis_output/fine_scale/",model_list[m]))
  for(sp in 1:length(key_sp)){
    act_results <- results_list[key_sp[sp]]
    act_results <- as.data.frame(act_results)
    colnames(act_results) <- c("V1","V2","V3","V4")
    act_results <- cbind(site_list, act_results)
    
    act_results$Site_ID <- as.numeric(gsub("Site_", "", act_results$site_list))
    act_results2 <- merge(act_results, site_data, by="Site_ID", all.y = TRUE)
    
    act_results2$V1[is.na(act_results2$V1)] <- 0
    
    dlist <- list(
      n_obs = nrow(act_results2),
      y_obs = act_results2$V1*10,
      opuntia = standardize(act_results2$opuntia_total_cover),
      surveys = standardize(sitedays$Days),
      d_water = standardize(act_results2$dist_river),
      d_road = standardize(act_results2$dist_road),
      livestock = standardize(act_results2$livestock_proportion),
      grass = standardize(act_results2$grass_total),
      forb = standardize(act_results2$forb_total),
      shrub = standardize(act_results2$shrub_total),
      succulent = standardize(act_results2$succulent_total),
      tree = standardize(act_results2$n_trees),
      dmat = dmat)
    
    m1 <- cstan(file = paste0("C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/hurdle_test/hurdle_",model_list[m],".stan"),
                data = dlist,
                chains = n_chains, 
                cores = n_cores,
                warmup = n_warmup,
                iter = n_iter)
    
    # Save diagnostic plots
    png(file = paste0(key_sp[sp],"_",model_list[m],"_diagnostics.png"), width = 804, height = 500, units = "px")
    dashboard(m1)
    dev.off()
    
    # Save posterior samples
    post <- extract.samples(m1)
    save(post, file = paste0(key_sp[sp],"_hurdle_",model_list[m],".Rdata"))
    
    # Parameters to save in traceplots and trankplots
    p <- names(post)[grep("beta", names(post))] # Beta parameters
    p2 <- names(post)[grep("gamma", names(post))] # Gamma parameters
    p3 <- c("sigma", "etasq", "rhosq","etasq2", "rhosq2",  "k_bar","omega_bar") # Other parameters
    
    # Save traceplots for key parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_traceplots.png"), width = 804, height = 500, units = "px")
    p1 <- rstan::traceplot(m1, pars=c(p, p2, p3), inc_warmup = TRUE)
    print(p1)
    dev.off()
    
    # Save trankplots for key parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_trankplots.png"), width = 804, height = 500, units = "px")
    trankplot(m1, pars=p3)
    dev.off()
    
    # Save hist of centred marginal energy distribution and first-differenced distribution overlaid
    color_scheme_set("darkgray") # Set colour scheme for Bayesplot 
    np <- nuts_params(m1) # Extract NUTS parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_HMC_energy.png"), width = 804, height = 500, units = "px")
    p2 <- mcmc_nuts_energy(np)
    print(p2)
    dev.off()
    
    # Clean up between iterations
    rm(post)
    rm(np)
    rm(p1)
    rm(p2)
    rm(m1)
    rm(dlist)
    rm(act_results2)
    rm(act_results)
    gc()
  }
}

# Fit hurdle model for each species - grid square-scale
grid_data <- site_data %>% filter(!is.na(volume_total))
sitedays_grid <- sitedays %>% filter(Site %in% grid_data$Site_ID)

dmat <- generate_distance_matrix(grid_data, rescale = TRUE, rescale_constant = 6000, log = FALSE, jitter = FALSE)

for(m in 1:length(model_list)){
  setwd(paste0("E:/ch3_post_new/activity_analysis/activity_analysis_output/grid_square/",model_list[m]))
  for(sp in 1:length(key_sp)){
    act_results <- results_list[key_sp[sp]]
    act_results <- as.data.frame(act_results)
    colnames(act_results) <- c("V1","V2","V3","V4")
    act_results <- cbind(site_list, act_results)
    
    act_results$Site_ID <- as.numeric(gsub("Site_", "", act_results$site_list))
    act_results2 <- merge(act_results, site_data, by="Site_ID", all.y = TRUE)
    
    act_results2$V1[is.na(act_results2$V1)] <- 0
    
    act_results2$volume_total <- as.numeric(act_results2$volume_total)
    act_results2 <- act_results2[!is.na(act_results2$volume_total),]
    
    dlist <- list(
      n_obs = nrow(act_results2),
      y_obs = act_results2$V1*10,
      opuntia = standardize(act_results2$volume_total),
      surveys = standardize(sitedays_grid$Days),
      d_water = standardize(act_results2$dist_river),
      d_road = standardize(act_results2$dist_road),
      livestock = standardize(act_results2$livestock_proportion),
      grass = standardize(act_results2$grass_total),
      forb = standardize(act_results2$forb_total),
      shrub = standardize(act_results2$shrub_total),
      succulent = standardize(act_results2$succulent_total),
      tree = standardize(act_results2$n_trees),
      dmat = dmat)
    
    m1 <- cstan(file = paste0("C:/Users/PeteS/OneDrive/R Scripts Library/Stan_code/hurdle_test/hurdle_",model_list[m],".stan"),
                data = dlist,
                chains = n_chains, 
                cores = n_cores,
                warmup = n_warmup,
                iter = n_iter)
    
    # Save diagnostic plots
    png(file = paste0(key_sp[sp],"_",model_list[m],"_diagnostics.png"), width = 804, height = 500, units = "px")
    dashboard(m1)
    dev.off()
    
    # Save posterior samples
    post <- extract.samples(m1)
    save(post, file = paste0(key_sp[sp],"_hurdle_",model_list[m],".Rdata"))
    
    # Parameters to save in traceplots and trankplots
    p <- names(post)[grep("beta", names(post))] # Beta parameters
    p2 <- names(post)[grep("gamma", names(post))] # Gamma parameters
    p3 <- c("sigma", "etasq", "rhosq","etasq2", "rhosq2",  "k_bar","omega_bar") # Other parameters
    
    # Save traceplots for key parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_traceplots.png"), width = 804, height = 500, units = "px")
    p1 <- rstan::traceplot(m1, pars=c(p, p2, p3), inc_warmup = TRUE)
    print(p1)
    dev.off()
    
    # Save trankplots for key parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_trankplots.png"), width = 804, height = 500, units = "px")
    trankplot(m1, pars=p3)
    dev.off()
    
    # Save hist of centred marginal energy distribution and first-differenced distribution overlaid
    color_scheme_set("darkgray") # Set colour scheme for Bayesplot 
    np <- nuts_params(m1) # Extract NUTS parameters
    png(file = paste0(key_sp[sp],"_",model_list[m],"_HMC_energy.png"), width = 804, height = 500, units = "px")
    p2 <- mcmc_nuts_energy(np)
    print(p2)
    dev.off()
    
    # Clean up between iterations
    rm(post)
    rm(np)
    rm(p1)
    rm(p2)
    rm(m1)
    rm(dlist)
    rm(act_results2)
    rm(act_results)
    gc()
  }
}

# Hurdle model plots for all species in a loop ####
# Species names
key_sp <- c("baboon",
            "elephant",
            "vervetmonkey",
            "zebragrevys",
            "impala",
            "dikdik",
            "giraffe",
            "hyenaspotted")

plot_titles <- c("A)", "B)", "C)", "D)", "E)", "F)", "G)", "H)")

# Colours for shading CI's for each species
#species_colours <- viridis(7)
species_colours <- rep("#35B779FF", 8)
colouralpha <- 0.4

# Open new graphics device to save as TIFF
#par(mfrow=c(2,4))
#pr <- par()

# Fine scale
setwd("E:/ch3_post_new/activity_analysis/activity_analysis_output/fine_scale/total_novegpath")
tiff("hurdle_plots_fine_scale_novegpath.tiff", width = 15.83, height = 8.46, units = 'cm', res = 300)
par(pr)

# Plot loop
for(sp in 1:length(key_sp)){
  
  act_results <- results_list[key_sp[sp]]
  act_results <- as.data.frame(act_results)
  colnames(act_results) <- c("V1","V2","V3","V4")
  act_results <- cbind(site_list, act_results)
  
  act_results$Site_ID <- as.numeric(gsub("Site_", "", act_results$site_list))
  act_results2 <- merge(act_results, site_data, by="Site_ID", all.y = TRUE)
  
  act_results2$V1[is.na(act_results2$V1)] <- 0
  
  dlist <- list(
    n_obs = nrow(act_results2),
    y_obs = act_results2$V1*10,
    opuntia = standardize(act_results2$opuntia_total_cover),
    surveys = standardize(sitedays$Days),
    d_water = standardize(act_results2$dist_river),
    d_road = standardize(act_results2$dist_road),
    livestock = standardize(act_results2$livestock_proportion),
    grass = standardize(act_results2$grass_total),
    forb = standardize(act_results2$forb_total),
    shrub = standardize(act_results2$shrub_total),
    succulent = standardize(act_results2$succulent_total),
    tree = standardize(act_results2$n_trees),
    dmat = dmat
  )
  
  post <- get(load(paste0(key_sp[sp],"_hurdle_total_novegpath.Rdata")))
  
  x_seq <- seq(min(dlist$opuntia), max(dlist$opuntia), by = 0.01)
  y_sim <- matrix(NA, nrow = length(x_seq), ncol=length(post$beta_opuntia))
  for(i in 1:length(x_seq)){
    y_sim[i,] <- rbinom(n = length(post$beta_opuntia), 
                        size = 1,
                        prob = 1 - (inv_logit(post$omega_bar + post$gamma_opuntia*x_seq[i])))*rlnorm(n = length(post$beta_opuntia), 
                                                                                                     meanlog = post$k_bar + post$beta_opuntia*x_seq[i], 
                                                                                                     sdlog = post$sigma)
  }
  
  y_sim_mu <- apply(y_sim, 1, mean)
  y_sim_mu2 <- apply(y_sim, 1, median)
  y_sim_ci <- apply(y_sim, 1, HPDI, prob = 0.95)
  y_sim_ci2 <- apply(y_sim, 1, HPDI, prob = 0.89)
  y_sim_ci3 <- apply(y_sim, 1, HPDI, prob = 0.90)
  y_sim_ci4 <- apply(y_sim, 1, HPDI, prob = 0.80)
  y_sim_ci5 <- apply(y_sim, 1, HPDI, prob = 0.70)
  y_sim_ci6 <- apply(y_sim, 1, HPDI, prob = 0.60)
  y_sim_ci7 <- apply(y_sim, 1, HPDI, prob = 0.50)
  
  plot( NULL , xlim=range(x_seq) , ylim=c(0,max(dlist$y_obs+1)) , 
        xlab="Opuntia cover" , 
        ylab="Activity", 
        main = "")
  title(paste(plot_titles[sp]), adj=0, line = 0.7)
  #lines(x_seq, y_sim_mu, lwd=2, lty=2)
  shade(y_sim_ci, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci2, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci3, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci4, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci5, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci6, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci7, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  lines(x_seq, y_sim_mu2, lwd=2, lty=1)
  points(x = dlist$opuntia, y = dlist$y_obs, pch = 16)
}
dev.off() # Close graphics device

# Grid square
#pr <- par()
setwd("E:/ch3_post_new/activity_analysis/activity_analysis_output/grid_square/total_novegpath")
tiff("hurdle_plots_grid_square_novegpath.tiff", width = 15.83, height = 8.46, units = 'cm', res = 300)
par(pr)

# Plot loop
for(sp in 1:length(key_sp)){
  
  act_results <- results_list[key_sp[sp]]
  act_results <- as.data.frame(act_results)
  colnames(act_results) <- c("V1","V2","V3","V4")
  act_results <- cbind(site_list, act_results)
  
  act_results$Site_ID <- as.numeric(gsub("Site_", "", act_results$site_list))
  act_results2 <- merge(act_results, site_data, by="Site_ID", all.y = TRUE)
  
  act_results2$V1[is.na(act_results2$V1)] <- 0
  
  act_results2$volume_total <- as.numeric(act_results2$volume_total)
  act_results2 <- act_results2[!is.na(act_results2$volume_total),]
  
  dlist <- list(
    n_obs = nrow(act_results2),
    y_obs = act_results2$V1*10,
    opuntia = standardize(act_results2$volume_total),
    surveys = standardize(sitedays$Days),
    d_water = standardize(act_results2$dist_river),
    d_road = standardize(act_results2$dist_road),
    livestock = standardize(act_results2$livestock_proportion),
    grass = standardize(act_results2$grass_total),
    forb = standardize(act_results2$forb_total),
    shrub = standardize(act_results2$shrub_total),
    succulent = standardize(act_results2$succulent_total),
    tree = standardize(act_results2$n_trees),
    dmat = dmat
  )
  
  post <- get(load(paste0(key_sp[sp],"_hurdle_total_novegpath.Rdata")))
  
  x_seq <- seq(min(dlist$opuntia), max(dlist$opuntia), by = 0.01)
  y_sim <- matrix(NA, nrow = length(x_seq), ncol=length(post$beta_opuntia))
  for(i in 1:length(x_seq)){
    y_sim[i,] <- rbinom(n = length(post$beta_opuntia), 
                        size = 1,
                        prob = 1 - (inv_logit(post$omega_bar + post$gamma_opuntia*x_seq[i])))*rlnorm(n = length(post$beta_opuntia), 
                                                                                                     meanlog = post$k_bar + post$beta_opuntia*x_seq[i], 
                                                                                                     sdlog = post$sigma)
  }
  
  y_sim_mu <- apply(y_sim, 1, mean)
  y_sim_mu2 <- apply(y_sim, 1, median)
  y_sim_ci <- apply(y_sim, 1, HPDI, prob = 0.95)
  y_sim_ci2 <- apply(y_sim, 1, HPDI, prob = 0.89)
  y_sim_ci3 <- apply(y_sim, 1, HPDI, prob = 0.90)
  y_sim_ci4 <- apply(y_sim, 1, HPDI, prob = 0.80)
  y_sim_ci5 <- apply(y_sim, 1, HPDI, prob = 0.70)
  y_sim_ci6 <- apply(y_sim, 1, HPDI, prob = 0.60)
  y_sim_ci7 <- apply(y_sim, 1, HPDI, prob = 0.50)
  
  plot( NULL , xlim=range(x_seq) , ylim=c(0,max(dlist$y_obs+1)) , 
        xlab="Opuntia grid square vol." , 
        ylab="Activity", 
        main = "")
  title(paste(plot_titles[sp]), adj=0, line = 0.7)
  #lines(x_seq, y_sim_mu, lwd=2, lty=2)
  shade(y_sim_ci, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci2, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci3, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci4, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci5, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci6, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  shade(y_sim_ci7, x_seq, col = col.alpha(species_colours[sp],colouralpha))
  lines(x_seq, y_sim_mu2, lwd=2, lty=1)
  points(x = dlist$opuntia, y = dlist$y_obs, pch = 16)
}
dev.off() # Close graphics device

# Plot posterior distributions of beta_opuntia and gamma_opuntia for each species ####
setwd("E:/ch3_post_new/activity_analysis/activity_analysis_output/fine_scale/total_novegpath")

species_names <- c("Olive baboon",
                   "Elephant",
                   "Vervet monkey",
                   "Grevy's zebra",
                   "Impala", 
                   "Dik-dik",
                   "Giraffe",
                   "Spotted hyena")

tiff("fine_scale_total_novegpath_coefs.tiff", width = 15.83, height = 8.46, units = 'cm', res = 300)
par(pr)
par(mfrow=c(1,2), mar = c(3, 5, 2.1, 0.6))

plot(NULL, xlim = c(-1,1), ylim = c(1,length(key_sp)), xlab = expression(beta), ylab = "", yaxt = "n")
title("A)", adj=0, line = 0.7)
axis(2, at = 1:length(key_sp), labels = rep("",length(key_sp)), tick = TRUE, las = 2)
text(par("usr")[1] - 0.1, seq(1, length(key_sp), by = 1)+0.05, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = species_names, cex = 0.9)
abline(v = 0, lty = 2)
for(sp in 1:length(key_sp)){
  post <- get(load(paste0(key_sp[sp],"_hurdle_total_novegpath.Rdata")))
  
  beta_mu <- median(post$beta_opuntia)
  beta_ci1 <- HPDI(post$beta_opuntia, prob = 0.95)
  beta_ci2 <- HPDI(post$beta_opuntia, prob = 0.89)
  beta_ci3 <- HPDI(post$beta_opuntia, prob = 0.90)
  beta_ci4 <- HPDI(post$beta_opuntia, prob = 0.80)
  beta_ci5 <- HPDI(post$beta_opuntia, prob = 0.70)
  beta_ci6 <- HPDI(post$beta_opuntia, prob = 0.60)
  beta_ci7 <- HPDI(post$beta_opuntia, prob = 0.50)
  
  points(x = beta_mu, y = sp, pch=16, col="black")
  lines(x = c(beta_ci1[1], beta_ci1[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(beta_ci2[1], beta_ci2[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(beta_ci3[1], beta_ci3[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(beta_ci4[1], beta_ci4[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(beta_ci5[1], beta_ci5[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(beta_ci6[1], beta_ci6[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(beta_ci7[1], beta_ci7[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
}
plot(NULL, xlim = c(-2,2), ylim = c(1,length(key_sp)), xlab = expression(gamma), ylab = "", yaxt = "n")
title("B)", adj=0, line = 0.7)
axis(2, at = 1:length(key_sp), labels = rep("",length(key_sp)), tick = TRUE, las = 2)
text(par("usr")[1] - 0.2, seq(1, length(key_sp), by = 1)+0.05, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = species_names, cex = 0.9)
abline(v = 0, lty = 2)
for(sp in 1:length(key_sp)){
  post <- get(load(paste0(key_sp[sp],"_hurdle_total_novegpath.Rdata")))
  
  gamma_mu <- median(post$gamma_opuntia)
  gamma_ci1 <- HPDI(post$gamma_opuntia, prob = 0.95)
  gamma_ci2 <- HPDI(post$gamma_opuntia, prob = 0.89)
  gamma_ci3 <- HPDI(post$gamma_opuntia, prob = 0.90)
  gamma_ci4 <- HPDI(post$gamma_opuntia, prob = 0.80)
  gamma_ci5 <- HPDI(post$gamma_opuntia, prob = 0.70)
  gamma_ci6 <- HPDI(post$gamma_opuntia, prob = 0.60)
  gamma_ci7 <- HPDI(post$gamma_opuntia, prob = 0.50)
  
  points(x = gamma_mu, y = sp, pch=16, col="black")
  lines(x = c(gamma_ci1[1], gamma_ci1[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(gamma_ci2[1], gamma_ci2[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(gamma_ci3[1], gamma_ci3[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(gamma_ci4[1], gamma_ci4[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(gamma_ci5[1], gamma_ci5[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(gamma_ci6[1], gamma_ci6[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
  lines(x = c(gamma_ci7[1], gamma_ci7[2]), y = rep(sp,2), col = col.alpha("black", 0.3), lwd = 3)
}
dev.off()

