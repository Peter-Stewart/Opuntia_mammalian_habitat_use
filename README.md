# Impacts of invasive *Opuntia* cacti on mammal habitat use

Code for reproducing the analyses in the manuscript "Multi-species impacts of invasive *Opuntia* cacti on mammal habitat use". 

Files are shown in **bold**, directories are shown in *italics*.

- **Zooniverse_processing.R** - main script for processing the Zooniverse classification data, creating output files which are subsequently used in the other scripts
  
- **helper_functions.R** - custom functions used in the other R scripts
    
- **accuracy_validation.R** - analysis of volunteer classification accuracy
    
- **analysis.R** - main analyses (occupancy, total detections, day vs. night detections) and code for supplementary figures S4-S7
    
- **distance_sampling.R** - distance sampling for estimating grid-square-level *Opuntia* density

- **figures.R** - figures 3-5 in main text

  **supp_figures.R** - supplementary figures S2 and S3

- **model_diagnostics.R** - model diagnostics for Stan models 
    
- *models* - Stan models
  - **day_night_detection_no_veg_path.stan** - model for effect of *Opuntia* on proportion of detections occurring at night, assuming no vegetation pathway
  - **day_night_detection_vegpath.stan** - model for effect of *Opuntia* on proportion of detections occurring at night, assuming vegetation pathway
  - **distance.stan** - distance sampling model for grid square-level *Opuntia* 
  - **occupancy_no_veg_path.stan** - model for effect of *Opuntia* on occupancy, assuming no vegetation pathway
  - **occupancy_vegpath.stan** - model for effect of *Opuntia* on occupancy, assuming vegetation pathway
  - **total_activity_no_veg_path.stan** - model for effect of *Opuntia* on total number of detections per day, assuming no vegetation pathway
  - **total_activity_vegpath.stan** - model for effect of *Opuntia* on total number of detections per day, assuming vegetation pathway


