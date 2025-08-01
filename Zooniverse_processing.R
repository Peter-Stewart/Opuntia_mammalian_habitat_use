# Load packages ####
library(dplyr)
library(tidyr)
library(lubridate)
library(jsonlite)

# Source helper functions ####
source("C:/temp/Zooniverse/Final/scripts/helper_functions.R", echo = FALSE)

# Load data ####
setwd("C:/temp/Zooniverse/Final")

subjects <- read.csv("prickly-pear-project-kenya-subjects.csv")
subjects <- subjects %>% filter(subject_set_id == 99701 | subject_set_id == 105787 | subject_set_id == 111085)

setwd("C:/temp/Zooniverse/Final/extracted")
survey_extractions <- read.csv("survey_extractor_extractions.csv")

# Process survey extractions ####
# Make count into an integer
survey_extractions$count <- rep(NA, nrow(survey_extractions))
survey_extractions$count <- as.integer(rep(NA, nrow(survey_extractions)))
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.1==1)] = 1L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.2==1)] = 2L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.3==1)] = 3L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.4==1)] = 4L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.5==1)] = 5L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.6==1)] = 6L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.7==1)] = 7L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.8==1)] = 8L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.9==1)] = 9L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.10==1)] = 10L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.1150==1)] = 11L
survey_extractions$count[which(survey_extractions$data.answers_howmanyindividualsofthisspecies.51==1)] = 51L

# Add variable indicating whether count is exact
survey_extractions$count_exact <- ifelse(survey_extractions$count > 10, FALSE, TRUE)

# Add index variable for "interacting with cactus"
survey_extractions$interacting <- as.integer(rep(NA, nrow(survey_extractions)))
survey_extractions$interacting[which(survey_extractions$data.answers_doyouseeanyindividualsinteractingwiththecactus.no==1)] = 0
survey_extractions$interacting[which(survey_extractions$data.answers_doyouseeanyindividualsinteractingwiththecactus.yes==1)] = 1

# Merge classification data with subject set data
survey_extractions <- survey_extractions %>% select(classification_id, 
                                                    user_id,
                                                    workflow_id,
                                                    task,
                                                    created_at,
                                                    subject_id,
                                                    data.choice,
                                                    count,
                                                    count_exact,
                                                    interacting)

subjects_sub <- subjects %>% select(subject_id, 
                                    metadata, 
                                    subject_set_id, 
                                    retired_at, 
                                    retirement_reason,
                                    classifications_count)

user_classifications <- merge(subjects_sub, survey_extractions, by="subject_id", all = TRUE)

# Remove subjects with no metadata / classification ID / subject set ID
user_classifications <- user_classifications %>% 
  filter(metadata != "NA") %>% 
  filter(classification_id != "NA") %>% 
  filter(subject_set_id != "NA")

# Rename data.choice to "species"
user_classifications$species <- as.factor(user_classifications$data.choice)
user_classifications <- user_classifications %>% select(-data.choice)

# Add missing metadata for Site_31_part1 (missing due to error when uploading to Zooniverse)
# First get subject ID's and image names for the images with missing metadata
missingIDs <- subjects %>% filter(grepl("Site_31_part1",metadata)) %>% select(subject_id, metadata)
missingIDs$metadata <- gsub('\"Filename\":\"','',missingIDs$metadata)
missingIDs$metadata <- gsub("[{}]", "" ,missingIDs$metadata)
missingIDs$metadata <- gsub('"', "" ,missingIDs$metadata)
missingIDs$File <- missingIDs$metadata
missingIDs <- missingIDs %>% select(-metadata)

# Then load the manifest csv containing the info for the metadata, and merge with the subject ID's
setwd("C:/temp/Zooniverse/Final/site_data")
manifest <- read.csv("Site_31_part1_TimelapseData.csv", header = TRUE)
fix <- merge(missingIDs, manifest, by="File")

# Finally, reconstruct metadata from the information in the manifest
fix$metadata <- paste('{\"Date\":\"',
                      fix$Date,
                      '\",\"File\":\"',
                      fix$File,
                      '\",\"Time\":\"',
                      fix$Time,
                      '\",\"#Empty\":\"FALSE\",\"Folder\":\"', # All are Empty == FALSE so just paste in directly
                      fix$Folder,
                      '\",\"#Animal\":\"TRUE\",\"#Person\":\"FALSE\",\"#DeleteFlag\":\"FALSE\",\"#ImageQuality\":\"Ok\",\"#RelativePath\":\"\"}' # These are the same for all images so just paste in directly
)
fix$metadata <- gsub(" ","",fix$metadata) # Remove spaces in the metadata

# Merge the corrected metadata into the user_classifications dataframe
for(i in 1:nrow(user_classifications)){
  if(user_classifications$subject_id[i] %in% fix$subject_id){
    user_classifications$metadata[i] <- fix$metadata[fix$subject_id == user_classifications$subject_id[i]]
  }
}


# Also merge into the subjects_sub data frame
for(i in 1:nrow(subjects_sub)){
  if(subjects_sub$subject_id[i] %in% fix$subject_id){
    subjects_sub$metadata[i] <- fix$metadata[fix$subject_id == subjects_sub$subject_id[i]]
  }
}

rm(fix)
rm(manifest)
rm(missingIDs)

# Add missing metadata for Site_80_part2 (missing due to error when uploading to Zooniverse)
# First get subject ID's and image names for the images with missing metadata
missingIDs <- subjects %>% filter(grepl("Site_80_part2",metadata)) %>% select(subject_id, metadata)
missingIDs$metadata <- gsub('\"Filename\":\"','',missingIDs$metadata)
missingIDs$metadata <- gsub("[{}]", "" ,missingIDs$metadata)
missingIDs$metadata <- gsub('"', "" ,missingIDs$metadata)
missingIDs$File <- missingIDs$metadata
missingIDs <- missingIDs %>% select(-metadata)

# Then load the manifest csv containing the info for the metadata, and merge with the subject ID's
setwd("C:/temp/Zooniverse/Final/site_data")
manifest <- read.csv("Site_80_part2_TimelapseData.csv", header = TRUE)
fix <- merge(missingIDs, manifest, by="File")

# Finally, reconstruct metadata from the information in the manifest
fix$metadata <- paste('{\"Date\":\"',
                      fix$Date,
                      '\",\"File\":\"',
                      fix$File,
                      '\",\"Time\":\"',
                      fix$Time,
                      '\",\"#Empty\":\"FALSE\",\"Folder\":\"', # All are Empty == FALSE so just paste in directly
                      fix$Folder,
                      '\",\"#Animal\":\"TRUE\",\"#Person\":\"FALSE\",\"#DeleteFlag\":\"FALSE\",\"#ImageQuality\":\"Ok\",\"#RelativePath\":\"\"}' # These are the same for all images so just paste in directly
)
fix$metadata <- gsub(" ","",fix$metadata) # Remove spaces in the metadata

# Merge the corrected metadata into the user_classifications dataframe
for(i in 1:nrow(user_classifications)){
  if(user_classifications$subject_id[i] %in% fix$subject_id){
    user_classifications$metadata[i] <- fix$metadata[fix$subject_id == user_classifications$subject_id[i]]
  }
}

# Also merge into the subjects_sub data frame
for(i in 1:nrow(subjects_sub)){
  if(subjects_sub$subject_id[i] %in% fix$subject_id){
    subjects_sub$metadata[i] <- fix$metadata[fix$subject_id == subjects_sub$subject_id[i]]
  }
}


# Split metadata into separate columns, drop non-useful info. Retain original metadata column too.
user_classifications <- user_classifications %>% separate(metadata, 
                                                          into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", 
                                                                   "k", "l", "m"),
                                                          sep = ":", 
                                                          remove = FALSE,
                                                          convert = FALSE,
                                                          extra = "warn",
                                                          fill = "warn") %>%
  separate(b, into = c("Date", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  separate(c, into = c("File", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  separate(f, into = c("Sec", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  select(-a, -g, -h, -i, -j, -k, -l, -m) %>%
  rename(Hour = d) %>%
  rename(Min = e) 


user_classifications$Date <- gsub('"','',user_classifications$Date)
user_classifications$Hour <- gsub('"','',user_classifications$Hour)
user_classifications$Sec <- gsub('"','',user_classifications$Sec)

user_classifications$site <- user_classifications$File
user_classifications$site <- gsub('"','',user_classifications$site)
user_classifications$site <- gsub('.jpg','',user_classifications$site)
user_classifications$site <- gsub('.{9}$','',user_classifications$site)
user_classifications$site <- gsub("_part.*",'',user_classifications$site)
user_classifications$site <- as.factor(user_classifications$site)

user_classifications$File <- gsub('"','',user_classifications$File)
user_classifications$File <- gsub('.jpg','',user_classifications$File)

# Split date into day, month, year columns (keep original)
user_classifications <- user_classifications %>% separate(Date, into = c("Day", "Month", "Year"), sep = "-", remove = FALSE, convert = FALSE)

# Convert month and year to required format
user_classifications$Month <- match(user_classifications$Month, month.abb)
user_classifications$Month2 <- as.character(user_classifications$Month)
user_classifications$Month2 <- ifelse(user_classifications$Month < 10, paste("0",user_classifications$Month2,sep=""), user_classifications$Month2)
user_classifications$Year <- as.integer(paste("20",user_classifications$Year, sep = ""))

user_classifications <- user_classifications %>% unite(DateNum, c("Year", "Month2", "Day"), sep = "-", remove = FALSE)
user_classifications <- user_classifications %>% unite(TimeNum, c("Hour", "Min", "Sec"), sep = ":", remove = FALSE)
user_classifications <- user_classifications %>% unite(DateTimeNum, c("DateNum", "TimeNum"), sep = " ", remove = FALSE)

# Create datetime column
user_classifications$DateTimeLub <- as_datetime(user_classifications$DateTimeNum)
user_classifications$ClassLub <- as_datetime(user_classifications$created_at)

# Repeat for subjects_sub dataframe ####
# Split metadata into separate columns, drop non-useful info. Retain original metadata column too.
subjects_sub <- subjects_sub %>% separate(metadata, 
                                          into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", 
                                                   "k", "l", "m"),
                                          sep = ":", 
                                          remove = FALSE,
                                          convert = FALSE,
                                          extra = "warn",
                                          fill = "warn") %>%
  separate(b, into = c("Date", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  separate(c, into = c("File", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  separate(f, into = c("Sec", NA), sep = ",", remove = TRUE, convert = FALSE) %>%
  select(-a, -g, -h, -i, -j, -k, -l, -m) %>%
  rename(Hour = d) %>%
  rename(Min = e) 

subjects_sub$Date <- gsub('"','',subjects_sub$Date)
subjects_sub$Hour <- gsub('"','',subjects_sub$Hour)
subjects_sub$Sec <- gsub('"','',subjects_sub$Sec)

subjects_sub$site <- subjects_sub$File
subjects_sub$site <- gsub('"','',subjects_sub$site)
subjects_sub$site <- gsub('.jpg','',subjects_sub$site)
subjects_sub$site <- gsub('.{9}$','',subjects_sub$site)
subjects_sub$site <- gsub("_part.*",'',subjects_sub$site)
subjects_sub$site <- as.factor(subjects_sub$site)

subjects_sub$File <- gsub('"','',subjects_sub$File)
subjects_sub$File <- gsub('.jpg','',subjects_sub$File)
subjects_sub$File <- gsub('.JPG','',subjects_sub$File)

# Split date into day, month, year columns (keep original)
subjects_sub <- subjects_sub %>% separate(Date, into = c("Day", "Month", "Year"), sep = "-", remove = FALSE, convert = FALSE)

# Convert month and year to required format
subjects_sub$Month <- match(subjects_sub$Month, month.abb)
subjects_sub$Month2 <- as.character(subjects_sub$Month)
subjects_sub$Month2 <- ifelse(subjects_sub$Month < 10, paste("0",subjects_sub$Month2,sep=""), subjects_sub$Month2)
subjects_sub$Year <- as.integer(paste("20",subjects_sub$Year, sep = ""))

subjects_sub <- subjects_sub %>% unite(DateNum, c("Year", "Month2", "Day"), sep = "-", remove = FALSE)
subjects_sub <- subjects_sub %>% unite(TimeNum, c("Hour", "Min", "Sec"), sep = ":", remove = FALSE)
subjects_sub <- subjects_sub %>% unite(DateTimeNum, c("DateNum", "TimeNum"), sep = " ", remove = FALSE)

# Create datetime column
subjects_sub$DateTimeLub <- as_datetime(subjects_sub$DateTimeNum)


# Date and time fixes ####
# Fieldseason 1 - manually correct using code below
# Dates/times are wrong for three sites where camera 15 was deployed, due to a fault with the camera
# Correct these using offset calculated from known deployment time

# Site 35 parts 1 and 2 (part 3 used cam 6 which is ok)
# timestamp = 2020-01-01 00:06:30
# true time = 2021-02-02 15:45:00
# offset = 34,443,510 seconds

errors <- user_classifications %>% filter(grepl("Site_35_part1|Site_35_part2",File))
correct <- user_classifications %>% filter(!grepl("Site_35_part1|Site_35_part2",File))

errors$DateTimeLub <- errors$DateTimeLub + 34443510 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

user_classifications <- rbind(errors, correct)

# Dates are wrong for Site 69
# timestamp = 2020-01-21 15:09:37
# true time = 2021-02-23 15:11:00
# offset = 34,473,683 seconds
rm(errors); rm(correct)

errors <- user_classifications %>% filter(grepl("Site_69_part1|Site_69_part2|Site_69_part3|Site_69_part4|Site_69_part5",File))
correct <- user_classifications %>% filter(!grepl("Site_69_part1|Site_69_part2|Site_69_part3|Site_69_part4|Site_69_part5",File))

errors$DateTimeLub <- errors$DateTimeLub + 34473683 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

user_classifications <- rbind(errors, correct)

# Dates are wrong for site 86 part 1
# timestamp = 2020-02-10 13:08:00
# true time = 2021-03-15 13:09:00
# offset = 34,473,660
rm(errors); rm(correct)

errors <- user_classifications %>% filter(grepl("Site_86_part1",File))
correct <- user_classifications %>% filter(!grepl("Site_86_part1",File))

errors$DateTimeLub <- errors$DateTimeLub + 34473660 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

user_classifications <- rbind(errors, correct)

# Dates are wrong for site 86 parts 2 to 3
# timestamp = 2020-01-01 00:01:29
# true time = 2021-03-19 11:55:00
# offset = 38,318,011

rm(errors); rm(correct)

errors <- user_classifications %>% filter(grepl("Site_86_part2|Site_86_part3",File))
correct <- user_classifications %>% filter(!grepl("Site_86_part2|Site_86_part3",File))

errors$DateTimeLub <- errors$DateTimeLub + 38318011 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

user_classifications <- rbind(errors, correct)

# Fieldseason 2 - many sites wrong due to battery removal between seasons
# source offset calculation script
source("C:/temp/Zooniverse/Final/scripts/Fieldseason2_datetime_offset_calculation.R", echo = FALSE)
season2_offset <- df %>% select(Site, Offset)
season2_offset$Site <- as.factor(paste0("Site_",season2_offset$Site))
setwd("C:/temp/Zooniverse/Final/site_data")
Fieldseason2_folders <- read.csv("Fieldseason2_folders.csv")

# Index variable for season
user_classifications$Folder <- gsub("_IMG.*", "", user_classifications$File)
user_classifications$Folder <- gsub("_RCNX.*", "", user_classifications$Folder)

user_classifications$season <- NA
user_classifications$season <- ifelse(user_classifications$Folder %in% Fieldseason2_folders$Folder, 2, 1)

season1 <- user_classifications %>% filter(season == 1)
season2 <- user_classifications %>% filter(season == 2)

# Correct season 2 datetimes using offset
season2 <- merge(season2, season2_offset, by.x = "site", by.y = "Site", all.x = TRUE)
season2$DateTimeLub <- season2$DateTimeLub + season2$Offset
season2 <- season2 %>% select(-Offset)

user_classifications <- rbind(season1, season2)

#hist(user_classifications$DateTimeLub, breaks=100) # Confirm that all date/times are now within the correct range

# Fixes for subjects_sub ####
# Dates/times are wrong for three sites where camera 15 was deployed, due to a fault with the camera
# Correct these using offset calculated from known deployment time

# Site 35 parts 1 and 2 (part 3 used cam 6 which is ok)
# timestamp = 2020-01-01 00:06:30
# true time = 2021-02-02 15:45:00
# offset = 34,443,510 seconds

rm(errors); rm(correct)
errors <- subjects_sub %>% filter(grepl("Site_35_part1|Site_35_part2",File))
correct <- subjects_sub %>% filter(!grepl("Site_35_part1|Site_35_part2",File))

errors$DateTimeLub <- errors$DateTimeLub + 34443510 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

subjects_sub <- rbind(errors, correct)

# Dates are wrong for Site 69
# timestamp = 2020-01-21 15:09:37
# true time = 2021-02-23 15:11:00
# offset = 34,473,683 seconds
rm(errors); rm(correct)

errors <- subjects_sub %>% filter(grepl("Site_69_part1|Site_69_part2|Site_69_part3|Site_69_part4|Site_69_part5",File))
correct <- subjects_sub %>% filter(!grepl("Site_69_part1|Site_69_part2|Site_69_part3|Site_69_part4|Site_69_part5",File))

errors$DateTimeLub <- errors$DateTimeLub + 34473683 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

subjects_sub <- rbind(errors, correct)

# Dates are wrong for site 86 part 1
# timestamp = 2020-02-10 13:08:00
# true time = 2021-03-15 13:09:00
# offset = 34,473,660
rm(errors); rm(correct)

errors <- subjects_sub %>% filter(grepl("Site_86_part1",File))
correct <- subjects_sub %>% filter(!grepl("Site_86_part1",File))

errors$DateTimeLub <- errors$DateTimeLub + 34473660 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

subjects_sub <- rbind(errors, correct)

# Dates are wrong for site 86 parts 2 to 3
# timestamp = 2020-01-01 12:01:29
# true time = 2021-03-19 11:55:00
# offset = 38,318,011

rm(errors); rm(correct)

errors <- subjects_sub %>% filter(grepl("Site_86_part2|Site_86_part3",File))
correct <- subjects_sub %>% filter(!grepl("Site_86_part2|Site_86_part3",File))

errors$DateTimeLub <- errors$DateTimeLub + 38318011 
errors$DateTimeLub <- as_datetime(errors$DateTimeLub)

subjects_sub <- rbind(errors, correct)

# Fieldseason 2 - many sites wrong due to battery removal between seasons
subjects_sub$Folder <- gsub("_IMG.*", "", subjects_sub$File)
subjects_sub$Folder <- gsub("_RCNX.*", "", subjects_sub$Folder)

# Index variable for season
subjects_sub$season <- NA
subjects_sub$season <- ifelse(subjects_sub$Folder %in% Fieldseason2_folders$Folder, 2, 1)

season1 <- subjects_sub %>% filter(season == 1)
season2 <- subjects_sub %>% filter(season == 2)

# Correct season 2 datetimes using offset
season2 <- merge(season2, season2_offset, by.x = "site", by.y = "Site", all.x = TRUE)
season2$DateTimeLub <- season2$DateTimeLub + season2$Offset
season2 <- season2 %>% select(-Offset)

subjects_sub <- rbind(season1, season2)

#hist(subjects_sub$DateTimeLub, breaks=100) # Confirm that all date/times are now within the correct range

# Generate consensus classifications ####
# Flag ID's made by expert users
expert_usernames <- c(1672000, 2293934)
user_classifications$gold <- ifelse(user_classifications$user_id %in% expert_usernames, 1, 0)

expert_verified <- user_classifications %>% filter(gold == 1)

# Validation set with expert vs. volunteer classifications on same images
validation_set <- user_classifications %>% filter(subject_id %in% expert_verified$subject_id)
validation_set <- rbind(validation_set, expert_verified)

# Separate the classifications made by the rest of the volunteers
user_classifications <- user_classifications %>% filter(subject_id %notin% expert_verified$subject_id)

# Calculate number of votes for each species in each subject
sp_votes <- user_classifications %>% group_by(subject_id) %>% count(species, name = "votes") 

# Calculate total number of times each subject has been classified - NB that Zooniverse "classifications_count" variable is NOT accurate
user_classifications$classification_id <- as.factor(user_classifications$classification_id)
total_classifications <- user_classifications %>% group_by(subject_id) %>% count(classification_id) %>% count(subject_id, name = "n_classifications")
colnames(total_classifications) <- c("subject_id","total_subject_classifications")

#check <- merge(user_classifications, total_classifications, by="subject_id")
#check$v <- check$total_subject_classifications - check$classifications_count
#check2 <- check %>% select(subject_id, classification_id, species, total_subject_classifications, classifications_count, v) %>% filter(check$v !=0)

sp_votes <- merge(sp_votes,total_classifications, all.x = TRUE)

# Calculate votes for a species as proportion of total classifications
sp_votes$votes_prop <- sp_votes$votes/sp_votes$total_subject_classifications

# Accept classification if votes exceed threshold, warn if close to threshold
consensus_threshold <- 0.66
warning_threshold <- 0.45

sp_votes$classification_accept <- ifelse(sp_votes$votes_prop >= consensus_threshold, TRUE, FALSE)
sp_votes$warning_disagreement <- ifelse(consensus_threshold > sp_votes$votes_prop & sp_votes$votes_prop >= warning_threshold, TRUE, FALSE)

# calculate information entropy of votes and warn if entropy is high 
entropy_threshold <- 1
entropy_calc <- function(x){
  -sum(x*log(x))
}

entropy <- sp_votes %>% select(subject_id, votes_prop)
entropy <- aggregate(entropy, by = list(entropy$subject_id), FUN = entropy_calc)
entropy <- entropy %>% select(Group.1, votes_prop)
colnames(entropy) <- c("subject_id","entropy")
entropy$entropy_warning <- ifelse(entropy$entropy > entropy_threshold, TRUE, FALSE)

sp_votes <- merge(sp_votes, entropy, by = "subject_id", all.x = TRUE)

# Subset accepted classifications and merge with site data - drop columns which aren't needed
# Only take images with 12 or more classifications - ones with fewer have been flagged "human" and need expert check
accepted <- sp_votes %>% filter(total_subject_classifications > 10) %>% filter(classification_accept==TRUE)

expert_verified <- expert_verified %>% select(subject_id, species, metadata, site, DateTimeLub)

consensus_classifications <- merge(accepted, subjects_sub, all=TRUE)
consensus_classifications <- consensus_classifications %>% select(
  subject_id,
  species,
  metadata,
  site,
  DateTimeLub
)

consensus_classifications <- rbind(consensus_classifications, expert_verified)

# Generate list of images where volunteers ID "interaction with cactus" ####

interactions_threshold <- 4 # Theshold for number of votes to appear in final list

# List of key words to search for in comments
key_words <- c("interact",
               "eating",
               "fruit",
               "feed",
               "forag")

# Number of times each image has been classified as containing interaction with cactus 
interactions <- user_classifications %>% filter(interacting == 1) %>% 
  group_by(subject_id) %>% 
  count(interacting, name = "votes") %>%
  select(-interacting)
interactions <- as.data.frame(interactions)

match_ids <- unique (grep(paste(key_words,collapse="|"), 
                          comments$comment_body, value=FALSE))

matches <- comments[match_ids,]

match_subjects <- matches$discussion_title
match_subjects <- gsub("Subject ","",match_subjects)
match_subjects <- as.data.frame(match_subjects)
colnames(match_subjects) <- "subject_id"


# Filter and merge with subject data
interactions <- interactions %>% filter(votes >= interactions_threshold)
interactions <- merge(interactions, subjects_sub, all.x =  TRUE)
interactions2 <- merge(match_subjects, subjects_sub, all.x = TRUE)
interactions2 <- interactions2 %>% filter(!is.na(metadata)) # Remove images with no metadata

# Generate list
interactions_filelist <- interactions
interactions_filelist$subfolder <- gsub("_IMG.*","",interactions_filelist$File)
interactions_filelist$subfolder <- gsub("_RCNX.*","",interactions_filelist$subfolder)
interactions_filelist$path <- paste0(interactions_filelist$site,"/",
                                     interactions_filelist$subfolder,"/",
                                     interactions_filelist$File)
interactions_filelist$path <- ifelse(grepl("RCNX", interactions_filelist$path, fixed = TRUE) == TRUE,
                                     paste0(interactions_filelist$path, ".JPG"),
                                     paste0(interactions_filelist$path, ".jpg"))
interactions_filelist <- interactions_filelist %>% select(path)

interactions2_filelist <- interactions2
interactions2_filelist$subfolder <- gsub("_IMG.*","",interactions2_filelist$File)
interactions2_filelist$subfolder <- gsub("_RCNX.*","",interactions2_filelist$subfolder)
interactions2_filelist$path <- paste0(interactions2_filelist$site,"/",
                                      interactions2_filelist$subfolder,"/",
                                      interactions2_filelist$File)
interactions2_filelist$path <- ifelse(grepl("RCNX", interactions2_filelist$path, fixed = TRUE) == TRUE,
                                      paste0(interactions2_filelist$path, ".JPG"),
                                      paste0(interactions2_filelist$path, ".jpg"))
interactions2_filelist <- interactions2_filelist %>% select(path)

interactions_filelist <- rbind(interactions_filelist, interactions2_filelist)
interactions_filelist <- unique(interactions_filelist)

interactions_filelist_old <- read.table("C:/temp/Zooniverse/Oct22/interactions_filelist.txt")

# Save the list
#setwd("C:/temp/Zooniverse/Final")
#write.table(interactions_filelist_update, file = "interactions_filelist_update.txt",
#            sep = "\t", col.names = FALSE, row.names = FALSE)

# Generate detection matrix for each species####
# Load starts/ends data - will need this so that days with no detections can be kept as zero
setwd("C:/temp/Zooniverse/Final/site_data")
startends1 <- read.csv("Fieldseason1_startends.csv")
startends2 <- read.csv("Fieldseason2_startends.csv")
startends1$Days <- as.integer(startends1$Days) + 1L # Add one or it doesn't count the deployment day!
startends2$Days <- as.integer(startends2$Days) + 1L # Add one or it doesn't count the deployment day!

# Get dates in correct format
startends1$Deploy_month <- ifelse(startends1$Deploy_month < 10, paste("0",startends1$Deploy_month,sep=""), startends1$Deploy_month)
startends1 <- startends1 %>% unite(Deploy_date_num, c("Deploy_year", "Deploy_month", "Deploy_day"), sep = "-", remove = FALSE)
startends1$Deploy_date_lub <- as_date(startends1$Deploy_date_num)

startends2$Deploy_month <- ifelse(startends2$Deploy_month < 10, paste("0",startends2$Deploy_month,sep=""), startends2$Deploy_month)
startends2 <- startends2 %>% unite(Deploy_date_num, c("Deploy_year", "Deploy_month", "Deploy_day"), sep = "-", remove = FALSE)
startends2$Deploy_date_lub <- as_date(startends2$Deploy_date_num)

user_classifications$Month2 <- as.character(user_classifications$Month)
user_classifications$Month2 <- ifelse(user_classifications$Month < 10, paste("0",user_classifications$Month2,sep=""), user_classifications$Month2)
user_classifications$Year <- as.integer(paste("20",user_classifications$Year, sep = ""))

user_classifications <- user_classifications %>% unite(DateNum, c("Year", "Month2", "Day"), sep = "-", remove = FALSE)

sitedays1 <- startends1 %>% select(Site, Days) %>% filter(!is.na(Days))
sitedays2 <- startends2 %>% select(Site, Days) %>% filter(!is.na(Days))

# Add 1k/2k to season1/2 site ID's to separate while still allowing easy identification
sitedays1$Site <- sitedays1$Site + 1000L
sitedays2$Site <- sitedays2$Site + 2000L

startends1$Site <- startends1$Site + 1000L
startends2$Site <- startends2$Site + 2000L

# Also apply this to the Site variable in consensus_classifications
consensus_classifications$site <- as.integer(gsub("Site_", "", consensus_classifications$site))
consensus_classifications$site <- ifelse(month(consensus_classifications$DateTimeLub) < 9, consensus_classifications$site + 1000L, consensus_classifications$site + 2000L)
consensus_classifications$site <- as.factor(paste0("Site_", consensus_classifications$site))

# Create (binary) detection matrix for each species 
sp_list <- unique(levels(consensus_classifications$species))

# First merge the startends / sitedays objects for the two seasons
startends1 <- startends1 %>% select(Site, Deploy_date_lub, Days)
startends2 <- startends2 %>% select(Site, Deploy_date_lub, Days)
startends <- rbind(startends1, startends2)
sitedays <- rbind(sitedays1, sitedays2)

# Ensure that startends/sitedays cover same sites
# This will remove sites where startends recorded but no data due to e.g. camera damage
startends <- startends %>% filter(Site %in% sitedays$Site)

# Create list of detection matrices
detmats <- list()
for(i in 1:length(sp_list)){
  detmats[[i]] <- generate_detection_matrix_days(sp=sp_list[i], binary=TRUE)
}
names(detmats) <- sp_list

# Save results ####
setwd("C:/temp/Zooniverse/Final/processed")

save(consensus_classifications, file = "consensus_classifications.Rdata")
save(detmats, file = "detmats.Rdata")
save(startends, file = "startends.Rdata")
save(sitedays, file = "sitedays.Rdata")
save(validation_set, file = "validation_set.Rdata")
