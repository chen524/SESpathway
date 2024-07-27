library(tidyverse)
library(stringr)

# gm.ses contains all participants with BNA data
# these image were processed by CAT12

# input FS meas -----------------------------------------------------------

# cortical.fs <- read_csv('./derivatives/CorticalMeas_fromFreeSurfer.csv',
#                         col_names = T)

# cortical.fs (n = 718) were the original data. Now I have 182 supplementary participants

# read supp fs meas (n = 126)

cortical.fs.sup <- 

# read multiple measurements
meas <- c('area', 'thickness', 'volume')
hemi <- c('lh', 'rh')

for (h in hemi) {
  for (m in meas) {
    # Construct the file name
    file.name <- paste0('./derivatives/aparc-', m, '-', h, '-stats.tsv')
    
    # Load the data from the file
    data <- read_delim(file.name)
    
    # Store the data in a separate data frame with a unique name
    assign(paste0(h, ".", m), data)
  }
}

aseg.volume <- read_delim('./derivatives/aseg-volume-stats.tsv')

# choose 34 features of each hemi of volume, cortical thickness, and ares
lh.area.new <- lh.area[, 1:35]
rh.area.new <- rh.area[, 1:35]
lh.thickness.new <- lh.thickness[, 1:35]
rh.thickness.new <- rh.thickness[, 1:35]
lh.volume.new <- lh.volume[, 1:35]
rh.volume.new <- rh.volume[, 1:35]

# choose aseg features according to Biological Psychiatry: CNNN (2022)
# no white matter measures
aseg.volume.new <- aseg.volume %>%
  select(!contains('hypointensities')) %>%
  select(!contains('SurfaceHoles'))

# merge all the measures into one file
area.new <- left_join(lh.area.new, rh.area.new, 
                      by = c('lh.aparc.area' = 'rh.aparc.area'),
                      keep = F)
thickness.new <- left_join(lh.thickness.new, rh.thickness.new, 
                           by = c('lh.aparc.thickness' = 'rh.aparc.thickness'),
                           keep = F)
volume.new <- left_join(lh.volume.new, rh.volume.new,
                        by = c('lh.aparc.volume' = 'rh.aparc.volume'),
                        keep = F)
aparc <- left_join(area.new, thickness.new, 
                   by = c('lh.aparc.area' = 'lh.aparc.thickness'),
                   keep = F) %>%
  left_join(., volume.new, by = c('lh.aparc.area' = 'lh.aparc.volume'),
            keep = F) 
fs.data.supp <- left_join(aparc, aseg.volume.new, 
                          by = c("lh.aparc.area" = 'Measure:volume'),
                          keep = F) %>%
  rename(mri = lh.aparc.area) %>%
  mutate(mri = sub("[^0-9]+", "", mri)) # extract the number of mri from sub-xx

cortical.fs <- read_csv('./derivatives/CorticalMeas_fromFreeSurfer.csv',
                        col_names = T) %>%
  select(-c(lhCerebralWhiteMatterVol, rhCerebralWhiteMatterVol, CerebralWhiteMatterVol))

# check if fs.data.supp and cortical.fs have same variables and bind them

if (identical(names(cortical.fs), names(fs.data.supp))) {
  # Row bind the tibbles if the variable names are identical
  cortical.all <- rbind(cortical.fs, fs.data.supp)
} else {
  # Handle the case where the variable names are not identical
  # Print an error message or perform alternative actions
}

write.csv(cortical.all, file = './derivatives/CorticalMeas_all900.csv',
           row.names = F)

# bind with ses -----------------------------------------------------------

cortical.ses <- cortical.ses %>%
  as_tibble() %>%
  mutate(ID = as.character(ID)) %>%
  mutate(SESGROUP = recode(SESGROUP,
                           '1' = 'Lowest',
                           '2' = 'Lower',
                           '3' = 'Medium',
                           '4' = 'Higher',
                           '5' = 'Highest'),
         GENDER = recode(GENDER,
                         '0' = 'Male',
                         '1' = 'Female'),
         NEWSESGROUP = recode(NEWSESGROUP,
                              '1' = 'Low',
                              '2' = 'High')) %>%
  arrange(ID)

write.csv(cortical.ses, file = './derivatives/FS_SES_841.csv',
          row.names = F)


# missing data imputation -------------------------------------------------

library(mice)
summary(cortical.ses[,7:32]) # check missing number of beh data

data.to.mice <- cortical.ses %>%
  dplyr::select(ID:GDS) # cognitive domains have missing value for mice to do multiple impute

md.pattern(data.to.mice) # check the feature of missing data

# multiple impute
tmpdata <- mice(data.to.mice, m = 5, maxit = 50, seed = 123)
summary(tmpdata) # PredictorMatrix is the predictor variable matrix
# the first line: general has missing data, use the other variavles "1" for imputation

tmpdata$imp$INTELLECTURAL # line94 was missing in general, five imputation give five values

# returen complete dataset
data.complete <- complete(tmpdata, action = 5) # action: choose which time of imputation
summary(data.complete)
densityplot(tmpdata) # blue is the original data, red are imputations

# choose imputations
densityplot(tmpdata, ~ 
              INTELLECTURAL + PHYSICAL + SOCIAL | .imp)
data.complete <- complete(tmpdata, action = 4)

# combine data: y and 8 cognitive domains
cortical.fs <- as_tibble(cbind(
  data.complete, cortical.ses[,33:291]))

write.csv(cortical.fs, file = './derivatives/FS_SES_841_IMPUTE.csv',
          row.names = F)

