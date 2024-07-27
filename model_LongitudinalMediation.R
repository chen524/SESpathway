
# bind with wave2 ---------------------------------------------------------

library(tidyverse)
cortical.fs$ID = as.character(cortical.fs$ID)

cortical.long <- ses_wave2_data %>%
  dplyr::select(-contains('_1')) %>%
  dplyr::select(number, general_2:executive_2) %>%
  mutate(number = as.character(number)) %>%
  left_join(., cortical.fs, by = c('number' = 'ID')) %>%
  filter(!is.na(EstimatedTotalIntraCranialVol))

# missing data imputation -------------------------------------------------

library(mice)
summary(cortical.long[,2:9]) # check missing number of beh data
# general: 1
# em: 13
# wm: 4
# logical: 252
# spatial: 11
# language: 189
# attention: 18
# executive: 20

data.to.mice <- cortical.long %>%
  dplyr::select(general_2:executive_2) # cognitive domains have missing value for mice to do multiple impute

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
densityplot(tmpdata, ~ em_2 + logical_2 + spatial_2 + language_2 | .imp)
data.complete <- complete(tmpdata, action = 3)

# combine data: y and 8 cognitive domains
cortical.long <- as_tibble(cbind(
  data.complete, cortical.long[, c(1, 10:299)]))
  

write.csv(cortical.long, file = './derivatives/FS_SES_L_543_IMPUTE.csv',
          row.names = F)


# ses-leisure-volume med ----------------------------------------------------

library(lavaan)

# sacle intellectual and _volume data

columns.to.scale <- ends_with('_volumn') | between(seq_along(cortical.long, 246, 299))

cortical.long.scaled <- cortical.long %>%
  mutate_at(vars(ends_with('_volume'), 
                 'INTELLECTURAL', 'PHYSICAL', 'SOCIAL',
                 'Left-Lateral-Ventricle':'EstimatedTotalIntraCranialVol'), 
            scale) %>%
  write.csv(
          file = './derivatives/FS_SES_L_543_IMPUTE_SCALE.csv',
          row.names = F)

library(mediation)

cortical.long.scaled <- list(cortical.long.scaled = cortical.long.scaled)
mediators.leisure <- c('INTELLECTURAL', 'PHYSICAL', 'SOCIAL')

med <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                  mediators = mediators.leisure,
                  outcome = mediators.roi, covariates = covars.tmp,
                  conf.level = 0.95, sims = 1000)

## select each mediation coeff from models

# empty tibble of int
med.intellectual.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.intellectual.result, 
          file = './derivatives/medresults_intellectual.csv',
          row.names = FALSE)

# empty tibble of physical
med.physical.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.physical.result, 
          file = './derivatives/medresults_physical.csv',
          row.names = FALSE)

# empty tibble of social
med.social.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.social.result, 
          file = './derivatives/medresults_social.csv',
          row.names = FALSE)


for (i in 1:length(med)) {
  if(med[[i]]$mediator == 'INTELLECTURAL'){
    roi.results <- tibble(
      
      roi = DKROI[(i+2)/3],
      
      medeff = med[[i]]$d.avg,
      medlow = med[[i]]$d.avg.ci[[1]],
      medup = med[[i]]$d.avg.ci[[2]],
      medp = med[[i]]$d.avg.p,
      
      deff = med[[i]]$z.avg,
      dlow = med[[i]]$z.avg.ci[[1]],
      dup = med[[i]]$z.avg.ci[[2]],
      dp = med[[i]]$z.avg.p,
      
      teff = med[[i]]$tau.coef,
      tlow = med[[i]]$tau.ci[[1]],
      tup = med[[i]]$tau.ci[[2]],
      tp = med[[i]]$tau.p,
      
      prop = med[[i]]$n.avg,
      proplow = med[[i]]$n.avg.ci[[1]],
      propup = med[[i]]$d.avg.ci[[2]],
      propp = med[[i]]$d.avg.p
      
    ) 
    
    med.intellectual.result <- read.csv('./derivatives/medresults_intellectual.csv') %>%
      rbind(roi.results) %>%
      write.csv(file = './derivatives/medresults_intellectual.csv',
                row.names = FALSE)
  }
  
  else if(med[[i]]$mediator == 'PHYSICAL'){
    roi.results <- tibble(
      
      roi = DKROI[(i+1)/3],
      
      medeff = med[[i]]$d.avg,
      medlow = med[[i]]$d.avg.ci[[1]],
      medup = med[[i]]$d.avg.ci[[2]],
      medp = med[[i]]$d.avg.p,
      
      deff = med[[i]]$z.avg,
      dlow = med[[i]]$z.avg.ci[[1]],
      dup = med[[i]]$z.avg.ci[[2]],
      dp = med[[i]]$z.avg.p,
      
      teff = med[[i]]$tau.coef,
      tlow = med[[i]]$tau.ci[[1]],
      tup = med[[i]]$tau.ci[[2]],
      tp = med[[i]]$tau.p,
      
      prop = med[[i]]$n.avg,
      proplow = med[[i]]$n.avg.ci[[1]],
      propup = med[[i]]$d.avg.ci[[2]],
      propp = med[[i]]$d.avg.p
      
    ) 
    
    med.physical.result <- read.csv('./derivatives/medresults_physical.csv') %>%
      rbind(roi.results) %>%
      write.csv(file = './derivatives/medresults_physical.csv',
                row.names = FALSE)
  }
  
  else{
    roi.results <- tibble(
      
      roi = DKROI[(i/3)],
      
      medeff = med[[i]]$d.avg,
      medlow = med[[i]]$d.avg.ci[[1]],
      medup = med[[i]]$d.avg.ci[[2]],
      medp = med[[i]]$d.avg.p,
      
      deff = med[[i]]$z.avg,
      dlow = med[[i]]$z.avg.ci[[1]],
      dup = med[[i]]$z.avg.ci[[2]],
      dp = med[[i]]$z.avg.p,
      
      teff = med[[i]]$tau.coef,
      tlow = med[[i]]$tau.ci[[1]],
      tup = med[[i]]$tau.ci[[2]],
      tp = med[[i]]$tau.p,
      
      prop = med[[i]]$n.avg,
      proplow = med[[i]]$n.avg.ci[[1]],
      propup = med[[i]]$d.avg.ci[[2]],
      propp = med[[i]]$d.avg.p
      
    ) 
    
    med.social.result <- read.csv('./derivatives/medresults_social.csv') %>%
      rbind(roi.results) %>%
      write.csv(file = './derivatives/medresults_social.csv',
                row.names = FALSE)
  }
  
}

# fdr correction of mediation results

med.social.result <- read.csv('./derivatives/medresults_social.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# plot --------------------------------------------------------------------
# 
# med.scico <- scico(3, palette = 'lajolla', begin = 0.2, end = 0.7,
#                    alpha = 0.8, direction = 1)
# scico(3, palette = 'lajolla', begin = 0, end = 1,
#       alpha = 0.8, direction = 1)
# "#F6D868CC" "#E48551CC" "#A04543CC"


med.physical.plot <- tibble(label = dk$data$label) %>%
  left_join(., dplyr::select(med.physical.result, 
                             roi, medeff, medp, medpfdr, teff, tp,
                             prop, propp, proppfdr),
            by = c('label' = 'roi')) %>%
  filter(!is.na(label)) %>%
  subset(medp <= 0.05) %>%
  subset(prop > 0) %>%
  slice(rep(1:n(), times = 2)) %>%
  mutate(side = rep(c('lateral', 'medial'), 
                    each = nrow(.)/2))

# med.scico <- scico(3, palette = 'lajolla', begin = 0.2, end = 0.7, 
#                    alpha = 0.8, direction = 1)
# scico(3, palette = 'lajolla', begin = 0, end = 1, 
#       alpha = 0.8, direction = 1)
# "#F6D868CC" "#E48551CC" "#A04543CC"

# ses-intellectual-brain, with fdr results
med.intellectual.plot <- tibble(label = dk$data$label) %>%
  left_join(., dplyr::select(med.intellectual.result, 
                             roi, medeff, medp, medpfdr, teff, tp,
                             prop, propp, proppfdr),
            by = c('label' = 'roi')) %>%
  filter(!is.na(label)) %>%
  subset(medp <= 0.05 | tp <= 0.05) %>%
  subset(prop > 0)

med.social.plot %>%
  ggseg(mapping = aes(fill = prop*100),
        color = 'black', size = 0.65,
        position = 'dispersed',) +
  scale_fill_gradient2(low = "#F6D868CC", mid = "#E48551CC", high = "#A04543CC",
                       midpoint = mean(range(10, 90)),
                       breaks = seq(10, 90, 20),
                       limits = c(10, 90),
                       na.value = 'lightgrey') +
  # scale_fill_scico(palette = 'lajolla',
  #                  alpha = 0.8,
  #                  begin = 0.2,
  #                  end = 0.7,
  #                  direction = 1,
  #                  na.value = 'lightgrey') +
  labs(title = 'social support',
       fill = '% Mediated') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 12, hjust = 0),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, hjust = 0))


# ses-leisure-aseg med ----------------------------------------------------

# library(lavaan)
# sacle intellectual and _volume data

# columns.to.scale <- ends_with('_volumn') | between(seq_along(cortical.long, 246, 299))
# 
# cortical.long.scaled <- cortical.long %>%
#   mutate_at(vars(ends_with('_volume'), 'Left-Lateral-Ventricle':'EstimatedTotalIntraCranialVol'), 
#             scale)

# cortical.long$INTELLECTURAL ~ a1 * cortical.long$SES
# cortical.long$lh_bankssts_area ~ a2 * cortical.long$SES + d21 * cortical.long$INTELLECTURAL
# cortical.long$general_2 ~ cp * cortical.long$SES + b1 * cortical.long$INTELLECTURAL + b2 * cortical.long$lh_bankssts_area
# 
# indeff := a1 * d21 * b2
# 
# model <- '
# 
# INTELLECTURAL ~ a1 * SES
# lh_bankssts_area ~ a2 * SES + d21 * INTELLECTURAL
# general_2 ~ cp * SES + b1 * INTELLECTURAL + b2 * lh_bankssts_area
# 
# indeff := a1 * d21 * b2
# 
# '
# fit <- sem(model = model, data = cortical.long, se = 'boot', bootstrap = 1000)
# parameterEstimates(fit, boot.ci.type = 'bca.simple')

library(mediation)

# cortical.long.scaled <- list(cortical.long.scaled = cortical.long.scaled)
# mediators.leisure <- c('INTELLECTURAL', 'PHYSICAL', 'SOCIAL')

cortical.long.scaled <- read.csv('./derivatives/FS_SES_L_543_IMPUTE_SCALE.csv')
cortical.long.scaled <- list(cortical.long.scaled = cortical.long.scaled)

med.aseg <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                       mediators = mediators.leisure,
                       outcome = mediators.aseg, covariates = covars.tmp,
                       conf.level = 0.95, sims = 1000)

## select each mediation coeff from models

# empty tibble of int
med.aseg.intellectual.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.intellectual.result, 
          file = './derivatives/medaseg_intellectual.csv',
          row.names = FALSE)

# empty tibble of physical
med.aseg.physical.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.physical.result, 
          file = './derivatives/medaseg_physical.csv',
          row.names = FALSE)

# empty tibble of social
med.aseg.social.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.social.result, 
          file = './derivatives/medaseg_social.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg)) {
  if(med.aseg[[i]]$mediator == 'INTELLECTURAL'){
    roi.results <- tibble(
      
      roi = mediators.aseg[(i+2)/3],
      
      medeff = med.aseg[[i]]$d.avg,
      medlow = med.aseg[[i]]$d.avg.ci[[1]],
      medup = med.aseg[[i]]$d.avg.ci[[2]],
      medp = med.aseg[[i]]$d.avg.p,
      
      deff = med.aseg[[i]]$z.avg,
      dlow = med.aseg[[i]]$z.avg.ci[[1]],
      dup = med.aseg[[i]]$z.avg.ci[[2]],
      dp = med.aseg[[i]]$z.avg.p,
      
      teff = med.aseg[[i]]$tau.coef,
      tlow = med.aseg[[i]]$tau.ci[[1]],
      tup = med.aseg[[i]]$tau.ci[[2]],
      tp = med.aseg[[i]]$tau.p,
      
      prop = med.aseg[[i]]$n.avg,
      proplow = med.aseg[[i]]$n.avg.ci[[1]],
      propup = med.aseg[[i]]$d.avg.ci[[2]],
      propp = med.aseg[[i]]$d.avg.p
      
    ) 
    
    med.aseg.intellectual.result <- read.csv('./derivatives/medaseg_intellectual.csv') %>%
      rbind(roi.results) %>%
      write.csv(file = './derivatives/medaseg_intellectual.csv',
                row.names = FALSE)
  }
  
  else if(med.aseg[[i]]$mediator == 'PHYSICAL'){
    roi.results <- tibble(
      
      roi = mediators.aseg[(i+1)/3],
      
      medeff = med.aseg[[i]]$d.avg,
      medlow = med.aseg[[i]]$d.avg.ci[[1]],
      medup = med.aseg[[i]]$d.avg.ci[[2]],
      medp = med.aseg[[i]]$d.avg.p,
      
      deff = med.aseg[[i]]$z.avg,
      dlow = med.aseg[[i]]$z.avg.ci[[1]],
      dup = med.aseg[[i]]$z.avg.ci[[2]],
      dp = med.aseg[[i]]$z.avg.p,
      
      teff = med.aseg[[i]]$tau.coef,
      tlow = med.aseg[[i]]$tau.ci[[1]],
      tup = med.aseg[[i]]$tau.ci[[2]],
      tp = med.aseg[[i]]$tau.p,
      
      prop = med.aseg[[i]]$n.avg,
      proplow = med.aseg[[i]]$n.avg.ci[[1]],
      propup = med.aseg[[i]]$d.avg.ci[[2]],
      propp = med.aseg[[i]]$d.avg.p
      
    ) 
    
    med.aseg.physical.result <- read.csv('./derivatives/medaseg_physical.csv') %>%
      rbind(roi.results) %>%
      write.csv(file = './derivatives/medaseg_physical.csv',
                row.names = FALSE)
  }
  
  else{
    roi.results <- tibble(
      
      roi = mediators.aseg[(i/3)],
      
      medeff = med.aseg[[i]]$d.avg,
      medlow = med.aseg[[i]]$d.avg.ci[[1]],
      medup = med.aseg[[i]]$d.avg.ci[[2]],
      medp = med.aseg[[i]]$d.avg.p,
      
      deff = med.aseg[[i]]$z.avg,
      dlow = med.aseg[[i]]$z.avg.ci[[1]],
      dup = med.aseg[[i]]$z.avg.ci[[2]],
      dp = med.aseg[[i]]$z.avg.p,
      
      teff = med.aseg[[i]]$tau.coef,
      tlow = med.aseg[[i]]$tau.ci[[1]],
      tup = med.aseg[[i]]$tau.ci[[2]],
      tp = med.aseg[[i]]$tau.p,
      
      prop = med.aseg[[i]]$n.avg,
      proplow = med.aseg[[i]]$n.avg.ci[[1]],
      propup = med.aseg[[i]]$d.avg.ci[[2]],
      propp = med.aseg[[i]]$d.avg.p
      
    ) 
    
    med.aseg.social.result <- read.csv('./derivatives/medaseg_social.csv') %>%
      rbind(roi.results) %>%
      write.csv(file = './derivatives/medaseg_social.csv',
                row.names = FALSE)
  }
  
}

# fdr correction of mediation results

med.aseg.intellectual.result <- read.csv('./derivatives/medaseg_intellectual.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )

# aseg plot ---------------------------------------------------------------

med.aseg.social.plot <- med.aseg.social.result %>%
  mutate(roi = str_replace_all(roi, '\\.', '-')) %>%
  mutate(roi = ifelse(row_number() == 9, 'x3rd-ventricle', roi),
         roi = ifelse(row_number() == 10, 'x4th-ventricle', roi)) %>%
  left_join(tibble(label = aseg$data$label), ., by = c('label' = 'roi')) %>%
  dplyr::select(label, medeff, medp, medpfdr,
                prop, propp, proppfdr) %>%
  filter(!is.na(label)) %>%
  subset(medp <= 0.05)
# slice(rep(1:n(), times = 2)) %>%
# mutate(side = rep(c('lateral', 'medial'), 
#                   each = nrow(.)/2))

# med.scico <- scico(3, palette = 'lajolla', begin = 0.2, end = 0.7,
#                    alpha = 0.8, direction = 1)
# scico(3, palette = 'lajolla', begin = 0, end = 1,
#       alpha = 0.8, direction = 1)
# "#F6D868CC" "#E48551CC" "#A04543CC"

med.aseg.intellectual.plot %>%
  ggseg(atlas = 'aseg',
        mapping = aes(fill = prop*100),
        color = 'black', size = 0.65,
        position = 'dispersed',) +
  scale_fill_gradient2(low = "#F6D868CC", mid = "#E48551CC", high = "#A04543CC",
                       midpoint = mean(range(20, 60)),
                       breaks = seq(20, 60, 20),
                       limits = c(20, 60),
                       na.value = 'lightgrey') +
  # scale_fill_scico(palette = 'lajolla',
  #                  alpha = 0.8,
  #                  begin = 0.2,
  #                  end = 0.7,
  #                  direction = 1,
  #                  na.value = 'lightgrey') +
  labs(
    fill = '% Mediated') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_blank(),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, hjust = 0))

# ses-volume-cog_2 med ----------------------------------------------------


# general -----------------------------------------------------------------


med.general <- mediations(datasets = cortical.long.scaled, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'general_2', covariates = covars.tmp,
                          conf.level = 0.95, sims = 1000)

med.general.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.general.result, file = './derivatives/LongitudinalMediation/medresults_general.csv',
          row.names = FALSE)


for (i in 1:length(med.general)) {
  
  roi.results <- tibble(
    
    roi = DKROI[i],
    
    medeff = med.general[[i]]$d.avg,
    medlow = med.general[[i]]$d.avg.ci[[1]],
    medup = med.general[[i]]$d.avg.ci[[2]],
    medp = med.general[[i]]$d.avg.p,
    
    deff = med.general[[i]]$z.avg,
    dlow = med.general[[i]]$z.avg.ci[[1]],
    dup = med.general[[i]]$z.avg.ci[[2]],
    dp = med.general[[i]]$z.avg.p,
    
    teff = med.general[[i]]$tau.coef,
    tlow = med.general[[i]]$tau.ci[[1]],
    tup = med.general[[i]]$tau.ci[[2]],
    tp = med.general[[i]]$tau.p,
    
    prop = med.general[[i]]$n.avg,
    proplow = med.general[[i]]$n.avg.ci[[1]],
    propup = med.general[[i]]$d.avg.ci[[2]],
    propp = med.general[[i]]$d.avg.p
    
  ) 
  
  med.general.result <- read.csv('./derivatives/LongitudinalMediation/medresults_general.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medresults_general.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.general.result <- read.csv('./derivatives/LongitudinalMediation/medresults_general.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )

# episodic ----------------------------------------------------------------

med.episodic <- mediations(datasets = cortical.long.scaled, treatment = 'SES', 
                           mediators = mediators.roi,
                           outcome = 'em_2', covariates = covars.tmp,
                           conf.level = 0.95, sims = 1000)

med.episodic.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.episodic.result, file = './derivatives/LongitudinalMediation/medresults_episodic.csv',
          row.names = FALSE)


for (i in 1:length(med.episodic)) {
  
  roi.results <- tibble(
    
    roi = DKROI[i],
    
    medeff = med.episodic[[i]]$d.avg,
    medlow = med.episodic[[i]]$d.avg.ci[[1]],
    medup = med.episodic[[i]]$d.avg.ci[[2]],
    medp = med.episodic[[i]]$d.avg.p,
    
    deff = med.episodic[[i]]$z.avg,
    dlow = med.episodic[[i]]$z.avg.ci[[1]],
    dup = med.episodic[[i]]$z.avg.ci[[2]],
    dp = med.episodic[[i]]$z.avg.p,
    
    teff = med.episodic[[i]]$tau.coef,
    tlow = med.episodic[[i]]$tau.ci[[1]],
    tup = med.episodic[[i]]$tau.ci[[2]],
    tp = med.episodic[[i]]$tau.p,
    
    prop = med.episodic[[i]]$n.avg,
    proplow = med.episodic[[i]]$n.avg.ci[[1]],
    propup = med.episodic[[i]]$d.avg.ci[[2]],
    propp = med.episodic[[i]]$d.avg.p
    
  ) 
  
  med.episodic.result <- read.csv(
    './derivatives/LongitudinalMediation/medresults_episodic.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medresults_episodic.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.episodic.result <- read.csv(
  './derivatives/LongitudinalMediation/medresults_episodic.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# working -----------------------------------------------------------------


med.working <- mediations(datasets = cortical.long.scaled, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'wm_2', covariates = covars.tmp,
                          conf.level = 0.95, sims = 1000)

med.working.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.working.result, file = './derivatives/LongitudinalMediation/medresults_working.csv',
          row.names = FALSE)


for (i in 1:length(med.working)) {
  
  roi.results <- tibble(
    
    roi = DKROI[i],
    
    medeff = med.working[[i]]$d.avg,
    medlow = med.working[[i]]$d.avg.ci[[1]],
    medup = med.working[[i]]$d.avg.ci[[2]],
    medp = med.working[[i]]$d.avg.p,
    
    deff = med.working[[i]]$z.avg,
    dlow = med.working[[i]]$z.avg.ci[[1]],
    dup = med.working[[i]]$z.avg.ci[[2]],
    dp = med.working[[i]]$z.avg.p,
    
    teff = med.working[[i]]$tau.coef,
    tlow = med.working[[i]]$tau.ci[[1]],
    tup = med.working[[i]]$tau.ci[[2]],
    tp = med.working[[i]]$tau.p,
    
    prop = med.working[[i]]$n.avg,
    proplow = med.working[[i]]$n.avg.ci[[1]],
    propup = med.working[[i]]$d.avg.ci[[2]],
    propp = med.working[[i]]$d.avg.p
    
  ) 
  
  med.working.result <- read.csv('./derivatives/LongitudinalMediation/medresults_working.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medresults_working.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.working.result <- read.csv('./derivatives/LongitudinalMediation/medresults_working.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )



# logic -------------------------------------------------------------------

med.logic <- mediations(datasets = cortical.long.scaled, treatment = 'SES', 
                        mediators = mediators.roi,
                        outcome = 'logical_2', covariates = covars.tmp,
                        conf.level = 0.95, sims = 1000)

med.logic.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.logic.result, file = './derivatives/LongitudinalMediation/medresults_logic.csv',
          row.names = FALSE)


for (i in 1:length(med.logic)) {
  
  roi.results <- tibble(
    
    roi = DKROI[i],
    
    medeff = med.logic[[i]]$d.avg,
    medlow = med.logic[[i]]$d.avg.ci[[1]],
    medup = med.logic[[i]]$d.avg.ci[[2]],
    medp = med.logic[[i]]$d.avg.p,
    
    deff = med.logic[[i]]$z.avg,
    dlow = med.logic[[i]]$z.avg.ci[[1]],
    dup = med.logic[[i]]$z.avg.ci[[2]],
    dp = med.logic[[i]]$z.avg.p,
    
    teff = med.logic[[i]]$tau.coef,
    tlow = med.logic[[i]]$tau.ci[[1]],
    tup = med.logic[[i]]$tau.ci[[2]],
    tp = med.logic[[i]]$tau.p,
    
    prop = med.logic[[i]]$n.avg,
    proplow = med.logic[[i]]$n.avg.ci[[1]],
    propup = med.logic[[i]]$d.avg.ci[[2]],
    propp = med.logic[[i]]$d.avg.p
    
  ) 
  
  med.logic.result <- read.csv('./derivatives/LongitudinalMediation/medresults_logic.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medresults_logic.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.logic.result <- read.csv('./derivatives/LongitudinalMediation/medresults_logic.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# spatial -----------------------------------------------------------------


med.spatial <- mediations(datasets = cortical.long.scaled, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'spatial_2', covariates = covars.tmp,
                          conf.level = 0.95, sims = 1000)

med.spatial.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.spatial.result, file = './derivatives/LongitudinalMediation/medresults_spatial.csv',
          row.names = FALSE)


for (i in 1:length(med.spatial)) {
  
  roi.results <- tibble(
    
    roi = DKROI[i],
    
    medeff = med.spatial[[i]]$d.avg,
    medlow = med.spatial[[i]]$d.avg.ci[[1]],
    medup = med.spatial[[i]]$d.avg.ci[[2]],
    medp = med.spatial[[i]]$d.avg.p,
    
    deff = med.spatial[[i]]$z.avg,
    dlow = med.spatial[[i]]$z.avg.ci[[1]],
    dup = med.spatial[[i]]$z.avg.ci[[2]],
    dp = med.spatial[[i]]$z.avg.p,
    
    teff = med.spatial[[i]]$tau.coef,
    tlow = med.spatial[[i]]$tau.ci[[1]],
    tup = med.spatial[[i]]$tau.ci[[2]],
    tp = med.spatial[[i]]$tau.p,
    
    prop = med.spatial[[i]]$n.avg,
    proplow = med.spatial[[i]]$n.avg.ci[[1]],
    propup = med.spatial[[i]]$d.avg.ci[[2]],
    propp = med.spatial[[i]]$d.avg.p
    
  ) 
  
  med.spatial.result <- read.csv('./derivatives/LongitudinalMediation/medresults_spatial.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medresults_spatial.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.spatial.result <- read.csv('./derivatives/LongitudinalMediation/medresults_spatial.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )



# language ----------------------------------------------------------------

med.language <- mediations(datasets = cortical.long.scaled, treatment = 'SES', 
                           mediators = mediators.roi,
                           outcome = 'language_2', covariates = covars.tmp,
                           conf.level = 0.95, sims = 1000)

med.language.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.language.result, file = './derivatives/LongitudinalMediation/medresults_language.csv',
          row.names = FALSE)


for (i in 1:length(med.language)) {
  
  roi.results <- tibble(
    
    roi = DKROI[i],
    
    medeff = med.language[[i]]$d.avg,
    medlow = med.language[[i]]$d.avg.ci[[1]],
    medup = med.language[[i]]$d.avg.ci[[2]],
    medp = med.language[[i]]$d.avg.p,
    
    deff = med.language[[i]]$z.avg,
    dlow = med.language[[i]]$z.avg.ci[[1]],
    dup = med.language[[i]]$z.avg.ci[[2]],
    dp = med.language[[i]]$z.avg.p,
    
    teff = med.language[[i]]$tau.coef,
    tlow = med.language[[i]]$tau.ci[[1]],
    tup = med.language[[i]]$tau.ci[[2]],
    tp = med.language[[i]]$tau.p,
    
    prop = med.language[[i]]$n.avg,
    proplow = med.language[[i]]$n.avg.ci[[1]],
    propup = med.language[[i]]$d.avg.ci[[2]],
    propp = med.language[[i]]$d.avg.p
    
  ) 
  
  med.language.result <- read.csv('./derivatives/LongitudinalMediation/medresults_language.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medresults_language.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.language.result <- read.csv('./derivatives/LongitudinalMediation/medresults_language.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# attention ---------------------------------------------------------------

med.attention <- mediations(datasets = cortical.long.scaled, treatment = 'SES', 
                            mediators = mediators.roi,
                            outcome = 'attention_2', covariates = covars.tmp,
                            conf.level = 0.95, sims = 1000)

med.attention.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.attention.result, file = './derivatives/LongitudinalMediation/medresults_attention.csv',
          row.names = FALSE)


for (i in 1:length(med.attention)) {
  
  roi.results <- tibble(
    
    roi = DKROI[i],
    
    medeff = med.attention[[i]]$d.avg,
    medlow = med.attention[[i]]$d.avg.ci[[1]],
    medup = med.attention[[i]]$d.avg.ci[[2]],
    medp = med.attention[[i]]$d.avg.p,
    
    deff = med.attention[[i]]$z.avg,
    dlow = med.attention[[i]]$z.avg.ci[[1]],
    dup = med.attention[[i]]$z.avg.ci[[2]],
    dp = med.attention[[i]]$z.avg.p,
    
    teff = med.attention[[i]]$tau.coef,
    tlow = med.attention[[i]]$tau.ci[[1]],
    tup = med.attention[[i]]$tau.ci[[2]],
    tp = med.attention[[i]]$tau.p,
    
    prop = med.attention[[i]]$n.avg,
    proplow = med.attention[[i]]$n.avg.ci[[1]],
    propup = med.attention[[i]]$d.avg.ci[[2]],
    propp = med.attention[[i]]$d.avg.p
    
  ) 
  
  med.attention.result <- read.csv('./derivatives/LongitudinalMediation/medresults_attention.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medresults_attention.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.attention.result <- read.csv('./derivatives/LongitudinalMediation/medresults_attention.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# executive ---------------------------------------------------------------


med.executive <- mediations(datasets = cortical.long.scaled, treatment = 'SES', 
                            mediators = mediators.roi,
                            outcome = 'executive_2', covariates = covars.tmp,
                            conf.level = 0.95, sims = 1000)

med.executive.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.executive.result, file = './derivatives/LongitudinalMediation/medresults_executive.csv',
          row.names = FALSE)


for (i in 1:length(med.executive)) {
  
  roi.results <- tibble(
    
    roi = DKROI[i],
    
    medeff = med.executive[[i]]$d.avg,
    medlow = med.executive[[i]]$d.avg.ci[[1]],
    medup = med.executive[[i]]$d.avg.ci[[2]],
    medp = med.executive[[i]]$d.avg.p,
    
    deff = med.executive[[i]]$z.avg,
    dlow = med.executive[[i]]$z.avg.ci[[1]],
    dup = med.executive[[i]]$z.avg.ci[[2]],
    dp = med.executive[[i]]$z.avg.p,
    
    teff = med.executive[[i]]$tau.coef,
    tlow = med.executive[[i]]$tau.ci[[1]],
    tup = med.executive[[i]]$tau.ci[[2]],
    tp = med.executive[[i]]$tau.p,
    
    prop = med.executive[[i]]$n.avg,
    proplow = med.executive[[i]]$n.avg.ci[[1]],
    propup = med.executive[[i]]$d.avg.ci[[2]],
    propp = med.executive[[i]]$d.avg.p
    
  ) 
  
  med.executive.result <- read.csv('./derivatives/LongitudinalMediation/medresults_executive.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medresults_executive.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.executive.result <- read.csv('./derivatives/LongitudinalMediation/medresults_executive.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# plot --------------------------------------------------------------------


# plot

med.executive.plot <- tibble(label = dk$data$label) %>%
  left_join(., dplyr::select(med.executive.result, 
                             roi, medeff, medp, medpfdr,
                             prop, propp, proppfdr),
            by = c('label' = 'roi')) %>%
  filter(!is.na(label)) %>%
  subset(medp <= 0.05) %>%
  slice(rep(1:n(), times = 2)) %>%
  mutate(side = rep(c('lateral', 'medial'), 
                    each = nrow(.)/2))

# med.scico <- scico(3, palette = 'lajolla', begin = 0.2, end = 0.7, 
#                    alpha = 0.8, direction = 1)
# scico(3, palette = 'lajolla', begin = 0, end = 1, 
#       alpha = 0.8, direction = 1)
# "#F6D868CC" "#E48551CC" "#A04543CC"

med.executive.plot %>%
  ggseg(mapping = aes(fill = prop*100),
        color = 'black', size = 0.65,
        position = 'dispersed',) +
  scale_fill_gradient2(low = "#F6D868CC", mid = "#E48551CC", high = "#A04543CC",
                       midpoint = mean(range(2, 12)),
                       breaks = seq(2, 12, 3),
                       limits = c(2, 12),
                       na.value = 'lightgrey') +
  # scale_fill_scico(palette = 'lajolla',
  #                  alpha = 0.8,
  #                  begin = 0.2,
  #                  end = 0.7,
  #                  direction = 1,
  #                  na.value = 'lightgrey') +
  labs(title = 'executive function',
       fill = '% Mediated') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 12, hjust = 0),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, hjust = 0))

# ses-aseg-cog_2 med ------------------------------------------------------


# aseg general -----------------------------------------------------------------

# cortical.long.scaled <- read.csv('./derivatives/FS_SES_L_543_IMPUTE_SCALE.csv')
# cortical.long.scaled <- list(cortical.long.scaled = cortical.long.scaled)

med.aseg.general <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                               mediators = mediators.aseg,
                               outcome = 'general_2', covariates = covars.tmp,
                               conf.level = 0.95, sims = 1000)

med.aseg.general.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.general.result, file = './derivatives/LongitudinalMediation/medaseg_general.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg.general)) {
  
  roi.results <- tibble(
    
    roi = mediators.aseg[i],
    
    medeff = med.aseg.general[[i]]$d.avg,
    medlow = med.aseg.general[[i]]$d.avg.ci[[1]],
    medup = med.aseg.general[[i]]$d.avg.ci[[2]],
    medp = med.aseg.general[[i]]$d.avg.p,
    
    deff = med.aseg.general[[i]]$z.avg,
    dlow = med.aseg.general[[i]]$z.avg.ci[[1]],
    dup = med.aseg.general[[i]]$z.avg.ci[[2]],
    dp = med.aseg.general[[i]]$z.avg.p,
    
    teff = med.aseg.general[[i]]$tau.coef,
    tlow = med.aseg.general[[i]]$tau.ci[[1]],
    tup = med.aseg.general[[i]]$tau.ci[[2]],
    tp = med.aseg.general[[i]]$tau.p,
    
    prop = med.aseg.general[[i]]$n.avg,
    proplow = med.aseg.general[[i]]$n.avg.ci[[1]],
    propup = med.aseg.general[[i]]$d.avg.ci[[2]],
    propp = med.aseg.general[[i]]$d.avg.p
    
  ) 
  
  med.aseg.general.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_general.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medaseg_general.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.general.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_general.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )

# aseg episodic -----------------------------------------------------------

med.aseg.episodic <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                                mediators = mediators.aseg,
                                outcome = 'em_2', covariates = covars.tmp,
                                conf.level = 0.95, sims = 1000)

med.aseg.episodic.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.episodic.result, file = './derivatives/LongitudinalMediation/medaseg_episodic.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg.episodic)) {
  
  roi.results <- tibble(
    
    roi = mediators.aseg[i],
    
    medeff = med.aseg.episodic[[i]]$d.avg,
    medlow = med.aseg.episodic[[i]]$d.avg.ci[[1]],
    medup = med.aseg.episodic[[i]]$d.avg.ci[[2]],
    medp = med.aseg.episodic[[i]]$d.avg.p,
    
    deff = med.aseg.episodic[[i]]$z.avg,
    dlow = med.aseg.episodic[[i]]$z.avg.ci[[1]],
    dup = med.aseg.episodic[[i]]$z.avg.ci[[2]],
    dp = med.aseg.episodic[[i]]$z.avg.p,
    
    teff = med.aseg.episodic[[i]]$tau.coef,
    tlow = med.aseg.episodic[[i]]$tau.ci[[1]],
    tup = med.aseg.episodic[[i]]$tau.ci[[2]],
    tp = med.aseg.episodic[[i]]$tau.p,
    
    prop = med.aseg.episodic[[i]]$n.avg,
    proplow = med.aseg.episodic[[i]]$n.avg.ci[[1]],
    propup = med.aseg.episodic[[i]]$d.avg.ci[[2]],
    propp = med.aseg.episodic[[i]]$d.avg.p
    
  ) 
  
  med.aseg.episodic.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_episodic.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medaseg_episodic.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.episodic.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_episodic.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg working ------------------------------------------------------------


med.aseg.working <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                               mediators = mediators.aseg,
                               outcome = 'wm_2', covariates = covars.tmp,
                               conf.level = 0.95, sims = 1000)

med.aseg.working.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.working.result, file = './derivatives/LongitudinalMediation/medaseg_working.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg.working)) {
  
  roi.results <- tibble(
    
    roi = mediators.aseg[i],
    
    medeff = med.aseg.working[[i]]$d.avg,
    medlow = med.aseg.working[[i]]$d.avg.ci[[1]],
    medup = med.aseg.working[[i]]$d.avg.ci[[2]],
    medp = med.aseg.working[[i]]$d.avg.p,
    
    deff = med.aseg.working[[i]]$z.avg,
    dlow = med.aseg.working[[i]]$z.avg.ci[[1]],
    dup = med.aseg.working[[i]]$z.avg.ci[[2]],
    dp = med.aseg.working[[i]]$z.avg.p,
    
    teff = med.aseg.working[[i]]$tau.coef,
    tlow = med.aseg.working[[i]]$tau.ci[[1]],
    tup = med.aseg.working[[i]]$tau.ci[[2]],
    tp = med.aseg.working[[i]]$tau.p,
    
    prop = med.aseg.working[[i]]$n.avg,
    proplow = med.aseg.working[[i]]$n.avg.ci[[1]],
    propup = med.aseg.working[[i]]$d.avg.ci[[2]],
    propp = med.aseg.working[[i]]$d.avg.p
    
  ) 
  
  med.aseg.working.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_working.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medaseg_working.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.working.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_working.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg logic --------------------------------------------------------------


med.aseg.logic <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                             mediators = mediators.aseg,
                             outcome = 'logical_2', covariates = covars.tmp,
                             conf.level = 0.95, sims = 1000)

med.aseg.logic.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.logic.result, file = './derivatives/LongitudinalMediation/medaseg_logic.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg.logic)) {
  
  roi.results <- tibble(
    
    roi = mediators.aseg[i],
    
    medeff = med.aseg.logic[[i]]$d.avg,
    medlow = med.aseg.logic[[i]]$d.avg.ci[[1]],
    medup = med.aseg.logic[[i]]$d.avg.ci[[2]],
    medp = med.aseg.logic[[i]]$d.avg.p,
    
    deff = med.aseg.logic[[i]]$z.avg,
    dlow = med.aseg.logic[[i]]$z.avg.ci[[1]],
    dup = med.aseg.logic[[i]]$z.avg.ci[[2]],
    dp = med.aseg.logic[[i]]$z.avg.p,
    
    teff = med.aseg.logic[[i]]$tau.coef,
    tlow = med.aseg.logic[[i]]$tau.ci[[1]],
    tup = med.aseg.logic[[i]]$tau.ci[[2]],
    tp = med.aseg.logic[[i]]$tau.p,
    
    prop = med.aseg.logic[[i]]$n.avg,
    proplow = med.aseg.logic[[i]]$n.avg.ci[[1]],
    propup = med.aseg.logic[[i]]$d.avg.ci[[2]],
    propp = med.aseg.logic[[i]]$d.avg.p
    
  ) 
  
  med.aseg.logic.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_logic.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medaseg_logic.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.logic.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_logic.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg spatial ------------------------------------------------------------


med.aseg.spatial <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                               mediators = mediators.aseg,
                               outcome = 'spatial_2', covariates = covars.tmp,
                               conf.level = 0.95, sims = 1000)

med.aseg.spatial.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.spatial.result, file = './derivatives/LongitudinalMediation/medaseg_spatial.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg.spatial)) {
  
  roi.results <- tibble(
    
    roi = mediators.aseg[i],
    
    medeff = med.aseg.spatial[[i]]$d.avg,
    medlow = med.aseg.spatial[[i]]$d.avg.ci[[1]],
    medup = med.aseg.spatial[[i]]$d.avg.ci[[2]],
    medp = med.aseg.spatial[[i]]$d.avg.p,
    
    deff = med.aseg.spatial[[i]]$z.avg,
    dlow = med.aseg.spatial[[i]]$z.avg.ci[[1]],
    dup = med.aseg.spatial[[i]]$z.avg.ci[[2]],
    dp = med.aseg.spatial[[i]]$z.avg.p,
    
    teff = med.aseg.spatial[[i]]$tau.coef,
    tlow = med.aseg.spatial[[i]]$tau.ci[[1]],
    tup = med.aseg.spatial[[i]]$tau.ci[[2]],
    tp = med.aseg.spatial[[i]]$tau.p,
    
    prop = med.aseg.spatial[[i]]$n.avg,
    proplow = med.aseg.spatial[[i]]$n.avg.ci[[1]],
    propup = med.aseg.spatial[[i]]$d.avg.ci[[2]],
    propp = med.aseg.spatial[[i]]$d.avg.p
    
  ) 
  
  med.aseg.spatial.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_spatial.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medaseg_spatial.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.spatial.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_spatial.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg language -----------------------------------------------------------


med.aseg.language <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                                mediators = mediators.aseg,
                                outcome = 'language_2', covariates = covars.tmp,
                                conf.level = 0.95, sims = 1000)

med.aseg.language.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.language.result, file = './derivatives/LongitudinalMediation/medaseg_language.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg.language)) {
  
  roi.results <- tibble(
    
    roi = mediators.aseg[i],
    
    medeff = med.aseg.language[[i]]$d.avg,
    medlow = med.aseg.language[[i]]$d.avg.ci[[1]],
    medup = med.aseg.language[[i]]$d.avg.ci[[2]],
    medp = med.aseg.language[[i]]$d.avg.p,
    
    deff = med.aseg.language[[i]]$z.avg,
    dlow = med.aseg.language[[i]]$z.avg.ci[[1]],
    dup = med.aseg.language[[i]]$z.avg.ci[[2]],
    dp = med.aseg.language[[i]]$z.avg.p,
    
    teff = med.aseg.language[[i]]$tau.coef,
    tlow = med.aseg.language[[i]]$tau.ci[[1]],
    tup = med.aseg.language[[i]]$tau.ci[[2]],
    tp = med.aseg.language[[i]]$tau.p,
    
    prop = med.aseg.language[[i]]$n.avg,
    proplow = med.aseg.language[[i]]$n.avg.ci[[1]],
    propup = med.aseg.language[[i]]$d.avg.ci[[2]],
    propp = med.aseg.language[[i]]$d.avg.p
    
  ) 
  
  med.aseg.language.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_language.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medaseg_language.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.language.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_language.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg attention ----------------------------------------------------------


med.aseg.attention <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                                 mediators = mediators.aseg,
                                 outcome = 'attention_2', covariates = covars.tmp,
                                 conf.level = 0.95, sims = 1000)

med.aseg.attention.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.attention.result, file = './derivatives/LongitudinalMediation/medaseg_attention.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg.attention)) {
  
  roi.results <- tibble(
    
    roi = mediators.aseg[i],
    
    medeff = med.aseg.attention[[i]]$d.avg,
    medlow = med.aseg.attention[[i]]$d.avg.ci[[1]],
    medup = med.aseg.attention[[i]]$d.avg.ci[[2]],
    medp = med.aseg.attention[[i]]$d.avg.p,
    
    deff = med.aseg.attention[[i]]$z.avg,
    dlow = med.aseg.attention[[i]]$z.avg.ci[[1]],
    dup = med.aseg.attention[[i]]$z.avg.ci[[2]],
    dp = med.aseg.attention[[i]]$z.avg.p,
    
    teff = med.aseg.attention[[i]]$tau.coef,
    tlow = med.aseg.attention[[i]]$tau.ci[[1]],
    tup = med.aseg.attention[[i]]$tau.ci[[2]],
    tp = med.aseg.attention[[i]]$tau.p,
    
    prop = med.aseg.attention[[i]]$n.avg,
    proplow = med.aseg.attention[[i]]$n.avg.ci[[1]],
    propup = med.aseg.attention[[i]]$d.avg.ci[[2]],
    propp = med.aseg.attention[[i]]$d.avg.p
    
  ) 
  
  med.aseg.attention.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_attention.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medaseg_attention.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.attention.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_attention.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg executive ----------------------------------------------------------


med.aseg.executive <- mediations(datasets = cortical.long.scaled, treatment = 'SES',
                                 mediators = mediators.aseg,
                                 outcome = 'executive_2', covariates = covars.tmp,
                                 conf.level = 0.95, sims = 1000)

med.aseg.executive.result <- tibble(
  
  roi = character(),
  
  medeff = numeric(),
  medlow = numeric(),
  medup = numeric(),
  medp = numeric(),
  
  deff = numeric(),
  dlow = numeric(),
  dup = numeric(),
  dp = numeric(),
  
  teff = numeric(),
  tlow = numeric(),
  tup = numeric(),
  tp = numeric(),
  
  prop = numeric(),
  proplow = numeric(),
  propup = numeric(),
  propp = numeric()
  
)

write.csv(med.aseg.executive.result, file = './derivatives/LongitudinalMediation/medaseg_executive.csv',
          row.names = FALSE)


for (i in 1:length(med.aseg.executive)) {
  
  roi.results <- tibble(
    
    roi = mediators.aseg[i],
    
    medeff = med.aseg.executive[[i]]$d.avg,
    medlow = med.aseg.executive[[i]]$d.avg.ci[[1]],
    medup = med.aseg.executive[[i]]$d.avg.ci[[2]],
    medp = med.aseg.executive[[i]]$d.avg.p,
    
    deff = med.aseg.executive[[i]]$z.avg,
    dlow = med.aseg.executive[[i]]$z.avg.ci[[1]],
    dup = med.aseg.executive[[i]]$z.avg.ci[[2]],
    dp = med.aseg.executive[[i]]$z.avg.p,
    
    teff = med.aseg.executive[[i]]$tau.coef,
    tlow = med.aseg.executive[[i]]$tau.ci[[1]],
    tup = med.aseg.executive[[i]]$tau.ci[[2]],
    tp = med.aseg.executive[[i]]$tau.p,
    
    prop = med.aseg.executive[[i]]$n.avg,
    proplow = med.aseg.executive[[i]]$n.avg.ci[[1]],
    propup = med.aseg.executive[[i]]$d.avg.ci[[2]],
    propp = med.aseg.executive[[i]]$d.avg.p
    
  ) 
  
  med.aseg.executive.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_executive.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/LongitudinalMediation/medaseg_executive.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.executive.result <- read.csv('./derivatives/LongitudinalMediation/medaseg_executive.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )

# subcortical med plot ----------------------------------------------------

med.aseg.executive.plot <- med.aseg.executive.result %>%
  mutate(roi = str_replace_all(roi, '\\.', '-')) %>%
  mutate(roi = ifelse(row_number() == 9, 'x3rd-ventricle', roi),
         roi = ifelse(row_number() == 10, 'x4th-ventricle', roi)) %>%
  left_join(tibble(label = aseg$data$label), ., by = c('label' = 'roi')) %>%
  dplyr::select(label, medeff, medp, medpfdr,
                prop, propp, proppfdr) %>%
  filter(!is.na(label)) %>%
  subset(medp <= 0.05)
# slice(rep(1:n(), times = 2)) %>%
# mutate(side = rep(c('lateral', 'medial'), 
#                   each = nrow(.)/2))

# med.scico <- scico(3, palette = 'lajolla', begin = 0.2, end = 0.7,
#                    alpha = 0.8, direction = 1)
# scico(3, palette = 'lajolla', begin = 0, end = 1,
#       alpha = 0.8, direction = 1)
# "#F6D868CC" "#E48551CC" "#A04543CC"

med.aseg.logic.plot %>%
  ggseg(atlas = 'aseg',
        mapping = aes(fill = prop*100),
        color = 'black', size = 0.65,
        position = 'dispersed',) +
  scale_fill_gradient2(low = "#F6D868CC", mid = "#E48551CC", high = "#A04543CC",
                       midpoint = mean(range(2, 12)),
                       breaks = seq(2, 12, 3),
                       limits = c(2, 12),
                       na.value = 'lightgrey') +
  # scale_fill_scico(palette = 'lajolla',
  #                  alpha = 0.8,
  #                  begin = 0.2,
  #                  end = 0.7,
  #                  direction = 1,
  #                  na.value = 'lightgrey') +
  labs(
    fill = '% Mediated') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_blank(),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, hjust = 0))

# serial mediation --------------------------------------------------------

library(lavaan)
# cortical.long$INTELLECTURAL ~ a1 * cortical.long$SES
# cortical.long$lh_bankssts_area ~ a2 * cortical.long$SES + d21 * cortical.long$INTELLECTURAL
# cortical.long$general_2 ~ cp * cortical.long$SES + b1 * cortical.long$INTELLECTURAL + b2 * cortical.long$lh_bankssts_area
# 
# indeff := a1 * d21 * b2


# https://stats.stackexchange.com/questions/174257/sem-switching-order-of-indicators-in-latent-variable-definition-changes-results/174287#174287
# a very good answer of model identification


model <- '

  # latent variables
  med.cortex =~ lh_rostralmiddlefrontal_volume + rh_rostralmiddlefrontal_volume +
  lh_precentral_volume + lh_superiorparietal_volume
  
  # regression
  INTELLECTURAL ~ a1 * SES
  med.cortex ~ a2 * SES + d21 * INTELLECTURAL
  general_2 ~ cp * SES + b1 * INTELLECTURAL + b2 * med.cortex
  
  direct := cp
  ind1 := a1 * b1
  ind2 := a2 * b2
  inds := a1 * d21 * b2
  total := cp + ind1 + ind2 + inds

  
'


fit <- sem(model = model, data = cortical.long.scaled$cortical.long.scaled,
           se = 'boot', bootstrap = 1000)
summary(fit, ci = T)


# stacked plot of serial mediation ----------------------------------------

serialmed <- tibble(
  domain = rev(domainorder),
  Direct = c(0, 0, 0.155, 0.161, 0.125, 0.116, 0, 0),
  Indirect_1 = c(0.064, 0.064, 0.065, 0.048, 0.042, 0.059, 0.072, 0.073),
  Indirect_2 = c(0, 0, 0.023, 0, 0, 0.020, 0.021, 0.019),
  Indirect_3 = c(0.009, 0, 0.009, 0.007, 0, 0.008, 0.008, 0.008)
) %>%
  pivot_longer(-domain, names_to = 'effect', values_to = 'estimate')

serialmed$domain <- factor(serialmed$domain, 
                           levels = domainorder)

effect.cols <- c('grey', "#8DA0CB", "#E41A1C", "#4DAF4A")

serialmed %>%
  subset(domain %in% c('Episodic Memory', 'Working Memory',
                       'Language', 'Attention', 'Executive Function')) %>%
  ggplot(aes(x = domain, y = estimate, fill = effect)) +
  geom_bar(
           stat = 'identity', position = position_stack(
                                                        reverse = T),
           lwd = 1, color = 'white',
           width = 0.7) +
  scale_fill_manual(values = effect.cols,
                    labels = c('SES Direct Effect',
                               'Leisure activity Mediation Effect',
                               'Brain Mediation Effect',
                               'Serial Mediation Effect')) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(14),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 14),
    legend.text = element_text(size = 8)
    
  ) +
  coord_flip()



ind1 "#8DA0CB" 
ind2 "#E41A1C" "#377EB8" 
inds "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628" "#F781BF"

direct 'darkgrey'

