# all ses participants with FS meas: cortical.fs

cortical.fs <- read.csv('./derivatives/FS_SES_841_IMPUTE.csv') %>%
  rename_all(~ gsub('\\.', '-', .))
  
library(tidyverse)

# eTIV
eticv.model <- aov(EstimatedTotalIntraCranialVol ~ NEWSESGROUP + 
                     AGE + GENDER + HPT + DIABETES + HPL,
                   data = cortical.fs)
summary(eticv.model)
emmeans::emmeans(eticv.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')

# subcortical GMV
subgmv.model <- aov(SubCortGrayVol ~ NEWSESGROUP + 
                      AGE + GENDER + HPT + DIABETES + HPL,
                    data = cortical.fs)
summary(subgmv.model)
emmeans::emmeans(subgmv.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')

# cortex volume
cortex.model <- aov(CortexVol ~ NEWSESGROUP + 
                      AGE + GENDER + HPT + DIABETES + HPL,
                    data = cortical.fs)
summary(cortex.model)
emmeans::emmeans(cortex.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')

lhcortex.model <- aov(lhCortexVol ~ NEWSESGROUP + 
                        AGE + GENDER + HPT + DIABETES + HPL,
                      data = cortical.fs)
summary(lhcortex.model)
emmeans::emmeans(lhcortex.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')

rhcortex.model <- aov(rhCortexVol ~ NEWSESGROUP + 
                        AGE + GENDER + HPT + DIABETES + HPL,
                      data = cortical.fs)
summary(rhcortex.model)
emmeans::emmeans(rhcortex.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')


# cortical.fs is the final participants pool

# thickness ---------------------------------------------------------------

DKROI <- gsub('_thickness', '', DK)

thickness.model <- cortical.fs %>%
  dplyr::select(ends_with('thickness')) %>%
  map(~ aov(.x ~ SES + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = cortical.fs)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
thickness.model['ROI'] <- DKROI
thickness.model['padj'] <- p.adjust(thickness.model$`Pr(>F)`, method = 'fdr')
colnames(thickness.model)[2] = 'punadj'
thickness.results <- subset(thickness.model, padj < 0.05)


# area --------------------------------------------------------------------

areaROI <- names(area.model) %>%
  gsub('_area', '', .)

area.model <- cortical.fs %>%
  dplyr::select(ends_with('area')) %>%
  map(~ aov(.x ~ SES + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = cortical.fs)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
area.model['ROI'] <- areaROI
area.model['padj'] <- p.adjust(area.model$`Pr(>F)`, method = 'fdr')
colnames(area.model)[2] = 'punadj'
area.results <- subset(area.model, padj < 0.05)


# volume ------------------------------------------------------------------

volume.model <- cortical.fs %>%
  dplyr::select(ends_with('volume')) %>%
  map(~ aov(.x ~ SES + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = cortical.fs)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
volume.model['ROI'] <- DKROI
volume.model['padj'] <- p.adjust(volume.model$`Pr(>F)`, method = 'fdr')
colnames(volume.model)[2] = 'punadj'
volume.results <- subset(volume.model, padj < 0.05)

# aseg --------------------------------------------------------------------

library(ggseg)
plot(aseg)
asegROI <- names(cortical.fs)[238:276]
asegplot <- aseg$data

# 238-275 columns were aseg results
aseg.model <- cortical.fs %>%
  dplyr::select(`Left-Lateral-Ventricle`:CC_Anterior) %>%
  map(~ aov(.x ~ SES + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = cortical.fs)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
aseg.model['ROI'] <- asegROI
aseg.model['padj'] <- p.adjust(aseg.model$`Pr(>F)`, method = 'fdr')
colnames(aseg.model)[2] = 'punadj'
aseg.results <- subset(aseg.model, padj < 0.05)


# causal mediation: ses-brain-cog -----------------------------------------

install.packages('mediation')
library(mediation)

# ses-cortex volume-cognition mediation model

model.M <- lm(CortexVol ~ SES + AGE + GENDER + HPT + DIABETES + HPL,
              data = cortical.fs)
model.Y <- lm(WORKING ~ SES + CortexVol + AGE + GENDER + HPT + DIABETES + HPL,
              data = cortical.fs)
out <- mediate(model.m = model.M, model.y =  model.Y, 
               treat = 'SES', mediator = 'CortexVol', boot = TRUE, sims = 1000)
summary(out)
plot(out)

# mediations

# GENEGRAL
generaldata <- cortical.fs %>%
  filter(!is.na(GENERAL))

mediators.roi <- paste0(DKROI, '_volume')
covars.tmp <- c('AGE + GENDER + EstimatedTotalIntraCranialVol + HPT + DIABETES + HPL')
generaldata <- list(generaldata = generaldata)

# the mediations function, dataset have to be a list
med.dataset <- list(med.dataset = cortical.fs)



# general -----------------------------------------------------------------


med.general <- mediations(datasets = generaldata, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'GENERAL', covariates = covars.tmp,
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

write.csv(med.general.result, file = './derivatives/medresults_general.csv',
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
  
  med.general.result <- read.csv('./derivatives/medresults_general.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medresults_general.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.general.result <- read.csv('./derivatives/medresults_general.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
         )

# episodic ----------------------------------------------------------------

med.episodic <- mediations(datasets = med.dataset, treatment = 'SES', 
                           mediators = mediators.roi,
                           outcome = 'EPISODIC', covariates = covars.tmp,
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

write.csv(med.episodic.result, file = './derivatives/medresults_episodic.csv',
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
  
  med.episodic.result <- read.csv('./derivatives/medresults_episodic.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medresults_episodic.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.episodic.result <- read.csv('./derivatives/medresults_episodic.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# working -----------------------------------------------------------------


med.spatial <- mediations(datasets = med.dataset, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'WORKING', covariates = covars.tmp,
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

write.csv(med.spatial.result, file = './derivatives/medresults_working.csv',
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
  
  med.spatial.result <- read.csv('./derivatives/medresults_working.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medresults_working.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.spatial.result <- read.csv('./derivatives/medresults_working.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )



 # logic -------------------------------------------------------------------

med.logic <- mediations(datasets = med.dataset, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'LOGIC', covariates = covars.tmp,
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

write.csv(med.logic.result, file = './derivatives/medresults_logic.csv',
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
  
  med.logic.result <- read.csv('./derivatives/medresults_logic.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medresults_logic.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.logic.result <- read.csv('./derivatives/medresults_logic.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# spatial -----------------------------------------------------------------


med.spatial <- mediations(datasets = med.dataset, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'SPATIAL', covariates = covars.tmp,
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

write.csv(med.spatial.result, file = './derivatives/medresults_spatial.csv',
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
  
  med.spatial.result <- read.csv('./derivatives/medresults_spatial.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medresults_spatial.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.spatial.result <- read.csv('./derivatives/medresults_spatial.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )



# language ----------------------------------------------------------------

med.language <- mediations(datasets = med.dataset, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'LANGUAGE', covariates = covars.tmp,
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

write.csv(med.language.result, file = './derivatives/medresults_language.csv',
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
  
  med.language.result <- read.csv('./derivatives/medresults_language.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medresults_language.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.language.result <- read.csv('./derivatives/medresults_language.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# attention ---------------------------------------------------------------

med.attention <- mediations(datasets = med.dataset, treatment = 'SES', 
                          mediators = mediators.roi,
                          outcome = 'ATTENTION', covariates = covars.tmp,
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

write.csv(med.attention.result, file = './derivatives/medresults_attention.csv',
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
  
  med.attention.result <- read.csv('./derivatives/medresults_attention.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medresults_attention.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.attention.result <- read.csv('./derivatives/medresults_attention.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# executive ---------------------------------------------------------------


med.executive <- mediations(datasets = med.dataset, treatment = 'SES', 
                            mediators = mediators.roi,
                            outcome = 'EXECUTIVE', covariates = covars.tmp,
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

write.csv(med.executive.result, file = './derivatives/medresults_executive.csv',
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
  
  med.executive.result <- read.csv('./derivatives/medresults_executive.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medresults_executive.csv',
              row.names = FALSE)
  
}


# fdr correction of mediation results

med.executive.result <- read.csv('./derivatives/medresults_executive.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# cortex med plot ---------------------------------------------------------

# plot

med.working.plot <- tibble(label = dk$data$label) %>%
  left_join(., dplyr::select(med.working.result, 
                             roi, medeff, medp,
                             prop, propp),
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

med.working.plot %>%
  ggseg(mapping = aes(fill = prop*100),
        color = 'black', size = 0.65,
        position = 'dispersed',) +
  scale_fill_gradient2(low = "#F6D868CC", mid = "#E48551CC", high = "#A04543CC",
                       midpoint = mean(range(1.5, 5)),
                       breaks = seq(1.5, 5, 1),
                       limits = c(1.5, 5),
                       na.value = 'lightgrey') +
  # scale_fill_scico(palette = 'lajolla',
  #                  alpha = 0.8,
  #                  begin = 0.2,
  #                  end = 0.7,
  #                  direction = 1,
  #                  na.value = 'lightgrey') +
  labs(title = 'working function',
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

# aseg general -----------------------------------------------------------------

library(tidyverse)

med.aseg.dataset <- read.csv('./derivatives/FS_SES_841_IMPUTE.csv') %>%
  dplyr::select(ID:NEWSESGROUP, 
                Left.Lateral.Ventricle:CC_Anterior,
                EstimatedTotalIntraCranialVol)
mediators.aseg <- names(med.aseg.dataset)[34:72]
med.aseg.dataset <- list(med.aseg.dataset = med.aseg.dataset)



generaldata <- med.aseg.dataset %>%
  filter(!is.na(GENERAL))
generaldata <- list(generaldata = generaldata)

med.aseg.general <- mediations(datasets = generaldata, treatment = 'SES',
                               mediators = mediators.aseg,
                               outcome = 'GENERAL', covariates = covars.tmp,
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

write.csv(med.aseg.general.result, file = './derivatives/medaseg_general.csv',
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
  
  med.aseg.general.result <- read.csv('./derivatives/medaseg_general.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medaseg_general.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.general.result <- read.csv('./derivatives/medaseg_general.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )

# aseg episodic -----------------------------------------------------------

med.aseg.episodic <- mediations(datasets = med.aseg.dataset, treatment = 'SES',
                               mediators = mediators.aseg,
                               outcome = 'EPISODIC', covariates = covars.tmp,
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

write.csv(med.aseg.episodic.result, file = './derivatives/medaseg_episodic.csv',
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
  
  med.aseg.episodic.result <- read.csv('./derivatives/medaseg_episodic.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medaseg_episodic.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.episodic.result <- read.csv('./derivatives/medaseg_episodic.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg working ------------------------------------------------------------


med.aseg.working <- mediations(datasets = med.aseg.dataset, treatment = 'SES',
                                mediators = mediators.aseg,
                                outcome = 'WORKING', covariates = covars.tmp,
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

write.csv(med.aseg.working.result, file = './derivatives/medaseg_working.csv',
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
  
  med.aseg.working.result <- read.csv('./derivatives/medaseg_working.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medaseg_working.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.working.result <- read.csv('./derivatives/medaseg_working.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg logic --------------------------------------------------------------


med.aseg.logic <- mediations(datasets = med.aseg.dataset, treatment = 'SES',
                                mediators = mediators.aseg,
                                outcome = 'LOGIC', covariates = covars.tmp,
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

write.csv(med.aseg.logic.result, file = './derivatives/medaseg_logic.csv',
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
  
  med.aseg.logic.result <- read.csv('./derivatives/medaseg_logic.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medaseg_logic.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.logic.result <- read.csv('./derivatives/medaseg_logic.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg spatial ------------------------------------------------------------


med.aseg.spatial <- mediations(datasets = med.aseg.dataset, treatment = 'SES',
                                mediators = mediators.aseg,
                                outcome = 'SPATIAL', covariates = covars.tmp,
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

write.csv(med.aseg.spatial.result, file = './derivatives/medaseg_spatial.csv',
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
  
  med.aseg.spatial.result <- read.csv('./derivatives/medaseg_spatial.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medaseg_spatial.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.spatial.result <- read.csv('./derivatives/medaseg_spatial.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg language -----------------------------------------------------------


med.aseg.language <- mediations(datasets = med.aseg.dataset, treatment = 'SES',
                                mediators = mediators.aseg,
                                outcome = 'LANGUAGE', covariates = covars.tmp,
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

write.csv(med.aseg.language.result, file = './derivatives/medaseg_language.csv',
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
  
  med.aseg.language.result <- read.csv('./derivatives/medaseg_language.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medaseg_language.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.language.result <- read.csv('./derivatives/medaseg_language.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg attention ----------------------------------------------------------


med.aseg.attention <- mediations(datasets = med.aseg.dataset, treatment = 'SES',
                                mediators = mediators.aseg,
                                outcome = 'ATTENTION', covariates = covars.tmp,
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

write.csv(med.aseg.attention.result, file = './derivatives/medaseg_attention.csv',
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
  
  med.aseg.attention.result <- read.csv('./derivatives/medaseg_attention.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medaseg_attention.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.attention.result <- read.csv('./derivatives/medaseg_attention.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )


# aseg executive ----------------------------------------------------------


med.aseg.executive <- mediations(datasets = med.aseg.dataset, treatment = 'SES',
                                mediators = mediators.aseg,
                                outcome = 'EXECUTIVE', covariates = covars.tmp,
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

write.csv(med.aseg.executive.result, file = './derivatives/medaseg_executive.csv',
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
  
  med.aseg.executive.result <- read.csv('./derivatives/medaseg_executive.csv') %>%
    rbind(roi.results) %>%
    write.csv(file = './derivatives/medaseg_executive.csv',
              row.names = FALSE)
  
}

# fdr correction of mediation results

med.aseg.executive.result <- read.csv('./derivatives/medaseg_executive.csv') %>%
  mutate(medpfdr = p.adjust(medp, method = 'fdr'),
         proppfdr = p.adjust(propp, method = 'fdr')
  )



# subcortical med plot ----------------------------------------------------

med.aseg.general.plot <- med.aseg.general.result %>%
  mutate(roi = str_replace_all(roi, '\\.', '-')) %>%
  mutate(roi = ifelse(row_number() == 9, 'x3rd-ventricle', roi),
         roi = ifelse(row_number() == 10, 'x4th-ventricle', roi)) %>%
  left_join(tibble(label = aseg$data$label), ., by = c('label' = 'roi')) %>%
  dplyr::select(label, medeff, medp,
          prop, propp) %>%
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

med.aseg.general.plot %>%
  ggseg(atlas = 'aseg',
        mapping = aes(fill = prop*100),
        color = 'black', size = 0.65,
        position = 'dispersed',) +
  scale_fill_gradient2(low = "#F6D868CC", mid = "#E48551CC", high = "#A04543CC",
                       midpoint = mean(range(1, 8)),
                       breaks = seq(1, 8, 2),
                       limits = c(1, 8),
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
