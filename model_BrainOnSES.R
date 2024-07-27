# all ses participants with FS meas: cortical.fs

library(tidyverse)
library(ggseg)

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

# plot

thickness.plot <- tibble(label = dk$data$label) %>%
  left_join(., dplyr::select(thickness.results, ROI, 'F value'),
            by = c('label' = 'ROI')) %>%
  filter(!is.na(label)) %>%
  rename(f = `F value`) 
# mutate(f = ifelse(is.na(f), 0, f))

thickness.plot %>%
  ggseg(mapping = aes(fill = f),
        color = 'black', size = 0.65,
        position = 'stacked',) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1,
                   na.value = 'lightgrey') +
  labs(title = 'F value map of cortical thickness correlation with SES Score',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
       fill = 'F value') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 12))


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

# plot

area.plot <- tibble(label = dk$data$label) %>%
  left_join(., dplyr::select(area.results, ROI, 'F value'),
            by = c('label' = 'ROI')) %>%
  filter(!is.na(label)) %>%
  distinct() %>%
  rename(f = `F value`) 

area.plot %>%
  ggseg(mapping = aes(fill = f),
        color = 'black', size = 0.65,
        position = 'stacked',) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1,
                   na.value = 'lightgrey') +
  labs(title = 'F value map of cortical area correlation with SES Score',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
       fill = 'F value') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 12))

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

# plot
library(ggseg)
library(scico)

plot(dk)
dkplot <- dk$data

volume.plot <- tibble(label = dk$data$label) %>%
  left_join(., dplyr::select(volume.results, ROI, 'F value'),
            by = c('label' = 'ROI')) %>%
  filter(!is.na(label)) %>%
  distinct() %>%
  rename(f = `F value`) 
  # mutate(f = ifelse(is.na(f), 0, f))

volume.plot %>%
  ggseg(mapping = aes(fill = f),
        color = 'black', size = 0.65,
        position = 'stacked',) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1,
                   na.value = 'lightgrey') +
  labs(title = 'F value map of cortical volume correlation with SES Score',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
       fill = 'F value') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 12))

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

# plot
aseg.plot <- tibble(label = aseg$data$label) %>%
  left_join(., dplyr::select(aseg.results, ROI, 'F value'),
            by = c('label' = 'ROI')) %>%
  filter(!is.na(label)) %>%
  rename(f = `F value`) 
# mutate(f = ifelse(is.na(f), 0, f))

aseg.plot %>%
  ggseg(atlas = 'aseg',
        mapping = aes(fill = f),
        position = 'stacked', 
        color = 'black', size = 0.65) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1,
                   na.value = 'lightgrey') +
  labs(title = 'F value map of subcortical volume correlation with SES Score',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
       fill = 'F value') +
  theme_void() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 12))


# causal mediation: ses-brain-cog -----------------------------------------

install.packages('mediation')
install.packages('')
library(mediation)

# ses-cortex volume-cognition mediation model

workingdata <- cortical.fs %>%
  subset(complete.cases(WORKING))
model.M <- lm(CortexVol ~ SES + AGE + GENDER + HPT + DIABETES + HPL,
              data = cortical.fs)
model.M2 <- lm(INTELLECTURAL ~ SES + AGE + GENDER + HPT + DIABETES + HPL,
               data = cortical.fs)
model.2 <- c('model.M', 'model.M2')
model.Y <- lm(WORKING ~ SES + CortexVol + AGE + GENDER + HPT + DIABETES + HPL,
              data = cortical.fs)
out <- mediate(model.m = model.M2, model.y =  model.Y, 
               treat = 'SES', mediator = 'CortexVol', boot = TRUE, sims = 1000)
summary(out)
plot(out.s)

# mediations

mediators.tmp <- c('INTELLECTURAL', 'PHYSICAL', 'SOCIAL')
outcome.tmp <- c('EPISODIC', 'WORKING', 'LOGIC', 'SPATIAL', 
                 'LANGUAGE', 'ATTENTION', 'EXECUTIVE')
covars.tmp <- c('AGE + GENDER + HPT + DIABETES + HPL')
data.tmp <- list(cortical.fs = cortical.fs)
tmp <- mediations(datasets = data.tmp, treatment = 'SES', mediators = mediators.tmp,
           outcome = outcome.tmp, covariates = covars.tmp,
           conf.level = 0.95, sims = 1000)
tmp$EPISODIC.SES.INTELLECTURAL




