library(tidyverse)
library(MatchIt)
library(ggseg)


# match -------------------------------------------------------------------

subsample.cortical <- read_csv('D:/Projects/SES_DTI/plots/subsample_agematched.csv',
                         col_names = T) %>%
  select(MRIIDNEW) %>%
  mutate(MRIIDNEW = as.character(MRIIDNEW)) %>%
  left_join(., cortical.ses, by = c('MRIIDNEW' = 'mri')) %>%
  filter(AGE >= 60)

subsample.gm <- read_csv('D:/Projects/SES_DTI/plots/subsample_agematched.csv',
                         col_names = T) %>%
  select(MRIIDNEW) %>%
  mutate(MRIIDNEW = as.character(MRIIDNEW)) %>%
  left_join(., gm.ses, by = c('MRIIDNEW' = 'MRI')) %>%
  filter(AGE >= 60)

ggplot(data = subsample.cortical, aes(x = AGE)) +
  geom_histogram(binwidth = 1, alpha = 0.75)
  
  
m.out <- matchit(as.factor(NEWSESGROUP) ~ GENDER, data = cortical.ses,
                 method = 'optimal', exact = ~ AGE,
                 distance = 'gam')
subsample <- match.data(m.out)

t.test(AGE~NEWSESGROUP, data = subsample.cortical)
t.test(AGE~NEWSESGROUP, data = newg_ses_wm_roi_data)


# area --------------------------------------------------------------------

subsample.area.model <- subsample.cortical %>%
 select(ends_with('area')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = subsample.cortical)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
subsample.area.model['ROI'] <- area.names
subsample.area.model['padj'] <- p.adjust(subsample.area.model$`Pr(>F)`, method = 'fdr')
colnames(subsample.area.model)[2] = 'punadj'
subsample.area.results <- subset(subsample.area.model, padj < 0.05)

subsample.area.names <- subsample.area.results$ROI

tmp <- subsample %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(all_of(subsample.area.results$ROI), 
           .fns = list(
             mean = mean,
             sd = sd),
           na.rm = T))

# area plot
area.subsample.plot.data <- tibble(hemi = rep(c('left', 'right'), 
                                       times = c(7, 6)),
                            region = c('caudal middle frontal', 'fusiform', 
                                       'middle temporal', 'precuneus', 
                                       'rostral middle frontal', 'superior frontal', 
                                       'superior temporal', 
                                       'fusiform', 'pars orbitalis', 
                                       'pars triangularis', 'rostral middle frontal',
                                       'superior temporal', 'supramarginal'
                            ),
                            f = subsample.area.results$`F value`
                            
)

area.subsample.plot.data %>%
  ggseg(mapping = aes(fill = f),
        color = 'black',
        position = 'stacked',) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(title = 'F value map of cortical area comparison between low SES group and high SES group',
       caption  = 'Participants age ≥60 and matched.\nAll comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
       fill = 'F value') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14, family = 'serif'),
        axis.text.x = element_text(size = 12, family = 'serif'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 14, family = 'serif'),
        axis.text.y = element_text(size = 12, family = 'serif'),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, family = 'serif', hjust = 0.5),
        plot.caption = element_text(size = 12, family = 'serif', hjust = 0),
        legend.text = element_text(size = 8, family = 'serif'),
        legend.title = element_text(size = 12, family = 'serif'))

# thickness ---------------------------------------------------------------

subsample.thickness.model <- subsample.cortical %>%
  select(ends_with('thickness')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = subsample.cortical)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
subsample.thickness.model['ROI'] <- DK
subsample.thickness.model['padj'] <- p.adjust(subsample.thickness.model$`Pr(>F)`, method = 'fdr')
colnames(subsample.thickness.model)[2] = 'punadj'
subsample.thickness.resuls <- subset(subsample.thickness.model, padj < 0.05)


# volume ------------------------------------------------------------------

subsample.volume.model <- subsample.cortical %>%
  select(ends_with('volume')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = subsample.cortical)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
subsample.volume.model['ROI'] <- DK
subsample.volume.model['padj'] <- p.adjust(subsample.volume.model$`Pr(>F)`, method = 'fdr')
colnames(subsample.volume.model)[2] = 'punadj'
subsample.volume.results <- subset(subsample.volume.model, padj < 0.05)

subsample.volume.names <- gsub('_thickness', '_volume', subsample.volume.results$ROI)

tmp <- subsample %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(all_of(subsample.volume.names), 
           .fns = list(
             mean = mean,
             sd = sd),
           na.rm = T))

# volume plot
volume.subsample.plot.data <- tibble(hemi = rep(c('left', 'right'), 
                                              times = c(3, 2)),
                                   region = c('caudal middle frontal', 'precuneus', 
                                              'superior temporal', 
                                              'rostral middle frontal', 'superior temporal'
                                   ),
                                   f = subsample.volume.results$`F value`
                                   
)

volume.subsample.plot.data %>%
  ggseg(mapping = aes(fill = f),
        color = 'black',
        position = 'stacked',) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(title = 'F value map of cortical volume comparison between low SES group and high SES group',
       caption  = 'Participants age ≥60 and matched.\nAll comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
       fill = 'F value') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14, family = 'serif'),
        axis.text.x = element_text(size = 12, family = 'serif'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 14, family = 'serif'),
        axis.text.y = element_text(size = 12, family = 'serif'),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, family = 'serif', hjust = 0.5),
        plot.caption = element_text(size = 12, family = 'serif', hjust = 0),
        legend.text = element_text(size = 8, family = 'serif'),
        legend.title = element_text(size = 12, family = 'serif'))


# volume from BNA ---------------------------------------------------------

subsample.gm.ancova.models <- subsample.gm %>%
  select(contains('_')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + TIV + HPT + DIABETES + HPL, 
            data = subsample.gm)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
# dvnames <- names(gm.ancova.models) # get dv names before map_dfr to get a list of 96 variables
subsample.gm.ancova.models['ROI'] <- dvnames # add variable 'dv names' to results data frame

subsample.gm.ancova.models['padj'] <- p.adjust(subsample.gm.ancova.models$`Pr(>F)`, method = 'fdr')
# add p_adjust as a variable to results data frame

colnames(subsample.gm.ancova.models)[2] = 'punadj'
subsample.gm.ancova.results <- subset(subsample.gm.ancova.models, padj < 0.05)
