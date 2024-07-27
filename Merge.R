
# merge -------------------------------------------------------------------

install.packages('naniar')
library(tidyverse)
library(stringr)
library(naniar)
library(multcomp)
library(scico)
library(ggpubr)
detach(package:ggpubr, unload = T)
setwd('D:/Projects/SES_T1/')

# ses cognition and demographic data
ses.cog <- readxl::read_excel('D:/Projects/SES_GMV/SES_MRIID_Beh_BNU.xlsx',
                              sheet = 1,
                              col_names = T) %>%
  as.tibble() %>%
  replace_with_na_at(.vars = 'MCI',
                     condition = ~.x == 99) %>%
  select(!c(ID, ID1...3, BirthYear, OCC, INN, EDU, 
            GrayMatter,WhiteMatter, WHOLE, MRINum, MRIIDOld)) %>%
  rename_with(toupper) %>%
  rename(ID = ID1...1,
         MRIID = MRIIDNEW,
         HPT = HYPERTENTION,
         HPL = HYPERLIPEMIA,
         UCLA = UCLA_ALL,
         GDS = GDS_ALL) %>%
  mutate(NEWSESGROUP = ifelse(SESGROUP ==1 | SESGROUP == 2 | SESGROUP == 3, 
                              1, 2)) %>%
  mutate(across(where(is.integer), as.numeric)) %>% # most of the variables are numbers
  mutate_at(vars(GENDER, SESGROUP, NEWSESGROUP, HPT, DIABETES,
                 HPL, MCI, MARRIAGE, HOUSING, SMOKING, DRINKING, NEWSESGROUP), 
            as_factor)
ses.cog$MRIID <- gsub('[^0-9]', '', ses.cog$MRIID) %>%
  as.character()

# ses.cog$NEWSESGROUP <- factor(ses.cog$NEWSESGROUP,
#                               levels = c('1', '2'),
#                               labels = c('Low', 'High'))

# ses_cog_mri_data$MRIIDNew <- gsub('[^0-9]', '', ses_cog_mri_data$MRIIDNew) %>%
#   as.character()
# mutate(mriid = str_extract(ses_new_data$subid, '[[:digit:]]+')) 

# [[:digit:]] is any number 0 to 9
# + means the preceding item (in this case, a digit) will be matched one or more times

# ses tiv data from suppliment 1 (n = 231)
gm.sup1 <- list.files(path = './data/new_copy_smoothed',
                      pattern = '.nii',
                      all.files = T,
                      full.names = F) %>%
  as.tibble() %>%
  mutate(value = str_extract(gm.sup1$value, '[[:digit:]]+')) 
gm.sup1$value <- as.numeric(gm.sup1$value)
gm.sup1 <- arrange(gm.sup1, value)

tiv.sup1 <- read_delim('./data/TIV.txt', col_names = F) %>%
  select(X1:X4) %>%
  cbind(., gm.sup1$value) %>%
  rename(TIV = X1,
         GM = X2,
         WM = X3,
         CSF = X4,
         MRI = 'gm.sup1$value') %>%
  select(MRI, TIV)
tiv.sup1$MRI <- as.character(tiv.sup1$MRI)
tiv.sup1$TIV <- as.numeric(tiv.sup1$TIV)

# ses tiv data from zsk (n = 470)
gm.bna <- read_csv('./derivatives/GMV_BN_Atlas.csv', col_names = T) %>%
  as.tibble() %>%
  mutate(subid = str_extract(gm.bna$subid, '[[:digit:]]+'))
gm.zsk <- readxl::read_excel('D:/Projects/SES_GMV/ROIsignals0531.xlsx') %>%
  select(ID, TIV)
gm.zsk$ID <- as.character(gm.zsk$ID)

# combine 701 tiv and bna volumes
tiv.zsk <- anti_join(gm.bna, tiv.sup1, by = c('subid' = 'MRI')) %>%
  select(subid) %>%
  left_join(., gm.zsk, by = c('subid' = 'ID')) %>%
  rename(MRI = subid)
tiv.all <- rbind(tiv.sup1, tiv.zsk) %>%
  left_join(., gm.bna, by = c('MRI' = 'subid')) %>%
  mutate(TIV = TIV * 1000)

# read sup2 data
sub.sup2 <- readxl::read_excel('./data/ses_t1w_smoothed_supp/sub_sup2.xlsx')
tiv.sup2 <- read_delim('./data/ses_t1w_smoothed_supp/TIV.txt', col_names = F) %>%
  select(X1:X4) %>%
  cbind(., sub.sup2) %>%
  rename(TIV = X1,
         GM = X2,
         WM = X3,
         CSF = X4,
         MRI = sub
         ) %>%
  select(MRI, TIV)
tiv.sup2$MRI <- as.character(tiv.sup2$MRI)
tiv.sup2$TIV <- as.numeric(tiv.sup2$TIV)
gm.sup2 <- read_csv('./derivatives/GMV_BN_Atlas_Supp2.csv', col_names = T) %>%
  as.tibble() %>%
  mutate(subid = str_extract(gm.sup2$subid, '[[:digit:]]+'))
tiv.sup2 <- left_join(tiv.sup2, gm.sup2, by = c('MRI' = 'subid')) %>%
  mutate(TIV = TIV * 1000)
gm.all <- rbind(tiv.all, tiv.sup2) %>%
  arrange(MRI)

# combine ses-cog data and gm data (n = 841)

gm.ses <- left_join(gm.all, ses.cog, by = c('MRI' = 'MRIID')) %>%
  select(ID, GENDER, AGE, NEWSESGROUP, SES:GDS, MRI, TIV, SFG_L_7_1:Tha_R_8_8) %>%
  as.tibble() %>%
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
                             '2' = 'High'))
        # HPT = recode(HPT,
        #              '0' = 'non-affected',
        #              '1' = 'affected'),
        # DIABETES = recode(DIABETES,
        #                   '0' = 'non-affected',
        #                   '1' = 'affected'),
        # HPL = recode(HPL,
        #              '0' = 'non-affected',
        #              '1' = 'affected'),
        # MCI = recode(MCI,
        #              '0' = 'non-MCI',
        #              '1' = 'MCI'),
        # MARRIAGE = recode(MARRIAGE,
        #                   '0' = 'single, divorced, or widowed',
        #                   '1' = 'having a couple' ),
        # HOUSING = recode(HOUSING,
        #                  '0' = 'others',
        #                  '1' = 'house-owner'),
        # SMOKING = recode(SMOKING,
        #                  '0' = 'non-smoker',
        #                  '1' = 'current-smoker'),
        # DRINKING = recode(DRINKING,
        #                   '0' = 'non-drinker',
        #                   '1' = 'current-drinker')
                              # )
write.csv(gm.ses, file = './derivatives/GMV_SES_BNA.csv',
          row.names = F)

# glm and fdr correction --------------------------------------------------

gm.ses.60 <- filter(gm.ses, AGE >= 60)
gm.ancova.models <- gm.ses.60 %>%
  select(contains('_')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + TIV + HPT + DIABETES + HPL, 
            data = gm.ses.60)) %>%
  # ano.models[[3]] is the model of precentral_l
  # summary(ano.model[[3]]) gets the p value of each inexpedient variables
  # a <- summary(anv.models[[3]])
  # a is a list of 1, a[[1]] is the summary results, a[[1]][1] is the first column: DF, of all IV
  # a [[1]][4, 4:5] is the F value and Pr(>F) of sesgroup (the 4th IV in the list)
  # map summary function to all elements of aov.models
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
# dvnames <- names(gm.ancova.models) # get dv names before map_dfr to get a list of 96 variables
gm.ancova.models['ROI'] <- dvnames # add variable 'dv names' to results data frame

gm.ancova.models['padj'] <- p.adjust(gm.ancova.models$`Pr(>F)`, method = 'fdr')
# add p_adjust as a variable to results data frame

colnames(gm.ancova.models)[2] = 'punadj'
gm.ancova.results <- subset(gm.ancova.models, padj < 0.05)

# TIV comparison and post-hoc comparison
tiv.model <- aov(TIV ~ NEWSESGROUP + AGE + GENDER + HPT + DIABETES + HPL,
                 data = gm.ses.60)
summary(tiv.model)
posth <- glht(tiv.model, linfct = mcp(NEWSESGROUP = 'Tukey'))
summary(posth)
emmeans::emmeans(tiv.model, specs = pairwise ~ NEWSESGROUP:GENDER, adjust = 'Tukey')

postHocs <- glht(newg_ancova, linfct = mcp(NEWSESGROUP = c('High - Low = 0')))
summary(postHocs)
# emmeans and glht are the same


# BNA246 nested in Yeo7 ---------------------------------------------------

bna.yeo <- read_csv('./atlas/subregion_func_network_Yeo_updated.csv') %>%
  select(Label:Yeo_17network)

for (i in 1:7){
  
  # get regions in network: yeo.i
  temp.yeo <- bna.yeo %>%
    filter(bna.yeo$Yeo_7network == i)
  
  # get gm+ses dataset in network: gm.yeo.i
  tmp.gm.yeo <- gm.ses %>%
    select(temp.yeo$region) %>%
    cbind(gm.ses[,1:34], .)
  
  # models for all regions in gm.yeo.i
  temp.yeo.models <- tmp.gm.yeo %>%
    select(contains('_')) %>%
    map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + TIV, 
              data = tmp.gm.yeo)) %>%
    map(summary) %>%
    map_dfr(~ as.data.frame(.[[1]][1,4:5]))
  temp.yeo.models['ROI'] <- temp.yeo$region
  temp.yeo.models['padj'] <- p.adjust(temp.yeo.models$`Pr(>F)`, method = 'fdr')
  colnames(temp.yeo.models)[2] = 'punadj'
  
  temp.yeo.models.name <- paste0(c('yeo', i, 'models'), 
                        collapse = '.')
  assign(paste0(c('yeo', i, 'models'), 
                collapse = '.'), temp.yeo.models)
  
  temp.yeo.results <- subset(temp.yeo.models, punadj < 0.05)
  assign(paste0(c('yeo', i, 'results'), 
                collapse = '.'), temp.yeo.results)
  


}


# yeo 17 network ----------------------------------------------------------

for (i in 1:17){
  
  # get regions in network: yeo.i
  temp.yeo <- bna.yeo %>%
    filter(bna.yeo$Yeo_17network == i)
  
  # get gm+ses dataset in network: gm.yeo.i
  tmp.gm.yeo <- gm.ses %>%
    select(temp.yeo$region) %>%
    cbind(gm.ses[,1:34], .)
  
  # models for all regions in gm.yeo.i
  temp.yeo.models <- tmp.gm.yeo %>%
    select(contains('_')) %>%
    map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + TIV, 
              data = tmp.gm.yeo)) %>%
    map(summary) %>%
    map_dfr(~ as.data.frame(.[[1]][1,4:5]))
  temp.yeo.models['ROI'] <- temp.yeo$region
  temp.yeo.models['padj'] <- p.adjust(temp.yeo.models$`Pr(>F)`, method = 'fdr')
  colnames(temp.yeo.models)[2] = 'punadj'
  

  assign(paste0(c('yeo17', i, 'models'), 
                collapse = '.'), temp.yeo.models)
  
  temp.yeo.results <- subset(temp.yeo.models, punadj < 0.05)
  assign(paste0(c('yeo17', i, 'results'), 
                collapse = '.'), temp.yeo.results)
  
  
  
}


# fs cortical measures tests ----------------------------------------------
# n = 718

cortical.fs <- read_csv('./derivatives/CorticalMeas_fromFreeSurfer.csv',
                        col_names = T) 
cortical.fs$mri <- as.character(cortical.fs$mri)
cortical.ses <- left_join(cortical.fs, ses.cog, by = c('mri' = 'MRIID')) %>%
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
                              '2' = 'High'))
write.csv2(cortical.ses, file = './derivatives/FS_SES.csv',
           row.names = F)

# TIV comparison and post-hoc comparison
eticv.model <- aov(EstimatedTotalIntraCranialVol ~ NEWSESGROUP + 
                   AGE + GENDER + HPT + DIABETES + HPL,
                 data = cortical.ses)
summary(eticv.model)
emmeans::emmeans(eticv.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')

# postHocs <- glht(newg_ancova, linfct = mcp(NEWSESGROUP = c('High - Low = 0')))
# summary(postHocs)
# posth <- glht(eticv.model, linfct = mcp(NEWSESGROUP = 'Tukey'))
# summary(posth)


# subcortical GMV comparison and post-hoc
subgmv.model <- aov(SubCortGrayVol ~ NEWSESGROUP + 
                     AGE + GENDER + HPT + DIABETES + HPL,
                   data = cortical.ses)
summary(subgmv.model)
emmeans::emmeans(subgmv.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')

# cortex volume
cortex.model <- aov(CortexVol ~ NEWSESGROUP + 
                      AGE + GENDER + HPT + DIABETES + HPL,
                    data = cortical.ses)
summary(cortex.model)
emmeans::emmeans(cortex.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')

lhcortex.model <- aov(lhCortexVol ~ NEWSESGROUP + 
                      AGE + GENDER + HPT + DIABETES + HPL,
                    data = cortical.ses)
summary(lhcortex.model)
emmeans::emmeans(lhcortex.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')

rhcortex.model <- aov(rhCortexVol ~ NEWSESGROUP + 
                      AGE + GENDER + HPT + DIABETES + HPL,
                    data = cortical.ses)
summary(rhcortex.model)
emmeans::emmeans(rhcortex.model, specs = pairwise ~ NEWSESGROUP, adjust = 'Tukey')




# fs meas comparison ------------------------------------------------------

cortical.ses.60 <- filter(cortical.ses, AGE >= 60)
DK <- names(thickness.model)
thickness.model <- cortical.ses.60 %>%
  dplyr::select(ends_with('thickness')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = cortical.ses.60)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
thickness.model['ROI'] <- DK
thickness.model['padj'] <- p.adjust(thickness.model$`Pr(>F)`, method = 'fdr')
colnames(thickness.model)[2] = 'punadj'
thickness.results <- subset(thickness.model, padj < 0.05)

area.names <- names(area.model)
area.model <- cortical.ses.60 %>%
  dplyr::select(ends_with('area')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = cortical.ses.60)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
area.model['ROI'] <- area.names
area.model['padj'] <- p.adjust(area.model$`Pr(>F)`, method = 'fdr')
colnames(area.model)[2] = 'punadj'
area.results <- subset(area.model, padj < 0.05)

volume.model <- cortical.ses.60 %>%
  dplyr::select(ends_with('volume')) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = cortical.ses.60)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
volume.model['ROI'] <- DK
volume.model['padj'] <- p.adjust(volume.model$`Pr(>F)`, method = 'fdr')
colnames(volume.model)[2] = 'punadj'
volume.results <- subset(volume.model, padj < 0.05)

aseg.name <- names(aseg.model)
aseg.model <- cortical.ses %>%
  dplyr::select(`Left-Lateral-Ventricle`:`MaskVol-to-eTIV`) %>%
  map(~ aov(.x ~ NEWSESGROUP + AGE + GENDER + EstimatedTotalIntraCranialVol + 
              HPT + DIABETES + HPL, 
            data = cortical.ses)) %>%
  map(summary) %>%
  map_dfr(~ as.data.frame(.[[1]][1,4:5]))
aseg.model['ROI'] <- aseg.name
aseg.model['padj'] <- p.adjust(aseg.model$`Pr(>F)`, method = 'fdr')
colnames(aseg.model)[2] = 'punadj'
aseg.results <- subset(aseg.model, padj < 0.05)


# plot > 60 ---------------------------------------------------------------

library(ggseg)
library(ggseg3d)

# Enable this universe
options(repos = c(
  ggseg = 'https://ggseg.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('ggsegBrainnetome')

library(ggsegBrainnetome)
library(scico)

plot(brainnetome)
plot(dk)
plot(aseg)

bna.data <- brainnetome$data

ggseg(view = 'medial',
      hemisphere = 'right')

# bna plot
bna.60.plot.data <- tibble(hemi = c('right', 'left', 'left', 'right',
                                    'left', 'right', 'left', 'left',
                                    'right', 'left', 'right', 'left',
                                    'right', 'left', 'right', 'left', 'right'),
                           region = c('A9l', 'A9/46d', 'IFJ', 'A46',
                                      'A9/46v', 'A9/46v', 'A8vl', 'IFS',
                                      'IFS', 'A45c', 'A45c', 'A45r',
                                      'A45r', 'A44op', 'A44op', 'A44v', 'A44v'),
                           f = gm.ancova.results$`F value` 
                             )


bna.60.plot.data %>%
  #ggplot() +
  ggseg(atlas = brainnetome,
        mapping = aes(fill = f),
        color = 'black',
        position = 'stacked'
        ) +
  # geom_brain(aes(fill = f),
  #            atlas = dk,
  #            hemi = 'right',
  #            position = 'stacked'
  #            show.legend = T) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(title = 'F value map of ROI volume comparison between low SES group and high SES group',
       caption  = 'Participants age ≥60.\nAll comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
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

# scale_fill_gradient(low = 'goldenrod',
#                     high = 'firebrick') +
#   labs(fill = 'F value') +
#   theme_classic()

# area plot
area.60.plot.data <- tibble(hemi = rep(c('left', 'right'), 
                                       times = c(17, 18)),
                            region = c('caudal anterior cingulate', 'caudal middle frontal', 'fusiform', 
                                       'inferior temporal', 'isthmus cingulate', 'lateral occipital',
                                       'lateral orbitofrontal', 'middle temporal',
                                       'paracentral', 'pars opercularis',
                                       'precentral', 'precuneus', 'rostral middle frontal',
                                       'superior frontal', 'superior parietal', 'superior temporal',
                                       'transverse temporal',
                                       'caudal middle frontal', 'cuneus', 'fusiform',
                                       'inferior parietal', 'inferior temporal', 'isthmus cingulate',
                                       'pars orbitalis', 'pars triangularis', 'precentral', 
                                       'precuneus', 'rostral middle frontal', 'superior frontal',
                                       'superior temporal', 'supramarginal', 'frontal pole',
                                       'temporal pole', 'transverse temporal', 'insula'
                                    ),
                            f = area.results$`F value`
                         
)

area.60.plot.data %>%
  ggseg(mapping = aes(fill = f),
        color = 'black',
        position = 'stacked',) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(title = 'F value map of cortical area comparison between low SES group and high SES group',
       caption  = 'Participants age ≥60.\nAll comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
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


# volume plot
volume.60.plot.data <- tibble(hemi = rep(c('left', 'right'), 
                                       times = c(10, 5)),
                              region = c('caudal middle frontal', 'fusiform', 'isthmus cingulate', 
                                         'lateral orbitofrontal', 'middle temporal',
                                         'precuneus', 'rostral middle frontal',
                                         'superior frontal', 'superior parietal', 'superior temporal',
                                         'pars triangularis', 'precuneus', 
                                         'rostral middle frontal', 'superior temporal', 'insula'
                            ),
                            f = volume.results$`F value`
                            
)

volume.60.plot.data %>%
  ggseg(mapping = aes(fill = f),
        color = 'black',
        position = 'stacked',) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(title = 'F value map of cortical volume comparison between low SES group and high SES group',
       caption  = 'Participants age ≥60.\nAll comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
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


# plot --------------------------------------------------------------------


# thickness plot
thickness.plot.data <- tibble(hemi = rep(c('right'), times = 4),
                              region = c('inferior parietal', 'middle temporal',
                                         'postcentral', 'supramarginal'),
                              f = c(9.433959, 8.917819, 10.314961, 10.678520)
)
                         
thickness.plot.data %>%
  #ggplot() +
  ggseg(mapping = aes(fill = f),
        color = 'black',
        position = 'stacked',
        hemisphere = 'right') +
  # geom_brain(aes(fill = f),
  #            atlas = dk,
  #            hemi = 'right',
  #            show.legend = T) +
  scale_fill_scico(palette = 'oslo',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = -1) +
  labs(title = 'F value map of cortical thickness comparison between low SES group and high SES group',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
       fill = 'F value') +
  xlab('right hemisphere') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14, family = 'serif'),
        axis.text.x = element_text(size = 12, family = 'serif'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size = 16, family = 'serif', hjust = 0.5),
        plot.caption = element_text(size = 12, family = 'serif', hjust = 0),
        legend.text = element_text(size = 8, family = 'serif'),
        legend.title = element_text(size = 12, family = 'serif'))
  
  scale_fill_gradient(low = 'goldenrod',
                      high = 'firebrick') +
  labs(fill = 'F value') +
  theme_classic()

# area plot
area.plot.data <- tibble(hemi = c(rep(c('left', 'right'), each = 11)),
                         region = c('caudal middle frontal', 'fusiform', 'lateral orbitofrontal',
                                    'paracentral', 'precentral', 'precuneus', 'rostral middle frontal',
                                    'superior frontal', 'superior parietal', 'superior temporal',
                                    'insula',
                                    'fusiform', 'pars orbitalis', 'pars triangularis', 'postcentral',
                                    'precentral', 'precuneus', 'rostral middle frontal', 'superior frontal',
                                    'superior temporal', 'supramarginal', 'frontal pole'
                                    ),
                         f = area.results$`F value`
                         
)

area.plot.data %>%
  ggseg(mapping = aes(fill = f),
        color = 'black',
        position = 'stacked',) +
  scale_fill_scico(palette = 'lajolla',
                   alpha = 0.8,
                   begin = 0.2,
                   end = 0.7,
                   direction = 1) +
  labs(title = 'F value map of cortical area comparison between low SES group and high SES group',
       caption  = 'All comparisions were adjusted by fdr.\nOnly significant regions after p-correction were shown.',
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

# alternative: define color palette with high and low
area.plot.data %>%
  ggplot() +
  geom_brain(aes(fill = f),
             atlas = dk,
             position = position_brain(hemi ~ side), # hemi ~ side gives 2*2 brains, 
             # 1st row is right hemi, 1st column is lateral view
             # .~ hemi + side gives 1*4 brain: left lateral/medial, right lateral/medial
             show.legend = T) +
  scale_fill_gradient(low = 'goldenrod',
                      high = 'firebrick') +
  labs(fill = 'F value') +
  theme_classic()

# # try
# somedata <- tibble(
#   region = rep(c("transverse temporal", "insula",
#                  "precentral","superior parietal"), 2), 
#   p = sample(seq(0,.5,.001), 8),
#   groups = c(rep("g1", 4), rep("g2", 4))
# )
# 
# somedata %>%
#   group_by(groups) %>%
#   ggplot() +
#   geom_brain(atlas = dk, 
#              position = position_brain(hemi ~ side),
#              aes(fill = p)) +
#   facet_wrap(~groups)


# ggseg3d -----------------------------------------------------------------

# thickness right lateral
ggseg3d(.data = thickness.plot.data,
        atlas = dk_3d,
        colour = 'f',
        text = 'f',
        na.alpha = 0.5,
        palette = c('#FAEB95', '#B04746')) %>%
  add_glassbrain('right') %>%
  pan_camera('right lateral') %>%
  remove_axes()
# thickness right medial
ggseg3d(.data = thickness.plot.data,
        atlas = dk_3d,
        colour = 'f',
        text = 'f',
        na.alpha = 0.5,
        palette = c('#FAEB95', '#B04746')) %>%
  add_glassbrain('right') %>%
  pan_camera('right medial') %>%
  remove_axes()
  
# area left lateral
area.plot.data <- tibble(hemi = c(rep(c('left', 'right'), each = 11)),
                         region = c('caudal middle frontal', 'fusiform', 'lateral orbitofrontal',
                                    'paracentral', 'precentral', 'precuneus', 'rostral middle frontal',
                                    'superior frontal', 'superior parietal', 'superior temporal',
                                    'insula',
                                    'fusiform', 'pars orbitalis', 'pars triangularis', 'postcentral',
                                    'precentral', 'precuneus', 'rostral middle frontal', 'superior frontal',
                                    'superior temporal', 'supramarginal', 'frontal pole'
                         ),
                         f = area.results$`F value`
                         
)
area.3dplot.data <- tibble(label = gsub('_area', '', area.results$ROI),
                           f = area.results$`F value`
  
)

lh.area.3dplot.data <- area.3dplot.data %>%
  subset(grepl('lh_', label)
         )
  
ggseg3d(.data = area.3dplot.data,
        surface = 'LCBC', # alternative: 'white' and 'inflated'
        atlas = dk_3d,
        hemisphere = c('left'),
        colour = 'f',
        text = 'f',
        na.alpha = 0.5,
        palette = c('#FAEB95', '#B04746')) %>%
  add_glassbrain('left') %>%
  pan_camera('left medial') %>%
  remove_axes()


# correlation -------------------------------------------------------------


## correlation
install.packages('ggpubr')
library(ggplot2)
library(ggpubr)
gm.corr.t <- gm.ses %>%
  select(contains('_')) %>%
  map(~ cor.test(.x, gm.ses$SES)) %>%
  map_dfr(~ as.data.frame(.[1])) %>%
  rename(t = statistic)

gm.corr.r <- gm.ses %>%
  select(contains('_')) %>%
  map(~ cor.test(.x, gm.ses$SES)) %>%
  map_dfr(~ as.data.frame(.[4]))

gm.corr.model <- gm.ses %>%
  select(contains('_')) %>%
  map(~ cor.test(.x, gm.ses$SES)) %>%
  map_dfr(~ as.data.frame(.[3])) %>%
  cbind(., gm.corr.t, gm.corr.r) %>%
  rename(p = p.value)

gm.corr.model['region'] <- dvnames
gm.corr.results <- subset(gm.corr.model, p < 0.05)

# plot
ggscatter(ses_gm_roi, x = 'ses', y = 'temporal_sup_r',
          add = 'reg.line', conf.int = T,
          cor.coef = T, cor.method = 'pearson',
          xlab = 'SES Score', ylab = 'GM Density of Temporal_Sup_R') + 
  geom_point(alpha = 0.08) +
  theme(
    text = element_text(size = 12, family = "serif")
  ) 

ggplot(data = ses_gm_roi) +
  geom_point(mapping = aes(x = ses, y = temporal_sup_r, color = sesgroup), alpha = 0.75) +
  geom_smooth(mapping = aes(x = ses, y = temporal_sup_r), method = lm) +
  xlab('SES Score') + ylab('GM Density of Temporal_Sup_R') +
  labs(color = 'SES Group') +
  scale_color_brewer(palette = 'RdYlBu') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'serif'))
        # axis.text = element_text(size = 10))
# axis.title change the title of axis, axis.text change the number value of axis

  
  

ggplot(data = ses_gm_roi, mapping = aes(x = ses, y = temporal_sup_r, color = sesgroup)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = lm) +
  labs(x = 'SES Score', y = 'GM Density of Temporal_Sup_R', fill = 'SES Group') +
  scale_color_brewer(palette = "RdYlBu") +
  theme(legend.text = c('Lowest', 'Lower', 'Medium', 'Higher', 'Highest'),
        axis.ticks.x = element_blank(),
        text = element_text(size = 12, family = "serif")) +
  theme_bw()

