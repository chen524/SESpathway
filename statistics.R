
# load data ---------------------------------------------------------------

library(tidyverse)


head(gm.ses)
head(cortical.ses)


# gm demographic ----------------------------------------------------------

gm.dg <- gm.ses %>%
  group_by(SESGROUP) %>%
  summarise(
    across(.cols = c(AGE, MMSE:SOCIAL, EATING, SLEEPING, UCLA, GDS),
           .fns = list(
             mean = mean,
             sd = sd),
           na.rm = T),
   across(.cols = c(HOUSING, MARRIAGE,SMOKING, DRINKING, HPT, DIABETES, HPL, MCI),
          .fns = list(
            yes = ~sum(. == '1', na.rm = T),
            no = ~sum(. == '0', na.rm = T))),
   across(.cols = c(GENDER),
          .fns = list(
            male = ~sum(. == 'Male'),
            female = ~sum(. == 'Female')))
            )

gm.dg <- subsample.gm %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(where(is.numeric) & !contains('_'), 
           .fns = list(
             mean = mean,
             sd = sd),
           na.rm = T),
    across(where(is.factor),
           .fns = list(
             yes = ~sum(. == '1', na.rm = T),
             no = ~sum(. == '0', na.rm = T))),
    across(.cols = c(GENDER),
           .fns = list(
             male = ~sum(. == 'Male'),
             female = ~sum(. == 'Female')))
  )


# gm demographic tests ----------------------------------------------------

# continous variables t tests
t.names <- names(gm.dg.model)
gm.dg.model <- subsample.gm %>%
  select(where(is.numeric) & !contains('_')) %>%
  map(~ t.test(.x ~ NEWSESGROUP, 
               data = subsample.gm)) %>%
  map_dfr(., ~ list(low = .x$estimate[1], high = .x$estimate[2],
                    t = .x$statistic, p = .x$p.value)) %>%
  # the map_dfr() function is expecting the output of the mapping function to be a list, 
  # but the data.frame() function is returning a data frame.
  cbind(., t.names)

# factors chi tests
chi.names <- names(gm.dg.model)
gm.dg.model <- subsample.gm %>%
  select(where(is.factor)) %>%
  map(~ chisq.test(.x, subsample.gm$NEWSESGROUP, 
                   correct = F)) %>%
  map_dfr(., ~ list(chisq = .x$statistic, p = .x$p.value)) %>%
  cbind(., chi.names)

# emmeans::emmeans(tiv.model, specs = pairwise ~ NEWSESGROUP:GENDER, adjust = 'Tukey')


# BNA results -------------------------------------------------------------

tmp <- gm.ses.60 %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(all_of(gm.ancova.results$ROI), 
           .fns = list(
             mean = mean,
             sd = sd),
           na.rm = T))

# fs results --------------------------------------------------------------
area.results.roi <- area.results$ROI
tmp <- cortical.ses.60 %>%
  group_by(NEWSESGROUP) %>%
  summarise(
    across(all_of(gsub('_thickness', '_volume',
                       volume.results$ROI)), 
           .fns = list(
             mean = mean),
           na.rm = T))
  


