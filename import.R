
# import ------------------------------------------------------------------

library(tidyverse)
library(haven)

charls <- read_dta('D:/projects/SES_charls/sourcedata/charls_job.dta') %>%
  mutate(across(c(wave, 
                  fd002),
                factor)) %>%
  select(1, 3, 109, 243, # gender, age
         244, # education
         41:68, # occupation
         144, 150, # family income & retirement
         80:90, # activities
         128:134, # physical activity, drinking, smoking
         247:249, # memory, executive function, general cognition
         96:107, # mental state
         112:127, # self-rated health and chronic diseases
         169:172, 177, # BMI, CESD (0-30)
         211:227 # blood biomarkers
         ) %>%
  rename(interact = 串门,
         majong = 打麻将,
         help = 提供帮助,
         sport = 跳舞,
         organization = 参加社团组织活动,
         voluntary = 志愿者活动或者慈善活动,
         course = 上学或者参加培训课程,
         care = 照顾病人或残疾人,
         stock = 炒股,
         internet = 上网,
         others = 其他社交活动)

charls %>% 
  group_by(wave) %>% 
  summarise(sum_na = sum(is.na(charls$跳舞)))



# read w1-w5 raw data -----------------------------------------------------

charls_w1_demo <- read_dta('D:/projects/SES_charls/sourcedata/w1-2011/household_and_community_questionnaire_data/demographic_background.dta')
charls_w1_work <- read_dta('D:/projects/SES_charls/sourcedata/w1-2011/household_and_community_questionnaire_data/work_retirement_and_pension.dta')
charls_w1_income <- read_dta('D:/projects/SES_charls/sourcedata/w1-2011/household_and_community_questionnaire_data/individual_income.dta')

charls_w2_demo <- read_dta('D:/projects/SES_charls/sourcedata/w2-2013/CHARLS2013_Dataset/Demographic_Background.dta')
charls_w2_work <- read_dta('D:/projects/SES_charls/sourcedata/w2-2013/CHARLS2013_Dataset/Work_Retirement_and_Pension.dta')
charls_w2_income <- read_dta('D:/projects/SES_charls/sourcedata/w2-2013/CHARLS2013_Dataset/Individual_Income.dta')

charls_w3_demo <- read_dta('D:/projects/SES_charls/sourcedata/w3-2015/CHARLS2015r/Demographic_Background.dta')
charls_w3_work <- read_dta('D:/projects/SES_charls/sourcedata/w3-2015/CHARLS2015r/Work_Retirement_and_Pension.dta')
charls_w3_income <- read_dta('D:/projects/SES_charls/sourcedata/w3-2015/CHARLS2015r/Individual_Income.dta')

charls_w4_demo <- read_dta('D:/projects/SES_charls/sourcedata/w4-2018/CHARLS2018r/Demographic_Background.dta')
charls_w4_work <- read_dta('D:/projects/SES_charls/sourcedata/w4-2018/CHARLS2018r/Work_Retirement.dta')
charls_w4_income <- read_dta('D:/projects/SES_charls/sourcedata/w4-2018/CHARLS2018r/Individual_Income.dta')

charls_w5_demo <- read_dta('D:/projects/SES_charls/sourcedata/w5-2020/CHARLS2020r/Demographic_Background.dta')
charls_w5_work <- read_dta('D:/projects/SES_charls/sourcedata/w5-2020/CHARLS2020r/Work_Retirement.dta')
charls_w5_income <- read_dta('D:/projects/SES_charls/sourcedata/w5-2020/CHARLS2020r/Individual_Income.dta')


# merge w1 ----------------------------------------------------------------

charls_w1_ses <- charls_w1_demo %>%
  select(ID, householdID, communityID, 
         ba002_1, ba002_2, ba002_3, # birth year/month/day
         bd001, bd002, bd003, bd005, bd006, bd008, # level of education, ys of primary school, ys after primary school
         be001, be002, # marital status, living with someone
         ) %>%
  full_join(., select(charls_w1_work,
                      c(
                        ID, householdID, communityID,
                        fc004, fc008, # farming for others (months), household(1 = yes) 
                        fd002, fd008, fd010, # employed, government, ownership type of bussiness
                        fd012_gb, fd012_isco, fd013,
                        fh005_gb, fh005_isco # non-farm self employed work
                        )
                      ), 
            by = c('ID' = 'ID')) %>%
  select(!c(householdID.x, communityID.x, householdID.y, communityID.y)) %>%
  full_join(., select(charls_w1_income,
                      c(ID, householdID, communityID,
                        ga002, ga002_1, # money last year or per month
                        ga004_1_1_:ga004_1_11_, # all kinds of income per year
                        ga004_2_1_:ga004_2_11_ # all kinds of income per month
                      )
                      ),
            by = c('ID' = 'ID')) %>%
  mutate(
    eduy = case_when(
      # If both bd005 and bd006 are not NA, calculate eduyear as bd006 - bd005
      !is.na(bd005) & !is.na(bd006) & 
        (bd006 - bd005 >= 0) & (bd006 - bd005 <= 22) ~ bd006 - bd005,

      # Fallback logic if bd005 or bd006 is NA
      bd001 == 1 ~ 0, # No formal education 
      bd001 == 2 ~ ifelse(is.na(bd002), 0, bd002), # Did not finish primary school
      bd001 == 3 ~ ifelse(is.na(bd002), 0, bd002), # Sishu/home school
      bd001 == 4 ~ 6 + ifelse(is.na(bd003), 0, bd003), # Elementary school
      bd001 == 5 ~ 8 + ifelse(is.na(bd003), 0, bd003), # Middle school
      bd001 == 6 ~ 11 + ifelse(is.na(bd003), 0, bd003), # High school
      bd001 == 7 ~ 12 + ifelse(is.na(bd003), 0, bd003), # Vocational school
      bd001 == 8 ~ 13 + ifelse(is.na(bd003), 0, bd003), # Two-/Three-Year College/Associate degree
      bd001 == 9 ~ 15 + ifelse(is.na(bd003), 0, bd003), # Four-Year College/Bachelor’s degree
      bd001 == 10 ~ 18 + ifelse(is.na(bd003), 0, bd003), # Master's degree
      bd001 == 11 ~ 21 + ifelse(is.na(bd003), 0, bd003), # PhD
      
      # If bd001 is NA
      is.na(bd001) ~ NA_real_ # bd001 is NA
    ),
    inny = if_else(
      rowSums(is.na(select(., ga002, ga004_1_1_:ga004_1_11_))) == ncol(select(., ga002, ga004_1_1_:ga004_1_11_)),
      NA_real_, # If all values are NA, set income to NA
      rowSums(select(., ga002, ga004_1_1_:ga004_1_11_), na.rm = TRUE) # Otherwise, calculate the sum
    ),
    innm = if_else(
      rowSums(is.na(select(., ga002_1, ga004_2_1_:ga004_2_11_))) == ncol(select(., ga002_1, ga004_2_1_:ga004_2_11_)),
      NA_real_, # If all values are NA, set income to NA
      rowSums(select(., ga002_1, ga004_2_1_:ga004_2_11_), na.rm = TRUE) # Otherwise, calculate the sum
    )

  ) %>%
  mutate(ID = paste0(substring(ID, 1, 9), '0', substring(ID, 10, 11))) # recode the ID to be harmonized with w2-w5 as 12 digits

# occupation code need to be solved and merged into charls_w1_ses

charls_w1_occ <- charls_w1_ses %>%
  select(ID, fc004, fc008, fd012_gb, fh005_gb) %>%
  mutate(occ_gb = as.numeric(as.character(fd012_gb)),
         selfocc_gb = as.numeric(as.character(fh005_gb))) %>%
  mutate(
    # (1) Determine employment type
    employed_status = case_when(
      !is.na(occ_gb) & is.na(selfocc_gb) ~ "employed",      # Only employed
      is.na(occ_gb) & !is.na(selfocc_gb) ~ "self-employed", # Only self-employed
      !is.na(occ_gb) & !is.na(selfocc_gb) ~ "both",         # Both employed and self-employed
      (fc004 > 0 | fc008 == 1) ~ "farmer",
      is.na(occ_gb) & is.na(selfocc_gb) ~ NA_character_     # Both are NA
    ) %>%
      factor(levels = c('employed', 'self-employed', 'both', 'farmer')),
    
    # (2) Combine occupation codes
    occupation = case_when(
      employed_status == "employed" ~ as.character(occ_gb),           # Copy `occupation` for employed
      employed_status == "self-employed" ~ as.character(selfocc_gb),  # Copy `self_occupation` for self-employed
      employed_status == "both" ~ as.character(occ_gb),               # Set to occ_gb if both are present
      is.na(employed_status) ~ NA_character_ 
    )
  ) %>%
  mutate(occupation = as.numeric(as.character(occupation)))

charls_w1_ses <- charls_w1_ses %>%
  left_join(., charls_w1_occ, by = c('ID' = 'ID'))

# charls_w1_ses shoul be merged with charls activities and cognitive performances for modelling



# merge w2 ----------------------------------------------------------------

charls_w2_ses <- charls_w2_demo %>%
  # filter(bd001_w2_1 == 1 & bd001_w2_4 != 12)
  # # 1017 bd001_w2_1 == 2 but dc001_w2_4 = 12; 2457 bd001_w2_1 == 1 but dc001_w2_4 = !12
  
  select(ID, householdID, communityID, 
         ba002_1, ba002_2, ba002_3, # birth year/month/day
         bd001_w2_1, bd001_w2_4, # check if it is the same with wave 1
         bd002, bd002_w2_1, bd003, bd005, bd006, bd008, 
         # level of education, ys of primary school, ys after primary school, only for thoes bd001_w2_1 == 2
         be001 # marital status, living with someone: be001 = 7
  ) %>%
  full_join(., select(charls_w2_work,
                      c(
                        ID,
                        fc001, fc004,  # farming 
                        fd002   # a lot of missings
                        )
                      ), 
            by = c('ID' = 'ID')) %>%
  full_join(., select(charls_w2_income,
                      c(ID, householdID, communityID,
                        ga002_1, ga002_2, # money last year or per month
                        ga004_1_1_:ga004_1_9_, # all kinds of income per year
                        ga004_2_1_:ga004_2_9_ # all kinds of income per month
                      )
  ),
  by = c('ID' = 'ID')) %>%
  left_join(., charls_w1_occ %>% select(ID, 
                                        employed_status_w1 = employed_status, 
                                        occupation_w1 = occupation),
            by = c('ID' = 'ID')) %>%
  select(!c(householdID.x, communityID.x,
            householdID.y, communityID.y)) %>%
  
  # Education: Left join Wave 1 education year by ID
  left_join(.,
    charls_w1_ses %>% select(ID, eduy_w1 = eduy),
    by = c('ID' = 'ID')
  ) %>%
  mutate(
    eduy = case_when(
      # Scenario 1: bd001_w2_1 == 1
      bd001_w2_1 == 1 ~ eduy_w1,
      
      # Scenario 2: bd001_w2_1 == 2 & bd001_w2_4 == 12 or bd001_w2_4 is NA
      bd001_w2_1 == 2 & (bd001_w2_4 == 12 | is.na(bd001_w2_4)) ~ eduy_w1,
      
      # Scenario 3: bd001_w2_1 == 2 & bd001_w2_4 != 12, apply fallback logic
      bd001_w2_1 == 2 & bd001_w2_4 != 12 ~ case_when(
        # Calculate education years as bd006 - bd005 if valid
        !is.na(bd005) & !is.na(bd006) & 
          (bd006 - bd005 >= 0) & (bd006 - bd005 <= 22) ~ bd006 - bd005,
        
        # Fallback to education level and additional years logic
        bd001_w2_4 == 1 ~ 0,  # No formal education
        bd001_w2_4 == 2 ~ ifelse(is.na(bd002), 0, bd002), # Did not finish primary school
        bd001_w2_4 == 3 ~ ifelse(is.na(bd002_w2_1), 0, bd002_w2_1), # Sishu/home school
        bd001_w2_4 == 4 ~ 6 + ifelse(is.na(bd003), 0, bd003), # Elementary school
        bd001_w2_4 == 5 ~ 8 + ifelse(is.na(bd003), 0, bd003), # Middle school
        bd001_w2_4 == 6 ~ 11 + ifelse(is.na(bd003), 0, bd003), # High school
        bd001_w2_4 == 7 ~ 12 + ifelse(is.na(bd003), 0, bd003), # Vocational school
        bd001_w2_4 == 8 ~ 13 + ifelse(is.na(bd003), 0, bd003), # Associate degree
        bd001_w2_4 == 9 ~ 15 + ifelse(is.na(bd003), 0, bd003), # Bachelor's degree
        bd001_w2_4 == 10 ~ 18 + ifelse(is.na(bd003), 0, bd003), # Master's degree
        bd001_w2_4 == 11 ~ 21 + ifelse(is.na(bd003), 0, bd003), # PhD
        
        # Default to NA if no conditions are met
        TRUE ~ NA_real_
      ),
      
      # Scenario 4: bd001_w2_1 is NA, but other variables may have values
      is.na(bd001_w2_1) ~ case_when(
        !is.na(bd005) & !is.na(bd006) & 
          (bd006 - bd005 >= 0) & (bd006 - bd005 <= 22) ~ bd006 - bd005,
        
        bd001_w2_4 == 1 ~ 0,
        bd001_w2_4 == 2 ~ ifelse(is.na(bd002), 0, bd002),
        bd001_w2_4 == 3 ~ ifelse(is.na(bd002_w2_1), 0, bd002_w2_1),
        bd001_w2_4 == 4 ~ 6 + ifelse(is.na(bd003), 0, bd003),
        bd001_w2_4 == 5 ~ 8 + ifelse(is.na(bd003), 0, bd003),
        bd001_w2_4 == 6 ~ 11 + ifelse(is.na(bd003), 0, bd003),
        bd001_w2_4 == 7 ~ 12 + ifelse(is.na(bd003), 0, bd003),
        bd001_w2_4 == 8 ~ 13 + ifelse(is.na(bd003), 0, bd003),
        bd001_w2_4 == 9 ~ 15 + ifelse(is.na(bd003), 0, bd003),
        bd001_w2_4 == 10 ~ 18 + ifelse(is.na(bd003), 0, bd003),
        bd001_w2_4 == 11 ~ 21 + ifelse(is.na(bd003), 0, bd003),
        
        TRUE ~ NA_real_
      ),
      
      # Scenario 5: All variables are NA
      is.na(bd001_w2_1) & is.na(bd001_w2_4) & 
        is.na(bd005) & is.na(bd006) ~ eduy_w1,
      
      # Default to NA
      TRUE ~ NA_real_
    ),
    
    # Income
    inny = if_else(
      rowSums(is.na(select(., ga002_1, ga004_1_1_:ga004_1_9_))) == ncol(select(., ga002_1, ga004_1_1_:ga004_1_9_)),
      NA_real_, # If all values are NA, set income to NA
      rowSums(select(., ga002_1, ga004_1_1_:ga004_1_9_), na.rm = TRUE) # Otherwise, calculate the sum
    ),
    
    innm = if_else(
      rowSums(is.na(select(., ga002_2, ga004_2_1_:ga004_2_9_))) == ncol(select(., ga002_2, ga004_2_1_:ga004_2_9_)),
      NA_real_, # If all values are NA, set income to NA
      rowSums(select(., ga002_2, ga004_2_1_:ga004_2_9_), na.rm = TRUE) # Otherwise, calculate the sum
    ),
    
    # Occupation
    employed_status = case_when(
      is.na(employed_status_w1) & fc004 > 0 ~ "farmer",
      !is.na(employed_status_w1) ~ as.character(employed_status_w1),
      
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c('employed', 'self-employed', 'both', 'farmer')),
             
    occupation = occupation_w1
    
    ) 


# charls_w1_ses shoul be merged with charls activities and cognitive performances for modelling


# merge w3 ----------------------------------------------------------------

charls_w3_ses <- charls_w3_demo %>%
  # filter(bd001_w2_1 == 1 & bd001_w2_4 != 12)
  # # 1017 bd001_w2_1 == 2 but dc001_w2_4 = 12; 2457 bd001_w2_1 == 1 but dc001_w2_4 = !12
  
  select(ID, householdID, communityID, 
         ba002_1, ba002_2, ba002_3, # birth year/month/day
         bd001_w2_4, # check if it is the same with wave 1
         bd002_w3, # ys after bd001_w2_4, only for thoes bd001_w2_4 != 12
         # bd002_w2_1, bd003, bd005, bd006, bd008,
         be001 # marital status, living with someone: be001 = 7
  ) %>%
  # no fd012
  full_join(., select(charls_w3_work,
                      c(
                        ID,
                        # fc001, # do farm employed
                        fd002   # Type of employer
                      )
  ),
  by = c('ID' = 'ID')) %>%
  full_join(., select(charls_w3_income,
                      c(ID, householdID, communityID,
                        ga002, # money last year
                        ga004_1_:ga004_9_ # all kinds of income per year
                        # ga004_1_1_:ga004_1_9_ # all kinds of income per month
                        # ga004_2_1_:ga004_2_9_ # all kinds of income per month
                        # The above are all NA
                      )), by = c('ID' = 'ID')) %>%
  left_join(., charls_w2_ses %>% select(ID, 
                                        employed_status_w2 = employed_status, 
                                        occupation_w2 = occupation),
            by = c('ID' = 'ID')) %>%
  select(!c(householdID.x, communityID.x,
            householdID.y, communityID.y)) %>%
  
  # Education: Left join Wave 1 education year by ID
  left_join(.,
            charls_w2_ses %>% select(ID, eduy_w2 = eduy),
            by = c('ID' = 'ID')
  ) %>%
  mutate(
    eduy = case_when(
      
      bd001_w2_4 == 12 ~ eduy_w2,
      bd001_w2_4 != 12 & is.na(bd002_w3) ~ eduy_w2,
      is.na(bd001_w2_4) ~ eduy_w2,
      bd001_w2_4 != 12 & !is.na(bd002_w3) ~ case_when(
        
        # Fallback to education level and additional years logic
        bd001_w2_4 == 1 ~ 0,  # No formal education
        bd001_w2_4 == 2 ~ ifelse(is.na(bd002_w3), 0, bd002_w3), # Did not finish primary school
        bd001_w2_4 == 3 ~ ifelse(is.na(bd002_w3), 0, bd002_w3), # Sishu/home school
        bd001_w2_4 == 4 ~ 6 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Elementary school
        bd001_w2_4 == 5 ~ 8 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Middle school
        bd001_w2_4 == 6 ~ 11 + ifelse(is.na(bd002_w3), 0, bd002_w3), # High school
        bd001_w2_4 == 7 ~ 12 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Vocational school
        bd001_w2_4 == 8 ~ 13 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Associate degree
        bd001_w2_4 == 9 ~ 15 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Bachelor's degree
        bd001_w2_4 == 10 ~ 18 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Master's degree
        bd001_w2_4 == 11 ~ 21 + ifelse(is.na(bd002_w3), 0, bd002_w3), # PhD
        
        # Default to NA if no conditions are met
        TRUE ~ NA_real_
      )),
     
    # Income
    inny = if_else(
      rowSums(is.na(select(., ga002, ga004_1_:ga004_9_))) == ncol(select(., ga002, ga004_1_:ga004_9_)),
      NA_real_, # If all values are NA, set income to NA
      rowSums(select(., ga002, ga004_1_:ga004_9_), na.rm = TRUE) # Otherwise, calculate the sum
    ),
    
    # Occupation
    employed_status = employed_status_w2,
    
    occupation = occupation_w2
    
  )


# merge w4 ----------------------------------------------------------------

charls_w4_ses <- charls_w4_demo %>%
  # filter(bd001_w2_1 == 1 & bd001_w2_4 != 12)
  # # 1017 bd001_w2_1 == 2 but dc001_w2_4 = 12; 2457 bd001_w2_1 == 1 but dc001_w2_4 = !12
  
  select(ID, householdID, communityID, 
         ba000_w2_3, # Gender
         ba002_1, ba002_2, ba002_3, # actual birth year/month/day
         bd001_w2_4, # education level
         bd002_w3, # ys after bd001_w2_4, only for thoes bd001_w2_4 != 12
         # bd002_w2_1, bd003, bd005, bd006, bd008,
         be001, be002 # marital status
  ) %>%
  # no fd012
  full_join(., select(charls_w4_work,
                      c(
                        ID,
                        # fc001, # do farm employed
                        fd002   # 4843 total
                      )
  ),
  by = c('ID' = 'ID')) %>%
  full_join(., select(charls_w4_income,
                      c(ID, householdID, communityID,
                        ga002, # money last year
                        ga003_w4_1:ga003_w4_9 # all kinds of income per year
                        # ga004_2_1_:ga004_2_9_ # all kinds of income per month
                      )), by = c('ID' = 'ID')) %>%
  left_join(., charls_w2_ses %>% select(ID, 
                                        employed_status_w2 = employed_status, 
                                        occupation_w2 = occupation),
            by = c('ID' = 'ID')) %>%
  select(!c(householdID.x, communityID.x,
            householdID.y, communityID.y)) %>%
  
  # Education: Left join Wave 1 education year by ID
  left_join(.,
            charls_w3_ses %>% select(ID, eduy_w2 = eduy),
            by = c('ID' = 'ID')
  ) %>%
  mutate(
    eduy = case_when(
        
        # Fallback to education level and additional years logic
        bd001_w2_4 == 1 ~ 0,  # No formal education
        bd001_w2_4 == 3 ~ ifelse(is.na(bd002_w3), 0, bd002_w3), # Did not finish primary school, as 3y
        bd001_w2_4 == 3 ~ ifelse(is.na(bd002_w3), 0, bd002_w3), # Sishu/home school
        bd001_w2_4 == 4 ~ 6 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Elementary school
        bd001_w2_4 == 5 ~ 8 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Middle school
        bd001_w2_4 == 6 ~ 11 + ifelse(is.na(bd002_w3), 0, bd002_w3), # High school
        bd001_w2_4 == 7 ~ 12 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Vocational school
        bd001_w2_4 == 8 ~ 13 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Associate degree
        bd001_w2_4 == 9 ~ 15 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Bachelor's degree
        bd001_w2_4 == 10 ~ 18 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Master's degree
        bd001_w2_4 == 11 ~ 21 + ifelse(is.na(bd002_w3), 0, bd002_w3), # PhD
        
        # Default to NA if no conditions are met
        TRUE ~ NA_real_
      ),
    
    # Income
    inny = if_else(
      rowSums(is.na(select(., ga002, ga003_w4_1:ga003_w4_9))) == ncol(select(., ga002, ga003_w4_1:ga003_w4_9)),
      NA_real_, # If all values are NA, set income to NA
      rowSums(select(., ga002, ga003_w4_1:ga003_w4_9), na.rm = TRUE) # Otherwise, calculate the sum
    ),
    
    # Occupation
    employed_status = employed_status_w2,
    
    occupation = occupation_w2
    
  )


# merge w5 ----------------------------------------------------------------

charls_w5_ses <- charls_w5_demo %>%
  # filter(bd001_w2_1 == 1 & bd001_w2_4 != 12)
  # # 1017 bd001_w2_1 == 2 but dc001_w2_4 = 12; 2457 bd001_w2_1 == 1 but dc001_w2_4 = !12
  
  select(ID, householdID, communityID, 
         ba001, # Recorded Gender
         ba003_1, ba003_2, ba003_3, # actual birth year/month/day
         ba010, # education level
         # bd002_w3, # ys after bd001_w2_4, only for thoes bd001_w2_4 != 12
         # bd002_w2_1, bd003, bd005, bd006, bd008,
         ba011, ba012 # marital status
  ) %>%
  # work
  full_join(., select(charls_w5_work,
                      c(
                        ID,
                        fa002_s1, fa002_s2 # do farm work self-employed or employed
                        # no employed exact job
                      )
  ),
  by = c('ID' = 'ID')) %>%
  full_join(., select(charls_w5_income,
                      c(ID, householdID, communityID,
                        ga002, # money last year, -1 for cannot answer
                        ga005_1:ga005_9 # all kinds of income per year
                      )), by = c('ID' = 'ID')) %>%
  left_join(., charls_w2_ses %>% select(ID, 
                                        employed_status_w2 = employed_status, 
                                        occupation_w2 = occupation),
            by = c('ID' = 'ID')) %>%
  left_join(., charls_w4_ses %>%
              select(ID, fd002),
            by = c('ID' = 'ID')) %>%
  select(!c(householdID.x, communityID.x,
            householdID.y, communityID.y)) %>%
  
  # Education: Left join Wave 1 education year by ID
  left_join(.,
            charls_w4_ses %>% select(ID, eduy),
            by = c('ID' = 'ID') # wave 5 only record education level so use wave 4 education years
  ) %>%
  mutate(
    # eduy = case_when(
    #   
    #   # Fallback to education level and additional years logic
    #   bd001_w2_4 == 1 ~ 0,  # No formal education
    #   bd001_w2_4 == 3 ~ ifelse(is.na(bd002_w3), 0, bd002_w3), # Did not finish primary school, as 3y
    #   bd001_w2_4 == 3 ~ ifelse(is.na(bd002_w3), 0, bd002_w3), # Sishu/home school
    #   bd001_w2_4 == 4 ~ 6 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Elementary school
    #   bd001_w2_4 == 5 ~ 8 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Middle school
    #   bd001_w2_4 == 6 ~ 11 + ifelse(is.na(bd002_w3), 0, bd002_w3), # High school
    #   bd001_w2_4 == 7 ~ 12 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Vocational school
    #   bd001_w2_4 == 8 ~ 13 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Associate degree
    #   bd001_w2_4 == 9 ~ 15 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Bachelor's degree
    #   bd001_w2_4 == 10 ~ 18 + ifelse(is.na(bd002_w3), 0, bd002_w3), # Master's degree
    #   bd001_w2_4 == 11 ~ 21 + ifelse(is.na(bd002_w3), 0, bd002_w3), # PhD
    #   
    #   # Default to NA if no conditions are met
    #   TRUE ~ NA_real_
    # ),
    
    # Income
    inny = if_else(
      rowSums(is.na(select(., ga002, ga005_1:ga005_9))) == ncol(select(., ga002, ga005_1:ga005_9)),
      NA_real_, # If all values are NA, set income to NA
      rowSums(select(., ga002, ga005_1:ga005_9), na.rm = TRUE) # Otherwise, calculate the sum
    ),
    
    # Occupation
    employed_status = employed_status_w2,
    
    occupation = occupation_w2
    
  )

# for summary:
length(which(charls_w4_ses$bd001_w2_4 == 2 & charls_w4_ses$bd002_w3 != 0))

# clean w1-w5 ses ---------------------------------------------------------

charls_w1 <- charls_w1_ses %>%
  select(ID, 
         eduy, inny, innm, fd002, employed_status, occupation) %>%
  mutate(waves = 1)

charls_w2 <- charls_w2_ses %>%
  select(ID, 
         eduy, inny, innm, fd002, employed_status, occupation
         ) %>%
  mutate(waves = 2)

charls_w3 <- charls_w3_ses %>%
  select(ID,
         eduy, inny, fd002, employed_status, occupation) %>%
  mutate(waves = 3)

charls_w4 <- charls_w4_ses %>%
  select(ID,
         eduy, inny, fd002, employed_status, occupation) %>%
  mutate(waves = 4)

charls_w5 <- charls_w5_ses %>%
  select(ID,
         eduy, inny, fd002, employed_status, occupation) %>%
  mutate(waves = 5)

# check innm for w1 and w2 and allign with other waves as inny

charls_w1 <- charls_w1 %>%
  mutate(inny_new = case_when(
    # If yearly income is NA and monthly income is not NA, calculate yearly income from monthly income
    is.na(inny) & !is.na(innm) ~ innm * 12,
    
    # If yearly income is not NA and monthly income is NA, use yearly income
    !is.na(inny) & is.na(innm) ~ inny,
    
    # If both yearly and monthly income are available
    !is.na(inny) & !is.na(innm) ~ case_when(
      inny == innm * 12 ~ inny,                       # If consistent, keep either
      inny != innm * 12 ~ (inny + innm * 12)/2       # Otherwise, choose the larger value
    ),
    
    # If both are NA, set result to NA
    TRUE ~ NA_real_
  )) %>%
  select(ID, eduy, inny_new, fd002, employed_status, occupation, waves) %>%
  rename(inny = inny_new)

charls_w2 <- charls_w2 %>%
  mutate(inny_new = case_when(
    # If yearly income is NA and monthly income is not NA, calculate yearly income from monthly income
    is.na(inny) & !is.na(innm) ~ innm * 12,
    
    # If yearly income is not NA and monthly income is NA, use yearly income
    !is.na(inny) & is.na(innm) ~ inny,
    
    # If both yearly and monthly income are available
    !is.na(inny) & !is.na(innm) ~ case_when(
      inny == innm * 12 ~ inny,                       # If consistent, keep either
      inny != innm * 12 ~ (inny + innm * 12)/2       # Otherwise, choose the larger value
    ),
    
    # If both are NA, set result to NA
    TRUE ~ NA_real_
  )) %>%
  select(ID, eduy, inny_new, fd002, employed_status, occupation, waves) %>%
  rename(inny = inny_new)

charls_ses <- rbind(charls_w1, charls_w2, charls_w3, charls_w4, charls_w5) %>%
  mutate(waves = factor(waves)) %>%
  select(ID, waves, eduy, employed_status, occupation, inny) %>%
  arrange(ID, waves)
  
