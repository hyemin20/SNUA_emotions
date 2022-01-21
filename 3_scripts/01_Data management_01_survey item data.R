###' ###########################################################################'
###' 
###' Project(project name): Emotion project
###' 
###' Category(stage in the project): Data management
###' 
###' Task(specific task in the category): Data cleaning
###' 
###' Data(data source): `survey item data`
###' 
###' Date: 
###' 2022-01-07 `initiated`
###' 2022-01-13 `updated`
###' 2022-01-14 `updated`
###' 
###' Author: Hyemin Park(`hyemin.park@snu.ac.kr`)
###' 
###'

###' ###########################################################################'
###' 
###' Basic settings
###' 
###' 

### Start with clean state
gc(); rm(list=ls())


### Set working directory and data directory
work_dir <- c("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/Emotions")
data_dir <- file.path(work_dir, "datasets")
setwd(work_dir)


### Call libraries
library(tidyverse)
library(readr)


### Call functions
list.files(file.path(work_dir, "functions"), full.names = TRUE) %>% walk(source)



###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

### Set file path
file_path <- file.path(data_dir, "whole_data.csv")
df <- read_csv(file = file_path) %>% tibble()


#### Check variable types

# lapply function
class(df)
lapply(df, class)
lapply(df, mean, na.rm = TRUE) # calculate means without missing values

# tidyverse function
map(df,mean, na.rm = TRUE)
map(df,summary, na.rm = TRUE)
map_df(df,mean, na.rm = TRUE)



###' ###########################################################################'
###' 
###' Name variables
###' 
###' (1) Demographic information
###' 
###' 

### list up names
names(df)

### check Q0 => student ID
df$Q0
df %>% 
  select(Q0) %>%
  print(n = 200)


### change name Q0:Q6

# once at a time
df <- df %>%
  rename('ID' = Q0,
         'birth_year' = Q1,
         'latino_dum' = Q2,
         'race' = Q4,
         'race_text'= Q4_6_TEXT,
         'sex' = Q5,
         'sex_orient' = Q6)

#' change at once(1)
#' vec_demographic 
#'    <- c('ID', 'birth_year','latino_dum','race','race_text','sex','sex_orient')
#' demo_df <- df %>%
#'   select(Q0:Q6) %>%
#'   set_names(vec_demographic)
#'   
#' change at once(2)***
#' vec_demographic
#'    <- c('ID', 'birth_year','latino_dum','race','race_text','sex','sex_orient')
#' df <- df %>%
#'   rename_at(vars(Q0:Q6), ~vec_demographic)


### change coded values: latino_dum
df <- df %>%
  mutate(latino_dum = factor(latino_dum,
                             levels = c(2,1),
                             labels = c('N_latino','Y_latino')))


### change coded values:race
vec_race <- seq(6)
vec_race_lab <- c('White','Black','AIAN','Asian','Polynesian','Other')
  
df <- df %>%
  mutate(race = factor(race,
                       levels = vec_race,
                       labels = vec_race_lab))


### change coded values:sex
df <- df %>%
  mutate(sex = factor(sex,
                      levels = c(2,1),
                      labels = c('Female','Male')))


### change coded values:sex_orient
vec_sex_orient <- seq(5)
vec_sex_orient_lab <- c('straight','gay','bi','other','notsaying')

df <- df %>%
  mutate(sex_orient = factor(sex_orient,
                      levels = vec_sex_orient,
                      labels = vec_sex_orient_lab))



###' ###########################################################################'
###' 
###' Name variables
###' 
###' (2) Internalization
###' - Lack of internalization: Q8-Q14
###' - Partial Ego Internalization: Q15-Q22
###' - Partial Guilt Internalization: Q23-Q25
###' - Identified Internalization: Q26-Q29
###' - Integrated Internalization: Q30-Q41
###' 
###' 

### change name Q8-Q14
vec_loi <- c('LI1_pointless', 'LI2_parentssaidso','LI3_forcedbyparents',
              'LI4_forcedbyteachers','LI5_noschool','LI6_promisedgifts',
              'LI7_avoidscolding')
 df <- df %>%
   rename_at(vars(Q8:Q14), ~vec_loi)
  

### change name Q15-Q22
 vec_pei <- c('PEI1_lookbetter', 'PEI2_getbettergrade','PEI3_lookhardworker',
               'PEI4_performbetter','PEI5_notlookincompetent','PEI6_provesmart',
               'PEI7_notfeelfailure')
 df <- df %>%
   rename_at(vars(Q15:Q20,Q22), ~vec_pei)


### change name Q23-Q25
vec_pgi <- c('PGI1_notlearnwell.', 'PGI2_mistake','PGI3_notfinishhw')
df <- df %>%
  rename_at(vars(Q23:Q25), ~vec_pgi)

#' df <- df %>%
#'  rename('PGI1_notlearnwell.' = Q23,
#'         'PGI2_mistake' = Q24,
#'         'PGI3_notfinishhw' = Q25)


### change name Q26-Q29
vec_idi <- c('IdI1_valuelearning', 'IdI2_impforgoals','IdI3_impstudyconsistently',
             'IdI4_toreachfoals')
df <- df %>%
  rename_at(vars(Q26:Q29), ~vec_idi)


### change name Q30-Q41
vec_ini <- c('InI1_knowpurpose', 'InI2_understandmyself','InI3_defineinterests',
             'InI4_overcomechallenges','InI5_getconfidence','InI6_findpurpose',
             'InI7_tohelpothers','InI8_findworld','InI9_findcareers',
             'InI10_getdesiretolearn','InI11_wannalearn')
df <- df %>%
  rename_at(vars(Q30,Q32:Q41), ~vec_ini)



###' ###########################################################################'
###' 
###' Name values
###' 
###' (2) Internalization
###' 
###' 

### change coded values: likert5_1
vec_likert5_1 <- seq(5)
vec_internalization_lab <- c('Vdisagree','disagree','soso','agree','Vagree')

### change coded values:Lack of internalization
df <- df %>%
  mutate(LI1_pointless = factor(LI1_pointless,
                                levels = vec_likert5_1,
                                labels = vec_internalization_lab))

df <- df %>%
  mutate(LI2_parentssaidso = factor(LI2_parentssaidso,
                                    levels = vec_likert5_1,
                                    labels = vec_internalization_lab))

df <- df %>%
  mutate(LI3_forcedbyparents = factor(LI3_forcedbyparents,
                                      levels = vec_likert5_1,
                                      labels = vec_internalization_lab))

df <- df %>%
  mutate(LI4_forcedbyteachers = factor(LI4_forcedbyteachers,
                                       levels = vec_likert5_1,
                                       labels = vec_internalization_lab))

df <- df %>%
  mutate(LI5_noschool = factor(LI5_noschool,
                               levels = vec_likert5_1,
                               labels = vec_internalization_lab))

df <- df %>%
  mutate(LI6_promisedgifts = factor(LI6_promisedgifts,
                                    levels = vec_likert5_1,
                                    labels = vec_internalization_lab))

df <- df %>%
  mutate(LI7_avoidscolding = factor(LI7_avoidscolding,
                                    levels = vec_likert5_1,
                                    labels = vec_internalization_lab))



###' ###########################################################################'
###' 
###' Name variables
###' 
###' (3) Effort & Ability: Q42-Q54
###' (4) Choice Scale: Q55-Q63
###' (5) Persistence: Q64-Q68
###' 
###' 

### change name Q42-Q54
vec_eaa <- paste0("EAA",seq(13))
df <- df %>%
  rename_at(vars(Q42:Q54), ~vec_eaa)


### change name Q55-Q63
vec_cs <- paste0("CS",seq(9))
df <- df %>%
  rename_at(vars(Q55:Q63), ~vec_cs)


### change name Q64-Q68
vec_per <- paste0("PER",seq(5))
df <- df %>%
  rename_at(vars(Q64:Q68), ~vec_per)



###' ###########################################################################'
###' 
###' Name values
###' 
###' (3) Effort & Ability: Q42-Q54
###' (4) Choice Scale: Q55-Q63
###' (5) Persistence: Q64-Q68
###' 
###' 

### change coded values: likert5_1
vec_likert5_1 <- seq(5)
vec_internalization_lab <- c('Vdisagree','disagree','soso','agree','Vagree')


### (3) Effort & Ability
for (i in 1:13) {
  cha_variables <- paste0("EAA", i)
  vec_variables <- (!!cha_variables)    
  df <- df %>%
    mutate_all(vec_variables = factor(vec_variables,
                                  levels = vec_likert5_1,
                                  labels = vec_internalization_lab))
  return(df)
}
is.tibble(df)
df$EAA1


### (4) Choice Scale
for (i in 1:9) {
  cha_variables <- paste0("CS", i)
  vec_variables <- as.character(cha_variables)    
  df <- df %>%
    mutate(vec_variables = factor(vec_variables,
                                  levels = vec_likert5_1,
                                  labels = vec_internalization_lab))
}


### (5) Persistence
for (i in 1:5) {
  cha_variables <- paste0("PER", i)
  vec_variables <- as.character(cha_variables)    
  df <- df %>%
    mutate(vec_variables = factor(vec_variables,
                                  levels = vec_likert5_1,
                                  labels = vec_internalization_lab))
}



###' ###########################################################################'

### change coded values
df <- df %>%
  mutate(EAA1 = factor(EAA1,
                       levels = vec_likert5_1,
                       labels = vec_internalization_lab))

df <- df %>%
  mutate(CS9 = factor(CS9,
                       levels = vec_likert5_1,
                       labels = vec_internalization_lab))


df <- df %>%
  mutate(PER1 = factor(PER1,
                      levels = vec_likert5_1,
                      labels = vec_internalization_lab))



###' ###########################################################################
###' 
###' Name variables
###' 
###' (6) Math Anxiety: Q69-Q77
###' (7) State Anxiety: Q78-Q87
###' (8) Trait Anxiety: Q89-Q98
###' 
###' 

### change name Q69-Q77
vec_ma <- paste0("MA",seq(9))
df <- df %>%
  rename_at(vars(Q69:Q77), ~vec_ma)


### change name Q78-Q87
vec_sa <- paste0("SA",seq(10))
df <- df %>%
  rename_at(vars(Q78:Q87), ~vec_sa)


### change name Q89-Q98
vec_ta <- paste0("TA",seq(10))
df <- df %>%
  rename_at(vars(Q89:Q98), ~vec_ta)



###' ###########################################################################'
###' 
###' Name values
###' 
###' (6) Math Anxiety: Q69-Q77
###' (7) State Anxiety: Q78-Q87
###' (8) Trait Anxiety: Q89-Q98
###' 
###' 

### (6) Math Anxiety
### change coded values: likert5_2
vec_likert5_2 <- seq(5)
vec_mathA_lab <- c('lowA','someA','moderateA','abitA','highA')

df <- df %>%
  mutate(MA1 = factor(MA1,
                       levels = vec_likert5_2,
                       labels = vec_mathA_lab))


### (7) State Anxiety
### change coded values: likert4_1
vec_likert4_1 <- seq(4)
vec_stateA_lab <- c('notatall','somewhat','moderately','verymuch')

df <- df %>%
  mutate(SA1 = factor(SA1,
                      levels = vec_likert4_1,
                      labels = vec_stateA_lab))


### (8) Trait Anxiety
### change coded values: likert4_2
vec_likert4_2 <- seq(4)
vec_traitA_lab <- c('never','sometimes','often','always')

df <- df %>%
  mutate(TA10 = factor(TA10,
                       levels = vec_likert4_2,
                       labels = vec_traitA_lab))



###' ###########################################################################'
###' 
###' Name variables & values
###' 
###' (9) others
###' 
###' 

### Checking item: Q99-Q101
#vec_chk <- c('CHECK1', 'CHECK2', 'CHECK3')
#df <- df %>%
#  rename_at(vars(Q99:Q101), ~vec_chk)

df <- df %>%
  rename('CHECK1' = Q99,
         'CHECK2' = Q100,
         'CHECK3' = Q101)

df$CHECK1 <- ifelse(df$CHECK1 == '19','pass','fail')
df$CHECK1 <- ifelse(!is.na(df$CHECK1),df$CHECK1,'fail')

df$CHECK2 <- ifelse(df$CHECK2 == '20','pass','fail')
df$CHECK2 <- ifelse(!is.na(df$CHECK2),df$CHECK2,'fail')

df$CHECK3 <- ifelse(df$CHECK3 == '21','pass','fail')
df$CHECK3 <- ifelse(!is.na(df$CHECK3),df$CHECK3,'fail')

#df <- df %>%
#  mutate(CHECK2 = factor(CHECK2,
#                         levels = c(19,20,21,NA),
#                         labels = c('fail','pass','fail','fail')))

#df <- df %>%
#  mutate(CHECK3 = factor(CHECK3,
#                         levels = c(19,20,21,NA),
#                         labels = c('fail','fail','pass','fail')))


### Choice Condition: Choice
df %>%
  count(choice)

df <- df %>%
  mutate(choice = factor(choice,
                      levels = c(0,1),
                      labels = c('N_Choice','Y_Choice')))


### Explanatory Variables
df <- df %>%
  rename_with(~gsub(".", "_", .x, fixed = TRUE))



###' ###########################################################################'
###' 
###' Data savings
###' 
###'

write.csv(df,file="phm_datamanagement01.1.15.csv")
saveRDS(df, file ="phm_datamanagement01.1.15.rds")



###' ###########################################################################'
###' 
###' LESSONS
###' 
###' 1. make a function where
###' - calculating % of responses in each variables: ratedf(data name, variable name)
###' - checking missing data
###' 
###' 2. how to relocate the order of variable
###' df <- df %>%
###'   relocate(race_text, .after = race)
###'   
###' 3. how to study tidyverse
###' - use youtube
###' - use cheatsheet
###' 
###' 4. Steps of Data management
###' (1) Data cleaning
###' - rename variables and values in the dataset
###' - check range of values of each variable, and see whether it makes sense
###' (2) Data manipulation
###' 
###' 5. LBNL
###' - learn `functional programming`, but it's bit hard
###' - use `stack overflow` website
###' 
###'

