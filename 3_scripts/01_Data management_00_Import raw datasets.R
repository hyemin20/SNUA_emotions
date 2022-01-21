###' ###########################################################################
###' 
###' Project(project name): Emotion project
###' 
###' Category(stage in the project): Data management
###' 
###' Task(specific task in the category): Import raw datasets
###' 
###' Data(data source): `test data`
###' 
###' Date: 2022-01-07
###' 
###' Author: Hyemin Park(`hyemin.park@snu.ac.kr`)
###' 
###'

###' ###########################################################################
###' 
###' Basic settings
###' 
###' 

### Start with clean state
gc(); rm(list=ls())


### Set working directory and data directory
work_dir <- c("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/Emotions")
data_dir <- file.path(work_dir, "datasets")


### Call libraries
library(tidyverse)
library(RSQLite)
library(lubridate)


### Call functions
list.files(file.path(work_dir, "functions"), full.names = TRUE) %>% walk(source)



###' ###########################################################################
###' 
###' (1) Test data - `Numeric` files
###' 
###' (2) Test data - `Logic` files
###' 
###' (3) Duration of the test
###' 
###' Combine two files
###' 
###' 

### Set folder path


### Check resulting data


###' ###########################################################################
###' 
###' LESSONS: Steps for data cleaning
###' 
###' 1. rename variables
###' 
###' 2. designate form of the variables
###' 
###' 3. descriptive statistics of each variable
###'    (mean, std, wrangling, check patterns of missing values, etc)
###'    
###' 4. fully understand datasets
###' 
###' 


