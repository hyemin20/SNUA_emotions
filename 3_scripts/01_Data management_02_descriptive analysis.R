###' ###########################################################################'
###' 
###' Project(project name): Emotion project
###' 
###' Category(stage in the project): Data management
###' 
###' Task(specific task in the category): Data Visualization
###' 
###' Data(data source): `survey item data`
###' 
###' Date: 2022-01-13
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



###' ###########################################################################'
###' 
###' Descriptive Analysis
###' 
###' (1) Data Visualization
###'     : USE Rmarkdown
###'  
###' 



###' ###########################################################################'
###' 
###' Descriptive Analysis
###' 
###' (2) missing values
###' 
###' 




###' ###########################################################################'
###' 
###' Data savings
###' 
###'

write.csv(df,file="phm_datamanagement02.csv")
saveRDS(df, file ="phm_datamanagement02.rds")



###' ###########################################################################'
###' 
###' LESSONS
###' 
###' 1. 
###' 
###' 
###' 

