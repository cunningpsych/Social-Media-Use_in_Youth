###############################################
#### Social Media Use in Youth Project ####
# Script for Data Simulation
# Created on Nov 3, 2025, by Natalie Cunningham
# Checked on DATE, by NAME
###############################################

#### workspace setup ####
library(groundhog)
groundhog.library(dplyr, "2025-04-02")
groundhog.library(faux, "2025-04-02")
groundhog.library(summarytools, "2025-04-02")
groundhog.library(missMethods, "2025-04-02")

sessionInfo()
# R version 4.4.1 
# groundhog_3.2.3
# dplyr_1.1.4
# faux_1.2.2   
# summarytools_1.1.3
# missMethods_0.4.0

# setting seed for reproducibility
set.seed(2005)

###############################################
#### Data Simulation ####

# Reading in the data dictionary to reference for creating different items in the simulated data set
dict <- read.csv("/Users/natal/Dropbox/Social-Media-Use_in_Youth/Data/2025-10-29_youth-media-use_data-dictionary.csv")

# Creating an empty data frame and populate with id variable based on 14,734 subjects

dat_sim <- data.frame(id=c(1:14734))
head(dat_sim)

### Simulating each individual item

# Demographics (all percentages are referenced from the survey demographics):

dat_sim$grade_sim <- sample(1:2, size = 14734,
                            replace = TRUE,
                            prob = c(.40,.60))
  table(dat_sim$grade_sim)
  
dat_sim$age_sim <- sample(1:2, size = 14734,
                          replace = TRUE,
                          prob = c(.50,.50))
  table(dat_sim$age_sim)

dat_sim$sex_sim <- sample(1:4, size = 14734,
                          replace = TRUE,
                          prob = c(.45,.45,.05,.05))
  table(dat_sim$sex_sim)

dat_sim$race_sim <- sample(1:3, size = 14734,
                           replace = TRUE,
                           prob = c(.20,.50,.25))
  table(dat_sim$race_sim)
  
dat_sim$residence_sim <- sample(1:3, size = 14734,
                                replace = TRUE,
                                prob = c(.5,.15,.75))
  table(dat_sim$residence_sim)

# Social media use:

dat_sim$smuse_1_sim <- sample(1:6, size = 14734, 
                              replace = TRUE, 
                              prob = c(.05,.15,.4,.4,.15,.05))
  table(dat_sim$smuse_1_sim)

dat_sim$smuse_2_sim <- sample(1:7, size = 14734,
                              replace=TRUE,
                              prob = c(.05,.10,.15,.35,.15,.10,.05))
  table(dat_sim$smuse_2_sim)

dat_sim$smuse_3_sim <- sample(1:7, size = 14734,
                              replace = TRUE,
                              prob = c(.05,.10,.15,.35,.15,.10,.05))
  table(dat_sim$smuse_3_sim)

dat_sim$smuse_4_sim <- sample(1:7, size = 14734,
                              replace = TRUE,
                              prob = c(.05,.10,.15,.35,.15,.10,.05))
  table(dat_sim$smuse_4_sim)

# Impulsivity:

dat_sim$impulse_1_sim <- sample(1:5, size = 14734,
                                replace = TRUE,
                                prob = c(.35,.15,.1,.05,.01))
  table(dat_sim$impulse_1_sim)

dat_sim$impulse_2_sim <- sample(1:5, size = 14734,
                                replace = TRUE,
                                prob = c(.35,.15,.1,.05,.01))
  table(dat_sim$impulse_2_sim)

dat_sim$impulse_3_sim <- sample(1:2, size = 14734,
                                replace = TRUE,
                                prob = c(.75,.25))
  table(dat_sim$impulse_3_sim)

dat_sim$impulse_4_sim <- sample(1:6, size = 14734, 
                                replace = TRUE, 
                                prob = c(.50,.1,.05,.03,.02,.01))
  table(dat_sim$impulse_4_sim)

dat_sim$impulse_5_sim <- sample(1:6, size = 14734, 
                                replace = TRUE, 
                                prob = c(.60,.35,.05,.01,.01,.01))  
  table(dat_sim$impulse_5_sim)

dat_sim$impulse_6_sim <- sample(1:6, size = 14734, 
                                replace = TRUE, 
                                prob = c(.2,.1,.15,.2,.15,.1))
  table(dat_sim$impulse_6_sim)

dat_sim$impulse_7_sim <- sample(1:6, size = 14734, 
                                replace = TRUE, 
                                prob = c(.15,.3,.3,.15,.05,.05))
  table(dat_sim$impulse_7_sim)

dat_sim$impulse_8_sim <- sample(1:7, size = 14734,
                                replace = TRUE,
                                prob = c(.35,.4,.15,.05,.05,.05,.05))  
  table(dat_sim$impulse_8_sim)

dat_sim$impulse_9_sim <- sample(1:5, size = 14734,
                                replace = TRUE,
                                prob = c(.35,.25,.15,.10,.05))
  table(dat_sim$impulse_9_sim)
  
dat_sim$impulse_10_sim <- sample(1:5, size = 14734,
                                 replace = TRUE,
                                 prob = c(.15,.1,.15,.15,.1))
  table(dat_sim$impulse_10_sim)

dat_sim$impulse_11_sim <- sample(1:5, size = 14734,
                                 replace = TRUE,
                                 prob = c(.35,.05,.01,.01,.01))  
  table(dat_sim$impulse_11_sim)

dat_sim$impulse_12_sim <- sample(1:5, size = 14734,
                                 replace = TRUE,
                                 prob = c(.4,.25,.1,.05,.01))  
  table(dat_sim$impulse_12_sim)

dat_sim$impulse_13_sim <- sample(1:5, size = 14734,
                                 replace = TRUE,
                                 prob = c(.4,.25,.1,.05,.01))  
  table(dat_sim$impulse_13_sim)

dat_sim$impulse_14_sim <- sample(1:5, size = 14734,
                                 replace = TRUE,
                                 prob = c(.4,.25,.1,.05,.01))  
  table(dat_sim$impulse_14_sim)

# Depression:
  
dat_sim$depression_1_sim <- sample(1:3, size = 14734,
                                   replace = TRUE,
                                   prob = c(.15,.65,.2))
  table(dat_sim$depression_1_sim)  

dat_sim$depression_2_sim <- sample(1:5, size = 14734,
                                   replace = TRUE,
                                   prob = c(.4,.2,.15,.1,.05))  
  table(dat_sim$depression_2_sim)

dat_sim$depression_3_sim <- sample(1:5, size = 14734,
                                   replace = TRUE,
                                   prob = c(.4,.2,.15,.1,.05))  
  table(dat_sim$depression_3_sim)

dat_sim$depression_4_sim <- sample(1:5, size = 14734,
                                   replace = TRUE,
                                   prob = c(.4,.2,.15,.1,.05))
  table(dat_sim$depression_4_sim)

dat_sim$depression_5_sim <- sample(1:5, size = 14734,
                                   replace = TRUE,
                                   prob = c(.4,.2,.15,.1,.05))  
  table(dat_sim$depression_5_sim)

dat_sim$depression_6_sim <- sample(1:5, size = 14734,
                                   replace = TRUE,
                                   prob = c(.25,.25,.1,.05,.01))  
  table(dat_sim$depression_6_sim)

dat_sim$depression_7_sim <- sample(1:5, size = 14734,
                                   replace = TRUE,
                                   prob = c(.25,.25,.1,.05,.05))  
  table(dat_sim$depression_7_sim)

dat_sim$depression_8_sim <- sample(1:5, size = 14734,
                                   replace = TRUE,
                                   prob = c(.4,.15,.15,.1,.05))  
  table(dat_sim$depression_8_sim)

dat_sim$depression_9_sim <- sample(1:5, size = 14734,
                                   replace = TRUE,
                                   prob = c(.4,.15,.15,.1,.05))  
  table(dat_sim$depression_9_sim)

dat_sim$depression_10_sim <- sample(1:5, size = 14734,
                                    replace = TRUE,
                                    prob = c(.4,.15,.15,.1,.05))
  table(dat_sim$depression_10_sim)


# Anxiety:
  
dat_sim$anxiety_1_sim <- sample(1:5, size = 14734,
                                replace = TRUE,
                                prob = c(.05,.05,.1,.15,.15))
  table(dat_sim$anxiety_1_sim)

dat_sim$anxiety_2_sim <- sample(1:5, size = 14734,
                                replace = TRUE,
                                prob = c(.05,.05,.1,.15,.1))  
  table(dat_sim$anxiety_2_sim)
  
# A few quick checks:
  
dim(dat_sim)
head(dat_sim)

dat_sim %>% dplyr::select(smuse_1_sim:anxiety_2_sim) %>% summarytools::freq()

# Creating missing data for a more realistic simulated data set:

dat_sim <- missMethods::delete_MCAR(dat_sim,.3, "age_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"sex_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.3,"race_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"residence_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"smuse_1_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"smuse_2_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"smuse_3_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"smuse_4_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"impulse_1_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"impulse_2_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"impulse_3_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.01,"impulse_4_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"impulse_5_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"impulse_6_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.01,"impulse_7_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"impulse_8_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"impulse_9_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"impulse_10_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"impulse_11_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"impulse_12_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"impulse_13_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"impulse_14_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.01,"depression_1_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"depression_2_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"depression_3_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"depression_4_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"depression_5_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"depression_6_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"depression_7_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"depression_8_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"depression_9_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"depression_10_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.1,"anxiety_1_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim,.05,"anxiety_2_sim")

# Checking the dataset to make sure it looks right
dat_sim %>% dplyr::select(age_sim:anxiety_2_sim) %>% summarytools::freq()
dat_sim

# Saving as a .csv file for preregistration and simulated analysis use
write.csv(dat_sim, "./Data/2025-11-13_youth-media-use_simulated-data.csv", row.names=FALSE)

