###############
# Sample Split
###############
library(pacman)

p_load(tidyverse, 
       rio, caret)

########## clean all 
rm(list=ls())

########## paths

if (getwd()=="/export/home/rcsguest/rcs_arengifojaramill/Documents/GitHub/ML_GEIH/code/ML") {
  root<- "/export/home/rcsguest/rcs_arengifojaramill/Documents/GitHub/ML_GEIH"
  
}  else {
  root<- "C:/Users/Andres Felipe/OneDrive - Universidad de los Andes/Research Proyects/ML_GEIH"
}

data<- paste0(root, "\\data")
raw<- paste0(data, "\\raw")
geih2012<- paste0(raw, "\\GEIH\\2012")
clean<-  paste0(data, "\\clean_prediction\\2012")



############
######## Import and prepare 
############

workers<- readRDS(paste0(clean, "\\final_workes_only.rds"))

# All sample has this number of households
length(unique(workers$house))


set.seed(24639763)
main_out <- workers %>%
  group_by(city) %>%
  distinct(house) %>%          # Get unique households per city
  sample_frac(0.40) %>%                # Randomly sample 40% of households for both test and leave out
  ungroup()

### leave out sample

set.seed(673575)
leave_out_sample <- main_out %>%
  group_by(city) %>%
  distinct(house) %>%          # Get unique households per city
  sample_frac(0.50) %>%                # Randomly sample 40% of households
  ungroup()

dim(leave_out_sample)

### test sample
test_sample <- main_out %>%
  anti_join(leave_out_sample, by = "house")


dim(test_sample)


leave_out <- workers %>%
  semi_join(leave_out_sample, by = "house")  # Keep only the sampled 20% households (leave-out sample)
# check
length(unique(leave_out$house))



test <- workers %>%
  semi_join(test_sample, by = "house")  # Keep only the sampled 20% households (leave-out sample)
# check
length(unique(test$house))




################### Keep drop leave out and test househoolds

## workers ready to select model
workers<- workers %>%
  anti_join(main_out, by = "house")



###################  final data sizes

## final analysis sample has this # of households
length(unique(workers$house))

## leave out has this # of households
length(unique(leave_out$house))


## test has this # of households
length(unique(test$house))

##############
### save data
##############

saveRDS( leave_out,  file= paste0(clean, "\\employees_leave_out.rds"))

saveRDS( test,  file= paste0(clean, "\\employees_test.rds"))

######### Part 2 generate Folds
#### Save analysis data with folds
## create folds

nfolds<- 5

set.seed(154320)
households_with_folds <- workers %>%
  distinct(house) %>%             # Get unique households
  mutate(fold = sample(1:nfolds, n(), replace = TRUE))  


workers <- workers %>%
  left_join(households_with_folds, by = "house") 


workers %>% select(house, household, person,  fold) %>%
  view()

## save analysis 
saveRDS( workers,  file= paste0(clean , "\\employees_analysis.rds"))


#### 
# end 
#####
