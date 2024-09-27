###############
# Sample Split
###############
library(pacman)

p_load(tidyverse, 
       rio, caret)

########## clean all 
rm(list=ls())

########## paths

root<- "C:\\Users\\Andres Felipe\\OneDrive - Universidad de los Andes\\Research Proyects\\ML_GEIH"
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
leave_out_sample <- workers %>%
  group_by(city) %>%
  distinct(house) %>%          # Get unique households per city
  sample_frac(0.20) %>%                # Randomly sample 20% of households
  ungroup()


dim(leave_out_sample)

leave_out <- workers %>%
  semi_join(leave_out_sample, by = "house")  # Keep only the sampled 20% households (leave-out sample)


## workers ready to select model
workers<- workers %>%
  anti_join(leave_out_sample, by = "house")

## final analysis sample has this # of households
length(unique(workers$house))

## leave out has this # of households
length(unique(leave_out$house))


### save leave out 
saveRDS( leave_out,  file= paste0(clean, "\\employees_leave_out.rds"))


#### Save analysis data with folds
## create folds


set.seed(154320)

households_with_folds <- workers %>%
  distinct(house) %>%             # Get unique households
  mutate(fold = sample(1:10, n(), replace = TRUE))  


workers <- workers %>%
  left_join(households_with_folds, by = "house") 


workers %>% select(house, household, person,  fold) %>%
  view()

## save analysis 
saveRDS( workers,  file= paste0(clean , "\\employees_analysis.rds"))
















discart<- c("house" , "household","person",   "Hogar", "P6016",  
            "oci", "job_type", "unionized" , "student", "monthly_mw50", "monthly_mw75",
            "monthly_mw125","inglabo")

workers_final<-workers%>% select(-all_of(discart))







