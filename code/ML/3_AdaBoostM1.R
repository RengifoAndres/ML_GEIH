###################
##  Simple AdaBoost.M1
###################

########### packages

library(pacman)

p_load(tidyverse, 
       rio, 
       adabag, 
       doParallel, 
       caret, 
       pROC, 
       foreach, 
       PRROC)


########## clean all 
rm(list=ls())


########## paths

if (getwd()=="/export/home/rcsguest/rcs_arengifojaramill/Documents/GitHub/ML_GEIH/code/ML") {
  root<- "/export/home/rcsguest/rcs_arengifojaramill/Documents/GitHub/ML_GEIH"
  
}  else {
  root<- "C:/Users/Andres Felipe/OneDrive - Universidad de los Andes/Research Proyects/ML_GEIH"
}


data<- paste0(root, "/data")
raw<- paste0(data, "/raw")
geih2012<- paste0(raw, "/GEIH/2012")
clean<-  paste0(data, "/clean_prediction/2012")



############
######## Import and prepare data.
############

workers<- readRDS(paste0(clean, "/employees_analysis.rds"))

discart<- c("house" , "household","person",   "Hogar", "P6016",  
            "oci", "job_type", "unionized" , "student", "monthly_mw50", "monthly_mw75",
            "monthly_mw125","inglabo", "mw_worker125" , "mw_worker50" )

workers_final<-workers%>% 
  select(-all_of(discart))

table(workers_final$mw_worker75)[2]/nrow(workers_final)

## use the function boosting() with formula. 


########## Test
#########

train<- workers_final %>%
  filter(fold==4 )
test<- workers_final %>%
  filter(fold==1 )

y<- train$mw_worker75
X<- model.matrix(mw_worker75~.-1 -fold, train )

X_test<- model.matrix(mw_worker75~.-1 -fold, test )
y_test<-  test$mw_worker75


ada<- boosting(mw_worker75~. -fold, 
                 data=train, 
               mfinal = 10, 
               coeflearn = 'Breiman', 
               boos=TRUE,
               rpart.control(maxdepth = 1))






