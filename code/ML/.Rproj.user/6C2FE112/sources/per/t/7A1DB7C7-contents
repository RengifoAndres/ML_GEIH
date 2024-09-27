###################
##  clean and merge
###################

########### packages

library(pacman)

p_load(tidyverse, 
       rio)

########## clean all 
rm(list=ls())

########## paths

root<- "C:\\Users\\Andres Felipe\\OneDrive - Universidad de los Andes\\Research Proyects\\ML_GEIH"

data<- paste0(root, "\\data")
raw<- paste0(data, "\\raw")
geih2012<- paste0(raw, "\\GEIH\\2012")
clean<-  paste0(data, "\\clean_prediction\\2012")



####################
#        Merge 
#  Prediction data
#####################

general_ch<- readRDS(paste0(clean, "\\general_characteristics_clean.rds"))
ocupados<- readRDS(paste0(clean, "\\ocupados_clean.rds"))
house_ch  <- readRDS(paste0(clean, "\\house_general_characteristics_clean.rds"))



all_persons<- general_ch  %>% 
          full_join(ocupados, by=c("house", "household","person")) %>%
          full_join(house_ch, by=c("house", "household") )


### select only cities
all_persons<- all_persons%>%
    filter(city!="")


  # number of minors 
all_persons <- all_persons %>% 
  mutate(flag = ifelse(age <= 5, 1, 0))  %>%
  group_by(house, household) %>%
  mutate(nminors = sum(flag)) %>%
  select(-flag) %>% 
  ungroup()

### get the number of children of sons of the household  head
all_persons <- all_persons %>% 
  mutate(flag = ifelse(relation_with_head == "Children" | relation_with_head == "Grandchildren" , 1, 0))  %>%
  group_by(house, household) %>%
  mutate(offspring = sum(flag)) %>%
  select(-flag) %>% 
  ungroup()

## get the number of brothers of the childrens

all_persons <- all_persons %>% 
  mutate(flag = ifelse(relation_with_head == "Children", 1, 0))  %>%
  group_by(house, household) %>%
  mutate(sons = sum(flag)) %>%
  select(-flag) %>% 
  ungroup() %>%
  mutate(brothers = ifelse(relation_with_head == "Children",sons-1 , 0))  
  
## get the dummy of older brother 

all_persons <- all_persons %>% 
  mutate(flag = ifelse(relation_with_head == "Children", age, 0))  %>%
  group_by(house, household) %>%
  mutate(max_age_son = max(flag, na.rm = TRUE)) %>%
  select(-flag,) %>% 
  ungroup() %>%
  mutate(older_brother=ifelse(age==max_age_son & relation_with_head == "Children",1, 0 ))%>%
  select(-max_age_son)

######


all_persons %>% 
  select(house, household, person, sex , brothers,sons  , age, oci, relation_with_head, older_brother) %>%
  view()


all_persons<- all_persons %>%
  select(-Fex_c_2011,-Clase)


### minimum wage

workers<- all_persons %>%
  filter(oci==1)

minimum<- import( paste0(raw, "\\Other\\1.1.1.SLR_Serie historicasalariominimo.xlsx"))

mw<-minimum %>% 
  filter(year==2012) %>%
  select(monthly_mw)
 
mw75<- mw*0.75
mw50<- mw*0.5
mw125<- mw*1.25

workers<- workers %>%
  mutate(monthly_mw75=mw75, 
         monthly_mw50=mw50,
         monthly_mw125=mw125,)%>%
  mutate(mw_worker125= if_else(inglabo<= monthly_mw125, 1, 0))%>%
  mutate(mw_worker50= if_else(inglabo<= monthly_mw50, 1, 0))%>%
  mutate(mw_worker75= if_else(inglabo<= monthly_mw75, 1, 0))%>%  
  mutate(mw_worker50= factor(mw_worker50, labels = c("No", "Yes")),
         mw_worker75= factor(mw_worker75, labels = c("No", "Yes")), 
         mw_worker125= factor(mw_worker125, labels = c("No", "Yes")))



### table for classification
table(workers$mw_worker75)


### Dealing with missing
## I am just going to drop them all

workers<- workers %>% 
  drop_na() %>%
  filter(inglabo!= 0)


### Save the data 

out_path<- paste0(data, "\\clean_prediction\\2012")
saveRDS( workers,  file= paste0(out_path, "\\final_workes_only.rds"))


############


p_load(PrettyCols)

workers %>% 
  mutate(inglabo= log(inglabo)) %>%
  ggplot( aes(y=factor(mw_worker125), x=inglabo))+
  geom_boxplot()+
  geom_vline(xintercept = log(566700), color = "red", linetype = "dashed")+
  theme_bw() +
  scale_color_pretty_d("Summer")


workers %>% 
  mutate(inglabo= log(inglabo)) %>%
  ggplot( aes(y=factor(mw_worker50), x=inglabo))+
  geom_boxplot()+
  geom_vline(xintercept = log(566700), color = "red", linetype = "dashed")+
  theme_bw() +
  scale_color_pretty_d("Summer")


workers %>% 
  mutate(inglabo= log(inglabo)) %>%
  ggplot( aes(y=factor(mw_worker75), x=inglabo))+
  geom_boxplot()+
  geom_vline(xintercept = log(566700), color = "red", linetype = "dashed")+
  theme_bw() +
  scale_color_pretty_d("Summer")

