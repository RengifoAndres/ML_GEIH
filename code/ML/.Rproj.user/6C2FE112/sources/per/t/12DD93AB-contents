
###################
##  Create 
## ocupados data
###################

library(pacman)

p_load(tidyverse, 
       rio)

## clean all. 
rm(list=ls())


### Paths 

root<- "C:\\Users\\Andres Felipe\\OneDrive - Universidad de los Andes\\Research Proyects\\ML_GEIH"

data<- paste0(root, "\\data")
raw<- paste0(data, "\\raw")
geih2012<- paste0(raw, "\\GEIH\\2012")


###################
## Importing Data
###################




df01<- import(paste0(geih2012,"\\Enero/01/Cabecera - Ocupados.sav") )
df02<- import(paste0(geih2012,"\\Febrero/02/Cabecera - Ocupados.sav") )
df03<- import(paste0(geih2012,"\\Marzo/03/Cabecera - Ocupados.sav") )
df04<- import(paste0(geih2012,"\\Abril/04/Cabecera - Ocupados.sav") )
df05<- import(paste0(geih2012,"\\Mayo/05/Cabecera - Ocupados.sav") )
df06<- import(paste0(geih2012,"\\Junio/06/Cabecera - Ocupados.sav") )
df07<- import(paste0(geih2012,"\\Julio/07/Cabecera - Ocupados.sav") )
df08<- import(paste0(geih2012,"\\Agosto/08/Cabecera - Ocupados.sav") )
df09<- import(paste0(geih2012,"\\Septiembre/09/Cabecera - Ocupados.sav") )
df10<- import(paste0(geih2012,"\\Octubre/10/Cabecera - Ocupados.sav") )
df11<- import(paste0(geih2012,"\\Noviembre/11/Cabecera - Ocupados.sav") )
df12<- import(paste0(geih2012,"\\Diciembre/12/Cabecera - Ocupados.sav") )


#########################
#        keep only 
#   variables in common
#########################

## names to lowercase

colnames(df01) <- tolower(colnames(df01))
colnames(df02) <- tolower(colnames(df02))
colnames(df03) <- tolower(colnames(df03))
colnames(df04) <- tolower(colnames(df04))
colnames(df05) <- tolower(colnames(df05))
colnames(df06) <- tolower(colnames(df06))
colnames(df07) <- tolower(colnames(df07))
colnames(df08) <- tolower(colnames(df08))
colnames(df09) <- tolower(colnames(df09))
colnames(df10) <- tolower(colnames(df10))
colnames(df11) <- tolower(colnames(df11))
colnames(df12) <- tolower(colnames(df12))


## get the variable names 

colnames01<- colnames(df01)
colnames02<- colnames(df02)
colnames03<- colnames(df03)
colnames04<- colnames(df04)
colnames05<- colnames(df05)
colnames06<- colnames(df06)
colnames07<- colnames(df07)
colnames08<- colnames(df08)
colnames09<- colnames(df09)
colnames10<- colnames(df10)
colnames11<- colnames(df11)
colnames12<- colnames(df12)

############ get the list of common variables

common<- Reduce(intersect, list(colnames01, 
                   colnames02,
                   colnames03,
                   colnames04,
                   colnames05,
                   colnames06,
                   colnames07,
                   colnames08,
                   colnames09,
                   colnames10,
                   colnames11,
                   colnames12))

df01<- df01 %>%  
    select(all_of(common))

df02<- df02 %>%  
  select(all_of(common))

df03<- df03 %>%  
  select(all_of(common))

df04<- df04 %>%  
  select(all_of(common))

df05<- df05 %>%  
  select(all_of(common))

df06<- df06 %>%  
  select(all_of(common))

df07<- df07 %>%  
  select(all_of(common))

df08<- df08 %>%  
  select(all_of(common))

df09<- df09 %>%  
  select(all_of(common))

df10<- df10 %>%  
  select(all_of(common))

df11<- df11 %>%  
  select(all_of(common))

df12<- df12 %>%  
  select(all_of(common))


##
df<- data.frame()

df<- df %>%
  rbind(df01)%>%
  rbind(df02)%>%
  rbind(df03)%>%
  rbind(df04)%>%
  rbind(df05)%>%
  rbind(df06)%>%
  rbind(df07)%>%
  rbind(df08)%>%
  rbind(df09)%>%
  rbind(df10)%>%
  rbind(df11)%>%
  rbind(df12)

rm(df01,
   df02,
   df03,
   df04, 
   df05,
   df06,
   df07, 
   df08,
   df09,
   df10,
   df11,
   df12)


##################
##  Selecting and renaming vars
##################

## p7180 dice si esta suscrito a un sindicato
## p6430 tipo de trabajo

ocupados<- df%>% 
  select(directorio,  secuencia_p, orden, inglabo, oci, p6430, p7180)

ocupados<-ocupados %>%
  rename(house= directorio, 
         household=secuencia_p, 
         person=orden, 
         unionized=p7180,
         job_type=p6430)

ocupados<-ocupados %>% 
  mutate(unionized= ifelse(unionized == 2, 0, unionized))%>%
  mutate(job_type=factor(job_type, 
                            labels = c("Employee in a private company",
                                       "Employee in the government",  
                                       "Domestic employee", 
                                       "Self-employed worker", 
                                       "Employer or boss", 
                                       "Unpaid family worker", 
                                       "Unpaid worker in businesses", 
                                       "Day laborer", 
                                       "Other")
  ))  





out_path<- paste0(data, "\\clean_prediction\\2012")
saveRDS( ocupados,  file= paste0(out_path, "\\ocupados_clean.rds"))





p_load(PrettyCols)

ocupados %>% 
  mutate(inglabo= log(inglabo)) %>%
  ggplot( aes(y=factor(p6430), x=inglabo, color= factor(p7180)))+
  geom_boxplot()+
  geom_vline(xintercept = log(566700), color = "red", linetype = "dashed")+
  theme_bw() +
  scale_color_pretty_d("Summer")



ocupados %>% 
  mutate(inglabo= log(inglabo)) %>%
  ggplot( aes( x=inglabo))+
  geom_histogram()+
  geom_vline(xintercept = log(566700), color = "red", linetype = "dashed")
  theme_bw() +
  scale_color_pretty_d("Summer")

