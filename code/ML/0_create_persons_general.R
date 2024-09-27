
###################
## Import Persons General 
## characteristic data
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


df01<- import(paste0(geih2012,"\\Enero/01/Cabecera - Características generales (Personas).sav") )
df02<- import(paste0(geih2012,"\\Febrero/02/Cabecera - Características generales (Personas).sav") )
df03<- import(paste0(geih2012,"\\Marzo/03/Cabecera - Características generales (Personas).sav") )
df04<- import(paste0(geih2012,"\\Abril/04/Cabecera - Características generales (Personas).sav") )
df05<- import(paste0(geih2012,"\\Mayo/05/Cabecera - Características generales (Personas).sav") )
df06<- import(paste0(geih2012,"\\Junio/06/Cabecera - Características generales (Personas).sav") )
df07<- import(paste0(geih2012,"\\Julio/07/Cabecera - Características generales (Personas).sav") )
df08<- import(paste0(geih2012,"\\Agosto/08/Cabecera - Características generales (Personas).sav") )
df09<- import(paste0(geih2012,"\\Septiembre/09/Cabecera - Características generales (Personas).sav") )
df10<- import(paste0(geih2012,"\\Octubre/10/Cabecera - Características generales (Personas).sav") )
df11<- import(paste0(geih2012,"\\Noviembre/11/Cabecera - Características generales (Personas).sav") )
df12<- import(paste0(geih2012,"\\Diciembre/12/Cabecera - Características generales (Personas).sav") )


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


  dropvars<- c("P6030", "P6030s1", "P6030s3", "Regis", "P6090",
               "P6140" , "P6150", "P6100", "P6110", "P6120", "P6125",
               "P6175", "P6175", "P6175","Dpto")
  
  general_ch<- df %>%  
    select(-all_of(dropvars))
  
### Rename
  
    general_ch<- general_ch %>%
     rename(sex=P6020, 
            age=P6040, 
            relation_with_head= P6050,
            marital_status= P6070, 
            literacy=P6160, 
            student=P6170, 
            maxedulevel01=P6210,
            maxedulevel02=P6220, 
            maxgrade= P6210s1,
            schyears= Esc, 
            city= Area, 
            month=Mes)
  
    ###### Feature engineering
    
    
    general_ch<- general_ch %>%
      mutate(sex= factor(sex, labels = c("Male", "Female")),
             relation_with_head= factor(relation_with_head, labels=c("Head", 
                                                             "Partner",
                                                            "Children",
                                                            "Grandchildren",
                                                            "Other Relative", 
                                                            "Domestic Service", 
                                                            "Pensioner", 
                                                            "Employeed", 
                                                            "Other no relative")),
             student= factor(student, labels = c("Yes", "No")))
      
    
    
    general_ch<- general_ch %>% 
            mutate(maxedulevel=case_when(
              maxedulevel01==1 ~ 1,
              maxedulevel01==2 ~ 2,  
              maxedulevel01==3 ~ 3, 
              maxedulevel01==4 ~ 4, 
              maxedulevel01==5 ~ 5, 
              maxedulevel02==3 ~ 6, 
              maxedulevel02==4 ~ 7, 
              maxedulevel02==5 ~ 8, 
              maxedulevel02==9 |maxedulevel01==9 ~ 9 )) %>%
            mutate(maxedulevel=factor(maxedulevel, 
                                       labels = c("None",
                                                  "Pre-School",  
                                                  "Primary education", 
                                                  "Lower secondary education ", 
                                                  "Upper secondary education", 
                                                  "Technical or technological studies", 
                                                  "Undergraduate", 
                                                  "Post Graduate", 
                                                  "Don't Know")
                                       ))  
              
    
    ## drop vars
    general_ch <- general_ch %>%
         select(-maxedulevel02, -maxedulevel01)
      
        
    #### rename to merge
    general_ch<-general_ch %>%
      rename(house= Directorio, 
             household=Secuencia_p, 
             person=Orden)
    
    
    ### save data
    
    out_path<- paste0(data, "\\clean_prediction\\2012")
    saveRDS( general_ch,  file= paste0(out_path, "\\general_characteristics_clean.rds"))
    
######################################################

    
   
#### Cool graphs 


p_load(PrettyCols)

ggplot(general_ch, aes(y=relation_with_head, x=age, color=sex))+
  geom_boxplot()+
  theme_bw() +
  scale_color_pretty_d("Summer")
    


ggplot(general_ch, aes(y=maxedulevel, x=age, color=sex))+
  geom_boxplot()+
  theme_bw() +
  scale_color_pretty_d("Summer")




