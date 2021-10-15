library(tidyverse)
library(ggplot2)
library(viridis)
library(tidyverse)
library(cowplot)
library(AICcmodavg)
library(betareg)
library(DataCombine)
library(performance)

meg_data = read.csv("Data/megan_unconnected_pa.csv")
summary(meg_data)

meg_datas<-meg_data%>% group_by(day,date,bottle,species)%>% 
  summarize(total=sum(total.present.in.drops), avg.pH=mean(pH))%>%
  mutate(proportion.present= ((total)/30 ))%>%
  mutate(occupancy=if_else(proportion.present > 0,1,0))%>%
  dplyr::select(-c(total))%>%
  pivot_wider(names_from = species, values_from=c(proportion.present,occupancy))%>%
  ungroup()%>%
  dplyr::select(-c(avg.pH,proportion.present_tetra,proportion.present_euplotes))%>%
  rename(pred.oc=occupancy_euplotes)%>%
  rename(prey.oc=occupancy_tetra)%>%
  add_column(connectivity=0)%>%
  add_column(structure="control")%>%
  add_column(position="H")%>%
  add_column(bottle.number="1")%>% 
  replace(is.na(.), 0)%>%
  mutate(replicate=if_else(bottle == "High1","A",if_else(bottle == "High2","B",
                                                                if_else(bottle == "High3","C",
                                                                        if_else(bottle == "High4","D",
                                                                                if_else(bottle == "High5","E",
                                                                                        if_else(bottle == "High6","F",
                                                                                                if_else(bottle == "High7","G","H"))))))))%>%
  filter(day>0)

###############################################################################################################################################################################
#Big Network Exp
Data = read.csv("Data/Net.2.data.csv")
summary(Data)
str(Data)

Datas<-Data%>%
  dplyr::select(-c(ln.prey,ln.pred, tetra.density,eup.density))
         
############################################################################################################################################
#Combine Data Sets
str(Datas)
str(meg_datas)
meg_datas$bottle.number<-as.integer(meg_datas$bottle.number)

all_pa_data<-full_join(Datas,meg_datas)
str(all_pa_data)
summary(all_pa_data)


all_pa_datas<-all_pa_data%>%mutate(total.bottle.nmbr=if_else(structure == "isolated","8",if_else(bottle == "dendritic","60","60")))%>%
  mutate(total.bottle.nmbr_per_rep=if_else(structure == "isolated","1",if_else(structure == "dendritic","15","15")))%>%
  mutate(total.bottle.nmbr_connect_per_rep=ifelse(structure == "isolated","1",ifelse(connectivity=="1","8",
                                                                                       ifelse(connectivity=="2" & structure == "dendritic" ,"1",
                                                                                               ifelse(structure == "dendritic" & connectivity=="3","6",
                                                                                                       ifelse(structure == "lattice" & connectivity=="2","4",
                                                                                                               ifelse(structure == "lattice" & connectivity=="3","8","3")))))))%>%
  mutate(total.bottle.nmbr_position_per_rep=ifelse(structure == "isolated","1",ifelse(position=="D","8",
                                                                                     ifelse(position=="A" & structure == "dendritic" ,"1",
                                                                                            ifelse(structure == "dendritic" & position=="B","2",
                                                                                                   ifelse(structure == "dendritic" & position=="C","4",
                                                                                                   ifelse(structure == "lattice" & position=="E","4",
                                                                                                          ifelse(structure == "lattice" & position=="F","8","3"))))))))%>%
  group_by(structure,replicate,bottle)%>%
  mutate(sampling_days=n())%>%
  group_by(replicate,structure, bottle.number, day)%>%
  mutate(nghbr_connect=ifelse(structure == "isolated","0",ifelse(connectivity=="1","3",
                                                                                   ifelse(connectivity=="2" & structure == "dendritic" ,"3",
                                                                                          ifelse(structure == "dendritic" & connectivity=="3","1.67",
                                                                                                 ifelse(structure == "lattice" & connectivity== "2","3",
                                                                                                        ifelse(structure == "lattice" & bottle.number== "6","2.67",
                                                                                                               ifelse(structure == "lattice" & bottle.number=="2","3",
                                                                                                                      ifelse(structure == "lattice" & bottle.number=="7","3.25",
                                                                                                                             ifelse(structure == "lattice" & bottle.number=="12","3",
                                                                                                                                    ifelse(structure == "lattice" & bottle.number=="3","3.33",
                                                                                                                                           ifelse(structure == "lattice" & bottle.number=="8","3.5",
                                                                                                                                                  ifelse(structure == "lattice" & bottle.number=="13","3.33",
                                                                                                                                                         ifelse(structure == "lattice" & bottle.number=="4","3",
                                                                                                                                                                ifelse(structure == "lattice" & bottle.number=="9","3.25",
                                                                                                                                                                       ifelse(structure == "lattice" & bottle.number=="14","3",
                                                                                                                                                                              ifelse(structure == "lattice" & bottle.number=="10", "2.67", "NA")))))))))))))))))%>%
  ungroup()
                                                                                                               


all_pa_datas$total.bottle.nmbr<-as.numeric(all_pa_datas$total.bottle.nmbr)  
all_pa_datas$total.bottle.nmbr_per_rep<-as.numeric(all_pa_datas$total.bottle.nmbr_per_rep)                                                                              
all_pa_datas$total.bottle.nmbr_connect_per_rep<-as.numeric(all_pa_datas$total.bottle.nmbr_connect_per_rep)                                                                              
all_pa_datas$total.bottle.nmbr_position_per_rep<-as.numeric(all_pa_datas$total.bottle.nmbr_position_per_rep)                                                                              
all_pa_datas$nghbr_connect<-as.numeric(all_pa_datas$nghbr_connect)                                                                              
all_pa_datas<-all_pa_datas%>%
  mutate_if(is.character, 
          str_replace_all, pattern = "isolated", replacement = "control")
str(all_pa_datas)
all_pa_datas%>%
  ggplot(aes(x=bottle.number,y=nghbr_connect))+
           geom_point()+facet_grid(~structure)
############################################################################################################################################
#Env Data
env<-read.csv("Data/Env.variables.csv")
str(env)
envz<-env%>%
  dplyr::select(-c(bottle, bottle.number,replicate,position))%>%
  dplyr::group_by(structure)%>%
  dplyr::summarise(across(everything(), list(mean)))

envzz<-as.data.frame(t(envz))
write.csv(envzz, file="Data/summary.env.csv")
         