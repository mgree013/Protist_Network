#Title: Effects of spatial structure on predator-prey dynamics
#Plotting script 
#July 12,2021
#Authors: Matthew green, Clara Woodie, Megan Whitesell, and Kurt E. Anderson
library(ggplot2)
library(viridis)
library(tidyverse)
library(cowplot)
library(AICcmodavg)
library(betareg)
#Data
#1)Occupancy Data (see "Organize_data.R")
setwd("/Users/matthewdouglasgreen/Dropbox/Protist Lab Data/Kurt_Matthew_Shared Data/Dendritic Data/Network.Summer.19/New_analysis")
source("Code/Organize_data.R")
all_pa_datas

#Density Data
Dataaa = read.csv("Data/new.net.2.data.csv")
summary(Dataaa)

#1) Figure 2
#Occupancy and Density Dynamics
#proportion/percent bottles occupied across structures

#Occupancy Data
prop_pa<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  group_by(structure,day)%>%
  summarise(prey =mean(prey.oc), pred= mean(pred.oc))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="occupancy")

#Density Data
Dataaa<-Dataaa%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  ungroup()%>%
  group_by(structure,day)%>%
  summarise(log.prey =mean(ln.prey), log.pred= mean(ln.pred))%>%
  pivot_longer(cols=log.prey:log.pred,names_to = "species", values_to="density")


a<-prop_pa%>%
  ggplot(aes(x=day,y=occupancy, colour=species))+ 
  geom_line() +
  scale_color_viridis(discrete = TRUE)+
  geom_vline(xintercept=75, linetype='dotted', col = 'black')+
  geom_vline(xintercept=150, linetype='dotted', col = 'black')+
  labs(x="Day",y="Proportion Predator or Prey Occupancy")+
  facet_grid(~factor(structure, levels=c('isolated','dendritic','lattice')))+
  theme(strip.text.x = element_text(size = 12),axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),axis.text=element_text(size=12),strip.background = element_rect(colour="black",fill="white"),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+theme(legend.position = c(0.95,0.75),legend.background = element_blank(),
                                                                                 legend.box.background = element_rect(colour = "black"))

b<-Dataaa%>%
  ggplot(aes(x=day,y=density, colour=species))+ 
  geom_line() +
  scale_color_viridis(discrete = TRUE)+
  geom_vline(xintercept=75, linetype='dotted', col = 'black')+
  geom_vline(xintercept=150, linetype='dotted', col = 'black')+
  labs(x="Day",y="Log Density + 1")+
  facet_grid(~factor(structure, levels=c('isolated','dendritic','lattice')))+
  theme(strip.text.x = element_text(size = 12),axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),axis.text=element_text(size=12),strip.background = element_rect(colour="black",fill="white"),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

plot_grid(a,b, ncol=1)
################################################################################################################################################################

#Figure 3-Occupancy vs structure and Connectivity

#2A) Entire Experiment
prop_pa_rep_no_t<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))


aa<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="")+
  theme(axis.title.x=element_blank(),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")
#+theme(legend.position = c(0.85, 0.15),legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))


prop_pa_rep_no_t<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  group_by(structure,replicate, connectivity,bottle)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")
prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))

bb<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="")+
  theme(axis.title.x=element_blank(),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

#2B) Phase 1
prop_pa_rep_no_t<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  filter(day < 75)%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")
prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))

a<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("c)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="")+
  theme(axis.title.x=element_blank(),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  filter(day < 75)%>%
  group_by(structure,replicate, connectivity,bottle)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")
prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))

b<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("d)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="")+
  theme(axis.title.x=element_blank(),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

#2C) Phase 2
prop_pa_rep_no_t<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  filter(day  > 75 & day < 150)%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")
prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))

c<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("e)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="")+
  theme(axis.title.x=element_blank(),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  filter(day  > 75 & day < 150)%>%
  group_by(structure,replicate, connectivity,bottle)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")
prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))

d<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("f)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="")+
  theme(axis.title.x=element_blank(),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")
prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))

#2D) Phase 3
prop_pa_rep_no_t<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  filter(day  > 150)%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")
prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))

e<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("g)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="")+
  theme(strip.text.x = element_text(size = 12),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  mutate_if(is.character, str_replace_all, pattern = "control", replacement = "isolated")%>%
  filter(day  > 150)%>%
  group_by(structure,replicate, connectivity,bottle)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")
prop_pa_rep_no_t$structure <- factor(prop_pa_rep_no_t$structure, levels=c("isolated", "dendritic", "lattice"))

f<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("h)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="")+
  theme(strip.text.x = element_text(size = 12),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

plot_grid(aa,bb,a,b,c,d,e,f,ncol=2)

################################################################################################################################################################
#Figure 5: Colonization/Extinction
#All phases
Ext_col_data_all<-newer_pa_datas%>%
  #filter(day >0)%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  ungroup()%>%
  rename(lag.pred.oc = `pred.oc-1`)%>%
  rename(lag.prey.oc = `prey.oc-1`)%>%
  replace(is.na(.), 7)%>%
  mutate(
    colonization_col_pred=if_else(pred.oc==1 & lag.pred.oc==0 ,1,0),
    non_colonization_col_pred=if_else(pred.oc==0 & lag.pred.oc==0 ,1,0), 
    extinction_col_pred=if_else(pred.oc==0 & lag.pred.oc==1 ,1,0),
    non_extinction_col_pred=if_else(pred.oc==1 & lag.pred.oc==1 ,1,0),
    colonization_col_prey=if_else(prey.oc==1 & lag.prey.oc==0 ,1,0), 
    non_colonization_col_prey=if_else(prey.oc==0 & lag.prey.oc==0 ,1,0), 
    extinction_col_prey=if_else(prey.oc==0 & lag.prey.oc== 1, 1,0),
    non_extinction_col_prey=if_else(prey.oc==1 & lag.prey.oc== 1, 1,0))%>%
  replace(is.na(.), 0)%>%
  group_by(bottle)%>%
  summarize(
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_potenital_pred=sum(colonization_col_pred+non_colonization_col_pred),colonization_potenital_prey=sum(colonization_col_prey+non_colonization_col_prey),
    extinction_potenital_pred=sum(extinction_col_pred+non_extinction_col_pred),extinction_potenital_prey=sum(extinction_col_prey+non_extinction_col_prey),
    colonization_sum_prey=sum(colonization_col_prey),non_colonization_sum_prey=sum(non_colonization_col_prey),extinction_sum_prey=sum(extinction_col_prey),non_extinction_sum_prey=sum(non_extinction_col_prey),
    colonization_prob_pred=sum(colonization_col_pred)/sum(colonization_col_pred+non_colonization_col_pred), extinction_prob_pred=sum(extinction_col_pred)/sum(extinction_col_pred+non_extinction_col_pred),
    colonization_prob_prey=sum(colonization_col_prey)/sum(colonization_col_prey+non_colonization_col_prey), extinction_prob_prey=sum(extinction_col_prey)/sum(extinction_col_prey+non_extinction_col_prey),
    ext_colon_ratio_pred=extinction_prob_pred/colonization_prob_pred,ext_colon_ratio_prey=extinction_prob_prey/colonization_prob_prey)%>%
  left_join(all_pa_dataz, by="bottle")%>%
  filter(structure !="control")

a1<-Ext_col_data_all%>%
  ggplot(aes(x=structure, y=extinction_prob_pred, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  ggtitle("a)") +
  labs(x="Network Structure",y="Exctinction Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

a2<-Ext_col_data_all%>%
  ggplot(aes(x=as.factor(connectivity), y=extinction_prob_pred, fill=as.factor(connectivity)))+
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Exctinction Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

a3<-Ext_col_data_all%>%
  ggplot(aes(x=structure, y=colonization_prob_pred, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  ggtitle("c)") +
  labs(x="Network Structure",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

a4<-Ext_col_data_all%>%
  ggplot(aes(x=as.factor(connectivity), y=colonization_prob_pred, fill=as.factor(connectivity)))+
  geom_boxplot()+
  ggtitle("d)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

#phase 1

Ext_col_data_1<-newer_pa_datas%>%
  #filter(day >0)%>%
  filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  ungroup()%>%
  rename(lag.pred.oc = `pred.oc-1`)%>%
  rename(lag.prey.oc = `prey.oc-1`)%>%
  replace(is.na(.), 7)%>%
  mutate(
    colonization_col_pred=if_else(pred.oc==1 & lag.pred.oc==0 ,1,0),
    non_colonization_col_pred=if_else(pred.oc==0 & lag.pred.oc==0 ,1,0), 
    extinction_col_pred=if_else(pred.oc==0 & lag.pred.oc==1 ,1,0),
    non_extinction_col_pred=if_else(pred.oc==1 & lag.pred.oc==1 ,1,0),
    colonization_col_prey=if_else(prey.oc==1 & lag.prey.oc==0 ,1,0), 
    non_colonization_col_prey=if_else(prey.oc==0 & lag.prey.oc==0 ,1,0), 
    extinction_col_prey=if_else(prey.oc==0 & lag.prey.oc== 1, 1,0),
    non_extinction_col_prey=if_else(prey.oc==1 & lag.prey.oc== 1, 1,0))%>%
  replace(is.na(.), 0)%>%
  group_by(bottle)%>%
  summarize(
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_potenital_pred=sum(colonization_col_pred+non_colonization_col_pred),colonization_potenital_prey=sum(colonization_col_prey+non_colonization_col_prey),
    extinction_potenital_pred=sum(extinction_col_pred+non_extinction_col_pred),extinction_potenital_prey=sum(extinction_col_prey+non_extinction_col_prey),
    colonization_sum_prey=sum(colonization_col_prey),non_colonization_sum_prey=sum(non_colonization_col_prey),extinction_sum_prey=sum(extinction_col_prey),non_extinction_sum_prey=sum(non_extinction_col_prey),
    colonization_prob_pred=sum(colonization_col_pred)/sum(colonization_col_pred+non_colonization_col_pred), extinction_prob_pred=sum(extinction_col_pred)/sum(extinction_col_pred+non_extinction_col_pred),
    colonization_prob_prey=sum(colonization_col_prey)/sum(colonization_col_prey+non_colonization_col_prey), extinction_prob_prey=sum(extinction_col_prey)/sum(extinction_col_prey+non_extinction_col_prey),
    ext_colon_ratio_pred=extinction_prob_pred/colonization_prob_pred,ext_colon_ratio_prey=extinction_prob_prey/colonization_prob_prey)%>%
  left_join(all_pa_dataz, by="bottle")%>%
  filter(structure !="control")
view(Ext_col_data_1)

b1<-Ext_col_data_1%>%
  ggplot(aes(x=structure, y=extinction_prob_prey, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  ggtitle("e)") +
  labs(x="Network Structure",y="Exctinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

b2<-Ext_col_data_1%>%
  ggplot(aes(x=as.factor(connectivity), y=extinction_prob_prey, fill=as.factor(connectivity)))+
  geom_boxplot()+
  ggtitle("f)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Exctinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

b3<-Ext_col_data_1%>%
  ggplot(aes(x=structure, y=colonization_prob_prey, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  ggtitle("g)") +
  labs(x="Network Structure",y="Colonization Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

b4<-Ext_col_data_1%>%
  ggplot(aes(x=as.factor(connectivity), y=colonization_prob_prey, fill=as.factor(connectivity)))+
  geom_boxplot()+
  ggtitle("h)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Colonization Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

#Phase 2
Ext_col_data_2<-newer_pa_datas%>%
  #filter(day >0)%>%
  #filter(day < 75)%>%
  filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  ungroup()%>%
  rename(lag.pred.oc = `pred.oc-1`)%>%
  rename(lag.prey.oc = `prey.oc-1`)%>%
  replace(is.na(.), 7)%>%
  mutate(
    colonization_col_pred=if_else(pred.oc==1 & lag.pred.oc==0 ,1,0),
    non_colonization_col_pred=if_else(pred.oc==0 & lag.pred.oc==0 ,1,0), 
    extinction_col_pred=if_else(pred.oc==0 & lag.pred.oc==1 ,1,0),
    non_extinction_col_pred=if_else(pred.oc==1 & lag.pred.oc==1 ,1,0),
    colonization_col_prey=if_else(prey.oc==1 & lag.prey.oc==0 ,1,0), 
    non_colonization_col_prey=if_else(prey.oc==0 & lag.prey.oc==0 ,1,0), 
    extinction_col_prey=if_else(prey.oc==0 & lag.prey.oc== 1, 1,0),
    non_extinction_col_prey=if_else(prey.oc==1 & lag.prey.oc== 1, 1,0))%>%
  replace(is.na(.), 0)%>%
  group_by(bottle)%>%
  summarize(
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_potenital_pred=sum(colonization_col_pred+non_colonization_col_pred),colonization_potenital_prey=sum(colonization_col_prey+non_colonization_col_prey),
    extinction_potenital_pred=sum(extinction_col_pred+non_extinction_col_pred),extinction_potenital_prey=sum(extinction_col_prey+non_extinction_col_prey),
    colonization_sum_prey=sum(colonization_col_prey),non_colonization_sum_prey=sum(non_colonization_col_prey),extinction_sum_prey=sum(extinction_col_prey),non_extinction_sum_prey=sum(non_extinction_col_prey),
    colonization_prob_pred=sum(colonization_col_pred)/sum(colonization_col_pred+non_colonization_col_pred), extinction_prob_pred=sum(extinction_col_pred)/sum(extinction_col_pred+non_extinction_col_pred),
    colonization_prob_prey=sum(colonization_col_prey)/sum(colonization_col_prey+non_colonization_col_prey), extinction_prob_prey=sum(extinction_col_prey)/sum(extinction_col_prey+non_extinction_col_prey),
    ext_colon_ratio_pred=extinction_prob_pred/colonization_prob_pred,ext_colon_ratio_prey=extinction_prob_prey/colonization_prob_prey)%>%
  left_join(all_pa_dataz, by="bottle")%>%
  filter(structure !="control")

c1<-Ext_col_data_2%>%
  ggplot(aes(x=structure, y=extinction_prob_pred, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  ggtitle("i)") +
  labs(x="Network Structure",y="Exctinction Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

c2<-Ext_col_data_2%>%
  ggplot(aes(x=as.factor(connectivity), y=extinction_prob_pred, fill=as.factor(connectivity)))+
  geom_boxplot()+
  ggtitle("j)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Exctinction Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

c3<-Ext_col_data_2%>%
  ggplot(aes(x=structure, y=colonization_prob_pred, fill=structure))+
  geom_boxplot()+
  ggtitle("e)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

c4<-Ext_col_data_2%>%
  ggplot(aes(x=as.factor(connectivity), y=colonization_prob_pred, fill=as.factor(connectivity)))+
  geom_boxplot()+
  ggtitle("f)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

#Phase 3 Dynamics
Ext_col_data_3<-newer_pa_datas%>%
  #filter(day >0)%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  filter(day  > 150)%>%
  ungroup()%>%
  rename(lag.pred.oc = `pred.oc-1`)%>%
  rename(lag.prey.oc = `prey.oc-1`)%>%
  replace(is.na(.), 7)%>%
  mutate(
    colonization_col_pred=if_else(pred.oc==1 & lag.pred.oc==0 ,1,0),
    non_colonization_col_pred=if_else(pred.oc==0 & lag.pred.oc==0 ,1,0), 
    extinction_col_pred=if_else(pred.oc==0 & lag.pred.oc==1 ,1,0),
    non_extinction_col_pred=if_else(pred.oc==1 & lag.pred.oc==1 ,1,0),
    colonization_col_prey=if_else(prey.oc==1 & lag.prey.oc==0 ,1,0), 
    non_colonization_col_prey=if_else(prey.oc==0 & lag.prey.oc==0 ,1,0), 
    extinction_col_prey=if_else(prey.oc==0 & lag.prey.oc== 1, 1,0),
    non_extinction_col_prey=if_else(prey.oc==1 & lag.prey.oc== 1, 1,0))%>%
  replace(is.na(.), 0)%>%
  group_by(bottle)%>%
  summarize(
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_potenital_pred=sum(colonization_col_pred+non_colonization_col_pred),colonization_potenital_prey=sum(colonization_col_prey+non_colonization_col_prey),
    extinction_potenital_pred=sum(extinction_col_pred+non_extinction_col_pred),extinction_potenital_prey=sum(extinction_col_prey+non_extinction_col_prey),
    colonization_sum_prey=sum(colonization_col_prey),non_colonization_sum_prey=sum(non_colonization_col_prey),extinction_sum_prey=sum(extinction_col_prey),non_extinction_sum_prey=sum(non_extinction_col_prey),
    colonization_prob_pred=sum(colonization_col_pred)/sum(colonization_col_pred+non_colonization_col_pred), extinction_prob_pred=sum(extinction_col_pred)/sum(extinction_col_pred+non_extinction_col_pred),
    colonization_prob_prey=sum(colonization_col_prey)/sum(colonization_col_prey+non_colonization_col_prey), extinction_prob_prey=sum(extinction_col_prey)/sum(extinction_col_prey+non_extinction_col_prey),
    ext_colon_ratio_pred=extinction_prob_pred/colonization_prob_pred,ext_colon_ratio_prey=extinction_prob_prey/colonization_prob_prey)%>%
  left_join(all_pa_dataz, by="bottle")%>%
  filter(structure !="control")

d1<-Ext_col_data_3%>%
  ggplot(aes(x=structure, y=extinction_prob_prey, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  ggtitle("g)") +
  labs(x="Network Structure",y="Exctinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

d2<-Ext_col_data_3%>%
  ggplot(aes(x=as.factor(connectivity), y=extinction_prob_prey, fill=as.factor(connectivity)))+
  geom_boxplot()+
  ggtitle("h)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Exctinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

d3<-Ext_col_data_3%>%
  ggplot(aes(x=structure, y=colonization_prob_pred, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  ggtitle("o)") +
  labs(x="Network Structure",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

d4<-Ext_col_data_3%>%
  ggplot(aes(x=as.factor(connectivity), y=colonization_prob_pred, fill=as.factor(connectivity)))+
  geom_boxplot()+
  ggtitle("p)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")


plot_grid(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4, nrow=4)
plot_grid(a1,a2,a3,a4,b1,b2,c1,c2,c3,c4,d1,d2, nrow=4)
plot_grid(a1,a2,a3,a4,c3,c4,d1,d2, nrow=4)

################################################################################################################################################################
#Figure 7: Trophic Interactions

#Data Subset: Entire Experiment

env_density2<-Data%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

env_density<-Data%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

#Data Subset: Phase 1

env_density2_1<-Data%>%
  filter(day < 75)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

env_density_1<-Data%>%
  filter(day < 75)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

#Data Subset: Phase 2

env_density2_2<-Data%>%
  filter(day  > 75 & day < 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

env_density_2<-Data%>%
  filter(day  > 75 & day < 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

#Data Subset: Phase 3
env_density2_3<-Data%>%
  filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

env_density_3<-Data%>%
  filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

#8A) All Phases
a1<-env_density%>%
  ggplot(aes(x=(bac.density_log),y=density, colour=species))+ 
  geom_point()+
  stat_smooth(method = glm, method.args = list(family=gaussian(link = "identity")))+
  #geom_smooth(method = "lm")+
  ggtitle("a)")+
  scale_color_viridis_d()+
  annotate("text", x = 11.9, y = 10, label = "Predator: R^2 == 0.17", parse = TRUE) +
  annotate("text", x = 11.8, y = 8, label = "Prey: R^2 == 0.10", parse = TRUE) +
  labs(x="Bacteria density",y=" Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
#+theme(legend.position = c(0.91, 0.97),legend.background = element_blank(),legend.box.background = element_rect(colour = "black")) 

env_density_pred<-env_density%>%filter(species=="pred")
y<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_pred)
y1<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_pred)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_pred)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_pred)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)


env_density_prey<-env_density%>%filter(species=="prey")
y<-glm(density~bac.density_log, family=gaussian(link = "identity"), data=env_density_prey)
y1<-glm(density~1, family=gaussian(link = "identity"), data=env_density_prey)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_prey)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_prey)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2

b1<-env_density2%>%
  ggplot(aes(x=(pred),y=prey))+ 
  geom_point()+
  stat_smooth(method = glm, method.args = list(family=gaussian(link = "identity")))+
  geom_smooth(method = "lm")+
  ggtitle("b)")+
  scale_color_viridis_d()+
  annotate("text", x = 1, y = 8, label = "R^2 == 0.22", parse = TRUE) +
  labs(x="Predator density",y=" Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

y<-glm(prey~pred, family=gaussian(link = "identity"), data=env_density2)
y1<-glm(prey~1, family=gaussian(link = "identity"), data=env_density2)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

dog=list()
dog[[1]]<-glm(prey~pred , family=gaussian(link = "identity"), data=env_density2)
dog[[2]]<-glm(prey~1 , family=gaussian(link = "identity"), data=env_density2)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2

#8B) Phase 1

a2<-env_density_1%>%
  ggplot(aes(x=(bac.density_log),y=density, colour=species))+ 
  geom_point()+
  stat_smooth(data=subset(env_density_1,species == "pred"),method = glm, method.args = list(family=gaussian(link = "identity")))+
  #geom_smooth(data=subset(env_density_1,species == "pred"),method = "lm")+
  ggtitle("c)")+
  scale_color_viridis_d()+
  annotate("text", x = 11.9, y = 12, label = "Predator: R^2 == 0.10", parse = TRUE) +
  annotate("text", x = 11.8, y = 10, label = "Prey: R^2 == 0", parse = TRUE) +
  labs(x="Bacteria density",y=" Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

env_density_1_pred<-env_density_1%>%filter(species=="pred")
y<-glm(density~bac.density_log,  family=gaussian(link = "identity"), data=env_density_1_pred)
y1<-glm(density~1,  family=gaussian(link = "identity"), data=env_density_1_pred)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

env_density_1_prey<-env_density_1%>%filter(species=="prey")
y<-glm(density~bac.density_log,  family=gaussian(link = "identity"), data=env_density_1_prey)
y1<-glm(density~1, family=gaussian(link = "identity"), data=env_density_1_prey)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

b2<-env_density2_1%>%
  ggplot(aes(x=(pred),y=prey))+ 
  geom_point()+
  #stat_smooth(method = glm, method.args = list(family=gaussian(link = "identity")))+
  #geom_smooth(method = "lm")+
  ggtitle("d)")+
  scale_color_viridis_d()+
  annotate("text", x = 1, y = 10, label = "R^2 == 0.0", parse = TRUE) +
  labs(x="Predator density",y=" Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

y<-glm(prey~pred,  family=gaussian(link = "identity"), data=env_density2_1)
y1<-glm(prey~1,  family=gaussian(link = "identity"), data=env_density2_1)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

#8B) Phase 2

a3<-env_density_2%>%
  ggplot(aes(x=(bac.density_log),y=density, colour=species))+ 
  geom_point()+
  stat_smooth(method = glm, method.args = list(family=gaussian(link = "identity")))+
  #geom_smooth(method = "lm")+
  ggtitle("e)")+
  scale_color_viridis_d()+
  annotate("text", x = 11.87, y = 12, label = "Predator: R^2 == 0.13", parse = TRUE) +
  annotate("text", x = 11.8, y = 10, label = "Prey: R^2 == 0.11", parse = TRUE) +
  labs(x="Bacteria density",y=" Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

env_density_2_pred<-env_density_2%>%filter(species=="pred")
y<-glm(density~bac.density_log,  family=gaussian(link = "identity"), data=env_density_2_pred)
y1<-glm(density~1,  family=gaussian(link = "identity"), data=env_density_2_pred)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

env_density_2_prey<-env_density_2%>%filter(species=="prey")
y<-glm(density~bac.density_log ,family=gaussian(link = "identity"), data=env_density_2_prey)
y1<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_2_prey)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

b3<-env_density2_2%>%
  ggplot(aes(x=(pred),y=prey))+ 
  geom_point()+
  stat_smooth(method = glm, method.args = list(family=gaussian(link = "identity")))+
  #geom_smooth(method = "lm")+
  ggtitle("f)")+
  annotate("text", x = 1, y = 9.5, label = "R^2 == 0.21", parse = TRUE) +
  scale_color_viridis_d()+
  labs(x="Predator density",y=" Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

y<-glm(prey~pred, family=gaussian(link = "identity"), data=env_density2_2)
y<-glm(prey~1, family=gaussian(link = "identity"), data=env_density2_2)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0
#8B) Phase 3

a4<-env_density_3%>%
  ggplot(aes(x=(bac.density_log),y=density, colour=species))+ 
  geom_point()+
  stat_smooth(method = glm, method.args = list(family=gaussian(link = "identity")))+
  #geom_smooth(method = "lm")+
  ggtitle("g)")+
  scale_color_viridis_d()+
  annotate("text", x = 11.87, y = 8, label = "Predator: R^2 == 0.08", parse = TRUE) +
  annotate("text", x = 11.8, y = 6, label = "Prey: R^2 == 0.09", parse = TRUE) +
  labs(x="Bacteria density",y=" Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

env_density_3_pred<-env_density_3%>%filter(species=="pred")
y<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_3_pred)
y1<-glm(density~1, family=gaussian(link = "identity"), data=env_density_3_pred)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

env_density_3_prey<-env_density_3%>%filter(species=="prey")
y<-glm(density~bac.density_log, family=gaussian(link = "identity"), data=env_density_3_prey)
y1<-glm(density~1, family=gaussian(link = "identity"), data=env_density_3_prey)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

b4<-env_density2_3%>%
  ggplot(aes(x=(pred),y=prey))+ 
  geom_point()+
  stat_smooth(method = glm, method.args = list(family=gaussian(link = "identity")))+
  #geom_smooth(method = "lm")+
  ggtitle("h)")+
  scale_color_viridis_d()+
  #annotate("text", x = 1, y = 23, label = "p-value < 0.05") +
  annotate("text", x = 1, y = 7, label = "R^2 == 0.67", parse = TRUE) +
  labs(x="Predator density",y=" Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

y<-glm(prey~pred , family=gaussian(link = "identity"), data=env_density2_3)
y1<-glm(prey~1 , family=gaussian(link = "identity"), data=env_density2_3)
summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)


plot_grid(a1,b1,a2,b2,a3,b3,a4,b4, ncol=2)

##################################################################################################################################################################################################################
#Figure 6:

#bottle Level
Ext_col_data_glm<-newer_pa_datas%>%
  filter(structure !="control")%>%
  rename(lag.pred.oc = `pred.oc-1`,lag.prey.oc = `prey.oc-1`)%>%
  mutate(
    colonization_col_pred=if_else(pred.oc==1 & lag.pred.oc==0 ,1,0),
    non_colonization_col_pred=if_else(pred.oc==0 & lag.pred.oc==0 ,1,0), 
    extinction_col_pred=if_else(pred.oc==0 & lag.pred.oc==1 ,1,0),
    non_extinction_col_pred=if_else(pred.oc==1 & lag.pred.oc==1 ,1,0),
    colonization_col_prey=if_else(prey.oc==1 & lag.prey.oc==0 ,1,0), 
    non_colonization_col_prey=if_else(prey.oc==0 & lag.prey.oc==0 ,1,0), 
    extinction_col_prey=if_else(prey.oc==0 & lag.prey.oc== 1, 1,0),
    non_extinction_col_prey=if_else(prey.oc==1 & lag.prey.oc== 1, 1,0))%>%
  mutate(lambda=if_else(structure=="dendritic",2.28825,3.14626))%>%
  replace(is.na(.), 0)%>%
  group_by(bottle)%>%
  summarize(
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_potenital_pred=sum(colonization_col_pred+non_colonization_col_pred),colonization_potenital_prey=sum(colonization_col_prey+non_colonization_col_prey),
    extinction_potenital_pred=sum(extinction_col_pred+non_extinction_col_pred),extinction_potenital_prey=sum(extinction_col_prey+non_extinction_col_prey),
    colonization_sum_prey=sum(colonization_col_prey),non_colonization_sum_prey=sum(non_colonization_col_prey),extinction_sum_prey=sum(extinction_col_prey),non_extinction_sum_prey=sum(non_extinction_col_prey),
    colonization_prob_pred=colonization_sum_pred/colonization_potenital_pred, extinction_prob_pred=extinction_sum_pred/extinction_potenital_pred,
    colonization_prob_prey=colonization_sum_prey/colonization_potenital_prey, extinction_prob_prey=extinction_sum_prey/extinction_potenital_prey,
    ext_colon_ratio_pred=(extinction_prob_pred/colonization_prob_pred),ext_colon_ratio_prey=(extinction_prob_prey/colonization_prob_prey),
    pred.prey.oc=colonization_prob_prey/(extinction_prob_prey+colonization_prob_prey),pred.pred.oc=colonization_prob_pred/(extinction_prob_pred+colonization_prob_pred))%>%
  #pred.prey.oc=1-((extinction_prob_prey/colonization_prob_prey)/lambda),pred.pred.oc=1-((extinction_prob_pred/colonization_prob_pred)/lambda))%>%
  left_join(all_pa_dataz, by="bottle")%>%
  filter(pred.prey.oc > 0.001)%>%
  filter(pred.pred.oc > 0.001)%>%
  filter(colonization_prob_prey > 0.001)%>%
  filter(colonization_prob_pred > 0.001)%>%
  filter(extinction_prob_prey >0.001)%>%
  filter(extinction_prob_pred >0.001)
  
#Network level
Ext_col_data_network<-newer_pa_datas%>%
  filter(structure !="control")%>%
  rename(lag.pred.oc = `pred.oc-1`,lag.prey.oc = `prey.oc-1`)%>%
  mutate(
    colonization_col_pred=if_else(pred.oc==1 & lag.pred.oc==0 ,1,0),
    non_colonization_col_pred=if_else(pred.oc==0 & lag.pred.oc==0 ,1,0), 
    extinction_col_pred=if_else(pred.oc==0 & lag.pred.oc==1 ,1,0),
    non_extinction_col_pred=if_else(pred.oc==1 & lag.pred.oc==1 ,1,0),
    colonization_col_prey=if_else(prey.oc==1 & lag.prey.oc==0 ,1,0), 
    non_colonization_col_prey=if_else(prey.oc==0 & lag.prey.oc==0 ,1,0), 
    extinction_col_prey=if_else(prey.oc==0 & lag.prey.oc== 1, 1,0),
    non_extinction_col_prey=if_else(prey.oc==1 & lag.prey.oc== 1, 1,0))%>%
  mutate(lambda=if_else(structure=="dendritic",2.28825,3.14626))%>%
  replace(is.na(.), 0)%>%
  group_by(structure,replicate)%>%
  summarize(
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_potenital_pred=sum(colonization_col_pred+non_colonization_col_pred),colonization_potenital_prey=sum(colonization_col_prey+non_colonization_col_prey),
    extinction_potenital_pred=sum(extinction_col_pred+non_extinction_col_pred),extinction_potenital_prey=sum(extinction_col_prey+non_extinction_col_prey),
    colonization_sum_prey=sum(colonization_col_prey),non_colonization_sum_prey=sum(non_colonization_col_prey),extinction_sum_prey=sum(extinction_col_prey),non_extinction_sum_prey=sum(non_extinction_col_prey),
    colonization_prob_pred=colonization_sum_pred/colonization_potenital_pred, extinction_prob_pred=extinction_sum_pred/extinction_potenital_pred,
    colonization_prob_prey=colonization_sum_prey/colonization_potenital_prey, extinction_prob_prey=extinction_sum_prey/extinction_potenital_prey,
    ext_colon_ratio_pred=(extinction_prob_pred/colonization_prob_pred),ext_colon_ratio_prey=(extinction_prob_prey/colonization_prob_prey),
    pred.prey.oc=1-((extinction_prob_prey/colonization_prob_prey)/lambda),pred.pred.oc=1-((extinction_prob_pred/colonization_prob_pred)/lambda))%>%
  left_join(all_pa_datazz, by=c("structure", "replicate"))%>%
  distinct(structure,replicate, .keep_all = T)%>%
  filter(pred.pred.oc > 0.001)%>%
  filter(pred.prey.oc > 0.001)
  
da<-Ext_col_data_network%>%
  ggplot(aes(x=pred.occupancy,y=pred.pred.oc, colour=structure))+ 
  geom_point()+
  ggtitle("b)") +
  geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  labs(x="Predator Observed Occupancy",y="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

dc<-Ext_col_data_network%>%
  ggplot(aes(x=prey.occupancy,y=pred.prey.oc, colour=structure))+ 
  geom_point()+
  ggtitle("a)") +
  geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  labs(x="Prey Observed Occupancy",y="Prey Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.a<-Ext_col_data_glm%>%
  ggplot(aes(x=prey.occupancy,y=pred.prey.oc, colour=as.factor(structure)))+ #))+ #
  geom_point()+
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Observed Occupancy",y="Prey Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.c<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.prey.oc,y=colonization_prob_prey, colour=as.factor(structure)))+ #))+ #
  geom_point()+
  ggtitle("e)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(y="Colonization Probability Prey ",x="Predicted  Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.e<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.prey.oc,y=extinction_prob_prey, colour=as.factor(structure)))+ #))+ #
  geom_point()+
  ggtitle("g)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(y="Extinction Probaility Prey",x="Predicted Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")


#pred
pred.b<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.occupancy,y=pred.pred.oc, colour=as.factor(structure)))+ #))+ #
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Observed Occupancy",y="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.d<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.pred.oc,y=colonization_prob_pred, colour=as.factor(structure)))+ 
  geom_point()+
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(y="Predator Colonization Probaility",x="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.f<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.pred.oc,y=extinction_prob_pred, colour=as.factor(structure)))+#+ ))+ #
  geom_point()+
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(y="Predator Extinction Probability ",x="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.c1<-Ext_col_data_glm%>%
  ggplot(aes(x=prey.occupancy,y=colonization_prob_prey, colour=as.factor(structure)))+ #))+ #
  geom_point()+
  ggtitle("e)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(y="Prey Colonization Probability",x="Prey Observed Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.e1<-Ext_col_data_glm%>%
  ggplot(aes(x=prey.occupancy,y=extinction_prob_prey, colour=as.factor(structure)))+ #))+ #
  geom_point()+
  ggtitle("g)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(y="Prey Extinction Probaility",x="Prey Observed Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")


pred.d1<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.occupancy,y=colonization_prob_pred, colour=as.factor(structure)))+ #))+ #
  geom_point()+
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(y="Predator Colonization Probaility",x="Predator Observed Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.f1<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.occupancy,y=extinction_prob_pred, colour=as.factor(structure)))+ #))+ #
  geom_point()+
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(y="Predator Extinction Probability ",x="Predator Observed Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

plot_grid(da,dc,pred.a,pred.b,pred.c,pred.d,pred.e,pred.f,ncol=2)
plot_grid(dc,da,pred.a,pred.b,pred.c1,pred.d1,pred.e1,pred.f1,ncol=2)

##################################################################################################################################################################################################################
#No structure
dog[[1]]<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_network)

dog=list()
dog[[1]]<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_network, link = "loglog")
dog[[2]]<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_network)
dog[[3]]<-betareg(pred.pred.oc~1, data=Ext_col_data_network)


Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
summary(dog[[1]])
AICc(dog[[1]])

MuMIn::AICc(y,y1)
summary(y)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0
dog<-list()
dog[[1]]<-betareg(pred.pred.oc~1,  data=Ext_col_data_network)
dog[[2]]<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_network)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

y<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_network)
AICc(y)
summary(y)


da<-Ext_col_data_network%>%
  ggplot(aes(x=pred.occupancy,y=pred.pred.oc))+ 
  geom_point()+
  ggtitle("b)") +
  stat_smooth(method = NULL,aes(y = predict(y, Ext_col_data_network))) +
  #geom_line(aes(y = predict(y, Ext_col_data_network),  linetype = "logit")) +
  #stat_smooth(method = betareg, method.args = list(family = binomial(link = "logit")))+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .5, y = 1, label = "R^2 == 0.91", parse = TRUE) +
  labs(x="Predator Observed Occupancy",y="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")



#Next

dog<-list()
dog[[1]]<-betareg(pred.prey.oc~1,  data=Ext_col_data_network)
dog[[2]]<-betareg(pred.prey.oc~prey.occupancy, data=Ext_col_data_network)
dog[[3]]<-betareg(pred.prey.oc~prey.occupancy, data=Ext_col_data_network, link = "loglog")

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

AICc(y1)

MuMIn::AICc(y,y1)
summary(y)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

y1<-betareg(pred.prey.oc~prey.occupancy, data=Ext_col_data_network)
dc<-Ext_col_data_network%>%
  ggplot(aes(x=prey.occupancy,y=pred.prey.oc))+ 
  geom_point()+
  ggtitle("a)") +
  #geom_smooth(method = "lm",se=T)+
  stat_smooth(method = NULL,aes(y = predict(y1, Ext_col_data_network))) +
  #geom_line(aes(y = predict(y, Ext_col_data_network), linetype = "logit")) +
  scale_color_viridis_d()+
  annotate("text", x = .78, y = 1, label = "R^2 == 0.42", parse = TRUE) +
  labs(x="Prey Observed Occupancy",y="Prey Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")





y<-glm(pred.prey.oc~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

dog<-list()
dog[[1]]<-betareg(pred.prey.oc~1,  data=Ext_col_data_glm)
dog[[2]]<-betareg(pred.prey.oc~prey.occupancy, data=Ext_col_data_glm)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

summary(dog[[2]])
AICc(dog[[2]])



pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

y2<-betareg(pred.prey.oc~prey.occupancy, data=Ext_col_data_glm)
pred.a<-Ext_col_data_glm%>%
  ggplot(aes(x=prey.occupancy,y=pred.prey.oc))+ 
  geom_point()+
  ggtitle("c)") +
  #geom_smooth(method = "lm",se=T)+
  stat_smooth(method = NULL,aes(y = predict(y2, Ext_col_data_glm)),se=F) +
  #stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")))+
  scale_color_viridis_d()+
  annotate("text", x = .75, y = 1, label = "R^2 == 0.74", parse = TRUE) +
  labs(x="Prey Observed Occupancy",y="Prey Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")





#pred
dog<-list()
dog[[1]]<-betareg(pred.pred.oc~1,  data=Ext_col_data_glm)
dog[[2]]<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_glm)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

AICc(y)
summary(y)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

y3<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_glm)
pred.b<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.occupancy,y=pred.pred.oc))+ 
  geom_point()+
  ggtitle("d)") +
  #geom_line(aes(y = predict(dog[[2]], Ext_col_data_glm), linetype = "logit")) +
  stat_smooth(aes(y = predict(y3, Ext_col_data_glm)),se=T) +
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .2, y = 1, label = "R^2 == 0.96", parse = TRUE) +
  labs(x="Predator Observed Occupancy",y="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")





pred.c1<-Ext_col_data_glm%>%
  ggplot(aes(x=prey.occupancy,y=colonization_prob_prey))+ 
  geom_point()+
  ggtitle("e)") +
  stat_smooth(linetype= "dashed",method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .75, y = .75, label = "R^2 == 0.05", parse = TRUE) +
  labs(y="Prey Colonization Probability",x="Prey Observed Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

z <- cbind(Ext_col_data_glm$colonization_sum_prey, Ext_col_data_glm$non_colonization_sum_prey)
y<-glm(z~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
y1<-glm(z~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(y,y1, weights = TRUE, sort = FALSE)

summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

pred.e1<-Ext_col_data_glm%>%
  ggplot(aes(x=prey.occupancy,y=extinction_prob_prey))+
  geom_point()+
  ggtitle("g)") +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .75, y = .2, label = "R^2 == 0.29", parse = TRUE) +
  labs(y="Prey Extinction Probaility",x="Prey Observed Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

z <- cbind(Ext_col_data_glm$extinction_sum_prey, Ext_col_data_glm$non_extinction_sum_prey)
y<-glm(z~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
y1<-glm(z~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(y,y1, weights = TRUE, sort = FALSE)

summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

pred.d1<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.occupancy,y=colonization_prob_pred))+ 
  geom_point()+
  ggtitle("f)") +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .2, y = .75, label = "R^2 == 0.40", parse = TRUE) +
  labs(y="Predator Colonization Probaility",x="Predator Observed Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

z <- cbind(Ext_col_data_glm$colonization_sum_pred, Ext_col_data_glm$non_colonization_sum_pred)
y<-glm(z~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
y1<-glm(z~1, family=binomial(link = "logit"), data=Ext_col_data_glm)

summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

pred.f1<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.occupancy,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("h)") +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .2, y = .75, label = "R^2 == 0.35", parse = TRUE) +
  labs(y="Predator Extinction Probability ",x="Predator Observed Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

z <- cbind(Ext_col_data_glm$extinction_sum_pred, Ext_col_data_glm$non_extinction_sum_pred)
y<-glm(z~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
y1<-glm(z~1, family=binomial(link = "logit"), data=Ext_col_data_glm)

summary(y)
AICc(y,y1)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

###############
plot_grid(dc,da,pred.a,pred.b,pred.c,pred.d,pred.e,pred.f,ncol=2)
plot_grid(dc,da,pred.a,pred.b,pred.c1,pred.d1,pred.e1,pred.f1,ncol=2)
plot_grid(pred.c1,pred.d1,pred.e1,pred.f1,ncol=2)







###############
pred.c<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.prey.oc,y=colonization_prob_prey))+ 
  geom_point()+
  ggtitle("e)") +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")))+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .7, y = .75, label = "R^2 == 0.34", parse = TRUE) +
  labs(y="Colonization Probability Prey ",x="Predicted  Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

y<-cbind(Ext_col_data_glm$colonization_potenital_prey,Ext_col_data_glm$non_colonization_sum_prey)
y<-glm(y~pred.prey.oc, family=binomial(link = "logit"), data=Ext_col_data_glm)
summary(y)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

pred.e<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.prey.oc,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("g)") +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")))+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .7, y = .2, label = "R^2 == 0.03", parse = TRUE) +
  labs(y="Extinction Probaility Prey",x="Predicted Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

y<-glm(extinction_prob_prey~pred.prey.oc, family=binomial(link = "logit"), data=Ext_col_data_glm)
summary(y)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0
pred.d<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.pred.oc,y=colonization_prob_pred))+ 
  geom_point()+
  ggtitle("f)") +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")))+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = .2, y = .75, label = "R^2 == 0.38", parse = TRUE) +
  labs(y="Predator Colonization Probaility",x="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

y<-glm(colonization_prob_pred~pred.pred.oc, family=binomial(link = "logit"), data=Ext_col_data_glm)
summary(y)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

pred.f<-Ext_col_data_glm%>%
  ggplot(aes(x=pred.pred.oc,y=extinction_prob_pred))+
  geom_point()+
  ggtitle("h)") +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")))+
  #geom_smooth(method = "lm",se=T)+
  scale_color_viridis_d()+
  annotate("text", x = 0.2, y = .75, label = "R^2 == 0.55", parse = TRUE) +
  labs(y="Predator Extinction Probability ",x="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

y<-glm(extinction_prob_pred~pred.pred.oc, family=binomial(link = "logit"), data=Ext_col_data_glm)
summary(y)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0