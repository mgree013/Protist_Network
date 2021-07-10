library(ggplot2)
library(tidyverse)
library(viridis)
library(DataCombine)
library(cowplot)


all_pa_datas%>%
  ggplot(aes(x=position,y=connectivity))+
  geom_boxplot()
################################################################################################################################################################
#1) Occupancy Dynamics
#proportion/percent bottles occupied across replicates
prop_pa<-all_pa_datas%>%
  group_by(structure,day)%>%
  summarise(prey =sum(prey.oc)/total.bottle.nmbr, pred= sum(pred.oc)/total.bottle.nmbr)%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="occupancy")

a<-prop_pa%>%
  ggplot(aes(x=day,y=occupancy, colour=species))+ 
  geom_line() +
  scale_color_viridis(discrete = TRUE)+
  geom_vline(xintercept=75, linetype='dotted', col = 'black')+
  geom_vline(xintercept=150, linetype='dotted', col = 'black')+
  labs(x="day",y="Percent Predator-Prey Occupancy")+
  facet_grid(~structure)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+theme(legend.position = c(0.95,0.75),legend.background = element_blank(),
                                                                                 legend.box.background = element_rect(colour = "black"))

Dataaa = read.csv("data/new.net.2.data.csv")
summary(Dataaa)

Dataaa<-Dataaa%>%
  mutate_if(is.character, str_replace_all, pattern = "isolated", replacement = "control")%>%
  ungroup()%>%
  group_by(structure,day)%>%
  summarise(log.prey =mean(ln.prey), log.pred= mean(ln.pred))%>%
  pivot_longer(cols=log.prey:log.pred,names_to = "species", values_to="density")
  
b<-Dataaa%>%
  ggplot(aes(x=day,y=density, colour=species))+ 
  geom_line() +
  scale_color_viridis(discrete = TRUE)+
  geom_vline(xintercept=75, linetype='dotted', col = 'black')+
  geom_vline(xintercept=150, linetype='dotted', col = 'black')+
  labs(x="day",y="Log Density")+
  facet_grid(~structure)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

plot_grid(a,b, ncol=1)
################################################################################################################################################################
#2) Conenctivity/Position Effectas on Occupancy

#2A)Conenctivity
#proportion/percent bottles occupied 
prop_pa_rep_connect<-all_pa_datas%>%
  group_by(structure,day,replicate,connectivity)%>%
  summarise(prey.oc =mean(prey.oc)/total.bottle.nmbr_connect_per_rep, pred.oc= sum(pred.oc)/total.bottle.nmbr_connect_per_rep)%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_connect%>%
  ggplot(aes(x=day,y=occupancy, colour=species))+ 
  geom_line() +
  labs(x="day",y="Percent Predator-Prey Occupancy")+
  facet_wrap(connectivity~structure)


#proportion/percent bottles occupied across replicates
prop_pa_connect<-all_pa_datas%>%
  group_by(structure,day,connectivity)%>%
  summarise(prey.oc =sum(prey.oc)/total.bottle.nmbr, pred.oc= sum(pred.oc)/total.bottle.nmbr)%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_connect%>%
  ggplot(aes(x=day,y=occupancy, colour=species))+ 
  geom_line() +
  labs(x="day",y="Percent Predator-Prey Occupancy")+
  facet_wrap(connectivity~structure)


#2B)Position
#proportion/percent bottles occupied 
prop_pa_rep_posit<-all_pa_datas%>%
  group_by(structure,day,replicate,position)%>%
  summarise(prey.oc =sum(prey.oc)/total.bottle.nmbr_position_per_rep, pred.oc= sum(pred.oc)/total.bottle.nmbr_position_per_rep)%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_posit%>%
  ggplot(aes(x=day,y=occupancy, colour=species))+ 
  geom_line() +
  labs(x="day",y="Percent Predator-Prey Occupancy")+
  facet_wrap(position~structure)


#proportion/percent bottles occupied across replicates
prop_pa_posit<-all_pa_datas%>%
  group_by(structure,day,position)%>%
  summarise(prey.oc =sum(prey.oc)/total.bottle.nmbr, pred.oc= sum(pred.oc)/total.bottle.nmbr)%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_posit%>%
  ggplot(aes(x=day,y=occupancy, colour=species))+ 
  geom_line() +
  labs(x="day",y="Percent Predator-Prey Occupancy")+
  facet_wrap(position~structure)
################################################################################################################################################################
#3)collapse time
############################################################################################################################################
#Figure 2
prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

a<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
        #legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate, connectivity)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

b<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +theme(legend.position = c(0.85, 0.18),legend.background = element_blank(),
                                                                                  legend.box.background = element_rect(colour = "black"))

plot_grid(a,b)
############################################################################################################################################
#Phases Figure 5

prop_pa_rep_no_t<-all_pa_datas%>%
  filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

a<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+theme(legend.position = c(0.25, 0.5),legend.background = element_blank(),
                                                                                                                  legend.box.background = element_rect(colour = "black"))
#legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate, connectivity)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

b<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

c<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("c)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
#legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate, connectivity)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

d<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("d)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  filter(day  > 150)%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

e<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("e)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
#legend.position = "none")

prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  filter(day  > 150)%>%
  group_by(structure,replicate, connectivity)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

f<-prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  ggtitle("f)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) + theme(legend.position = "none")

plot_grid(a,b,c,d,e,f,ncol=2)


############################################################################################################################################

prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="connectivity",y="Percent Predator-Prey Occupancy")+
  facet_grid(~structure)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

prop_pa_rep_no_t<-all_pa_datas%>%
  group_by(structure,replicate, position)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_no_t%>%
  ggplot(aes(x=as.factor(position),y=occupancy, fill=species))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="position",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

prop_pa_rep_no_t<-all_pa_datas%>%
  group_by(structure,replicate, connectivity)%>%
  summarise(prey.oc =raster::cv(prey.oc), pred.oc= raster::cv(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_no_t%>%
  filter(connectivity >0)%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="CV Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

prop_pa_rep_no_t_prey<-prop_pa_rep_no_t%>%filter(species=='pred.oc')
dog<-aov(occupancy~as.factor(connectivity),prop_pa_rep_no_t_prey)
summary(dog)
TukeyHSD(dog)

prop_pa_rep_no_t%>%
  filter(connectivity >0)%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="CV Percent Predator-Prey Occupancy")+
  facet_wrap(~structure,scales="free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

prop_pa_rep_no_t<-all_pa_datas%>%
  group_by(structure,replicate, position)%>%
  summarise(prey.oc =cv(prey.oc), pred.oc= cv(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_no_t%>%
  filter(position != "H")%>%
  ggplot(aes(x=as.factor(position),y=occupancy, fill=species))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Position",y="CV Percent Predator-Prey Occupancy")+
  facet_wrap(~structure,scales="free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 


################################################################################################################################################################
#Neighboor Bottle connections
prop_pa_rep_no_t<-all_pa_datas%>%
  group_by(structure,nghbr_connect)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_no_t%>%
  #filter(connectivity >0)%>%
  ggplot(aes(x=(nghbr_connect),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Neighboor Connectivity",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) #+facet_grid(~structure)



prop_pa_rep_no_t_prey<-prop_pa_rep_no_t%>%filter(species=="pred.oc")
dog<-lm(occupancy~nghbr_connect,prop_pa_rep_no_t_prey)
summary(dog)

#by rep

prop_pa_rep_no_t<-all_pa_datas%>%
  group_by(structure,replicate,nghbr_connect)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_no_t%>%
  #filter(connectivity >0)%>%
  ggplot(aes(x=(nghbr_connect),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Neighboor Connectivity",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) #+facet_grid(~structure)

prop_pa_rep_no_t_prey<-prop_pa_rep_no_t%>%filter(species=="pred.oc")
dog<-lm(occupancy~nghbr_connect,prop_pa_rep_no_t_prey)
summary(dog)

#By bottle number
prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,bottle.number,nghbr_connect)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

prop_pa_rep_no_t%>%
  #filter(connectivity >0)%>%
  ggplot(aes(x=(nghbr_connect),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Neighboor Connectivity",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) #+facet_grid(~structure)

prop_pa_rep_no_t_prey<-prop_pa_rep_no_t%>%filter(species=="prey.oc")
dog<-lm(occupancy~nghbr_connect,prop_pa_rep_no_t_prey)
summary(dog)

################################################################################################################################################################
#Create lag of pred occupancy.
#Note Lage/lead does not work in tidyverse becuase it cannto serpate by group. Data combine slide works well

library(DataCombine)
all_pa_datass<-all_pa_datas

all_pa_dataz<-all_pa_datas%>%
  group_by(structure,replicate,nghbr_connect,bottle,bottle.number,connectivity)%>%
  summarise(prey.occupancy =mean(prey.oc), pred.occupancy= mean(pred.oc))
 

new_pa_datas <- slide(all_pa_datass, Var = "pred.oc", GroupVar = "bottle",
                   slideBy = -1)
newer_pa_datas <- slide(new_pa_datas, Var = "prey.oc", GroupVar = "bottle",
                      slideBy = -1)


################################################################################################################################################################
#Exctinction/Colonization Dynamics

Ext_col_data<-newer_pa_datas%>%
  #filter(structure=="isolated")%>%
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
  #ungroup()%>%
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
    left_join(all_pa_dataz, by="bottle")


Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=structure, y=value, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var, scales = "free")

Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=as.factor(connectivity), y=value, fill=as.factor(connectivity)))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var, scales = "free")

Ext_col_data%>%
  filter(structure !="isolated")%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey, key = "var", value = "value")%>%
  ggplot(aes(x=prey.occupancy, y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(~var, scales = "free")+
  theme_bw()

Ext_col_data%>%
  filter(structure !="isolated")%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey, key = "var", value = "value")%>%
  ggplot(aes(x=pred.occupancy, y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(~var, scales = "free")+
  theme_bw()

#Averages
Ext_col_data_av<-Ext_col_data%>%
  drop_na()%>%
  group_by(bottle.number,structure)%>%
  mutate(colonization_prob_pred_av =mean(colonization_prob_pred), colonization_prob_prey_av=mean(colonization_prob_prey),
         extinction_prob_pred_av=mean(extinction_prob_pred), extinction_prob_prey_av=mean(extinction_prob_prey))%>%
  distinct(bottle.number,structure,.keep_all = TRUE)


Ext_col_data_av%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey, key = "var", value = "value")%>%
  ggplot(aes(x=(nghbr_connect), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Average Nearest Neighboors Connectivity")+
  scale_color_viridis_d()+
  facet_grid(~var)

Ext_col_data_av%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey, key = "var", value = "value")%>%
  ggplot(aes(x=(connectivity), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  xlab("Connectivity")+
  facet_grid(~var)


##########################################################################################################################################
#Extinction Colonization Dynamics respect to repdator presence


Ext_col_data_pred<-newer_pa_datas%>%
  #filter(structure=="isolated")%>%
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
    colonization_col_prey_yes_pred=if_else(prey.oc==1 & lag.prey.oc==0  & pred.oc==1, 1,0), 
    non_colonization_col_prey_yes_pred=if_else(prey.oc==0 & lag.prey.oc==0 & pred.oc==1,1,0), 
    extinction_col_prey_yes_pred=if_else(prey.oc==0 & lag.prey.oc== 1 & pred.oc==1, 1,0),
    non_extinction_col_prey_yes_pred=if_else(prey.oc==1 & lag.prey.oc== 1& pred.oc==1, 1,0),
    colonization_col_prey_no_pred=if_else(prey.oc==1 & lag.prey.oc==0 & pred.oc==0,1,0), 
    non_colonization_col_prey_no_pred=if_else(prey.oc==0 & lag.prey.oc==0 & pred.oc==0,1,0), 
    extinction_col_prey_no_pred=if_else(prey.oc==0 & lag.prey.oc== 1& pred.oc==0, 1,0),
    non_extinction_col_prey_no_pred=if_else(prey.oc==1 & lag.prey.oc== 1& pred.oc==0, 1,0))%>%
  ungroup()%>%
  replace(is.na(.), 0)%>%
  group_by(bottle)%>%
  summarize(
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_sum_prey_no_pred=sum(colonization_col_prey_no_pred),non_colonization_sum_prey_no_pred=sum(non_colonization_col_prey_no_pred),extinction_sum_prey_no_pred=sum(extinction_col_prey_no_pred),non_extinction_sum_prey_no_pred=sum(non_extinction_col_prey_no_pred),
    colonization_sum_prey_yes_pred=sum(colonization_col_prey_yes_pred),non_colonization_sum_prey_yes_pred=sum(non_colonization_col_prey_yes_pred),extinction_sum_prey_yes_pred=sum(extinction_col_prey_yes_pred),non_extinction_sum_prey_yes_pred=sum(non_extinction_col_prey_yes_pred),
    colonization_prob_pred=sum(colonization_col_pred)/sum(colonization_col_pred+non_colonization_col_pred), extinction_prob_pred=sum(extinction_col_pred)/sum(extinction_col_pred+non_extinction_col_pred),
    colonization_prob_prey_no_pred=sum(colonization_col_prey_no_pred)/sum(colonization_col_prey_no_pred+non_colonization_col_prey_no_pred), extinction_prob_prey_no_pred=sum(extinction_col_prey_no_pred)/sum(extinction_col_prey_no_pred+non_extinction_col_prey_no_pred),
    colonization_prob_prey_yes_pred=sum(colonization_col_prey_yes_pred)/sum(colonization_col_prey_yes_pred+non_colonization_col_prey_yes_pred), extinction_prob_prey_yes_pred=sum(extinction_col_prey_yes_pred)/sum(extinction_col_prey_yes_pred+non_extinction_col_prey_yes_pred))%>%
  left_join(all_pa_dataz, by="bottle")


Ext_col_data_pred%>%
  gather(colonization_prob_prey_no_pred,colonization_prob_prey_yes_pred,extinction_prob_prey_no_pred,extinction_prob_prey_yes_pred,key = "var", value = "value")%>%
  ggplot(aes(x=structure, y=value, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var, scales = "free")

Ext_col_data_pred%>%
  filter(structure !="isolated")%>%
  gather(colonization_prob_prey_no_pred,colonization_prob_prey_yes_pred,extinction_prob_prey_no_pred,extinction_prob_prey_yes_pred,key = "var", value = "value")%>%
  ggplot(aes(x=pred.occupancy, y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(~var, scales = "free")+
  theme_bw()

Ext_col_data_pred%>%
  filter(structure !="isolated")%>%
  gather(colonization_prob_prey_no_pred,colonization_prob_prey_yes_pred,extinction_prob_prey_no_pred,extinction_prob_prey_yes_pred,key = "var", value = "value")%>%
  ggplot(aes(x=prey.occupancy, y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(~var, scales = "free")+
  theme_bw()

Ext_col_data_pred%>%
  gather(colonization_prob_prey_no_pred,colonization_prob_prey_yes_pred,extinction_prob_prey_no_pred,extinction_prob_prey_yes_pred,key = "var", value = "value")%>%
  ggplot(aes(x=(nghbr_connect), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Average Nearest Neighboors Connectivity")+
  scale_color_viridis_d()+
  facet_grid(~var)

Ext_col_data_pred%>%
  gather(colonization_prob_prey_no_pred,colonization_prob_prey_yes_pred,extinction_prob_prey_no_pred,extinction_prob_prey_yes_pred,key = "var", value = "value")%>%
  ggplot(aes(x=(connectivity), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  xlab("Connectivity")+
  facet_grid(~var)

#AVERAGES
Ext_col_data_pred_av<-Ext_col_data_pred%>%
  drop_na()%>%
  group_by(connectivity,structure)%>%
  mutate( colonization_prob_prey_no_pred_av=mean(colonization_prob_prey_no_pred),colonization_prob_prey_yes_pred_av=mean(colonization_prob_prey_yes_pred),
          extinction_prob_prey_no_pred_av=mean(extinction_prob_prey_no_pred),extinction_prob_prey_yes_pred_av=mean(extinction_prob_prey_yes_pred))%>%
  distinct(bottle.number,structure,.keep_all = TRUE)


Ext_col_data_pred_av%>%
  gather(colonization_prob_prey_no_pred_av,colonization_prob_prey_yes_pred_av,extinction_prob_prey_no_pred_av,extinction_prob_prey_yes_pred_av, key = "var", value = "value")%>%
  ggplot(aes(x=(nghbr_connect), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Average Nearest Neighboors Connectivity")+
  scale_color_viridis_d()+
  facet_grid(~var)

Ext_col_data_pred_av%>%
  gather(colonization_prob_prey_no_pred_av,colonization_prob_prey_yes_pred_av,extinction_prob_prey_no_pred_av,extinction_prob_prey_yes_pred_av, key = "var", value = "value")%>%
  ggplot(aes(x=(connectivity), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  xlab("Connectivity")+
  facet_grid(~var)


#Sums Ext/Colon
Ext_col_data_pred%>%
  gather(colonization_sum_prey_yes_pred,colonization_sum_prey_no_pred,non_colonization_sum_prey_yes_pred,extinction_sum_prey_yes_pred,extinction_sum_prey_no_pred,non_extinction_sum_prey_yes_pred,non_extinction_sum_prey_no_pred,key = "var", value = "value")%>%
  ggplot(aes(x=structure, y=value, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var, scales = "free")

Ext_col_data_pred%>%
  gather(colonization_sum_prey_yes_pred,colonization_sum_prey_no_pred,extinction_sum_prey_yes_pred,extinction_sum_prey_no_pred,key = "var", value = "value")%>%
  ggplot(aes(x=structure, y=value, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var, scales = "free")

Ext_col_data_pred%>%
  filter(structure !="isolated")%>%
  gather(colonization_sum_prey_yes_pred,colonization_sum_prey_no_pred,extinction_sum_prey_yes_pred,extinction_sum_prey_no_pred,key = "var", value = "value")%>%
  ggplot(aes(x=pred.occupancy, y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(~var, scales = "free")+
  theme_bw()

Ext_col_data_pred%>%
  gather(colonization_sum_prey_yes_pred,colonization_sum_prey_no_pred,extinction_sum_prey_yes_pred,extinction_sum_prey_no_pred,key = "var", value = "value")%>%
  ggplot(aes(x=(nghbr_connect), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Average Nearest Neighboors Connectivity")+
  scale_color_viridis_d()+
  facet_grid(~var)

Ext_col_data_pred%>%
  gather(colonization_sum_prey_yes_pred,colonization_sum_prey_no_pred,extinction_sum_prey_yes_pred,extinction_sum_prey_no_pred,key = "var", value = "value")%>%
  ggplot(aes(x=(connectivity), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  xlab("Connectivity")+
  facet_grid(~var)

Ext_col_data_pred%>%
  gather(colonization_sum_prey_yes_pred,colonization_sum_prey_no_pred,extinction_sum_prey_yes_pred,extinction_sum_prey_no_pred,key = "var", value = "value")%>%
  ggplot(aes(x=(connectivity), y=value, fill=as.factor(connectivity)))+
  geom_boxplot()+
  scale_color_viridis(discrete=TRUE)+
  xlab("Connectivity")+
  facet_grid(~var)

##########################################################################################################################################


####################################################################################################################################################################################################################################################################################

