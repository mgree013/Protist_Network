library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)

####################################################################################################################################################################################
#1) Occupancy Data
prop_pa_rep_no_t_env<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")%>%
  left_join(env, by=c("bottle","structure"))%>%
  mutate(bac.density_log=log(bac.density))
str(prop_pa_rep_no_t_env)

prop_pa_rep_no_t_env%>%
  gather(bottle.color,temp.,DO ,DO.percent,cond.,ppm.nitrate,ppm.nitrite,ppm.ammonia,rate.of.resp,pH,avg.chlorophyll,bac.density_log,key = "var", value = "value")%>%
  ggplot(aes(x=structure,y=value, fill=structure))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="structure")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_wrap(~var, scales = "free")

prop_pa_rep_no_t_env%>%
  gather(bottle.color,temp.,DO ,DO.percent,cond.,ppm.nitrate,ppm.nitrite,ppm.ammonia,rate.of.resp,pH,avg.chlorophyll,bac.density_log,key = "var", value = "value")%>%
  ggplot(aes(x=as.factor(connectivity),y=value, fill=as.factor(connectivity)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="structure")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_wrap(~var, scales = "free")

prop_pa_rep_no_t_env%>%
  gather(bottle.color,temp.,DO ,DO.percent,cond.,ppm.nitrate,ppm.nitrite,ppm.ammonia,rate.of.resp,pH,avg.chlorophyll,bac.density_log,key = "var", value = "value")%>%
  ggplot(aes(x=(value),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_wrap(~var, scales = "free")

prop_pa_rep_no_t_env%>%
  filter(structure !="control")%>%
  ggplot(aes(x=(avg.chlorophyll),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Chlorophyll",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) #+facet_grid(~structure)

prop_pa_rep_no_t_env%>%
  filter(structure !="control")%>%
  ggplot(aes(x=log(bac.density),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Bacteria density",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) #+facet_grid(~structure)

prop_pa_rep_no_t_env%>%
  ggplot(aes(x=(avg.chlorophyll),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Chlorophyll",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

prop_pa_rep_no_t_env%>%
  ggplot(aes(x=log(bac.density),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Bacteria density",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

####################################################################################################################################################################################
#2) Ext-Col data
Ext_col_data_envs<-Ext_col_data_glm%>%
  left_join(env, by=c("bottle", "structure"))%>%
  mutate(bac.density_log=log(bac.density))
str(Ext_col_data_envs)

Ext_col_data_envs%>%
  gather(bottle.color,temp.,DO ,DO.percent,cond.,ppm.nitrate,ppm.nitrite,ppm.ammonia,rate.of.resp,pH,avg.chlorophyll,bac.density_log,key = "var", value = "value")%>%
  ggplot(aes(x=(value),y=colonization_prob_prey, colour=var))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(y="Colonization Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_wrap(~var, scales = "free")

Ext_col_data_envs%>%
  gather(bottle.color,temp.,DO ,DO.percent,cond.,ppm.nitrate,ppm.nitrite,ppm.ammonia,rate.of.resp,pH,avg.chlorophyll,bac.density_log,key = "var", value = "value")%>%
  ggplot(aes(x=(value),y=colonization_prob_pred, colour=var))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(y="Colonization Probability Pred")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_wrap(~var, scales = "free")

Ext_col_data_envs%>%
  gather(bottle.color,temp.,DO ,DO.percent,cond.,ppm.nitrate,ppm.nitrite,ppm.ammonia,rate.of.resp,pH,avg.chlorophyll,bac.density_log,key = "var", value = "value")%>%
  ggplot(aes(x=(value),y=extinction_prob_pred, colour=var))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(y="Extinction Probability Pred")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_wrap(~var, scales = "free")

Ext_col_data_envs%>%
  gather(bottle.color,temp.,DO ,DO.percent,cond.,ppm.nitrate,ppm.nitrite,ppm.ammonia,rate.of.resp,pH,avg.chlorophyll,bac.density_log,key = "var", value = "value")%>%
  ggplot(aes(x=(value),y=extinction_prob_prey, colour=var))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(y="Extinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_wrap(~var, scales = "free")

Ext_col_data_envs%>%
  ggplot(aes(x=avg.chlorophyll,y=colonization_prob_prey, colour=structure))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Chlorophyll",y="Extinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

Ext_col_data_envs%>%
  filter(structure !="control")%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=log(bac.density),y=value, colour=var))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Bacteria density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  facet_grid(structure~var, scales = "free")

Ext_col_data_envs%>%
  filter(structure !="control")%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=avg.chlorophyll,y=value, colour=var))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Chlorophyll")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  facet_grid(structure~var, scales = "free")

####################################################################################################################################################################################
#Env and Density

env_density<-Data%>%
  #filter(day >150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))
str(env_density)


env_density%>%
  gather(bottle.color,temp.,DO ,DO.percent,cond.,ppm.nitrate,ppm.nitrite,ppm.ammonia,rate.of.resp,pH,avg.chlorophyll,bac.density_log,key = "var", value = "value")%>%
  ggplot(aes(x=(value),y=density, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(y="Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_wrap(~var, scales = "free")

env_density%>%
  ggplot(aes(x=(avg.chlorophyll),y=density, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Chlorophyll",y="Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_grid(~structure)

env_density%>%
  ggplot(aes(x=log(bac.density),y=density, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Bacteria density",y="Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) +facet_grid(~structure)

env_density%>%
  ggplot(aes(x=(avg.chlorophyll),y=density, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Average Chlorophyll",y=" Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

env_density%>%
  ggplot(aes(x=log(bac.density),y=density, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  labs(x="Bacteria density",y="Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 


#figure 8:
env_density2<-Data%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  #pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))
str(env_density2)

aaa<-env_density%>%
  ggplot(aes(x=(bac.density_log),y=density, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("a)")+
  scale_color_viridis_d()+
  labs(x="Bacteria density",y=" Predator-Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+theme(legend.position = c(0.91, 0.97),legend.background = element_blank(),
                                                                                 legend.box.background = element_rect(colour = "black")) 

bbb<-env_density2%>%
  ggplot(aes(x=(pred),y=prey))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("b)")+
  scale_color_viridis_d()+
  labs(x="Predator density",y=" Prey Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) 

plot_grid(aaa,bbb)


#figuire 8:occupancy
prop_pa_rep_no_t_env2<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
 # pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")%>%
  left_join(env, by=c("bottle","structure"))%>%
  mutate(bac.density_log=log(bac.density))

a1<-prop_pa_rep_no_t_env%>%
  filter(structure !="control")%>%
  ggplot(aes(x=log(bac.density),y=occupancy, colour=species))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  ggtitle("a)")+
  labs(x="Bacteria density",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) #+facet_grid(~structure)

a2<-prop_pa_rep_no_t_env2%>%
  filter(structure !="control")%>%
  ggplot(aes(x=pred.oc,y=prey.oc))+ 
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("b)")+
  scale_color_viridis_d()+
  labs(x="Predator occupancy",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank()) #+facet_grid(~structure)
plot_grid(a1,a2)
