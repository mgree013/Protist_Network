#GLM's
library(multcomp)
library(tidyverse)
library(viridis)
library(cowplot)

#http://www.simonqueenborough.info/R/stats-basic/glm.html

########################################################################################################################
#1)Occupany 

#1A) Occupancy by network

prop_pa_rep_no_t<-all_pa_datas%>%
  filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)


prop_pa_rep_no_t%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")%>%
  ggplot(aes(x=as.factor(structure),y=occupancy, fill=species))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())
########################################################################
#1B) Prey
y <- cbind(prop_pa_rep_no_t$prey.occupany, prop_pa_rep_no_t$prey.absence)

dog1<-glm(y~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog2<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog3<-glm(y~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog4<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

prop_pa_rep_no_t$structure<-as.factor(prop_pa_rep_no_t$structure)
dog0<-glm(y~-1 +structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dogg<-glht(dog0,mcp(structure = "Tukey"), test=adjusted(type="holm"))
summary(dogg)

prop_pa_rep_no_t$replicate<-as.factor(prop_pa_rep_no_t$replicate)
dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)
########################################################################
#1B) Predator
y <- cbind(prop_pa_rep_no_t$pred.occupany, prop_pa_rep_no_t$pred.absence)

dog1<-glm(y~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog2<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog3<-glm(y~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog4<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

prop_pa_rep_no_t$structure<-as.factor(prop_pa_rep_no_t$structure)
dog0<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dogg<-glht(dog0, linfct=mcp(structure = "Tukey"))
summary(dogg)

prop_pa_rep_no_t$replicate<-as.factor(prop_pa_rep_no_t$replicate)
dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)


####################################################################################################################################################################################
####################################################################################################################################################################################
#2)Connectivity Occupancy


prop_pa_rep_connect<-all_pa_datas%>%
  filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate, connectivity, bottle)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)

prop_pa_rep_connect$connectivity<-as.factor(prop_pa_rep_connect$connectivity)

prop_pa_rep_connect%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")%>%
  ggplot(aes(x=as.factor(connectivity),y=occupancy, fill=species))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())


########################################################################
#1B) Prey
y <- cbind(prop_pa_rep_connect$prey.occupany, prop_pa_rep_connect$prey.absence)

dog1<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog2<-glm(y~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog3<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog4<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog5<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog6<-glm(y~replicate, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog7<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog8<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8)
r22<-as.data.frame(r2, ncol=1)
r22

prop_pa_rep_connect$structure<-as.factor(prop_pa_rep_connect$structure)
prop_pa_rep_connect$replicate<-as.factor(prop_pa_rep_connect$replicate)
prop_pa_rep_connect$connectivity<-as.factor(prop_pa_rep_connect$connectivity)

dog0<-glm(y~connectivity, family=binomial(), data=prop_pa_rep_connect)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)
########################################################################
#1B) Predator
y <- cbind(prop_pa_rep_connect$pred.occupany, prop_pa_rep_connect$pred.absence)

dog1<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog2<-glm(y~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog3<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog4<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog5<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog6<-glm(y~replicate, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog7<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog8<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8, weights = TRUE, sort = FALSE)
reported.table2

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8)
r22<-as.data.frame(r2, ncol=1)
r22

prop_pa_rep_connect$structure<-as.factor(prop_pa_rep_connect$structure)
prop_pa_rep_connect$connectivity<-as.factor(prop_pa_rep_connect$connectivity)
prop_pa_rep_connect$replicate<-as.factor(prop_pa_rep_connect$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


########################################################################################################################
########################################################################################################################
#2) Extinction-Coolonization Dynamics

Ext_col_data_glm<-Ext_col_data%>%filter(structure!="control")

#2A)Structure Effects-Colonization
aa<-Ext_col_data_glm%>%
  ggplot(aes(x=structure,y=colonization_prob_pred, fill=structure))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

bb<-Ext_col_data_glm%>%
  ggplot(aes(x=as.factor(structure),y=colonization_prob_prey, fill=structure))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Colonization Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+theme(legend.position = c(0.85, 0.9))

cc<-Ext_col_data_glm%>%
  ggplot(aes(x=as.factor(structure),y=extinction_prob_pred, fill=structure))+ 
  geom_boxplot()+
  ggtitle("c)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Extinction Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

dd<-Ext_col_data_glm%>%
  ggplot(aes(x=as.factor(structure),y=extinction_prob_prey, fill=structure))+ 
  geom_boxplot()+
  ggtitle("d)") +
  ylim(0,0.6)+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Extinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

plot_grid(aa,bb,cc,dd,ncol=2)
########################################################################
#Prey
y <- cbind(Ext_col_data_glm$colonization_sum_prey, Ext_col_data_glm$non_colonization_sum_prey)

dog1<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
dog0<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(structure = "Tukey"))
summary(dogg)

Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)
dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)
########################################################################
# Predator
y <- cbind(Ext_col_data_glm$colonization_sum_pred, Ext_col_data_glm$non_colonization_sum_pred)

dog1<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
dog0<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(structure = "Tukey"))
summary(dogg)

Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)
dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

################################################################################################################################################
#2A)Extinction
Ext_col_data_glm%>%
  ggplot(aes(x=as.factor(structure),y=extinction_prob_pred, fill=structure))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())
########################################################################
#1B) Prey
y <- cbind(Ext_col_data_glm$extinction_sum_prey, Ext_col_data_glm$non_extinction_sum_prey)

dog1<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
dog0<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(structure = "Tukey"))
summary(dogg)

Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)
dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)
########################################################################
#2B) Predator
y <- cbind(Ext_col_data_glm$extinction_sum_pred, Ext_col_data_glm$non_extinction_sum_pred)

dog1<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
dog0<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(structure = "Tukey"))
summary(dogg)

Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)
dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

################################################################################################################################################
#Connectivity Effects on COlonization_ext Dynamcis
Ext_col_data_glm%>%
  ggplot(aes(x=as.factor(connectivity),y=colonization_prob_pred, fill=as.factor(connectivity)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())
########################################################################
#Prey
y <- cbind(Ext_col_data_glm$colonization_sum_prey, Ext_col_data_glm$non_colonization_sum_prey)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)

dog1<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8)
r22<-as.data.frame(r2, ncol=1)

Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)
dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)
########################################################################
# Predator
y <- cbind(Ext_col_data_glm$colonization_sum_pred, Ext_col_data_glm$non_colonization_sum_pred)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)

dog1<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8)
r22<-as.data.frame(r2, ncol=1)
################################################################################################################################################
#2A)Extinction
Ext_col_data_glm%>%
  ggplot(aes(x=as.factor(connectivity),y=extinction_prob_prey, fill=as.factor(connectivity)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Network Structure",y="Percent Predator-Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())
########################################################################
#1B) Prey
y <- cbind(Ext_col_data_glm$extinction_sum_prey, Ext_col_data_glm$non_extinction_sum_prey)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)

dog1<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8)
r22<-as.data.frame(r2, ncol=1)
########################################################################
#2B) Predator
y <- cbind(Ext_col_data_glm$extinction_sum_pred, Ext_col_data_glm$non_extinction_sum_pred)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
dog1<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8)
r22<-as.data.frame(r2, ncol=1)
####################################################################################################
#Plots
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)

aaa<-Ext_col_data_glm%>%
  ggplot(aes(x=connectivity,y=colonization_prob_pred, fill=connectivity))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

bbb<-Ext_col_data_glm%>%
  ggplot(aes(x=connectivity,y=colonization_prob_prey, fill=connectivity))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity ",y="Colonization Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

ccc<-Ext_col_data_glm%>%
  ggplot(aes(x=connectivity,y=extinction_prob_pred, fill=connectivity))+ 
  geom_boxplot()+
  ggtitle("c)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Extinction Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

ddd<-Ext_col_data_glm%>%
  ggplot(aes(x=connectivity,y=extinction_prob_prey, fill=connectivity))+ 
  geom_boxplot()+
  ggtitle("d)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Connectivity",y="Extinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

plot_grid(aaa,bbb,ccc,ddd,ncol=2)

########################################################################################################################################################################################################################
#3)Trophic Interactions

ext1<-Ext_col_data_glm%>%
  filter(structure !="control")%>%
  ggplot(aes(x=pred.occupancy,y=colonization_prob_prey, colour=structure))+ 
  geom_point()+
  ggtitle("a)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Occupancy",y="Colonization Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
########################################################################
#3A)Pred  Occ vs. colonization_prob_prey
y <- cbind(Ext_col_data_glm$colonization_sum_prey, Ext_col_data_glm$non_colonization_sum_prey)

dog1<-glm(y~structure+replicate+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+pred.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog10<-glm(y~pred.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog11<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)

################################################################################################
#3B) Pred Occup vs. Colon Pred

ext2<-Ext_col_data_glm%>%
  filter(structure !="control")%>%
  ggplot(aes(x=pred.occupancy,y=colonization_prob_pred, colour=structure))+ 
  geom_point()+
  ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Occupancy",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
########################################################################
#Pred  Occ vs. colonization_prob_pred
y <- cbind(Ext_col_data_glm$colonization_sum_pred, Ext_col_data_glm$non_colonization_sum_pred)

dog1<-glm(y~structure+replicate+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+pred.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog10<-glm(y~pred.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog11<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)
r22
################################################################################################################################################

#3C) Pred Occup vs. Ext Prey

ext3<-Ext_col_data_glm%>%
  filter(structure !="control")%>%
  ggplot(aes(x=pred.occupancy,y=extinction_prob_prey, colour=structure))+ 
  geom_point()+
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Occupancy",y="Extinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
########################################################################
#Pred  Occ vs. extinction_prob_prey
y <- cbind(Ext_col_data_glm$extinction_sum_prey, Ext_col_data_glm$non_extinction_sum_prey)

dog1<-glm(y~structure+replicate+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+pred.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog10<-glm(y~pred.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog11<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)
r22

################################################################################################################################################

#3D) Pred Occup vs. Ext Pred

ext4<-Ext_col_data_glm%>%
  filter(structure !="control")%>%
  ggplot(aes(x=pred.occupancy,y=extinction_prob_pred, colour=structure))+ 
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Occupancy",y="Extinction Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+theme(legend.position = c(0.85, 0.75),legend.background = element_blank(),
                                                                                 legend.box.background = element_rect(colour = "black"))
########################################################################
#Pred  Occ vs. extinction_prob_prey
y <- cbind(Ext_col_data_glm$extinction_sum_pred, Ext_col_data_glm$non_extinction_sum_pred)

dog1<-glm(y~structure+replicate+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+pred.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog10<-glm(y~pred.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog11<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)
r22
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
#3-1 Torphic Interactions Prey Occ
ext5<-Ext_col_data_glm%>%
  filter(structure !="control")%>%
  ggplot(aes(x=prey.occupancy,y=colonization_prob_prey, colour=structure))+ 
  geom_point()+
  ggtitle("e)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Occupancy",y="Colonization Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
########################################################################
#3A)Prey  Occ vs. colonization_prob_prey
y <- cbind(Ext_col_data_glm$colonization_sum_prey, Ext_col_data_glm$non_colonization_sum_prey)

dog1<-glm(y~structure+replicate+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+prey.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog10<-glm(y~prey.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog11<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)

################################################################################################
#3B) Prey Occup vs. Colon Pred

ext6<-Ext_col_data_glm%>%
  filter(structure !="control")%>%
  ggplot(aes(x=prey.occupancy,y=colonization_prob_pred, colour=structure))+ 
  geom_point()+
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Occupancy",y="Colonization Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
########################################################################
#Prey  Occ vs. colonization_prob_pred
y <- cbind(Ext_col_data_glm$colonization_sum_pred, Ext_col_data_glm$non_colonization_sum_pred)

dog1<-glm(y~structure+replicate+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+prey.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog10<-glm(y~prey.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog11<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8)
r22<-as.data.frame(r2, ncol=1)
################################################################################################################################################

#3C) Prey Occup vs. Ext Prey

ext7<-Ext_col_data_glm%>%
  filter(structure !="control")%>%
  ggplot(aes(x=prey.occupancy,y=extinction_prob_prey, colour=structure))+ 
  geom_point()+
  ggtitle("g)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Occupancy",y="Extinction Probability Prey")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
########################################################################
#Prey  Occ vs. extinction_prob_prey
y <- cbind(Ext_col_data_glm$extinction_sum_prey, Ext_col_data_glm$non_extinction_sum_prey)

dog1<-glm(y~structure+replicate+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+prey.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog10<-glm(y~prey.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog11<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)

dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8)
r22<-as.data.frame(r2, ncol=1)


################################################################################################################################################

#3D) Prey Occup vs. Ext Pred

ext8<-Ext_col_data_glm%>%
  filter(structure !="control")%>%
  ggplot(aes(x=prey.occupancy,y=extinction_prob_pred, colour=structure))+ 
  geom_point()+
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Occupancy",y="Extinction Probability Predator")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
########################################################################
#Prey  Occ vs. extinction_prob_prey
y <- cbind(Ext_col_data_glm$extinction_sum_pred, Ext_col_data_glm$non_extinction_sum_pred)

dog1<-glm(y~structure+replicate+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+prey.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog5<-glm(y~structure+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~replicate+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog10<-glm(y~prey.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog11<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

Ext_col_data_glm$structure<-as.factor(Ext_col_data_glm$structure)
Ext_col_data_glm$connectivity<-as.factor(Ext_col_data_glm$connectivity)
Ext_col_data_glm$replicate<-as.factor(Ext_col_data_glm$replicate)

dog0<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)


dogg<-glht(dog0, linfct=mcp(replicate = "Tukey"))
summary(dogg)
####
y <- cbind(Ext_col_data_glm$extinction_sum_pred, Ext_col_data_glm$non_extinction_sum_pred)
#y <- cbind(Ext_col_data_glm$extinction_sum_prey, Ext_col_data_glm$non_extinction_sum_prey)
#y <- cbind(Ext_col_data_glm$colonization_sum_pred, Ext_col_data_glm$non_colonization_sum_pred)
#y <- cbind(Ext_col_data_glm$colonization_sum_prey, Ext_col_data_glm$non_colonization_sum_prey)


dog0<-glm(y~structure+replicate+connectivity+pred.occupancy+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

dog1<-glm(y~structure+replicate+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog2<-glm(y~structure+replicate+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog3<-glm(y~structure+replicate+pred.occupancy+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog4<-glm(y~structure+connectivity+pred.occupancy+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

dog5<-glm(y~replicate+connectivity+pred.occupancy+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog6<-glm(y~structure+replicate+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog7<-glm(y~structure+replicate+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog8<-glm(y~structure+connectivity+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9<-glm(y~structure+replicate+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9.1<-glm(y~structure+connectivity+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9.2<-glm(y~structure+replicate+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog9.3<-glm(y~structure+pred.occupancy+prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)


dog10<-glm(y~structure+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)#
dog10.1<-glm(y~replicate+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)#
dog11<-glm(y~replicate+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog12<-glm(y~structure+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog13<-glm(y~connectivity+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog14<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog15<-glm(y~prey.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)#
dog16<-glm(y~pred.occupancy+replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)#
dog17<-glm(y~pred.occupancy+connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)#
dog18<-glm(y~prey.occupancy+pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)#
dog18.1<-glm(y~prey.occupancy+structure, family=binomial(link = "logit"), data=Ext_col_data_glm)#

dog19<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog20<-glm(y~replicate, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog21<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog22<-glm(y~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog23<-glm(y~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

dog24<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_glm)
reported.table2 <- bbmle::AICtab(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog9.1,dog9.2,dog9.3,dog10,dog10.1,dog11,dog12,dog13,dog14,dog15,dog16,dog17,dog18,dog18.1,dog19,dog20,dog21,dog22,dog23,dog24, weights = TRUE, sort = FALSE)
reported.table2

pseudoR0 <- ((dog0$null.deviance-dog0$deviance)/dog0$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR9.1 <- ((dog9.1$null.deviance-dog9.1$deviance)/dog9.1$null.deviance)
pseudoR9.2 <- ((dog9.2$null.deviance-dog9.2$deviance)/dog9.2$null.deviance)
pseudoR9.3 <- ((dog9.3$null.deviance-dog9.3$deviance)/dog9.3$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR10.1 <- ((dog10.1$null.deviance-dog10.1$deviance)/dog10.1$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)
pseudoR16 <- ((dog16$null.deviance-dog16$deviance)/dog16$null.deviance)
pseudoR17 <- ((dog17$null.deviance-dog17$deviance)/dog17$null.deviance)
pseudoR18 <- ((dog18$null.deviance-dog18$deviance)/dog18$null.deviance)
pseudoR18.1 <- ((dog18.1$null.deviance-dog18.1$deviance)/dog18.1$null.deviance)
pseudoR19 <- ((dog19$null.deviance-dog19$deviance)/dog19$null.deviance)
pseudoR20 <- ((dog20$null.deviance-dog20$deviance)/dog20$null.deviance)
pseudoR21 <- ((dog21$null.deviance-dog21$deviance)/dog21$null.deviance)
pseudoR22 <- ((dog22$null.deviance-dog22$deviance)/dog22$null.deviance)
pseudoR23 <- ((dog23$null.deviance-dog23$deviance)/dog23$null.deviance)
pseudoR24 <- ((dog24$null.deviance-dog24$deviance)/dog24$null.deviance)

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR9.1,pseudoR9.2,pseudoR9.3,pseudoR10,pseudoR10.1,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16,pseudoR17,pseudoR18,pseudoR18.1,pseudoR19,pseudoR20,pseudoR21,pseudoR22,pseudoR23,pseudoR24)
r22<-as.data.frame(r2, ncol=1)
r22


########
plot_grid(ext1,ext2,ext3,ext4,ext5,ext6,ext7,ext8, ncol=4)

################################################################################################
#Figure 8: Trophic Interactions

prop_pa_rep_no_t_env2
prop_pa_rep_no_t_env

prop_pa_rep_no_t_env2_glm<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate,bottle)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  left_join(env, by=c("bottle","structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

view(prop_pa_rep_no_t_env2_glm)
#prey occupancy
y <- cbind(prop_pa_rep_no_t_env2_glm$prey.occupany, prop_pa_rep_no_t_env2_glm$prey.absence)

dog0<-glm(y~pred.oc+bac.density_log+replicate+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

dog1<-glm(y~pred.oc+bac.density_log+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog2<-glm(y~pred.oc+bac.density_log+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog3<-glm(y~pred.oc+replicate+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog4<-glm(y~bac.density_log+replicate+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

dog5<-glm(y~pred.oc+bac.density_log, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog6<-glm(y~replicate+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog7<-glm(y~pred.oc+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog8<-glm(y~pred.oc+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog9<-glm(y~bac.density_log+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog10<-glm(y~bac.density_log+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

dog11<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog13<-glm(y~bac.density_log, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog14<-glm(y~pred.oc, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

dog15<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

reported.table2 <- bbmle::AICtab(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

pseudoR0 <- ((dog0$null.deviance-dog0$deviance)/dog0$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)


r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)
r22


#preadtor occupnacy
y <- cbind(prop_pa_rep_no_t_env2_glm$pred.occupany, prop_pa_rep_no_t_env2_glm$pred.absence)

dog0<-glm(y~prey.oc+bac.density_log+replicate+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

dog1<-glm(y~prey.oc+bac.density_log+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog2<-glm(y~prey.oc+bac.density_log+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog3<-glm(y~prey.oc+replicate+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog4<-glm(y~bac.density_log+replicate+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

dog5<-glm(y~prey.oc+bac.density_log, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog6<-glm(y~replicate+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog7<-glm(y~prey.oc+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog8<-glm(y~prey.oc+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog9<-glm(y~bac.density_log+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog10<-glm(y~bac.density_log+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

dog11<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog12<-glm(y~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog13<-glm(y~bac.density_log, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)
dog14<-glm(y~prey.oc, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

dog15<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_env2_glm)

reported.table2 <- bbmle::AICtab(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

pseudoR0 <- ((dog0$null.deviance-dog0$deviance)/dog0$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)


r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)
r22
####################################################################################
#Density

env_density2<-Data%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  #pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

#prey density

dog0<-glm(prey~pred+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog1<-glm(prey~pred+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog2<-glm(prey~pred+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog3<-glm(prey~pred+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog4<-glm(prey~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog5<-glm(prey~pred+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog6<-glm(prey~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog7<-glm(prey~pred+replicate, family=gaussian(link = "identity"), data=env_density2)
dog8<-glm(prey~pred+structure, family=gaussian(link = "identity"), data=env_density2)
dog9<-glm(prey~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog10<-glm(prey~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog11<-glm(prey~structure, family=gaussian(link = "identity"), data=env_density2)
dog12<-glm(prey~replicate, family=gaussian(link = "identity"), data=env_density2)
dog13<-glm(prey~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog14<-glm(prey~pred,family=gaussian(link = "identity"), data=env_density2)

dog15<-glm(y~1,family=gaussian(link = "identity"), data=env_density2)

reported.table2 <- bbmle::AICtab(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

pseudoR0 <- ((dog0$null.deviance-dog0$deviance)/dog0$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)


r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)
r22


#preadtor density
dog0<-glm(pred~prey+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog1<-glm(pred~prey+bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog2<-glm(pred~prey+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog3<-glm(pred~prey+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog4<-glm(pred~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog5<-glm(pred~prey+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog6<-glm(pred~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog7<-glm(pred~prey+replicate, family=gaussian(link = "identity"), data=env_density2)
dog8<-glm(pred~prey+structure, family=gaussian(link = "identity"), data=env_density2)
dog9<-glm(pred~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog10<-glm(pred~bac.density_log+structure,family=gaussian(link = "identity"), data=env_density2)

dog11<-glm(pred~structure, family=gaussian(link = "identity"), data=env_density2)
dog12<-glm(pred~replicate,family=gaussian(link = "identity"), data=env_density2)
dog13<-glm(pred~bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog14<-glm(pred~prey, family=gaussian(link = "identity"), data=env_density2)

dog15<-glm(y~1, family=gaussian(link = "identity"), data=env_density2)

reported.table2 <- bbmle::AICtab(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15, weights = TRUE, sort = FALSE)
reported.table2

pseudoR0 <- ((dog0$null.deviance-dog0$deviance)/dog0$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR5 <- ((dog5$null.deviance-dog5$deviance)/dog5$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)
pseudoR7 <- ((dog7$null.deviance-dog7$deviance)/dog7$null.deviance)
pseudoR8 <- ((dog8$null.deviance-dog8$deviance)/dog8$null.deviance)
pseudoR9 <- ((dog9$null.deviance-dog9$deviance)/dog9$null.deviance)
pseudoR10 <- ((dog10$null.deviance-dog10$deviance)/dog10$null.deviance)
pseudoR11 <- ((dog11$null.deviance-dog11$deviance)/dog11$null.deviance)
pseudoR12 <- ((dog12$null.deviance-dog12$deviance)/dog12$null.deviance)
pseudoR13 <- ((dog13$null.deviance-dog13$deviance)/dog13$null.deviance)
pseudoR14 <- ((dog14$null.deviance-dog14$deviance)/dog14$null.deviance)
pseudoR15 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)


r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15)
r22<-as.data.frame(r2, ncol=1)
r22