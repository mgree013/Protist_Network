#GLM's
library(betareg)
########################################################################################################################
#1)Occupany 

#1A) Occupancy by network

prop_pa_rep_no_tz<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))


##############################################################################
#1A1-prey occupancy

prop_pa_rep_no_t_prey<-prop_pa_rep_no_t%>%filter(species=="prey.oc")

dog1<-glm(y~structure+replicate, family=quasibinomial, data=prop_pa_rep_no_t)
dog2<-glm(y~structure, family=quasibinomial, data=prop_pa_rep_no_t)
dog3<-glm(y~replicate, family=quasibinomial, data=prop_pa_rep_no_t)
dog4<-glm(y~1, family=quasibinomial, data=prop_pa_rep_no_t)
reported.table2 <- bbmle::AICtab(dog2,dog4, weights = TRUE, sort = FALSE)

summary(dog1)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

aov.prey<-aov(occupancy~structure+replicate,data = prop_pa_rep_no_t_prey)
summary(aov.prey)
TukeyHSD(aov.prey)

#correct
prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
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

y <- cbind(prop_pa_rep_no_t$prey.occupany, prop_pa_rep_no_t$n)

dog1<-glm(y~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog2<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog3<-glm(y~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog4<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

y <- cbind(prop_pa_rep_no_t$pred.occupany, prop_pa_rep_no_t$n)

dog1<-glm(y~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog2<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog3<-glm(y~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog4<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2
summary(dog1)
summary(dog2)

prop_pa_rep_no_t$structure<-as.factor(prop_pa_rep_no_t$structure)
library(multcomp)
dog0<-glm(y~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dogg<-glht(dog0, linfct=mcp(structure = "Tukey"))
summary(dogg)
##############################################################################
#1A2-pred occupancy
prop_pa_rep_no_t_pred<-prop_pa_rep_no_t%>%filter(species=="pred.oc")

dog1<-glm(occupancy~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
dog2<-glm(occupancy~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
dog3<-glm(occupancy~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
dog4<-glm(occupancy~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
summary(dog2)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
reported.table2 <- bbmle::AICtab(dog2,dog4, weights = TRUE, sort = FALSE)

#pseudoR0 <- ((dog0$null.deviance-dog0$deviance)/dog0$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)


aov.prey<-aov(occupancy~structure,data = prop_pa_rep_no_t_pred)
summary(aov.prey)
TukeyHSD(aov.prey)
############################################################################################################################################################
#1B)Connectivity

prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate,connectivity)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

##############################################################################
#1B1-prey occupancy
prop_pa_rep_no_t_prey<-prop_pa_rep_no_t%>%filter(species=="prey.oc")

dog0<-glm(occupancy~connectivity*structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_prey)
dog1<-glm(occupancy~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_prey)
dog2<-glm(occupancy~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_prey)
dog3<-glm(occupancy~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_prey)
dog4<-glm(occupancy~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_no_t_prey)
dog5<-glm(occupancy~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_prey)

reported.table2 <- bbmle::AICtab(dog0,dog1, dog2,dog3,dog4,dog5, weights = TRUE, sort = FALSE)
reported.table2
pseudoR0 <- ((dog0$null.deviance-dog0$deviance)/dog0$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

aov.prey<-aov(occupancy~as.factor(connectivity),data = prop_pa_rep_no_t_prey)
summary(aov.prey)
TukeyHSD(aov.prey)


#TABLE 
prop_pa_rep_no_t_prey$connectivity<-as.factor(prop_pa_rep_no_t_prey$connectivity)
library(multcomp)
dog0<-glm(occupancy~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_no_t_prey)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
##############################################################################
#1B2-pred occupancy
prop_pa_rep_no_t_pred<-prop_pa_rep_no_t%>%filter(species=="pred.oc")

#dog0<-glm(occupancy~connectivity+structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
#dog1<-glm(occupancy~structure+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
dog2<-glm(occupancy~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
#dog3<-glm(occupancy~replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
dog4<-glm(occupancy~connectivity+structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
#dog5<-glm(occupancy~connectivity+replicate, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
dog6<-glm(occupancy~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
dog7<-glm(occupancy~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)

reported.table2 <- bbmle::AICtab(dog0,dog1, dog2,dog3,dog4,dog5,dog6,dog7, weights = TRUE, sort = FALSE)
reported.table2 <- bbmle::AICtab(dog0,dog1,dog5,dog7, weights = TRUE, sort = FALSE)
reported.table2 <- bbmle::AICtab(dog2,dog4,dog6,dog7, weights = TRUE, sort = FALSE)
reported.table2 <- bbmle::AICtab(dog6,dog7, weights = TRUE, sort = FALSE)

pseudoR0 <- ((dog0$null.deviance-dog0$deviance)/dog0$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)
pseudoR6 <- ((dog6$null.deviance-dog6$deviance)/dog6$null.deviance)

aov.prey<-aov(occupancy~as.factor(connectivity),data = prop_pa_rep_no_t_pred)
summary(aov.prey)
TukeyHSD(aov.prey)

prop_pa_rep_no_t_pred$connectivity<-as.factor(prop_pa_rep_no_t_pred$connectivity)
library(multcomp)
dog0<-glm(occupancy~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_no_t_pred)
dogg<-glht(dog0, linfct=mcp(connectivity = "Tukey"))
summary(dogg)

########################################################################################################################
#2)Extinction-Colonization
dog1<-glm(colonization_prob_pred~structure, family=binomial(link = "logit"), data=Ext_col_data)
dog2<-glm(colonization_prob_pred~(nghbr_connect), family=binomial(link = "logit"), data=Ext_col_data)
dog3<-glm(colonization_prob_pred~(nghbr_connect)*structure, family=binomial(link = "logit"), data=Ext_col_data)
dog4<-glm(colonization_prob_pred~1, family=binomial(link = "logit"), data=Ext_col_data)

reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)


########################################################################################################################

#lme4/lmer
library(lmerTest)
library(lme4) 

prop_pa_rep_no_t<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate, connectivity)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))%>%
  pivot_longer(cols=prey.oc:pred.oc,names_to = "species", values_to="occupancy")

##############################################################################
#1A1-prey occupancy

prop_pa_rep_no_t_prey<-prop_pa_rep_no_t%>%filter(species=="pred.oc")


anova(lm(occupancy~structure+connectivity+replicate,data=prop_pa_rep_no_t_prey))

lmer0 <- lmer(occupancy~structure+connectivity+(1|replicate),data=prop_pa_rep_no_t_prey)
summary(lmer0)
anova(lmer0)
plot(lmer0)

ranova(lmer0)


stepTVbo <- step(lmer0)
stepTVbo
plot(stepTVbo)



