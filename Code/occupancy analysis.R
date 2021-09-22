#Occupancy Analaysis
library(AICcmodavg)
########################################################################################################################
# Part 1)Occupany by network

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
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  mutate(net1=if_else(structure=="control","nonspatial","spatial"))


#1)All Phases
#1A) Prey
y <- cbind(prop_pa_rep_no_t$prey.occupany, prop_pa_rep_no_t$prey.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog[[2]]<-glm(y~net1, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog[[3]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3)
r22<-as.data.frame(r2, ncol=1)

dog1<-"~structure"
dog2<-"~net1"
dog3<-"~1"

predictor<-c(dog1,dog2,dog3)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(phase="all")%>%add_column(species="prey")
prey_all

########################################################################
#1B) Predator
y <- cbind(prop_pa_rep_no_t$pred.occupany, prop_pa_rep_no_t$pred.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog[[2]]<-glm(y~net1, family=binomial(link = "logit"), data=prop_pa_rep_no_t)
dog[[3]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3)
r22<-as.data.frame(r2, ncol=1)

dog1<-"~structure"
dog2<-"~net1"
dog3<-"~1"

predictor<-c(dog1,dog2,dog3)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(phase="all")%>%add_column(species="pred")
pred_all
##################################################################################################################
#2)Phase 1
prop_pa_rep_no_t_1<-all_pa_datas%>%
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
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  mutate(net1=if_else(structure=="control","nonspatial","spatial"))


#2A) Prey
y <- cbind(prop_pa_rep_no_t_1$prey.occupany, prop_pa_rep_no_t_1$prey.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_1)
dog[[2]]<-glm(y~net1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_1)
dog[[3]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_1)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3)
r22<-as.data.frame(r2, ncol=1)

dog1<-"~structure"
dog2<-"~net1"
dog3<-"~1"

predictor<-c(dog1,dog2,dog3)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(phase="1")%>%add_column(species="prey")
prey_1

########################################################################
#2B) Predator
y <- cbind(prop_pa_rep_no_t_1$pred.occupany, prop_pa_rep_no_t_1$pred.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_1)
dog[[2]]<-glm(y~net1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_1)
dog[[3]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_1)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3)
r22<-as.data.frame(r2, ncol=1)

dog1<-"~structure"
dog2<-"~net1"
dog3<-"~1"

predictor<-c(dog1,dog2,dog3)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(phase="1")%>%add_column(species="pred")
pred_1

########################################################################################
#3)Phase 2
prop_pa_rep_no_t_2<-all_pa_datas%>%
  #filter(day < 75)%>%
  filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  mutate(net1=if_else(structure=="control","nonspatial","spatial"))

#3A) Prey
y <- cbind(prop_pa_rep_no_t_2$prey.occupany, prop_pa_rep_no_t_2$prey.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_2)
dog[[2]]<-glm(y~net1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_2)
dog[[3]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_2)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3)
r22<-as.data.frame(r2, ncol=1)

dog1<-"~structure"
dog2<-"~net1"
dog3<-"~1"

predictor<-c(dog1,dog2,dog3)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(phase="2")%>%add_column(species="prey")
prey_2

########################################################################
#3B) Predator
y <- cbind(prop_pa_rep_no_t_2$pred.occupany, prop_pa_rep_no_t_2$pred.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_2)
dog[[2]]<-glm(y~net1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_2)
dog[[3]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_2)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3)
r22<-as.data.frame(r2, ncol=1)

dog1<-"~structure"
dog2<-"~net1"
dog3<-"~1"

predictor<-c(dog1,dog2,dog3)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(phase="2")%>%add_column(species="pred")
pred_2

########################################################################################
#4)Phase 3
prop_pa_rep_no_t_3<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  mutate(net1=if_else(structure=="control","nonspatial","spatial"))

#4A) Prey
y <- cbind(prop_pa_rep_no_t_3$prey.occupany, prop_pa_rep_no_t_3$prey.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_3)
dog[[2]]<-glm(y~net1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_3)
dog[[3]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_3)
summary(dog[[3]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3)
r22<-as.data.frame(r2, ncol=1)

dog1<-"~structure"
dog2<-"~net1"
dog3<-"~1"

predictor<-c(dog1,dog2,dog3)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(phase="3")%>%add_column(species="prey")
prey_3

########################################################################
#4B) Predator
y <- cbind(prop_pa_rep_no_t_3$pred.occupany, prop_pa_rep_no_t_3$pred.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_no_t_3)
dog[[2]]<-glm(y~net1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_3)
dog[[3]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_no_t_3)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3)
r22<-as.data.frame(r2, ncol=1)

dog1<-"~structure"
dog2<-"~net1"
dog3<-"~1"

predictor<-c(dog1,dog2,dog3)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(phase="3")%>%add_column(species="pred")
pred_3
############################################################
#Save network models
all<-rbind(prey_all,pred_all,
           prey_1,pred_1,
           prey_2,pred_2,
           prey_3,pred_3)

write.csv(all, "Data_analysis_2.csv")

####################################################################################################################################################################################
####################################################################################################################################################################################
#2)Connectivity Occupancy

#2A)ALl phases
prop_pa_rep_connect<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate, connectivity, bottle)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=(prey.occupany/n,pred.oc=(pred.occupany/n)+0.000001,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  mutate(con.0=if_else(connectivity==0,"yes","no"))


prop_pa_rep_connect$connectivity<-as.factor(prop_pa_rep_connect$connectivity)
prop_pa_rep_connect$con.0<-as.factor(prop_pa_rep_connect$con.0)

str(prop_pa_rep_connect)
########################################################################
#1B) Prey
y <- cbind(prop_pa_rep_connect$prey.occupany, prop_pa_rep_connect$prey.absence)

dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog[[2]]<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog[[3]]<-glm(y~con.0, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect)

summary(dog[[2]])

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22


dog1<-"~structure"
dog2<-"~connectivity"
dog3<-"~con.0"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(species="prey")%>%add_column(phase="all")
prey_all

########################################################################
#1B) Predator
y <- cbind(prop_pa_rep_connect$pred.occupany, prop_pa_rep_connect$pred.absence)


dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog[[2]]<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog[[3]]<-glm(y~con.0, family=binomial(link = "logit"), data=prop_pa_rep_connect)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect)

summary(dog[[2]])

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22


dog1<-"~structure"
dog2<-"~connectivity"
dog3<-"~con.0"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(species="pred")%>%add_column(phase="all")
pred_all

################################################################################################################################################
#2)Phase 1

prop_pa_rep_connect_1<-all_pa_datas%>%
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
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  mutate(con.0=if_else(connectivity==0,"yes","no"))

prop_pa_rep_connect_1$connectivity<-as.factor(prop_pa_rep_connect_1$connectivity)
prop_pa_rep_connect_1$con.0<-as.factor(prop_pa_rep_connect_1$con.0)

str(prop_pa_rep_connect_1)
########################################################################
#1B) Prey
y <- cbind(prop_pa_rep_connect_1$prey.occupany, prop_pa_rep_connect_1$prey.absence)


dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect_1)
dog[[2]]<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect_1)
dog[[3]]<-glm(y~con.0, family=binomial(link = "logit"), data=prop_pa_rep_connect_1)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect_1)

summary(dog[[2]])

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22


dog1<-"~structure"
dog2<-"~connectivity"
dog3<-"~con.0"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(species="prey")%>%add_column(phase="1")
prey_1

########################################################################
#1B) Predator
y <- cbind(prop_pa_rep_connect_1$pred.occupany, prop_pa_rep_connect_1$pred.absence)


dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect_1)
dog[[2]]<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect_1)
dog[[3]]<-glm(y~con.0, family=binomial(link = "logit"), data=prop_pa_rep_connect_1)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect_1)

summary(dog[[2]])

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22


dog1<-"~structure"
dog2<-"~connectivity"
dog3<-"~con.0"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(species="pred")%>%add_column(phase="1")
pred_1

################################################################################################################################################
#Phase 2

prop_pa_rep_connect_2<-all_pa_datas%>%
  #filter(day < 75)%>%
  filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate, connectivity, bottle)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  mutate(con.0=if_else(connectivity==0,"yes","no"))

prop_pa_rep_connect_2$connectivity<-as.factor(prop_pa_rep_connect_2$connectivity)
prop_pa_rep_connect_2$con.0<-as.factor(prop_pa_rep_connect_2$con.0)

str(prop_pa_rep_connect_2)
########################################################################
#1B) Prey
y <- cbind(prop_pa_rep_connect_2$prey.occupany, prop_pa_rep_connect_2$prey.absence)


dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect_2)
dog[[2]]<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect_2)
dog[[3]]<-glm(y~con.0, family=binomial(link = "logit"), data=prop_pa_rep_connect_2)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect_2)

summary(dog[[2]])

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22


dog1<-"~structure"
dog2<-"~connectivity"
dog3<-"~con.0"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(species="prey")%>%add_column(phase="2")
prey_2

########################################################################
#1B) Predator
y <- cbind(prop_pa_rep_connect_2$pred.occupany, prop_pa_rep_connect_2$pred.absence)


dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect_2)
dog[[2]]<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect_2)
dog[[3]]<-glm(y~con.0, family=binomial(link = "logit"), data=prop_pa_rep_connect_2)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect_2)

summary(dog[[2]])

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22


dog1<-"~structure"
dog2<-"~connectivity"
dog3<-"~con.0"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(species="pred")%>%add_column(phase="2")
pred_2

################################################################################################################################################
#Phase 3

prop_pa_rep_connect_3<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate, connectivity, bottle)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  mutate(con.0=if_else(connectivity==0,"yes","no"))

prop_pa_rep_connect_3$connectivity<-as.factor(prop_pa_rep_connect_3$connectivity)
prop_pa_rep_connect_3$con.0<-as.factor(prop_pa_rep_connect_3$con.0)

str(prop_pa_rep_connect3)
########################################################################
#1B) Prey
y <- cbind(prop_pa_rep_connect_3$prey.occupany, prop_pa_rep_connect_3$prey.absence)


dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect_3)
dog[[2]]<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect_3)
dog[[3]]<-glm(y~con.0, family=binomial(link = "logit"), data=prop_pa_rep_connect_3)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect_3)

summary(dog[[2]])

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22


dog1<-"~structure"
dog2<-"~connectivity"
dog3<-"~con.0"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(species="prey")%>%add_column(phase="3")
prey_3

########################################################################
#1B) Predator
y <- cbind(prop_pa_rep_connect_3$pred.occupany, prop_pa_rep_connect_3$pred.absence)


dog=list()
dog[[1]]<-glm(y~structure, family=binomial(link = "logit"), data=prop_pa_rep_connect_3)
dog[[2]]<-glm(y~connectivity, family=binomial(link = "logit"), data=prop_pa_rep_connect_3)
dog[[3]]<-glm(y~con.0, family=binomial(link = "logit"), data=prop_pa_rep_connect_3)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=prop_pa_rep_connect_3)

summary(dog[[2]])

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22


dog1<-"~structure"
dog2<-"~connectivity"
dog3<-"~con.0"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(species="pred")%>%add_column(phase="3")
pred_3

################################################################
#Save network models
all<-rbind(prey_all,pred_all,
           prey_1,pred_1,
           prey_2,pred_2,
           prey_3,pred_3)

write.csv(all, "Data_analysis_5.csv")
################################################################################################################################################

