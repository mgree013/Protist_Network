####################################################################################
#Density

env_density2<-Data%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  #pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

#prey density

dog=list()
dog[[1]]<-glm(prey~pred+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[2]]<-glm(prey~pred+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog[[3]]<-glm(prey~pred+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[4]]<-glm(prey~pred+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[5]]<-glm(prey~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[6]]<-glm(prey~pred+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog[[7]]<-glm(prey~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[8]]<-glm(prey~pred+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[9]]<-glm(prey~pred+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[10]]<-glm(prey~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[11]]<-glm(prey~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[12]]<-glm(prey~structure, family=gaussian(link = "identity"), data=env_density2)
dog[[13]]<-glm(prey~replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[14]]<-glm(prey~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog[[15]]<-glm(prey~pred,family=gaussian(link = "identity"), data=env_density2)
dog[[16]]<-glm(prey~1,family=gaussian(link = "identity"), data=env_density2)


Modnames <- paste("mod", 1:length(dog), sep = " ")
##generate AICc table
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)
pseudoR5 <- ((dog[[5]]$null.deviance-dog[[5]]$deviance)/dog[[5]]$null.deviance)
pseudoR6 <- ((dog[[6]]$null.deviance-dog[[6]]$deviance)/dog[[6]]$null.deviance)
pseudoR7 <- ((dog[[7]]$null.deviance-dog[[7]]$deviance)/dog[[7]]$null.deviance)
pseudoR8 <- ((dog[[8]]$null.deviance-dog[[8]]$deviance)/dog[[8]]$null.deviance)
pseudoR9 <- ((dog[[9]]$null.deviance-dog[[9]]$deviance)/dog[[9]]$null.deviance)
pseudoR10 <- ((dog[[10]]$null.deviance-dog[[10]]$deviance)/dog[[10]]$null.deviance)
pseudoR11 <- ((dog[[11]]$null.deviance-dog[[11]]$deviance)/dog[[11]]$null.deviance)
pseudoR12 <- ((dog[[12]]$null.deviance-dog[[12]]$deviance)/dog[[12]]$null.deviance)
pseudoR13 <- ((dog[[13]]$null.deviance-dog[[13]]$deviance)/dog[[13]]$null.deviance)
pseudoR14 <- ((dog[[14]]$null.deviance-dog[[14]]$deviance)/dog[[14]]$null.deviance)
pseudoR15 <- ((dog[[15]]$null.deviance-dog[[15]]$deviance)/dog[[15]]$null.deviance)
pseudoR16 <- ((dog[[16]]$null.deviance-dog[[16]]$deviance)/dog[[16]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16)
r22<-as.data.frame(r2, ncol=1)
r22

dog0<-"~prey+bac.density_log+replicate+structure"
dog1<-"~prey+bac.density_log+replicate"
dog2<-"~prey+bac.density_log+structure"
dog3<-"~prey+replicate+structure"
dog4<-"~bac.density_log+replicate+structure"
dog5<-"~prey+bac.density_log"
dog6<-"~replicate+structure"
dog7<-"~prey+replicate"
dog8<-"~prey+structure"
dog9<-"~bac.density_log+replicate"
dog10<-"~bac.density_log+structure"
dog11<-"~structure"
dog12<-"~replicate"
dog13<-"~bac.density_log"
dog14<-"~prey"
dog15<-"~1"

predictor<-c(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_all")
prey_all

##################################################
#preadtor density
dog=list()
dog[[1]]<-glm(pred~prey+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[2]]<-glm(pred~prey+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog[[3]]<-glm(pred~prey+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[4]]<-glm(pred~prey+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[5]]<-glm(pred~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[6]]<-glm(pred~prey+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog[[7]]<-glm(pred~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[8]]<-glm(pred~prey+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[9]]<-glm(pred~prey+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[10]]<-glm(pred~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[11]]<-glm(pred~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[12]]<-glm(pred~structure, family=gaussian(link = "identity"), data=env_density2)
dog[[13]]<-glm(pred~replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[14]]<-glm(pred~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog[[15]]<-glm(pred~prey,family=gaussian(link = "identity"), data=env_density2)
dog[[16]]<-glm(pred~1,family=gaussian(link = "identity"), data=env_density2)


Modnames <- paste("mod", 1:length(dog), sep = " ")
##generate AICc table
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)
pseudoR5 <- ((dog[[5]]$null.deviance-dog[[5]]$deviance)/dog[[5]]$null.deviance)
pseudoR6 <- ((dog[[6]]$null.deviance-dog[[6]]$deviance)/dog[[6]]$null.deviance)
pseudoR7 <- ((dog[[7]]$null.deviance-dog[[7]]$deviance)/dog[[7]]$null.deviance)
pseudoR8 <- ((dog[[8]]$null.deviance-dog[[8]]$deviance)/dog[[8]]$null.deviance)
pseudoR9 <- ((dog[[9]]$null.deviance-dog[[9]]$deviance)/dog[[9]]$null.deviance)
pseudoR10 <- ((dog[[10]]$null.deviance-dog[[10]]$deviance)/dog[[10]]$null.deviance)
pseudoR11 <- ((dog[[11]]$null.deviance-dog[[11]]$deviance)/dog[[11]]$null.deviance)
pseudoR12 <- ((dog[[12]]$null.deviance-dog[[12]]$deviance)/dog[[12]]$null.deviance)
pseudoR13 <- ((dog[[13]]$null.deviance-dog[[13]]$deviance)/dog[[13]]$null.deviance)
pseudoR14 <- ((dog[[14]]$null.deviance-dog[[14]]$deviance)/dog[[14]]$null.deviance)
pseudoR15 <- ((dog[[15]]$null.deviance-dog[[15]]$deviance)/dog[[15]]$null.deviance)
pseudoR16 <- ((dog[[16]]$null.deviance-dog[[16]]$deviance)/dog[[16]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16)
r22<-as.data.frame(r2, ncol=1)
r22

dog0<-"~prey+bac.density_log+replicate+structure"
dog1<-"~prey+bac.density_log+replicate"
dog2<-"~prey+bac.density_log+structure"
dog3<-"~prey+replicate+structure"
dog4<-"~bac.density_log+replicate+structure"
dog5<-"~prey+bac.density_log"
dog6<-"~replicate+structure"
dog7<-"~prey+replicate"
dog8<-"~prey+structure"
dog9<-"~bac.density_log+replicate"
dog10<-"~bac.density_log+structure"
dog11<-"~structure"
dog12<-"~replicate"
dog13<-"~bac.density_log"
dog14<-"~prey"
dog15<-"~1"

predictor<-c(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_all")
pred_all


###############################################################################
#Density by Phase

#1)Phase One
env_density2<-Data%>%
  filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey+1), pred= mean(ln.pred+1))%>%
  #pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

##################################################
#prey density

dog=list()
dog[[1]]<-glm(prey~pred+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[2]]<-glm(prey~pred+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog[[3]]<-glm(prey~pred+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[4]]<-glm(prey~pred+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[5]]<-glm(prey~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[6]]<-glm(prey~pred+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog[[7]]<-glm(prey~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[8]]<-glm(prey~pred+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[9]]<-glm(prey~pred+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[10]]<-glm(prey~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[11]]<-glm(prey~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[12]]<-glm(prey~structure, family=gaussian(link = "identity"), data=env_density2)
dog[[13]]<-glm(prey~replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[14]]<-glm(prey~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog[[15]]<-glm(prey~pred,family=gaussian(link = "identity"), data=env_density2)
dog[[16]]<-glm(prey~1,family=gaussian(link = "identity"), data=env_density2)


Modnames <- paste("mod", 1:length(dog), sep = " ")
##generate AICc table
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)
pseudoR5 <- ((dog[[5]]$null.deviance-dog[[5]]$deviance)/dog[[5]]$null.deviance)
pseudoR6 <- ((dog[[6]]$null.deviance-dog[[6]]$deviance)/dog[[6]]$null.deviance)
pseudoR7 <- ((dog[[7]]$null.deviance-dog[[7]]$deviance)/dog[[7]]$null.deviance)
pseudoR8 <- ((dog[[8]]$null.deviance-dog[[8]]$deviance)/dog[[8]]$null.deviance)
pseudoR9 <- ((dog[[9]]$null.deviance-dog[[9]]$deviance)/dog[[9]]$null.deviance)
pseudoR10 <- ((dog[[10]]$null.deviance-dog[[10]]$deviance)/dog[[10]]$null.deviance)
pseudoR11 <- ((dog[[11]]$null.deviance-dog[[11]]$deviance)/dog[[11]]$null.deviance)
pseudoR12 <- ((dog[[12]]$null.deviance-dog[[12]]$deviance)/dog[[12]]$null.deviance)
pseudoR13 <- ((dog[[13]]$null.deviance-dog[[13]]$deviance)/dog[[13]]$null.deviance)
pseudoR14 <- ((dog[[14]]$null.deviance-dog[[14]]$deviance)/dog[[14]]$null.deviance)
pseudoR15 <- ((dog[[15]]$null.deviance-dog[[15]]$deviance)/dog[[15]]$null.deviance)
pseudoR16 <- ((dog[[16]]$null.deviance-dog[[16]]$deviance)/dog[[16]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16)
r22<-as.data.frame(r2, ncol=1)
r22

dog0<-"~prey+bac.density_log+replicate+structure"
dog1<-"~prey+bac.density_log+replicate"
dog2<-"~prey+bac.density_log+structure"
dog3<-"~prey+replicate+structure"
dog4<-"~bac.density_log+replicate+structure"
dog5<-"~prey+bac.density_log"
dog6<-"~replicate+structure"
dog7<-"~prey+replicate"
dog8<-"~prey+structure"
dog9<-"~bac.density_log+replicate"
dog10<-"~bac.density_log+structure"
dog11<-"~structure"
dog12<-"~replicate"
dog13<-"~bac.density_log"
dog14<-"~prey"
dog15<-"~1"

predictor<-c(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_1")
prey_1

##################################################
#preadtor density
dog=list()
dog[[1]]<-glm(pred~prey+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[2]]<-glm(pred~prey+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog[[3]]<-glm(pred~prey+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[4]]<-glm(pred~prey+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[5]]<-glm(pred~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[6]]<-glm(pred~prey+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog[[7]]<-glm(pred~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[8]]<-glm(pred~prey+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[9]]<-glm(pred~prey+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[10]]<-glm(pred~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[11]]<-glm(pred~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[12]]<-glm(pred~structure, family=gaussian(link = "identity"), data=env_density2)
dog[[13]]<-glm(pred~replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[14]]<-glm(pred~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog[[15]]<-glm(pred~prey,family=gaussian(link = "identity"), data=env_density2)
dog[[16]]<-glm(pred~1,family=gaussian(link = "identity"), data=env_density2)


Modnames <- paste("mod", 1:length(dog), sep = " ")
##generate AICc table
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)
pseudoR5 <- ((dog[[5]]$null.deviance-dog[[5]]$deviance)/dog[[5]]$null.deviance)
pseudoR6 <- ((dog[[6]]$null.deviance-dog[[6]]$deviance)/dog[[6]]$null.deviance)
pseudoR7 <- ((dog[[7]]$null.deviance-dog[[7]]$deviance)/dog[[7]]$null.deviance)
pseudoR8 <- ((dog[[8]]$null.deviance-dog[[8]]$deviance)/dog[[8]]$null.deviance)
pseudoR9 <- ((dog[[9]]$null.deviance-dog[[9]]$deviance)/dog[[9]]$null.deviance)
pseudoR10 <- ((dog[[10]]$null.deviance-dog[[10]]$deviance)/dog[[10]]$null.deviance)
pseudoR11 <- ((dog[[11]]$null.deviance-dog[[11]]$deviance)/dog[[11]]$null.deviance)
pseudoR12 <- ((dog[[12]]$null.deviance-dog[[12]]$deviance)/dog[[12]]$null.deviance)
pseudoR13 <- ((dog[[13]]$null.deviance-dog[[13]]$deviance)/dog[[13]]$null.deviance)
pseudoR14 <- ((dog[[14]]$null.deviance-dog[[14]]$deviance)/dog[[14]]$null.deviance)
pseudoR15 <- ((dog[[15]]$null.deviance-dog[[15]]$deviance)/dog[[15]]$null.deviance)
pseudoR16 <- ((dog[[16]]$null.deviance-dog[[16]]$deviance)/dog[[16]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16)
r22<-as.data.frame(r2, ncol=1)
r22


dog0<-"~prey+bac.density_log+replicate+structure"
dog1<-"~prey+bac.density_log+replicate"
dog2<-"~prey+bac.density_log+structure"
dog3<-"~prey+replicate+structure"
dog4<-"~bac.density_log+replicate+structure"
dog5<-"~prey+bac.density_log"
dog6<-"~replicate+structure"
dog7<-"~prey+replicate"
dog8<-"~prey+structure"
dog9<-"~bac.density_log+replicate"
dog10<-"~bac.density_log+structure"
dog11<-"~structure"
dog12<-"~replicate"
dog13<-"~bac.density_log"
dog14<-"~prey"
dog15<-"~1"

predictor<-c(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_1")
pred_1
####################################################################################
#2) Phase 2

env_density2<-Data%>%
  #filter(day < 75)%>%
  filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  #pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

##################################################
#prey density

dog=list()
dog[[1]]<-glm(prey~pred+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[2]]<-glm(prey~pred+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog[[3]]<-glm(prey~pred+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[4]]<-glm(prey~pred+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[5]]<-glm(prey~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[6]]<-glm(prey~pred+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog[[7]]<-glm(prey~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[8]]<-glm(prey~pred+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[9]]<-glm(prey~pred+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[10]]<-glm(prey~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[11]]<-glm(prey~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[12]]<-glm(prey~structure, family=gaussian(link = "identity"), data=env_density2)
dog[[13]]<-glm(prey~replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[14]]<-glm(prey~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog[[15]]<-glm(prey~pred,family=gaussian(link = "identity"), data=env_density2)
dog[[16]]<-glm(prey~1,family=gaussian(link = "identity"), data=env_density2)


Modnames <- paste("mod", 1:length(dog), sep = " ")
##generate AICc table
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)
pseudoR5 <- ((dog[[5]]$null.deviance-dog[[5]]$deviance)/dog[[5]]$null.deviance)
pseudoR6 <- ((dog[[6]]$null.deviance-dog[[6]]$deviance)/dog[[6]]$null.deviance)
pseudoR7 <- ((dog[[7]]$null.deviance-dog[[7]]$deviance)/dog[[7]]$null.deviance)
pseudoR8 <- ((dog[[8]]$null.deviance-dog[[8]]$deviance)/dog[[8]]$null.deviance)
pseudoR9 <- ((dog[[9]]$null.deviance-dog[[9]]$deviance)/dog[[9]]$null.deviance)
pseudoR10 <- ((dog[[10]]$null.deviance-dog[[10]]$deviance)/dog[[10]]$null.deviance)
pseudoR11 <- ((dog[[11]]$null.deviance-dog[[11]]$deviance)/dog[[11]]$null.deviance)
pseudoR12 <- ((dog[[12]]$null.deviance-dog[[12]]$deviance)/dog[[12]]$null.deviance)
pseudoR13 <- ((dog[[13]]$null.deviance-dog[[13]]$deviance)/dog[[13]]$null.deviance)
pseudoR14 <- ((dog[[14]]$null.deviance-dog[[14]]$deviance)/dog[[14]]$null.deviance)
pseudoR15 <- ((dog[[15]]$null.deviance-dog[[15]]$deviance)/dog[[15]]$null.deviance)
pseudoR16 <- ((dog[[16]]$null.deviance-dog[[16]]$deviance)/dog[[16]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16)
r22<-as.data.frame(r2, ncol=1)
r22

dog0<-"~prey+bac.density_log+replicate+structure"
dog1<-"~prey+bac.density_log+replicate"
dog2<-"~prey+bac.density_log+structure"
dog3<-"~prey+replicate+structure"
dog4<-"~bac.density_log+replicate+structure"
dog5<-"~prey+bac.density_log"
dog6<-"~replicate+structure"
dog7<-"~prey+replicate"
dog8<-"~prey+structure"
dog9<-"~bac.density_log+replicate"
dog10<-"~bac.density_log+structure"
dog11<-"~structure"
dog12<-"~replicate"
dog13<-"~bac.density_log"
dog14<-"~prey"
dog15<-"~1"

predictor<-c(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_2")
prey_2

##################################################
#preadtor density
dog=list()
dog[[1]]<-glm(pred~prey+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[2]]<-glm(pred~prey+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog[[3]]<-glm(pred~prey+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[4]]<-glm(pred~prey+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[5]]<-glm(pred~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[6]]<-glm(pred~prey+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog[[7]]<-glm(pred~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[8]]<-glm(pred~prey+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[9]]<-glm(pred~prey+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[10]]<-glm(pred~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[11]]<-glm(pred~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[12]]<-glm(pred~structure, family=gaussian(link = "identity"), data=env_density2)
dog[[13]]<-glm(pred~replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[14]]<-glm(pred~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog[[15]]<-glm(pred~prey,family=gaussian(link = "identity"), data=env_density2)
dog[[16]]<-glm(pred~1,family=gaussian(link = "identity"), data=env_density2)


Modnames <- paste("mod", 1:length(dog), sep = " ")
##generate AICc table
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)
pseudoR5 <- ((dog[[5]]$null.deviance-dog[[5]]$deviance)/dog[[5]]$null.deviance)
pseudoR6 <- ((dog[[6]]$null.deviance-dog[[6]]$deviance)/dog[[6]]$null.deviance)
pseudoR7 <- ((dog[[7]]$null.deviance-dog[[7]]$deviance)/dog[[7]]$null.deviance)
pseudoR8 <- ((dog[[8]]$null.deviance-dog[[8]]$deviance)/dog[[8]]$null.deviance)
pseudoR9 <- ((dog[[9]]$null.deviance-dog[[9]]$deviance)/dog[[9]]$null.deviance)
pseudoR10 <- ((dog[[10]]$null.deviance-dog[[10]]$deviance)/dog[[10]]$null.deviance)
pseudoR11 <- ((dog[[11]]$null.deviance-dog[[11]]$deviance)/dog[[11]]$null.deviance)
pseudoR12 <- ((dog[[12]]$null.deviance-dog[[12]]$deviance)/dog[[12]]$null.deviance)
pseudoR13 <- ((dog[[13]]$null.deviance-dog[[13]]$deviance)/dog[[13]]$null.deviance)
pseudoR14 <- ((dog[[14]]$null.deviance-dog[[14]]$deviance)/dog[[14]]$null.deviance)
pseudoR15 <- ((dog[[15]]$null.deviance-dog[[15]]$deviance)/dog[[15]]$null.deviance)
pseudoR16 <- ((dog[[16]]$null.deviance-dog[[16]]$deviance)/dog[[16]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16)
r22<-as.data.frame(r2, ncol=1)
r22


dog0<-"~prey+bac.density_log+replicate+structure"
dog1<-"~prey+bac.density_log+replicate"
dog2<-"~prey+bac.density_log+structure"
dog3<-"~prey+replicate+structure"
dog4<-"~bac.density_log+replicate+structure"
dog5<-"~prey+bac.density_log"
dog6<-"~replicate+structure"
dog7<-"~prey+replicate"
dog8<-"~prey+structure"
dog9<-"~bac.density_log+replicate"
dog10<-"~bac.density_log+structure"
dog11<-"~structure"
dog12<-"~replicate"
dog13<-"~bac.density_log"
dog14<-"~prey"
dog15<-"~1"

predictor<-c(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_2")
pred_2

####################################################################################
#3)Phase 3
env_density2<-Data%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  #pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

##################################################
#prey density

dog=list()
dog[[1]]<-glm(prey~pred+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[2]]<-glm(prey~pred+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog[[3]]<-glm(prey~pred+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[4]]<-glm(prey~pred+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[5]]<-glm(prey~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[6]]<-glm(prey~pred+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog[[7]]<-glm(prey~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[8]]<-glm(prey~pred+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[9]]<-glm(prey~pred+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[10]]<-glm(prey~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[11]]<-glm(prey~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[12]]<-glm(prey~structure, family=gaussian(link = "identity"), data=env_density2)
dog[[13]]<-glm(prey~replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[14]]<-glm(prey~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog[[15]]<-glm(prey~pred,family=gaussian(link = "identity"), data=env_density2)
dog[[16]]<-glm(prey~1,family=gaussian(link = "identity"), data=env_density2)


Modnames <- paste("mod", 1:length(dog), sep = " ")
##generate AICc table
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)
pseudoR5 <- ((dog[[5]]$null.deviance-dog[[5]]$deviance)/dog[[5]]$null.deviance)
pseudoR6 <- ((dog[[6]]$null.deviance-dog[[6]]$deviance)/dog[[6]]$null.deviance)
pseudoR7 <- ((dog[[7]]$null.deviance-dog[[7]]$deviance)/dog[[7]]$null.deviance)
pseudoR8 <- ((dog[[8]]$null.deviance-dog[[8]]$deviance)/dog[[8]]$null.deviance)
pseudoR9 <- ((dog[[9]]$null.deviance-dog[[9]]$deviance)/dog[[9]]$null.deviance)
pseudoR10 <- ((dog[[10]]$null.deviance-dog[[10]]$deviance)/dog[[10]]$null.deviance)
pseudoR11 <- ((dog[[11]]$null.deviance-dog[[11]]$deviance)/dog[[11]]$null.deviance)
pseudoR12 <- ((dog[[12]]$null.deviance-dog[[12]]$deviance)/dog[[12]]$null.deviance)
pseudoR13 <- ((dog[[13]]$null.deviance-dog[[13]]$deviance)/dog[[13]]$null.deviance)
pseudoR14 <- ((dog[[14]]$null.deviance-dog[[14]]$deviance)/dog[[14]]$null.deviance)
pseudoR15 <- ((dog[[15]]$null.deviance-dog[[15]]$deviance)/dog[[15]]$null.deviance)
pseudoR16 <- ((dog[[16]]$null.deviance-dog[[16]]$deviance)/dog[[16]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16)
r22<-as.data.frame(r2, ncol=1)
r22

dog0<-"~prey+bac.density_log+replicate+structure"
dog1<-"~prey+bac.density_log+replicate"
dog2<-"~prey+bac.density_log+structure"
dog3<-"~prey+replicate+structure"
dog4<-"~bac.density_log+replicate+structure"
dog5<-"~prey+bac.density_log"
dog6<-"~replicate+structure"
dog7<-"~prey+replicate"
dog8<-"~prey+structure"
dog9<-"~bac.density_log+replicate"
dog10<-"~bac.density_log+structure"
dog11<-"~structure"
dog12<-"~replicate"
dog13<-"~bac.density_log"
dog14<-"~prey"
dog15<-"~1"

predictor<-c(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_3")
prey_3

##################################################
#preadtor density
dog=list()
dog[[1]]<-glm(pred~prey+bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[2]]<-glm(pred~prey+bac.density_log+replicate,family=gaussian(link = "identity"), data=env_density2)
dog[[3]]<-glm(pred~prey+bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[4]]<-glm(pred~prey+replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[5]]<-glm(pred~bac.density_log+replicate+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[6]]<-glm(pred~prey+bac.density_log, family=gaussian(link = "identity"), data=env_density2)
dog[[7]]<-glm(pred~replicate+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[8]]<-glm(pred~prey+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[9]]<-glm(pred~prey+structure, family=gaussian(link = "identity"), data=env_density2)
dog[[10]]<-glm(pred~bac.density_log+replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[11]]<-glm(pred~bac.density_log+structure, family=gaussian(link = "identity"), data=env_density2)

dog[[12]]<-glm(pred~structure, family=gaussian(link = "identity"), data=env_density2)
dog[[13]]<-glm(pred~replicate, family=gaussian(link = "identity"), data=env_density2)
dog[[14]]<-glm(pred~bac.density_log,family=gaussian(link = "identity"), data=env_density2)
dog[[15]]<-glm(pred~prey,family=gaussian(link = "identity"), data=env_density2)
dog[[16]]<-glm(pred~1,family=gaussian(link = "identity"), data=env_density2)


Modnames <- paste("mod", 1:length(dog), sep = " ")
##generate AICc table
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)
pseudoR5 <- ((dog[[5]]$null.deviance-dog[[5]]$deviance)/dog[[5]]$null.deviance)
pseudoR6 <- ((dog[[6]]$null.deviance-dog[[6]]$deviance)/dog[[6]]$null.deviance)
pseudoR7 <- ((dog[[7]]$null.deviance-dog[[7]]$deviance)/dog[[7]]$null.deviance)
pseudoR8 <- ((dog[[8]]$null.deviance-dog[[8]]$deviance)/dog[[8]]$null.deviance)
pseudoR9 <- ((dog[[9]]$null.deviance-dog[[9]]$deviance)/dog[[9]]$null.deviance)
pseudoR10 <- ((dog[[10]]$null.deviance-dog[[10]]$deviance)/dog[[10]]$null.deviance)
pseudoR11 <- ((dog[[11]]$null.deviance-dog[[11]]$deviance)/dog[[11]]$null.deviance)
pseudoR12 <- ((dog[[12]]$null.deviance-dog[[12]]$deviance)/dog[[12]]$null.deviance)
pseudoR13 <- ((dog[[13]]$null.deviance-dog[[13]]$deviance)/dog[[13]]$null.deviance)
pseudoR14 <- ((dog[[14]]$null.deviance-dog[[14]]$deviance)/dog[[14]]$null.deviance)
pseudoR15 <- ((dog[[15]]$null.deviance-dog[[15]]$deviance)/dog[[15]]$null.deviance)
pseudoR16 <- ((dog[[16]]$null.deviance-dog[[16]]$deviance)/dog[[16]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16)
r22<-as.data.frame(r2, ncol=1)
r22


dog0<-"~prey+bac.density_log+replicate+structure"
dog1<-"~prey+bac.density_log+replicate"
dog2<-"~prey+bac.density_log+structure"
dog3<-"~prey+replicate+structure"
dog4<-"~bac.density_log+replicate+structure"
dog5<-"~prey+bac.density_log"
dog6<-"~replicate+structure"
dog7<-"~prey+replicate"
dog8<-"~prey+structure"
dog9<-"~bac.density_log+replicate"
dog10<-"~bac.density_log+structure"
dog11<-"~structure"
dog12<-"~replicate"
dog13<-"~bac.density_log"
dog14<-"~prey"
dog15<-"~1"

predictor<-c(dog0,dog1,dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_3")
pred_3

####################################################################################################################################################################################################
all<-rbind(prey_all,pred_all,
           prey_1,pred_1,
           prey_2,pred_2,
           prey_3,pred_3)

write.csv(all, "Data_analysis_1.csv")
