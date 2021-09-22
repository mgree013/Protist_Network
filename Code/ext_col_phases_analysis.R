################################################################################################################################################
#Connectivity Effects on Colonization_ext Dynamcis
########################################################################################################################################################################################################
#1) Entire Exp
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

#Prey-Ext

y <- cbind(Ext_col_data_all$extinction_sum_prey, Ext_col_data_all$non_extinction_sum_prey)

Ext_col_data_all$structure<-as.factor(Ext_col_data_all$structure)
Ext_col_data_all$connectivity<-as.factor(Ext_col_data_all$connectivity)
Ext_col_data_all$replicate<-as.factor(Ext_col_data_all$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_all)

summary(dog[[4]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_ext_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_Ext_all")
prey_ext_all

####################################################################################
#predator-Ext
y <- cbind(Ext_col_data_all$extinction_sum_pred, Ext_col_data_all$non_extinction_sum_pred)

Ext_col_data_all$structure<-as.factor(Ext_col_data_all$structure)
Ext_col_data_all$connectivity<-as.factor(Ext_col_data_all$connectivity)
Ext_col_data_all$replicate<-as.factor(Ext_col_data_all$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_all)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_ext_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_Ext_all")
pred_ext_all
####################################################################################
#Pred-col
y <- cbind(Ext_col_data_all$colonization_sum_pred, Ext_col_data_all$non_colonization_sum_pred)

Ext_col_data_all$structure<-as.factor(Ext_col_data_all$structure)
Ext_col_data_all$connectivity<-as.factor(Ext_col_data_all$connectivity)
Ext_col_data_all$replicate<-as.factor(Ext_col_data_all$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_all)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_col_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_Col_all")
pred_col_all

####################################################################################
#Prey-Col

y <- cbind(Ext_col_data_all$colonization_sum_prey, Ext_col_data_all$non_colonization_sum_prey)

Ext_col_data_all$structure<-as.factor(Ext_col_data_all$structure)
Ext_col_data_all$connectivity<-as.factor(Ext_col_data_all$connectivity)
Ext_col_data_all$replicate<-as.factor(Ext_col_data_all$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_all)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_all)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_col_all<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_Col_all")
prey_col_all

########################################################################################################################################################################################################
#2)Phase 1
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

####################################################################################
#Prey-Ext

y <- cbind(Ext_col_data_1$extinction_sum_prey, Ext_col_data_1$non_extinction_sum_prey)

Ext_col_data_1$structure<-as.factor(Ext_col_data_1$structure)
Ext_col_data_1$connectivity<-as.factor(Ext_col_data_1$connectivity)
Ext_col_data_1$replicate<-as.factor(Ext_col_data_1$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_1)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_ext_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_Ext_1")
prey_ext_1

####################################################################################
#predator-Ext
y <- cbind(Ext_col_data_1$extinction_sum_pred, Ext_col_data_1$non_extinction_sum_pred)

Ext_col_data_1$structure<-as.factor(Ext_col_data_1$structure)
Ext_col_data_1$connectivity<-as.factor(Ext_col_data_1$connectivity)
Ext_col_data_1$replicate<-as.factor(Ext_col_data_1$replicate)

dog=list()
dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_1)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_ext_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_Ext_1")
pred_ext_1
####################################################################################
#Pred-col
y <- cbind(Ext_col_data_1$colonization_sum_pred, Ext_col_data_1$non_colonization_sum_pred)

Ext_col_data_1$structure<-as.factor(Ext_col_data_1$structure)
Ext_col_data_1$connectivity<-as.factor(Ext_col_data_1$connectivity)
Ext_col_data_1$replicate<-as.factor(Ext_col_data_1$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_1)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_col_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_Col_1")
pred_col_1

####################################################################################
#Prey-Col

y <- cbind(Ext_col_data_1$colonization_sum_prey, Ext_col_data_1$non_colonization_sum_prey)

Ext_col_data_1$structure<-as.factor(Ext_col_data_1$structure)
Ext_col_data_1$connectivity<-as.factor(Ext_col_data_1$connectivity)
Ext_col_data_1$replicate<-as.factor(Ext_col_data_1$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_1)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_1)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_col_1<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_Col_1")
prey_col_1


########################################################################################################################################################################################################
#3)Phase 2

#phase 2-Pred col
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

####################################################################################
#Prey-Ext

y <- cbind(Ext_col_data_2$extinction_sum_prey, Ext_col_data_2$non_extinction_sum_prey)

Ext_col_data_2$structure<-as.factor(Ext_col_data_2$structure)
Ext_col_data_2$connectivity<-as.factor(Ext_col_data_2$connectivity)
Ext_col_data_2$replicate<-as.factor(Ext_col_data_2$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_2)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_ext_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_Ext_2")
prey_ext_2

####################################################################################
#predator-Ext
y <- cbind(Ext_col_data_2$extinction_sum_pred, Ext_col_data_2$non_extinction_sum_pred)

Ext_col_data_2$structure<-as.factor(Ext_col_data_2$structure)
Ext_col_data_2$connectivity<-as.factor(Ext_col_data_2$connectivity)
Ext_col_data_2$replicate<-as.factor(Ext_col_data_2$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_2)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_ext_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_Ext_2")
pred_ext_2
####################################################################################
#Pred-col
y <- cbind(Ext_col_data_2$colonization_sum_pred, Ext_col_data_2$non_colonization_sum_pred)

Ext_col_data_2$structure<-as.factor(Ext_col_data_2$structure)
Ext_col_data_2$connectivity<-as.factor(Ext_col_data_2$connectivity)
Ext_col_data_2$replicate<-as.factor(Ext_col_data_2$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_2)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_col_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_Col_2")
pred_col_2

####################################################################################
#Prey-Col

y <- cbind(Ext_col_data_2$colonization_sum_prey, Ext_col_data_2$non_colonization_sum_prey)

Ext_col_data_2$structure<-as.factor(Ext_col_data_2$structure)
Ext_col_data_2$connectivity<-as.factor(Ext_col_data_2$connectivity)
Ext_col_data_2$replicate<-as.factor(Ext_col_data_2$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_2)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_2)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_col_2<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_Col_2")
prey_col_2
####################################################################################################
#4) Phase 3

#phase 3 prey ext
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

####################################################################################
#Prey-Ext

y <- cbind(Ext_col_data_3$extinction_sum_prey, Ext_col_data_3$non_extinction_sum_prey)

Ext_col_data_3$structure<-as.factor(Ext_col_data_3$structure)
Ext_col_data_3$connectivity<-as.factor(Ext_col_data_3$connectivity)
Ext_col_data_3$replicate<-as.factor(Ext_col_data_3$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_3)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_ext_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_Ext_3")
prey_ext_3

####################################################################################
#predator-Ext
y <- cbind(Ext_col_data_3$extinction_sum_pred, Ext_col_data_3$non_extinction_sum_pred)

Ext_col_data_3$structure<-as.factor(Ext_col_data_3$structure)
Ext_col_data_3$connectivity<-as.factor(Ext_col_data_3$connectivity)
Ext_col_data_3$replicate<-as.factor(Ext_col_data_3$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_3)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_ext_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_Ext_3")
pred_ext_3
####################################################################################
#Pred-col
y <- cbind(Ext_col_data_3$colonization_sum_pred, Ext_col_data_3$non_colonization_sum_pred)

Ext_col_data_3$structure<-as.factor(Ext_col_data_3$structure)
Ext_col_data_3$connectivity<-as.factor(Ext_col_data_3$connectivity)
Ext_col_data_3$replicate<-as.factor(Ext_col_data_3$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_3)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
pred_col_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Pred_Col_3")
pred_col_3

####################################################################################
#Prey-Col

y <- cbind(Ext_col_data_3$colonization_sum_prey, Ext_col_data_3$non_colonization_sum_prey)

Ext_col_data_3$structure<-as.factor(Ext_col_data_3$structure)
Ext_col_data_3$connectivity<-as.factor(Ext_col_data_3$connectivity)
Ext_col_data_3$replicate<-as.factor(Ext_col_data_3$replicate)

dog=list()
dog[[1]]<-glm(y~structure+connectivity, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[2]]<-glm(y~structure, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[3]]<-glm(y~connectivity, family=binomial(link = "logit"), data=Ext_col_data_3)
dog[[4]]<-glm(y~1, family=binomial(link = "logit"), data=Ext_col_data_3)

summary(dog[[1]])
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2

pseudoR1 <- ((dog[[1]]$null.deviance-dog[[1]]$deviance)/dog[[1]]$null.deviance)
pseudoR2 <- ((dog[[2]]$null.deviance-dog[[2]]$deviance)/dog[[2]]$null.deviance)
pseudoR3 <- ((dog[[3]]$null.deviance-dog[[3]]$deviance)/dog[[3]]$null.deviance)
pseudoR4 <- ((dog[[4]]$null.deviance-dog[[4]]$deviance)/dog[[4]]$null.deviance)


r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4)
r22<-as.data.frame(r2, ncol=1)
r22

dog1<-"~structure+connectivity"
dog2<-"~structure"
dog3<-"~connectivity"
dog4<-"~1"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)
reported.table2<-as.data.frame(reported.table2)
prey_col_3<-reported.table2%>%mutate(r22)%>%mutate(pred.df)%>%add_column(Species="Prey_Col_3")
prey_col_3

###############################################################################################################################################################################
all<-rbind(prey_ext_all,pred_ext_all,prey_col_all,pred_col_all,
           prey_ext_1,pred_ext_1,prey_col_1,pred_col_1,
           prey_ext_2,pred_ext_2,prey_col_2,pred_col_2,
           prey_ext_3,pred_ext_3,prey_col_3,pred_col_3)

write.csv(all, "Data_analysis_7.csv")
