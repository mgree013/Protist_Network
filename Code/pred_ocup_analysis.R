#Figure 6:
new_pa_datas <- slide(all_pa_datas, Var = "pred.oc", GroupVar = "bottle",
                      slideBy = -1)
newer_pa_datas <- slide(new_pa_datas, Var = "prey.oc", GroupVar = "bottle",
                        slideBy = -1)

all_pa_datazz<-all_pa_datas%>%
  group_by(structure,replicate)%>%
  summarise(prey.occupancy =mean(prey.oc), pred.occupancy= mean(pred.oc))

all_pa_dataz<-all_pa_datas%>%
  group_by(structure,replicate,nghbr_connect,bottle,bottle.number,connectivity)%>%
  summarise(prey.occupancy =mean(prey.oc), pred.occupancy= mean(pred.oc))

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

####################################################
#6a: Prey observed occupancy network level
dog<-list()
dog[[1]]<-betareg(pred.prey.oc~1,  data=Ext_col_data_network)
dog[[2]]<-betareg(pred.prey.oc~prey.occupancy, data=Ext_col_data_network)
dog[[3]]<-betareg(pred.prey.oc~prey.occupancy, data=Ext_col_data_network, link = "loglog")

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

####################################################
#Figure 6b: pred obs occupancy network level
dog<-list()
dog[[1]]<-betareg(pred.pred.oc~1,  data=Ext_col_data_network)
dog[[2]]<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_network)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

####################################################
#6c: Prey occuonacy bottle level
y<-glm(pred.prey.oc~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

dog<-list()
dog[[1]]<-betareg(pred.prey.oc~1,  data=Ext_col_data_glm)
dog[[2]]<-betareg(pred.prey.oc~prey.occupancy, data=Ext_col_data_glm)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

####################################################
#6d: Predator occupancy bottle level
dog<-list()
dog[[1]]<-betareg(pred.pred.oc~1,  data=Ext_col_data_glm)
dog[[2]]<-betareg(pred.pred.oc~pred.occupancy, data=Ext_col_data_glm)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

####################################################
#6e: prey occupancy bottle level from colonization prob

z <- cbind(Ext_col_data_glm$colonization_sum_prey, Ext_col_data_glm$non_colonization_sum_prey)
dog<-list()
dog[[1]]<-glm(z~1, family=binomial(link = "logit"),  data=Ext_col_data_glm)
dog[[2]]<-glm(z~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

####################################################
#6e: prey occupancy bottle level from extinction prob

z <- cbind(Ext_col_data_glm$extinction_sum_prey, Ext_col_data_glm$non_extinction_sum_prey)
dog<-list()
dog[[1]]<-glm(z~1, family=binomial(link = "logit"),  data=Ext_col_data_glm)
dog[[2]]<-glm(z~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

y<-glm(z~prey.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

####################################################
#6e: predator occupancy bottle level from colonization prob

z <- cbind(Ext_col_data_glm$colonization_sum_pred, Ext_col_data_glm$non_colonization_sum_pred)
y<-glm(z~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

dog<-list()
dog[[1]]<-glm(z~1, family=binomial(link = "logit"),  data=Ext_col_data_glm)
dog[[2]]<-glm(z~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0

####################################################
#6e: predator occupancy bottle level from extinction prob

z <- cbind(Ext_col_data_glm$extinction_sum_pred, Ext_col_data_glm$non_extinction_sum_pred)
y<-glm(z~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)
dog<-list()
dog[[1]]<-glm(z~1, family=binomial(link = "logit"),  data=Ext_col_data_glm)
dog[[2]]<-glm(z~pred.occupancy, family=binomial(link = "logit"), data=Ext_col_data_glm)

Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])
pseudoR0 <- ((y$null.deviance-y$deviance)/y$null.deviance)
pseudoR0
