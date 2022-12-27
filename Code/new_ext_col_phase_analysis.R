##############################################################################################################################
#Col/Ext Phase Analysis Figure 6 and Table ?

############################################################################################################
#Entire Exp
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
  filter(structure !="control")%>%
  mutate(rep = paste(substr(structure,1,1),replicate,sep=""), 
         connectivity = as.integer(connectivity), 
         bottle.number = case_when(rep == "dA" ~ as.numeric(bottle.number),
                                   rep == "dB" ~ as.numeric(bottle.number) + 15,
                                   rep == "dC" ~ as.numeric(bottle.number) + 30,
                                   rep == "dD" ~ as.numeric(bottle.number) + 45,
                                   rep == "lA" ~ as.numeric(bottle.number) + 60,
                                   rep == "lB" ~ as.numeric(bottle.number) + 75,
                                   rep == "lC" ~ as.numeric(bottle.number) + 90,
                                   rep == "lD" ~ as.numeric(bottle.number) + 105))

####################################################################################
#Prey-Ext

y <- cbind(Ext_col_data_all$extinction_sum_prey, Ext_col_data_all$non_extinction_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("extinction",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_all_prey_ext<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

####################################################################################
#Pred-Ext

y <- cbind(Ext_col_data_all$extinction_sum_pred, Ext_col_data_all$non_extinction_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("extinction",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_all_pred_ext<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))
####################################################################################
#Prey-col

y <- cbind(Ext_col_data_all$colonization_sum_prey, Ext_col_data_all$non_colonization_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("colonization",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_all_prey_col<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))
####################################################################################
#Pred-col

y <- cbind(Ext_col_data_all$colonization_sum_pred, Ext_col_data_all$non_colonization_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("colonization",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_all_pred_col<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))
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
  filter(structure !="control")%>%
  mutate(rep = paste(substr(structure,1,1),replicate,sep=""), 
         connectivity = as.integer(connectivity), 
         bottle.number = case_when(rep == "dA" ~ as.numeric(bottle.number),
                                   rep == "dB" ~ as.numeric(bottle.number) + 15,
                                   rep == "dC" ~ as.numeric(bottle.number) + 30,
                                   rep == "dD" ~ as.numeric(bottle.number) + 45,
                                   rep == "lA" ~ as.numeric(bottle.number) + 60,
                                   rep == "lB" ~ as.numeric(bottle.number) + 75,
                                   rep == "lC" ~ as.numeric(bottle.number) + 90,
                                   rep == "lD" ~ as.numeric(bottle.number) + 105))

####################################################################################
#Prey-Ext

y <- cbind(Ext_col_data_1$extinction_sum_prey, Ext_col_data_1$non_extinction_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("exctinction",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_one_prey_ext<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


####################################################################################
#Pred-Ext

y <- cbind(Ext_col_data_1$extinction_sum_pred, Ext_col_data_1$non_extinction_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("extinction",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_one_pred_ext<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))
####################################################################################
#Prey-col

#y <- cbind(Ext_col_data_1$colonization_sum_prey, Ext_col_data_1$non_colonization_sum_prey)

#model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

#model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

#model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

#model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

#model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("colonization",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_one_prey_col<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))
####################################################################################
#Pred-col

y <- cbind(Ext_col_data_1$colonization_sum_pred, Ext_col_data_1$non_colonization_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("colonization",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_one_pred_col<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

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
  filter(structure !="control")%>%
  mutate(rep = paste(substr(structure,1,1),replicate,sep=""), 
         connectivity = as.integer(connectivity), 
         bottle.number = case_when(rep == "dA" ~ as.numeric(bottle.number),
                                   rep == "dB" ~ as.numeric(bottle.number) + 15,
                                   rep == "dC" ~ as.numeric(bottle.number) + 30,
                                   rep == "dD" ~ as.numeric(bottle.number) + 45,
                                   rep == "lA" ~ as.numeric(bottle.number) + 60,
                                   rep == "lB" ~ as.numeric(bottle.number) + 75,
                                   rep == "lC" ~ as.numeric(bottle.number) + 90,
                                   rep == "lD" ~ as.numeric(bottle.number) + 105))

####################################################################################
#Prey-Ext

y <- cbind(Ext_col_data_2$extinction_sum_prey, Ext_col_data_2$non_extinction_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("extinction",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_two_prey_ext<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

####################################################################################
#Pred-Ext

#y <- cbind(Ext_col_data_2$extinction_sum_pred, Ext_col_data_2$non_extinction_sum_pred)

#model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

#model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

#model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

#model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

#model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("extinction",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_two_pred_ext<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

####################################################################################
#Prey-col

#y <- cbind(Ext_col_data_2$colonization_sum_prey, Ext_col_data_2$non_colonization_sum_prey)

#model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

#model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

#model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

#model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

#model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("colonization",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_two_prey_col<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

####################################################################################
#Pred-col

y <- cbind(Ext_col_data_2$colonization_sum_pred, Ext_col_data_2$non_colonization_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("colonization",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_two_pred_col<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


####################################################################################################
#4) Phase 3

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
  filter(structure !="control")%>%mutate(rep = paste(substr(structure,1,1),replicate,sep=""), 
                                      connectivity = as.integer(connectivity), 
                                      bottle.number = case_when(rep == "dA" ~ as.numeric(bottle.number),
                                                                rep == "dB" ~ as.numeric(bottle.number) + 15,
                                                                rep == "dC" ~ as.numeric(bottle.number) + 30,
                                                                rep == "dD" ~ as.numeric(bottle.number) + 45,
                                                                rep == "lA" ~ as.numeric(bottle.number) + 60,
                                                                rep == "lB" ~ as.numeric(bottle.number) + 75,
                                                                rep == "lC" ~ as.numeric(bottle.number) + 90,
                                                                rep == "lD" ~ as.numeric(bottle.number) + 105))


####################################################################################
#Prey-Ext

y <- cbind(Ext_col_data_3$extinction_sum_prey, Ext_col_data_3$non_extinction_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("extinction",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_three_prey_ext<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

####################################################################################
#Pred-Ext

y <- cbind(Ext_col_data_3$extinction_sum_pred, Ext_col_data_3$non_extinction_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("extinction",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_three_pred_ext<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

####################################################################################
#Prey-col

y <- cbind(Ext_col_data_3$colonization_sum_prey, Ext_col_data_3$non_colonization_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("colonization",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_three_prey_col<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

####################################################################################
#Pred-col

#y <- cbind(Ext_col_data_3$colonization_sum_pred, Ext_col_data_3$non_colonization_sum_pred)

#model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

#model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

#model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

#model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

#model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_last_period_pred)
mod3_r2<-pseudoR2(model3_last_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_last_period_pred)
mod4_r2<-pseudoR2(model4_last_period_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_last_period_pred)
mod5_r2<-pseudoR2(model5_last_period_pred, nullform = ~1)

dog1<-"density ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

col_ext<-c(rep("colonization",5))
col_ext2<-as.data.frame(col_ext, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,col_ext2,pred.df,df2,aic2,r22))
merged_three_pred_col<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

####################################################################################################
ent<-merged_all_prey_col%>%full_join(merged_all_pred_col)%>%full_join(merged_all_prey_ext)%>%full_join(merged_all_pred_ext)%>%
  full_join(merged_one_pred_col)%>%full_join(merged_one_prey_ext)%>%full_join(merged_one_pred_ext)%>%
  full_join(merged_two_pred_col)%>%full_join(merged_two_prey_ext)%>%
  full_join(merged_three_prey_col)%>%full_join(merged_three_prey_ext)%>%full_join(merged_three_pred_ext)
  
  
write.csv(ent, "new_col_ext_model_3.csv")

