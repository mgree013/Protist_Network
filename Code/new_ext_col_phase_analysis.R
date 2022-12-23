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
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)


####################################################################################
#Pred-Ext

y <- cbind(Ext_col_data_all$extinction_sum_pred, Ext_col_data_all$non_extinction_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
####################################################################################
#Prey-col

y <- cbind(Ext_col_data_all$colonization_sum_prey, Ext_col_data_all$non_colonization_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
####################################################################################
#Pred-col

y <- cbind(Ext_col_data_all$colonization_sum_pred, Ext_col_data_all$non_colonization_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
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
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)


####################################################################################
#Pred-Ext

y <- cbind(Ext_col_data_1$extinction_sum_pred, Ext_col_data_1$non_extinction_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
####################################################################################
#Prey-col

y <- cbind(Ext_col_data_1$colonization_sum_prey, Ext_col_data_1$non_colonization_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
####################################################################################
#Pred-col

y <- cbind(Ext_col_data_1$colonization_sum_pred, Ext_col_data_1$non_colonization_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_1, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)

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
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)

####################################################################################
#Pred-Ext

y <- cbind(Ext_col_data_2$extinction_sum_pred, Ext_col_data_2$non_extinction_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
####################################################################################
#Prey-col

y <- cbind(Ext_col_data_2$colonization_sum_prey, Ext_col_data_2$non_colonization_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
####################################################################################
#Pred-col

y <- cbind(Ext_col_data_2$colonization_sum_pred, Ext_col_data_2$non_colonization_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_2, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)

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
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_all, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)

####################################################################################
#Pred-Ext

y <- cbind(Ext_col_data_3$extinction_sum_pred, Ext_col_data_3$non_extinction_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
####################################################################################
#Prey-col

y <- cbind(Ext_col_data_3$colonization_sum_prey, Ext_col_data_3$non_colonization_sum_prey)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)
####################################################################################
#Pred-col

y <- cbind(Ext_col_data_3$colonization_sum_pred, Ext_col_data_3$non_colonization_sum_pred)

model1_last_period_pred <- HLCor(y ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(y ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(y ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(y ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(y ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = Ext_col_data_3, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)

####################################################################################################


