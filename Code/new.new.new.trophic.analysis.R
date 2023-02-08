##############################################################################################################################
#Trophic Analysis Figure 7 and Table ?

############################################################################################################
#Entire Exp

bac_density<-env%>%dplyr::select(c(structure, bac.density,replicate,bottle.number,connectivity))%>%
  filter(structure=="dendritic" | structure=="lattice") %>% 
  mutate(rep = paste(substr(structure,1,1),replicate,sep=""), 
         connectivity = as.integer(connectivity), 
         bottle.number = case_when(rep == "dA" ~ as.numeric(bottle.number),
                                   rep == "dB" ~ as.numeric(bottle.number) + 15,
                                   rep == "dC" ~ as.numeric(bottle.number) + 30,
                                   rep == "dD" ~ as.numeric(bottle.number) + 45,
                                   rep == "lA" ~ as.numeric(bottle.number) + 60,
                                   rep == "lB" ~ as.numeric(bottle.number) + 75,
                                   rep == "lC" ~ as.numeric(bottle.number) + 90,
                                   rep == "lD" ~ as.numeric(bottle.number) + 105))%>%
  mutate(bac.density_log=log(bac.density))

env_density<-Data %>%
  filter(structure=="dendritic" | structure=="lattice") %>% 
  mutate(rep = paste(substr(structure,1,1),replicate,sep=""), 
         connectivity = as.integer(connectivity), 
         bottle.number = case_when(rep == "dA" ~ as.numeric(bottle.number),
                                   rep == "dB" ~ as.numeric(bottle.number) + 15,
                                   rep == "dC" ~ as.numeric(bottle.number) + 30,
                                   rep == "dD" ~ as.numeric(bottle.number) + 45,
                                   rep == "lA" ~ as.numeric(bottle.number) + 60,
                                   rep == "lB" ~ as.numeric(bottle.number) + 75,
                                   rep == "lC" ~ as.numeric(bottle.number) + 90,
                                   rep == "lD" ~ as.numeric(bottle.number) + 105)) %>% 
  group_by(structure, rep, bottle.number)%>%#%>%summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  #pivot_longer(cols=ln.prey:ln.pred,names_to = "species", values_to="density")%>%
  left_join(bac_density, by=c("bottle.number", "structure","rep"))


env_density_pred<-env_density

model1_last_period_pred <- HLCor(ln.pred ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(ln.pred ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)


mod1_aic<-extractAIC(model1_last_period_pred)
mod1_r2<-pseudoR2(model1_last_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_last_period_pred)
mod2_r2<-pseudoR2(model2_last_period_pred, nullform = ~1)


dog1<"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",2))
species2<-as.data.frame(species, ncol=1)


df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_all_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


#Prey All
env_density_prey<-env_density

model1_all_period_prey <- HLCor(ln.prey ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_all_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_period_prey, corr = FALSE)

mod1_aic<-extractAIC(model1_all_period_prey)
mod1_r2<-pseudoR2(model1_all_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_all_period_prey)
mod2_r2<-pseudoR2(model2_all_period_prey, nullform = ~1)


dog1<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_all_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


model1_all_period_prey <- HLCor(ln.prey ~ ln.pred  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_all_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_period_prey, corr = FALSE)

mod1_aic<-extractAIC(model1_all_period_prey)
mod1_r2<-pseudoR2(model1_all_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_all_period_prey)
mod2_r2<-pseudoR2(model2_all_period_prey, nullform = ~1)


dog1<-"density ~ ln.pred  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_all_prey_2<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

##########################################################################################################################################################################################
#Phase 1 Pred

env_density_pred<-env_density%>% filter(day > 0 & day < 76) 


model1_first_period_pred <- HLCor(ln.pred ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model2_first_period_pred <- HLCor(ln.pred ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_first_period_pred)
mod1_r2<-pseudoR2(model1_first_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_first_period_pred)
mod2_r2<-pseudoR2(model2_first_period_pred, nullform = ~1)


dog1<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_one_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

#Prey 1
env_density_prey<-env_density%>%  filter(day > 0 & day < 76) 

model1_all_period_prey  <- HLCor(ln.prey ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_all_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_period_prey, corr = FALSE)


mod1_aic<-extractAIC(model1_all_period_prey)
mod1_r2<-pseudoR2(model1_all_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_all_period_prey)
mod2_r2<-pseudoR2(model2_all_period_prey, nullform = ~1)


dog1<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_one_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


model1_all_period_prey  <- HLCor(ln.prey ~ ln.pred  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_all_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_period_prey, corr = FALSE)


mod1_aic<-extractAIC(model1_all_period_prey)
mod1_r2<-pseudoR2(model1_all_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_all_period_prey)
mod2_r2<-pseudoR2(model2_all_period_prey, nullform = ~1)


dog1<-"density ~ ln.pred  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_one_prey_2<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

##########################################################################################################################################################################################
#Phase 2

env_density_pred<-env_density%>%filter(day > 75 & day < 151)

model1_two_period_pred  <- HLCor(ln.pred ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model4_two_period_pred <- HLCor(ln.pred ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_two_period_pred)
mod1_r2<-pseudoR2(model1_two_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_two_period_pred)
mod2_r2<-pseudoR2(model2_two_period_pred, nullform = ~1)


dog1<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_two_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


#Prey two
env_density_prey<-env_density%>%
  filter(day > 75 & day < 151)

model1_two_period_prey <- HLCor(ln.prey ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_two_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_period_prey, corr = FALSE)

mod1_aic<-extractAIC(model1_two_period_prey)
mod1_r2<-pseudoR2(model1_two_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_two_period_prey)
mod2_r2<-pseudoR2(model2_two_period_prey, nullform = ~1)


dog1<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_two_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


model1_two_period_prey <- HLCor(ln.prey ~ ln.pred  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_two_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_period_prey, corr = FALSE)

mod1_aic<-extractAIC(model1_two_period_prey)
mod1_r2<-pseudoR2(model1_two_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_two_period_prey)
mod2_r2<-pseudoR2(model2_two_period_prey, nullform = ~1)


dog1<-"density ~ ln.pred  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_two_prey_2<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

##########################################################################################################################################################################################
#Phase 3

env_density_pred<-env_density%>%filter(day > 150)

model1_three_period_pred <- HLCor(ln.pred ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model2_three_period_pred <- HLCor(ln.pred ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_three_period_pred)
mod1_r2<-pseudoR2(model1_three_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_three_period_pred)
mod2_r2<-pseudoR2(model2_three_period_pred, nullform = ~1)

dog1<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_three_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))




#Prey All
env_density_prey<-env_density%>%
  filter(day > 150)

model1_three_period_prey  <- HLCor(ln.prey ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_three_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_period_prey, corr = FALSE)

mod1_aic<-extractAIC(model1_three_period_prey)
mod1_r2<-pseudoR2(model1_three_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_three_period_prey)
mod2_r2<-pseudoR2(model2_three_period_prey, nullform = ~1)


dog1<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_three_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))



model1_three_period_prey  <- HLCor(ln.prey ~ ln.pred  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_three_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_period_prey, corr = FALSE)

mod1_aic<-extractAIC(model1_three_period_prey)
mod1_r2<-pseudoR2(model1_three_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_three_period_prey)
mod2_r2<-pseudoR2(model2_three_period_prey, nullform = ~1)


dog1<-"density ~ ln.pred  + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",2))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",2))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_three_prey_2<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

#####
ent<-merged_all_pred%>%full_join(merged_all_prey)%>%full_join(merged_all_prey_2)%>%full_join(merged_one_pred)%>%full_join(merged_one_prey)%>%full_join(merged_one_prey_2)%>%
  full_join(merged_two_pred)%>%full_join(merged_two_prey)%>%full_join(merged_two_prey_2)%>%full_join(merged_three_pred)%>%full_join(merged_three_prey)%>%full_join(merged_three_prey_2)

write.csv(ent, "new_trophic_model_3.csv")


#######################################################################################################################################################################################################################################
#edits


#Phase 3

env_density_pred<-env_density%>%filter(day > 150)

model1_three_period_pred <- HLCor(ln.pred ~ bac.density_log+ ln.prey  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)
model2_three_period_pred <- HLCor(ln.pred ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))

model3_three_period_pred <- HLCor(ln.pred ~  ln.prey  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))

model4_three_period_pred <- HLCor(ln.pred ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_three_period_pred)
mod1_r2<-pseudoR2(model1_three_period_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_three_period_pred)
mod2_r2<-pseudoR2(model2_three_period_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_three_period_pred)
mod3_r2<-pseudoR2(model3_three_period_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_three_period_pred)
mod4_r2<-pseudoR2(model4_three_period_pred, nullform = ~1)

dog1<-"density ~ bac.density_log+ ln.prey   + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ ln.prey  + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",4))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",4))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_three_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

#PREY
env_density_prey<-env_density%>%
  filter(day > 150)

model1_three_period_prey  <- HLCor(ln.prey ~ bac.density_log  + ln.pred+ (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_period_prey, corr = FALSE)

model2_three_period_prey <- HLCor(ln.prey ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))

model3_three_period_prey <- HLCor(ln.prey ~  ln.pred  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))

model4_three_period_prey <- HLCor(ln.prey ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_three_period_prey)
mod1_r2<-pseudoR2(model1_three_period_prey, nullform = ~1)

mod2_aic<-extractAIC(model2_three_period_prey)
mod2_r2<-pseudoR2(model2_three_period_prey, nullform = ~1)

mod3_aic<-extractAIC(model3_three_period_prey)
mod3_r2<-pseudoR2(model3_three_period_prey, nullform = ~1)

mod4_aic<-extractAIC(model4_three_period_prey)
mod4_r2<-pseudoR2(model4_three_period_prey, nullform = ~1)

dog1<-"density ~ bac.density_log+ ln.pred   + (1|rep) + adjacency(1|bottle.number)"
dog2<-"density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number)"
dog3<-"density ~ ln.pred  + (1|rep) + adjacency(1|bottle.number)"
dog4<-"density ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",4))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",4))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_three_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


ent<-merged_three_pred%>%full_join(merged_three_prey)

write.csv(ent, "new_trophic_model.csv")
