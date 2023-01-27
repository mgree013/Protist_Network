data <- all_pa_datas
adj_matrix <- read.csv("Data/Adj_matrix_big.csv", header=FALSE)

spaMM.options(separation_max = 100)

#Full Experimental Period Predators
all_pred <- data %>%
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, pred.oc,prey.oc) %>%
  group_by(structure, rep, connectivity)

##### Models
model1_all_pred <- HLCor(pred.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model1_all_pred, corr = FALSE)

model2_all_pred <- HLCor(pred.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_pred, corr = FALSE)

model3_all_pred <- HLCor(pred.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model3_all_pred, corr = FALSE)

model4_all_pred <- HLCor(pred.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_pred, corr = FALSE)

model5_all_pred <- HLCor(pred.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model5_all_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_all_pred)
mod1_r2<-pseudoR2(model1_all_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_all_pred)
mod2_r2<-pseudoR2(model2_all_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_all_pred)
mod3_r2<-pseudoR2(model3_all_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_all_pred)
mod4_r2<-pseudoR2(model4_all_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_all_pred)
mod5_r2<-pseudoR2(model5_all_pred, nullform = ~1)

dog1<-"occupancy ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"occupancy ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"occupancy ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"occupancy ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"occupancy ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_all_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


#All Prey
model1_all_pred <- HLCor(prey.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model1_all_pred, corr = FALSE)

model2_all_pred <- HLCor(prey.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_all_pred, corr = FALSE)

model3_all_pred <- HLCor(prey.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model3_all_pred, corr = FALSE)

model4_all_pred <- HLCor(prey.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_all_pred, corr = FALSE)

model5_all_pred <- HLCor(prey.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model5_all_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_all_pred)
mod1_r2<-pseudoR2(model1_all_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_all_pred)
mod2_r2<-pseudoR2(model2_all_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_all_pred)
mod3_r2<-pseudoR2(model3_all_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_all_pred)
mod4_r2<-pseudoR2(model4_all_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_all_pred)
mod5_r2<-pseudoR2(model5_all_pred, nullform = ~1)

dog1<-"occupancy ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"occupancy ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"occupancy ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"occupancy ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"occupancy ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Entire Exp",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_all_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

########################################################################################################################################################################

#Early Experimental Period Predators
early_period_pred <- data %>%
  filter(day > 0 & day < 76) %>%
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, pred.oc,prey.oc) %>%
  group_by(structure, rep, connectivity)


##### Models
model1_all_pred <- HLCor(pred.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model1_early_period_pred, corr = FALSE)

model2_all_pred <- HLCor(pred.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_early_period_pred, corr = FALSE)

model3_all_pred <- HLCor(pred.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model3_early_period_pred, corr = FALSE)

model4_all_pred <- HLCor(pred.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_early_period_pred, corr = FALSE)

model5_all_pred <- HLCor(pred.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model5_early_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_all_pred)
mod1_r2<-pseudoR2(model1_all_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_all_pred)
mod2_r2<-pseudoR2(model2_all_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_all_pred)
mod3_r2<-pseudoR2(model3_all_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_all_pred)
mod4_r2<-pseudoR2(model4_all_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_all_pred)
mod5_r2<-pseudoR2(model5_all_pred, nullform = ~1)

dog1<-"occupancy ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"occupancy ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"occupancy ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"occupancy ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"occupancy ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_one_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

#####
#Prey
model1_all_pred <- HLCor(prey.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix),
                         control.HLfit=list(max.iter.mean=300))
#summary(model1_early_period_pred, corr = FALSE)

model2_all_pred <- HLCor(prey.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_early_period_pred, corr = FALSE)

model3_all_pred <- HLCor(prey.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model3_early_period_pred, corr = FALSE)

model4_all_pred <- HLCor(prey.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_early_period_pred, corr = FALSE)

model5_all_pred <- HLCor(prey.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model5_early_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_all_pred)
mod1_r2<-pseudoR2(model1_all_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_all_pred)
mod2_r2<-pseudoR2(model2_all_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_all_pred)
mod3_r2<-pseudoR2(model3_all_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_all_pred)
mod4_r2<-pseudoR2(model4_all_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_all_pred)
mod5_r2<-pseudoR2(model5_all_pred, nullform = ~1)

dog1<-"occupancy ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"occupancy ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"occupancy ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"occupancy ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"occupancy ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 1",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_one_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


########################################################################################################################################################################
mid_period_pred <- data %>%
  filter(day > 75 & day < 151) %>%
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, pred.oc,prey.oc) %>%
  group_by(structure, rep, connectivity)


##### Models
model1_all_pred <- HLCor(pred.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model1_mid_period_pred, corr = FALSE)

model2_all_pred <- HLCor(pred.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_mid_period_pred, corr = FALSE)

model2_all_pred <- HLCor(pred.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model3_mid_period_pred, corr = FALSE)

model4_all_pred <- HLCor(pred.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_mid_period_pred, corr = FALSE)

model5_all_pred <- HLCor(pred.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model5_mid_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_all_pred)
mod1_r2<-pseudoR2(model1_all_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_all_pred)
mod2_r2<-pseudoR2(model2_all_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_all_pred)
mod3_r2<-pseudoR2(model3_all_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_all_pred)
mod4_r2<-pseudoR2(model4_all_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_all_pred)
mod5_r2<-pseudoR2(model5_all_pred, nullform = ~1)

dog1<-"occupancy ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"occupancy ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"occupancy ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"occupancy ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"occupancy ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_two_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


#######
#Prey
model1_all_pred <- HLCor(prey.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model1_mid_period_pred, corr = FALSE)

model2_all_pred <- HLCor(prey.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_mid_period_pred, corr = FALSE)

model3_all_pred <- HLCor(prey.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model3_mid_period_pred, corr = FALSE)

model4_all_pred <- HLCor(prey.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_mid_period_pred, corr = FALSE)

model5_all_pred <- HLCor(prey.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model5_mid_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_all_pred)
mod1_r2<-pseudoR2(model1_all_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_all_pred)
mod2_r2<-pseudoR2(model2_all_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_all_pred)
mod3_r2<-pseudoR2(model3_all_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_all_pred)
mod4_r2<-pseudoR2(model4_all_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_all_pred)
mod5_r2<-pseudoR2(model5_all_pred, nullform = ~1)

dog1<-"occupancy ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"occupancy ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"occupancy ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"occupancy ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"occupancy ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 2",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_two_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))


########################################################################################################################################################################
#Last Experimental Period Predators
last_period_pred <- data %>%
  filter(day > 150) %>%
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, pred.oc,prey.oc) %>%
  group_by(structure, rep, connectivity)


##### Models
model1_all_pred <- HLCor(pred.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_all_pred <- HLCor(pred.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_all_pred <- HLCor(pred.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_all_pred <- HLCor(pred.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_all_pred <- HLCor(pred.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_all_pred)
mod1_r2<-pseudoR2(model1_all_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_all_pred)
mod2_r2<-pseudoR2(model2_all_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_all_pred)
mod3_r2<-pseudoR2(model3_all_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_all_pred)
mod4_r2<-pseudoR2(model4_all_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_all_pred)
mod5_r2<-pseudoR2(model5_all_pred, nullform = ~1)

dog1<-"occupancy ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"occupancy ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"occupancy ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"occupancy ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"occupancy ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("predator",5))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_three_pred<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

######
#Prey
model1_all_pred <- HLCor(prey.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model1_last_period_pred, corr = FALSE)

model2_all_pred <- HLCor(prey.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model2_last_period_pred, corr = FALSE)

model3_all_pred <- HLCor(prey.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model3_last_period_pred, corr = FALSE)

model4_all_pred <- HLCor(prey.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model4_last_period_pred, corr = FALSE)

model5_all_pred <- HLCor(prey.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
#summary(model5_last_period_pred, corr = FALSE)

mod1_aic<-extractAIC(model1_all_pred)
mod1_r2<-pseudoR2(model1_all_pred, nullform = ~1)

mod2_aic<-extractAIC(model2_all_pred)
mod2_r2<-pseudoR2(model2_all_pred, nullform = ~1)

mod3_aic<-extractAIC(model3_all_pred)
mod3_r2<-pseudoR2(model3_all_pred, nullform = ~1)

mod4_aic<-extractAIC(model4_all_pred)
mod4_r2<-pseudoR2(model4_all_pred, nullform = ~1)

mod5_aic<-extractAIC(model5_all_pred)
mod5_r2<-pseudoR2(model5_all_pred, nullform = ~1)

dog1<-"occupancy ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number)"
dog2<-"occupancy ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number)"
dog3<-"occupancy ~ connectivity + (1|rep) + adjacency(1|bottle.number)"
dog4<-"occupancy ~ structure + (1|rep) + adjacency(1|bottle.number)"
dog5<-"occupancy ~ 1 + (1|rep) + adjacency(1|bottle.number)"

predictor<-c(dog1,dog2,dog3,dog4,dog5)
pred.df<-as.data.frame(predictor,ncol=1)

time<-c(rep("Phase 3",5))
time2<-as.data.frame(time, ncol=1)

species<-c(rep("prey",5))
species2<-as.data.frame(species, ncol=1)

df<-c(mod1_aic[1],mod2_aic[1],mod3_aic[1],mod4_aic[1],mod5_aic[1])
df2<-as.data.frame(df, ncol=1)

aic<-c(mod1_aic[2],mod2_aic[2],mod3_aic[2],mod4_aic[2],mod5_aic[2])
aic2<-as.data.frame(aic, ncol=1)

r2<-c(mod1_r2,mod2_r2,mod3_r2,mod4_r2,mod5_r2)
r22<-as.data.frame(r2, ncol=1)

merged<-as.data.frame(cbind(time2,species2,pred.df,df2,aic2,r22))
merged_three_prey<-merged%>%arrange(aic)%>%mutate(delta_aic=aic-min(aic))%>%mutate(lklhd=exp(-.5*delta_aic))%>%mutate(weight=lklhd/(sum(lklhd)))

########################################################################################################################################################################
ent<-merged_all_pred%>%full_join(merged_all_prey)%>%
  full_join(merged_one_pred)%>%full_join(merged_one_prey)%>%
  full_join(merged_two_pred)%>%full_join(merged_two_prey)%>%
  full_join(merged_three_pred)%>%full_join(merged_three_prey)

write.csv(ent, "new_occup_model_2.csv")

