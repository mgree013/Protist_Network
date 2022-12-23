##############################################################################################################################
#Trophic Analysis Figure 8 and Table ?

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
  pivot_longer(cols=ln.prey:ln.pred,names_to = "species", values_to="density")%>%
  left_join(bac_density, by=c("bottle.number", "structure","rep"))


env_density_pred<-env_density%>%filter(species=="ln.pred")

model1_last_period_pred <- HLCor(density ~ structure + bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(density ~ structure + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(density ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)

#Prey All
env_density_prey<-env_density%>%filter(species=="ln.prey")

model1_all_period_prey <- HLCor(density ~ structure + bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model1_all_period_prey, corr = FALSE)

model2_all_period_prey <- HLCor(density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model2_all_period_prey, corr = FALSE)

model3_all_period_prey <- HLCor(density ~ structure + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model3_all_period_prey, corr = FALSE)

model4_all_period_prey <- HLCor(density ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model4_all_period_prey, corr = FALSE)

AIC.HLfit(model1_all_period_prey)
AIC.HLfit(model2_all_period_prey)
AIC.HLfit(model3_all_period_prey)
AIC.HLfit(model4_all_period_prey)

##########################################################################################################################################################################################
#Phase 1

env_density_pred<-env_density%>%filter(species=="ln.pred")%>%  filter(day > 0 & day < 76) 

model1_last_period_pred <- HLCor(density ~ structure + bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(density ~ structure + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(density ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)

#Prey All
env_density_prey<-env_density%>%filter(species=="ln.prey")%>%  filter(day > 0 & day < 76) 

model1_all_period_prey <- HLCor(density ~ structure + bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model1_all_period_prey, corr = FALSE)

model2_all_period_prey <- HLCor(density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model2_all_period_prey, corr = FALSE)

model3_all_period_prey <- HLCor(density ~ structure + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model3_all_period_prey, corr = FALSE)

model4_all_period_prey <- HLCor(density ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model4_all_period_prey, corr = FALSE)

AIC.HLfit(model1_all_period_prey)
AIC.HLfit(model2_all_period_prey)
AIC.HLfit(model3_all_period_prey)
AIC.HLfit(model4_all_period_prey)

##########################################################################################################################################################################################
#Phase 2

env_density_pred<-env_density%>%filter(species=="ln.pred")%>%
  filter(day > 75 & day < 151)

model1_last_period_pred <- HLCor(density ~ structure + bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(density ~ structure + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(density ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)

#Prey All
env_density_prey<-env_density%>%filter(species=="ln.prey")%>%
  filter(day > 75 & day < 151)

model1_all_period_prey <- HLCor(density ~ structure + bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model1_all_period_prey, corr = FALSE)

model2_all_period_prey <- HLCor(density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model2_all_period_prey, corr = FALSE)

model3_all_period_prey <- HLCor(density ~ structure + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model3_all_period_prey, corr = FALSE)

model4_all_period_prey <- HLCor(density ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model4_all_period_prey, corr = FALSE)

AIC.HLfit(model1_all_period_prey)
AIC.HLfit(model2_all_period_prey)
AIC.HLfit(model3_all_period_prey)
AIC.HLfit(model4_all_period_prey)

##########################################################################################################################################################################################
#Phase 3

env_density_pred<-env_density%>%filter(species=="ln.pred")%>%
  filter(day > 150)

model1_last_period_pred <- HLCor(density ~ structure + bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(density ~ structure + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(density ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)

#Prey All
env_density_prey<-env_density%>%filter(species=="ln.prey")%>%
  filter(day > 150)

model1_all_period_prey <- HLCor(density ~ structure + bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model1_all_period_prey, corr = FALSE)

model2_all_period_prey <- HLCor(density ~ bac.density_log  + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model2_all_period_prey, corr = FALSE)

model3_all_period_prey <- HLCor(density ~ structure + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model3_all_period_prey, corr = FALSE)

model4_all_period_prey <- HLCor(density ~ 1 + (1|rep) + adjacency(1|bottle.number), family = gaussian, data = env_density_prey, adjMatrix = as.matrix(adj_matrix))
summary(model4_all_period_prey, corr = FALSE)

AIC.HLfit(model1_all_period_prey)
AIC.HLfit(model2_all_period_prey)
AIC.HLfit(model3_all_period_prey)
AIC.HLfit(model4_all_period_prey)
