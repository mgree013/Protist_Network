#Trophic Interactions

#Data Subset: Entire Experiment
env_density2<-Data%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

env_density<-Data%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

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
  group_by(structure, rep, bottle.number)%>%summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(bac_density, by=c("bottle.number", "structure","rep"))

#Data Subset: Phase 1
env_density2_1<-Data%>%
  filter(day < 75)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

env_density_1<-Data%>%
  filter(day < 75)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

#Data Subset: Phase 2
env_density2_2<-Data%>%
  filter(day  > 75 & day < 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

env_density_2<-Data%>%
  filter(day  > 75 & day < 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

#Data Subset: Phase 3
env_density2_3<-Data%>%
  filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

env_density_3<-Data%>%
  filter(day  > 150)%>%
  group_by(structure,replicate,bottle)%>%
  summarise(prey =mean(ln.prey), pred= mean(ln.pred))%>%
  pivot_longer(cols=prey:pred,names_to = "species", values_to="density")%>%
  left_join(env, by=c("bottle", "structure","replicate"))%>%
  mutate(bac.density_log=log(bac.density))

##################################################
#8A) All Phases

env_density_pred<-env_density%>%filter(species=="pred")

model1_last_period_pred <- HLCor(density ~ structure + bac.density_log  + (1|replicate) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(density ~ bac.density_log  + (1|replicate) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(density ~ structure + (replicate) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(density ~ 1 + (1|replicate) + adjacency(1|bottle.number), family = gaussian, data = env_density_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)


#Pred vs bacteria
env_density_pred<-env_density%>%filter(species=="pred")
dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_pred)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_pred)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

#Prey vs bacteria
env_density_prey<-env_density%>%filter(species=="prey")
dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_prey)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_prey)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

#Pred vs prey
dog=list()
dog[[1]]<-glm(prey~pred , family=gaussian(link = "identity"), data=env_density2)
dog[[2]]<-glm(prey~1 , family=gaussian(link = "identity"), data=env_density2)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

##################################################
#8B) Phase 1

#Pred vs bacteria
env_density_1_pred<-env_density_1%>%filter(species=="pred")
dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_1_pred)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_1_pred)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = FALSE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

#Prey vs bacteria
env_density_1_prey<-env_density_1%>%filter(species=="prey")
dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_1_prey)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_1_prey)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

#Pred vs prey
dog=list()
dog[[1]]<-glm(prey~pred , family=gaussian(link = "identity"), data=env_density2_1)
dog[[2]]<-glm(prey~1 , family=gaussian(link = "identity"), data=env_density2_1)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

##################################################
#8B) Phase 2

#Pred vs bacteria
env_density_2_pred<-env_density_2%>%filter(species=="pred")
dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_2_pred)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_2_pred)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

#Prey vs bacteria
env_density_2_prey<-env_density_2%>%filter(species=="prey")
dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_2_prey)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_2_prey)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

#Pred vs prey
dog=list()
dog[[1]]<-glm(prey~pred , family=gaussian(link = "identity"), data=env_density2_2)
dog[[2]]<-glm(prey~1 , family=gaussian(link = "identity"), data=env_density2_2)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

##################################################
#8B) Phase 3

#Pred vs bacteria
env_density_3_pred<-env_density_3%>%filter(species=="pred")
dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_3_pred)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_3_pred)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

#Prey vs bacteria
env_density_3_prey<-env_density_3%>%filter(species=="prey")
dog=list()
dog[[1]]<-glm(density~bac.density_log , family=gaussian(link = "identity"), data=env_density_3_prey)
dog[[2]]<-glm(density~1 , family=gaussian(link = "identity"), data=env_density_3_prey)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])

#Prey vs.Pred
dog=list()
dog[[1]]<-glm(prey~pred , family=gaussian(link = "identity"), data=env_density2_3)
dog[[2]]<-glm(prey~1 , family=gaussian(link = "identity"), data=env_density2_3)
Modnames <- paste("mod", 1:length(dog), sep = " ")
reported.table2<-aictab(cand.set = dog, modnames = Modnames, sort = TRUE)
reported.table2
r2(dog[[1]])
r2(dog[[2]])
