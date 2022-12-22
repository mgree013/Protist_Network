library(ggplot2)
library(tidyverse)
library(spaMM)
library(rcdd)
library(Rglpk)
library(ROI)
library(ROI.plugin.glpk)
library(e1071)

setwd("G:/My Drive/Anderson Lab Protist Folder")

data <- read.csv("net.data.csv")
adj_matrix <- read.csv("Adj_matrix_big.csv", header=FALSE)

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
  dplyr::select(day, structure, connectivity, rep, bottle.number, pred.oc) %>%
  group_by(structure, rep, connectivity)

ggplot(all_pred, aes(x=day)) + geom_point(aes(y=pred.oc,colour=(structure)))

##### test models without spatial errors
# model_test1 <- glmer(pred.oc ~ structure * connectivity + (1|rep), family = binomial, data = all_pred)
# summary(model_test1, corr = FALSE)
# 
# model_test2 <- fitme(pred.oc ~ connectivity * structure + (1|rep), family = binomial, data = all_pred, rand.family = Beta(logit))
# summary(model_test2, corr = FALSE)
#####

##### Models
model1_all_pred <- HLCor(pred.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_all_pred, corr = FALSE)

model2_all_pred <- HLCor(pred.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_all_pred, corr = FALSE)

model3_all_pred <- HLCor(pred.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_all_pred, corr = FALSE)

model4_all_pred <- HLCor(pred.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
summary(model4_all_pred, corr = FALSE)

model5_all_pred <- HLCor(pred.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_pred, adjMatrix = as.matrix(adj_matrix))
summary(model5_all_pred, corr = FALSE)

AIC.HLfit(model1_all_pred)
AIC.HLfit(model2_all_pred)
AIC.HLfit(model3_all_pred)
AIC.HLfit(model4_all_pred)
AIC.HLfit(model5_all_pred)

extractAIC.HLfit(model1_all_pred)
anova.HLfit(model2_all_pred,model1_all_pred) #Way of implementing LRT if we go that route

#####
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, pred.oc) %>%
  group_by(structure, rep, connectivity)

ggplot(last_period_pred, aes(x=day)) + geom_point(aes(y=pred.oc,colour=(structure)))

##### Models
model1_last_period_pred <- HLCor(pred.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_pred, corr = FALSE)

model2_last_period_pred <- HLCor(pred.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_pred, corr = FALSE)

model3_last_period_pred <- HLCor(pred.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_pred, corr = FALSE)

model4_last_period_pred <- HLCor(pred.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_pred, corr = FALSE)

model5_last_period_pred <- HLCor(pred.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_pred, corr = FALSE)

AIC.HLfit(model1_last_period_pred)
AIC.HLfit(model2_last_period_pred)
AIC.HLfit(model3_last_period_pred)
AIC.HLfit(model4_last_period_pred)
AIC.HLfit(model5_last_period_pred)

#####
#Middle Experimental Period Predators
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, pred.oc) %>%
  group_by(structure, rep, connectivity)

ggplot(mid_period_pred, aes(x=day)) + geom_point(aes(y=pred.oc,colour=(structure)))

##### Models
model1_mid_period_pred <- HLCor(pred.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_mid_period_pred, corr = FALSE)

model2_mid_period_pred <- HLCor(pred.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_mid_period_pred, corr = FALSE)

model3_mid_period_pred <- HLCor(pred.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_mid_period_pred, corr = FALSE)

model4_mid_period_pred <- HLCor(pred.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model4_mid_period_pred, corr = FALSE)

model5_mid_period_pred <- HLCor(pred.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = mid_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model5_mid_period_pred, corr = FALSE)

AIC.HLfit(model1_mid_period_pred)
AIC.HLfit(model2_mid_period_pred)
AIC.HLfit(model3_mid_period_pred)
AIC.HLfit(model4_mid_period_pred)
AIC.HLfit(model5_mid_period_pred)

#####
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, pred.oc) %>%
  group_by(structure, rep, connectivity)

ggplot(early_period_pred, aes(x=day)) + geom_point(aes(y=pred.oc,colour=(structure)))

##### Models
model1_early_period_pred <- HLCor(pred.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model1_early_period_pred, corr = FALSE)

model2_early_period_pred <- HLCor(pred.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model2_early_period_pred, corr = FALSE)

model3_early_period_pred <- HLCor(pred.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model3_early_period_pred, corr = FALSE)

model4_early_period_pred <- HLCor(pred.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model4_early_period_pred, corr = FALSE)

model5_early_period_pred <- HLCor(pred.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = early_period_pred, adjMatrix = as.matrix(adj_matrix))
summary(model5_early_period_pred, corr = FALSE)

AIC.HLfit(model1_early_period_pred)
AIC.HLfit(model2_early_period_pred)
AIC.HLfit(model3_early_period_pred)
AIC.HLfit(model4_early_period_pred)
AIC.HLfit(model5_early_period_pred)

#####
#Full Experimental Period Prey
all_prey <- data %>%
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, prey.oc) %>%
  group_by(structure, rep, connectivity)

ggplot(all_prey, aes(x=day)) + geom_point(aes(y=prey.oc,colour=(structure)))

##### Models
model1_all_prey <- HLCor(prey.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_prey, adjMatrix = as.matrix(adj_matrix))
summary(model1_all_prey, corr = FALSE)

model2_all_prey <- HLCor(prey.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_prey, adjMatrix = as.matrix(adj_matrix))
summary(model2_all_prey, corr = FALSE)

model3_all_prey <- HLCor(prey.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_prey, adjMatrix = as.matrix(adj_matrix))
summary(model3_all_prey, corr = FALSE)

model4_all_prey <- HLCor(prey.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_prey, adjMatrix = as.matrix(adj_matrix))
summary(model4_all_prey, corr = FALSE)

model5_all_prey <- HLCor(prey.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = all_prey, adjMatrix = as.matrix(adj_matrix))
summary(model5_all_prey, corr = FALSE)

AIC.HLfit(model1_all_prey)
AIC.HLfit(model2_all_prey)
AIC.HLfit(model3_all_prey)
AIC.HLfit(model4_all_prey)
AIC.HLfit(model5_all_prey)

#####
#Last Experimental Period Prey
last_period_prey <- data %>%
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, prey.oc) %>%
  group_by(structure, rep, connectivity)

ggplot(last_period_prey, aes(x=day)) + geom_point(aes(y=prey.oc,colour=(structure)))

##### Models
model1_last_period_prey <- HLCor(prey.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model1_last_period_prey, corr = FALSE)

model2_last_period_prey <- HLCor(prey.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model2_last_period_prey, corr = FALSE)

model3_last_period_prey <- HLCor(prey.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model3_last_period_prey, corr = FALSE)

model4_last_period_prey <- HLCor(prey.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model4_last_period_prey, corr = FALSE)

model5_last_period_prey <- HLCor(prey.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model5_last_period_prey, corr = FALSE)

AIC.HLfit(model1_last_period_prey)
AIC.HLfit(model2_last_period_prey)
AIC.HLfit(model3_last_period_prey)
AIC.HLfit(model4_last_period_prey)
AIC.HLfit(model5_last_period_prey)

#####
#Middle Experimental Period Prey
mid_period_prey <- data %>%
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, prey.oc) %>%
  group_by(structure, rep, connectivity)

ggplot(mid_period_prey, aes(x=day)) + geom_point(aes(y=prey.oc,colour=(structure)))

##### Models
model1_mid_period_prey <- HLCor(prey.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model1_mid_period_prey, corr = FALSE)

model2_mid_period_prey <- HLCor(prey.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model2_mid_period_prey, corr = FALSE)

model3_mid_period_prey <- HLCor(prey.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model3_mid_period_prey, corr = FALSE)

model4_mid_period_prey <- HLCor(prey.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model4_mid_period_prey, corr = FALSE)

model5_mid_period_prey <- HLCor(prey.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model5_mid_period_prey, corr = FALSE)

AIC.HLfit(model1_mid_period_prey)
AIC.HLfit(model2_mid_period_prey)
AIC.HLfit(model3_mid_period_prey)
AIC.HLfit(model4_mid_period_prey)
AIC.HLfit(model5_mid_period_prey)

#####
#Early Experimental Period Prey
early_period_prey <- data %>%
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
  dplyr::select(day, structure, connectivity, rep, bottle.number, prey.oc) %>%
  group_by(structure, rep, connectivity)

ggplot(early_period_prey, aes(x=day)) + geom_point(aes(y=prey.oc,colour=(structure)))

##### Models
model1_early_period_prey <- HLCor(prey.oc ~ connectivity + structure + connectivity * structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model1_early_period_prey, corr = FALSE)

model2_early_period_prey <- HLCor(prey.oc ~ connectivity + structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model2_early_period_prey, corr = FALSE)

model3_early_period_prey <- HLCor(prey.oc ~ connectivity + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model3_early_period_prey, corr = FALSE)

model4_early_period_prey <- HLCor(prey.oc ~ structure + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model4_early_period_prey, corr = FALSE)

model5_early_period_prey <- HLCor(prey.oc ~ 1 + (1|rep) + adjacency(1|bottle.number), family = binomial, data = last_period_prey, adjMatrix = as.matrix(adj_matrix))
summary(model5_early_period_prey, corr = FALSE)

AIC.HLfit(model1_early_period_prey)
AIC.HLfit(model2_early_period_prey)
AIC.HLfit(model3_early_period_prey)
AIC.HLfit(model4_early_period_prey)
AIC.HLfit(model5_early_period_prey)





























##### Code purgatory

#%>% 
#  summarize(avg.pred.oc = mean(pred.oc), logit.oc = log(avg.pred.oc/(1-avg.pred.oc)))

ggplot(all, aes(x=connectivity)) + geom_point(aes(y=avg.pred.oc,colour=(structure)))
ggplot(all, aes(x=connectivity)) + geom_point(aes(y=logit.oc,colour=(structure)))

lin.oc = lm(formula = logit.oc ~ structure + connectivity, data = all)
summary(lin.oc)

#######################

bot.data <- Data %>%
  filter(structure=="dendritic" | structure=="lattice") %>% 
  mutate(str.rep = paste(substr(structure,1,1),replicate,sep=""))

ggplot(bot.data, aes(x=day)) + geom_line(aes(y=ln.pred,colour=(bottle)))

#######################

# Look at Replicates of structures individually
all.den<-Data %>%
  filter(structure=="dendritic")%>%
  group_by(replicate, day) %>%
  summarise(density.prey =mean(ln.prey),sd.prey = sd(ln.prey), cv.prey= (sd.prey/density.prey) *100, density.pred= mean(ln.pred), sd.pred=sd(ln.pred), cv.pred=(sd.pred/density.pred)*100)

ggplot(all.den, aes(x=day))  +geom_line(aes(y=density.pred, linetype="dotted", colour=(replicate)))+ geom_line(aes(y=density.prey, linetype="dashed", colour=interaction(as.factor(replicate)))) + labs(x="Time (days)",y="Log Predator-Prey Density Dendritic Reps")+scale_fill_discrete(labels=c("Dendritic", "Linear", "Straight"))  + guides(colour=guide_legend(title="Treatment"), linetype=guide_legend(title = "Predator or Prey"))
ggplot(all.den, aes(x=day)) + geom_line(aes(y=density.pred, linetype="dotted", colour=(replicate))) + labs(x="Time (days)",y="Log Predator-Prey Density Dendritic Reps") + scale_fill_discrete(labels=c("Dendritic", "Linear", "Straight")) + guides(colour=guide_legend(title="Treatment"), linetype=guide_legend(title = "Predator or Prey"))

#Notes:lotf of varibaility intitally

all.lat<-Data %>%
  filter(structure=="lattice")%>%
  group_by(replicate, day) %>%
  summarise(density.prey =mean(ln.prey),sd.prey = sd(ln.prey), cv.prey= (sd.prey/density.prey) *100, density.pred= mean(ln.pred), sd.pred=sd(ln.pred), cv.pred=(sd.pred/density.pred)*100)

#Plots by replicate
ggplot(all.lat, aes(x=day))  +geom_line(aes(y=density.pred, linetype="dotted", colour=interaction(as.factor(replicate))))+ geom_line(aes(y=density.prey, linetype="dashed", colour=interaction(as.factor(replicate)))) + labs(x="Time (days)",y="Log Predator-Prey Density Lattice Reps")+scale_fill_discrete(labels=c("Dendritic", "Linear", "Straight"))  + guides(colour=guide_legend(title="Treatment"), linetype=guide_legend(title = "Predator or Prey"))
ggplot(all.lat, aes(x=day))  +geom_line(aes(y=density.pred, linetype="dotted", colour=interaction(as.factor(replicate))))+ labs(x="Time (days)",y="Log Predator-Prey Density Lattice Reps")+scale_fill_discrete(labels=c("Dendritic", "Linear", "Straight"))  + guides(colour=guide_legend(title="Treatment"), linetype=guide_legend(title = "Predator or Prey"))
