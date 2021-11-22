### Clara Woodie, 2021
### Paper: Long transients and network structure effect persistence and spatial predator-prey dynamics in experimental microcosms

library(tidyverse)
library(ggplot2)
library(zoo)
library(AICcmodavg)
library(viridis)
library(gridExtra)

setwd("~/Desktop/network")
net.data <- read.csv("net.data.csv")

###################################
# Moving Averages on bottle level #
###################################
bot.mov.avg1 <- net.data %>%
  group_by(structure, replicate, connectivity, bottle, day) %>%
  summarise(mean.eup.dens = mean(eup.density, na.rm=TRUE),
            mean.tet.dens = mean(tetra.density, na.rm=TRUE)) %>%
  mutate(ra3.eup = zoo::rollmean(mean.eup.dens, k=3, fill=NA),
         ra3.tet = zoo::rollmean(mean.tet.dens, k=3, fill=NA)) %>%
  dplyr::ungroup()

# Rollmean does something weird with zeros - when all values to be averaged are zero, it returns a negative number
min(na.omit(bot.mov.avg1$ra3.eup)) 
min(na.omit(bot.mov.avg1$ra3.tet)) 
# This looks to be a common issue with this function according to stackoverflow 
# Reassign all negative values zero:
bot.mov.avg1$ra3.eup[bot.mov.avg1$ra3.eup<="0"] <- 0
bot.mov.avg1$ra3.tet[bot.mov.avg1$ra3.tet<="0"] <- 0
min(na.omit(bot.mov.avg1$ra3.eup)) 
min(na.omit(bot.mov.avg1$ra3.tet)) 

# Reconfigure dataset for visualization:
bot.mov.avg2 <- bot.mov.avg1 %>%
  tidyr::pivot_longer(names_to = "rolling.mean.key",
                      values_to = "rolling.mean.value",
                      cols = c(mean.eup.dens, mean.tet.dens,
                               ra3.eup, ra3.tet)) 

bot.mov.avg2.eup <- bot.mov.avg1 %>%
  dplyr::select(structure, replicate, connectivity, bottle, day, mean.eup.dens, ra3.eup) %>%
  tidyr::pivot_longer(names_to = "rolling.mean.key",
                      values_to = "rolling.mean.value",
                      cols = c(mean.eup.dens, ra3.eup)) 
ggplot(bot.mov.avg2.eup, aes(x = day, y = rolling.mean.value,
                        color = rolling.mean.key))+
  geom_line()

bot.mov.avg2.tet <- bot.mov.avg1 %>%
  tidyr::pivot_longer(names_to = "rolling.mean.key",
                      values_to = "rolling.mean.value",
                      cols = c(mean.tet.dens, ra3.tet)) 

ggplot(bot.mov.avg2.tet, aes(x = day, y = rolling.mean.value,
                        color = rolling.mean.key))+
  geom_line()


# Define low and high thresholds for Eup:
# low threshold = avg of density in phase 1 *after peak, ~day30*
df1 <- bot.mov.avg1 %>% filter(day>=30, day<=75)
mean(na.omit(df1$ra3.eup)) # 2.267064 is our low threshold value
# high threshold = avg of eup density in phase 2
df2<- bot.mov.avg1 %>% filter(day>=75, day<=150)
mean(na.omit(df2$ra3.eup)) # high threshold = 37.44639

## Day at extintinction calculation:
df.ext.bottle <- bot.mov.avg1 %>%
  filter(day > 10, day < 100)%>%
  group_by(bottle,structure,replicate,connectivity)%>%
  summarize(day.at.extinction = day[min(which(ra3.eup <= 2.267064))]) 
df.ext.bottle$day.at.extinction[df.ext.bottle$day.at.extinction %in% NA] <- 22 # we know from occupancy data that single bottle reps 1-8 went extinct at day 22

## Day at recolonization calculation:
df.recol.bottle <- bot.mov.avg1 %>%
  filter(day > 100)%>%
  group_by(bottle,structure,replicate,connectivity)%>%
  summarize(day.at.recol = day[ra3.eup >= 37.44639]) %>% 
  slice_min(day.at.recol) # the ones that didn't recolonize are removed here but will show back up when we merge datasets below

# Merge data frames:
merged.df <- merge(df.ext.bottle, df.recol.bottle, by.x=c("structure", "replicate", "bottle", "connectivity"), by.y=c("structure", "replicate", "bottle", "connectivity"),
                all.x=TRUE, all.y=TRUE)
merged.df$day.at.recol[merged.df$day.at.recol %in% NA] <- 236 # all the ones with NA mean they never returned past high threshold, set to 236 (final day of exp) to get total extinction time calculation
merged.df2 <- merged.df %>%
  mutate(total.extinction.time = as.numeric(day.at.recol) - as.numeric(day.at.extinction)) # all bottles accounted for, no NAs present

write.csv(merged.df2, "MA.bottle.csv") 


##############
# Statistics #
##############
network.wide <- read.csv("~/Desktop/Network/network-wide.calculations.csv") # These were determined by visualizing occupancy time series for each replicate.
network.wide <- network.wide %>%
  mutate(structure2 = case_when(structure == "single" ~ "single.bottle",
                                structure == "dendritic" ~ "multiple.bottles",
                                structure == "lattice" ~ "multiple.bottles"),
         structure3 = case_when(structure == "single" ~ "not.dendritic",
                                structure == "lattice" ~ "not.dendritic",
                                structure == "dendritic" ~ "is.dendritic"),
         structure4 = case_when(structure == "single" ~ "not.lattice",
                                structure == "lattice" ~ "is.lattice",
                                structure == "dendritic" ~ "not.lattice"))
network.wide$structure <- factor(network.wide$structure, levels=c("single","dendritic","lattice"))

MA.bottle <- read.csv("~/Desktop/Network/MA.bottle.csv")
MA.bottle <- MA.bottle %>%
  mutate(structure2 = case_when(structure == "single" ~ "single.bottle",
                                structure == "dendritic" ~ "multiple.bottles",
                                structure == "lattice" ~ "multiple.bottles"),
         structure3 = case_when(structure == "single" ~ "not.dendritic",
                                structure == "lattice" ~ "not.dendritic",
                                structure == "dendritic" ~ "is.dendritic"),
         structure4 = case_when(structure == "single" ~ "not.lattice",
                                structure == "lattice" ~ "is.lattice",
                                structure == "dendritic" ~ "not.lattice"),
         con.0 = case_when(connectivity == "single" ~ "single",
                           connectivity == "1" ~ "connected",
                           connectivity == "2" ~ "connected",
                           connectivity == "3" ~ "connected",
                           connectivity == "4" ~ "connected"))
MA.bottle$structure <- factor(MA.bottle$structure, levels=c("single","dendritic","lattice"))
MA.bottle$connectivity <- factor(MA.bottle$connectivity, levels=c("single","1","2","3","4"))

### Time to Extinction - Network-wide - Structure ###
p1 <- ggplot(network.wide, aes(x=structure, y=day.at.extinction, fill=structure))+geom_boxplot()+
  ylab("Predator time to extinction (days)")+xlab("Structure")+
  ggtitle("a)")+
  scale_fill_viridis(discrete = TRUE, name = "Structure", labels = c("Isolated", "Dendritic", "Lattice"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),legend.position = "none")+
  scale_x_discrete(breaks=c("single","dendritic","lattice"),
                   labels=c("Isolated","Dendritic","Lattice"))
#legend.key.size = unit(0.5, 'cm'),  legend.title = element_text(size=7),  legend.text = element_text(size=7), legend.position = c(0.15, 0.9))
print(p1)

structure <- glm(day.at.extinction ~ structure, data=network.wide, family=poisson)
spatial <- glm(day.at.extinction ~ structure2, data=network.wide, family=poisson)
null <- glm(day.at.extinction ~ 1, data=network.wide, family=poisson)

cand.models <- list("structure" = structure, 
                    "spatial" = spatial, 
                    "null" = null) 
aictab(cand.models) 
((structure$null.deviance-structure$deviance)/structure$null.deviance)
((spatial$null.deviance-spatial$deviance)/spatial$null.deviance)
((null$null.deviance-null$deviance)/null$null.deviance)

### Time to Extinction - Bottle level - Structure ###
p2 <- ggplot(MA.bottle, aes(x=structure, y=day.at.extinction, fill=structure))+geom_boxplot()+
  ylab("Predator time to extinction (days)")+xlab("Structure")+
  ggtitle("b)")+
  scale_fill_viridis(discrete = TRUE, name = "Structure", labels = c("Isolated", "Dendritic", "Lattice"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),legend.position = "none")+
  scale_x_discrete(breaks=c("single","dendritic","lattice"),
                   labels=c("Isolated","Dendritic","Lattice"))
print(p2)
# *stats done below which includes connectivity

### Time to Extinction - Bottle level - Connectivity ###
p3 <- ggplot(MA.bottle, aes(x=connectivity, y=day.at.extinction, fill=connectivity))+geom_boxplot()+
  ylab("Predator time to extinction (days)")+xlab("Connectivity")+
  ggtitle("c)")+
  scale_fill_viridis(discrete = TRUE, name = "Connectivity", labels = c("0", "1", "2", "3", "4"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),legend.position = "none")+
  scale_x_discrete(breaks=c("single","1","2","3","4"),
                   labels=c("0","1","2","3","4"))
print(p3)

connectivity <- glm(day.at.extinction ~ connectivity, data=MA.bottle, family=poisson)
structure <- glm(day.at.extinction ~ structure, data=MA.bottle, family=poisson)
spatial <- glm(day.at.extinction ~ structure2, data=MA.bottle, family=poisson)
null <- glm(day.at.extinction ~ 1, data=MA.bottle, family=poisson)

cand.models <- list("connectivity" = connectivity,
                    "structure" = structure, 
                    "spatial" = spatial, 
                    "null" = null) 

aictab(cand.models) 
((connectivity$null.deviance-connectivity$deviance)/connectivity$null.deviance)
((structure$null.deviance-structure$deviance)/structure$null.deviance)
((spatial$null.deviance-spatial$deviance)/spatial$null.deviance)
((null$null.deviance-null$deviance)/null$null.deviance)

### Total Extinction Time - Network-wide - Structure ###
p4 <- ggplot(network.wide, aes(x=structure, y=total.extinction.time, fill=structure))+geom_boxplot()+
  ylab("Predator total extinction time (days)")+xlab("Structure")+
  ggtitle("d)")+
  scale_fill_viridis(discrete = TRUE, name = "Structure", labels = c("Isolated", "Dendritic", "Lattice"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),legend.position = "none")+
  scale_x_discrete(breaks=c("single","dendritic","lattice"),
                   labels=c("Isolated","Dendritic","Lattice"))
print(p4)

structure <- glm(total.extinction.time ~ structure, data=network.wide, family=poisson)
spatial <- glm(total.extinction.time ~ structure2, data=network.wide, family=poisson)
null <- glm(total.extinction.time ~ 1, data=network.wide, family=poisson)

cand.models <- list("structure" = structure, 
                    "spatial" = spatial, 
                    "null" = null) 

aictab(cand.models) 
((structure$null.deviance-structure$deviance)/structure$null.deviance)
((spatial$null.deviance-spatial$deviance)/spatial$null.deviance)
((null$null.deviance-null$deviance)/null$null.deviance)


### Total Extinction Time - Bottle level - Structure ###
p5 <- ggplot(MA.bottle, aes(x=structure, y=total.extinction.time, fill=structure))+geom_boxplot()+
  ylab("Predator total extinction time (days)")+xlab("Structure")+
  ggtitle("e)")+
  scale_fill_viridis(discrete = TRUE, name = "Structure", labels = c("Isolated", "Dendritic", "Lattice"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),legend.position = "none")+
  scale_x_discrete(breaks=c("single","dendritic","lattice"),
                   labels=c("Isolated","Dendritic","Lattice"))
print(p5)
# *stats done below including connectivity

### Total Extinction Time - Bottle level - Connectivity ###
p6 <- ggplot(MA.bottle, aes(x=connectivity, y=total.extinction.time, fill=connectivity))+geom_boxplot()+
  ylab("Predator total extinction time (days)")+xlab("Connectivity")+
  ggtitle("f)")+
  scale_fill_viridis(discrete = TRUE, name = "Connectivity", labels = c("0", "1", "2", "3", "4"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),legend.position = "none")+
  scale_x_discrete(breaks=c("single","1","2","3","4"),
                   labels=c("0","1","2","3","4"))
print(p6)

connectivity <- glm(total.extinction.time ~ connectivity, data=MA.bottle, family=poisson)
structure <- glm(total.extinction.time ~ structure, data=MA.bottle, family=poisson)
spatial <- glm(total.extinction.time ~ structure2, data=MA.bottle, family=poisson)
null <- glm(total.extinction.time ~ 1, data=MA.bottle, family=poisson)

cand.models <- list("connectivity" = connectivity,
                    "structure" = structure, 
                    "spatial" = spatial, 
                    "null" = null) 

aictab(cand.models) 
((connectivity$null.deviance-connectivity$deviance)/connectivity$null.deviance)
((structure$null.deviance-structure$deviance)/structure$null.deviance)
((spatial$null.deviance-spatial$deviance)/spatial$null.deviance)
((null$null.deviance-null$deviance)/null$null.deviance)

# Combine Plots
plot.combined <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2)
plot(plot.combined)
png("~/Desktop/plot.combined.png", width = 12.25, height = 5.25, units = "in", res = 1200, pointsize = 4)
par(mar = c(5, 5, 2, 2),xaxs  = "i", yaxs = "i", cex.axis = 2, cex.lab  = 2)
plot(plot.combined)
dev.off()
