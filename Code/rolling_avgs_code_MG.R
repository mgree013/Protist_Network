library(geofacet)
library(zoo)
library(tidyverse)
library(ggplot2)

setwd("~/Desktop/network")
net.data <- read.csv("net.data.csv")
net.data<-Data

net.datas<-net.data%>%
  group_by(structure, replicate, day,bottle,connectivity,position)%>%
  dplyr::summarise(mean.eup.dens = mean(eup.density, na.rm=TRUE),
                   mean.tet.dens = mean(tetra.density, na.rm=TRUE))



net.ra1.bot <- net.data %>%
  group_by(structure, replicate, day,bottle) %>% # day needs to be to right of density or whatever y variable we are taking rolling avg of
  # filter(structure != "single") %>%
  summarise(mean.eup.dens = mean(eup.density, na.rm=TRUE),
            mean.tet.dens = mean(tetra.density, na.rm=TRUE)) %>%
  mutate(ra3.eup = zoo::rollmean(mean.eup.dens, k=3, fill=NA),
         ra3.tet = zoo::rollmean(mean.tet.dens, k=3, fill=NA)) %>%
  dplyr::ungroup()
# ** Rollmean does something weird with zeros - when all 3, 5 or 7 values to be averaged are zero, it returns a negative number
min(na.omit(net.ra1.bot$ra3.eup)) 
min(na.omit(net.ra1.bot$ra3.tet)) 

# This looks to be a common issue with this function according to stackoverflow 
# Until I figure out how to fix this, I am reassigning all negative values zero:
net.ra1.bot$ra3.eup[net.ra1.bot$ra3.eup<="0"] <- 0
net.ra1.bot$ra3.tet[net.ra1.bot$ra3.tet<="0"] <- 0

min(na.omit(net.ra1.bot$ra3.eup)) 
min(na.omit(net.ra1.bot$ra3.tet)) 

f <- net.ra1.bot%>% filter(bottle=="D11A")
ggplot(f, aes(x=day, y=ra3.eup, colour="ra3"))+geom_line()+
  geom_line(aes(x=day, y=mean.eup.dens, colour="mean.eup"))


# Check that rollmean calculations are correct:
c <- net.ra1.bot %>% 
  utils::head(25)
view(c)
# the first value in our new variable ra3 is the average dens from the first day with a data point on either side of it
# check  math below:
mean(c(13.64754,10.94412,123.14858))
# the first value in ra5 is the average dens from first day with two data points on either side
# check math below:
mean(c(17.17907116,6.04655464,0.05714838,0.01181813,0.05770000))

# Reconfigure dataset for visualization:
net.ra2.bot <- net.ra1.bot %>%
  filter(replicate =="A" & structure=="dendritic")%>%
  tidyr::pivot_longer(names_to = "rolling_mean_key",
                      values_to = "rolling_mean_value",
                      cols = c(mean.eup.dens, mean.tet.dens,
                               ra3.eup, ra3.tet)) 

ggplot(net.ra2.bot, aes(x = day, y = rolling_mean_value,
                        color = rolling_mean_key))+
  geom_line()+
  facet_grid(structure~replicate)

net.ra1.bot %>%
  filter(replicate =="A" & structure=="dendritic")%>%
  tidyr::pivot_longer(names_to = "rolling_mean_key",
                      values_to = "rolling_mean_value",
                      cols = c(mean.eup.dens, mean.tet.dens,
                               ra3.eup, ra3.tet)) %>%
  ggplot(aes(x = day, y = rolling_mean_value,
                        color = rolling_mean_key))+scale_y_log10()+
  geom_line()+
  facet_wrap(~bottle)


# Define low and high thresholds for eup:
# high threshold = avg of eup density in phase 2
# low threshold = avg of density in phase 1 *after peak, ~day30*
df1.bot <- net.ra1.bot %>% filter(day>=30, day<=75) # chose these days by looking at graph
mean(na.omit(df1.bot$ra3.eup)) # avg of low phase = 2.480592 our low threshold value

df2.bot <- net.ra1.bot %>% filter(day>=75, day<=150)
mean(na.omit(df2.bot$ra3.eup)) # high threshold = 37.02566



## Day at extintinction calculation:
df.ext.bottle <- net.ra1.bot %>%
  filter(day > 10, day < 100)%>%
  group_by(bottle,structure,replicate)%>%
  #summarize(day.ext = day[min(which(mean.eup.dens <= 2.480592))])
  summarize(day.ext = day[which.min(mean.eup.dens <= 2.480592)]) 

#%>% slice_max(day.ext)
#day.ext=day[which.min(ra3)],
#density.at.ext=min(mean.eup.dens)
df.ext.bottle$day.ext[df.ext.bottle$day.ext %in% NA] <- "22" # we know from occupancy data that these single bottles that megan sampled go extinct at day 22


## Day at recolonization calculation:
df.recol.bottle <- net.ra1.bot %>%
  filter(day > 75)%>%
  group_by(bottle,structure,replicate)%>%
  summarize(day.recol = day[mean.eup.dens >= 37.02566]) %>% 
  slice_min(day.recol) 
# day.col=day[min(which(ra3 > 46.7359))]

## Day at first zero prey oc:
df3 <- df2 <- net.ra1.bot %>% filter(day>=150) # avg tet phase 3
ggplot(df2, aes(x=day, y=ra3.tet))+geom_line()
mean(na.omit(df2$ra3.tet)) # 474.7037

df.prey.ex.bottle <- net.ra1.bot %>%
  filter(day>50) %>%
  group_by(bottle, structure, replicate, connectivity) %>%
  summarise(day.prey.ext = day[mean.tet.dens <= 474.7037]) %>%
  slice_min(day.prey.ext)
# ** issue: some of these are before the predator's recol

# Merge data frames:
newdf2 <- merge(df.ext.bottle, df.recol.bottle, by.x=c("structure", "replicate", "bottle", "connectivity"), by.y=c("structure", "replicate", "bottle"),
                all.x=TRUE, all.y=TRUE)
newdf.bot <- merge(newdf2, df.prey.ex.bottle, all.x=TRUE, all.y = TRUE)
newdf.bot<-merge(net.datas,newdf.bot)

view(newdf.bot) 

newdf.bot <- newdf.bot %>%
  mutate(days.extinct = as.numeric(day.recol) - as.numeric(day.ext))

# Check that calculations make sense:
z <- net.ra1.bot %>% filter(bottle=="D14B")
a <- ggplot(z, aes(x=day, y=ra3.eup, colour="pred"))+geom_line()
b <- ggplot(z, aes(x=day, y=ra3.tet, colour="prey"))+geom_line()
grid.arrange(a,b,ncol=2,nrow=1)

# Visualize new measurements:
d = ggplot(newdf.bot, aes(x=structure, y=as.numeric(day.ext)))+geom_boxplot() +ggtitle("By Individual Bottle")+ylab("predator persistence phase 1") 
# not same as rep - no relationship???
e = ggplot(newdf.bot, aes(x=structure, y=days.extinct))+geom_boxplot() +ggtitle("By Individual Bottle")+ylab("predator total extinction time") 
# doesnt look sig
f = ggplot(newdf.bot, aes(x=structure, y=day.prey.ext))+geom_boxplot() +ggtitle("By Individual Bottle")+ylab("prey crash after predator returns") 
# no relationship
d = ggplot(newdf.bot, aes(x=as.factor(connectivity), y=as.numeric(day.ext)))+geom_boxplot() +ggtitle("By Individual Bottle")+ylab("predator persistence phase 1") 
# not same as rep - no relationship???
e = ggplot(newdf.bot, aes(x=as.factor(connectivity), y=days.extinct))+geom_boxplot() +ggtitle("By Individual Bottle")+ylab("predator total extinction time") 
# doesnt look sig
f = ggplot(newdf.bot, aes(x=as.factor(connectivity), y=day.prey.ext))+geom_boxplot() +ggtitle("By Individual Bottle")+ylab("prey crash after predator returns") 
# no relationship

newdf.bot%>%
  filter(day.ext< 70)%>%
  ggplot(aes(x=as.factor(connectivity), y=as.numeric(day.ext)))+geom_boxplot() +ggtitle("By Individual Bottle")+ylab("predator persistence phase 1") 

#
##
###
####
##### Average the Bottle data and check metapops #####
newdf.avg <- newdf.bot %>%
  group_by(structure, replicate) %>%
  summarise(avg.day.ext = mean(as.numeric(day.ext), na.rm=TRUE),
            avg.day.recol = mean(day.recol, na.rm=TRUE),
            avg.day.prey.ext = mean(day.prey.ext, na.rm=TRUE),
            avg.days.extinct = mean(days.extinct, na.rm=TRUE))

g = ggplot(newdf.avg, aes(x=structure, y=avg.day.ext))+geom_boxplot() +ggtitle("By Bottle then averaged by replicate")+ylab("predator persistence phase 1") 
# oppposite as first one (by rep) - single bottles go extinct slower???
h = ggplot(newdf.avg, aes(x=structure, y=avg.days.extinct))+geom_boxplot()+ggtitle("By Bottle then averaged by replicate")+ylab("predator total extinction time") 
# similar to both by-rep and by-bottle
i = ggplot(newdf.avg, aes(x=structure, y=avg.day.prey.ext))+geom_boxplot()+ggtitle("By Bottle then averaged by replicate")+ylab("prey crash after predator returns") 



####
grid.arrange(a,b,c,
             d,e,f,
             g,h,i, ncol=3, nrow=3)



net.ra1_metrics<-net.ra1%>%
  filter(day >5)%>%
  group_by(structure,replicate)%>%
  summarize(day.col=day[which.max(ra3)], day.ext=day[which.min(ra3)],
            day.col2=day[min(which(ra3 > 500))],
            density.at.col=max(mean.eup.dens), density.at.ext=min(mean.eup.dens))
  
