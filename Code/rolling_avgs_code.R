library(geofacet)
library(zoo)
library(tidyverse)
library(ggplot2)

setwd("~/Desktop/network")
net.data <- read.csv("net.data.csv")
#net.data<-Data

net.ra1 <- net.data %>%
  group_by(structure, replicate, day) %>% # day needs to be to right of density or whatever y variable we are taking rolling avg of
  # filter(structure != "single") %>%
  summarise(mean.eup.dens = mean(eup.density, na.rm=TRUE)) %>%
  mutate(ra3 = zoo::rollmean(mean.eup.dens, k=3, fill=NA),
         ra5 = zoo::rollmean(mean.eup.dens, k=5, fill=NA),
         ra7 = zoo::rollmean(mean.eup.dens, k=7, fill=NA)) %>%
  dplyr::ungroup()
# ** Rollmean does something weird with zeros - when all 3, 5 or 7 values to be averaged are zero, it returns a negative number
min(na.omit(net.ra1$ra3)) 
min(na.omit(net.ra1$ra5))
min(na.omit(net.ra1$ra7))
# This looks to be a common issue with this function according to stackoverflow 
# Until I figure out how to fix this, I am reassigning all negative values zero:
net.ra1$ra3[net.ra1$ra3<="0"] <- 0
net.ra1$ra5[net.ra1$ra5<="0"] <- 0
net.ra1$ra7[net.ra1$ra7<="0"] <- 0

min(na.omit(net.ra1$ra3))
min(na.omit(net.ra1$ra5))
min(na.omit(net.ra1$ra7))


# Check that rollmean calculations are correct:
c <- net.ra1 %>% 
  utils::head(25)
view(c)
# the first value in our new variable ra3 is the average dens from the first day with a data point on either side of it
# check  math below:
mean(c(13.64754,10.94412,123.14858))
# the first value in ra5 is the average dens from first day with two data points on either side
# check math below:
mean(c(17.17907116,6.04655464,0.05714838,0.01181813,0.05770000))

# Reconfigure dataset for visualization:
net.ra2 <- net.ra1 %>%
  tidyr::pivot_longer(names_to = "rolling_mean_key",
                      values_to = "rolling_mean_value",
                      cols = c(mean.eup.dens,
                               ra3,
                               ra5,
                               ra7)) 

ggplot(net.ra2, aes(x = day, y = rolling_mean_value,
                    color = rolling_mean_key))+
  geom_line()+
  facet_grid(structure~replicate)


# Define low and high thresholds:
# high threshold = avg density in phases 2 & 3?
# low threshold = max of avg of all networks low phase?

df1 <- net.ra1 %>% filter(day>=30, day<=75) # chose these days by looking at graph
ggplot(df1, aes(x=day, y=mean.eup.dens))+geom_line()
max(na.omit(df1$mean.eup.dens)) # max of low phase = 29.14591 our low threshold value
df2 <- net.ra1 %>% filter(day>=100, day<=150)
ggplot(df2, aes(x=day, y=mean.eup.dens))+geom_line()
mean(df2$mean.eup.dens) # avg of high phase = 46.7359

net.ra1_metrics<-net.ra1%>%
  filter(day >5)%>%
  group_by(structure,replicate)%>%
  summarize(day.col=day[which.max(ra3)], day.ext=day[which.min(ra3)],
            density.at.col=max(mean.eup.dens), density.at.ext=min(mean.eup.dens))
  
