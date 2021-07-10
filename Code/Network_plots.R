library(ggraph)
library(igraph)
library(ggplot2)
library(tidyverse)
library(raster)
library(RColorBrewer)
library(cowplot)
library(viridis)

alls<-all_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 175)%>%
  #filter(day  > 175)%>%
  group_by(structure,bottle.number)%>%
  summarise(prey.oc =mean(prey.oc), pred.oc= mean(pred.oc))


edges = read_csv("dend.edges.csv")

nodes = alls%>%filter(structure=="dendritic")%>%dplyr::select(c(bottle.number,prey.oc))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$prey.oc)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "prey.oc")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay = create_layout(g, layout = "fr")
lay <- layout.reingold.tilford(g, params=list(root='1')) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()
  

dend.prey<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = prey.oc),size=8) +
  #geom_node_text(aes(label= name),color='black') +
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis_c()+
  # scale_colour_gradientn(colours=viridis(10))+
  #scale_colour_viridis(limits=c(0,1))+
  #scale_size_continuous(range = c(0,1))+
  #scale_color_continuous(type = "viridis")+
  ggtitle("Dendritic Prey Occupancy")


####Dend Pred
nodes = alls%>%filter(structure=="dendritic")%>%dplyr::select(c(bottle.number,pred.oc))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$pred.oc)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "pred.oc")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay = create_layout(g, layout = "fr")
lay <- layout.reingold.tilford(g, params=list(root='1')) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

dend.pred<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = pred.oc),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Dendritic Predator Occupancy")



####Lattice

edges = read_csv("latt.edge.csv")

nodes = alls%>%filter(structure=="lattice")%>%dplyr::select(c(bottle.number,prey.oc))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$prey.oc)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "prey.oc")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay <- layout_on_grid(g, width=5 , height=3) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

latt.prey<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = prey.oc),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Lattice Prey Occupancy")

######B
nodes = alls%>%filter(structure=="lattice")%>%dplyr::select(c(bottle.number,pred.oc))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$pred.oc)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "pred.oc")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay <- layout_on_grid(g, width=5 , height=3) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

latt.pred<-ggraph(g, layout=lay) + 
  #ggraph::scale_color_viridis(limits=c(0,1))+
  geom_edge_link() + 
  geom_node_point(aes(colour = pred.oc),size=8) +
  geom_node_text(aes(label= name),color='black') +
 # ggplot::scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_gradientn(colors=viridis(10))+
  ggtitle("Lattice Predator Occupancy")

plot_grid(dend.prey,dend.pred,latt.prey,latt.pred, ncol=2)

prow <- plot_grid(
  dend.prey + theme(legend.position="none"),
  dend.pred + theme(legend.position="none"),
  latt.prey + theme(legend.position="none"),
  latt.pred,
  #align = 'vh',
  labels = c("A", "B", "C", "D"),
  hjust = -1,
  nrow = 1
)
prow

legend <- get_legend(
  # create some space to the left of the legend
  dend.prey + theme(legend.box.margin = margin(0, 0, 0, 12))
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(prow, legend, rel_widths = c(3, .4))
################################################################################################################################################################
#Ext-Colonization
Ext_col_data

allz<-Ext_col_data%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 175)%>%
  #filter(day  > 175)%>%
  group_by(structure,bottle.number)%>%
  summarise(ext.pred =mean(extinction_prob_pred), ext.prey= mean(extinction_prob_prey),
            col.pred =mean(colonization_prob_pred), col.prey= mean(colonization_prob_prey))


edges = read_csv("dend.edges.csv")

###Dendrtic Ext Prey

nodes = allz%>%filter(structure=="dendritic")%>%dplyr::select(c(bottle.number,ext.prey))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$ext.prey)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "ext.prey")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay = create_layout(g, layout = "fr")
lay <- layout.reingold.tilford(g, params=list(root='1')) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

dend.prey.ext<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = ext.prey),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Dendritic Extinction Prob Prey")


###Dendrtic Col Prey

nodes = allz%>%filter(structure=="dendritic")%>%dplyr::select(c(bottle.number,col.prey))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$col.prey)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "col.prey")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay = create_layout(g, layout = "fr")
lay <- layout.reingold.tilford(g, params=list(root='1')) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

dend.prey.col<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = col.prey),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Dendritic Colonization Prob Prey")


####Dend Pred Ext
nodes = allz%>%filter(structure=="dendritic")%>%dplyr::select(c(bottle.number,ext.pred))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$ext.pred)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "ext.pred")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay = create_layout(g, layout = "fr")
lay <- layout.reingold.tilford(g, params=list(root='1')) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

dend.ext.pred<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = ext.pred),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Dendritic Extinction Prob Predator")

###Dendrtic Col Pred

nodes = allz%>%filter(structure=="dendritic")%>%dplyr::select(c(bottle.number,col.pred))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$col.pred)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "col.pred")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay = create_layout(g, layout = "fr")
lay <- layout.reingold.tilford(g, params=list(root='1')) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

dend.col.pred<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = col.pred),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Dendritic Colonization Prob Predator")


####Lattice

edges = read_csv("latt.edge.csv")
###Lattice Ext.Prey

nodes = allz%>%filter(structure=="lattice")%>%dplyr::select(c(bottle.number,ext.prey))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$ext.prey)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "ext.prey")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay <- layout_on_grid(g, width=5 , height=3) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

latt.ext.prey<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = ext.prey),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Lattice Extinction Prob Prey")

###Lattice Col.Prey
nodes = allz%>%filter(structure=="lattice")%>%dplyr::select(c(bottle.number,col.prey))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$col.prey)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "col.prey")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay <- layout_on_grid(g, width=5 , height=3) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

latt.col.prey<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = col.prey),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Lattice Colonization Prob Prey")

###Lattice Predator Ext
nodes = allz%>%filter(structure=="lattice")%>%dplyr::select(c(bottle.number,ext.pred))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$ext.pred)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "ext.pred")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay <- layout_on_grid(g, width=5 , height=3) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

latt.pred.ext<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = ext.pred),size=8) +
  geom_node_text(aes(label= name),color='black') +
  # scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Lattice Extinction Prob Predator")

###Lattice Predator Col
nodes = allz%>%filter(structure=="lattice")%>%dplyr::select(c(bottle.number,col.pred))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$col.pred)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "col.pred")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay <- layout_on_grid(g, width=5 , height=3) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

latt.pred.col<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = col.pred),size=8) +
  geom_node_text(aes(label= name),color='black') +
  # scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Lattice Colonization Prob Predator")



plot_grid(dend.prey.col,dend.prey.ext,dend.col.pred,dend.ext.pred,latt.col.prey,latt.ext.prey,latt.pred.col,latt.pred.ext, ncol=4)


######################################################################################################################################################################################################
#Ext-Col Ratio

Ext_col_data

allz<-Ext_col_data%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 175)%>%
  #filter(day  > 175)%>%
  group_by(structure,bottle.number)%>%
  drop_na()%>%
  summarise(ext_colon_ratio_pred =mean(ext_colon_ratio_pred), ext_colon_ratio_prey= mean(ext_colon_ratio_prey))


edges = read_csv("dend.edges.csv")

###Dendrtic Ext Prey

nodes = allz%>%filter(structure=="dendritic")%>%dplyr::select(c(bottle.number,ext_colon_ratio_prey))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$ext_colon_ratio_prey)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "ext_colon_ratio_prey")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay = create_layout(g, layout = "fr")
lay <- layout.reingold.tilford(g, params=list(root='1')) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

dend.prey.ext<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = ext_colon_ratio_prey),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Dendritic Extinction-Colonization Ratio Prey")


###Dendrtic Col Prey

nodes = allz%>%filter(structure=="dendritic")%>%dplyr::select(c(bottle.number,ext_colon_ratio_pred))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$ext_colon_ratio_pred)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "ext_colon_ratio_pred")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay = create_layout(g, layout = "fr")
lay <- layout.reingold.tilford(g, params=list(root='1')) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

dend.prey.col<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = ext_colon_ratio_pred),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Dendritic Extinction-Colonization Ratio Predator")

####Lattice

edges = read_csv("latt.edge.csv")
###Lattice Ext.Pred

nodes = allz%>%filter(structure=="lattice")%>%dplyr::select(c(bottle.number,ext_colon_ratio_pred))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$ext_colon_ratio_pred)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "ext_colon_ratio_pred")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay <- layout_on_grid(g, width=5 , height=3) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

latt.ext.pred<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = ext_colon_ratio_pred),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Lattice Extinction-Colonization Ratio Predator")

###Lattice Col.Prey
nodes = allz%>%filter(structure=="lattice")%>%dplyr::select(c(bottle.number,ext_colon_ratio_prey))
nodes<-nodes%>%dplyr::select(-c(structure))
nodes<-cbind(nodes$bottle.number,nodes$ext_colon_ratio_prey)
nodes<-as.data.frame(nodes)
colnames(nodes)<-c("bottle.number", "ext_colon_ratio_prey")
edges<-edges%>%add_column(weight=rep(1))

g = graph_from_data_frame(d=edges,vertices=nodes, directed = FALSE)
lay <- layout_on_grid(g, width=5 , height=3) 
plot(g, layout=lay)
pal<- brewer.pal(11, "Spectral")

ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()

latt.col.prey<-ggraph(g, layout=lay) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = ext_colon_ratio_prey),size=8) +
  geom_node_text(aes(label= name),color='black') +
  #scale_edge_colour_gradientn(limits=c(-1,1), colours = pal, name="Amp")+
  theme(axis.line = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  scale_color_viridis()+
  ggtitle("Lattice Extinction-Colonization Ratio Prey")

plot_grid(dend.prey.col,dend.prey.ext,latt.ext.pred,latt.col.prey,ncol=2)
