# Read peer data and produce network diagrams for default institution
# Default institution = University of Notre Dame, unitid = 158080

rm(list=ls())

# Load needed libraries
library(odbc)
library(igraph)
library(stringr)
library(tidyverse)

# set global variables
odbcString <- "ipeds"

# set this for reproducibility of results
set.seed(1001)

grabData <- function(dbString, queryString)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(), dbString)
  
  response <- dbSendQuery(connection, queryString)
  tbl <- dbFetch(response)
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  return(as_tibble(tbl))
}

qry <- 'SELECT i.UNITID AS Unitid,
               i.INSTNM AS InstitutionName,
               i.STABBR AS [State],
               i.CONTROL AS ControlCode,
               i.HLOFFER AS HighestLevelOffering,
               i.DFRCUSCG AS ListType
         FROM  dbo.HD2014 i'

insts <- grabData(odbcString, qry) %>%
  mutate(ListType = recode(ListType,
                           `1`='Custom',
                           `2`='Generated',
                           .default = 'Unknown'))

qry <- "SELECT c.UNITID AS Unitid, c.CGUNITID AS PeerUnitid FROM dbo.CUSTOMCGIDS2014 c"

peers <- grabData(odbcString, qry)

rm(qry)

unl.peers <- peers %>%
  filter(Unitid==181464) %>%
  inner_join(insts, by=c('PeerUnitid' = 'Unitid')) %>%
  mutate(Unitid = PeerUnitid) %>%
  select(Unitid, InstitutionName, State, ListType)

unl.peers

unl.monitors <- peers %>%
  filter(PeerUnitid==181464) %>%
  group_by(Unitid) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  inner_join(insts) %>%
  select(Unitid, InstitutionName, State, ListType)

unl.monitors

mutual <- unl.monitors %>%
  intersect(unl.peers)

mutual

unl <- insts %>%
  filter(Unitid==181464) %>%
  select(Unitid, InstitutionName, State, ListType)

base.group <- unl.monitors %>%
  union(unl.peers) %>%
  union(unl)

base.group

net.nodes <- peers %>%
  inner_join(base.group[,c('Unitid')]) %>%
  inner_join(insts, by=c('PeerUnitid' = 'Unitid')) %>%
  mutate(Unitid = PeerUnitid) %>%
  select(Unitid, InstitutionName, State, ListType) %>%
  union(base.group) %>%
  mutate(vgroup = 5,
         vgroup = ifelse(Unitid %in% unl.monitors$Unitid, 4, vgroup),
         vgroup = ifelse(Unitid %in% unl.peers$Unitid, 3, vgroup),
         vgroup = ifelse(Unitid %in% mutual$Unitid, 2, vgroup),
         vgroup = ifelse(Unitid == 181464, 1, vgroup))

net.nodes

# create edges for graph -- this contains the id pairs for each relatiohship.
net.edges <- base.group %>%
  select(Unitid) %>%
  inner_join(peers) %>%
  mutate(type = 'Other',
         type = ifelse(Unitid %in% unl.monitors$Unitid, 'Monitors', type),
         type = ifelse(Unitid %in% unl.peers$Unitid, 'Peers', type),
         type = ifelse(Unitid %in% mutual$Unitid, 'Mutual Peers', type),
         type = ifelse(Unitid == 181464, 'UNL', type))

net.edges

# create igraph object from nodes and edges
g <- graph_from_data_frame(d=net.edges, vertices=net.nodes, directed=TRUE)

#plot the network
l <- layout_with_kk(g)
# l <- layout_with_fr(g)


V(g)$label <- net.nodes$InstitutionName
V(g)$label[!net.nodes$vgroup < 4] <- NA

V(g)$group <- net.nodes$vgroup

E(g)$color <- '#e6e6e6'
E(g)[net.edges$Unitid==181464]$color <- 'black'

#plot the network
par(family='sans')
plot(g, edge.arrow.size=.2,
     vertex.size=5,
     vertex.label.cex=.65,
     vertex.color=c("#cc0000","#ff6666","#ffcccc","#737373","#e6e6e6")[V(g)$group],
     vertex.frame.color=c("#cc0000","#ff6666","#ffcccc","#737373","#e6e6e6")[V(g)$group],
     layout=l)
legend(x=-1.5, y=-0.9, pch=21,
       legend=c('UNL','Mutual Peer','Peer','Monitor','Other'),
       col=c("#cc0000","#ff6666","#ffcccc","#737373","#e6e6e6"),
       pt.bg=c("#cc0000","#ff6666","#ffcccc","#737373","#e6e6e6"))

# net.unl <- g - E(g)[E(g)$type == 'Monitors' | E(g)$type == 'Other']
# net.unl <- net.unl - V(net.unl)[V(net.unl)$vgroup > 3]
# 
# plot(net.unl, edge.arrow.size=.2,
#      vertex.size=5,
#      vertex.label.cex=.65,
#      vertex.color=c("#cc0000","#ff6666","#ffcccc","#737373","#e6e6e6")[V(g)$group],
#      vertex.frame.color=c("#cc0000","#ff6666","#ffcccc","#737373","#e6e6e6")[V(g)$group],
#      layout=l)
# legend(x=-1.5, y=-0.9, pch=21,
#        legend=c('UNL','Mutual Peer','Peer','Monitor','Other'),
#        col=c("#cc0000","#ff6666","#ffcccc","#737373","#e6e6e6"),
#        pt.bg=c("#cc0000","#ff6666","#ffcccc","#737373","#e6e6e6"))

overlap <- net.nodes %>%
  filter(vgroup %in% c(2,3,4)) %>%
  mutate(Peer = ifelse(Unitid %in% unl.peers$Unitid, 1, 0),
         Monitor = ifelse(Unitid %in% unl.monitors$Unitid, 1, 0)) %>%
  group_by(Peer, Monitor) %>%
  summarise(Counts = n())

overlap

library(VennDiagram)

grid.newpage()
draw.pairwise.venn(area1=10,
                   area2=25,
                   cross.area=3,
                   fontfamily = 'sans',
                   category=c('Peers', 'Monitors'),
                   fill = c("red", "light grey"),
                   alpha=rep(0.5, 2),
                   lty=rep('blank', 2),
                   rotation.degree = 45,
                   euler.d=TRUE)

# library(venneuler)

# plot(venneuler(overlap))
