# Read peer data and produce network diagrams for default institution
# Default institution = University of Notre Dame, unitid = 158080

rm(list=ls())

# Load needed libraries
library(tidyverse)
library(odbc)
library(igraph)

# set this for reproducibility of results
set.seed(1001)

# Open a connection
con <- dbConnect(odbc::odbc(), "ipeds")

qry <- 'SELECT i.UNITID AS Unitid,
               i.INSTNM AS InstitutionName,
               i.STABBR AS [State],
               i.CONTROL AS ControlCode,
               i.HLOFFER AS HighestLevelOffering
         FROM  dbo.HD2014 i'

# " INNER JOIN dbo.WatchersOfUNL h ON i.UNITID = h.Unitid"


# the command gets passed to a response object
# this object passes back data and/or an error
# should the SQL fail
resp <- dbSendQuery(con,qry)

# the fetch statement pulls the data into a dataframe
# named adm
insts <- dbFetch(resp)

# this clears the result so another command can be passed
dbClearResult(resp)

qry <- "SELECT c.UNITID AS Unitid, c.CGUNITID AS PeerUnitid FROM dbo.CUSTOMCGIDS2014 c"

# "SELECT c.UNITID AS Unitid, c.CGUNITID AS PeerUnitid FROM dbo.CUSTOMCGIDS2014 c INNER JOIN dbo.WatchersOfUNL h ON c.UNITID = h.UNITID
#          INNER JOIN dbo.WatchersOfUNL I ON c.CGUNITID = i.UNITID"

resp <- dbSendQuery(con,qry)

# peers query
peers <- dbFetch(resp)

dbClearResult(resp)

# this removes the response and qry objects from memory
rm(resp, qry)

# disconnect from the database
dbDisconnect(con)

peers.unl <- peers %>%
  filter(Unitid == 181464)

# peers.unl <- subset(peers, Unitid==181464)
# universities <- subset(insts, CarnegieCode==15)
edges <- subset(peers, Unitid %in% insts$Unitid | PeerUnitid %in% insts$Unitid)
nodes <- subset(insts, Unitid %in% edges$Unitid | Unitid %in% edges$PeerUnitid)

# create igraph object from nodes and edges
g <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)

# Set edge names to institution names rather than unitid's
V(g)$label <- V(g)$InstitutionName
V(g)$group <- 3
V(g)[V(g)$name %in% peers.unl$PeerUnitid]$group <- 2
V(g)[V(g)$name=="181464"]$group <- 1

#plot the network
# l <- layout_with_kk(g)
l <- layout_with_fr(g)
plot(g, edge.arrow.size=.2,
     vertex.size=5,
     vertex.label.cex=.5,
     vertex.label='',
     vertex.color=c("#000099","#CCFFFF","#FFFFFF")[V(g)$group],
     layout=l)

# cx <- cluster_edge_betweenness(g)
# dendPlot(cx, mode="hclust")
# plot(cx, g)
# 
# edges2 <- subset(peers, Unitid == 152080 | PeerUnitid==152080)
# nodes2 <- subset(insts, Unitid %in% edges2$Unitid | Unitid %in% edges2$PeerUnitid)
# 
# # create igraph object from nodes and edges
# g2 <- graph_from_data_frame(d=edges2, vertices=nodes2, directed=TRUE)
# 
# # Set edge names to institution names rather than unitid's
# V(g2)$label <- V(g2)$InstitutionName
# V(g2)$group <- 3
# V(g2)[V(g2)$name %in% peers.nd$PeerUnitid]$group <- 2
# V(g2)[V(g2)$name=="152080"]$group <- 1
# 
# #plot the network
# # l <- layout_with_kk(g2)
# l <- layout_with_fr(g2)
# plot(g2, edge.arrow.size=.2,
#      vertex.size=5,
#      vertex.label.cex=.5,
#      vertex.label='',
#      vertex.color=c("#000099","#CCFFFF","#FFFFFF")[V(g2)$group],
#      layout=l)
# 
# 
# 
# # Make counts
# # Create aggregates
# # Raw Peer Score = Peer List Counts
# # Aspirant Estimation (Reciprocity) -- % Reciprocity in Peer List
# # Weighted Peer Score 1 -- Raw Score * Reciprocal Raw Score
# # Weighted Peer Score 2 -- Raw Score - Segment Mean
# 
# peer.score <- data.frame(summarize(group_by(peers, Unitid), count = n()))

