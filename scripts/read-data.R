# Read peer data and produce network diagrams for default institution
# Default institution = University of Notre Dame, unitid = 158080

# Load needed libraries
library(igraph)
library(dplyr)
library(reshape2)
library(RODBC)

# set this for reproducibility of results
set.seed(1001)

# https://www.dropbox.com/s/bvuzpwfqitcucfq/CompGroups.csv?dl=0
# https://www.dropbox.com/s/7vvws09fkvg8dub/Insts.csv?dl=0

# Open a connection
con <- odbcConnect("OSPIR-DEV")

# Retrieve institutional data as NODES (the objects in the igraph)
# Contains unitid's and several institution attributes
# nodes <- read.table(file='https://www.dropbox.com/s/7vvws09fkvg8dub/Insts.csv?dl=0',
#                     header=TRUE,
#                     sep=",",
#                     quote="\"",
#                     skip=0,
#                     row.names=NULL,
#                     stringsAsFactors=FALSE,
#                     fileEncoding="utf-8")

insts <- sqlQuery(con, "SELECT [Unitid]
                         ,[InstitutionName]
                         ,[StateAbbreviation] AS [State]
                         ,[RegionLiteral]
                         ,[ControlCode]
                         ,[ControlLiteral]
                         ,[HighestLevelOfferingLiteral]
                         ,[HighestDegreeOfferingLiteral]
                         ,[ReligiousAffiliationLiteral]
                         ,[LocaleLiteral] AS [Locale]
                         ,[CarnegieBasic2010Code] AS [CarnegieCode]
                         ,[CarnegieBasic2010Literal] AS [Carnegie]
                         ,[CbsaLiteral] AS [CBSA]
                         ,[CountyName]
                         ,[Longitude]
                         ,[Latitude]
                         ,[ExpandedComparisonGroupLiteral] AS [ComparisonGroup]
                         FROM  [staged].[Institutions]
                         WHERE [ControlCode] IN (1,2)  AND
                         [LevelCode] = 1 AND
                         [CarnegieBasic2010Code] > 0 AND
                         [IsClosed] = 0")

write.table(insts,
            file="insts.csv",
            append=FALSE,
            quote=TRUE,
            sep=",",
            row.names=FALSE,       # If you want to create row numbers, set this to TRUE
            col.names=TRUE,
            na = "")               # Set the missing values to blanks

# Retrieve peer lists (the EDGES or connections between NODES)
# Contains the unitid of the selecting institution and the unitid's of its selections
# Because there can be no edges without corresponding nodes, I've used WHERE clauses to ensure
#    that the same criteria are used for unitid and peer unitid as were used in the NODES query
peers <- sqlQuery(con, "SELECT [UnitID] AS [Unitid]
                         ,[PeerUnitID] AS [PeerUnitid]
                         FROM extract.DFRPeerLists WHERE [UnitID] IN
                         (SELECT [Unitid]
                         FROM  [staged].[Institutions]
                         WHERE [ControlCode] IN (1,2)  AND
                         [LevelCode] = 1 AND
                         [CarnegieBasic2010Code] > 0 AND
                         [IsClosed] = 0)
                         AND [PeerUnitID] IN
                         (SELECT [Unitid]
                         FROM  [staged].[Institutions]
                         WHERE [ControlCode] IN (1,2)  AND
                         [LevelCode] = 1 AND
                         [CarnegieBasic2010Code] > 0 AND
                         [IsClosed] = 0)")

write.table(peers,
            file="peers.csv",
            append=FALSE,
            quote=TRUE,
            sep=",",
            row.names=FALSE,       # If you want to create row numbers, set this to TRUE
            col.names=TRUE,
            na = "")               # Set the missing values to blanks

#close connection
close(con)
rm(con)

peers.nd <- subset(peers, Unitid==152080)
universities <- subset(insts, CarnegieCode==15)
edges <- subset(peers, Unitid %in% universities$Unitid | PeerUnitid %in% universities$unitid)
nodes <- subset(insts, Unitid %in% edges$Unitid | Unitid %in% edges$PeerUnitid)

# create igraph object from nodes and edges
g <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)

# Set edge names to institution names rather than unitid's
V(g)$label <- V(g)$InstitutionName
V(g)$group <- 3
V(g)[V(g)$name %in% peers.nd$PeerUnitid]$group <- 2
V(g)[V(g)$name=="152080"]$group <- 1

#plot the network
# l <- layout_with_kk(g)
l <- layout_with_fr(g)
plot(g, edge.arrow.size=.2,
     vertex.size=5,
     vertex.label.cex=.5,
     vertex.label='',
     vertex.color=c("#000099","#CCFFFF","#FFFFFF")[V(g)$group],
     layout=l)

cx <- cluster_edge_betweenness(g)
dendPlot(cx, mode="hclust")
plot(cx, g)

edges2 <- subset(peers, Unitid == 152080 | PeerUnitid==152080)
nodes2 <- subset(insts, Unitid %in% edges2$Unitid | Unitid %in% edges2$PeerUnitid)

# create igraph object from nodes and edges
g2 <- graph_from_data_frame(d=edges2, vertices=nodes2, directed=TRUE)

# Set edge names to institution names rather than unitid's
V(g2)$label <- V(g2)$InstitutionName
V(g2)$group <- 3
V(g2)[V(g2)$name %in% peers.nd$PeerUnitid]$group <- 2
V(g2)[V(g2)$name=="152080"]$group <- 1

#plot the network
# l <- layout_with_kk(g2)
l <- layout_with_fr(g2)
plot(g2, edge.arrow.size=.2,
     vertex.size=5,
     vertex.label.cex=.5,
     vertex.label='',
     vertex.color=c("#000099","#CCFFFF","#FFFFFF")[V(g2)$group],
     layout=l)



# Make counts
# Create aggregates
# Raw Peer Score = Peer List Counts
# Aspirant Estimation (Reciprocity) -- % Reciprocity in Peer List
# Weighted Peer Score 1 -- Raw Score * Reciprocal Raw Score
# Weighted Peer Score 2 -- Raw Score - Segment Mean


