# Read peer data and produce network diagrams for default institution
# Default institution = University of Notre Dame, unitid = 158080

# Load needed libraries
library(igraph)
library(dplyr)
library(reshape2)
library(RODBC)


# Open a connection
con <- odbcConnect("OSPIR-DEV")

# Retrieve institutional data as NODES (the objects in the igraph)
# Contains unitid's and several institution attributes
nodes <- sqlQuery(con, "SELECT [Unitid]
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

# Retrieve peer lists (the EDGES or connections between NODES)
# Contains the unitid of the selecting institution and the unitid's of its selections
# Because there can be no edges without corresponding nodes, I've used WHERE clauses to ensure
#    that the same criteria are used for unitid and peer unitid as were used in the NODES query
edges <- sqlQuery(con, "SELECT [UnitID] AS [Unitid]
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

#close connection
close(con)
rm(con)

# create igraph object from nodes and edges
g <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)

# Set edge names to institution names rather than unitid's
V(g)$name <- V(g)$InstitutionName

#plot the network
plot(g)

# total <- merge(data frameA,data frameB,by=c("ID","Country"))

# Make counts
# Create aggregates
# Raw Peer Score = Peer List Counts
# Aspirant Estimation (Reciprocity) -- % Reciprocity in Peer List
# Weighted Peer Score 1 -- Raw Score * Reciprocal Raw Score
# Weighted Peer Score 2 -- Raw Score - Segment Mean
