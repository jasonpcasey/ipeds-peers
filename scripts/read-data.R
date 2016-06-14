# Read peer data and produce network diagrams for default institution
# Default institution = University of Notre Dame, unitid = 158080

# Load needed libraries
library(igraph)
library(dplyr)
library(reshape2)
library(RODBC)


# Open a connection
con <- odbcConnect("OSPIR-DEV")

# Retrieve Institutional Data
institutions <- sqlQuery(con, "SELECT [Unitid]
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

# Retrieve Peer Lists
# Contains the unitid of the selecting institution and the unitid's of its selections
peer.lists <- sqlFetch(con, "extract.DFRPeerLists")

#close connection
close(con)
rm(con)

# Join the files
# Make counts
# Create aggregates