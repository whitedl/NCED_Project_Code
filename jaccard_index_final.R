#Calculates the jaccard index 
#06/07/2019
#BY: D. White


library(rgdal)
library(sf)
library(sp)
library(mapview)
library(maptools)
library(raster)
library(rgeos)

setwd("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/tabulate/raster")

#-----------------------------------------------------------------------------
setwd("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED")
#Read Geodatabasere
readOGR(dsn = "NCED_CUCED_2018.gdb", verbose = FALSE, drop_unsupported_fields = TRUE, dropNULLGeometries = FALSE)
ogrListLayers("NCED_CUCED_2018.gdb")
#Import Feature Class
c_ce <- sf::st_read(dsn = "NCED_CUCED_2018.gdb", layer = "SON_CE_data")

as(sf, "Spatial")
as(st_sfc(c_ce), "Spatial")
sf:::as_Spatial(c_ce)

#-----------------------------------------------------------------------------
  # Some parameters
  cnty <- "SON"
  county <- "Sonoma"
  
  # fetch the data
  # Clemson CE data
  c_layer <- paste0(cnty, "_CE_data.shp")
  c_ce <- readOGR(c_layer)
  # Zero Buffer to Clean Up Potential Topology Issues
  c_ce <- gBuffer(c_ce, byid=TRUE, width=0)
  plot(c_ce)
  summary(c_ce)
  projection(c_ce)
  
  # NCED data
  n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
  n_ce <- readOGR(n_layer)
  # Zero Buffer to Clean Up Potential Topology Issues
  n_ce <- gBuffer(n_ce, byid=TRUE, width=0)
  plot(n_ce)
  summary(n_ce)
  projection(n_ce)
  
  # Vector Intersect
  SON_CE_NCED_INT<-intersect(c_ce,n_ce)
  png('SON_CE_NCED_INT.png')
  plot(SON_CE_NCED_INT, main ="SON_CE & NCED Intersect")
  dev.off()
  projection(SON_CE_NCED_INT)
  
  # Vector Union
  SON_CE_NCED_UNION <- union(c_ce,n_ce)
  png("SON_CE_NCED_UNION.png")
  plot(SON_CE_NCED_UNION)
  dev.off()
  projection(SON_CE_NCED_UNION)
  
  # Create Raster Intersect 
  # Mask Raster
  SON.CE.Ras.int <- raster()
  # Set Projection/Extent based on Vector Intersect
  extent(SON.CE.Ras.int) <- extent(SON_CE_NCED_INT)
  # Check 
  summary(SON.CE.Ras.int)
  projection(SON.CE.Ras.int)
  # Set Resolution
  res(SON.CE.Ras.int) <- 10
  # Generate Vector Intersect Raster
  SON.r.int <-rasterize(SON_CE_NCED_INT, SON.CE.Ras.int)
  # Check
  summary(SON.r.int)
  projection(SON.r.int)
  #plot(SON.r.int, main='SON Intersect Raster')
  # Classify all Raster Values to 1
  SON.r.int <- reclassify(SON.r.int, c(1,1500,1))
  # Check
  summary(SON.r.int)
  plot(SON.r.int, main='SON Intersect Raster')
  
  
  # Create Raster Union
  # Mask Raster
  SON.CE.Ras.union <- raster()
  # Set Projection/Extent based on Vector Intersect
  extent(SON.CE.Ras.union) <- extent(SON_CE_NCED_UNION)
  # Check 
  summary(SON.CE.Ras.union)
  projection(SON.CE.Ras.union)
  # Set Resolution
  res(SON.CE.Ras.union) <- 10
  # Generate Vector Intersect Raster
  SON.r.union <-rasterize(SON_CE_NCED_UNION, SON.CE.Ras.union)
  # Check
  summary(SON.r.union)
  projection(SON.r.union)
  #plot(SON.r.union, main = 'SON Union Raster')
  # Classify all Raster Values to 1
  SON.r.union <- reclassify(SON.r.union, c(1,1500,1))
  # Check
  summary(SON.r.union)
  plot(SON.r.union, main = 'SON Union Raster')

  
  # Create a multi-plot figure
  pdf(file = 'SON_Plots.pdf', width = 8, height = 11)
  #dev.new(width=10, height=10, unit="in")
  par(mfrow=c(2,2))
  plot(SON_CE_NCED_INT, main ="SON CE & NCED Intersect")
  plot(SON_CE_NCED_UNION, main='SON CE & NCED Union')
  plot(SON.r.int, main='SON Intersect Raster', legend = FALSE)
  plot(SON.r.union, main = 'SON Union Raster', legend = FALSE)
  dev.off()
  
    
  # Calculate the Jaccard Index
 jaccard_SON <- cellStats(SON.r.int, 'sum' )/cellStats(SON.r.union, 'sum' )

#-----------------------------------------------------------------------------
# Boulder
# Some parameters
cnty <- "BLD"
county <- "Boulder"
    
# fetch the data
# Clemson CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
BLD_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(BLD_CE_NCED_INT)
projection(BLD_CE_NCED_INT)

# Vector Union
BLD_CE_NCED_UNION <- union(c_ce,n_ce)
plot(BLD_CE_NCED_UNION)
projection(BLD_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
BLD.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(BLD.CE.Ras.int) <- extent(BLD_CE_NCED_INT)
# Check 
summary(BLD.CE.Ras.int)
projection(BLD.CE.Ras.int)
# Set Resolution
res(BLD.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
BLD.r.int <-rasterize(BLD_CE_NCED_INT, BLD.CE.Ras.int)
# Check
summary(BLD.r.int)
projection(BLD.r.int)
#plot(BLD.r.int, main='BLD Intersect Raster')
# Classify all Raster Values to 1
BLD.r.int <- reclassify(BLD.r.int, c(1,1500,1))
# Check
summary(BLD.r.int)
plot(BLD.r.int, main='BLD Intersect Raster')


# Create Raster Union
# Mask Raster
BLD.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(BLD.CE.Ras.union) <- extent(BLD_CE_NCED_UNION)
# Check 
summary(BLD.CE.Ras.union)
projection(BLD.CE.Ras.union)
# Set Resolution
res(BLD.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
BLD.r.union <-rasterize(BLD_CE_NCED_UNION, BLD.CE.Ras.union)
# Check
summary(BLD.r.union)
projection(BLD.r.union)
#plot(BLD.r.union, main = 'BLD Union Raster')
# Classify all Raster Values to 1
BLD.r.union <- reclassify(BLD.r.union, c(1,1600,1))
# Check
summary(BLD.r.union)
plot(BLD.r.union, main = 'BLD Union Raster')

# Create a multi-plot figure
pdf(file = 'BLD_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(BLD_CE_NCED_INT, main ="BLD CE & NCED Intersect")
plot(BLD_CE_NCED_UNION, main='BLD CE & NCED Union')
plot(BLD.r.int, main='BLD Intersect Raster', legend = FALSE)
plot(BLD.r.union, main = 'BLD Union Raster', legend = FALSE)
dev.off()



# Calculate the Jaccard Index
jaccard_BLD <- cellStats(BLD.r.int, 'sum' )/cellStats(BLD.r.union, 'sum' )

#-----------------------------------------------------------------------------
# Charleston
# Some parameters
cnty <- "CHS"
county <- "Charleston"

# fetch the data
# ClemSON CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
CHS_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(CHS_CE_NCED_INT)
projection(CHS_CE_NCED_INT)

# Vector Union
CHS_CE_NCED_UNION <- union(c_ce,n_ce)
plot(CHS_CE_NCED_UNION)
projection(CHS_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
CHS.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(CHS.CE.Ras.int) <- extent(CHS_CE_NCED_INT)
# Check 
summary(CHS.CE.Ras.int)
projection(CHS.CE.Ras.int)
# Set Resolution
res(CHS.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
CHS.r.int <-rasterize(CHS_CE_NCED_INT, CHS.CE.Ras.int)
# Check
summary(CHS.r.int)
projection(CHS.r.int)
#plot(CHS.r.int, main='CHS Intersect Raster')
# Classify all Raster Values to 1
CHS.r.int <- reclassify(CHS.r.int, c(1,1500,1))
# Check
summary(CHS.r.int)
plot(CHS.r.int, main='CHS Intersect Raster')


# Create Raster Union
# Mask Raster
CHS.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(CHS.CE.Ras.union) <- extent(CHS_CE_NCED_UNION)
# Check 
summary(CHS.CE.Ras.union)
projection(CHS.CE.Ras.union)
# Set Resolution
res(CHS.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
CHS.r.union <-rasterize(CHS_CE_NCED_UNION, CHS.CE.Ras.union)
# Check
summary(CHS.r.union)
projection(CHS.r.union)
#plot(CHS.r.union, main = 'CHS Union Raster')
# Classify all Raster Values to 1
CHS.r.union <- reclassify(CHS.r.union, c(1,1500,1))
# Check
summary(CHS.r.union)
plot(CHS.r.union, main = 'CHS Union Raster')

# Create a multi-plot figure
pdf(file = 'CHS_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(CHS_CE_NCED_INT, main ="CHS CE & NCED Intersect")
plot(CHS_CE_NCED_UNION, main='CHS CE & NCED Union')
plot(CHS.r.int, main='CHS Intersect Raster', legend = FALSE)
plot(CHS.r.union, main = 'CHS Union Raster', legend = FALSE)
dev.off()


# Calculate the Jaccard Index
jaccard_CHS <- cellStats(CHS.r.int, 'sum' )/cellStats(CHS.r.union, 'sum' )


#-----------------------------------------------------------------------------
# Albemarle
# Some parameters

cnty <- "ALB"
county <- "Albemarle"

# fetch the data
# Clemson CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce,main = "ALB Vector CE")
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce,main = "ALB Vector NCED")
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
ALB_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(ALB_CE_NCED_INT,main = "ALB Vector Intersect")
projection(ALB_CE_NCED_INT)

# Vector Union
ALB_CE_NCED_UNION <- union(c_ce,n_ce)
plot(ALB_CE_NCED_UNION,main = "ALB Vector Union")
projection(ALB_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
ALB.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(ALB.CE.Ras.int) <- extent(ALB_CE_NCED_INT)
# Check 
summary(ALB.CE.Ras.int)
projection(ALB.CE.Ras.int)
# Set Resolution
res(ALB.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
ALB.r.int <-rasterize(ALB_CE_NCED_INT, ALB.CE.Ras.int)
# Check
summary(ALB.r.int)
projection(ALB.r.int)
#plot(ALB.r.int, main='ALB Intersect Raster')
# Classify all Raster Values to 1
ALB.r.int <- reclassify(ALB.r.int, c(1,1500,1))
# Check
summary(ALB.r.int)
plot(ALB.r.int, main='ALB Intersect Raster')


# Create Raster Union
# Mask Raster
ALB.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(ALB.CE.Ras.union) <- extent(ALB_CE_NCED_UNION)
# Check 
summary(ALB.CE.Ras.union)
projection(ALB.CE.Ras.union)
# Set Resolution
res(ALB.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
ALB.r.union <-rasterize(ALB_CE_NCED_UNION, ALB.CE.Ras.union)
# Check
summary(ALB.r.union)
projection(ALB.r.union) 
#plot(ALB.r.union, main = 'ALB Union Raster')
# Classify all Raster Values to 1
ALB.r.union <- reclassify(ALB.r.union, c(1,1500,1))
# Check
summary(ALB.r.union)
plot(ALB.r.union, main='ALB Union Raster')

# Create a multi-plot figure
pdf(file = 'ALB_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(ALB_CE_NCED_INT, main ="ALB CE & NCED Intersect")
plot(ALB_CE_NCED_UNION, main='ALB CE & NCED Union')
plot(ALB.r.int, main='ALB Intersect Raster', legend = FALSE)
plot(ALB.r.union, main = 'ALB Union Raster', legend = FALSE)
dev.off()

# Calculate the Jaccard Index
jaccard_ALB <- cellStats(ALB.r.int, 'sum' )/cellStats(ALB.r.union, 'sum' )

#-----------------------------------------------------------------------------
  # Douglas
  # Some parameters
  
  cnty <- "DGL"
county <- "Douglas"

#gdb <- "NCED_CUCED_2017.gdb"
# see what the layer names are

# Prints feature classess in GDB
#st_layers(gdb)

# fetch the data
# ClemSON CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
DGL_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(DGL_CE_NCED_INT)
projection(DGL_CE_NCED_INT)

# Vector Union
DGL_CE_NCED_UNION <- union(c_ce,n_ce)
plot(DGL_CE_NCED_UNION)
projection(DGL_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
DGL.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(DGL.CE.Ras.int) <- extent(DGL_CE_NCED_INT)
# Check 
summary(DGL.CE.Ras.int)
projection(DGL.CE.Ras.int)
# Set Resolution
res(DGL.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
DGL.r.int <-rasterize(DGL_CE_NCED_INT, DGL.CE.Ras.int)
# Check
summary(DGL.r.int)
projection(DGL.r.int)
#plot(DGL.r.int, main='DGL Intersect Raster')
# Classify all Raster Values to 1
DGL.r.int <- reclassify(DGL.r.int, c(1,1500,1))
# Check
summary(DGL.r.int)
plot(DGL.r.int, main='DGL Intersect Raster')


# Create Raster Union
# Mask Raster
DGL.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(DGL.CE.Ras.union) <- extent(DGL_CE_NCED_UNION)
# Check 
summary(DGL.CE.Ras.union)
projection(DGL.CE.Ras.union)
# Set Resolution
res(DGL.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
DGL.r.union <-rasterize(DGL_CE_NCED_UNION, DGL.CE.Ras.union)
# Check
summary(DGL.r.union)
projection(DGL.r.union)
#plot(DGL.r.union, main = 'DGL Union Raster')
# Classify all Raster Values to 1
DGL.r.union <- reclassify(DGL.r.union, c(1,1500,1))
# Check
summary(DGL.r.union)
plot(DGL.r.union, main = 'DGL Union Raster')

# Create a multi-plot figure
pdf(file = 'DGL_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(DGL_CE_NCED_INT, main ="DGL CE & NCED Intersect")
plot(DGL_CE_NCED_UNION, main='DGL CE & NCED Union')
plot(DGL.r.int, main='DGL Intersect Raster', legend = FALSE)
plot(DGL.r.union, main = 'DGL Union Raster', legend = FALSE)
dev.off()

# Calculate the Jaccard Index
jaccard_DGL <- cellStats(DGL.r.int, 'sum' )/cellStats(DGL.r.union, 'sum' )


#----------------------------------------------------------------------------
  # Greenville
  # Some parameters
  
  cnty <- "GVL"
county <- "Greenville"

#gdb <- "NCED_CUCED_2017.gdb"
# see what the layer names are

# Prints feature classess in GDB
#st_layers(gdb)

# fetch the data
# ClemDGL CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
GVL_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(GVL_CE_NCED_INT)
projection(GVL_CE_NCED_INT)

# Vector Union
GVL_CE_NCED_UNION <- union(c_ce,n_ce)
plot(GVL_CE_NCED_UNION)
projection(GVL_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
GVL.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(GVL.CE.Ras.int) <- extent(GVL_CE_NCED_INT)
# Check 
summary(GVL.CE.Ras.int)
projection(GVL.CE.Ras.int)
# Set Resolution
res(GVL.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
GVL.r.int <-rasterize(GVL_CE_NCED_INT, GVL.CE.Ras.int)
# Check
summary(GVL.r.int)
projection(GVL.r.int)
#plot(GVL.r.int, main='GVL Intersect Raster')
# Classify all Raster Values to 1
GVL.r.int <- reclassify(GVL.r.int, c(1,1500,1))
# Check
summary(GVL.r.int)
plot(GVL.r.int, main='GVL Intersect Raster')


# Create Raster Union
# Mask Raster
GVL.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(GVL.CE.Ras.union) <- extent(GVL_CE_NCED_UNION)
# Check 
summary(GVL.CE.Ras.union)
projection(GVL.CE.Ras.union)
# Set Resolution
res(GVL.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
GVL.r.union <-rasterize(GVL_CE_NCED_UNION, GVL.CE.Ras.union)
# Check
summary(GVL.r.union)
projection(GVL.r.union)
#plot(GVL.r.union, main = 'GVL Union Raster')
# Classify all Raster Values to 1
GVL.r.union <- reclassify(GVL.r.union, c(1,1500,1))
# Check
summary(GVL.r.union)
plot(GVL.r.union, main = 'GVL Union Raster')

# Create a multi-plot figure
pdf(file = 'GVL_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(GVL_CE_NCED_INT, main ="GVL CE & NCED Intersect")
plot(GVL_CE_NCED_UNION, main='GVL CE & NCED Union')
plot(GVL.r.int, main='GVL Intersect Raster', legend = FALSE)
plot(GVL.r.union, main = 'GVL Union Raster', legend = FALSE)
dev.off()

# Calculate the Jaccard Index
jaccard_GVL <- cellStats(GVL.r.int, 'sum' )/cellStats(GVL.r.union, 'sum' )

#-----------------------------------------------------------------------------
  # Mesa
  # Some parameters
  
  cnty <- "MES"
county <- "Mesa"

#gdb <- "NCED_CUCED_2017.gdb"
# see what the layer names are

# Prints feature classess in GDB
#st_layers(gdb)

# fetch the data
# ClemDGL CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
MES_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(MES_CE_NCED_INT)
projection(MES_CE_NCED_INT)

# Vector Union
MES_CE_NCED_UNION <- union(c_ce,n_ce)
plot(MES_CE_NCED_UNION)
projection(MES_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
MES.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(MES.CE.Ras.int) <- extent(MES_CE_NCED_INT)
# Check 
summary(MES.CE.Ras.int)
projection(MES.CE.Ras.int)
# Set Resolution
res(MES.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
MES.r.int <-rasterize(MES_CE_NCED_INT, MES.CE.Ras.int)
# Check
summary(MES.r.int)
projection(MES.r.int)
#plot(MES.r.int, main='MES Intersect Raster')
# Classify all Raster Values to 1
MES.r.int <- reclassify(MES.r.int, c(1,1500,1))
# Check
summary(MES.r.int)
plot(MES.r.int, main='MES Intersect Raster')


# Create Raster Union
# Mask Raster
MES.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(MES.CE.Ras.union) <- extent(MES_CE_NCED_UNION)
# Check 
summary(MES.CE.Ras.union)
projection(MES.CE.Ras.union)
# Set Resolution
res(MES.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
MES.r.union <-rasterize(MES_CE_NCED_UNION, MES.CE.Ras.union)
# Check
summary(MES.r.union)
projection(MES.r.union)
#plot(MES.r.union, main = 'MES Union Raster')
# Classify all Raster Values to 1
MES.r.union <- reclassify(MES.r.union, c(1,1500,1))
# Check
summary(MES.r.union)
plot(MES.r.union, main = 'MEs Union Raster')

# Create a multi-plot figure
pdf(file = 'MES_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(MES_CE_NCED_INT, main ="MES CE & NCED Intersect")
plot(MES_CE_NCED_UNION, main='MES CE & NCED Union')
plot(MES.r.int, main='MES Intersect Raster', legend = FALSE)
plot(MES.r.union, main = 'MES Union Raster', legend = FALSE)
dev.off()


# Calculate the Jaccard Index
jaccard_MES <- cellStats(MES.r.int, 'sum' )/cellStats(MES.r.union, 'sum' )


#-----------------------------------------------------------------------------
  # Sacramento
  # Some parameters
  
  cnty <- "SAC"
county <- "Sacramento"

#gdb <- "NCED_CUCED_2017.gdb"
# see what the layer names are

# Prints feature classess in GDB
#st_layers(gdb)

# fetch the data
# ClemDGL CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
SAC_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(SAC_CE_NCED_INT)
projection(SAC_CE_NCED_INT)

# Vector Union
SAC_CE_NCED_UNION <- union(c_ce,n_ce)
plot(SAC_CE_NCED_UNION)
projection(SAC_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
SAC.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(SAC.CE.Ras.int) <- extent(SAC_CE_NCED_INT)
# Check 
summary(SAC.CE.Ras.int)
projection(SAC.CE.Ras.int)
# Set Resolution
res(SAC.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
SAC.r.int <-rasterize(SAC_CE_NCED_INT, SAC.CE.Ras.int)
# Check
summary(SAC.r.int)
projection(SAC.r.int)
#plot(SAC.r.int, main='SAC Intersect Raster')
# Classify all Raster Values to 1
SAC.r.int <- reclassify(SAC.r.int, c(1,1500,1))
# Check
summary(SAC.r.int)
plot(SAC.r.int, main='SAC Intersect Raster')


# Create Raster Union
# Mask Raster
SAC.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(SAC.CE.Ras.union) <- extent(SAC_CE_NCED_UNION)
# Check 
summary(SAC.CE.Ras.union)
projection(SAC.CE.Ras.union)
# Set Resolution
res(SAC.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
SAC.r.union <-rasterize(SAC_CE_NCED_UNION, SAC.CE.Ras.union)
# Check
summary(SAC.r.union)
projection(SAC.r.union)
#plot(SAC.r.union, main = 'SAC Union Raster')
# Classify all Raster Values to 1
SAC.r.union <- reclassify(SAC.r.union, c(1,1500,1))
# Check
summary(SAC.r.union)
plot(SAC.r.union, main = 'SAC Union Raster')

# Create a multi-plot figure
pdf(file = 'SAC_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(SAC_CE_NCED_INT, main ="SAC CE & NCED Intersect")
plot(SAC_CE_NCED_UNION, main='SAC CE & NCED Union')
plot(SAC.r.int, main='SAC Intersect Raster', legend = FALSE)
plot(SAC.r.union, main = 'SAC Union Raster', legend = FALSE)
dev.off()


# Calculate the Jaccard Index
jaccard_SAC <- cellStats(SAC.r.int, 'sum' )/cellStats(SAC.r.union, 'sum' )

#-----------------------------------------------------------------------------
# Lebanon
# Some parameters

cnty <- "LEB"
county <- "Lebanon"

#gdb <- "NCED_CUCED_2017.gdb"
# see what the layer names are

# Prints feature classess in GDB
#st_layers(gdb)

# fetch the data
# ClemDGL CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
LEB_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(LEB_CE_NCED_INT)
projection(LEB_CE_NCED_INT)

# Vector Union
LEB_CE_NCED_UNION <- union(c_ce,n_ce)
plot(LEB_CE_NCED_UNION)
projection(LEB_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
LEB.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(LEB.CE.Ras.int) <- extent(LEB_CE_NCED_INT)
# Check 
summary(LEB.CE.Ras.int)
projection(LEB.CE.Ras.int)
# Set Resolution
res(LEB.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
LEB.r.int <-rasterize(LEB_CE_NCED_INT, LEB.CE.Ras.int)
# Check
summary(LEB.r.int)
projection(LEB.r.int)
#plot(LEB.r.int, main='LEB Intersect Raster')
# Classify all Raster Values to 1
LEB.r.int <- reclassify(LEB.r.int, c(1,1500,1))
# Check
summary(LEB.r.int)
plot(LEB.r.int, main='LEB Intersect Raster')


# Create Raster Union
# Mask Raster
LEB.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(LEB.CE.Ras.union) <- extent(LEB_CE_NCED_UNION)
# Check 
summary(LEB.CE.Ras.union)
projection(LEB.CE.Ras.union)
# Set Resolution
res(LEB.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
LEB.r.union <-rasterize(LEB_CE_NCED_UNION, LEB.CE.Ras.union)
# Check
summary(LEB.r.union)
projection(LEB.r.union)
#plot(LEB.r.union, main = 'LEB Union Raster')
# Classify all Raster Values to 1
LEB.r.union <- reclassify(LEB.r.union, c(1,1500,1))
# Check
summary(LEB.r.union)
plot(LEB.r.union, main = 'LEB Union Raster')

# Create a multi-plot figure
pdf(file = 'LEB_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(LEB_CE_NCED_INT, main ="LEB CE & NCED Intersect")
plot(LEB_CE_NCED_UNION, main='LEB CE & NCED Union')
plot(LEB.r.int, main='LEB Intersect Raster', legend = FALSE)
plot(LEB.r.union, main = 'LEB Union Raster', legend = FALSE)
dev.off()


# Calculate the Jaccard Index
jaccard_LEB <- cellStats(LEB.r.int, 'sum' )/cellStats(LEB.r.union, 'sum' )

#-----------------------------------------------------------------------------
# Loudon
# Some parameters

cnty <- "LDN"
county <- "Loudon"

#gdb <- "NCED_CUCED_2017.gdb"
# see what the layer names are

# Prints feature classess in GDB
#st_layers(gdb)

# fetch the data
# ClemDGL CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
LDN_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(LDN_CE_NCED_INT)
projection(LDN_CE_NCED_INT)

# Vector Union
LDN_CE_NCED_UNION <- union(c_ce,n_ce)
plot(LDN_CE_NCED_UNION)
projection(LDN_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
LDN.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(LDN.CE.Ras.int) <- extent(LDN_CE_NCED_INT)
# Check 
summary(LDN.CE.Ras.int)
projection(LDN.CE.Ras.int)
# Set Resolution
res(LDN.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
LDN.r.int <-rasterize(LDN_CE_NCED_INT, LDN.CE.Ras.int)
# Check
summary(LDN.r.int)
projection(LDN.r.int)
#plot(LDN.r.int, main='LDN Intersect Raster')
# Classify all Raster Values to 1
LDN.r.int <- reclassify(LDN.r.int, c(1,1500,1))
# Check
summary(LDN.r.int)
plot(LDN.r.int, main='LDN Intersect Raster')


# Create Raster Union
# Mask Raster
LDN.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(LDN.CE.Ras.union) <- extent(LDN_CE_NCED_UNION)
# Check 
summary(LDN.CE.Ras.union)
projection(LDN.CE.Ras.union)
# Set Resolution
res(LDN.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
LDN.r.union <-rasterize(LDN_CE_NCED_UNION, LDN.CE.Ras.union)
# Check
summary(LDN.r.union)
projection(LDN.r.union)
#plot(LDN.r.union, main = 'LDN Union Raster')
# Classify all Raster Values to 1
LDN.r.union <- reclassify(LDN.r.union, c(1,1500,1))
# Check
summary(LDN.r.union)
plot(LDN.r.union, main = 'LDN Union Raster')

# Create a multi-plot figure
pdf(file = 'LDN_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(LDN_CE_NCED_INT, main ="LDN CE & NCED Intersect")
plot(LDN_CE_NCED_UNION, main='LDN CE & NCED Union')
plot(LDN.r.int, main='LDN Intersect Raster', legend = FALSE)
plot(LDN.r.union, main = 'LDN Union Raster', legend = FALSE)
dev.off()


# Calculate the Jaccard Index
jaccard_LDN <- cellStats(LDN.r.int, 'sum' )/cellStats(LDN.r.union, 'sum' )

#-----------------------------------------------------------------------------
# York
# Some parameters

cnty <- "YRK"
county <- "York"

#gdb <- "NCED_CUCED_2017.gdb"
# see what the layer names are

# Prints feature classess in GDB
#st_layers(gdb)

# fetch the data
# ClemDGL CE data
c_layer <- paste0(cnty, "_CE_data.shp")
c_ce <- readOGR(c_layer)
plot(c_ce)
summary(c_ce)
projection(c_ce)
# Zero Buffer to Clean Up Potential 
c_ce <- gBuffer(c_ce, byid=TRUE, width=0)

# NCED data
n_layer <- paste0("NCED_timeframe_", cnty, ".shp")
n_ce <- readOGR(n_layer)
plot(n_ce)
summary(n_ce)
projection(n_ce)
n_ce <- gBuffer(n_ce, byid=TRUE, width=0)

# Vector Intersect
YRK_CE_NCED_INT<-intersect(c_ce,n_ce)
plot(YRK_CE_NCED_INT)
projection(YRK_CE_NCED_INT)

# Vector Union
YRK_CE_NCED_UNION <- union(c_ce,n_ce)
plot(YRK_CE_NCED_UNION)
projection(YRK_CE_NCED_UNION)

# Create Raster Intersect 
# Mask Raster
YRK.CE.Ras.int <- raster()
# Set Projection/Extent based on Vector Intersect
extent(YRK.CE.Ras.int) <- extent(YRK_CE_NCED_INT)
# Check 
summary(YRK.CE.Ras.int)
projection(YRK.CE.Ras.int)
# Set Resolution
res(YRK.CE.Ras.int) <- 10
# Generate Vector Intersect Raster
YRK.r.int <-rasterize(YRK_CE_NCED_INT, YRK.CE.Ras.int)
# Check
summary(YRK.r.int)
projection(YRK.r.int)
#plot(YRK.r.int, main='YRK Intersect Raster')
# Classify all Raster Values to 1
YRK.r.int <- reclassify(YRK.r.int, c(1,1500,1))
# Check
summary(YRK.r.int)
plot(YRK.r.int, main='YRK Intersect Raster')


# Create Raster Union
# Mask Raster
YRK.CE.Ras.union <- raster()
# Set Projection/Extent based on Vector Intersect
extent(YRK.CE.Ras.union) <- extent(YRK_CE_NCED_UNION)
# Check 
summary(YRK.CE.Ras.union)
projection(YRK.CE.Ras.union)
# Set Resolution
res(YRK.CE.Ras.union) <- 10
# Generate Vector Intersect Raster
YRK.r.union <-rasterize(YRK_CE_NCED_UNION, YRK.CE.Ras.union)
# Check
summary(YRK.r.union)
projection(YRK.r.union)
#plot(YRK.r.union, main = 'YRK Union Raster')
# Classify all Raster Values to 1
YRK.r.union <- reclassify(YRK.r.union, c(1,1500,1))
# Check
summary(YRK.r.union)
plot(YRK.r.union, main = 'YRK Union Raster')

# Create a multi-plot figure
pdf(file = 'YRK_Plots.pdf', width = 8, height = 11)
#dev.new(width=10, height=10, unit="in")
par(mfrow=c(2,2))
plot(YRK_CE_NCED_INT, main ="YRK CE & NCED Intersect")
plot(YRK_CE_NCED_UNION, main='YRK CE & NCED Union')
plot(YRK.r.int, main='YRK Intersect Raster', legend = FALSE)
plot(YRK.r.union, main = 'YRK Union Raster', legend = FALSE)
dev.off()


# Calculate the Jaccard Index
jaccard_YRK <- cellStats(YRK.r.int, 'sum' )/cellStats(YRK.r.union, 'sum' )




#-----------------Create a DataFrame----------------------

jaccard.final <- as.data.frame(matrix(ncol = 2,nrow = 11))
colnames(jaccard.final) <- c("county","jaccard_index")
jaccard.final <- rbind(jaccard_ALB,jaccard_BLD,jaccard_CHS,jaccard_SON,jaccard_GVL,jaccard_MES,jaccard_SAC,jaccard_DGL,jaccard_LEB,jaccard_LDN,jaccard_YRK)
#colnames(jaccard.final) <- c("county","jacca rd_index")
write.csv(jaccard.final, file = "Jaccard_Index_10.csv")
#as.data.frame(jaccard.final)
#colnames(jaccard.final) <- c("county","jaccard_index")

#--------------------------------------------------------------------
  # INCOMPLETE LOOP IS NOT READY TO RUN
#--------------------------------------------------------------------  

  df_list <- list()
cnty_list <- c("DGL","BLD")
county_list <- c("DGLoma", "Boulder")

for (i  in 1:2) {
  cnty <- cnty_list[i]
  county <- county_list[i]
  
  # fetch the data
  # ClemDGL CE data
  c_layer <- paste0(cnty, "_CE_data.shp")
  c_ce <- readOGR(c_layer)
  plot(c_ce)
  summary(c_ce)
  projection(c_ce)
  
  # NCED data
  n_layer <- paste0("NCED_timeframe_", county, ".shp")
  n_ce <- readOGR(n_layer)
  plot(n_ce)
  summary(n_ce)
  projection(n_ce)
  
  # Vector Intersect
  DGL_CE_NCED_INT<-intersect(c_ce,n_ce)
  plot(DGL_CE_NCED_INT)
  projection(DGL_CE_NCED_INT)
  
  # Vector Union
  DGL_CE_NCED_UNION <- union(c_ce,n_ce)
  plot(DGL_CE_NCED_UNION)
  projection(DGL_CE_NCED_UNION)
  
  # Create Raster Intersect 
  # Mask Raster
  DGL.CE.Ras.int <- raster()
  # Set Projection/Extent based on Vector Intersect
  extent(DGL.CE.Ras.int) <- extent(DGL_CE_NCED_INT)
  # Check 
  summary(DGL.CE.Ras.int)
  projection(DGL.CE.Ras.int)
  # Set Resolution
  res(DGL.CE.Ras.int) <- 10
  # Generate Vector Intersect Raster
  DGL.r.int <-rasterize(DGL_CE_NCED_INT, DGL.CE.Ras.int)
  # Check
  summary(DGL.r.int)
  projection(DGL.r.int)
  plot(DGL.r.int)
  # Classify all Raster Values to 1
  DGL.r.int <- reclassify(DGL.r.int, c(1,89,1))
  # Check
  summary(DGL.r.int)
  plot(DGL.r.int)
  
  
  # Create Raster Union
  # Mask Raster
  DGL.CE.Ras.union <- raster()
  # Set Projection/Extent based on Vector Intersect
  extent(DGL.CE.Ras.union) <- extent(DGL_CE_NCED_UNION)
  # Check 
  summary(DGL.CE.Ras.union)
  projection(DGL.CE.Ras.union)
  # Set Resolution
  res(DGL.CE.Ras.union) <- 10
  # Generate Vector Intersect Raster
  DGL.r.union <-rasterize(DGL_CE_NCED_UNION, DGL.CE.Ras.union)
  # Check
  summary(DGL.r.union)
  projection(DGL.r.union)
  plot(DGL.r.union)
  # Classify all Raster Values to 1
  DGL.r.union <- reclassify(DGL.r.union, c(1,1500,1))
  # Check
  summary(DGL.r.union)
  plot(DGL.r.union)
  
  # Calculate the Jaccard Index
  df_list[[i]] <- cellStats(DGL.r.int, 'sum' )/cellStats(DGL.r.union, 'sum' )
  
}

