# Documenation ------------------------------------------------------------

# R script that compare voxel intensities
# Date: October 30, 2017
# Author: Deepak Sharma
 
# Read data ---------------------------------------------------------------
orig <- read.csv(file = "fmri.csv",sep = ",",header = F)
samp <- orig[1:5,1:10]    # Take short sample of original for fast testing

# Functions defined -------------------------------------------------------
vx <- function(x){
  samp[1,x]
}
vy <- function(y){
  samp[2,y]
}
vz <- function(z){
  samp[3,z]
}
xneighbor <- function(xv){
  if(xv==0 || xv==-1 || xv==1)
    return(T)
  else
    return(F)
}
yneighbor <- function(yv){
  if(yv==0 || yv==-1 || yv==1)
    return(T)
  else
    return(F)
}
zneighbor <- function(zv){
  if(zv==0 || zv==-1 || zv==1)
    return(T)
  else
    return(F)
}

# Generate relations ------------------------------------------------------

voxels <- 1:ncol(samp)
rels <- t(combn(voxels,2))
# image(rels)

# Extract neighbors -------------------------------------------------------

cat(dim(rels))
dim(orig)
View(samp)

neighbors <- matrix(nrow = dim(rels)[1], ncol = dim(rels)[2],data = 0)
neighbors

for(i in 1:dim(rels)[1]){
  if(xneighbor(vx(rels[i,1]) - vx(rels[i,2])) || yneighbor(vy(rels[i,1]) - vy(rels[i,2])) || zneighbor(vz(rels[i,1]) - vz(rels[i,2]))){
    neighbors[i,1] <- rels[i,1]
    neighbors[i,2] <- rels[i,2]   
  }
  
}
neighbors
