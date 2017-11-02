# Documenation ------------------------------------------------------------

# R script to compare voxel intensities
# Date: October 30, 2017
# Author: Deepak Sharma

# Basic clean up ----------------------------------------------------------

cat('\f')     # Clear console
rm(list=ls()) # Clear all variables

# Start stopwatch ---------------------------------------------------------

start_time <- proc.time()

# Read data ---------------------------------------------------------------

orig <- read.csv(file = "fmri.csv", sep = ",", header = F)
# orig <- orig[1:5,1:10]    # Take short sample of original for fast testing

# Functions defined -------------------------------------------------------

# Get voxel's cartesian coordinates ---------------------------------------

vx <- function(x){
  orig[1,x]
}
vy <- function(y){
  orig[2,y]
}
vz <- function(z){
  orig[3,z]
}

# Check neighborhood in cartesian coordinates -----------------------------

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

voxels <- 1:ncol(orig)
rels <- t(combn(voxels,2))

# Extract neighbors -------------------------------------------------------

neighbors <- matrix(nrow = dim(rels)[1], ncol = dim(rels)[2],data = NA)
for(i in 1:dim(rels)[1]){
  if(xneighbor(vx(rels[i,1]) - vx(rels[i,2])) && yneighbor(vy(rels[i,1]) - vy(rels[i,2])) && zneighbor(vz(rels[i,1]) - vz(rels[i,2]))){
    neighbors[i,1] <- rels[i,1]
    neighbors[i,2] <- rels[i,2]   
  }
  
}

# Data summary ------------------------------------------------------------

cat("Total relations = ",length(neighbors),"\n")
true_nhood <- cat("Neighborhood relations = ",sum(!is.na(neighbors)))

# Data visualization ------------------------------------------------------

image(neighbors)

# Stop stopwatch ----------------------------------------------------------

time_taken <- proc.time() - start_time 
cat("Time taken = ",unname(time_taken[1]),"ms\n")
