# Documenation ------------------------------------------------------------

# R script to compare voxel intensities
# Date: October 30, 2017
# Author: Deepak Sharma

# Basic clean up ----------------------------------------------------------

cat('\f')     # Clear console
rm(list=ls()) # Clear all variables

# Configurations ----------------------------------------------------------

profiler_range <- 2:1288
vox_num <- profiler_range

# Code profiling ----------------------------------------------------------

time_taken <- rep(NA,length(profiler_range))
total_rels <- rep(NA,length(profiler_range))
true_nhood <- rep(NA,length(profiler_range))
true_nhood_p <- rep(NA,length(profiler_range))

for(j in profiler_range){
  
  # Start stopwatch ---------------------------------------------------------
  
  start_time <- proc.time()
  
  # Read data ---------------------------------------------------------------
  
  orig <- read.csv(file = "fmri.csv", sep = ",", header = F)
  orig <- orig[1:3,1:j]    # Take short sample of original for fast testing
  
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
    if(xneighbor(vx(rels[i,1]) - vx(rels[i,2])) 
       && yneighbor(vy(rels[i,1]) - vy(rels[i,2])) 
       && zneighbor(vz(rels[i,1]) - vz(rels[i,2]))){
      neighbors[i,1] <- rels[i,1]
      neighbors[i,2] <- rels[i,2]   
    }
  }
  
  # Data summary ------------------------------------------------------------
  
  cat("Step no. ",j-1,"/",length(profiler_range),"\r")
  total_rels[j] <- length(neighbors)
  true_nhood[j] <- sum(!is.na(neighbors))
  true_nhood_p[j] <- true_nhood[j] / total_rels[j] * 100

  # Stop stopwatch ----------------------------------------------------------
  
  time_taken[j] <- proc.time() - start_time 
}

# Data visualization ------------------------------------------------------

# image(neighbors)
plot(total_rels[2:length(time_taken)],type = 'l',xlab = "Number of voxels",ylab = "",col="gray",main = "Time complexity - Order of growth")
par(new=T)
plot(time_taken[2:length(time_taken)],type = 'l',col="blue",axes = F,xlab = "",ylab = "")
par(new=T)
plot(true_nhood[2:length(time_taken)],type = 'l',col="green",axes = F,xlab = "",ylab = "")
par(new=T)
plot(true_nhood_p[3:length(time_taken)],type = 'l',col="red",axes = F,xlab = "",ylab = "")
legend(y.intersp = 0.2, par('usr')[1],par('usr')[4],
       bty='n',c("No.of total relations","Time taken","No. of true neighbors","Percentage of true neighbors"),
       col = c("gray","blue","green","red"),pch = c(15,15,15,15))

# Write output ------------------------------------------------------------

write.csv(time_taken,"Time_taken_data.csv")
write.csv(total_rels,"total_rels.csv")
write.csv(true_nhood,"true_nhood.csv")
write.csv(true_nhood_p,"true_nhood_p.csv")
