# R script that compare voxel intensities
# Date: October 30, 2017
# Author: Deepak Sharma
 
# install.packages("combinat")
library(combinat)
orig_dat <- read.table("data.txt")
sam_dat <- orig_dat[1:10,1:20]
View(sam_dat)
cols <- colnames(sam_dat)
rels <- combn2(cols)
cat(nrow(rels),"relations generated for",length(cols), "variables")
combn2(cols)

