# Date: October 30, 2017
# Author: Deepak Sharma
 
# install.packages("combinat")
library(combinat)
colss <- c("V1","V2","V3","V4")
rels <- combn2(colss)
cat(nrow(rels),"relations generated for",length(colss), "variables")
