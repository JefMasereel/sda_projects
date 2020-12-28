
##################################################################
## Util functions for Project 1 SDA - by Jef Masereel, r0631651 ##
##################################################################

## NOTE ON CODE REFERENCES
# some code was written by reference to the provided exercise solutions from earlier sessions.
# besides this, some online sources such as Quick-R and Stackoverflow were consulted at times.


# tool to compute permutations
# ref: https://www.r-bloggers.com/learning-r-permutations-and-combinations-with-base-r/
perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}


# tool to process clustering results
mycomp <- function(faults,
                   clusters,
                   nb_classes=3,
                   classes=c('Z_Scratch','K_Scratch','Other_Faults'),
                   class_prev=c(190,390,673)){
  
  # check all possible cluster interpretations
  prm <- perm(1:nb_classes)
  
  # init counters
  tp <- matrix(0,dim(prm)[1],nb_classes)
  fp <- c(rep(length(clusters),dim(prm)[1]))
  
  for (i in 1:dim(prm)[1]){           # iterate over possible permutation sets
    for (j in 1:length(clusters)){    # iterate over observations
      for (k in 1:nb_classes){        # iterate over classes
        
        if (clusters[[j]]==prm[i,k] & faults[[j]]==classes[k]){
          tp[i,k] <- tp[i,k] + 1
          fp[i]   <- fp[i] - 1
        }
      }
    }
  } 
  
  # init output matrix
  ss <- matrix(0,dim(prm)[1],nb_classes+2)
  colnames(ss) <- c('hr1','hr2','hr3','mr','bac')
  
  # compute results
  for (i in 1:dim(prm)[1]){
    for (k in 1:nb_classes){
      # hit percentages (TPR)
      ss[i,k] <- tp[i,k]/class_prev[k]*100
    }
    # miss percentage (FDR)
    ss[i,nb_classes+1] <- fp[i]/length(clusters)*100
    # balanced accuracy (see report for citation)
    ss[i,nb_classes+2] <- sum(ss[i,1:3])/3
  }
  return(list(ss,prm))
}
