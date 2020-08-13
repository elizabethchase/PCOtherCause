splinecindex <- function(data, formula, nrep, n_folds, times, k){
  nobs <- nrow(data)
  foldid <- matrix(NA,nobs,nrep)
  for (i in 1:nrep) {
    foldid[,i] <- sample(rep(1:n_folds,length.out = nobs))
  }
  mycindex <- rep(0, length(times))
  
  for (i in 1:nrep){
    for (j in 1:n_folds){
      splinemod <- flexsurvspline(formula, data=data[foldid[,i]!=j, ], k=k, scale="hazard")
      testdat <- data[foldid[,i]==j, ]
      mysum <- summary(splinemod, newdata=testdat, type="survival", ci=FALSE, se=FALSE)
      
      predprobs <- matrix(NA, nrow=nrow(testdat), ncol=length(times))
      for (l in 1:nrow(testdat)){
        predprobs[l,] <- mysum[[l]]$est[mysum[[l]]$time %in% times]
      }
      
      test <- pec::cindex(object=predprobs, formula = formula, data = testdat, eval.times = times)
      
      mycindex <- mycindex + test$AppCindex$matrix
    }
  }
  
  finalcindex <- mycindex/(nrep*n_folds)
  return(finalcindex)
}