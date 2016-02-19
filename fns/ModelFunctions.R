#  Random Forest  -------------------------------------------------------------
RunRF <- function(trainX, target.train, 
                  nTreesRF, tuneRF){
  
  if (tuneRF == 'n'){
    writeLines('Training Random Forest...')
    myfit.rf = randomForest(trainX, target.train, ntree=nTreesRF)
    writeLines('Random Forest training complete!')
  } else if (tuneRF == 'y') {
    writeLines('Training tuned Random Forest...')
    bestmtry = tuneRF(trainX, target.train, ntreeTry=150, 
                      stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE) 
    a = which.min(bestmtry[,2])
    myfit.rf = randomForest(trainX, target.train, ntree=nTreesRF,mtry=bestmtry[a,1])
    writeLines('Random Forest training complete!')
  }
  return(myfit.rf)
}


#  Generalized Boosted Regression Models  -------------------------------------
RunGBM <- function(trainX, target.train, nTreesGBM, train.fraction.gbm, distribution, tuneGBM){
  
  writeLines('Training GBM...')
  
  if (distribution == 'quantile'){
    distrib = list(name='quantile', alpha = .90)
  } else {
    distrib = distribution
  }
  
  myfit.gbm =
    gbm(target.train ~ .,                   # formula
        data = trainX,                      # dataset
        var.monotone = NULL,
        distribution = distrib,        # see the help for other choices
        n.trees = nTreesGBM,                # number of trees
        shrinkage = 0.01,                   # shrinkage or learning rate,
                                            # 0.001 to 0.1 usually work
        interaction.depth = 3,              # 1: additive model, 2: two-way interactions, etc.
        bag.fraction = 0.5,                 # subsampling fraction, 0.5 is probably best
        train.fraction = train.fraction.gbm,# fraction of data for training,
                                            # first train.fraction*N used for training
        n.minobsinnode = 10,                # minimum total weight needed in each node
        cv.folds = 5,                       # do 5-fold cross-validation
        keep.data = TRUE,                   # keep a copy of the dataset with the object
        verbose = FALSE,                    # don't print out progress
        n.cores = 1)                        # use only a single core 
  writeLines('GBM training complete!')
  best.iter = gbm.perf(myfit.gbm, method = tuneGBM, plot.it = T)
  
  output = list('myfit.gbm' = myfit.gbm, 'best.iter' = best.iter)
  return(output)
}


