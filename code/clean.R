# Project: WindUpdated
# Author: Min Lwin
# Maintainer: Who to complain to <m.lwin@utexas.edu>

# All the potentially messy data cleanup

#  Section 1 - Feature Engineering  -------------------------------------------

input.opt = 1 
input.data = FeatureEng2(train.raw, test.raw, input.opt)           #THIS IS THE STRUCTURE OF TRAINING DATA
trainX = input.data$trainX             # data frame of all Training x's
testX = input.data$testX                # data frame of all Test/Scoring x's
trainY = input.data$trainY
testY = input.data$testY
amp100 = input.data$amp100
rm(input.data, input.opt)

#remove outliers
rows.outliers = OutlierRows(trainY, amp100)
for (i in 1:10){
  trainX[[i]] = trainX[[i]][-rows.outliers[[i]], ]
  trainY[[i]]$TARGETVAR = trainY[[i]]$TARGETVAR[-rows.outliers[[i]]]
  trainX[[i]] = trainX[[i]][c(-1,-2), ]   #remove lagged NA rows
  trainY[[i]][[1]] = trainY[[i]][[1]][c(-1,-2)]
}


#  remove NA rows
for (i in 1:10){
  x=data.frame(trainY[[i]]$TARGETVAR)
  any = subset(x,is.na(x))
  rows = as.numeric(row.names(any))
  
  if (length(rows) > 0) {
    trainX[[i]] = trainX[[i]][-rows,]
    trainY[[i]]$TARGETVAR = x[-rows,]
  }
}

rm(test.raw, train.raw, amp100,i,rows.outliers,x,any,rows)

writeLines('Data has been cleaned, raw data removed')
# readline("Press <return to continue ") 

#save(testX, testY, trainX, trainY, file = "cleanedData.RData")




