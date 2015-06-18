#  System Functions


#  Cartesian to Polar Converter  ---------------------------------------------- 

Cart2Polar <-function(U,V){
  Angles=matrix(0,length(U),1)
  for (i in 1:length(U)){
    ComplexNumber=complex(real=U[i],imaginary=V[i])  
    Angles[i]=Arg(ComplexNumber)        
  }      
  return(as.numeric(Angles))  # in radians
}


#  Read Input Data  ----------------------------------------------------------- 

ReadData <-function(Task){
#   Reads CSV files for this week and loads them into Training and Test data
  
  ## Read Data
  TrainingData=vector('list',10)          # Save Data Frame into List
  #OriginalTrainY=vector('list',10)          # List
  TestData=vector('list',10)          # List
  #OriginalTotalTime = vector('list', 10)
  for (i in 1:10){
    #read in training data
    FileNameA=paste("Task",as.character(Task),"_W_Zone",as.character(i),".csv",sep="")
    DataA = read.csv(FileNameA, header = TRUE, stringsAsFactors = F)
    
    #read in evalutaion data
    FileNameB=paste("TaskExpVars",as.character(Task),"_W_Zone",as.character(i),".csv",sep="")
    DataB = read.csv(FileNameB, header = TRUE, stringsAsFactors = F)
    
    TrainingData[[i]]=DataA[2:7] #all the training data
    TestData[[i]]=DataB[2:6]   #all the test data used for scoring
  }
  trainLength<<-nrow(DataA)
  testLength<<-nrow(DataB)
  
  
  #setwd(workingdir)
  writeLines('CSV files loaded ...\n')
  Output=list("TrainingData"=TrainingData,"TestData"=TestData)
  return(Output)
  
}


#  Convert Time Stamps  -------------------------------------------------------

ConvertTime <- function(rawData){
  #  Searches and replaces factor names
  
  for (i in 1:length(rawData)){
    loc = rawData[[i]]
    
    for (t in 0:23){
      time = paste(" ",as.character(t),":00", sep="")
      time.idx = which(grepl(time,loc$TIMESTAMP))
      
      loc$time[time.idx] = time
    }
    
    loc$month = NA
    for (t in c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')) {
      mth1 = paste("2012", t, sep="")
      mth2 = paste("2013", t, sep="")
      
      time.idx = which(grepl(mth1, loc$TIMESTAMP) | grepl(mth2, loc$TIMESTAMP))
      loc$month[time.idx] = month.abb[as.numeric(t)]  
    }
    
    loc$time = as.factor(loc$time)
    loc$month = factor(loc$month, levels = month.abb)
    rawData[[i]] = loc
  }
  
  writeLines('Time stamps converted into factors for "time" and "month".\n')
  output=rawData
  return(output)
}

#  Visualize Predictor Distributions  -----------------------------------------

PredictorDist <- function (testX.local, trainX.local) {
  
  for (i in 1:10){
    
    scor = data.frame(testX.local[[i]]$Amp100)
    train = data.frame(trainX.local[[i]]$Amp100)
    
    names(scor)[1] = 'Amp100'
    names(train)[1] = 'Amp100'
    
    scor$type = 'scoring'
    train$type = 'training'
    xdens = rbind(scor, train)
    
    print(ggplot(xdens, aes(Amp100, fill = type)) + geom_density(alpha = 0.5))
  }
}

#  Reject Outliers in Each Farm  ----------------------------------------------

OutlierRows <- function (trainY, trainX.local){
  #par(mfrow=c(1,2))
  rows.outliers = vector('list',10)
  for (i in 1:10){
    x = trainX.local[[i]] #trainX.local
    y = trainY[[i]]$TARGETVAR #trainY
    #plot(x,y)
    up.scl = c(1, 1, 1, 1, 1, 1, .5, .6, .9, 1)
    up.shft = c(4, 4, 4, 4, 4, 4, 5, 4.5, 4.5, 3)
    low.scl = c(.8, .8, .6, 1, 1, .8, .5, .5, .8, .7)
    low.shft = c(10, 10, 10, 10, 9, 9, 11, 11.5, 10.5, 9)
    upperlim = 1/(1+exp(up.scl[i]*(-x+up.shft[i]))) + .05
    lowerlim = 1/(1+exp(low.scl[i]*(-x+low.shft[i]))) -.2
    
    up.rows = which(y>upperlim)
    low.rows = which(y<lowerlim)
    rows.outliers[[i]] = c(up.rows, low.rows)
    
#     trainX.local[[i]] = trainX.local[[i]][-rows.outliers, ]
#     trainY[[i]]$TARGETVAR = trainY[[i]]$TARGETVAR[-rows.outliers] 
  }
  return(rows.outliers)
}


