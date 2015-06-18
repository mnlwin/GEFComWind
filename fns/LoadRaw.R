LoadRaw <- function (Task) {  
  
  ptm = proc.time()
  
  currwd = getwd()
  setwd(paste(currwd,"/data/",Task,sep=""))
  
  # Read in data from working directory and store
  
  SourceData = ReadData(Task)
  train.raw = SourceData$TrainingData
  test.raw = SourceData$TestData
  rm(SourceData) 
  
  
  #convert timestmap to factors for month and time
  writeLines('Converting time stamps into factor variables...\n')
  train.raw = ConvertTime(train.raw)  
  test.raw = ConvertTime(test.raw)
  
  # Create variables for previous week's data: training and scoring
  
  prevTrain.raw = vector('list', 10)
  prevScoring.raw = vector('list',10)
  
  for (i in 1:10){
    prevTrain.raw[[i]] = train.raw[[i]][1:prevTrain.length,]
    prevScoring.raw[[i]] = train.raw[[i]][(prevTrain.length +1): trainLength,]
  }
  
  rawData = list('train.raw' = train.raw, 'test.raw' = test.raw, 
                 'prevTrain.raw' = prevTrain.raw, 'prevScoring.raw' = prevScoring.raw)
  #writeLines('Raw data loaded as "train.raw", "test.raw", "prevTrain.raw", and "prevScoring.raw"')
  
  setwd(currwd)
  return(rawData)
  
  
}

