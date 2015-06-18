#  Description  ---------------------------------------------------------------
# This is the main file for the project
# It should do very little except call the other files
# Code outline:
#   load/loadPrev
#     load raw data
#   clean
#     combine train and test X into one data frame
#     calculate features - all 10 farms
#     select features based on choice
#     split train and test
#     remove outliers from train - each farm
#     save train and test
#   doCV
#     train models
#     predict on cv
#     report cv rmse
#     save emp error quantiles
#   doPrevScore
#     train models
#     predict on prev test
#     report pinball loss
#   doSubmit
#     train models
#     predict on test
#     calculate quantiles
#     save output