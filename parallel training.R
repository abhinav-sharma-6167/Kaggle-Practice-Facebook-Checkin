resolutionx<-0.1 #grid size in km (x dimension)
resolutiony<-0.1 #grid size in km (y dimension)
marginx<-0.05 #km added to each grid ONLY for training in x axis
marginy<-0.05 #km added to each grid ONLY for training in y axis
minOccurrences<-5 #Filter places with less frequency
nCores=3 #Number of cores used by the script


#Training grid limits. Note that some examples may be part of two grids at the same time. No problem for training.
#Margins are used to add more training examples to the grid (expanding it in the x and y axis)
gridLimitsExpanded<-function(x,y)
{
  return(c(((x-1)*resolutionx)-marginx,(x*resolutionx)+marginx,((y-1)*resolutiony)-marginy,(y*resolutiony)+marginy))
}

#Testing grid limits. Here each example is only part of one grid.
gridLimits<-function(x,y)
{
  limits<-(c((x-1)*resolutionx,x*resolutionx,(y-1)*resolutiony,y*resolutiony))
  if (limits[1]==0)
    limits[1]<--0.01
  if (limits[3]==0)
    limits[3]<--0.01
  return(limits)
}

#Size of the grid based on the x and y resolution
gridSize<-function()
{
  return (c((10/resolutionx),(10/resolutiony)))
}

#Summary function for caret based on MAP@3 accuracy
mapSummary <- function (data, lev = NULL, model = NULL, grid=NULL) {
  probs<-data[,-which(names(data) %in% c('pred','obs'))]
  pprob<-t(apply(probs,1,function(x) names((sort(x,decreasing = TRUE))[1:3])))
  out<-computeMap3(pprob,data[,'obs'])
  names(out) <- paste("MAP (",grid,")",sep="")
  #names(out) <- "MAP3"
  out
}

#Compute map@3 accuracy
computeMap3<-function(predicted,real)
{
  m<-matrix(real,ncol = 3,nrow=length(real))
  m<-(m==predicted)
  v<-vector(length = length(real))
  v[m[,1]==TRUE]<-1
  v[m[,2]==TRUE]<-1/2
  v[m[,3]==TRUE]<-1/3
  return(mean(v))
}

#Computes new features for this dataset
computeNewVariables<-function(dataset)
{
  dataset$hour<-(dataset$time/60) %% 24
  dataset$day<-(dataset$time/(60*24)) %% 7
  dataset$week<-(dataset$time/(60*24*7))%%52
  dataset$month<-(dataset$time/(60*24*31))%%12
  dataset$year<-(dataset$time/(60*24*365))
  return(dataset)
}

#Main function. Receives as parameter the number of grids to make the train/test process
#This function will parallize the work to make it faster.
trainTestGrids<-function(nGrids)
{ 
  #Load libraries
  library(caret)
  library(data.table)
  library(doMC)
  registerDoMC(cores = nCores) #Configure the number of cores you want to use
  
  #Compute all posible grids
  set.seed(8)
  possibleGrids<-expand.grid(c(1:gridSize()[1]),c(1:gridSize()[2]))
  #Select randomly only the number of grids the user wants
  selectedGrids<-possibleGrids[sample(nrow(possibleGrids),nGrids),]
 
  #Read the data from disk
  print("Loading data...")
  train <- fread("../input/train.csv", integer64 = "character",showProgress=FALSE)
  train<-data.table(train,key="row_id")
  
  #Prepare datasets (parallelized)
  print("Subsetting data...")
  datasets<-foreach (i=1:nrow(selectedGrids))%dopar%
  {
    #Get all the examples inside the expanded (with margins) grid
    limitsExpanded<-gridLimitsExpanded(selectedGrids[i,1],selectedGrids[i,2])
    gridExpanded<-train[train$x >= limitsExpanded[1] & train$x <= limitsExpanded[2] & train$y >= limitsExpanded[3] & train$y <= limitsExpanded[4],]
    
    #Copute the features for all the examples
    gridExpanded<-computeNewVariables(gridExpanded)
    
    #From the extended grid, subset only the elements that fall inside the real grid (without margins)
    limits<-gridLimits(selectedGrids[i,1],selectedGrids[i,2])
    grid<-gridExpanded[gridExpanded$x >= limits[1] & gridExpanded$x <= limits[2] & gridExpanded$y >= limits[3] & gridExpanded$y <= limits[4],]
    
    #Take 90% for training and 10% for testing based on time
    grid<-grid[with(grid, order(time)), ]
    indexTrain<-1:as.integer(nrow(grid)*0.9)
    grid_train<-grid[indexTrain,]
    grid_test<-grid[-indexTrain,]
    
    #Add to train set, examples from the expanded grid. We must be careful and add only examples
    expanded_examples<-gridExpanded[!grid]
    expanded_examples<-expanded_examples[expanded_examples$time<=max(grid_train$time),]
    
    #Add to the train set, the examples that fall into the margins
    grid_train<-rbind(grid_train,expanded_examples)
    
    #Filter examples (only in the training set), with less than minOcurrences (by place_id)
    ids<-names(table(grid_train$place_id))[table(grid_train$place_id)>minOccurrences]
    grid_train<-grid_train[grid_train$place_id %in% ids,]
    
    list(grid_train=grid_train,grid_test=grid_test)
  } 
  
  #Train models
  print("Training models...")
  models<-foreach(i=1:nrow(selectedGrids))%dopar%
  {   
    train<-datasets[[i]][['grid_train']]
    y<-paste("P",train$place_id,sep="")
    x<-train[,c('place_id','row_id','time') := NULL]
    set.seed(7)
    #Here you can play with other classifiers (caret supports lots of them)
    train(x,y,method="rf",ntrees=50,
          trControl=trainControl(method="none",trim=TRUE,classProbs = TRUE,allowParallel = FALSE),
          tuneGrid = expand.grid(mtry=3),preProc=c("center","scale"))   
  }
  
  #Validation phase
  sum<-foreach (i=1:nrow(selectedGrids)) %dopar%
  { 
    test<-datasets[[i]][['grid_test']]
    y<-paste("P",test$place_id,sep="")
    x<-test[,c('place_id','row_id','time') := NULL]
    p<-predict(models[[i]],type="prob",x)
    p$obs<-y
    mapSummary(p,grid=i)
  }
  #Print results. Map3 error by grid and mean error
  print(unlist(sum))
  print("Mean MAP:")
  print(mean(unlist(sum)))
}


#Call to the main function
trainTestGrids(3)

