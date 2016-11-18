library(readr) # CSV file I/O, e.g. the read_csv function

train = read_csv("../input/train.csv")
test = read_csv("../input/test.csv")

test$place_id <- -1
All_Data <- rbind(train, test)

All_Data$t1 <- All_Data$time %% 60
All_Data$t2 <- (trunc(All_Data$time /  60 ) %% 24) 
All_Data$t3 <- (trunc(All_Data$time / 1440 ) %% 7)
#print(summary(All_Data))
library(caret)

train <- All_Data[which(All_Data$place_id >= 0),]
test <- All_Data[which(All_Data$place_id < 0),]
#trainIndex1 <- createDataPartition(train$place_id, p = .2, list = FALSE, times = 1)
#train <- train[which(train$y > 9 & train$x > 9), ]


All_Data <- NULL
gc()


sp=TRUE
if(sp) {
#    train1 <- train[which(train$place_id == 6619811780), ]
#    print(summary(train1))
    xy = cbind(train$x,train$y)
    library(sp)
#    s1 <- SpatialPoints(xy)
#    print(summary(s1))
#    print(head(s1))
    
    train1 <- SpatialPointsDataFrame(coords=xy, data=as.data.frame(train))
    print(nrow(train1))
    print(bbox(train1))
    agtrn <- aggregate(x=train, by=list(place_id=train$place_id), FUN="mean", dissolve = TRUE)
    print(nrow(agtrn))
    print(head(agtrn))
#    pg <- Polygon(xy)
#    print(summary(pg))
    txy <- cbind(test$x, test$y)
    test1 <- SpatialPointsDataFrame(txy, as.data.frame(test))
    print(head(test1))

#    pgs = Polygons(list(pg), "pg")
#    Spgs = SpatialPolygons(list(pgs), 1:1)
#    pover <- over(Spgs, ts1, returnList=TRUE)
#    print(summary(pover$pg))
#    print(head(pover$pg))
}


boost=FALSE
if(boost) {
    trainIndex <- createDataPartition(train$place_id, p = .5, list = FALSE, times = 1)
    Ftrain <- train[trainIndex, ]
    Fval  <- train[-trainIndex, ]
    feature.names <- c("x", "y", "accuracy", "t1", "t2", "t3")
    print(dim(Ftrain))
    print(date())
    
    library(xgboost)

    param1 <- list( objective           = "reg:linear", 
                    eta                 = 0.3, 
                    max_depth           = 3
                    )
    
    xgbtrain <- xgb.DMatrix(data.matrix(Ftrain[, feature.names]), label=Ftrain$place_id)
    xgbval <- xgb.DMatrix(data.matrix(Fval[,feature.names]), label=Fval$place_id)
    Watchlist <- list(val=xgbval, train=xgbtrain)
    
    fit <- xgb.train(data=xgbtrain, params=param1, nround=10, watchlist=Watchlist, verbose=1, print.every.n = 1)
    print(date())      
    print(summary(fit))
    
    importance_matrix <- xgb.importance(model = fit)
    print(feature.names[c(as.numeric(importance_matrix$Feature[1:nrow(importance_matrix)])+1)])
    
}

dt=FALSE
if(dt) {
    library(data.table)
    print(date())
    tt <- data.table(train)
    setkey(tt,place_id)
    write_csv(tt[accuracy < 101][,list(mx=mean(x),my=mean(y), sdx=sd(x),sdy=sd(y), macc=mean(accuracy), cnt=.N),by=place_id], "meanxy_by_placeid.csv")
}