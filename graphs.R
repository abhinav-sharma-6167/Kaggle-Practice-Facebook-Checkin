
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)

#Import train dataset
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")

#Add minute of the day, and day
train <- train %>% 
    mutate(minute = time %% (24*60), day = (time-minute)/(24*60))

se <- function(x) sqrt(var(x)/length(x))

by_day <- train %>% group_by(day) %>% summarize(x.mean = mean(x), y.mean= mean(y), accuracy.mean = mean(accuracy),
                                                x.se = se(x), y.se = se(y), accuracy.se = se(accuracy))

limits_x <- aes(ymax = x.mean + x.se, ymin=x.mean - x.se)
limits_y <- aes(ymax = y.mean + y.se, ymin=y.mean - y.se)
limits_acc <- aes(ymax = accuracy.mean + accuracy.se, ymin=accuracy.mean - accuracy.se)
ggplot(by_day, aes(x= x.mean, y=y.mean, color = day)) + geom_point()  + ggtitle("y vs x per day")
ggplot(by_day, aes(x= day, y=x.mean)) + geom_point() + geom_pointrange(limits_x) +  ggtitle("Mean x vs day")
ggplot(by_day, aes(x= day, y=y.mean)) + geom_point() + geom_pointrange(limits_y) + ggtitle("Mean y vs day")
ggplot(by_day, aes(x= day, y=accuracy.mean)) + geom_point() + geom_pointrange(limits_acc) + ggtitle("Mean accuracy vs day")

test <- test %>% 
    mutate(minute = time %% (24*60), day = (time-minute)/(24*60))
by_day_test <- test %>% group_by(day) %>% summarize(x.mean = mean(x), y.mean= mean(y), accuracy.mean = mean(accuracy),
                                                    x.se = se(x), y.se = se(y), accuracy.se = se(accuracy))

limits_x <- aes(ymax = x.mean + x.se, ymin=x.mean - x.se)
limits_y <- aes(ymax = y.mean + y.se, ymin=y.mean - y.se)
limits_acc <- aes(ymax = accuracy.mean + accuracy.se, ymin=accuracy.mean - accuracy.se)
ggplot(by_day_test, aes(x= x.mean, y=y.mean, color = day)) + geom_point() + ggtitle("y vs x per day (test)")
ggplot(by_day_test, aes(x= day, y=x.mean)) + geom_point() + geom_pointrange(limits_x) + ggtitle("Mean x vs day (test)")
ggplot(by_day_test, aes(x= day, y=y.mean)) + geom_point() + geom_pointrange(limits_y) + ggtitle("Mean y vs day (test)")
ggplot(by_day_test, aes(x= day, y=accuracy.mean)) + geom_point() + geom_pointrange(limits_acc) + ggtitle("Mean accuracy vs day (test)")