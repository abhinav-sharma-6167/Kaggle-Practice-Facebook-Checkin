#THE SCRIPT TO LEARN




setwd("D:/Learning/Kaggle/facebook")

library(data.table)
library(bit64)
library(magrittr)
library(ggplot2)

require(caret)
require(corrplot)
require(Rtsne)
require(xgboost)
require(stats)
require(knitr)
require(ggplot2)
knitr::opts_chunk$set(cache=TRUE)

#most important to set integer64 to character
train_fb <- fread("train.csv",integer64 = "character")
test_fb <- fread("test.csv")
train_data <-copy(train_fb)
test_data<-copy(test_fb)

#what way is this?! ;)
#train_data %>% filter(train_data$x >4, train_data$x <4.25, train_data$y >5.5, train_data$y < 5.75) 
#train_data <-filter(train_data$x >4, train_data$x <4.25, train_data$y >5.5, train_data$y < 5.75)


#Selecting random area!

train_data<-train_data[(train_data$y > 2)]
train_data<-train_data[(train_data$x > 7)]
train_data<-train_data[(train_data$y < 2.25)]
train_data<-train_data[(train_data$x < 7.25)]


train_data$hour <- (train_data$time/60) %% 24

small_train <- train_data[train_data$time < 7.3e5]
small_val <- train_data[train_data$time >= 7.3e5] 




# Visualizing the cluster by colour..remember removing legend..plots all points as x vs y

#1

ggplot(train_data, aes(x, y )) +
  geom_point(aes(color = place_id))+theme_minimal()+theme(legend.position = "none")

#this is the important one.....theme(legend.position = "none")


#2
plot(train_data$accuracy)
plot(train_data$accuracy,type = 'l',col="red",main="Title!",sub="Below title of this plot!",xlab = "accuracy",ylab = "freq of accuracy")#black lines..cool dx
#not many different colors na
#same as plot.default
#how does plot.formula work..seems imp..if u know how to use..
points(train_data$accuracy,pch=20)#kinda overlapping these points on a particular plot..pch 8 and 20 are cool!
#similar function is lines ,see doc for par and 100s of others



#3
hist(train_data$accuracy,breaks = c(0,20,40,60,80,100,150,200,300,400,500,700,900,1010),col = "red",border = "black",density = 120)
barplot(train_data$accuracy,width = 0.75,space = 0.05,beside = F,horiz = T)#heavy...comp can take time

#4
qplot(train_data$accuracy,geom ="count"  )#looks better since it is from ggplot2
qplot(train_data$x,train_data$y,geom="count")
qplot(x,y,data = train_data,colour=place_id)+theme(legend.position="none")
qplot(accuracy,y,data = train_data,colour=x)+theme(legend.position="none")
qplot(x,y,data = train_data,geom = "path",colour=place_id)+theme(legend.position="none")

ls(pattern = '^geom_', env = as.environment('package:ggplot2'))


#5  a bit of advanced things like ggplot2 , xkcd etc



#the handdrawn line
ggplot(mapping=aes(x=seq(1,10,.1), y=seq(1,10,.1))) + 
  geom_line(position="jitter", color="red", size=1.3) + theme_bw()

#learning xkcd
vignette("xkcd-intro")


#xkcd axes
xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)

ggplot() +
geom_point(aes(mpg, wt), data=mtcars) +
xkcdaxis(xrange,yrange)

#xkcd font?!
#font not installed?! see if the ttf file gets downloaded

#XKCDMAN


ratioxy <- diff(xrange)/diff(yrange)
mapping <- aes(x, y, scale, ratioxy, angleofspine,anglerighthumerus, anglelefthumerus, anglerightradius, angleleftradius,
                  anglerightleg, angleleftleg,angleofneck,linetype=city)
dataman <- data.frame(x= 30, y=4,scale = 0.51 ,ratioxy = ratioxy,angleofspine = -pi/2 ,anglerighthumerus = -pi/6,anglelefthumerus = pi +pi/6,anglerightradius = -pi/3,angleleftradius = -pi/3,
                        anglerightleg = 3*pi/2 - pi / 12,angleleftleg = 3*pi/2 + pi / 12 ,angleofneck = runif(1, 3*pi/2-pi/10, 3*pi/2+pi/10),
                        city=c("Liliput","Brobdingnag"))
                       
p <- ggplot() + geom_point(aes(mpg, wt, colour=as.character(vs)), data=mtcars) +
   xkcdaxis(xrange,yrange) +
   xkcdman(mapping, dataman) +theme(legend.position="none")
                        
#making multiple men..just do c() function








volunteers <- data.frame(year=c(2007:2011),number=c(56470, 56998, 59686, 61783, 64251))
 xrange <- range(volunteers$year)
 yrange <- range(volunteers$number)
 ratioxy <- diff(xrange) / diff(yrange)
 datalines <- data.frame(xbegin=c(2008.3,2010.5),ybegin=c(63000,59600),xend=c(2008.5,2010.3), yend=c(63400,59000))
 
 mapping <- aes(x, y, scale, ratioxy, angleofspine,
                 anglerighthumerus, anglelefthumerus,
                 anglerightradius, angleleftradius,
                anglerightleg, angleleftleg, angleofneck)
 
 dataman <- data.frame( x= c(2008,2010), y=c(63000, 58850
 ),
  scale = 1000 ,
  ratioxy = ratioxy,
  angleofspine = -pi/2 ,
  anglerighthumerus = c(-pi/6, -pi/6),
  anglelefthumerus = c(-pi/2 - pi/6, -pi/2 - pi/6),
  anglerightradius = c(pi/5, -pi/5),
  angleleftradius = c(pi/5, -pi/5),
  angleleftleg = 3*pi/2 + pi / 12 ,
  anglerightleg = 3*pi/2 - pi / 12,
  angleofneck = runif(1, 3*pi/2-pi/10, 3*pi/2+pi/10))
                        
 p <- ggplot() + geom_smooth(mapping=aes(x=year, y =number),data =volunteers, method="loess") +
    xkcdaxis(xrange,yrange) +
    ylab("Volunteers at Caritas Spain") +
    xkcdman(mapping, dataman) +
    annotate("text", x=2008.7, y = 63700,label = "We Need\nVolunteers!", family="xkcd" ) +
    annotate("text", x=2010.5, y = 60000,label = "Sure\nI can!", family="xkcd" ) +
    xkcdline(aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend),
              datalines, xjitteramount = 0.12)
 
 
 
 
 #do something about the font
 #downloading the font_not effective though
 xkcdFontURL <- "http://simonsoftware.se/other/xkcd.ttf"
 download.file(xkcdFontURL,dest="xkcd.ttf",mode="wb")
 
 
 
 
 


train_data = train_data[i = site_name != 2]
g = ggplot(d, aes(x = site_name, y = Prop))
g1 = g + geom_bar(aes(fill = factor(site_name)), stat = 'identity') + scale_fill_discrete(name = 'site_name')
ggplotly(g1)





