library(readr)
library(GISTools)
library(animation)

train <- read_csv("../input/train.csv")

tsp <- as.data.frame(train)

coordinates(tsp) <- ~x+y

q <- quantile(tsp$time, probs = seq(0, 1, 0.00001))

dens.plot <- function(i, quantiles=q) {
        tsp_i <- subset(tsp, time >= q[i] & time <= q[i+5])
        dens <- kde.points(tsp_i)
        level.plot(dens)
}

ani.options(interval = 0.1)

saveGIF( {
        for(i in 1:240) { # set some upper bound
            dens.plot(i)
            ani.pause()
        }
},
movie.name = "dens.gif",
img.name = "Rplot", 
convert = "convert")
