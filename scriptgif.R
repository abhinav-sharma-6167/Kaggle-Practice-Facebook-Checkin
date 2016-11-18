### Uses http://www.r-bloggers.com/animated-plots-with-r/ as a guide

library(ggplot2)
library(readr)
library(animation)

print("Loading data")
train_data <- read_delim('../input/train.csv', ",")

time_start <- 70*7*24*60  # in minutes
time_end <- time_start + 4*7*24*60  # in minutes
x_min <- 5
x_max <- 6
y_min <- 5
y_max <- 6

length <- time_end - time_start
x_wid <- x_max - x_min
y_wid <- y_max - y_min


print("Creating small data set")
small_data <- train_data[(train_data$time>=time_start)&(train_data$time<=time_end)&(train_data$x>=x_min)&(train_data$x<x_max)&(train_data$y>=y_min)&(train_data$y<y_max),]
print(nrow(small_data))

print("Finding top place_ids")
place_id.table <- table(small_data$place_id)
place_id.table <- place_id.table[order(-place_id.table)]
place_ids <- names(place_id.table[1:1000])

print("Creating smaller data set")
smaller_data <- small_data[small_data$place_id%in%place_ids,]

interval <- 30 # in minutes
smaller_data$time_int <- smaller_data$time%/%interval

clock_centre <- c(x_min-0.1*x_wid,y_max-0.1*y_wid)
clock_txt_centre <- c(x_min-0.1*x_wid,y_max-0.25*y_wid)

week_centre <- c(x_min-0.1*x_wid,y_max-0.4*y_wid)
week_clock_txt_centre <- c(x_min-0.1*x_wid,y_max-0.55*y_wid)

day_txt_centre <- c(x_min-0.1*x_wid,y_max-0.65*y_wid)
week_txt_centre <- c(x_min-0.1*x_wid,y_max-0.75*y_wid)

clock_ticks <- data.frame(x=clock_centre[1]+0.09*x_wid*(sin((1:24)*2*pi/24)),y=clock_centre[2]+0.09*y_wid*(cos((1:24)*2*pi/24)))
week_ticks <- data.frame(x=(week_centre[1]+0.09*x_wid*sin((0:6)*2*pi/7)),y=week_centre[2]+0.09*y_wid*(cos((0:6)*2*pi/7)))

all_places <- data.frame(place_id = place_ids,x = clock_centre[1], y = clock_centre[2])

BG_colour_palette <-colorRampPalette(c("gray95","gray70"))

draw.curve<-function(i){
  min <- ((i*interval)%%60)/60
  hour <- ((i*interval)%%(60*24))/60/24
  day <- ((i*interval)%/%(60*24))
  week <- ((i*interval)%/%(60*24*7))
  day_of_week <- ((i*interval)%%(60*24*7))/60/24/7
  
  a<-ggplot(smaller_data[smaller_data$time_int%in%((i-3):i),],aes(x = x, y = y))+
    geom_point(data = all_places, aes(x = x, y = y, colour=as.factor(place_id)))+
    geom_point(aes(colour=as.factor(place_id),alpha=time_int))+
    geom_point(data = clock_ticks,aes(x=x,y=y),color="slategray")+
    geom_polygon(data = clock_ticks,aes(x=x,y=y),fill="slategray1")+
    annotate("text", x = clock_txt_centre[1], y = clock_txt_centre[2], label = "Time of day",size=4)+
    geom_point(data = week_ticks,aes(x=x,y=y),color="slategray")+
    geom_polygon(data = week_ticks,aes(x=x,y=y),fill="slategray1")+
    annotate("text", x = week_clock_txt_centre[1], y = week_clock_txt_centre[2], label = "Day of week",size=4)+
    annotate("text", x = day_txt_centre[1], y = day_txt_centre[2], label = paste("Day",day),size=5)+
    annotate("text", x = week_txt_centre[1], y = week_txt_centre[2], label = paste("Week",week),size=5)+
    
    geom_segment(aes(x=clock_centre[1],xend=clock_centre[1]+0.08*x_wid*sin(min*2*pi),y=clock_centre[2],yend=clock_centre[2]+0.08*y_wid*cos(min*2*pi)),color="slategray")+
    geom_segment(aes(x=clock_centre[1],xend=clock_centre[1]+0.06*x_wid*sin(hour*2*pi),y=clock_centre[2],yend=clock_centre[2]+0.06*y_wid*cos(hour*2*pi)),color="slategray")+
    geom_segment(aes(x=week_centre[1],xend=week_centre[1]+0.08*x_wid*sin(day_of_week*2*pi),y=week_centre[2],yend=week_centre[2]+0.08*y_wid*cos(day_of_week*2*pi)),color="slategray")+
    scale_fill_manual(values = palette("default"))+
    xlim(x_min-0.2*x_wid,x_max)+ylim(y_min,y_max)+
    theme(legend.position="none",
          panel.background=element_rect(fill=BG_colour_palette(13)[1+round(abs(24*hour-12))]))
  
  print(a)
}
#draw.curve(i=20)

print("Plotting graphs")
plot.animate <- function() {
  lapply((time_start/interval):(time_end/interval), function(i) {
    draw.curve(i)
  })
}

print("Creating GIF")
saveGIF(plot.animate(), interval = .2, movie.name="plot.gif")