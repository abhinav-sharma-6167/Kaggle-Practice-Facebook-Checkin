



###########All aspects for the fb codes!!################


############################1 The one with the Random forest##############################################



# ---
#   title: "Random Forest on a Few Blocks"
# author: "Alexandru Papiu"
# date: "May 18, 2016"
# output:
#   html_document:
#   fig_height: 5
# fig_width: 7
# highlight: tango
# theme: readable
# ---
#   
#   In this competition we're given around 30 million (simulated) check-ins on Facebook in a 10km by 10km grid. The goal is to build a model that predicts what business a user checks into based on spatial and temporal information. The tricky part here is that there are around 100k different classes(`place_id`'s) so most supervised learning techniques won't work on the entire dataset. However most classes are clustered in only certain parts of the grid so the idea I'll pursue here is to select a small-ish square within the grid and try to see if we can do better within the small square. First I'll do some exploratory data analysis in the smaller square then I'll use a random forest algorithm for prediction and finally, I'll analyze the results.
# 
# ### Read and Clean:
# 
# Let's load the required packages and read in the data:
  
  # ```{r, message = FALSE, warning = FALSE}
library(data.table) #reading in the data
library(dplyr) #dataframe manipulation
library(ggplot2) #viz
library(ranger) #the random forest implementation
library(plotly) #3D plotting
library(tidyr) #dataframe manipulation

fb <- fread("../input/train.csv", integer64 = "character", showProgress = FALSE)
# ```
# 
# Now we'll select a subset of the data - I'll just pick a random 250 meters by 250 meters square in our imaginary Facebook city.
# 
# ```{r}
fb %>% filter(x >1, x <1.25, y >2.5, y < 2.75) -> fb
head(fb, 3)
# ```
# 
# Notice that time is given to us simply as a numeric value. There have been quite a few great scripts exploring the timestamp and the unit of `time` here is almost certainly minutes. Since businesses tend to run on daily cycles let's extract a new feature called `hour` that gives the hour in the day (from 0 to 24). 
# 
# ```{r}
fb$hour <- (fb$time/60) %% 24
# ```
# 
# We will split our dataset into a training and validation set so we can check the results. We choose the validation set to be the more recent check-ins so that our validation structure is similar to the one kaggle does behind the scenes on the test set.
# 
# ```{r}
small_train = fb[fb$time < 7.3e5,]
small_val = fb[fb$time >= 7.3e5,] 
# ```


### Exploratory Analysis:

# Let's take a look at our training set:
  # 
  # ```{r, fig.height = 8, fig.width = 10}
ggplot(small_train, aes(x, y )) +
  geom_point(aes(color = place_id)) + 
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Check-ins colored by place_id")
# ```
# 
# Ok, so the clusters are pretty visible, however there seems to be quite a lot of overlap - the place_id's are definitely not separable. Let's try plotting them using the hour component as our third variable. We will just look at the most popular clusters outherwise it gets really messy:
#   
#   ```{r, fig.height = 8, fig.width = 8}
small_train %>% count(place_id) %>% filter(n > 500) -> ids
small_trainz = small_train[small_train$place_id %in% ids$place_id,]

plot_ly(data = small_trainz, x = x , y = y, z = hour, color = place_id,  type = "scatter3d", mode = "markers") %>% layout(title = "Place_id's by position and Time of Day")

# ```
# 
# Whoa very cool! Adding the time dimension definitely helps. The daily cycles are clearly visible above - for certain places the check in's stop for a few hours and then start picking up again. Other businesses have quite a few peaks throughtout the day, and the peaks tend to be rather different for different businesses. Also keep in mind that the upper z-square (z = 24) and the lower z-square (z = 0) are really the same thing since time of day is, well, prediodic. So really this thing we're looking at is better viewed not as a cube but as a (flat) solid torus!
#   
#   However we still might have too many classes for something like random forest to work at its best. Let's check it out:
# ```{r}
length(unique(small_train$place_id))
# ```
# 
# Let's for now remove the `place_id`'s that have only three or less occurences in the city are we picked. This will decrease the number of classes by a lot. Since we have a validation set we can always come back and change the filter level to see if we get better results.
# 
# ```{r}
small_train %>% count(place_id) %>% filter(n > 3) -> ids
small_train = small_train[small_train$place_id %in% ids$place_id,]
# ```
# 
# Sweet, now we have **`r nrow(small_train)`** training examples and **`r nrow(ids)`** classes and we're ready to do some machine learning!
#   
#   ### The Forest:
#   
#   Let's use the `ranger` implementation of the random forest algorithm. The `ranger` package tends to be significantly faster(around 5x) and more memory efficient than the `randomForest` implementation and we'll need as much of that as we can get for this problem. 
# 
# ```{r}
set.seed(131L)
small_train$place_id <- as.factor(small_train$place_id) # ranger needs factors for classification
model_rf <- ranger(place_id ~ x + y + accuracy + hour,
                   small_train,
                   num.trees = 100,
                   write.forest = TRUE,
                   importance = "impurity")


pred = predict(model_rf, small_val)
pred = pred$predictions
accuracy = mean(pred == small_val$place_id) 
# ```
# 
# We get an accuracy of **`r accuracy`**. Hey not bad! Keep in mind that the evaluation metric for this competition is mean average precision at 3 so predicting votes/probabilities by class and then counting the top three id's is guaranteed to improve our score. But for simplicity we'll just stick to accuracy.
# 
# Let's take take a look at the predictions on the validation set:
# 
# ```{r}
small_val$Correct = (pred == small_val$place_id)

ggplot(small_val, aes(x, y)) +
geom_point(aes(color = Correct)) + 
theme_minimal() +
scale_color_brewer(palette = "Set1") +
ggtitle("RF Results")

# ```
# 
# It does seem that the correctly identified check-ins are more "clustered" while the wrongly identified ones are more uniformly distributed but other than that no clear patters here.
# 
# Let's also take a look at what kind of id's our random forest gets wrong. To do this we will look at accuracy by id and also plot the id's based on how often they appear in the validation set. We see below that our model is doing actually really great on the more popular id's(more blue on the right). However it loses when it looks at id's that appear only a few times. 
# 
# ```{r, fig.width = 12}
#reordering the levels based on counts:
small_val$place_id <- factor(small_val$place_id,
                             levels = names(sort(table(small_val$place_id), decreasing = TRUE)))

small_val %>% 
  ggplot(aes(x = place_id)) + geom_bar(aes(fill = Correct)) + 
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Prediction Accuracy by ID and Popularity") +
  scale_fill_brewer(palette = "Set1")
# ```
# 
# We see above that our model is doing actually really good on the more popular id's(the blue area on the right). However it loses when it looks at id's that appear only a few times.  
# 
# Let's look at the importance of our variables as well:
# 
# ```{r}

data.frame(as.list(model_rf$variable.importance)) %>% gather() %>% 
ggplot(aes(x = reorder(key, value), y = value)) +
geom_bar(stat = "identity", width = 0.6, fill = "grey") +
coord_flip() +
theme_minimal() +
ggtitle("Variable Importance (Gini Index)") +
theme(axis.title.y = element_blank()) 
# 
# ```
# 
# This is quite interesting. First of all the `y` variable is more important than the `x` coordinate. This is in line with a lot of observations in other scripts: the variance by `place_id` is significantly higer in the `x` direction than in the `y` direction. This means that the `y` axis is a better predictior of `place_id` and the random forest figures this out on its own. `hour` is also a good predictor but less so than the spatial features - this makes sense since the location of a check-in should be more important than the time of the check-in. And lastly we see that accuracy _is_ important. Accuracy is a bit misterious since we don't get an actual definition for it is but at least the model tells us it's somewhat important.
# 
# ### Further Directions:
# 
# So where to go from here?
# 
# Here are a few sugesstions:
# 
# - play with how big the grid size is and see if you can get better validation results
# - try different areas on the map
# - try different models (maybe xgboost or somehing simple like logistic regression)
# - split the grid into n*n little squares and run you algorithm in each of the squares - this might take a while though.
# 
# Feel free to fork this script and explore these ideas more.
# 
# Thanks for reading!











###########################################2. The one with the Gif ka Animation#################################



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




































######################################3.The one written by legendary ZFTurbo########################################







# The idea of data transformation and raiting calculations is from Python script 
# by 'ZFTurbo: https://kaggle.com/zfturbo'

#########################################################################
new_dim_x1  <- 290     # new dimensions for x 
new_dim_y1  <- 725     # new dimensions for y
new_dim_x2  <- 145     # new dimensions for x 
new_dim_y2  <- 362     # new dimensions for y
chunk_size  <- 1000000  # for memory usage optimization only
#########################################################################

library(dplyr) 
library(readr) 
library(foreach)
library(data.table)

train <- fread("../input/train.csv", integer64 = "character")
test <- fread("../input/test.csv", integer64 = "character")

train <- train[,
               .(row_id, time, place_id,
                 x1 = as.integer(floor(x/10 * new_dim_x1)),
                 y1 = as.integer(floor(y/10 * new_dim_y1)),
                 x2 = as.integer(floor(x/10 * new_dim_x2)),
                 y2 = as.integer(floor(y/10 * new_dim_y2)),
                 quarter_period_of_day = floor((time + 120) / (6*60)) %% 4,
                 rating = log10(3+((time + 120.0) / (60 * 24 * 30)))
               ),
               ]

test <- test[,
             .(row_id,
               x1 = as.integer(floor(x/10 * new_dim_x1)),
               y1 = as.integer(floor(y/10 * new_dim_y1)),
               x2 = as.integer(floor(x/10 * new_dim_x2)),
               y2 = as.integer(floor(y/10 * new_dim_y2)),
               quarter_period_of_day = floor((time + 120) / (6*60)) %% 4),
             ]

# Train group 1
train_group1  <- train[,.(rating=sum(rating), max_time=max(time)), by=.(x1, y1, quarter_period_of_day, place_id)] 
setorder(train_group1,x1,y1, quarter_period_of_day, -rating, -max_time)
train_group1 <- train_group1[,.(place_id=head(place_id, n = 3)), by=.(x1, y1, quarter_period_of_day)]

# Train group 2
train_group2  <- train[,.(rating=.N, max_time=max(time)), by=.(x1, y1, place_id)] 
setorder(train_group2,x1,y1, -rating, -max_time)
train_group2 <- train_group2[,.(place_id=head(place_id, n = 3)),by=.(x1, y1)]

# Train group 3
train_group3  <- train[,.(rating=.N, max_time=max(time)), by=.(x2, y2, place_id)] 
setorder(train_group3,x2,y2, -rating, -max_time)
train_group3 <- train_group3[,.(place_id=head(place_id, n = 3)),by=.(x2, y2)]

test_chunks <- split(test, ceiling(1:nrow(test)/chunk_size))
result <- foreach(chunk=1:length(test_chunks), .combine="rbind", .packages = "dplyr") %do% 
{
  print(sprintf("task %d/%d", chunk, length(test_chunks)))
  test_chunk <- test_chunks[[chunk]]
  
  # Join 1  
  test_train_join1 <- inner_join(select(test_chunk, row_id, x1, y1, quarter_period_of_day),
                                 train_group1,
                                 by = c("x1", "y1", "quarter_period_of_day")) %>% select(row_id, place_id)
  print(sprintf("Join1"))
  
  # Join 2
  test_train_join2 <- inner_join(select(test_chunk, row_id, x1, y1),
                                 train_group2,
                                 by = c("x1", "y1")) %>% select(row_id, place_id)
  print(sprintf("Join2"))
  
  # Join 3
  test_train_join3 <- left_join(select(test_chunk, row_id, x2, y2),
                                train_group3,
                                by = c("x2", "y2")) %>% select(row_id, place_id)
  ##why does this seem like python to me??!
  print(sprintf("Join3"))
  
  # Group all joins
  test_train_join_all <- rbindlist(list(test_train_join1,test_train_join2,test_train_join3), use.names=TRUE) %>% 
    unique()
  
  result_new <- test_train_join_all[, .(place_id = paste(head(place_id, 3),collapse=" ")), by = row_id]
  print(sprintf("Group all"))
  return(result_new)
}

result$place_id[result$place_id=="NA"] <- ""
write_csv(result, "result.csv")






















###########################################4.The one with loads of explanation wala banda##############################






# ---
#   title: "EDA"
# author: "Michael Griffiths"
# date: "May 11, 2016"
# output: 
#   html_document: 
#   fig_height: 10
# fig_width: 16
# highlight: pygments
# toc: yes
# ---
#   
#   # Background
#   
#   Facebook has simulated a dataset, consisting of:
#   
#   > ...an artificial world consisting of more than 100,000 places located in a 10 km by 10 km square.
# 
# For a given set of co-ordinates, the prediction task is to return a _ranked list_ of most likely "places."
# 
# # Analysis
# 
# To begin with, let's load some libaries at aid and abet the analysis.
# 
# ```{r Load_Libraries, warning=F}
library(needs)
needs(dplyr, tidyr, stringr, lubridate, readr, ggplot2,
MASS,
pander, formattable, viridis)
# ```
# 
# ## Data 
# 
# We receive three files from Facebook - 
# 
# ```{r}
list.files("../input") %>%
as.list %>%
pander
# ```
# 
# Each file contains a single CSV file. In the case of `train.csv` and `test.csv`, we can _guess_ that the schema is the same, except for a "correct" answer in `train.csv`.
# 
# I put "correct" in quotes, because Facebook says that:
# 
# > Data was fabricated to resemble location signals coming from mobile devices, giving you a flavor of what it takes to work with real data complicated by inaccurate and noisy values. Inconsistent and erroneous location data can disrupt experience for services like Facebook Check In.
# 
# It's very possible that errors have been deliberately inserted into the `train.csv` dataset.
# 
# Well, let's begin. We'll use `readr::read_csv`, which will automatically unpack the (single) file. This is equivalent to something like `base::read.csv(unz("../data/train.csv.zip", "train.csv"))`, but is a little cleaner. Plus, `readr::read_csv` is faster than base `read.csv`.
# 
# ```{r Load_Data}
train <- read_csv("../input/train.csv.zip")
train <- read_csv("../input/train.csv")
glimpse(train)
# ```
# 
# OK: so we have ~29 million records. 
# 
# A few notes:
#   
#   * `row_id` seems to be ... a row ID. It is `r length(unique(train$row_id)) == nrow(train)` that the number of unique `row_id`s is the same as the number of rows in the data frame.
# * `x` is presumably bounded between [0, 10] as the x-axis on the 10-km square.
# * `y` looks to be the same as `x`, just the other dimension.
# * `accuracy` is intersting: it's all over the place. The smallest value is `r comma(min(train$accuracy))`; the biggest value is `r comma(max(train$accuracy))`. We'll have to look into that.
# * `time` has no units. Since Facebook notes that time an accuracy are `"intentionally left vague in their definitions."`, we will have to look into that.
# * `place_id` is probably a unique identifier. There `r length(unique(train$place_id))` unique values.
# 
# Let's start by examining the "vague" variables.
# 
# ### Accuracy
# 
# We already know that accuracy isn't exactly defined. From first principles, we could think of it a few ways - 
#   
#   * Error on some arbitary scale. This seems unlikely, since the max is > 1,000.
# * Error on the same scale as `x` and `y`. Now, this could be an estimated radius (with the `x` and `y` values as the center); either normal or squared.
# 
# Since we have _a lot_ of data, and we're running this in Kaggle scripts, we can randomly sample 1% of the data and look at the data. The pattern will (almost certainly) be the same.
# 
# ```{r}
train %>%
sample_frac(.01) %>%
ggplot(aes(x = accuracy)) +
geom_density()
# ```
# 
# It looks like we have three peaks. Do we think there are underlying parameters in the simulation at these peaks?
# 
# We might also think that there are different measures of `accuracy` at different locations. 
# 
# Let's see how even the accuracy is over `x` and `y`.
# 
# Since we have two dimensions and quite a few buckets (50 x 50 = 2,500 bins).
# 
# ```{r}
train %>%
  sample_frac(0.01) %>%
  ggplot(aes(x, y, z = accuracy)) +
  stat_summary_2d(fun = mean, bins = 50) +
  scale_fill_viridis()
# ```
# 
# Now, that looks pretty random to me. Doesn't look like there are either (large) hotspots, or a tendency across the whole square.
# 
# We can look at it a few other ways (median, max, min, sd, var, etc), but I don't think they add a great deal.
# 
# Even if the _general_ distribution is pretty even, are the high-accuracy values (which we think mean _low_ accuracy) anywhere in particular? 
# 
# ```{r}
train %>%
  filter(accuracy > 200) %>%
  # Since we're dealing with the subset that's over 200,
  # let's take a bigger sample.
  sample_frac(0.05) %>%
  ggplot(aes(x, y)) +
  geom_bin2d(bins = 50) +
  scale_fill_viridis() +
  lims(x = c(0, 10), y = c(0, 10))
# ```
# 
# I guess the short answer is "not really". If there's any signal in there, it's not poppin' to eyes.
# 
# ### Time
# 
# Now let's think about time!
#   
#   We'll take the same approach as above. First, let's plot the density of the time values.
# 
# ```{r}
train %>%
  sample_frac(.01) %>%
  ggplot(aes(x = time)) +
  geom_density()
# ```
# 
# That's interesting: there are two _very big_ drops. 
# 
# What's also interesting is how evenly dispersed the time is - if this were, say, _daily_ data we'd expect much more of a seasonal pattern.
# 
# Do we think `time` interacts with either `x` or `y`?
# 
# ```{r}
train %>%
sample_frac(.01) %>%
ggplot(aes(x = x, y = time)) + 
geom_bin2d(bins = 50)
# ```
# 
# The short answer is "no". We see the time bands that have lower density, but apart from that everything in X seems pretty similar. OK, so relatively even dispersal so far.
# 
# We can run the chart as well for `y`, but it's basically the same.
# 
# ### Place ID
# 
# Now that we have some background on `time`, `accuracy` and (sideways) on `x` and `y` let's look at `place_id`.
# 
# First, a quick check. Sometimes ID values aren't really uniformly distributed.
# 
# ```{r}
train %>%
  sample_frac(0.01) %>%
  ggplot(aes(x = place_id)) +
  geom_density()
# ```
# 
# OK, so `place_id` is (essentially) uniformly distributed. That's good news - no obvious leakage.
# 
# Let's also check the correlation between `place_id` and both `x` and `y` to make sure the ID doesn't encode information about the location. It probably doesn't; but worth checking.
# 
# ```{r}
print(sprintf("Correlation of place_id and x: %s", cor(train$place_id, train$x)))
print(sprintf("Correlation of place_id and y: %s", cor(train$place_id, train$y)))
# ```
# 
# OK, so _probably not_ related. That's good news as well. 
# 
# First question: what's the distribution of `place_id`?
# 
# ```{r}
place_ids <-
  train %>%
  sample_frac(0.05) %>%
  group_by(place_id) %>%
  summarise(freq = n())

place_ids %>%
  ggplot(aes(x = freq)) +
  geom_density()
# ```
# 
# Well, if that doesn't look like a Poisson distribution!
# 
# Let's fit and compare.
# 
# ```{r}
fitted_distr <- fitdistr(place_ids$freq, "Poisson")
print(sprintf("Log Liklihood: %s", fitted_distr$loglik))

pois_samples <- rpois(nrow(place_ids), fitted_distr$estimate[[1]])

place_ids %>%
  mutate(simulated = pois_samples) %>%
  ggplot(aes(x = freq)) +
  geom_density() +
  geom_density(aes(x = simulated), colour = "red") 
# ```
# 
# Well, obviously, it's not. Probably something bounded below by zero with a much fatter tail. 
# 
# Perhaps a Cauchy distribution?
# 
# ```{r warning=F}
fitted_distr <- fitdistr(place_ids$freq, "cauchy")
print(sprintf("Log Liklihood: %s", fitted_distr$loglik))

cauchy_samples <- rcauchy(nrow(place_ids), fitted_distr$estimate[[1]], fitted_distr$estimate[[2]])

place_ids %>%
mutate(simulated = cauchy_samples) %>%
ggplot(aes(x = freq)) +
geom_density() +
geom_density(aes(x = simulated), colour = "red") +
xlim(0, 100)
# ```
# 
# Well, it's *better*. 
# 
# We could play around with this for a while; the principle point is that this looks like a pretty clean distribution for frequency, which is nice. Humps - or something else suggesting another type of mixture - would be _far_ harder to work with.
# 
# How variable are the estimates for the popular places?
# 
# Let's take a handful (10 places) that have a frequency of more than 10 (in our sample) and find the `x` and `y` parameters for each.
# 
# ```{r}
place_ids %>%
filter(freq > 10) %>%
top_n(n = 10, wt = freq) %>%
inner_join(train %>% sample_frac(0.05)) ->
places

# Now we can plot them - 
places %>%
ggplot(aes(x, y, colour = as.factor(place_id))) +
geom_point() +
lims(x = c(0, 10), y = c(0, 10))
# 
# ```
# 
# Well now, _that_ is interesting. It looks like we have *way* more horizontal (`x`) variation than we have vertical (`y`) variation.
# 
# What on earth is going on?
# 
# And how does accuracy play into this?
# 
# Let's try again.
# 
# ```{r}
places %>%
    ggplot(aes(x, y, colour = as.factor(place_id))) +
    geom_point(aes(size = 1 / accuracy), alpha = 0.6) +
    lims(x = c(0, 10), y = c(0, 10))
# ```
# 
# I don't know - that's interesting. While in general it seems like the places with the _smalles_ accuracy scores are in the middle, there are some pretty odd patterns here. 
# 
# The whole "much more variation in `x` than `y`" is killing me. Let's see a table.
# 
# ```{r}
places %>% 
group_by(place_id) %>% 
summarise(mean_x = mean(x), sd_x = sd(x), mean_y = mean(y), sd_y = sd(y)) %>%
arrange(desc(sd_x)) %>%
mutate_each(funs(comma), ends_with("x"), ends_with("y")) %>% 
formattable(
list(sd_x = color_bar("orange", min = 0, max = 1),
sd_y = color_bar("pink", min = 0, max = 1)), 
align = 'l'
)
# ```
# 
# Wow. That's such a weird pattern. And note that the biggest `y` value is 0.04 - the same as the *smallest* `x` value. Crazy.
# 
# # Conclusion
# 
# There's some interesting stuff going on here. I'm going to guess that the simulation has some toggles that were set to selected values, and part of the plan will be to reverse-engineer those toggles (where appropriate).
# 
# I haven't begin the modeling assignment. Before that, we may also want to look at:
# 
# * *Overlap*: How many `place_id` overlap with others? How much? Does it depend on popularity? 
# * Do things overlap more in the `x` or `y` direction?
# * Does the bias of the error depend on popularity? 
# * How accuracy play into the variability we see in the dataset?
# * Given that the error is MAP@3 (Mean Average Precision Error) with three values to provide, what can we do to take advantage of that?














############################################5.The one with hell too many plots!!################################







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





#########################################6.The one who wrote that in the notebook!!!###########################




library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(caret) # createDataPartition function
library(dplyr) # select, filter, group_by, arrange, summarize functions
library(RColorBrewer)
library(repr)
library(IRdisplay)

set.seed(42)




# Read data from file (Runtime: About 40 seconds)
train <- read_csv("../input/train.csv")

head(train)





# Convert place_id column to characer
train$place_id <- as.character(train$place_id)



# Create a list of the top 100 place_ids by number of events (Runtime: About 15 seconds)
places_top_100 <- train %>%
  group_by(place_id) %>%
  summarise(
    cnt_events = n(),
    x_avg = mean(x, na.rm = T),
    y_avg = mean(y, na.rm = T),      
    acc_avg = mean(accuracy, na.rm = T),
    time_avg = mean(time, na.rm = T)
  ) %>%
  arrange(desc(cnt_events)) %>%
  mutate(row_id = 1:n()) %>%
  filter(row_id <= 100)

# Show the first few rows in the data frame
head(places_top_100)





# Join the original train dataframe to the list of top 100 place_ids
train_top_100_places <- inner_join(train, places_top_100, by="place_id")

# How many rows to we have now?
paste("Number of rows: ",nrow(train_top_100_places))


# Create X vs. Y plot (Runtime: About ## seconds)
options(repr.plot.width=8, repr.plot.height=8)
ggplot(data=train_top_100_places, aes(x=y, y=x)) +
  geom_point(aes(color=place_id, size=accuracy)) +
  coord_cartesian(xlim = c(2,5), ylim = c(0,10)) +
  ggtitle("X versus Y") +
  theme(legend.position="none")

# Note: This graph shows a different color for each place_id.  There aren't
# many colors in the pallet, and we have 100 places.  Several places will have
# the same color.  Also the size of the bubbles are proportional to the accuracy.









# Create 'time' vs. 'y' plot (Runtime: About ## seconds)
options(repr.plot.width=8, repr.plot.height=8)
ggplot(data=train_top_100_places, aes(x=y, y=time)) +
  geom_point(aes(color=place_id, size=accuracy)) +
  coord_cartesian(xlim = c(2,5)) +
  ggtitle("Time versus Y") +
  theme(legend.position="none")

# Note:  It looks like some events are continuous over time, and 
# others occur during specific time intervals.







# Create 'time' vs. 'x' plot
options(repr.plot.width=8, repr.plot.height=8)
ggplot(data=train_top_100_places, aes(x=x, y=time)) +
  geom_point(aes(color=place_id, size=accuracy, alpha=0.2)) +
  coord_cartesian(xlim = c(3,4)) +
  ggtitle("Time versus X") +
  theme(legend.position="none")

# Note: This graph is interesting.  I'm not exactly sure how to interpret it yet.





#agar acchese se graph dekhne ha...toh  https://www.flickr.com/photos/swied/26819618540/






##############################################6.The one seeming as python ka dhasu visualization####################################



# 
# import pandas as pd
# import numpy as np
# import matplotlib.pyplot as plt
# import datetime
# import scipy.signal
# 
# %matplotlib inline
# plt.rcParams['figure.figsize'] = (15.0, 15.0)
# 
# print('Reading train data')
# df_train = pd.read_csv('../input/train.csv')
# 
# print('\nSize of training data: ' + str(df_train.shape))
# print('Columns:' + str(df_train.columns.values))
# print('Number of places: ' + str(len(list(set(df_train['place_id'].values.tolist())))))
# print('\n')
# 
# 
# 
# 
# 
# 
# 
# # Pull out timestamps
# times = np.squeeze(df_train.as_matrix(columns=['time']))
# 
# n_events = times.size
# n_samples = n_events
# hist_range = (-100000.0, 100000.0)
# n_bins = 100000 # One bin per 2.0 time units
# n_loops = 100
# 
# # Estimate autocorrelations
# # Randomly pull timestamps and subtract from each other. Get histogram of these values to estimate autocorrelation
# print('Estimating autocorrelation')
# all_autocorrs_global = np.zeros((n_bins, n_loops))
# for loop_n in range(n_loops):
#   hist_vals, bin_edges = np.histogram(np.random.choice(times, size=n_samples, replace=True) - \
#                                       np.random.choice(times, size=n_samples, replace=True), bins=n_bins, range=hist_range)
# all_autocorrs_global[:, loop_n] = hist_vals
# 
# # Plot the autocorrelation and fft of autocorrelation
# fig, axs = plt.subplots(2,1)
# 
# axs[0].plot(bin_edges[:-1], all_autocorrs_global)
# axs[0].set_xlim([-20000.0, 20000.0])
# axs[0].set_title('Autocorrelation of all timestamps')
# axs[0].set_xlabel('time units')
# 
# # Plot the fft of the autocorrelation
# f, psd = scipy.signal.welch(all_autocorrs_global, nperseg=25000, noverlap=20000, return_onesided=True, axis=0)
# 
# # Adjust the X axis to be in time points instead of 1/F
# f /= 2.0  # Remember that there is one bin per 4.0 time units
# f = 1.0/f # Go back to time points
# 
# # Plot the fft of the autocorrelation (The PSD of the timestamps)
# axs[1].plot(f, np.log(psd))
# axs[1].set_title('Log FFT of global autocorrelations')
# axs[1].set_xlabel('time units')
# axs[1].set_xlim([0.0, 20000.0])
# axs[1].set_xticks(np.arange(0.0, 20000.0, 1000))
# axs[1].grid(True)
# 
# fig.tight_layout()
# 
# plt.show()
# 
# 












########################################7.The one who did FFT and stuff###########################################



# # This Python 3 environment comes with many helpful analytics libraries installed
# # It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# # For example, here's several helpful packages to load in 
# 
# import numpy as np # linear algebra
# import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
# 
# # Input data files are available in the "../input/" directory.
# # For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
# 
# import matplotlib.pyplot as plt
# 
# df_train = pd.read_csv('../input/train.csv')
# df_test = pd.read_csv('../input/test.csv')
# 
# # Any results you write to the current directory are saved as output.
# 
# 
# 
# 
# 
# # Compute the histogram of the event time
# time = df_train['time']
# hist = hist = np.histogram(time,5000)
# 
# # To know the unit definition of Time
# # we can look into the frequency structure of the histogram
# hist_fft = np.absolute(np.fft.fft(hist[0]))
# plt.plot(hist_fft)
# plt.xlim([0,2500])
# plt.ylim([0,1e6])
# plt.title('FFT of event time histogram')
# plt.xlabel('1/T')
# plt.grid(True)
# plt.show()
# 
# 
# 
# 
# 
# 
# print(time.max()/78)
# 
# 
# time = df_train[df_train['place_id']==8772469670]['time']
# hist = np.histogram(time,5000)
# hist_fft = np.absolute(np.fft.fft(hist[0]))
# 
# plt.plot(hist_fft)
# plt.xlim([0,2500])
# plt.title('FFT of event time histogram')
# plt.xlabel('1/T')
# plt.grid(True)
# plt.show()
# 
# 
# 
# 
# 
# 
# T1 = time.max()/64
# T2 = time.max()/451
# print('period T1:', T1)
# print('period T2:', T2)
# 
# 
# 
# 
# # Another place_id for confirmation
# time = df_train[df_train['place_id']==4823777529]['time']
# hist = np.histogram(time,5000)
# hist_fft = np.absolute(np.fft.fft(hist[0]))
# 
# plt.plot(hist_fft)
# plt.xlim([0,2500])
# plt.title('FFT of event time histogram')
# plt.xlabel('1/T')
# plt.grid(True)
# plt.show()
# 
# 
# 
# 
# 
# # peaks at 77 and 539, same periods: 10080 and 1440
# T1 = time.max()/77
# T2 = time.max()/539
# print('period T1:', T1)
# print('period T2:', T2)
# 
# 
# 
# 

















###################################################8.The one with hell lot lotttt of graphs###################





# 
# import numpy as np
# import pandas as pd
# import os
# import matplotlib.pyplot as plt
# from scipy.stats import gaussian_kde
# import time
# import seaborn as sns 
# %matplotlib inline
# 
# 
# 
# 
# 
# 
# 
# 
# 
# df_train = pd.read_csv("../input/train.csv")
# df_test = pd.read_csv("../input/test.csv")
# df_train.head()
# 
# 
# 
# 
# 
# 
# 
# 
# # Sample them for quicker visualisations
# df_train_sample = df_train.sample(n=1000000)
# df_test_sample = df_test.sample(n=1000000)
# 
# 
# 
# 
# 
# 
# 
# 
# counts1, bins1 = np.histogram(df_train["accuracy"], bins=50)
# binsc1 = bins1[:-1] + np.diff(bins1)/2.
# 
# counts2, bins2 = np.histogram(df_test["accuracy"], bins=50)
# binsc2 = bins2[:-1] + np.diff(bins2)/2.
# 
# plt.figure(0, figsize=(14,4))
# 
# plt.subplot(121)
# plt.bar(binsc1, counts1/(counts1.sum()*1.0), width=np.diff(bins1)[0])
# plt.grid(True)
# plt.xlabel("Accuracy")
# plt.ylabel("Fraction")
# plt.title("Train")
# 
# plt.subplot(122)
# plt.bar(binsc2, counts2/(counts2.sum()*1.0), width=np.diff(bins2)[0])
# plt.grid(True)
# plt.xlabel("Accuracy")
# plt.ylabel("Fraction")
# plt.title("Test")
# 
# plt.show()
# 
# 
# 
# 
# 
# 
# 
# current_palette = sns.color_palette()
# 
# counts1, bins1 = np.histogram(df_train["time"], bins=50)
# binsc1 = bins1[:-1] + np.diff(bins1)/2.
# 
# counts2, bins2 = np.histogram(df_test["time"], bins=50)
# binsc2 = bins2[:-1] + np.diff(bins2)/2.
# 
# plt.figure(1, figsize=(12,3))
# 
# plt.subplot(121)
# plt.bar(binsc1, counts1/(counts1.sum()*1.0), width=np.diff(bins1)[0], color=current_palette[0])
# plt.grid(True)
# plt.xlabel("Time")
# plt.ylabel("Fraction")
# plt.title("Train")
# 
# plt.subplot(122)
# plt.bar(binsc2, counts2/(counts2.sum()*1.0), width=np.diff(bins2)[0], color=current_palette[1])
# plt.grid(True)
# plt.xlabel("Time")
# plt.ylabel("Fraction")
# plt.title("Test")
# 
# plt.show()
# 
# 
# 
# 
# 
# 
# plt.figure(2, figsize=(12,3))
# plt.bar(binsc1, counts1/(counts1.sum()*1.0), width=np.diff(bins1)[0], color=current_palette[0], label="Train")
# plt.bar(binsc2, counts2/(counts2.sum()*1.0), width=np.diff(bins2)[0], color=current_palette[1], label="Test")
# plt.grid(True)
# plt.xlabel("Time")
# plt.ylabel("Fraction")
# plt.title("Test")
# plt.legend()
# plt.show()
# 
# 
# 
# 
# 
# 
# 
# # Check how how frequently different locations appear
# df_placecounts = df_train["place_id"].value_counts()
# 
# counts, bins = np.histogram(df_placecounts.values, bins=50)
# binsc = bins[:-1] + np.diff(bins)/2.
# 
# plt.figure(3, figsize=(12,6))
# plt.bar(binsc, counts/(counts.sum()*1.0), width=np.diff(bins)[0])
# plt.grid(True)
# plt.xlabel("Number of place occurances")
# plt.ylabel("Fraction")
# plt.title("Train")
# plt.show()
# 
# 
# 
# 
# 
# 
# 
# # Check if accuracy of signal corresponds with time
# plt.figure(4, figsize=(12,10))
# 
# plt.subplot(211)
# plt.scatter(df_train_sample["time"], df_train_sample["accuracy"], s=1, c='k', lw=0, alpha=0.1)
# plt.xlabel("Time")
# plt.ylabel("Accuracy")
# plt.xlim(df_train_sample["time"].min(), df_train_sample["time"].max())
# plt.ylim(df_train_sample["accuracy"].min(), df_train_sample["accuracy"].max())
# plt.title("Train")
# 
# plt.subplot(212)
# plt.scatter(df_test_sample["time"], df_test_sample["accuracy"], s=1, c='k', lw=0, alpha=0.1)
# plt.xlabel("Time")
# plt.ylabel("Accuracy")
# plt.xlim(df_test_sample["time"].min(), df_test_sample["time"].max())
# plt.ylim(df_test_sample["accuracy"].min(), df_test_sample["accuracy"].max())
# plt.title("Test")
# 
# plt.show()
# 
# 
# 
# 
# 
# 
# # Does the accuracy vary with location?  Check within 100x100m spots
# df_train_sample["xround"] = df_train_sample["x"].round(decimals=1)
# df_train_sample["yround"] = df_train_sample["y"].round(decimals=1)
# df_groupxy = df_train_sample.groupby(["xround", "yround"]).agg({"accuracy":[np.mean, np.std]})
# df_groupxy.head()
# 
# 
# 
# 
# 
# 
# idx = np.asarray(list(df_groupxy.index.values))
# plt.figure(5, figsize=(14,6))
# 
# plt.subplot(121)
# plt.scatter(idx[:,0], idx[:,1], s=20, c=df_groupxy["accuracy", "mean"], marker='s', lw=0, cmap=plt.cm.viridis)
# plt.colorbar().set_label("Mean accuracy")
# plt.grid(True)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.xlim(0,10)
# plt.ylim(0,10)
# 
# plt.subplot(122)
# plt.scatter(idx[:,0], idx[:,1], s=20, c=df_groupxy["accuracy", "std"], marker='s', lw=0, cmap=plt.cm.viridis)
# plt.colorbar().set_label("Std accuracy")
# plt.grid(True)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.xlim(0,10)
# plt.ylim(0,10)
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# 
# 
# # Get a list of the top 20 places for future reference
# df_topplaces = df_placecounts.iloc[0:20]
# l_topplaces = list(df_topplaces.index)
# print(l_topplaces)
# 
# 
# 
# 
# 
# # Check if any of the top places have time correlated visits
# plt.figure(6, figsize=(14,10))
# for i in range(len(l_topplaces)):
#   place = l_topplaces[i]
# 
# df_place = df_train[df_train["place_id"]==place]
# 
# counts, bins = np.histogram(df_place["time"], bins=50, range=[df_train["time"].min(), df_train["time"].max()])
# binsc = bins[:-1] + np.diff(bins)/2.
# 
# plt.subplot(5,4,i+1)
# plt.bar(binsc, counts/(counts.sum()*1.0), width=np.diff(bins)[0])
# plt.xlim(df_train["time"].min(), df_train["time"].max())
# plt.grid(True)
# plt.xlabel("Time")
# plt.ylabel("Fraction")
# plt.gca().get_xaxis().set_ticks([])
# plt.title("pid: " + str(place))
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# 
# # Try to infer time
# plt.figure(7, figsize=(14,10))
# for i in range(len(l_topplaces)):
#   place = l_topplaces[i]
# 
# df_place = df_train[df_train["place_id"]==place]
# 
# # Try % 3600*24 to see daily trend assuming it's in seconds
# # Try %   60*24 if minutes
# counts, bins = np.histogram(df_place["time"]%(60*24), bins=50)
# binsc = bins[:-1] + np.diff(bins)/2.
# 
# plt.subplot(5,4,i+1)
# plt.bar(binsc, counts/(counts.sum()*1.0), width=np.diff(bins)[0])
# plt.grid(True)
# plt.xlabel("Time")
# plt.ylabel("Fraction")
# plt.gca().get_xaxis().set_ticks([])
# plt.title("pid: " + str(place))
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# # Add some columns to make calculations easier
# df_train["hour"] = (df_train["time"]%(60*24))/60.
# df_train["dayofweek"] = np.ceil((df_train["time"]%(60*24*7))/(60.*24))
# df_train["dayofyear"] = np.ceil((df_train["time"]%(60*24*365))/(60.*24))
# df_train.head()
# 
# 
# 
# 
# df_train_sample["hour"] = (df_train_sample["time"]%(60*24))/60.
# df_train_sample["dayofweek"] = np.ceil((df_train_sample["time"]%(60*24*7))/(60.*24))
# df_train_sample["dayofyear"] = np.ceil((df_train_sample["time"]%(60*24*365))/(60.*24))
# 
# 
# 
# 
# # Check the top 20 locations again for any weekly trends
# plt.figure(8, figsize=(14,10))
# for i in range(20):
#   place = l_topplaces[i]
# df_place = df_train[df_train["place_id"]==place]
# 
# # Group by weekday
# df_groupday = df_place.groupby("dayofweek").agg("count")
# 
# plt.subplot(5,4,i+1)
# plt.bar(df_groupday.index.values-0.5, df_groupday["time"], width=1)
# plt.grid(True)
# plt.xlabel("Day")
# plt.ylabel("Count")
# plt.title("pid: " + str(place))
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# 
# 
# plt.figure(9, figsize=(14,10))
# for i in range(20):
#   place = l_topplaces[i]
# df_place = df_train[df_train["place_id"]==place]
# 
# # Add some colums
# df_place = df_place[df_place["time"]<(60*24*365)] # Restrict to 1 year so the counts don't double up
# df_groupday = df_place.groupby("dayofyear").agg("count")
# 
# plt.subplot(5,4,i+1)
# plt.bar(df_groupday.index.values-0.5, df_groupday["time"], width=1)
# plt.grid(True)
# plt.xlabel("Day of year")
# plt.ylabel("Count")
# plt.xlim(0,365)
# plt.title("pid: " + str(place))
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# # Check the 2d distribution of (x,y) for the top 20 places
# plt.figure(10, figsize=(14,16))
# cmapm = plt.cm.viridis
# cmapm.set_bad("0.5",1.)
# 
# for i in range(len(l_topplaces)):
#   place = l_topplaces[i]
# df_place = df_train[df_train["place_id"]==place]
# counts, binsX, binsY = np.histogram2d(df_place["x"], df_place["y"], bins=100)
# extent = [binsX.min(),binsX.max(),binsY.min(),binsY.max()]
# 
# plt.subplot(5,4,i+1)
# plt.imshow(np.log10(counts.T),
#            interpolation='none',
#            origin='lower',
#            extent=extent,
#            aspect="auto",
#            cmap=cmapm)
# plt.grid(True, c='0.6', lw=0.5)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# 
# 
# # See if the accuracy varies with distance from centroid point
# plt.figure(11, figsize=(14,16))
# 
# for i in range(len(l_topplaces)):
#   plt.subplot(5,4,i+1)
# plt.gca().set_axis_bgcolor("0.5")
# place = l_topplaces[i]
# df_place = df_train[df_train["place_id"]==place]
# plt.scatter(df_place["x"], df_place["y"], s=10, c=df_place["accuracy"], lw=0, cmap=plt.cm.viridis)
# plt.grid(True, c='0.6', lw=0.5)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# 
# # See if the time varies with distance from centroid point
# plt.figure(12, figsize=(14,16))
# 
# for i in range(len(l_topplaces)):
#   plt.subplot(5,4,i+1)
# plt.gca().set_axis_bgcolor("0.5")
# place = l_topplaces[i]
# df_place = df_train[df_train["place_id"]==place]
# plt.scatter(df_place["x"], df_place["y"], s=10, c=df_place["hour"], lw=0, cmap=plt.cm.viridis)
# plt.grid(True, c='0.6', lw=0.5)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# 
# 
# # Pick a place, and see if it's shape profile stands out against background noise (i.e., every other point)
# i = 11
# place = l_topplaces[i]
# df_place = df_train[df_train["place_id"]==place]
# xmin, xmax = df_place["x"].min(), df_place["x"].max()
# ymin, ymax = df_place["y"].min(), df_place["y"].max()
# df_noise = df_train[(df_train["x"]>xmin) &
#                       (df_train["x"]<xmax) &
#                       (df_train["y"]>ymin) &
#                       (df_train["y"]<ymax)]
# 
# plt.figure(13, figsize=(8,4))
# plt.subplot(121)
# plt.gca().set_axis_bgcolor("0.5")
# plt.scatter(df_noise["x"], df_noise["y"], s=10, c='k', lw=0, alpha=0.005)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# plt.xlim(xmin,xmax)
# plt.ylim(ymin,ymax)
# plt.grid(True, c='0.6', lw=0.5)
# 
# plt.subplot(122)
# plt.gca().set_axis_bgcolor("0.5")
# plt.scatter(df_noise["x"], df_noise["y"], s=10, c='k', lw=0, alpha=0.005)
# plt.scatter(df_place["x"], df_place["y"], s=10, c=current_palette[5], lw=0, alpha=0.5)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# plt.xlim(xmin,xmax)
# plt.ylim(ymin,ymax)
# plt.grid(True, c='0.6', lw=0.5)
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# 
# 
# 
# # Go back to the x-axis stretching, and visualise some location checkins on a map
# plt.figure(14, figsize=(12,12))
# 
# for i in range(20):
#   place = l_topplaces[i]
# df_place = df_train[df_train["place_id"]==place]
# plt.scatter(df_place["x"], df_place["y"], s=3, alpha=0.5, c=plt.cm.viridis(int(i*(255/20.))), lw=0)
# 
# plt.grid(True)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.tight_layout()
# plt.xlim(0,10)
# plt.ylim(0,10)
# plt.show()
# 
# 
# 
# 
# 
# 
# # Check the stdev of x/y for each place
# df_groupplace = df_train.groupby("place_id").agg({"time":"count", "x":"std", "y":"std"})
# df_groupplace.sort_values(by="time", inplace=True, ascending=False)
# df_groupplace.head()
# 
# 
# 
# 
# 
# # Density plot
# gkde_stddevx = gaussian_kde(df_groupplace["x"][~df_groupplace["x"].isnull()].values)
# gkde_stddevy = gaussian_kde(df_groupplace["y"][~df_groupplace["y"].isnull()].values)
# 
# # Compute the functions
# rangeX = np.linspace(0, 3, 100)
# x_density = gkde_stddevx(rangeX)
# y_density = gkde_stddevy(rangeX)
# 
# plt.figure(15, figsize=(12,6))
# plt.subplot(111)
# plt.plot(rangeX, x_density, c=current_palette[0], ls="-", alpha=0.75)
# plt.plot(rangeX, y_density, c=current_palette[1], ls="-", alpha=0.75)
# plt.gca().fill_between(rangeX, 0, x_density, facecolor=current_palette[0], alpha=0.2)
# plt.gca().fill_between(rangeX, 0, y_density, facecolor=current_palette[1], alpha=0.2)
# plt.ylabel("Density")
# plt.xlabel("Std dev")
# plt.plot([], [], c=current_palette[0], alpha=0.2, linewidth=10, label="stddev x")
# plt.plot([], [], c=current_palette[1], alpha=0.2, linewidth=10, label="stddev y")
# plt.legend()
# plt.grid(True)
# 
# 
# 
# 
# 
# 
# 
# 
# # With the new found time features, we can re-check how accuracy varies with it:
# plt.figure(19, figsize=(12,6))
# plt.scatter(df_train_sample["hour"], df_train_sample["accuracy"], s=1, c='k', lw=0, alpha=0.05)
# plt.xlabel("Hour")
# plt.ylabel("Accuracy")
# plt.xlim(df_train_sample["hour"].min(), df_train_sample["hour"].max())
# plt.ylim(df_train_sample["accuracy"].min(), df_train_sample["accuracy"].max())
# plt.title("Train")
# plt.show()
# 
# 
# 
# 
# 
# 
# # Try some KDEs, if we can define the density where check-ins are likely, maybe we can assign points based on this
# # They will also be time variant
# i = 11
# place = l_topplaces[i]
# df_place = df_train[df_train["place_id"]==place]
# xmin, xmax = df_place["x"].min(), df_place["x"].max()
# ymin, ymax = df_place["y"].min(), df_place["y"].max()
# 
# # Calculate the KDE
# res = 200 # resolution
# gkde_place = gaussian_kde(np.asarray((df_place["x"], df_place["y"])))
# x_flat = np.linspace(xmin, xmax, res)
# y_flat = np.linspace(ymin, ymax, res)
# x, y = np.meshgrid(x_flat,y_flat)
# grid_coords = np.append(x.reshape(-1,1),y.reshape(-1,1),axis=1)
# z = gkde_place(grid_coords.T)
# z = z.reshape(res,res)
# 
# # Plot
# extent = [xmin,xmax,ymin,ymax]
# plt.figure(20, figsize=(12,6))
# 
# # KDE only
# plt.subplot(121)
# plt.imshow(z[::-1,:],
#            extent=extent,
#            aspect="auto",
#            cmap=plt.cm.viridis,
#            interpolation="bilinear")
# plt.grid(False)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# plt.xlim(xmin,xmax)
# plt.ylim(ymin,ymax)
# 
# # Overplot the points
# plt.subplot(122)
# plt.imshow(z[::-1,:],
#            extent=extent,
#            aspect="auto",
#            cmap=plt.cm.viridis,
#            interpolation="bilinear")
# plt.colorbar().set_label("density")
# plt.scatter(df_place["x"], df_place["y"], s=10, c='k', lw=0, alpha=0.5)
# plt.grid(False)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# plt.xlim(xmin,xmax)
# plt.ylim(ymin,ymax)
# 
# plt.tight_layout()
# plt.show()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Try some more
# pids = [0,8,9,10,11,14] # A few places
# kdes = []
# plt.figure(21, figsize=(14,5))
# for i in range(len(pids)):
#   place = l_topplaces[pids[i]]
# df_place = df_train[df_train["place_id"]==place]
# xmin, xmax = df_place["x"].min(), df_place["x"].max()
# ymin, ymax = df_place["y"].min(), df_place["y"].max()
# 
# # Calculate the KDE
# res = 50 # resolution
# gkde_place = gaussian_kde(np.asarray((df_place["x"], df_place["y"])))
# kdes.append(gkde_place) # Keep these KDEs for later
# x_flat = np.linspace(xmin, xmax, res)
# y_flat = np.linspace(ymin, ymax, res)
# x, y = np.meshgrid(x_flat,y_flat)
# grid_coords = np.append(x.reshape(-1,1),y.reshape(-1,1),axis=1)
# z = gkde_place(grid_coords.T)
# z = z.reshape(res,res)
# 
# # Plot
# extent = [xmin,xmax,ymin,ymax]
# 
# # KDE only
# plt.subplot(2,6,i+1)
# plt.imshow(z[::-1,:],
#            extent=extent,
#            aspect="auto",
#            cmap=plt.cm.viridis,
#            interpolation="bilinear")
# plt.grid(False)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# plt.xlim(xmin,xmax)
# plt.ylim(ymin,ymax)
# 
# # Overplot the points
# plt.subplot(2,6,i+7)
# plt.imshow(z[::-1,:],
#            extent=extent,
#            aspect="auto",
#            cmap=plt.cm.viridis,
#            interpolation="bilinear")
# plt.scatter(df_place["x"], df_place["y"], s=5, c='k', lw=0, alpha=0.5)
# plt.grid(False)
# plt.xlabel("X")
# plt.ylabel("Y")
# plt.title("pid: " + str(place))
# plt.xlim(xmin,xmax)
# plt.ylim(ymin,ymax)
# 
# plt.tight_layout()
# plt.show()
# 
# 




##DAMN!! THAT WAS A LOT..IF U ARE HERE..CONGOS!!




















########################################9.The one by the keep it simple type guy..some ml..###################################




# 
# import pandas as pd
# import numpy as np
# 
# df_train = pd.read_csv('../input/train.csv')
# df_test = pd.read_csv('../input/test.csv')
# 
# print('Size of training data: ' + str(df_train.shape))
# print('Size of testing data:  ' + str(df_test.shape))
# 
# print('\nColumns:' + str(df_train.columns.values))
# 
# print(df_train.describe())
# 
# #print(df_train['place_id'])
# 
# print('\nNumber of place ids: ' + str(len(list(set(df_train['place_id'].values.tolist())))))
# 
# 
# 
# 
# print('Target values:')
# print(df_train['place_id'].value_counts())
# 
# 
# 
# 
# 
# 
# 
# import matplotlib.pyplot as plt
# import matplotlib.mlab as mlab
# from matplotlib.colors import LogNorm
# 
# time = df_train['time']
# x = df_train['x']
# y = df_train['y']
# accuracy = df_train['accuracy']
# 
# n, bins, patches = plt.hist(time, 50, normed=1, facecolor='green', alpha=0.75)
# plt.title('Data density at different time dimension')
# plt.xlabel('Time')
# plt.ylabel('Frequency')
# plt.grid(True)
# plt.show()
# 
# n, bins, patches = plt.hist(accuracy, 50, normed=1, facecolor='green', alpha=0.75)
# plt.title('Histogram of location accuracy')
# plt.xlabel('Accuracy')
# plt.ylabel('Frequency')
# plt.show()
# 
# 
# 
# 
# 
# 
# 
# 
# bins = 20
# while bins <=160:
#   plt.hist2d(x, y, bins=bins, norm=LogNorm())
# plt.colorbar()
# plt.title('x and y location histogram - ' + str(bins) + ' bins')
# plt.xlabel('x')
# plt.ylabel('y')
# plt.show()
# bins = bins * 2





















######################################10. The one with the retuen of ZFTurbo####################################







# 
# # coding: utf-8
# __author__ = 'ZFTurbo: https://kaggle.com/zfturbo'
# 
# import datetime
# from heapq import nlargest
# from operator import itemgetter
# import os
# import time
# import math
# from collections import defaultdict
# 
# 
# def prep_xy(x, y):
#   range = 800
# ix = math.floor(range*x/10)
# if ix < 0:
#   ix = 0
# if ix >= range:
#   ix = range-1
# 
# iy = math.floor(range*y/10)
# if iy < 0:
#   iy = 0
# if iy >= range:
#   iy = range-1
# 
# return ix, iy
# 
# 
# def run_solution():
#   print('Preparing data...')
# f = open("../input/train.csv", "r")
# f.readline()
# total = 0
# 
# grid = defaultdict(lambda: defaultdict(int))
# grid_sorted = dict()
# 
# # Calc counts
# while 1:
#   line = f.readline().strip()
# total += 1
# 
# if line == '':
#   break
# 
# arr = line.split(",")
# row_id = arr[0]
# x = float(arr[1])
# y = float(arr[2])
# accuracy = arr[3]
# time = arr[4]
# place_id = arr[5]
# 
# ix, iy = prep_xy(x, y)
# 
# grid[(ix, iy)][place_id] += 1
# 
# f.close()
# 
# # Sort array
# for el in grid:
#   grid_sorted[el] = nlargest(3, sorted(grid[el].items()), key=itemgetter(1))
# 
# print('Generate submission...')
# sub_file = os.path.join('submission_' + str(datetime.datetime.now().strftime("%Y-%m-%d-%H-%M")) + '.csv')
# out = open(sub_file, "w")
# f = open("../input/test.csv", "r")
# f.readline()
# total = 0
# out.write("row_id,place_id\n")
# 
# while 1:
#   line = f.readline().strip()
# total += 1
# 
# if line == '':
#   break
# 
# arr = line.split(",")
# row_id = arr[0]
# x = float(arr[1])
# y = float(arr[2])
# accuracy = arr[3]
# time = arr[4]
# 
# out.write(str(row_id) + ',')
# filled = []
# 
# ix, iy = prep_xy(x, y)
# 
# s1 = (ix, iy)
# if s1 in grid_sorted:
#   topitems = grid_sorted[s1]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# out.write(' ' + topitems[i][0])
# filled.append(topitems[i][0])
# 
# out.write("\n")
# 
# out.close()
# f.close()
# 
# 
# start_time = time.time()
# run_solution()
# print("Elapsed time overall: %s seconds" % (time.time() - start_time))


























# # coding: utf-8
# __author__ = 'ZFTurbo: https://kaggle.com/zfturbo'
# 
# import datetime
# from heapq import nlargest
# from operator import itemgetter
# import os
# import time
# import math
# from collections import defaultdict
# 
# 
# def apk(actual, predicted, k=3):
#   if len(predicted) > k:
#   predicted = predicted[:k]
# 
# score = 0.0
# num_hits = 0.0
# 
# for i,p in enumerate(predicted):
#   if p in actual and p not in predicted[:i]:
#   num_hits += 1.0
# score += num_hits / (i+1.0)
# 
# if not actual:
#   return 0.0
# 
# return score / min(len(actual), k)
# 
# 
# def prep_xy(x, y, range_x, range_y):
#   ix = math.floor(range_x*x/10)
# if ix < 0:
#   ix = 0
# if ix >= range_x:
#   ix = range_x-1
# 
# iy = math.floor(range_y*y/10)
# if iy < 0:
#   iy = 0
# if iy >= range_y:
#   iy = range_y-1
# 
# return ix, iy
# 
# 
# def run_solution():
#   print('Preparing arrays...')
# f = open("../input/train.csv", "r")
# f.readline()
# total = 0
# grid_size_x = 285
# grid_size_y = 725
# # Maximum T = 786239. Take -10% of it
# split_t = math.floor((1.0 - 0.1) * 786239)
# out_of_business_time = 0.1
# split_out_of_business = math.floor((1.0 - out_of_business_time) * 786239)
# split_out_of_business_valid = math.floor((1.0 - out_of_business_time) * split_t)
# test_arr = []
# 
# grid = defaultdict(lambda: defaultdict(int))
# grid_valid = defaultdict(lambda: defaultdict(int))
# working_place = dict()
# working_place_valid = dict()
# grid_sorted = dict()
# grid_sorted_valid = dict()
# 
# # Calc counts
# train_samples = 0
# test_samples = 0
# while 1:
#   line = f.readline().strip()
# total += 1
# 
# if line == '':
#   break
# 
# arr = line.split(",")
# row_id = arr[0]
# x = float(arr[1])
# y = float(arr[2])
# accuracy = int(arr[3])
# time1 = int(arr[4])
# place_id = arr[5]
# time_of_week = time1 % 10080
# quarter_period_of_day = math.floor((time1 + 120) / (6*60)) % 4
# # day_of_week = math.floor(time_of_week / 1440)
# # hour_of_day = math.floor(time_of_week / 60) % 24
# log_month = math.log10(3+((time1 + 120.0) / (60 * 24 * 30)))
# if accuracy > 100:
#   log_month = log_month*0.5
# 
# ix, iy = prep_xy(x, y, grid_size_x, grid_size_y)
# grid[(ix, iy, quarter_period_of_day)][place_id] += log_month
# grid[(ix, iy)][place_id] += log_month
# 
# if time1 < split_t:
#   grid_valid[(ix, iy, quarter_period_of_day)][place_id] += log_month
# grid_valid[(ix, iy)][place_id] += log_month
# train_samples += 1
# if time1 >= split_out_of_business_valid:
#   working_place_valid[place_id] = 1
# else:
#   test_arr.append(arr)
# test_samples += 1
# 
# if time1 >= split_out_of_business:
#   working_place[place_id] = 1
# 
# f.close()
# 
# print('Sorting arrays...')
# for el in grid:
#   grid_sorted[el] = nlargest(3, sorted(grid[el].items()), key=itemgetter(1))
# for el in grid_valid:
#   grid_sorted_valid[el] = nlargest(3, sorted(grid_valid[el].items()), key=itemgetter(1))
# 
# print('Run validation...')
# total = 0
# score = 0.0
# score_num = 0
# accuracy_test = defaultdict(int)
# accuracy_test_count = defaultdict(int)
# for arr in test_arr:
#   total += 1
# 
# row_id = arr[0]
# x = float(arr[1])
# y = float(arr[2])
# accuracy = int(arr[3])
# time1 = int(arr[4])
# place_id = arr[5]
# time_of_week = time1 % 10080
# quarter_period_of_day = math.floor((time1 + 120) / (6*60)) % 4
# 
# filled = []
# 
# ix, iy = prep_xy(x, y, grid_size_x, grid_size_y)
# 
# s1 = (ix, iy, quarter_period_of_day)
# s2 = (ix, iy)
# 
# if len(filled) < 3 and s1 in grid_sorted_valid:
#   topitems = grid_sorted_valid[s1]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# if topitems[i][0] in working_place_valid:
#   filled.append(topitems[i][0])
# if len(filled) < 3 and s2 in grid_sorted_valid:
#   topitems = grid_sorted_valid[s2]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# if topitems[i][0] in working_place_valid:
#   filled.append(topitems[i][0])
# 
# if len(filled) < 3 and s1 in grid_sorted_valid:
#   topitems = grid_sorted_valid[s1]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# filled.append(topitems[i][0])
# 
# if len(filled) < 3 and s2 in grid_sorted_valid:
#   topitems = grid_sorted_valid[s2]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# filled.append(topitems[i][0])
# 
# scr = apk([place_id], filled, 3)
# accuracy_test[scr] += accuracy
# accuracy_test_count[scr] += 1
# score += scr
# score_num += 1
# 
# f.close()
# score /= score_num
# for el in accuracy_test:
#   print('Avg accuracy {}: {}'.format(el, accuracy_test[el]/accuracy_test_count[el]))
# 
# print('Predicted score: {}'.format(score))
# print('Train samples: ', train_samples)
# print('Test samples: ', test_samples)
# 
# print('Generate submission...')
# sub_file = os.path.join('submission_' + str(datetime.datetime.now().strftime("%Y-%m-%d-%H-%M")) + '.csv')
# out = open(sub_file, "w")
# f = open("../input/test.csv", "r")
# f.readline()
# total = 0
# count_empty = 0
# second_pass_count = 0
# out.write("row_id,place_id\n")
# 
# while 1:
#   line = f.readline().strip()
# total += 1
# 
# if line == '':
#   break
# 
# arr = line.split(",")
# row_id = arr[0]
# x = float(arr[1])
# y = float(arr[2])
# accuracy = arr[3]
# time1 = int(arr[4])
# quarter_period_of_day = math.floor((time1 + 120) / (6*60)) % 4
# 
# out.write(str(row_id) + ',')
# filled = []
# 
# ix, iy = prep_xy(x, y, grid_size_x, grid_size_y)
# 
# s1 = (ix, iy, quarter_period_of_day)
# s2 = (ix, iy)
# if len(filled) < 3 and s1 in grid_sorted:
#   topitems = grid_sorted[s1]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# if topitems[i][0] in working_place:
#   out.write(' ' + topitems[i][0])
# filled.append(topitems[i][0])
# 
# if len(filled) < 3 and s2 in grid_sorted:
#   topitems = grid_sorted[s2]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# if topitems[i][0] in working_place:
#   out.write(' ' + topitems[i][0])
# filled.append(topitems[i][0])
# 
# if len(filled) < 3 and s1 in grid_sorted:
#   topitems = grid_sorted[s1]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# out.write(' ' + topitems[i][0])
# filled.append(topitems[i][0])
# second_pass_count += 1
# 
# if len(filled) < 3 and s2 in grid_sorted:
#   topitems = grid_sorted[s2]
# for i in range(len(topitems)):
#   if topitems[i][0] in filled:
#   continue
# if len(filled) == 3:
#   break
# out.write(' ' + topitems[i][0])
# filled.append(topitems[i][0])
# second_pass_count += 1
# 
# if len(filled) < 3:
#   count_empty += 1
# out.write("\n")
# 
# print('Empty cases:', str(count_empty))
# print('Is second pass nesessary?:', str(second_pass_count))
# out.close()
# f.close()
# 
# 
# start_time = time.time()
# run_solution()
# print("Elapsed time overall: %s seconds" % (time.time() - start_time))
# 
# # Best: 0.4781861319422314


















