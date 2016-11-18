
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)

#Import train dataset
train <- read_csv("../input/train.csv")

train <- train %>% group_by(place_id) %>% 
    mutate(check_ins = n()) %>% 
    ungroup() %>%
    arrange(desc(check_ins))

most_popular_place <- train %>% filter(place_id == train$place_id[1])

ggplot(train %>%
        filter(x > min(most_popular_place$x),
            x < max(most_popular_place$x),
            y >  min(most_popular_place$y),
            y < max(most_popular_place$y)),
    aes(x=x, y=y, color = as.factor(place_id))) +
    geom_point(alpha = .05, size = .05) + 
    theme(legend.position="none") +
    annotate("point",
           x= most_popular_place$x,
          y =  most_popular_place$y)

ggplot(most_popular_place,
    aes(x=x, y=y, color = accuracy, size = accuracy)) +
    geom_point() +
    theme(legend.position="none")

a_random_place <- train %>% filter(place_id == sample(train$place_id, 1))

ggplot(train %>%
        filter(x > min(a_random_place$x),
            x < max(a_random_place$x),
            y >  min(a_random_place$y),
            y < max(a_random_place$y)),
    aes(x=x, y=y, color =  as.factor(place_id))) +
    geom_point(alpha = .05, size = .05) +
    theme(legend.position="none") +
    annotate("point",
           a_random_place$x,
           a_random_place$y)