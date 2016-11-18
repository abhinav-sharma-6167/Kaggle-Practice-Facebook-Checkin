#this is the R version of the Mad Scripts Battle by ZFTurbo:
#https://www.kaggle.com/zfturbo/facebook-v-predicting-check-ins/mad-scripts-battle/code
#the idea is very simple: split the map into n by m grid and then predict most popular 
#place_id's in each bin.

library(data.table)
library(dplyr)
library(tidyr)

train <- fread("../input/train.csv", integer64 = "character")
test <-  fread("../input/test.csv", integer64 = "character")

#create bins:
bin_it <- function(dat) {
    dat$bin_x <- cut.default(dat$x, breaks = 500, labels = FALSE)
    dat$bin_y <- cut.default(dat$y, breaks = 1000, labels = FALSE)
    dat$bins <- paste(dat$bin_x, dat$bin_y)
    dat}
train <- bin_it(train)
test <- bin_it(test)

#now we count top 3 in each bin:
train %>%
    count(bins, place_id) %>%
    arrange(desc(n)) %>% 
    mutate(obs = paste0("y_", 1:n())) %>% 
    filter(obs %in% c("y_1", "y_2", "y_3")) %>% #select top three
    select(-n) %>% 
    spread(obs, place_id) %>% #formatting for submission
    mutate(preds = paste(y_1, y_2, y_3)) %>% 
    right_join(test, by  = "bins")  %>% #joining with the test set
    select(row_id, place_id = preds) %>% 
    write.csv("preds_grid_500_1000.csv", row.names = FALSE)