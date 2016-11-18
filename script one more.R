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
