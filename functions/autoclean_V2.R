library(tidyverse)

# #load df
# file <- "./example data/ID_breath.csv"
# 
# time_ave <- 30
# 
# df1 <- read.csv(file,
#          header = TRUE,
#          sep = ",",
#          )
# colnames(df1)[1] <- "time"
# df1$exclude <- FALSE
# 
# df <- df1 %>% pivot_longer(!c(time, exclude)) %>% select(name, time, value, exclude) %>% arrange(name, time)



#Function works
outliers <- function(t, v, df, time_ave) {
  outliers <- boxplot(df %>% dplyr::filter(time >= t - time_ave/2 & time <= t + time_ave/2) %>% .$value, plot = FALSE)$out
  return(v %in% outliers)
}

#function applied successfully!
# test <- df %>% group_by(name) %>% nest() %>% mutate(data = map(.x = data, ~{
#   .x %>% mutate(exclude = map2(.x = .x$time, .y = .x$value, .f = outliers, df = .x, time_ave = time_ave))
# })) %>% unnest(data)

  









  
