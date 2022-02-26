library(tidyverse)

# load df
 # file <- "./example data/breath_03_PDX.csv"
 # 
 # time_ave <- 30
 # 
 # df <- read.csv(file,
 #          header = TRUE,
 #          sep = ",",
 #          )
 # colnames(df)[1] <- "time"
 # df$exclude <- FALSE
 # 
 # df <- df %>% pivot_longer(!c(time, exclude)) %>% select(name, time, value, exclude) %>% arrange(name, time)
 # 
 # df <- df %>% group_by(name) %>% nest()
 # 
 # temp <- df %>% mutate(data = map(data, autoclean, time_ave)) %>% unnest(data)


autoclean <- function(df, time_ave) {

      for (i in seq_along(df$time)) {

      x <- df %>% dplyr::filter(time >= df$time[[i]] - time_ave/2 & time <= df$time[[i]] + time_ave/2)
      
      Q1 <- quantile(x$value, probs = 0.25, na.rm = TRUE)
      Q3 <- quantile(x$value, probs = 0.75, na.rm = TRUE)
      IQR_1.5 <- IQR(x$value, na.rm = TRUE) * 1.5 
      lower_limit <- Q1 - IQR_1.5
      upper_limit <- Q3 + IQR_1.5
      
      if (is.na(df$value[[i]])) {
        
        outlier <- TRUE
        
      } else {
        
        outlier <- df$value[[i]] >= Q3 + IQR_1.5 | df$value[[i]] <= Q1 - IQR_1.5
          
      }
      

      df$exclude[[i]] <- xor(outlier, df$exclude[[i]])
      
}

return(df)
  
#clean <- temp_df[temp_df$outlier != TRUE,]
#removed <- temp_df[temp_df$outlier == TRUE, ]


#ggplot(clean, aes(Time, value)) + geom_point() +
#  geom_smooth(method = "loess", span = 0.3, color = "black") +
#  geom_point(data = removed, shape = 21, fill = NA, color = "red", alpha = 0.25) 
}
