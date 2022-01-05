##################
# Assignment 5   #
# Deadline:      #
#  Nov. 24:      # 
#     13.30      #
##################


##
# You will look at measurement error in hotel ratings!

# 0) Clear memory and import packages (tidyverse and fixest is enough for this assignment)
rm(list=ls())

library(tidyverse)
library(fixest)
library(grid)
library(modelsummary)
library(lspline)
library(fixest)
library(grid)
library(scales)

# 1) Load Vienna and do a sample selection:
# Apply filters:  3-4 stars, no NA from stars, Vienna actual, 
#   without  extreme value: price <= 600

hotels <- read_csv("https://osf.io/y6jvb/download")

hotels <- hotels %>% filter(accommodation_type=="Hotel") %>%
  filter(city_actual=="Vienna") %>%
  filter(stars>=3 & stars<=4) %>%
  filter(!is.na(stars)) %>%
  filter(price<=600)

# 2) Create a variable which takes the log price
# And define two cutoffs: k1 = 100, k2=200 for rating count

hotels <- hotels %>% mutate( log_price = log ( price ) ) 

k1 <- 100
k2 <- 200

hotels <- hotels %>% 
  mutate( cutoffs = 1*as.numeric ( hotels$rating_count <= k1 ) +
  1*as.numeric ( hotels$rating_count <= k2 ) )


# 3-5) Run 3 regressions on DIFFERENT SAMPLES:
#     reg1: logprice = alpha + beta + rating: data = rating_count < k1
#     reg2: logprice = alpha + beta + rating: data = k1 <= rating_count < k2
#     reg3: logprice = alpha + beta + rating: data = rating_count >= k2
# and save the predicted values as: yhat1, yhat2, yhat3 into the hotels tibble

reg1 <- feols( log_price ~ rating, data = hotels %>%  filter ( rating_count < k1  ) )
reg2 <- feols( log_price ~ rating, data = hotels %>%  filter ( rating_count >= k1 & rating_count < k2 ) )
reg3 <- feols( log_price ~ rating, data = hotels %>%  filter ( rating_count >= k2 ) )


hotels$yhat1 <- ifelse(hotels$rating_count < k1, reg1$fitted.values, 0)
hotels$yhat2 <- ifelse(hotels$rating_count >= k2 & hotels$rating_count < k2, reg2$fitted.values, 0)
hotels$yhat3 <- ifelse(hotels$rating_count > k2, reg3$fitted.values, 0)


# 6) Create a simple summary table for the three models.

etable(reg1, reg2, reg3)

# 7) Create a Graph, which plots the rating agianst yhat1 and yhat3 with a simple line
# also add an annotation: yhat1: `More noisy: # of ratings<100`
#                         yhat3: `Less noisy: # of ratings>200`
#
# Take care of labels, axis limits and breaks!

#graphs for more noisy
p1 <- ggplot( data = hotels, aes( x = rating, y = yhat1 )) + 
  geom_point( color='green') +
  geom_smooth(method = "loess", color = 'red' ) +
  annotate("text", x = 3.5, y = 5.1, label ="yhat1 : More noisy: # of ratings<100", size=4.2) +
  scale_y_continuous(limits = c(3.9, 5.2), breaks = seq(1, 5.2, by = 0.2)) +
  scale_x_continuous(limits= c(2, 5), breaks = seq(1, 5.2, by = 1)) +
  labs(x = "Hotel Rating", y = "Predicted Y1")


p2 <- ggplot( data = hotels, aes( x = rating, y = yhat1 )) + 
  geom_point( color='blue') +
  geom_smooth(method = "lm", color = 'red' ) +
  annotate("text", x = 3.5, y = 5.1, label ="yhat1 : More noisy: # of ratings<100", size=4.2) +
  scale_y_continuous(limits = c(3.9, 5.2), breaks = seq(1, 5.2, by = 0.2)) +
  scale_x_continuous(limits= c(2, 5), breaks = seq(1, 5.2, by = 1)) +
  labs(x = "Hotel Rating", y = "Predicted Y1")

#graphs for less noisy
p3 <- ggplot( data = hotels, aes( x = rating, y = yhat3 )) + 
  geom_point( color='blue') +
  geom_smooth(method = "loess", color = 'red' ) +
  annotate("text", x = 3.5, y = 5.1, label ="yhat3: Less noisy: # of ratings>200", size=4.2) +
  scale_y_continuous(limits = c(3.9, 5.2), breaks = seq(1, 5.2, by = 0.2)) +
  scale_x_continuous(limits= c(2, 5), breaks = seq(1, 5.2, by = 1)) +
  labs(x = "Hotel Rating", y = "Predicted Y3")


p4 <- ggplot( data = hotels, aes( x = rating, y = yhat3 )) + 
  geom_point( color='blue') +
  geom_smooth(method = "lm", color = 'red' ) +
  annotate("text", x = 3.5, y = 5.1, label ="yhat3: Less noisy: # of ratings>200", size=4.2) +
  scale_y_continuous(limits = c(3.9, 5.2), breaks = seq(1, 5.2, by = 0.2)) +
  scale_x_continuous(limits= c(2, 5), breaks = seq(1, 5.2, by = 1)) +
  labs(x = "Hotel Rating", y = "Predicted Y3")




