#######################################
#--------Data612 Final Project--------#
#--Data Processing & Transformations--#
#######################################

# R DEPENDENCIES
library(dplyr) 
library(tidyr)
library(jsonlite)
library(stringr)
setwd("~/GitHub/612-group/final-project")

# DATA AQUISITION

## Business Data 
business<-stream_in(file("data/yelp_training_set_business.json"),verbose = F)
## User Data 
user <-stream_in(file("data/yelp_training_set_user.json"),verbose = F)
## Review  Data 
review <-stream_in(file("data/yelp_training_set_review.json"),verbose = F)

# DATA TRANSFORMATIONS


#------BUSINESS-------#
#Selected only open businesses. 
#Removed out-of-state businesses. 
#Deleted unused columns. 
#Filtered food/beverage categories. 
#Factored categories.
#---------------------#

## Subset data 
business <- business %>% 
  filter(open == "TRUE", state=="AZ") %>% 
  select(-open, -neighborhoods, -full_address, -review_count, -type, -stars) %>% 
  mutate(categories = sapply(categories, toString)) 

## Create category filter
filter <- 'Argentine| Burmese| Cambodian| Cocktail Bars|Laotian|Lebanese|Live/Raw Food|Russian|African|Champagne Bars|Kosher|Modern European|Scandinavian|Taiwanese|Tapas/Small Plates|Afghan|Brazilian|Food Trucks|Shaved Ice|Wineries|Dim Sum|Ethiopian|Fondue|Hookah Bars|Persian/Iranian|Peruvian| Polish| Seafood Markets| Tapas Bars|Halal|British| Cheese Shops|German|Spanish|Cheesesteaks|Cuban|Do-It-Yourself Food|Gastropubs|Salad|Creperies|Soup|Chocolatiers & Shops|Filipino|Food Stands|Fruits & Veggies|Meat Shops|Mongolian|Soul Food|Comfort Food| Irish|Fish & Chips|Cajun/Creole|Caribbean|Pakistani|Southern|Candy Stores|Vegan|Latin American|Breweries|French|Gay Bars|Korean|Gluten-Free|Hawaiian|Farmers Market|Vegetarian|Middle Eastern|Ethnic Food|Indian|Pubs|Chicken Wings|Dive Bars| Juice Bars & Smoothies|Vietnamese|Cafes|Wine Bars|Bagels|Diners|Hot Dogs|Tex-Mex|Donuts|Greek|Thai| Desserts|Mediterranean|Beer| Wine & Spirits|Seafood|Sushi Bars| Lounges|Steakhouses|Buffets|Japanese|Sports Bars|Delis|Bakeries|Specialty Food|Breakfast & Brunch|Ice Cream & Frozen Yogurt|Burgers|Italian| Chinese|Coffee & Tea|American (New)|Sandwiches|Fast Food|Pizza|American (Traditional)|Bars|Mexican|Food| Restaurants'

## Filter businesses using filter 
business <- business %>% 
  filter(str_detect(categories, filter)) %>% 
  mutate(categories = as.factor(categories))

#-------REVIEW--------#
#Used business subset to filter out-of-scope reviews.
#Deleted unused columns. 
#---------------------#

## filter data in business
review <- review %>% 
  filter(business_id %in% business$business_id) %>% 
  select(-type)

#--------USER---------#
#Used business subset to filter out-of-scope reviews.
#Deleted unused columns.
#Took random sample of 10k users.
#Dropped aggregate data and recalculated avgs based on subset. 
#---------------------#

## filter data in business
user <- user %>% 
  filter(user_id %in% review$user_id) %>% 
  select(-type) %>%
  mutate(user_id = as.character(user_id))

## randomly sample 10k
set.seed(50)
user<-sample_n(user, 10000)

# FILTER REVIEW/BUSINESS FROM USER SAMPLE 

## filter review data from user subset (121329 reviews were reomved)
review <- review %>% 
  filter(user_id %in% user$user_id) 

## assemble ratings data (funny/useful/cool) into singular columns.
review <- do.call(data.frame, review)

## assemble ratings data (funny/useful/cool) into singular columns.
user <- do.call(data.frame, user)

## Recalculate aggregate votes/average stars/review counts.

user <- user %>% select(user_id, name) %>% 
  inner_join(review, by="user_id") %>% 
  select(-date, -review_id)%>%
  group_by(user_id,name) %>% 
  rename(user_name = name) %>%
  summarize(review_count = n(), 
            votes.funny = round(mean(votes.funny, na.rm = TRUE)), 
            votes.useful = round(mean(votes.useful, na.rm = TRUE)), 
            votes.cool = round(mean(votes.cool, na.rm = TRUE)), 
            average_stars = round(mean(stars),2)) %>%
  ungroup()

## change factors to character
user$user_id <- as.character(user$user_id)
review$user_id<-as.character(review$user_id)
review$business_id<-as.character(review$business_id)

## filter business data from new review/user subset (496 businesses were removed)
business <- business %>% 
  filter(business_id %in% review$business_id) 

# MERGE DATAFRAME

## Created our main dataframe business and review dataframes.
## Set `Business_ID` as unique keys. 
## Set numeric user/item keys for spark. 

df <- business %>% 
  inner_join(review, by="business_id") %>% 
  transform(userID=match(user_id, unique(user_id)))%>%
  transform(itemID=match(business_id, unique(business_id)))
