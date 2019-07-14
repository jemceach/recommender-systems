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
df_business <- business %>% 
  filter(open == "TRUE", state=="AZ") %>% 
  select(-open, -neighborhoods, -full_address, -review_count, -type, -stars) %>% 
  mutate(categories = sapply(categories, toString)) 

## Create category filter
filter <- 'Argentine| Burmese| Cambodian| Cocktail Bars|Laotian|Lebanese|Live/Raw Food|Russian|African|Champagne Bars|Kosher|Modern European|Scandinavian|Taiwanese|Tapas/Small Plates|Afghan|Brazilian|Food Trucks|Shaved Ice|Wineries|Dim Sum|Ethiopian|Fondue|Hookah Bars|Persian/Iranian|Peruvian| Polish| Seafood Markets| Tapas Bars|Halal|British| Cheese Shops|German|Spanish|Cheesesteaks|Cuban|Do-It-Yourself Food|Gastropubs|Salad|Creperies|Soup|Chocolatiers & Shops|Filipino|Food Stands|Fruits & Veggies|Meat Shops|Mongolian|Soul Food|Comfort Food| Irish|Fish & Chips|Cajun/Creole|Caribbean|Pakistani|Southern|Candy Stores|Vegan|Latin American|Breweries|French|Gay Bars|Korean|Gluten-Free|Hawaiian|Farmers Market|Vegetarian|Middle Eastern|Ethnic Food|Indian|Pubs|Chicken Wings|Dive Bars| Juice Bars & Smoothies|Vietnamese|Cafes|Wine Bars|Bagels|Diners|Hot Dogs|Tex-Mex|Donuts|Greek|Thai| Desserts|Mediterranean|Beer| Wine & Spirits|Seafood|Sushi Bars| Lounges|Steakhouses|Buffets|Japanese|Sports Bars|Delis|Bakeries|Specialty Food|Breakfast & Brunch|Ice Cream & Frozen Yogurt|Burgers|Italian| Chinese|Coffee & Tea|American (New)|Sandwiches|Fast Food|Pizza|American (Traditional)|Bars|Mexican|Food| Restaurants'

## Filter businesses using filter & 
df_business <- df_business %>% 
  filter(str_detect(categories, filter)) %>% 
  mutate(categories = as.factor(categories))

#-------REVIEW--------#
#Used business subset to filter out-of-scope reviews.
#Deleted unused columns. 
#Convert ratings dataframe (funny/useful/cool) into singular columns.
#---------------------#

## filter data in df_business
df_review <- review %>% 
  filter(business_id %in% df_business$business_id) %>% 
  select(-type)

## assemble ratings data (funny/useful/cool) into singular columns.
df_review <- do.call(data.frame, df_review)

## remove factor from user_id
df_review$user_id <- as.character(df_review$user_id)
df_review$business_id <- as.character(df_review$business_id)

#--------USER---------#
#Used business subset to filter out-of-scope reviews.
#Deleted unused columns. 
#---------------------#

## filter data in df_business
df_user <- user %>% 
  filter(user_id %in% df_review$user_id) %>% 
  select(-type)

## assemble ratings data (funny/useful/cool) into singular columns.
df_user <- do.call(data.frame, df_user)


# MERGE DATAFRAME

## Created our main dataframe business and review dataframes.
## Set `Business_ID` as unique keys. 

df_main <- df_business %>% 
  inner_join(df_review, by="business_id")
