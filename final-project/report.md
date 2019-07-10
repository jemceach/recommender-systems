---
title: "Final Project 607: Naive Bayes"
author: "Rajwant Mishra"
date: "May 8, 2019"
output:
  html_document:
    code_folding: hide
    fig_caption: yes
    highlight: tango
    keep_md: yes
    toc: yes
    toc_depth: 5
    toc_float: yes
---



```r
library(dplyr); library(tidyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Warning: package 'tidyr' was built under R version 3.5.3
```

```r
library(jsonlite)
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.5.3
```

```
## -- Attaching packages -------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.0     v purrr   0.2.5
## v tibble  2.0.1     v stringr 1.3.1
## v readr   1.3.1     v forcats 0.3.0
```

```
## Warning: package 'ggplot2' was built under R version 3.5.3
```

```
## Warning: package 'tibble' was built under R version 3.5.3
```

```
## Warning: package 'readr' was built under R version 3.5.2
```

```
## Warning: package 'forcats' was built under R version 3.5.2
```

```
## -- Conflicts ----------- tidyverse_conflicts() --
## x dplyr::filter()  masks stats::filter()
## x purrr::flatten() masks jsonlite::flatten()
## x dplyr::lag()     masks stats::lag()
```

```r
library(plyr)
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following object is masked from 'package:purrr':
## 
##     compact
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```



We aim to build out a system where we recommend businesses to users based on past reviews, check-in’s and location. 


```r
# sort alphabetically
df_main_test_business <- business1[ , order(names(business1))] %>% .[,c(3,1,2,7,9,10,11,12)]
df_main_train_business<- business2[ , order(names(business2))] %>% .[,c(3,1,2,7,9,10,12,13,11)] ## Keeping STARTS for Training set at index 11
head(df_main_test_business)
```

```
##              city            business_id
## 1       Ahwatukee LNdwp-9Isnd6xmBKUz4K_A
## 2          Anthem -ZUEwUytc63ZWvgXJz7gHQ
## 3          Anthem NHBfYwGcQk5RUVCbnpWRsg
## 4 Apache Junction zvFhC2Lywo6e8UaRTVj7sw
## 5 Apache Junction tgFTrk5o3_TxwokIlLxOKQ
## 6 Apache Junction zOayTVSvtaWDt8pWH1N6OQ
##                                                                                categories
## 1                                                         Burgers, Fast Food, Restaurants
## 2                        Women's Clothing, Men's Clothing, Fashion, Shopping, Accessories
## 3                                                       Web Design, Professional Services
## 4                                                                             Restaurants
## 5 Professional Services, Electronics, Shopping, Home Services, Internet Service Providers
## 6                                                                      Pizza, Restaurants
##                 name open review_count state     type
## 1         McDonald's TRUE            3    AZ business
## 2       Calvin Klein TRUE            3    AZ business
## 3 Fox Designs Studio TRUE            3    AZ business
## 4        Dairy Queen TRUE            3    AZ business
## 5           Mediacom TRUE            3    AZ business
## 6      Papa Murphy's TRUE            3    AZ business
```


```r
head(df_main_train_business)
```

```
##          city            business_id
## 1      Peoria rncjoVoEFUJGCUoC1JgnUA
## 2     Phoenix 0FNFSzCFP_rGUoJx8W7tJg
## 3     Phoenix 3f_lyB6vFK48ukH6ScvLHg
## 4     Phoenix usAsSV36QmUej8--yvN-dg
## 5 Glendale Az PzOqRohWw7F7YEPBz6AubA
## 6    Glendale gtQzAiy7D-dPU8WzT3jX3Q
##                                                             categories
## 1 Accountants, Professional Services, Tax Services, Financial Services
## 2                                      Sporting Goods, Bikes, Shopping
## 3                                                                     
## 4                                                        Food, Grocery
## 5                                     Food, Bagels, Delis, Restaurants
## 6                                  Women's Clothing, Fashion, Shopping
##                           name open review_count state     type stars
## 1    Peoria Income Tax Service TRUE            3    AZ business   5.0
## 2                  Bike Doctor TRUE            5    AZ business   5.0
## 3 Valley Permaculture Alliance TRUE            4    AZ business   5.0
## 4                    Food City TRUE            5    AZ business   3.5
## 5            Hot Bagels & Deli TRUE           14    AZ business   3.5
## 6      Barney's New York Co-op TRUE            6    CA business   4.5
```

```r
#-----------------------------------------------------------------------BACKUP ZONE START
# glimpse(df_main_train_business)
# glimpse(df_main_test_business)
# 
# unnest(df_main_train_business,.id=
#          "R")

# ddply(df_main_train_business, .(`business_id`), mutate,
#        categories_all =paste(unique(`categories`),collapse=":"))

#Not needed , keeping this code for backup
#mutate(df_main_train_business,categories_all = as.character(categories)) 
#-----------------------------------------------------------------------BACKUP ZONE ENDs

# Expanding Categories to each Row for each business.

df_main_train_business_long<- df_main_train_business %>% 
  unnest(categories ,.id= "id") %>% .[,c(2,3,4,5,6,9,1,8)] # Star column at index at 8

df_main_test_business_long<- df_main_test_business %>% 
  unnest(categories ,.id= "id") %>% .[,c(2,3,4,5,6,9,1)]

head(df_main_test_business_long)
```

```
##              business_id         name open review_count state id      city
## 1 LNdwp-9Isnd6xmBKUz4K_A   McDonald's TRUE            3    AZ  1 Ahwatukee
## 2 LNdwp-9Isnd6xmBKUz4K_A   McDonald's TRUE            3    AZ  1 Ahwatukee
## 3 LNdwp-9Isnd6xmBKUz4K_A   McDonald's TRUE            3    AZ  1 Ahwatukee
## 4 -ZUEwUytc63ZWvgXJz7gHQ Calvin Klein TRUE            3    AZ  2    Anthem
## 5 -ZUEwUytc63ZWvgXJz7gHQ Calvin Klein TRUE            3    AZ  2    Anthem
## 6 -ZUEwUytc63ZWvgXJz7gHQ Calvin Klein TRUE            3    AZ  2    Anthem
```

```r
head(df_main_train_business_long)
```

```
##              business_id                      name open review_count state
## 1 rncjoVoEFUJGCUoC1JgnUA Peoria Income Tax Service TRUE            3    AZ
## 2 rncjoVoEFUJGCUoC1JgnUA Peoria Income Tax Service TRUE            3    AZ
## 3 rncjoVoEFUJGCUoC1JgnUA Peoria Income Tax Service TRUE            3    AZ
## 4 rncjoVoEFUJGCUoC1JgnUA Peoria Income Tax Service TRUE            3    AZ
## 5 0FNFSzCFP_rGUoJx8W7tJg               Bike Doctor TRUE            5    AZ
## 6 0FNFSzCFP_rGUoJx8W7tJg               Bike Doctor TRUE            5    AZ
##              categories    city stars
## 1           Accountants  Peoria     5
## 2 Professional Services  Peoria     5
## 3          Tax Services  Peoria     5
## 4    Financial Services  Peoria     5
## 5        Sporting Goods Phoenix     5
## 6                 Bikes Phoenix     5
```




```r
# Test data 
head(user1)
```

```
##                  user_id review_count name type
## 1 LVc6c7dS2EtO7PY4MfKcEg            1    A user
## 2 Xuvj2Fq6c3mCmplVG7h21w            1    A user
## 3 Ez8ugc0Y1vAXpCFt-HrBQg            1    A user
## 4 0BPd1CJYJJoK4BewZFC5dg            1    A user
## 5 ckav1I3Pz63OqYXPdmqZ5A            1    A user
## 6 frf7yJH7m_gKJn1VFajRJw            1    A user
```

```r
# Train Data 
# ! Keeping the extra column in Train data 
head(user2)
```

```
##   votes.funny votes.useful votes.cool                user_id      name
## 1           0            7          0 CR2y7yEm4X035ZMzrTtN9Q       Jim
## 2           0            1          0 _9GXoHhdxc30ujPaQwh6Ew     Kelle
## 3           0            1          0 8mM-nqxjg6pT04kwcjMbsw Stephanie
## 4           0            2          0 Ch6CdTR2IVaVANr-RglMOg         T
## 5           0            0          0 NZrLmHRyiHmyT1JrfzkCOA      Beth
## 6          30           45         36 mWx5Sxt_dx-sYBZg6RgJHQ       Amy
##   average_stars review_count type
## 1          5.00            6 user
## 2          1.00            2 user
## 3          5.00            2 user
## 4          5.00            2 user
## 5          1.00            1 user
## 6          3.79           19 user
```


```r
# Train  User
rm(vote)
```

```
## Warning in rm(vote): object 'vote' not found
```

```r
vote <- user2$votes
df_main_train_user <- user2 %>% select( -votes) %>% bind_cols(.,vote) %>% .[,c(1,2,4,5,3,6,7,8)]

df_main_train_user_long <- gather(df_main_train_user,key= "mood", value = "rating",funny,useful,cool)
# Test User
df_main_test_user <- user1[,c(1,3,2,4)] 

# Train
head(df_main_train_user)
```

```
##                  user_id      name review_count type average_stars funny
## 1 CR2y7yEm4X035ZMzrTtN9Q       Jim            6 user          5.00     0
## 2 _9GXoHhdxc30ujPaQwh6Ew     Kelle            2 user          1.00     0
## 3 8mM-nqxjg6pT04kwcjMbsw Stephanie            2 user          5.00     0
## 4 Ch6CdTR2IVaVANr-RglMOg         T            2 user          5.00     0
## 5 NZrLmHRyiHmyT1JrfzkCOA      Beth            1 user          1.00     0
## 6 mWx5Sxt_dx-sYBZg6RgJHQ       Amy           19 user          3.79    30
##   useful cool
## 1      7    0
## 2      1    0
## 3      1    0
## 4      2    0
## 5      0    0
## 6     45   36
```

```r
head(df_main_train_user_long)
```

```
##                  user_id      name review_count type average_stars  mood
## 1 CR2y7yEm4X035ZMzrTtN9Q       Jim            6 user          5.00 funny
## 2 _9GXoHhdxc30ujPaQwh6Ew     Kelle            2 user          1.00 funny
## 3 8mM-nqxjg6pT04kwcjMbsw Stephanie            2 user          5.00 funny
## 4 Ch6CdTR2IVaVANr-RglMOg         T            2 user          5.00 funny
## 5 NZrLmHRyiHmyT1JrfzkCOA      Beth            1 user          1.00 funny
## 6 mWx5Sxt_dx-sYBZg6RgJHQ       Amy           19 user          3.79 funny
##   rating
## 1      0
## 2      0
## 3      0
## 4      0
## 5      0
## 6     30
```

```r
# Test 
head(df_main_test_user)
```

```
##                  user_id name review_count type
## 1 LVc6c7dS2EtO7PY4MfKcEg    A            1 user
## 2 Xuvj2Fq6c3mCmplVG7h21w    A            1 user
## 3 Ez8ugc0Y1vAXpCFt-HrBQg    A            1 user
## 4 0BPd1CJYJJoK4BewZFC5dg    A            1 user
## 5 ckav1I3Pz63OqYXPdmqZ5A    A            1 user
## 6 frf7yJH7m_gKJn1VFajRJw    A            1 user
```


```r
head(checkin1) # Test 
```

```
##   checkin_info.0-4 checkin_info.2-3 checkin_info.13-5 checkin_info.19-5
## 1                1                1                 1                 1
## 2                1               NA                 1                NA
## 3               NA               NA                NA                NA
## 4               NA               NA                NA                NA
## 5               NA               NA                NA                NA
## 6               NA               NA                NA                NA
##   checkin_info.15-3 checkin_info.20-5 checkin_info.1-4 checkin_info.1-6
## 1                 1                 1                1                1
## 2                NA                NA               NA                3
## 3                NA                NA               NA               NA
## 4                NA                NA                1                5
## 5                NA                NA               NA                1
## 6                NA                NA               NA               NA
##   checkin_info.18-6 checkin_info.21-3 checkin_info.20-2 checkin_info.15-2
## 1                 1                 1                NA                NA
## 2                NA                NA                 1                 1
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                 1                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.15-5 checkin_info.23-2 checkin_info.2-5 checkin_info.22-4
## 1                NA                NA               NA                NA
## 2                 2                 2                2                 1
## 3                NA                NA               NA                 4
## 4                NA                NA                1                NA
## 5                NA                NA               NA                NA
## 6                NA                NA               NA                NA
##   checkin_info.22-3 checkin_info.22-1 checkin_info.23-6 checkin_info.14-0
## 1                NA                NA                NA                NA
## 2                 3                 1                 2                 1
## 3                 1                NA                NA                NA
## 4                NA                NA                 1                NA
## 5                 1                NA                NA                NA
## 6                 1                NA                NA                NA
##   checkin_info.14-1 checkin_info.2-2 checkin_info.0-6 checkin_info.0-1
## 1                NA               NA               NA               NA
## 2                 2                1                1                1
## 3                NA               NA                1               NA
## 4                NA               NA                3               NA
## 5                NA               NA               NA               NA
## 6                NA               NA                1               NA
##   checkin_info.0-2 checkin_info.0-3 checkin_info.1-5 checkin_info.1-3
## 1               NA               NA               NA               NA
## 2                2                1                6                2
## 3               NA               NA               NA               NA
## 4               NA               NA                1               NA
## 5               NA               NA                1               NA
## 6               NA               NA               NA               NA
##   checkin_info.19-6 checkin_info.19-1 checkin_info.19-3 checkin_info.19-2
## 1                NA                NA                NA                NA
## 2                 2                 1                 1                 1
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                 1
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                 1
##   checkin_info.11-5 checkin_info.13-2 checkin_info.13-1 checkin_info.13-6
## 1                NA                NA                NA                NA
## 2                 1                 1                 1                 1
## 3                NA                NA                NA                NA
## 4                NA                 1                NA                NA
## 5                NA                NA                NA                NA
## 6                 1                NA                NA                NA
##   checkin_info.12-6 checkin_info.12-5 checkin_info.12-1 checkin_info.20-3
## 1                NA                NA                NA                NA
## 2                 1                 2                 2                 1
## 3                NA                 1                NA                NA
## 4                NA                NA                 1                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.7-0 checkin_info.16-6 checkin_info.17-1 checkin_info.17-0
## 1               NA                NA                NA                NA
## 2                1                 1                 1                 1
## 3               NA                NA                NA                NA
## 4               NA                NA                NA                NA
## 5               NA                NA                NA                NA
## 6               NA                NA                 1                NA
##   checkin_info.10-0 checkin_info.17-3 checkin_info.21-6 checkin_info.21-0
## 1                NA                NA                NA                NA
## 2                 1                 1                 1                 1
## 3                 1                NA                NA                NA
## 4                NA                NA                 1                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.16-4 checkin_info.0-5 checkin_info.15-1 checkin_info.20-4
## 1                NA               NA                NA                NA
## 2                 1               NA                NA                NA
## 3                NA                1                 1                 2
## 4                NA                1                NA                NA
## 5                NA                1                NA                NA
## 6                NA               NA                NA                NA
##   checkin_info.16-3 checkin_info.22-5 checkin_info.22-2 checkin_info.23-4
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                 1                 1                 1                 2
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                 1
## 6                NA                NA                 1                 1
##   checkin_info.23-5 checkin_info.21-4 checkin_info.8-5 checkin_info.10-4
## 1                NA                NA               NA                NA
## 2                NA                NA               NA                NA
## 3                 3                 1                1                 1
## 4                NA                NA               NA                NA
## 5                NA                NA               NA                NA
## 6                NA                NA               NA                NA
##   checkin_info.2-6 checkin_info.11-4 checkin_info.1-0 checkin_info.17-6
## 1               NA                NA               NA                NA
## 2               NA                NA               NA                NA
## 3               NA                NA               NA                NA
## 4                2                 1                1                 1
## 5               NA                NA               NA                NA
## 6               NA                NA               NA                NA
##   checkin_info.11-6 checkin_info.11-1 checkin_info.16-5 checkin_info.0-0
## 1                NA                NA                NA               NA
## 2                NA                NA                NA               NA
## 3                NA                NA                NA               NA
## 4                 1                 1                 1                1
## 5                NA                NA                NA               NA
## 6                NA                NA                NA                1
##   checkin_info.12-4 checkin_info.12-2 checkin_info.2-0 checkin_info.22-6
## 1                NA                NA               NA                NA
## 2                NA                NA               NA                NA
## 3                NA                NA               NA                NA
## 4                 1                 3               NA                NA
## 5                NA                NA               NA                NA
## 6                NA                NA                1                 1
##   checkin_info.10-5 checkin_info.7-1 checkin_info.20-6 checkin_info.9-5
## 1                NA               NA                NA               NA
## 2                NA               NA                NA               NA
## 3                NA               NA                NA               NA
## 4                NA               NA                NA               NA
## 5                NA               NA                NA               NA
## 6                NA               NA                NA               NA
##   checkin_info.19-4 checkin_info.16-0 checkin_info.11-0 checkin_info.8-2
## 1                NA                NA                NA               NA
## 2                NA                NA                NA               NA
## 3                NA                NA                NA               NA
## 4                NA                NA                NA               NA
## 5                NA                NA                NA               NA
## 6                NA                NA                NA               NA
##   checkin_info.23-0 checkin_info.11-3 checkin_info.1-1 checkin_info.20-0
## 1                NA                NA               NA                NA
## 2                NA                NA               NA                NA
## 3                NA                NA               NA                NA
## 4                NA                NA               NA                NA
## 5                NA                NA               NA                NA
## 6                NA                NA               NA                NA
##   checkin_info.12-0 checkin_info.18-4 checkin_info.18-5 checkin_info.13-4
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.14-4 checkin_info.21-5 checkin_info.10-3 checkin_info.15-6
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.8-4 checkin_info.18-2 checkin_info.7-6 checkin_info.17-4
## 1               NA                NA               NA                NA
## 2               NA                NA               NA                NA
## 3               NA                NA               NA                NA
## 4               NA                NA               NA                NA
## 5               NA                NA               NA                NA
## 6               NA                NA               NA                NA
##   checkin_info.17-2 checkin_info.13-0 checkin_info.17-5 checkin_info.18-0
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.18-1 checkin_info.18-3 checkin_info.14-5 checkin_info.14-6
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.14-2 checkin_info.14-3 checkin_info.19-0 checkin_info.11-2
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.12-3 checkin_info.16-1 checkin_info.21-1 checkin_info.10-6
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.21-2 checkin_info.15-0 checkin_info.15-4 checkin_info.22-0
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.13-3 checkin_info.20-1 checkin_info.16-2 checkin_info.10-1
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.10-2 checkin_info.9-2 checkin_info.8-0 checkin_info.9-0
## 1                NA               NA               NA               NA
## 2                NA               NA               NA               NA
## 3                NA               NA               NA               NA
## 4                NA               NA               NA               NA
## 5                NA               NA               NA               NA
## 6                NA               NA               NA               NA
##   checkin_info.9-3 checkin_info.9-4 checkin_info.6-5 checkin_info.23-1
## 1               NA               NA               NA                NA
## 2               NA               NA               NA                NA
## 3               NA               NA               NA                NA
## 4               NA               NA               NA                NA
## 5               NA               NA               NA                NA
## 6               NA               NA               NA                NA
##   checkin_info.5-5 checkin_info.9-1 checkin_info.7-3 checkin_info.7-5
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.8-3 checkin_info.5-0 checkin_info.3-0 checkin_info.4-1
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.6-4 checkin_info.6-2 checkin_info.6-3 checkin_info.6-0
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.6-1 checkin_info.5-4 checkin_info.4-2 checkin_info.5-1
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.5-3 checkin_info.5-2 checkin_info.7-2 checkin_info.7-4
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.8-6 checkin_info.8-1 checkin_info.9-6 checkin_info.6-6
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.3-2 checkin_info.23-3 checkin_info.3-4 checkin_info.4-3
## 1               NA                NA               NA               NA
## 2               NA                NA               NA               NA
## 3               NA                NA               NA               NA
## 4               NA                NA               NA               NA
## 5               NA                NA               NA               NA
## 6               NA                NA               NA               NA
##   checkin_info.5-6 checkin_info.4-0 checkin_info.1-2 checkin_info.2-4
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.3-6 checkin_info.3-3 checkin_info.4-5 checkin_info.4-4
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.3-5 checkin_info.2-1 checkin_info.3-1 checkin_info.4-6
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##      type            business_id
## 1 checkin FVpeXmiI8sa86fw63E0_0A
## 2 checkin Yrjj_LCy_bPD_PINGjhdng
## 3 checkin g8WX8u6cM_z7zBDT9_sFBw
## 4 checkin 7DDRk6CozeS__8aLCJ80-A
## 5 checkin -j3BF7gOIi-1tAHoFA3WIw
## 6 checkin eAuZtSnA6cgwR7Um7eA2rg
```

```r
head(checkin2) # Train 
```

```
##   checkin_info.11-3 checkin_info.8-5 checkin_info.15-0 checkin_info.15-3
## 1                17                1                 2                 2
## 2                NA               NA                NA                NA
## 3                 1                1                NA                 1
## 4                NA               NA                NA                NA
## 5                NA               NA                NA                NA
## 6                NA               NA                NA                NA
##   checkin_info.15-5 checkin_info.14-4 checkin_info.14-5 checkin_info.14-6
## 1                 2                 1                 3                 6
## 2                NA                NA                NA                 1
## 3                NA                NA                NA                 1
## 4                NA                NA                NA                NA
## 5                NA                NA                 1                NA
## 6                 1                NA                NA                NA
##   checkin_info.14-0 checkin_info.14-1 checkin_info.14-3 checkin_info.0-5
## 1                 2                 2                 2                1
## 2                NA                NA                NA                1
## 3                NA                NA                NA               NA
## 4                NA                NA                NA               NA
## 5                NA                NA                NA               NA
## 6                NA                NA                NA               NA
##   checkin_info.1-6 checkin_info.11-5 checkin_info.11-4 checkin_info.13-1
## 1                1                 3                11                 1
## 2                2                NA                NA                NA
## 3               NA                 4                NA                NA
## 4               NA                 2                NA                NA
## 5               NA                NA                NA                NA
## 6               NA                NA                NA                NA
##   checkin_info.11-6 checkin_info.11-1 checkin_info.13-6 checkin_info.13-5
## 1                 6                18                 5                 4
## 2                NA                NA                 1                NA
## 3                 5                NA                NA                NA
## 4                NA                 2                NA                 1
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.11-2 checkin_info.12-6 checkin_info.12-4 checkin_info.12-5
## 1                 9                 5                 8                 5
## 2                 1                 1                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                 1                NA                 1
## 5                NA                NA                NA                 2
## 6                NA                NA                NA                NA
##   checkin_info.12-2 checkin_info.12-3 checkin_info.12-0 checkin_info.12-1
## 1                12                19                20                14
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                 1
## 4                NA                 1                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.13-3 checkin_info.9-5 checkin_info.9-4 checkin_info.13-2
## 1                 1                2                1                 6
## 2                NA               NA               NA                NA
## 3                NA                1               NA                NA
## 4                NA               NA               NA                 1
## 5                NA               NA               NA                NA
## 6                NA               NA               NA                NA
##   checkin_info.20-1 checkin_info.9-6 checkin_info.16-3 checkin_info.16-1
## 1                 1                4                 1                 1
## 2                NA               NA                NA                NA
## 3                NA               NA                 1                NA
## 4                NA               NA                NA                NA
## 5                NA               NA                NA                NA
## 6                NA               NA                NA                NA
##   checkin_info.16-5 checkin_info.10-0 checkin_info.10-1 checkin_info.10-2
## 1                 1                 3                 4                 4
## 2                NA                NA                NA                NA
## 3                NA                 1                 1                NA
## 4                NA                 1                 1                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.10-3 checkin_info.10-4 checkin_info.10-5 checkin_info.10-6
## 1                 4                 1                 2                 2
## 2                NA                NA                NA                 1
## 3                 1                 1                 1                 2
## 4                NA                NA                 2                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.11-0 checkin_info.2-6 checkin_info.2-5 checkin_info.3-6
## 1                 3               NA               NA               NA
## 2                NA                2                3                1
## 3                NA               NA               NA               NA
## 4                 1               NA               NA               NA
## 5                NA               NA               NA               NA
## 6                NA               NA               NA               NA
##   checkin_info.3-5 checkin_info.19-4 checkin_info.19-2 checkin_info.18-0
## 1               NA                NA                NA                NA
## 2                1                 1                 1                 1
## 3               NA                NA                 1                NA
## 4               NA                NA                 1                NA
## 5               NA                NA                NA                NA
## 6               NA                NA                NA                NA
##   checkin_info.23-0 checkin_info.17-4 checkin_info.18-5 checkin_info.17-1
## 1                NA                NA                NA                NA
## 2                 1                 1                 4                 1
## 3                NA                 1                 1                NA
## 4                NA                NA                NA                 1
## 5                NA                NA                NA                NA
## 6                NA                NA                 1                NA
##   checkin_info.0-0 checkin_info.20-3 checkin_info.13-4 checkin_info.7-4
## 1               NA                NA                NA               NA
## 2                3                 1                NA               NA
## 3               NA                NA                 1                1
## 4               NA                NA                 2               NA
## 5               NA                NA                 1               NA
## 6               NA                NA                NA               NA
##   checkin_info.23-4 checkin_info.14-2 checkin_info.0-2 checkin_info.19-1
## 1                NA                NA               NA                NA
## 2                NA                NA               NA                NA
## 3                 1                 1                1                 1
## 4                NA                NA               NA                NA
## 5                NA                NA               NA                NA
## 6                NA                NA               NA                NA
##   checkin_info.19-0 checkin_info.19-3 checkin_info.6-4 checkin_info.6-2
## 1                NA                NA               NA               NA
## 2                NA                NA               NA               NA
## 3                 1                 1                1                1
## 4                NA                NA               NA               NA
## 5                NA                NA               NA               NA
## 6                NA                 1               NA               NA
##   checkin_info.6-3 checkin_info.9-2 checkin_info.20-5 checkin_info.7-5
## 1               NA               NA                NA               NA
## 2               NA               NA                NA               NA
## 3                1                1                 1                2
## 4               NA               NA                NA               NA
## 5               NA               NA                NA               NA
## 6               NA               NA                NA               NA
##   checkin_info.20-6 checkin_info.21-6 checkin_info.21-0 checkin_info.8-2
## 1                NA                NA                NA               NA
## 2                NA                NA                NA               NA
## 3                 1                 1                 1                1
## 4                NA                NA                NA               NA
## 5                NA                NA                NA               NA
## 6                NA                NA                NA               NA
##   checkin_info.17-6 checkin_info.15-1 checkin_info.20-0 checkin_info.16-2
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                 1                 1                 1                NA
## 5                NA                NA                NA                 1
## 6                NA                NA                NA                NA
##   checkin_info.15-4 checkin_info.18-4 checkin_info.8-4 checkin_info.6-6
## 1                NA                NA               NA               NA
## 2                NA                NA               NA               NA
## 3                NA                NA               NA               NA
## 4                NA                NA               NA               NA
## 5                 1                NA               NA               NA
## 6                NA                 1               NA               NA
##   checkin_info.18-1 checkin_info.5-4 checkin_info.7-6 checkin_info.17-5
## 1                NA               NA               NA                NA
## 2                NA               NA               NA                NA
## 3                NA               NA               NA                NA
## 4                NA               NA               NA                NA
## 5                NA               NA               NA                NA
## 6                NA               NA               NA                NA
##   checkin_info.8-6 checkin_info.16-0 checkin_info.21-1 checkin_info.8-0
## 1               NA                NA                NA               NA
## 2               NA                NA                NA               NA
## 3               NA                NA                NA               NA
## 4               NA                NA                NA               NA
## 5               NA                NA                NA               NA
## 6               NA                NA                NA               NA
##   checkin_info.17-2 checkin_info.15-2 checkin_info.15-6 checkin_info.18-2
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.18-6 checkin_info.17-3 checkin_info.19-6 checkin_info.19-5
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.9-1 checkin_info.1-1 checkin_info.18-3 checkin_info.16-6
## 1               NA               NA                NA                NA
## 2               NA               NA                NA                NA
## 3               NA               NA                NA                NA
## 4               NA               NA                NA                NA
## 5               NA               NA                NA                NA
## 6               NA               NA                NA                NA
##   checkin_info.7-2 checkin_info.17-0 checkin_info.21-2 checkin_info.13-0
## 1               NA                NA                NA                NA
## 2               NA                NA                NA                NA
## 3               NA                NA                NA                NA
## 4               NA                NA                NA                NA
## 5               NA                NA                NA                NA
## 6               NA                NA                NA                NA
##   checkin_info.23-2 checkin_info.22-6 checkin_info.8-3 checkin_info.21-4
## 1                NA                NA               NA                NA
## 2                NA                NA               NA                NA
## 3                NA                NA               NA                NA
## 4                NA                NA               NA                NA
## 5                NA                NA               NA                NA
## 6                NA                NA               NA                NA
##   checkin_info.23-3 checkin_info.20-4 checkin_info.16-4 checkin_info.21-5
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.3-2 checkin_info.22-4 checkin_info.2-3 checkin_info.2-4
## 1               NA                NA               NA               NA
## 2               NA                NA               NA               NA
## 3               NA                NA               NA               NA
## 4               NA                NA               NA               NA
## 5               NA                NA               NA               NA
## 6               NA                NA               NA               NA
##   checkin_info.20-2 checkin_info.21-3 checkin_info.7-0 checkin_info.4-2
## 1                NA                NA               NA               NA
## 2                NA                NA               NA               NA
## 3                NA                NA               NA               NA
## 4                NA                NA               NA               NA
## 5                NA                NA               NA               NA
## 6                NA                NA               NA               NA
##   checkin_info.5-6 checkin_info.5-1 checkin_info.6-5 checkin_info.6-0
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.6-1 checkin_info.9-0 checkin_info.9-3 checkin_info.4-4
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.5-3 checkin_info.5-2 checkin_info.7-3 checkin_info.7-1
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.3-4 checkin_info.8-1 checkin_info.0-3 checkin_info.22-5
## 1               NA               NA               NA                NA
## 2               NA               NA               NA                NA
## 3               NA               NA               NA                NA
## 4               NA               NA               NA                NA
## 5               NA               NA               NA                NA
## 6               NA               NA               NA                NA
##   checkin_info.23-1 checkin_info.22-0 checkin_info.22-3 checkin_info.23-6
## 1                NA                NA                NA                NA
## 2                NA                NA                NA                NA
## 3                NA                NA                NA                NA
## 4                NA                NA                NA                NA
## 5                NA                NA                NA                NA
## 6                NA                NA                NA                NA
##   checkin_info.22-2 checkin_info.0-1 checkin_info.1-0 checkin_info.22-1
## 1                NA               NA               NA                NA
## 2                NA               NA               NA                NA
## 3                NA               NA               NA                NA
## 4                NA               NA               NA                NA
## 5                NA               NA               NA                NA
## 6                NA               NA               NA                NA
##   checkin_info.23-5 checkin_info.0-6 checkin_info.1-5 checkin_info.5-0
## 1                NA               NA               NA               NA
## 2                NA               NA               NA               NA
## 3                NA               NA               NA               NA
## 4                NA               NA               NA               NA
## 5                NA               NA               NA               NA
## 6                NA               NA               NA               NA
##   checkin_info.0-4 checkin_info.3-1 checkin_info.1-4 checkin_info.1-2
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.4-3 checkin_info.4-6 checkin_info.5-5 checkin_info.4-5
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.4-0 checkin_info.4-1 checkin_info.1-3 checkin_info.3-3
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##   checkin_info.2-0 checkin_info.2-1 checkin_info.2-2 checkin_info.3-0
## 1               NA               NA               NA               NA
## 2               NA               NA               NA               NA
## 3               NA               NA               NA               NA
## 4               NA               NA               NA               NA
## 5               NA               NA               NA               NA
## 6               NA               NA               NA               NA
##      type            business_id
## 1 checkin KO9CpaSPOoqm0iCWm5scmg
## 2 checkin oRqBAYtcBYZHXA7G8FlPaA
## 3 checkin 6cy2C9aBXUwkrh4bY1DApw
## 4 checkin D0IB17N66FiyYDCzTlAI4A
## 5 checkin HLQGo3EaYVvAv22bONGkIw
## 6 checkin J6OojF0R_1OuwNlrZI-ynQ
```

```r
# Test 
df_checkin_info1 <- checkin1$checkin_info
df_main_test_checkin <- checkin1  %>% select( -checkin_info) %>% bind_cols(.,df_checkin_info1) # %>% .[,c(1,2,4,5,3,6,7,8)]

# Train
df_checkin_info2 <- checkin2$checkin_info
df_main_train_checkin <- checkin2  %>% select( -checkin_info) %>% bind_cols(.,df_checkin_info2) # %>% .[,c(1,2,4,5,3,6,7,8)]

head(df_main_train_checkin)
```

```
##      type            business_id 11-3 8-5 15-0 15-3 15-5 14-4 14-5 14-6
## 1 checkin KO9CpaSPOoqm0iCWm5scmg   17   1    2    2    2    1    3    6
## 2 checkin oRqBAYtcBYZHXA7G8FlPaA   NA  NA   NA   NA   NA   NA   NA    1
## 3 checkin 6cy2C9aBXUwkrh4bY1DApw    1   1   NA    1   NA   NA   NA    1
## 4 checkin D0IB17N66FiyYDCzTlAI4A   NA  NA   NA   NA   NA   NA   NA   NA
## 5 checkin HLQGo3EaYVvAv22bONGkIw   NA  NA   NA   NA   NA   NA    1   NA
## 6 checkin J6OojF0R_1OuwNlrZI-ynQ   NA  NA   NA   NA    1   NA   NA   NA
##   14-0 14-1 14-3 0-5 1-6 11-5 11-4 13-1 11-6 11-1 13-6 13-5 11-2 12-6 12-4
## 1    2    2    2   1   1    3   11    1    6   18    5    4    9    5    8
## 2   NA   NA   NA   1   2   NA   NA   NA   NA   NA    1   NA    1    1   NA
## 3   NA   NA   NA  NA  NA    4   NA   NA    5   NA   NA   NA   NA   NA   NA
## 4   NA   NA   NA  NA  NA    2   NA   NA   NA    2   NA    1   NA    1   NA
## 5   NA   NA   NA  NA  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 6   NA   NA   NA  NA  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
##   12-5 12-2 12-3 12-0 12-1 13-3 9-5 9-4 13-2 20-1 9-6 16-3 16-1 16-5 10-0
## 1    5   12   19   20   14    1   2   1    6    1   4    1    1    1    3
## 2   NA   NA   NA   NA   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA
## 3   NA   NA   NA   NA    1   NA   1  NA   NA   NA  NA    1   NA   NA    1
## 4    1   NA    1   NA   NA   NA  NA  NA    1   NA  NA   NA   NA   NA    1
## 5    2   NA   NA   NA   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA
## 6   NA   NA   NA   NA   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA
##   10-1 10-2 10-3 10-4 10-5 10-6 11-0 2-6 2-5 3-6 3-5 19-4 19-2 18-0 23-0
## 1    4    4    4    1    2    2    3  NA  NA  NA  NA   NA   NA   NA   NA
## 2   NA   NA   NA   NA   NA    1   NA   2   3   1   1    1    1    1    1
## 3    1   NA    1    1    1    2   NA  NA  NA  NA  NA   NA    1   NA   NA
## 4    1   NA   NA   NA    2   NA    1  NA  NA  NA  NA   NA    1   NA   NA
## 5   NA   NA   NA   NA   NA   NA   NA  NA  NA  NA  NA   NA   NA   NA   NA
## 6   NA   NA   NA   NA   NA   NA   NA  NA  NA  NA  NA   NA   NA   NA   NA
##   17-4 18-5 17-1 0-0 20-3 13-4 7-4 23-4 14-2 0-2 19-1 19-0 19-3 6-4 6-2
## 1   NA   NA   NA  NA   NA   NA  NA   NA   NA  NA   NA   NA   NA  NA  NA
## 2    1    4    1   3    1   NA  NA   NA   NA  NA   NA   NA   NA  NA  NA
## 3    1    1   NA  NA   NA    1   1    1    1   1    1    1    1   1   1
## 4   NA   NA    1  NA   NA    2  NA   NA   NA  NA   NA   NA   NA  NA  NA
## 5   NA   NA   NA  NA   NA    1  NA   NA   NA  NA   NA   NA   NA  NA  NA
## 6   NA    1   NA  NA   NA   NA  NA   NA   NA  NA   NA   NA    1  NA  NA
##   6-3 9-2 20-5 7-5 20-6 21-6 21-0 8-2 17-6 15-1 20-0 16-2 15-4 18-4 8-4
## 1  NA  NA   NA  NA   NA   NA   NA  NA   NA   NA   NA   NA   NA   NA  NA
## 2  NA  NA   NA  NA   NA   NA   NA  NA   NA   NA   NA   NA   NA   NA  NA
## 3   1   1    1   2    1    1    1   1   NA   NA   NA   NA   NA   NA  NA
## 4  NA  NA   NA  NA   NA   NA   NA  NA    1    1    1   NA   NA   NA  NA
## 5  NA  NA   NA  NA   NA   NA   NA  NA   NA   NA   NA    1    1   NA  NA
## 6  NA  NA   NA  NA   NA   NA   NA  NA   NA   NA   NA   NA   NA    1  NA
##   6-6 18-1 5-4 7-6 17-5 8-6 16-0 21-1 8-0 17-2 15-2 15-6 18-2 18-6 17-3
## 1  NA   NA  NA  NA   NA  NA   NA   NA  NA   NA   NA   NA   NA   NA   NA
## 2  NA   NA  NA  NA   NA  NA   NA   NA  NA   NA   NA   NA   NA   NA   NA
## 3  NA   NA  NA  NA   NA  NA   NA   NA  NA   NA   NA   NA   NA   NA   NA
## 4  NA   NA  NA  NA   NA  NA   NA   NA  NA   NA   NA   NA   NA   NA   NA
## 5  NA   NA  NA  NA   NA  NA   NA   NA  NA   NA   NA   NA   NA   NA   NA
## 6  NA   NA  NA  NA   NA  NA   NA   NA  NA   NA   NA   NA   NA   NA   NA
##   19-6 19-5 9-1 1-1 18-3 16-6 7-2 17-0 21-2 13-0 23-2 22-6 8-3 21-4 23-3
## 1   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA   NA  NA   NA   NA
## 2   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA   NA  NA   NA   NA
## 3   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA   NA  NA   NA   NA
## 4   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA   NA  NA   NA   NA
## 5   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA   NA  NA   NA   NA
## 6   NA   NA  NA  NA   NA   NA  NA   NA   NA   NA   NA   NA  NA   NA   NA
##   20-4 16-4 21-5 3-2 22-4 2-3 2-4 20-2 21-3 7-0 4-2 5-6 5-1 6-5 6-0 6-1
## 1   NA   NA   NA  NA   NA  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA
## 2   NA   NA   NA  NA   NA  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA
## 3   NA   NA   NA  NA   NA  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA
## 4   NA   NA   NA  NA   NA  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA
## 5   NA   NA   NA  NA   NA  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA
## 6   NA   NA   NA  NA   NA  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA
##   9-0 9-3 4-4 5-3 5-2 7-3 7-1 3-4 8-1 0-3 22-5 23-1 22-0 22-3 23-6 22-2
## 1  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA
## 2  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA
## 3  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA
## 4  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA
## 5  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA
## 6  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA
##   0-1 1-0 22-1 23-5 0-6 1-5 5-0 0-4 3-1 1-4 1-2 4-3 4-6 5-5 4-5 4-0 4-1
## 1  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 2  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 3  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 4  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 5  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 6  NA  NA   NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
##   1-3 3-3 2-0 2-1 2-2 3-0
## 1  NA  NA  NA  NA  NA  NA
## 2  NA  NA  NA  NA  NA  NA
## 3  NA  NA  NA  NA  NA  NA
## 4  NA  NA  NA  NA  NA  NA
## 5  NA  NA  NA  NA  NA  NA
## 6  NA  NA  NA  NA  NA  NA
```

```r
head(df_main_test_checkin)
```

```
##      type            business_id 0-4 2-3 13-5 19-5 15-3 20-5 1-4 1-6 18-6
## 1 checkin FVpeXmiI8sa86fw63E0_0A   1   1    1    1    1    1   1   1    1
## 2 checkin Yrjj_LCy_bPD_PINGjhdng   1  NA    1   NA   NA   NA  NA   3   NA
## 3 checkin g8WX8u6cM_z7zBDT9_sFBw  NA  NA   NA   NA   NA   NA  NA  NA   NA
## 4 checkin 7DDRk6CozeS__8aLCJ80-A  NA  NA   NA   NA   NA   NA   1   5   NA
## 5 checkin -j3BF7gOIi-1tAHoFA3WIw  NA  NA   NA   NA   NA   NA  NA   1    1
## 6 checkin eAuZtSnA6cgwR7Um7eA2rg  NA  NA   NA   NA   NA   NA  NA  NA   NA
##   21-3 20-2 15-2 15-5 23-2 2-5 22-4 22-3 22-1 23-6 14-0 14-1 2-2 0-6 0-1
## 1    1   NA   NA   NA   NA  NA   NA   NA   NA   NA   NA   NA  NA  NA  NA
## 2   NA    1    1    2    2   2    1    3    1    2    1    2   1   1   1
## 3   NA   NA   NA   NA   NA  NA    4    1   NA   NA   NA   NA  NA   1  NA
## 4   NA   NA   NA   NA   NA   1   NA   NA   NA    1   NA   NA  NA   3  NA
## 5   NA   NA   NA   NA   NA  NA   NA    1   NA   NA   NA   NA  NA  NA  NA
## 6   NA   NA   NA   NA   NA  NA   NA    1   NA   NA   NA   NA  NA   1  NA
##   0-2 0-3 1-5 1-3 19-6 19-1 19-3 19-2 11-5 13-2 13-1 13-6 12-6 12-5 12-1
## 1  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 2   2   1   6   2    2    1    1    1    1    1    1    1    1    2    2
## 3  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA    1   NA
## 4  NA  NA   1  NA   NA   NA   NA    1   NA    1   NA   NA   NA   NA    1
## 5  NA  NA   1  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 6  NA  NA  NA  NA   NA   NA   NA    1    1   NA   NA   NA   NA   NA   NA
##   20-3 7-0 16-6 17-1 17-0 10-0 17-3 21-6 21-0 16-4 0-5 15-1 20-4 16-3 22-5
## 1   NA  NA   NA   NA   NA   NA   NA   NA   NA   NA  NA   NA   NA   NA   NA
## 2    1   1    1    1    1    1    1    1    1    1  NA   NA   NA   NA   NA
## 3   NA  NA   NA   NA   NA    1   NA   NA   NA   NA   1    1    2    1    1
## 4   NA  NA   NA   NA   NA   NA   NA    1   NA   NA   1   NA   NA   NA   NA
## 5   NA  NA   NA   NA   NA   NA   NA   NA   NA   NA   1   NA   NA   NA   NA
## 6   NA  NA   NA    1   NA   NA   NA   NA   NA   NA  NA   NA   NA   NA   NA
##   22-2 23-4 23-5 21-4 8-5 10-4 2-6 11-4 1-0 17-6 11-6 11-1 16-5 0-0 12-4
## 1   NA   NA   NA   NA  NA   NA  NA   NA  NA   NA   NA   NA   NA  NA   NA
## 2   NA   NA   NA   NA  NA   NA  NA   NA  NA   NA   NA   NA   NA  NA   NA
## 3    1    2    3    1   1    1  NA   NA  NA   NA   NA   NA   NA  NA   NA
## 4   NA   NA   NA   NA  NA   NA   2    1   1    1    1    1    1   1    1
## 5   NA    1   NA   NA  NA   NA  NA   NA  NA   NA   NA   NA   NA  NA   NA
## 6    1    1   NA   NA  NA   NA  NA   NA  NA   NA   NA   NA   NA   1   NA
##   12-2 2-0 22-6 10-5 7-1 20-6 9-5 19-4 16-0 11-0 8-2 23-0 11-3 1-1 20-0
## 1   NA  NA   NA   NA  NA   NA  NA   NA   NA   NA  NA   NA   NA  NA   NA
## 2   NA  NA   NA   NA  NA   NA  NA   NA   NA   NA  NA   NA   NA  NA   NA
## 3   NA  NA   NA   NA  NA   NA  NA   NA   NA   NA  NA   NA   NA  NA   NA
## 4    3  NA   NA   NA  NA   NA  NA   NA   NA   NA  NA   NA   NA  NA   NA
## 5   NA  NA   NA   NA  NA   NA  NA   NA   NA   NA  NA   NA   NA  NA   NA
## 6   NA   1    1   NA  NA   NA  NA   NA   NA   NA  NA   NA   NA  NA   NA
##   12-0 18-4 18-5 13-4 14-4 21-5 10-3 15-6 8-4 18-2 7-6 17-4 17-2 13-0 17-5
## 1   NA   NA   NA   NA   NA   NA   NA   NA  NA   NA  NA   NA   NA   NA   NA
## 2   NA   NA   NA   NA   NA   NA   NA   NA  NA   NA  NA   NA   NA   NA   NA
## 3   NA   NA   NA   NA   NA   NA   NA   NA  NA   NA  NA   NA   NA   NA   NA
## 4   NA   NA   NA   NA   NA   NA   NA   NA  NA   NA  NA   NA   NA   NA   NA
## 5   NA   NA   NA   NA   NA   NA   NA   NA  NA   NA  NA   NA   NA   NA   NA
## 6   NA   NA   NA   NA   NA   NA   NA   NA  NA   NA  NA   NA   NA   NA   NA
##   18-0 18-1 18-3 14-5 14-6 14-2 14-3 19-0 11-2 12-3 16-1 21-1 10-6 21-2
## 1   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 2   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 3   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 4   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 5   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 6   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
##   15-0 15-4 22-0 13-3 20-1 16-2 10-1 10-2 9-2 8-0 9-0 9-3 9-4 6-5 23-1 5-5
## 1   NA   NA   NA   NA   NA   NA   NA   NA  NA  NA  NA  NA  NA  NA   NA  NA
## 2   NA   NA   NA   NA   NA   NA   NA   NA  NA  NA  NA  NA  NA  NA   NA  NA
## 3   NA   NA   NA   NA   NA   NA   NA   NA  NA  NA  NA  NA  NA  NA   NA  NA
## 4   NA   NA   NA   NA   NA   NA   NA   NA  NA  NA  NA  NA  NA  NA   NA  NA
## 5   NA   NA   NA   NA   NA   NA   NA   NA  NA  NA  NA  NA  NA  NA   NA  NA
## 6   NA   NA   NA   NA   NA   NA   NA   NA  NA  NA  NA  NA  NA  NA   NA  NA
##   9-1 7-3 7-5 8-3 5-0 3-0 4-1 6-4 6-2 6-3 6-0 6-1 5-4 4-2 5-1 5-3 5-2 7-2
## 1  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 2  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 3  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 4  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 5  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 6  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
##   7-4 8-6 8-1 9-6 6-6 3-2 23-3 3-4 4-3 5-6 4-0 1-2 2-4 3-6 3-3 4-5 4-4 3-5
## 1  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 2  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 3  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 4  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 5  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
## 6  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
##   2-1 3-1 4-6
## 1  NA  NA  NA
## 2  NA  NA  NA
## 3  NA  NA  NA
## 4  NA  NA  NA
## 5  NA  NA  NA
## 6  NA  NA  NA
```


```r
head(review1) # Test 
```

```
##                  user_id            business_id   type
## 1 2WkM3pYfx7bt46tv7u4hHA AuMz7XGkjLcIUurp_AD51w review
## 2 eHWbF0k5QOBLgQXhGdeHmg 8i5hB_dmf33NVbWE5SwoMQ review
## 3 HrjjHfDGTafXyKpQKNrYHg nvaAUTTl7oqiJDhuimNG6A review
## 4 DrWLhrK8WMZf7Jb-Oqc7ww QwaoxP5Mgm3PJuZo_4bFsw review
## 5 jDCONTPR6nyc3J7iimwzkQ 0lEp4vISRmOXa8Xz2pWhbw review
## 6 u92heLV7I15JSszhSbYYHA kctHb_sEIkeH47ldwqDxww review
```

```r
head(review2) # Train 
```

```
##   votes.funny votes.useful votes.cool                user_id
## 1           0            5          2 rLtl8ZkDX5vH5nAx9C3q5Q
## 2           0            0          0 0a2KyEL0d3Yb1V6aivbIuQ
## 3           0            1          0 0hT2KtfLiobPvh6cDC8JQg
## 4           0            2          1 uZetl9T0NcROGOyFfughhg
## 5           0            0          0 vYmM4KTsC8ZfQBg-j5MWkw
## 6           1            3          4 sqYN3lNgvPbPCTRsMFu27g
##                review_id stars       date
## 1 fWKvX83p0-ka4JS3dc6E5A     5 2011-01-26
## 2 IjZ33sJrzXqU-0X6U8NwyA     5 2011-07-27
## 3 IESLBzqUCLdSzSqm0eCSxQ     4 2012-06-14
## 4 G-WvGaISbqqaMHlNnByodA     5 2010-05-27
## 5 1uJFq2r5QfJG_6ExMRCaGw     5 2012-01-05
## 6 m2CKSsepBCoRYWxiRUsxAg     4 2007-12-13
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     text
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        My wife took me here on my birthday for breakfast and it was excellent.  The weather was perfect which made sitting outside overlooking their grounds an absolute pleasure.  Our waitress was excellent and our food arrived quickly on the semi-busy Saturday morning.  It looked like the place fills up pretty quickly so the earlier you get here the better.\n\nDo yourself a favor and get their Bloody Mary.  It was phenomenal and simply the best I've ever had.  I'm pretty sure they only use ingredients from their garden and blend them fresh when you order it.  It was amazing.\n\nWhile EVERYTHING on the menu looks excellent, I had the white truffle scrambled eggs vegetable skillet and it was tasty and delicious.  It came with 2 pieces of their griddled bread with was amazing and it absolutely made the meal complete.  It was the best "toast" I've ever had.\n\nAnyway, I can't wait to go back!
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  I have no idea why some people give bad reviews about this place. It goes to show you, you can please everyone. They are probably griping about something that their own fault...there are many people like that.\n\nIn any case, my friend and I arrived at about 5:50 PM this past Sunday. It was pretty crowded, more than I thought for a Sunday evening and thought we would have to wait forever to get a seat but they said we'll be seated when the girl comes back from seating someone else. We were seated at 5:52 and the waiter came and got our drink orders. Everyone was very pleasant from the host that seated us to the waiter to the server. The prices were very good as well. We placed our orders once we decided what we wanted at 6:02. We shared the baked spaghetti calzone and the small "Here's The Beef" pizza so we can both try them. The calzone was huge and we got the smallest one (personal) and got the small 11" pizza. Both were awesome! My friend liked the pizza better and I liked the calzone better. The calzone does have a sweetish sauce but that's how I like my sauce!\n\nWe had to box part of the pizza to take it home and we were out the door by 6:42. So, everything was great and not like these bad reviewers. That goes to show you that  you have to try these things yourself because all these bad reviewers have some serious issues.
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           love the gyro plate. Rice is so good and I also dig their candy selection :)
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Rosie, Dakota, and I LOVE Chaparral Dog Park!!! It's very convenient and surrounded by a lot of paths, a desert xeriscape, baseball fields, ballparks, and a lake with ducks.\n\nThe Scottsdale Park and Rec Dept. does a wonderful job of keeping the park clean and shaded.  You can find trash cans and poopy-pick up mitts located all over the park and paths.\n\nThe fenced in area is huge to let the dogs run, play, and sniff!
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                General Manager Scott Petello is a good egg!!! Not to go into detail, but let me assure you if you have any issues (albeit rare) speak with Scott and treat the guy with some respect as you state your case and I'd be surprised if you don't walk out totally satisfied as I just did. Like I always say..... "Mistakes are inevitable, it's how we recover from them that is important"!!!\n\nThanks to Scott and his awesome staff. You've got a customer for life!! .......... :^)
## 6 Quiessence is, simply put, beautiful.  Full windows and earthy wooden walls give a feeling of warmth inside this restaurant perched in the middle of a farm.  The restaurant seemed fairly full even on a Tuesday evening; we had secured reservations just a couple days before.\n\nMy friend and I had sampled sandwiches at the Farm Kitchen earlier that week, and were impressed enough to want to eat at the restaurant.  The crisp, fresh veggies didn't disappoint: we ordered the salad with orange and grapefruit slices and the crudites to start.  Both were very good; I didn't even know how much I liked raw radishes and turnips until I tried them with their pesto and aioli sauces.\n\nFor entrees, I ordered the lamb and my friend ordered the pork shoulder.  Service started out very good, but trailed off quickly.  Waiting for our food took a very long time (a couple seated after us received and finished their entrees before we received our's), and no one bothered to explain the situation until the maitre'd apologized almost 45 minutes later.  Apparently the chef was unhappy with the sauce on my entree, so he started anew.  This isn't really a problem, but they should have communicated this to us earlier.  For our troubles, they comped me the glass of wine I ordered, but they forgot to bring out with my entree  as I had requested.  Also, they didn't offer us bread, but I will echo the lady who whispered this to us on her way out: ask for the bread.  We received warm foccacia, apple walnut, and pomegranate slices of wonder with honey and butter.  YUM.\n\nThe entrees were both solid, but didn't quite live up to the innovation and freshness of the vegetables.  My lamb's sauce was delicious, but the meat was tough.  Maybe the vegetarian entrees are the way to go?  But our dessert, the gingerbread pear cake, was yet another winner.\n\nIf the entrees were tad more inspired, or the service weren't so spotty, this place definitely would have warranted five stars.  If I return, I'd like to try the 75$ tasting menu.  Our bill came out to about 100$ for two people, including tip, no drinks.
##     type            business_id
## 1 review 9yKzy9PApeiPPOUJEtnvkg
## 2 review ZRJwVLyzEJq1VAihDhYiow
## 3 review 6oRAC4uyJCsJl1X0WZpVSA
## 4 review _1QQZuf4zZOyFCvXc0o6Vg
## 5 review 6ozycU1RpktNG2-1BroVtw
## 6 review -yxfBYGB6SEqszmxJxd97A
```

```r
#Test
df_main_test_review <- review1  
# Train
df_review2_vote <- review2$votes
df_main_train_review <- review2  %>% select( -votes) %>% bind_cols(.,df_review2_vote) 
head(df_main_test_review)
```

```
##                  user_id            business_id   type
## 1 2WkM3pYfx7bt46tv7u4hHA AuMz7XGkjLcIUurp_AD51w review
## 2 eHWbF0k5QOBLgQXhGdeHmg 8i5hB_dmf33NVbWE5SwoMQ review
## 3 HrjjHfDGTafXyKpQKNrYHg nvaAUTTl7oqiJDhuimNG6A review
## 4 DrWLhrK8WMZf7Jb-Oqc7ww QwaoxP5Mgm3PJuZo_4bFsw review
## 5 jDCONTPR6nyc3J7iimwzkQ 0lEp4vISRmOXa8Xz2pWhbw review
## 6 u92heLV7I15JSszhSbYYHA kctHb_sEIkeH47ldwqDxww review
```

```r
head(df_main_train_review)
```

```
##                  user_id              review_id stars       date
## 1 rLtl8ZkDX5vH5nAx9C3q5Q fWKvX83p0-ka4JS3dc6E5A     5 2011-01-26
## 2 0a2KyEL0d3Yb1V6aivbIuQ IjZ33sJrzXqU-0X6U8NwyA     5 2011-07-27
## 3 0hT2KtfLiobPvh6cDC8JQg IESLBzqUCLdSzSqm0eCSxQ     4 2012-06-14
## 4 uZetl9T0NcROGOyFfughhg G-WvGaISbqqaMHlNnByodA     5 2010-05-27
## 5 vYmM4KTsC8ZfQBg-j5MWkw 1uJFq2r5QfJG_6ExMRCaGw     5 2012-01-05
## 6 sqYN3lNgvPbPCTRsMFu27g m2CKSsepBCoRYWxiRUsxAg     4 2007-12-13
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     text
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        My wife took me here on my birthday for breakfast and it was excellent.  The weather was perfect which made sitting outside overlooking their grounds an absolute pleasure.  Our waitress was excellent and our food arrived quickly on the semi-busy Saturday morning.  It looked like the place fills up pretty quickly so the earlier you get here the better.\n\nDo yourself a favor and get their Bloody Mary.  It was phenomenal and simply the best I've ever had.  I'm pretty sure they only use ingredients from their garden and blend them fresh when you order it.  It was amazing.\n\nWhile EVERYTHING on the menu looks excellent, I had the white truffle scrambled eggs vegetable skillet and it was tasty and delicious.  It came with 2 pieces of their griddled bread with was amazing and it absolutely made the meal complete.  It was the best "toast" I've ever had.\n\nAnyway, I can't wait to go back!
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  I have no idea why some people give bad reviews about this place. It goes to show you, you can please everyone. They are probably griping about something that their own fault...there are many people like that.\n\nIn any case, my friend and I arrived at about 5:50 PM this past Sunday. It was pretty crowded, more than I thought for a Sunday evening and thought we would have to wait forever to get a seat but they said we'll be seated when the girl comes back from seating someone else. We were seated at 5:52 and the waiter came and got our drink orders. Everyone was very pleasant from the host that seated us to the waiter to the server. The prices were very good as well. We placed our orders once we decided what we wanted at 6:02. We shared the baked spaghetti calzone and the small "Here's The Beef" pizza so we can both try them. The calzone was huge and we got the smallest one (personal) and got the small 11" pizza. Both were awesome! My friend liked the pizza better and I liked the calzone better. The calzone does have a sweetish sauce but that's how I like my sauce!\n\nWe had to box part of the pizza to take it home and we were out the door by 6:42. So, everything was great and not like these bad reviewers. That goes to show you that  you have to try these things yourself because all these bad reviewers have some serious issues.
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           love the gyro plate. Rice is so good and I also dig their candy selection :)
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Rosie, Dakota, and I LOVE Chaparral Dog Park!!! It's very convenient and surrounded by a lot of paths, a desert xeriscape, baseball fields, ballparks, and a lake with ducks.\n\nThe Scottsdale Park and Rec Dept. does a wonderful job of keeping the park clean and shaded.  You can find trash cans and poopy-pick up mitts located all over the park and paths.\n\nThe fenced in area is huge to let the dogs run, play, and sniff!
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                General Manager Scott Petello is a good egg!!! Not to go into detail, but let me assure you if you have any issues (albeit rare) speak with Scott and treat the guy with some respect as you state your case and I'd be surprised if you don't walk out totally satisfied as I just did. Like I always say..... "Mistakes are inevitable, it's how we recover from them that is important"!!!\n\nThanks to Scott and his awesome staff. You've got a customer for life!! .......... :^)
## 6 Quiessence is, simply put, beautiful.  Full windows and earthy wooden walls give a feeling of warmth inside this restaurant perched in the middle of a farm.  The restaurant seemed fairly full even on a Tuesday evening; we had secured reservations just a couple days before.\n\nMy friend and I had sampled sandwiches at the Farm Kitchen earlier that week, and were impressed enough to want to eat at the restaurant.  The crisp, fresh veggies didn't disappoint: we ordered the salad with orange and grapefruit slices and the crudites to start.  Both were very good; I didn't even know how much I liked raw radishes and turnips until I tried them with their pesto and aioli sauces.\n\nFor entrees, I ordered the lamb and my friend ordered the pork shoulder.  Service started out very good, but trailed off quickly.  Waiting for our food took a very long time (a couple seated after us received and finished their entrees before we received our's), and no one bothered to explain the situation until the maitre'd apologized almost 45 minutes later.  Apparently the chef was unhappy with the sauce on my entree, so he started anew.  This isn't really a problem, but they should have communicated this to us earlier.  For our troubles, they comped me the glass of wine I ordered, but they forgot to bring out with my entree  as I had requested.  Also, they didn't offer us bread, but I will echo the lady who whispered this to us on her way out: ask for the bread.  We received warm foccacia, apple walnut, and pomegranate slices of wonder with honey and butter.  YUM.\n\nThe entrees were both solid, but didn't quite live up to the innovation and freshness of the vegetables.  My lamb's sauce was delicious, but the meat was tough.  Maybe the vegetarian entrees are the way to go?  But our dessert, the gingerbread pear cake, was yet another winner.\n\nIf the entrees were tad more inspired, or the service weren't so spotty, this place definitely would have warranted five stars.  If I return, I'd like to try the 75$ tasting menu.  Our bill came out to about 100$ for two people, including tip, no drinks.
##     type            business_id funny useful cool
## 1 review 9yKzy9PApeiPPOUJEtnvkg     0      5    2
## 2 review ZRJwVLyzEJq1VAihDhYiow     0      0    0
## 3 review 6oRAC4uyJCsJl1X0WZpVSA     0      1    0
## 4 review _1QQZuf4zZOyFCvXc0o6Vg     0      2    1
## 5 review 6ozycU1RpktNG2-1BroVtw     0      0    0
## 6 review -yxfBYGB6SEqszmxJxd97A     1      3    4
```







Identified business2 does not have "stars" variable. Dropping and merging dataframes.




Bind dataframes. 







