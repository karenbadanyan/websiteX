---
categories: 
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2023-06-10"
description: Credit Card Fraud # the title that will show up once someone gets to this page
draft: false
image: cc2.jpeg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: cc_fraud # slug is the shorthand URL address... no spaces plz
title: Data Visualisation - Exploration

---
title: "Homework 2"
author: "Karen Badanyan"
date: "2023-06-10"
output:
  word_document:
    toc: yes
  pdf_document:
    toc: yes
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
---
---









```r
library (GGally)
```

```
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
```


# Exploring credit card fraud

We will be using a dataset with credit card transactions containing legitimate and fraud transactions. Fraud is typically well below 1% of all transactions, so a naive model that predicts that all transactions are legitimate and not fraudulent would have an accuracy of well over 99%-- pretty good, no? (well, not quite as we will see later in the course)

You can read more on credit card fraud on [Credit Card Fraud Detection Using Weighted Support Vector Machine](https://www.scirp.org/journal/paperinformation.aspx?paperid=105944)

The dataset we will use consists of credit card transactions and it includes information about each transaction including customer details, the merchant and category of purchase, and whether or not the transaction was a fraud.

## Obtain the data

The dataset is too large to be hosted on Canvas or Github, so please download it from dropbox https://www.dropbox.com/sh/q1yk8mmnbbrzavl/AAAxzRtIhag9Nc_hODafGV2ka?dl=0 and save it in your `dsb` repo, under the `data` folder


```
## Rows: 671,028
## Columns: 14
## $ trans_date_trans_time <dttm> 2019-02-22 07:32:58, 2019-02-16 15:07:20, 2019-…
## $ trans_year            <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2020, …
## $ category              <chr> "entertainment", "kids_pets", "personal_care", "…
## $ amt                   <dbl> 7.79, 3.89, 8.43, 40.00, 54.04, 95.61, 64.95, 3.…
## $ city                  <chr> "Veedersburg", "Holloway", "Arnold", "Apison", "…
## $ state                 <chr> "IN", "OH", "MO", "TN", "CO", "GA", "MN", "AL", …
## $ lat                   <dbl> 40.1186, 40.0113, 38.4305, 35.0149, 39.4584, 32.…
## $ long                  <dbl> -87.2602, -80.9701, -90.3870, -85.0164, -106.385…
## $ city_pop              <dbl> 4049, 128, 35439, 3730, 277, 1841, 136, 190178, …
## $ job                   <chr> "Development worker, community", "Child psychoth…
## $ dob                   <date> 1959-10-19, 1946-04-03, 1985-03-31, 1991-01-28,…
## $ merch_lat             <dbl> 39.41679, 39.74585, 37.73078, 34.53277, 39.95244…
## $ merch_long            <dbl> -87.52619, -81.52477, -91.36875, -84.10676, -106…
## $ is_fraud              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

The data dictionary is as follows

| column(variable)      | description                                 |
|-----------------------|---------------------------------------------|
| trans_date_trans_time | Transaction DateTime                        |
| trans_year            | Transaction year                            |
| category              | category of merchant                        |
| amt                   | amount of transaction                       |
| city                  | City of card holder                         |
| state                 | State of card holder                        |
| lat                   | Latitude location of purchase               |
| long                  | Longitude location of purchase              |
| city_pop              | card holder's city population               |
| job                   | job of card holder                          |
| dob                   | date of birth of card holder                |
| merch_lat             | Latitude Location of Merchant               |
| merch_long            | Longitude Location of Merchant              |
| is_fraud              | Whether Transaction is Fraud (1) or Not (0) |

-   In this dataset, how likely are fraudulent transactions? Generate a table that summarizes the number and frequency of fraudulent transactions per year.


```r
card_fraud %>%
  group_by(trans_year) %>%
  mutate(fraud = sum(is_fraud)) %>%
  group_by(trans_year, fraud) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * fraud / count, 2)) %>%
  ggplot(aes(x = trans_year, y = count, fill = factor(fraud))) +
  geom_bar(stat = "identity", position = "stack") +
  # add percentage of fraudulent transactions
  geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray")) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Count and Percentage of Fraudulent Transactions per Year") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = unique(card_fraud$trans_year))
```

```
## `summarise()` has grouped output by 'trans_year'. You can override using the
## `.groups` argument.
```

```
## Warning: Continuous limits supplied to discrete scale.
## ℹ Did you mean `limits = factor(...)` or `scale_*_continuous()`?
```

<img src="/blogs/homework2_2_files/figure-html/unnamed-chunk-5-1.png" width="672" />

-   How much money (in US\$ terms) are fraudulent transactions costing the company? Generate a table that summarizes the total amount of legitimate and fraudulent transactions per year and calculate the % of fraudulent transactions, in US\$ terms.


```r
card_fraud %>%
  mutate(fraud_amt = is_fraud * amt) %>%
  group_by(trans_year) %>%
  mutate(fraud_total = sum((fraud_amt))) %>%
  group_by(trans_year) %>%
  mutate(legitim_total = sum(amt)-fraud_total) %>%
  group_by(trans_year) %>%
  mutate(fraud_percent = round(100*fraud_total/sum(amt),2)) %>%
  select (trans_year,fraud_total,legitim_total,fraud_percent) %>%
  distinct ()
```

```
## # A tibble: 2 × 4
## # Groups:   trans_year [2]
##   trans_year fraud_total legitim_total fraud_percent
##        <dbl>       <dbl>         <dbl>         <dbl>
## 1       2019    1423140.     32182901.          4.23
## 2       2020     651949.     12925914.          4.8
```

-   Generate a histogram that shows the distribution of amounts charged to credit card, both for legitimate and fraudulent accounts. Also, for both types of transactions, calculate some quick summary statistics.


```r
library(scales)

card_fraud %>%
  mutate(fraud = round(is_fraud * amt, 0)) %>%
  group_by(trans_year) %>%
  mutate(fraud_per_year = sum(fraud), total_amt = round(sum(amt), 0)) %>%
  distinct(trans_year, fraud_per_year, total_amt) %>%
  ggplot() +
  geom_bar(aes(x = trans_year, y = total_amt, fill = "Total Amount"), stat = "identity", width = 0.8) +
  # add Fraud Amount per Year in the Total Amount column
  geom_bar(aes(x = trans_year, y = fraud_per_year, fill = "Fraud Amount"), stat = "identity", width = 0.4) +
  geom_text(aes(x = trans_year, y = total_amt, label = comma(total_amt)), vjust = -0.5, color = "black", size = 4, position = position_dodge(width = 0.8)) +
  geom_text(aes(x = trans_year, y = fraud_per_year, label = comma(fraud_per_year)), vjust = -0.5, color = "black", size = 4, position = position_dodge(width = 0.4)) +
  xlab("Year") +
  ylab("Amount") +
  ggtitle("Legitimate and Fraudulent Amounts per Year") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = unique(card_fraud$trans_year)) +
  scale_fill_manual(values = c("Total Amount" = "#A6CEE3", "Fraud Amount" = "#FF0000")) +
  theme_minimal()
```

```
## Warning: Continuous limits supplied to discrete scale.
## ℹ Did you mean `limits = factor(...)` or `scale_*_continuous()`?
```

<img src="/blogs/homework2_2_files/figure-html/unnamed-chunk-7-1.png" width="672" />

-   What types of purchases are most likely to be instances of fraud? Consider category of merchants and produce a bar chart that shows % of total fraudulent transactions sorted in order.


```r
card_fraud %>%
  group_by(category, is_fraud) %>%
  summarise(total_txns = n(),
         total_amounts = sum(amt)) %>% 
  mutate(percent_total_txns =  round(total_txns/sum(total_txns),2),
         percent_amounts = round(total_amounts/sum(total_amounts),4)) %>% 
  ungroup() %>% 
  filter(is_fraud == 1) %>% 
  mutate(category = fct_reorder(category, percent_amounts)) %>% 
  ggplot()+
  aes(x=percent_amounts, y = category) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_amounts)),hjust = 0.5, color = "red") + 
  scale_x_continuous(labels = scales::percent) +
  xlab("Percentage") +
  ylab("Category") +
  ggtitle("Fraud Percentage per Category")
```

```
## `summarise()` has grouped output by 'category'. You can override using the
## `.groups` argument.
```

<img src="/blogs/homework2_2_files/figure-html/unnamed-chunk-8-1.png" width="672" />

-   When is fraud more prevalent? Which days, months, hours? To create new variables to help you in your analysis, we use the `lubridate` package and the following code

```         

mutate(
  date_only = lubridate::date(trans_date_trans_time),
  month_name = lubridate::month(trans_date_trans_time, label=TRUE),
  hour = lubridate::hour(trans_date_trans_time),
  weekday = lubridate::wday(trans_date_trans_time, label = TRUE)
  )
```
-   Are older customers significantly more likely to be victims of credit card fraud? To calculate a customer's age, we use the `lubridate` package and the following code

```         
  mutate(
   age = interval(dob, trans_date_trans_time) / years(1),
    )
```


```r
library(lubridate)

card_fraud %>%
  mutate(date_only = lubridate::date(trans_date_trans_time),
  month_name = lubridate::month(trans_date_trans_time, label=TRUE),
  hour = lubridate::hour(trans_date_trans_time),
  weekday = lubridate::wday(trans_date_trans_time, label = TRUE)) %>%
  group_by(month_name) %>%
  summarise(month_fraud = sum(is_fraud)) %>%
  slice_max(month_fraud)
```

```
## # A tibble: 2 × 2
##   month_name month_fraud
##   <ord>            <dbl>
## 1 Mar                472
## 2 May                472
```

```r
card_fraud %>%
  mutate(date_only = lubridate::date(trans_date_trans_time),
  month_name = lubridate::month(trans_date_trans_time, label=TRUE),
  hour = lubridate::hour(trans_date_trans_time),
  weekday = lubridate::wday(trans_date_trans_time, label = TRUE)) %>%
  group_by(hour) %>%
  summarise(hour_fraud = sum(is_fraud)) %>%
  slice_max(hour_fraud)
```

```
## # A tibble: 1 × 2
##    hour hour_fraud
##   <int>      <dbl>
## 1    23       1012
```

```r
card_fraud %>%
  mutate(date_only = lubridate::date(trans_date_trans_time),
  month_name = lubridate::month(trans_date_trans_time, label=TRUE),
  hour = lubridate::hour(trans_date_trans_time),
  weekday = lubridate::wday(trans_date_trans_time, label = TRUE)) %>%
  group_by(weekday) %>%
  summarise(day_fraud = sum(is_fraud)) %>%
  slice_max(day_fraud)
```

```
## # A tibble: 1 × 2
##   weekday day_fraud
##   <ord>       <dbl>
## 1 Mon           639
```

```r
card_fraud %>%
  mutate(age = interval(dob, trans_date_trans_time) / years(1),) %>%
  mutate(age = round(age,0)) %>%
  group_by(age) %>%
  mutate(count = sum(is_fraud)) %>%
  ggplot(aes(x=age,y=count))+geom_col()+
  xlab("Age") +
  ylab("Number of Fraud Cases") +
  ggtitle("Fraud Distribution per Age") +
  scale_y_continuous(labels = comma)
```

<img src="/blogs/homework2_2_files/figure-html/unnamed-chunk-9-1.png" width="672" />

-   Is fraud related to distance? The distance between a card holder's home and the location of the transaction can be a feature that is related to fraud. To calculate distance, we need the latidue/longitude of card holders's home and the latitude/longitude of the transaction, and we will use the [Haversine formula](https://en.wikipedia.org/wiki/Haversine_formula) to calculate distance. I adapted code to [calculate distance between two points on earth](https://www.geeksforgeeks.org/program-distance-two-points-earth/amp/) which you can find below


```r
# distance between card holder's home and transaction
# code adapted from https://www.geeksforgeeks.org/program-distance-two-points-earth/amp/


card_fraud <- card_fraud %>%
  mutate(
    
    # convert latitude/longitude to radians
    lat1_radians = lat / 57.29577951,
    lat2_radians = merch_lat / 57.29577951,
    long1_radians = long / 57.29577951,
    long2_radians = merch_long / 57.29577951,
    
    # calculate distance in miles
    distance_miles = 3963.0 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians)),

    # calculate distance in km
    distance_km = 6377.830272 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians))

  )

card_fraud %>%
  mutate(distance_km = round(distance_km,0)) %>%
  group_by(distance_km) %>%
  mutate(count = sum(is_fraud)) %>%
  ggplot(aes(x=distance_km,y=count))+geom_col()+
  xlab("Distance (km)") +
  ylab("Number of Fraud Cases") +
  ggtitle("Fraud Distribution per Distance") +
  scale_y_continuous(labels = comma)
```

<img src="/blogs/homework2_2_files/figure-html/unnamed-chunk-10-1.png" width="672" />

```r
card_fraud %>%
 ggplot(aes(x=is_fraud,y=distance_km))+geom_boxplot() +
  xlab("Fraud") +
  ylab("Distance (km)") +
  ggtitle("Relationship of Distance and Fraud")
```

```
## Warning: Continuous x aesthetic
## ℹ did you forget `aes(group = ...)`?
```

<img src="/blogs/homework2_2_files/figure-html/unnamed-chunk-10-2.png" width="672" />

Plot a boxplot or a violin plot that looks at the relationship of distance and `is_fraud`. Does distance seem to be a useful feature in explaining fraud?

card_fraud
