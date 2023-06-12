---
categories: 
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2023-06-10"
description: ML Model for Credit Card Frauds # the title that will show up once someone gets to this page
draft: false
image: cc1.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: ccf_model # slug is the shorthand URL address... no spaces plz
title: ML Model for Credit Card Frauds

---
title: "Final Group project"\
author: "Ignacio Landerreche Velasco, Mike Dizon, Karen Badanyan"
date: "2023-06-10"
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
  pdf_document:
    toc: yes
---




# The problem: predicting credit card fraud

The goal of the project is to predict fraudulent credit card transactions.

We will be using a dataset with credit card transactions containing legitimate and fraud transactions. Fraud is typically well below 1% of all transactions, so a naive model that predicts that all transactions are legitimate and not fraudulent would have an accuracy of well over 99%-- pretty good, no? 

You can read more on credit card fraud on [Credit Card Fraud Detection Using Weighted Support Vector Machine](https://www.scirp.org/journal/paperinformation.aspx?paperid=105944)

The dataset we will use consists of credit card transactions and it includes information about each transaction including customer details, the merchant and category of purchase, and whether or not the transaction was a fraud.

## Obtain the data

The dataset is too large to be hosted on Canvas or Github, so please download it from dropbox https://www.dropbox.com/sh/q1yk8mmnbbrzavl/AAAxzRtIhag9Nc_hODafGV2ka?dl=0 and save it in your `dsb` repo, under the `data` folder.

As we will be building a classifier model using tidymodels, there's two things we need to do:

1. Define the outcome variable `is_fraud` as a factor, or categorical, variable, instead of the numerical 0-1 varaibles.
2. In tidymodels, the first level is the event of interest. If we leave our data as is, `0` is the first level, but we want to find out when we actually did (`1`) have a fraudulent transaction


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
## $ is_fraud              <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
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

We also add some of the variables we considered in our EDA for this dataset during homework 2.


```r
card_fraud <- card_fraud %>% 
  mutate( hour = hour(trans_date_trans_time),
          wday = wday(trans_date_trans_time, label = TRUE),
          month_name = month(trans_date_trans_time, label = TRUE),
          age = interval(dob, trans_date_trans_time) / years(1)
) %>% 
  rename(year = trans_year) %>% 
  
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
```

## Exploratory Data Analysis (EDA) 

You have done some EDA and you can pool together your group's expertise in which variables to use as features.
You can reuse your EDA from earlier, but we expect at least a few visualisations and/or tables to explore teh dataset and identify any useful features.

Group all variables by type and examine each variable class by class. The dataset has the following types of variables:

1.  Strings
2.  Geospatial Data
3.  Dates
4.  Date/Times
5.  Numerical

Strings are usually not a useful format for classification problems. The strings should be converted to factors, dropped, or otherwise transformed.

***Strings to Factors*** 

-   `category`, Category of Merchant
-   `job`, Job of Credit Card Holder

***Strings to Geospatial Data*** 

We have plenty of geospatial data as lat/long pairs, so I want to convert city/state to lat/long so I can compare to the other geospatial variables. This will also make it easier to compute new variables like the distance the transaction is from the home location. 

-   `city`, City of Credit Card Holder
-   `state`, State of Credit Card Holder

##  Exploring factors: how is the compactness of categories?

-   Do we have excessive number of categories? Do we want to combine some?


```r
card_fraud %>% 
  count(category, sort=TRUE)%>% 
  mutate(perc = n/sum(n))
```

```
## # A tibble: 14 × 3
##    category           n   perc
##    <chr>          <int>  <dbl>
##  1 gas_transport  68046 0.101 
##  2 grocery_pos    63791 0.0951
##  3 home           63597 0.0948
##  4 shopping_pos   60416 0.0900
##  5 kids_pets      58772 0.0876
##  6 shopping_net   50743 0.0756
##  7 entertainment  48521 0.0723
##  8 food_dining    47527 0.0708
##  9 personal_care  46843 0.0698
## 10 health_fitness 44341 0.0661
## 11 misc_pos       41244 0.0615
## 12 misc_net       32829 0.0489
## 13 grocery_net    23485 0.0350
## 14 travel         20873 0.0311
```

```r
card_fraud %>% 
  count(job, sort=TRUE) %>% 
  mutate(perc = n/sum(n))
```

```
## # A tibble: 494 × 3
##    job                            n    perc
##    <chr>                      <int>   <dbl>
##  1 Film/video editor           5106 0.00761
##  2 Exhibition designer         4728 0.00705
##  3 Naval architect             4546 0.00677
##  4 Surveyor, land/geomatics    4448 0.00663
##  5 Materials engineer          4292 0.00640
##  6 Designer, ceramics/pottery  4262 0.00635
##  7 IT trainer                  4014 0.00598
##  8 Financial adviser           3959 0.00590
##  9 Systems developer           3948 0.00588
## 10 Environmental consultant    3831 0.00571
## # ℹ 484 more rows
```


The predictors `category` and `job` are transformed into factors.


```r
card_fraud <- card_fraud %>% 
  mutate(category = factor(category),
         job = factor(job))
```

`category` has 14 unique values, and `job` has 494 unique values. The dataset is quite large, with over 670K records, so these variables don't have an excessive number of levels at first glance. However, it is worth seeing if we can compact the levels to a smaller number.

### Why do we care about the number of categories and whether they are "excessive"?

Consider the extreme case where a dataset had categories that only contained one record each. There is simply insufficient data to make correct predictions using category as a predictor on new data with that category label. Additionally, if your modeling uses dummy variables, having an extremely large number of categories will lead to the production of a huge number of predictors, which can slow down the fitting. This is fine if all the predictors are useful, but if they aren't useful (as in the case of having only one record for a category), trimming them will improve the speed and quality of the data fitting.

If I had subject matter expertise, I could manually combine categories. If you don't have subject matter expertise, or if performing this task would be too labor intensive, then you can use cutoffs based on the amount of data in a category. If the majority of the data exists in only a few categories, then it might be reasonable to keep those categories and lump everything else in an "other" category or perhaps even drop the data points in smaller categories. 




## Do all variables have sensible types?

Consider each variable and decide whether to keep, transform, or drop it. This is a mixture of Exploratory Data Analysis and Feature Engineering, but it's helpful to do some simple feature engineering as you explore the data. In this project, we have all data to begin with, so any transformations will be performed on the entire dataset. Ideally, do the transformations as a `recipe_step()` in the tidymodels framework. Then the transformations would be applied to any data the recipe was used on as part of the modeling workflow. There is less chance of data leakage or missing a step when you perform the feature engineering in the recipe.

## Which variables to keep in your model?

You have a number of variables and you have to decide which ones to use in your model. For instance, you have the latitude/lognitude of the customer, that of the merchant, the same data in radians, as well as the `distance_km` and `distance_miles`. Do you need them all? 



## Fit your workflows in smaller sample

You will be running a series of different models, along the lines of the California housing example we have seen in class. However, this dataset has 670K rows and if you try various models and run cross validation on them, your computer may slow down or crash.

Thus, we will work with a smaller sample of 10% of the values the original dataset to identify the best model, and once we have the best model we can use the full dataset to train- test our best model.



```r
# select a smaller subset
my_card_fraud <- card_fraud %>% 
  select(category, amt, is_fraud,hour, wday,age,distance_km) %>%
  # select a smaller subset, 10% of the entire dataframe 
  slice_sample(prop = 0.10) 
```


## Split the data in training - testing


```r
# **Split the data**

set.seed(123)

data_split <- initial_split(my_card_fraud, # updated data
                           prop = 0.8, 
                           strata = is_fraud)

card_fraud_train <- training(data_split) 
card_fraud_test <- testing(data_split)
```


## Cross Validation

Start with 3 CV folds to quickly get an estimate for the best model and you can increase the number of folds to 5 or 10 later.


```r
set.seed(123)
cv_folds <- vfold_cv(data = card_fraud_train, 
                          v = 3, 
                          strata = is_fraud)
cv_folds 
```

```
## #  3-fold cross-validation using stratification 
## # A tibble: 3 × 2
##   splits                id   
##   <list>                <chr>
## 1 <split [35787/17894]> Fold1
## 2 <split [35787/17894]> Fold2
## 3 <split [35788/17893]> Fold3
```


## Define a tidymodels `recipe`

What steps are you going to add to your recipe? Do you need to do any log transformations?


```r
fraud_rec <- recipe(is_fraud ~ ., data = card_fraud_train) %>%
  step_log(amt) %>%
  step_novel(all_nominal(), -all_outcomes()) %>% # Use before `step_dummy()` so new level is dummified
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric(), -all_outcomes())  %>% 
  step_normalize(all_numeric())
```

Once you have your recipe, you can check the pre-processed dataframe 


```r
prepped_data <- 
  fraud_rec %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

glimpse(prepped_data)
```

```
## Rows: 53,681
## Columns: 25
## $ amt                     <dbl> 0.90449051, 0.31158321, 0.64559109, 0.54732942…
## $ hour                    <dbl> -1.00562704, 0.31626693, 0.61002114, -0.124364…
## $ age                     <dbl> 0.11719493, 1.58357901, -1.34055085, -0.941650…
## $ distance_km             <dbl> -1.0943379468, 0.2750032401, 0.0707683369, -0.…
## $ is_fraud                <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_food_dining    <dbl> -0.2729857, -0.2729857, -0.2729857, -0.2729857…
## $ category_gas_transport  <dbl> -0.338546, -0.338546, -0.338546, -0.338546, -0…
## $ category_grocery_net    <dbl> -0.1872784, -0.1872784, -0.1872784, -0.1872784…
## $ category_grocery_pos    <dbl> 3.1181967, -0.3206922, -0.3206922, -0.3206922,…
## $ category_health_fitness <dbl> -0.2670693, -0.2670693, -0.2670693, -0.2670693…
## $ category_home           <dbl> -0.3217156, -0.3217156, -0.3217156, -0.3217156…
## $ category_kids_pets      <dbl> -0.3093666, 3.2323503, -0.3093666, -0.3093666,…
## $ category_misc_net       <dbl> -0.2274704, -0.2274704, -0.2274704, -0.2274704…
## $ category_misc_pos       <dbl> -0.2557224, -0.2557224, -0.2557224, -0.2557224…
## $ category_personal_care  <dbl> -0.2767879, -0.2767879, -0.2767879, -0.2767879…
## $ category_shopping_net   <dbl> -0.2889258, -0.2889258, 3.4610319, -0.2889258,…
## $ category_shopping_pos   <dbl> -0.3162572, -0.3162572, -0.3162572, -0.3162572…
## $ category_travel         <dbl> -0.1798504, -0.1798504, -0.1798504, -0.1798504…
## $ wday_1                  <dbl> -1.2718546, 1.5403772, -0.3344440, 0.1342613, …
## $ wday_2                  <dbl> 1.703419, 0.259762, -0.702676, -1.183895, -0.7…
## $ wday_3                  <dbl> -1.0590924, -0.6985916, 1.4644132, 0.7434116, …
## $ wday_4                  <dbl> 0.9873475, -1.1911966, -0.1019246, 1.2052019, …
## $ wday_5                  <dbl> -0.3768631, -1.2799549, -0.9412954, -0.8284090…
## $ wday_6                  <dbl> 0.2513726, -0.7823282, 1.6296403, -0.7823282, …
## $ wday_7                  <dbl> -0.03326251, -0.33967450, -1.05463580, 1.80520…
```


## Define various models

You should define the following classification models:

1. Logistic regression, using the `glm` engine
2. Decision tree, using the `C5.0` engine
3. Random Forest, using  the `ranger` engine and setting `importance = "impurity"`)  
4. A boosted tree using Extreme Gradient Boosting, and the `xgboost` engine
5. A k-nearest neighbours,  using 4 nearest_neighbors and the `kknn` engine  


```r
## Model Building 

# 1. Pick a `model type`
# 2. set the `engine`
# 3. Set the `mode`:  classification

# Logistic regression
log_spec <-  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

# Show your model specification
log_spec
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
# Decision Tree
tree_spec <- decision_tree() %>%
  set_engine(engine = "C5.0") %>%
  set_mode("classification")

tree_spec
```

```
## Decision Tree Model Specification (classification)
## 
## Computational engine: C5.0
```

```r
# Random Forest
library(ranger)

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")


# Boosted tree (XGBoost)
library(xgboost)
```

```
## 
## Attaching package: 'xgboost'
```

```
## The following object is masked from 'package:dplyr':
## 
##     slice
```

```r
xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

# K-nearest neighbour (k-NN)
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification")
```

## Bundle recipe and model with `workflows`


```r
## Bundle recipe and model with `workflows`


log_wflow <- # new workflow object
 workflow() %>% # use workflow function
 add_recipe(fraud_rec) %>%   # use the new recipe
 add_model(log_spec)   # add your model spec

# show object
log_wflow
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: logistic_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 5 Recipe Steps
## 
## • step_log()
## • step_novel()
## • step_dummy()
## • step_zv()
## • step_normalize()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
## A few more workflows

tree_wflow <-
 workflow() %>%
 add_recipe(fraud_rec) %>% 
 add_model(tree_spec) 

rf_wflow <-
 workflow() %>%
 add_recipe(fraud_rec) %>% 
 add_model(rf_spec) 

xgb_wflow <-
 workflow() %>%
 add_recipe(fraud_rec) %>% 
 add_model(xgb_spec)

knn_wflow <-
 workflow() %>%
 add_recipe(fraud_rec) %>% 
 add_model(knn_spec)
```


## Fit models

You may want to compare the time it takes to fit each model. `tic()` starts a simple timer and `toc()` stops it


```r
tic()
log_res <- log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
```

```
## → A | warning: prediction from a rank-deficient fit may be misleading
```

```
## There were issues with some computations   A: x1
```

```
## There were issues with some computations   A: x2
```

```
## There were issues with some computations   A: x3
```

```
## 
```

```r
time <- toc()
```

```
## 2.252 sec elapsed
```

```r
log_time <- time[[4]]

## Decision Tree results

tic()
tree_res <-
  tree_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 
time <- toc()
```

```
## 12.822 sec elapsed
```

```r
tree_time <- time[[4]]

tree_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n  std_err .config             
##   <chr>     <chr>      <dbl> <int>    <dbl> <chr>               
## 1 accuracy  binary     0.998     3 0.000264 Preprocessor1_Model1
## 2 f_meas    binary     0.735     3 0.0279   Preprocessor1_Model1
## 3 kap       binary     0.734     3 0.0280   Preprocessor1_Model1
## 4 precision binary     0.879     3 0.0327   Preprocessor1_Model1
## 5 recall    binary     0.636     3 0.0405   Preprocessor1_Model1
## 6 roc_auc   binary     0.867     3 0.0183   Preprocessor1_Model1
## 7 sens      binary     0.636     3 0.0405   Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.000135 Preprocessor1_Model1
```

```r
## Random Forest

tic()
rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 
time <- toc()
```

```
## 20.011 sec elapsed
```

```r
rf_time <- time[[4]]

rf_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n   std_err .config             
##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.997     3 0.000238  Preprocessor1_Model1
## 2 f_meas    binary     0.591     3 0.0379    Preprocessor1_Model1
## 3 kap       binary     0.590     3 0.0379    Preprocessor1_Model1
## 4 precision binary     0.967     3 0.0171    Preprocessor1_Model1
## 5 recall    binary     0.428     3 0.0402    Preprocessor1_Model1
## 6 roc_auc   binary     0.963     3 0.00558   Preprocessor1_Model1
## 7 sens      binary     0.428     3 0.0402    Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.0000374 Preprocessor1_Model1
```

```r
## Boosted tree - XGBoost

tic()
xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 
time <- toc()
```

```
## 2.641 sec elapsed
```

```r
xgb_time <- time[[4]]

xgb_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n   std_err .config             
##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.998     3 0.000258  Preprocessor1_Model1
## 2 f_meas    binary     0.749     3 0.0265    Preprocessor1_Model1
## 3 kap       binary     0.748     3 0.0266    Preprocessor1_Model1
## 4 precision binary     0.889     3 0.0238    Preprocessor1_Model1
## 5 recall    binary     0.650     3 0.0413    Preprocessor1_Model1
## 6 roc_auc   binary     0.978     3 0.00995   Preprocessor1_Model1
## 7 sens      binary     0.650     3 0.0413    Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.0000990 Preprocessor1_Model1
```

```r
## K-nearest neighbour

tic()
knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
    ) 
time <- toc()
```

```
## 111.061 sec elapsed
```

```r
knn_time <- time[[4]]

knn_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n   std_err .config             
##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.995     3 0.000197  Preprocessor1_Model1
## 2 f_meas    binary     0.443     3 0.00821   Preprocessor1_Model1
## 3 kap       binary     0.441     3 0.00816   Preprocessor1_Model1
## 4 precision binary     0.587     3 0.00637   Preprocessor1_Model1
## 5 recall    binary     0.356     3 0.00831   Preprocessor1_Model1
## 6 roc_auc   binary     0.742     3 0.00809   Preprocessor1_Model1
## 7 sens      binary     0.356     3 0.00831   Preprocessor1_Model1
## 8 spec      binary     0.999     3 0.0000679 Preprocessor1_Model1
```

## Compare models


```r
## Model Comparison

log_metrics <- 
  log_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Logistic Regression",
         time = log_time)

# add mode models here

tree_metrics <- 
  tree_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Decision Tree",
         time = tree_time)

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest",
         time = rf_time)

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost",
         time = xgb_time)

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Knn",
         time = knn_time)

# create dataframe with all models
model_compare <- bind_rows(log_metrics,
                            tree_metrics,
                            rf_metrics,
                           xgb_metrics,
                           knn_metrics
                      ) %>% 
  # get rid of 'sec elapsed' and turn it into a number
  mutate(time = str_sub(time, end = -13) %>% 
           as.double()
         )

#Pivot wider to create barplot
  model_comp <- model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# show mean are under the curve (ROC-AUC) for every model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>% # order results
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
   geom_text(
     size = 3,
     aes(label = round(mean_roc_auc, 2), 
         y = mean_roc_auc + 0.08),
     vjust = 1
  )+
  theme_light()+
  theme(legend.position = "none")+
  labs(y = NULL)
```

<img src="/blogs/final_group_project_files/figure-html/compare_models-1.png" width="672" />

## Which metric to use

This is a highly imbalanced data set, as roughly 99.5% of all transactions are ok, and it's only 0.5% of transactions that are fraudulent. A `naive` model, which classifies everything as ok and not-fraud, would have an accuracy of 99.5%, but what about the sensitivity, specificity, the AUC, etc?

## `last_fit()`

```r
## `last_fit()` on test set

# - `last_fit()`  fits a model to the whole training data and evaluates it on the test set. 
# - provide the workflow object of the best model as well as the data split object (not the training data). 
 
last_fit_xgb <- last_fit(xgb_wflow, 
                        split = data_split,
                        metrics = metric_set(
                          accuracy, f_meas, kap, precision,
                          recall, roc_auc, sens, spec))

last_fit_xgb %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 4
##   .metric   .estimator .estimate .config             
##   <chr>     <chr>          <dbl> <chr>               
## 1 accuracy  binary         0.997 Preprocessor1_Model1
## 2 f_meas    binary         0.730 Preprocessor1_Model1
## 3 kap       binary         0.728 Preprocessor1_Model1
## 4 precision binary         0.967 Preprocessor1_Model1
## 5 recall    binary         0.586 Preprocessor1_Model1
## 6 sens      binary         0.586 Preprocessor1_Model1
## 7 spec      binary         1.00  Preprocessor1_Model1
## 8 roc_auc   binary         0.981 Preprocessor1_Model1
```

```r
#Compare to training
xgb_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n   std_err .config             
##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.998     3 0.000258  Preprocessor1_Model1
## 2 f_meas    binary     0.749     3 0.0265    Preprocessor1_Model1
## 3 kap       binary     0.748     3 0.0266    Preprocessor1_Model1
## 4 precision binary     0.889     3 0.0238    Preprocessor1_Model1
## 5 recall    binary     0.650     3 0.0413    Preprocessor1_Model1
## 6 roc_auc   binary     0.978     3 0.00995   Preprocessor1_Model1
## 7 sens      binary     0.650     3 0.0413    Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.0000990 Preprocessor1_Model1
```



## Get variable importance using `vip` package



```r
library(vip)

last_fit_xgb %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()
```

```
## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
## ℹ Please use `extract_fit_parsnip()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-13-1.png" width="672" />

## Plot Final Confusion matrix and ROC curve



```r
## Final Confusion Matrix

last_fit_xgb %>%
  collect_predictions() %>% 
  conf_mat(is_fraud, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-14-1.png" width="672" />

```r
## Final ROC curve
last_fit_xgb %>% 
  collect_predictions() %>% 
  roc_curve(is_fraud, .pred_1) %>% 
  autoplot()
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-14-2.png" width="672" />


##  Calculating the cost of fraud to the company


- How much money (in US\$ terms) are fraudulent transactions costing the company? Generate a table that summarizes the total amount of legitimate and fraudulent transactions per year and calculate the % of fraudulent transactions, in US\$ terms. Compare your model vs the naive classification that we do not have any fraudulent transactions. 


```r
best_model_wflow <- xgb_wflow

best_model_preds <- 
  best_model_wflow %>% 
  fit(data = card_fraud_train) %>%  
  
  ## Use `augment()` to get predictions for entire data set
  augment(new_data = card_fraud)

best_model_preds %>% 
  conf_mat(truth = is_fraud, estimate = .pred_class)
```

```
##           Truth
## Prediction      1      0
##          1   2459    220
##          0   1477 666872
```

```r
cost <- best_model_preds %>%
  select(is_fraud, amt, pred = .pred_class) 

cost <- cost %>%
  # naive false-- we think every single transaction is ok and not fraud
  mutate(false_naives = ifelse(is_fraud == 1, amt, 0)) %>% 
  # false negatives-- we thought they were not fraud, but they were
  mutate(false_negatives = ifelse((is_fraud == 1 & pred == 0), amt, 0)) %>% 
  # false positives-- we thought they were fraud, but they were not
  mutate(false_positives = ifelse((is_fraud == 0 & pred == 1), amt, 0)) %>% 
  # true positives-- we thought they were fraud, and they were 
  mutate(true_positives = ifelse((is_fraud == 1 & pred == 1), amt, 0)) %>% 
  # true negatives-- we thought they were ok, and they were
  mutate(true_negatives = ifelse((is_fraud == 0 & pred == 0), amt, 0))
  

# Summarising

cost_summary <- cost %>% 
  summarise(across(starts_with(c("false","true", "amt")), 
            ~ sum(.x, na.rm = TRUE)))

cost_summary
```

```
## # A tibble: 1 × 6
##   false_naives false_negatives false_positives true_positives true_negatives
##          <dbl>           <dbl>           <dbl>          <dbl>          <dbl>
## 1     2075089.         411859.         170236.       1663230.      44938579.
## # ℹ 1 more variable: amt <dbl>
```


- If we use a naive classifier thinking that all transactions are legitimate and not fraudulent, the cost to the company is $2,075,089.
- With our best model, the total cost of false negatives, namely transactions our classifier thinks are legitimate but which turned out to be fraud, is $411,859.

```r
scales::dollar(cost_summary$false_naives)
```

```
## [1] "$2,075,089"
```

```r
scales::dollar(cost_summary$false_negatives)
```

```
## [1] "$411,859"
```

```r
scales::dollar(cost_summary$false_positives)
```

```
## [1] "$170,236"
```

```r
scales::dollar(cost_summary$true_negatives)
```

```
## [1] "$44,938,579"
```

```r
scales::dollar(cost_summary$true_positives)
```

```
## [1] "$1,663,230"
```

- Our classifier also has some false positives, $170,236, namely flagging transactions as fraudulent, but which were legitimate. Assuming the card company makes around 2% for each transaction (source: https://startups.co.uk/payment-processing/credit-card-processing-fees/), the amount of money lost due to these false positives is $3,404.72

```r
scales::dollar(cost_summary$false_positives * 0.02)
```

```
## [1] "$3,404.72"
```

- The \$ improvement over the naive policy is $1,659,826.

```r
scales::dollar(cost_summary$false_naives - cost_summary$false_negatives - cost_summary$false_positives * 0.02)
```

```
## [1] "$1,659,826"
```

