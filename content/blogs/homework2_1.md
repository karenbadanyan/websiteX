---
categories: 
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2023-06-10"
description: Mass shootings in the US # the title that will show up once someone gets to this page
draft: false
image: shootings.jpeg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: mass_shootings # slug is the shorthand URL address... no spaces plz
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



# Data Visualisation - Exploration

Now that you've demonstrated your software is setup, and you have the basics of data manipulation, the goal of this assignment is to practice transforming, visualising, and exploring data.

# Mass shootings in the US

In July 2012, in the aftermath of a mass shooting in a movie theater in Aurora, Colorado, [Mother Jones](https://www.motherjones.com/politics/2012/07/mass-shootings-map/) published a report on mass shootings in the United States since 1982. Importantly, they provided the underlying data set as [an open-source database](https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/) for anyone interested in studying and understanding this criminal behavior.

## Obtain the data


```
## Rows: 125
## Columns: 14
## $ case                 <chr> "Oxford High School shooting", "San Jose VTA shoo…
## $ year                 <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2020, 2020, 2…
## $ month                <chr> "Nov", "May", "Apr", "Mar", "Mar", "Mar", "Mar", …
## $ day                  <dbl> 30, 26, 15, 31, 22, 16, 16, 26, 10, 6, 31, 4, 3, …
## $ location             <chr> "Oxford, Michigan", "San Jose, California", "Indi…
## $ summary              <chr> "Ethan Crumbley, a 15-year-old student at Oxford …
## $ fatalities           <dbl> 4, 9, 8, 4, 10, 8, 4, 5, 4, 3, 7, 9, 22, 3, 12, 5…
## $ injured              <dbl> 7, 0, 7, 1, 0, 1, 0, 0, 3, 8, 25, 27, 26, 12, 4, …
## $ total_victims        <dbl> 11, 9, 15, 5, 10, 9, 4, 5, 7, 11, 32, 36, 48, 15,…
## $ location_type        <chr> "School", "Workplace", "Workplace", "Workplace", …
## $ male                 <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
## $ age_of_shooter       <dbl> 15, 57, 19, NA, 21, 21, 31, 51, NA, NA, 36, 24, 2…
## $ race                 <chr> NA, NA, "White", NA, NA, "White", NA, "Black", "B…
## $ prior_mental_illness <chr> NA, "Yes", "Yes", NA, "Yes", NA, NA, NA, NA, NA, …
```

| column(variable)     | description                                                                 |
|--------------------------|----------------------------------------------|
| case                 | short name of incident                                                      |
| year, month, day     | year, month, day in which the shooting occurred                             |
| location             | city and state where the shooting occcurred                                 |
| summary              | brief description of the incident                                           |
| fatalities           | Number of fatalities in the incident, excluding the shooter                 |
| injured              | Number of injured, non-fatal victims in the incident, excluding the shooter |
| total_victims        | number of total victims in the incident, excluding the shooter              |
| location_type        | generic location in which the shooting took place                           |
| male                 | logical value, indicating whether the shooter was male                      |
| age_of_shooter       | age of the shooter when the incident occured                                |
| race                 | race of the shooter                                                         |
| prior_mental_illness | did the shooter show evidence of mental illness prior to the incident?      |

## Explore the data

### Specific questions

-   Generate a data frame that summarizes the number of mass shootings per year.


```r
# number of mass shootings per year
mass_shootings %>%
  group_by(year) %>%
  summarise(count = n())
```

```
## # A tibble: 37 × 2
##     year count
##    <dbl> <int>
##  1  1982     1
##  2  1984     2
##  3  1986     1
##  4  1987     1
##  5  1988     1
##  6  1989     2
##  7  1990     1
##  8  1991     3
##  9  1992     2
## 10  1993     4
## # ℹ 27 more rows
```

-   Generate a bar chart that identifies the number of mass shooters associated with each race category. The bars should be sorted from highest to lowest and each bar should show its number.


```r
# number of mass shooters associated with each race category
mass_shootings %>%
  group_by(race) %>%
  summarise(count = n()) %>%
  na.omit(count) %>%
  mutate(race = fct_reorder(race,count)) %>%
  ggplot(aes(x=count,y=race))+geom_col()+geom_text(aes(label = count), vjust = -0.5, color = "red", size = 4) + 
  xlab("Number of Mass Shooters") +
  ylab("Race") +
  ggtitle("Dominance of White Race in the Number of Mass Shooters")
```

<img src="/blogs/homework2_1_files/figure-html/unnamed-chunk-4-1.png" width="672" />

-   Generate a boxplot visualizing the number of total victims, by type of location.


```r
# all categories
mass_shootings %>%
  mutate(location_type = fct_reorder(location_type,total_victims)) %>%
  ggplot(aes(x=location_type,y=total_victims))+geom_boxplot() +
  xlab("Location") +
  ylab("Number of Victims") +
  ggtitle("Number of Victims per Location Type")
```

<img src="/blogs/homework2_1_files/figure-html/unnamed-chunk-5-1.png" width="672" />

```r
# without "other" location_type
mass_shootings %>%
  filter(location_type != "Other") %>%
  mutate(location_type = fct_reorder(location_type, total_victims)) %>%
  ggplot(aes(x = location_type, y = total_victims)) +
  geom_boxplot() +
  xlab("Location") +
  ylab("Number of Victims") +
  ggtitle("Number of Victims per Location Type")
```

<img src="/blogs/homework2_1_files/figure-html/unnamed-chunk-5-2.png" width="672" />


-   Redraw the same plot, but remove the Las Vegas Strip massacre from the dataset.


```r
# without the Las Vegas Strip massacre
mass_shootings %>%
  filter(case != "Las Vegas Strip massacre") %>%
  mutate(location_type = fct_reorder(location_type, total_victims)) %>%
  ggplot(aes(x = location_type, y = total_victims)) +
  geom_boxplot() +
  xlab("Location") +
  ylab("Number of Victims") +
  ggtitle("Number of Victims per Location Type")
```

<img src="/blogs/homework2_1_files/figure-html/unnamed-chunk-6-1.png" width="672" />

### More open-ended questions

Address the following questions. Generate appropriate figures/tables to support your conclusions.

-   How many white males with prior signs of mental illness initiated a mass shooting after 2000?


```r
# filter white males with prior signs of mental illness after Year 2000
mass_shootings %>%
  filter(male & race == "White" & prior_mental_illness == "Yes" & year >  2000) %>%
  summarise(count = n())
```

```
## # A tibble: 1 × 1
##   count
##   <int>
## 1    22
```

-   Which month of the year has the most mass shootings? Generate a bar chart sorted in chronological (natural) order (Jan-Feb-Mar- etc) to provide evidence of your answer.


```r
# assign month order
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                 "Aug", "Sep", "Oct", "Nov", "Dec")

# group by month for count and order chronologically
mass_shootings %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  mutate(month = factor(month, levels = month_order)) %>%
  ggplot(aes(x = month, y = count)) +
  geom_col() +
  xlab("Month") +
  ylab("Number of Victims") +
  ggtitle("Number of Victims per Month")
```

<img src="/blogs/homework2_1_files/figure-html/unnamed-chunk-8-1.png" width="672" />

-   How does the distribution of mass shooting fatalities differ between White and Black shooters? What about White and Latino shooters?


```r
# distribution of mass shooting fatalities for White, Black and Latino
mass_shootings %>%
  filter(race == "White" | race == "Black" | race == "Latino") %>%
  group_by(race, year) %>%
  summarise (fatalities = sum(fatalities)) %>%
  ggplot(aes(x=year,y=fatalities))+geom_line(aes(colour = race)) +
  xlab("Year") +
  ylab("Fatalities") +
  ggtitle("White Fatalities above the others throughout the Years")
```

```
## `summarise()` has grouped output by 'race'. You can override using the
## `.groups` argument.
```

<img src="/blogs/homework2_1_files/figure-html/unnamed-chunk-9-1.png" width="672" />

### Very open-ended

-   Are mass shootings with shooters suffering from mental illness different from mass shootings with no signs of mental illness in the shooter?


```r
mass_shootings %>%
  filter(prior_mental_illness == "Yes" | prior_mental_illness == "No") %>%
  group_by(prior_mental_illness, year) %>%
  summarise (count = n()) %>%
  ggplot(aes(x=year,y=count))+geom_col(aes(colour = prior_mental_illness)) +
  xlab("Year") +
  ylab("Number of Shooters") +
  ggtitle("People with Mental Ilness have more tendency to Mass Shootings")
```

```
## `summarise()` has grouped output by 'prior_mental_illness'. You can override
## using the `.groups` argument.
```

<img src="/blogs/homework2_1_files/figure-html/unnamed-chunk-10-1.png" width="672" />

-   Assess the relationship between mental illness and total victims, mental illness and location type, and the intersection of all three variables.


```r
library (GGally)
```

```
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
```

```r
mass_shootings %>%
 select(prior_mental_illness, location_type, total_victims) %>%
 drop_na(prior_mental_illness) %>%
 ggpairs()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="/blogs/homework2_1_files/figure-html/unnamed-chunk-11-1.png" width="672" />
