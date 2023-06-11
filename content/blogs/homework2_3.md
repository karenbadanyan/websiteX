---
categories: 
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2023-06-10"
description: CO2 Emissions # the title that will show up once someone gets to this page
draft: false
image: co2.jpeg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: co2_emissions # slug is the shorthand URL address... no spaces plz
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



```r
library(scales)
```

```r
library(lubridate)
```


# Exploring sources of electricity production, CO2 emissions, and GDP per capita.

There are many sources of data on how countries generate their electricity and their CO2 emissions. I would like you to create three graphs:

## 1. A stacked area chart that shows how your own country generated its electricity since 2000.

You will use

`geom_area(colour="grey90", alpha = 0.5, position = "fill")`

## 2. A scatter plot that looks at how CO2 per capita and GDP per capita are related

## 3. A scatter plot that looks at how electricity usage (kWh) per capita/day GDP per capita are related

We will get energy data from the Our World in Data website, and CO2 and GDP per capita emissions from the World Bank, using the `wbstats`package.



```r
# Download electricity data
url <- "https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv"

energy <- read_csv(url) %>% 
  filter(year >= 1990) %>% 
  drop_na(iso_code) %>% 
  select(1:3,
         biofuel = biofuel_electricity,
         coal = coal_electricity,
         gas = gas_electricity,
         hydro = hydro_electricity,
         nuclear = nuclear_electricity,
         oil = oil_electricity,
         other_renewable = other_renewable_exc_biofuel_electricity,
         solar = solar_electricity,
         wind = wind_electricity, 
         electricity_demand,
         electricity_generation,
         net_elec_imports,	# Net electricity imports, measured in terawatt-hours
         energy_per_capita,	# Primary energy consumption per capita, measured in kilowatt-hours	Calculated by Our World in Data based on BP Statistical Review of World Energy and EIA International Energy Data
         energy_per_gdp,	# Energy consumption per unit of GDP. This is measured in kilowatt-hours per 2011 international-$.
         per_capita_electricity, #	Electricity generation per capita, measured in kilowatt-hours
  ) 

# Download data for C02 emissions per capita https://data.worldbank.org/indicator/EN.ATM.CO2E.PC
co2_percap <- wb_data(country = "countries_only", 
                      indicator = "EN.ATM.CO2E.PC", 
                      start_date = 1990, 
                      end_date = 2022,
                      return_wide=FALSE) %>% 
  filter(!is.na(value)) %>% 
  #drop unwanted variables
  select(-c(unit, obs_status, footnote, last_updated)) %>% 
  rename(year = date,
         co2percap = value)


# Download data for GDP per capita  https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
gdp_percap <- wb_data(country = "countries_only", 
                      indicator = "NY.GDP.PCAP.PP.KD", 
                      start_date = 1990, 
                      end_date = 2022,
                      return_wide=FALSE) %>% 
  filter(!is.na(value)) %>% 
  #drop unwanted variables
  select(-c(unit, obs_status, footnote, last_updated)) %>% 
  rename(year = date,
         GDPpercap = value)

energy_new <- energy %>% 
  pivot_longer(
    cols = 4:12,
    names_to = "source",
    values_to = "value") %>%
  group_by(iso_code,year) %>%
  mutate(percent = round(100*value/sum(value),2))
  

library(dplyr)

co2_percap_new <- co2_percap %>%
  select(iso3c,year,co2percap)

gdp_percap_new <- gdp_percap %>%
  select(iso3c,year,GDPpercap)

new_dataset1 <- left_join (co2_percap_new,gdp_percap_new,by = "iso3c") %>%
  mutate(logic = year.x == year.y) %>%
  filter(logic == "TRUE")

energy_new_reformed <- energy_new %>%
  mutate(iso3c = iso_code,per_capita_electricity_per_day = per_capita_electricity/365)
  
new_dataset2 <- left_join(energy_new_reformed,co2_percap_new,by = "iso3c") %>%
  mutate(logic = year.x == year.y) %>%
  filter(logic == "TRUE")

library(patchwork)
library(countrycode)

# Function to create the first diagram
create_diagram1 <- function(country_var) {
  energy_new %>%
    filter(year >= 2000, iso_code == country_var) %>%
    ggplot(aes(x = year, y = percent, fill = source)) +
    geom_area(colour = "grey90", alpha = 0.5, position = "fill") +
    xlab("Year") +
    ylab("Fraction of Energy Type") +
    ggtitle("Electricity Production Mix")
}

# Function to create the second diagram
create_diagram2 <- function(country_var) {
  new_dataset1 %>%
    filter(iso3c == country_var) %>%
    ggplot(aes(x = GDPpercap, y = co2percap)) +
    geom_point() +
    geom_text(aes(label = year.x), vjust = -1) +
    xlab("GDP per capita") +
    ylab("CO2 per capita") +
    ggtitle("CO2 vs GDP per capita")
}

# Function to create the third diagram
create_diagram3 <- function(country_var) {
  new_dataset2 %>%
    filter(iso3c == country_var) %>%
    group_by(year.x) %>%
    ggplot(aes(x = per_capita_electricity_per_day, y = co2percap)) +
    geom_point() +
    geom_text(aes(label = year.x), vjust = -1) +
    xlab("Electricity used (kWh) per capita/day") +
    ylab("CO2 per capita") +
    ggtitle("CO2 vs electricity consumption per capita/day")
}

# User input for country code
country_var <- countrycode(readline("Enter the country name: "), origin='country.name', destination = 'iso3c')
```

```
## Enter the country name:
```

```r
# Create the diagrams
plot1 <- create_diagram1(country_var)
plot2 <- create_diagram2(country_var)
plot3 <- create_diagram3(country_var)

# Arrange and display the plots using patchwork
plots <- (plot1) / (plot2 | plot3)
plots
```

<img src="/blogs/homework2_3_files/figure-html/unnamed-chunk-7-1.png" width="672" />

