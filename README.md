# Project1
---
title: "Project 1"
author: "Brennan Clinch"
date: "9/26/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

```


## Introduction

For this vignette, we want to give a tutorial on summarizing financial data from an API. For starters, an API is a connection between computers or between computer programs, which can also be called a software interface. ("https://en.wikipedia.org/wiki/API"). API's are an important source for retrieving data and working with it, especially in data science. We are interested in knowing how to first setup our API for financial data and then how to interact with it and do data analysis, etc. To do this, we need to know what is required in able to be able to connect with our API (what packages are needed)

## Requirements

To be able to work with our API, we must first note that these following packages were required.

-`tidyverse: an R package for data science, with tons of tools for data visualization, manipulation, and analysis`

-`jsonlite: this is one package used to read API files, which is basically a json parsor and generator`

`httr: another API package used which gives tools for working with urls and http`

## Functions used for contacting our API

To begin contacting our requested API, we need to first connect with it. This can be done by going to the API and requesting a key and then getting it to then connect with what section of our API we are interested in.

Let's start with my functions I used for connecting with my API.


`COVIDarea` 

I wrote this simple function takes the `summary` endpoint and subset the columns that we want returned for COVID data such as `Country`, `NewRecovered`, `TotalRecovered`, `NewConfirmed`, `TotalConfirmed`, etc in order to return a data frame that we are interested in for the overall summary statistics for COVID-19 data for each country in the world.
```{r}
COVIDarea <- function(type = "all"){
  outputAPI <- GET("https://api.covid19api.com/summary")
  data <- fromJSON(rawToChar(outputAPI$content))
  output <- data$Countries
  if (type != "all"){
    output <- output %>% select(type) 
  }
  return(output)
}
```

`dayonestatus`

This is another simple function I made that takes in the `dayone` endpoint and let's me customize my endpoint I want to get a data frame for data since the beginning of the COVID-19 pandemic for either recovered or confirmed cases by typing in what I want in parentheses(`confirmed` or `recovered`) after calling the function

```{r}
dayonestatus <- function(type = "all"){
  baseurl <- "https://api.covid19api.com/dayone/country/canada"
  fullURL <- paste0(baseurl, "/status/", type)
  output <- fromJSON(fullURL)
  return(output)
}
```


`timeframe`

```{r}
timeframe <- function(country = "all", year = "all", date1 = "all", date2 = "all"){
  baseurl <- "https://api.covid19api.com/country/"
  countryurl <- country
  yearurl <- year
  date1url <- date1
  date2url <- date2
  fullurl <- paste0(baseurl,country,"/status/confirmed?from=",yearurl,"-",date1url,"T00:00:00Z&to=",yearurl,"-",date2url,"T00:00:00Z")
  output <- fromJSON(fullurl)
  if (year != 2020:2021 ){
    stop("Error with year")
  }
  return(output)
}

```


`periodCA`

This function connects with `Day one Total` endpoint which takes in all data for the confirmed number of cases since the pandemic began. In this case we are looking at data for Canada. This function creates an updated data frame that can give us the data for each season of the year since the pandemic began.

```{r}
periodCA <- function(x){
 COVID4 <- fromJSON("https://api.covid19api.com/dayone/country/canada/status/recovered")
 Period <- COVID4$Date
 Cases <- COVID4$Cases

 output <- COVID4 %>% mutate(Period = case_when(Cases < 1449 ~  "Winter 2020",
  Cases %in% 1773:95240 ~  "Spring 2020",
  Cases %in% 95564:131294 ~ "Summer 2020",
  Cases %in% 132449:430591 ~"Fall 2020",
  Cases %in% 435801:796896 ~  "Winter 2020-2021",
  Cases %in% 801299:1186180~ "Spring 2021",
  Cases >1186180~ "Summer 2021"
))
 return(output)
}

```

`Illinoistype`

This function interacts with the `USA`  for the `live data`endpoint and returns a data frame for COVID related data over the past several months for the United States in Illinois after selecting what data you want whether it be active cases, recovered, confirmed cases, or deaths.

```{r}
Illinoistype <- function(type= "all", Province = "Illinois"){
  outputAPI <- GET("https://api.covid19api.com/live/country/USA")
  data <- fromJSON(rawToChar(outputAPI$content))
  if (Province != "all" & type != "all"){
    data <- data %>% select(Province,type) %>% filter(Province == "Illinois")
  }
  return(data)
}

```

`NorthAmerica`


```{r, warning = 'FALSE'}
NorthAmerica <- function(type = "all", Country = "all"){
  outputAPI <- GET("https://api.covid19api.com/total/country/united-states")
  data <- fromJSON(rawToChar(outputAPI$content))
  if (type == c("Deaths", "Recovered", "Active", "Confirmed", "Date", "Country")){
     if (Country == "canada"){
       baseurl <- "https://api.covid19api.com/total/country/"
       fullURL <- paste0(baseurl, Country)
       data <- fromJSON(fullURL) 
       data <- data %>% select(type)
     }
     else if (Country == "united-states"){
      baseurl <- "https://api.covid19api.com/total/country/"
       fullURL <- paste0(baseurl, Country)
       data <- fromJSON(fullURL) 
       data <- data %>% select(type)
    }
  }
  else {
    stop("Error: please specify either united-states or canada or a valid type")
  }
  return(data)
}

```


`countrydic`

This function I created is a helper function that takes the `countries` endpoint for COVID data and let's you look up the `slug` of the Country name you picked in capital letters from the `Country` column which gives you reference for the `slug` and `ISO2` for some of the later functions I created that let you customize the enpoint you want returned from the  COVID API. Also note I created a not in operator to be able to pass it on in the else if statement without causing an error when parsing the function with the purr package.

```{r, warning = F, message = T}
library(purrr)
`%not_in%` <- purrr::negate(`%in%`)

countrydic <- function(type = "all"){
 outputAPI <- fromJSON("https://api.covid19api.com/countries")
 if (type != "all"){
   if (type %in% outputAPI$Country){
      outputAPI <- outputAPI %>%
        filter(Country == type)
   }
   else if (type %not_in% outputAPI$Country){
     stop("Invalid entry")
   }
 }
 else {
   stop("Invalid entry")
 }
 return(outputAPI)
}
```

https://api.covid19api.com/dayone/country/south-africa/status/confirmed/live

`COVIDapi`
```{r}
COVIDapi <- function(func, ...){
  if (func == "NorthAmerica"){
    output <- NorthAmerica(type = "Confirmed", Country = "canada")
  }
  else if (func == "COVIDarea"){
    output <- COVIDarea(...)
  }
  else if (func == "Illinoistype"){
    output <- Illinoistype(type = c("Confirmed", "Deaths"))
  }
  else if (func == "dayonestatus"){
    output <- dayonestatus(type = "confirmed")
  }
  else if (func == "countrydic"){
    output <- countrydic(...)
  }
  else if (func == "timeframe"){
    output <- timeframe(...)
  }
  
  else {
    stop("ERROR: Argument for func is not valid!")
  }
  return(output)
}
```


##Exploratory data analysis

Let's first look at a numeric summary of our periodical data for each season for Canada. Note that we must first convert the Total number of cases to be individual cases each day. To do this, we need to use a for loop to subtract the current day total case count from the one from the previous day.

Let's first create an object called `CA` which queries our  function from the `Day One Total` endpoint for Canada. To be able to do data analysis on this variable, I first did some data manipulation on this data frame first. I created a new column called period which gives us the season of when the confirmed cases of COVID-19 occurred. I also wanted to be able to look at the statistics for the number of cases, so I had to convert the sum of the number of cases as shown in the original `CA` data frame to be the daily number of cases for each specified date. This does however create one `NA` value since it doesn't count the latest total data, so this updated data frame contains values up till the date prior to the current date.
```{r}
CA <- COVIDapi("dayonestatus")
CA <- CA %>% mutate(Period = case_when(Cases < 1449 ~  "Winter 2020",
  Cases %in% 1773:95240~  "Spring 2020",
  Cases %in% 95564:131294~ "Summer 2020",
  Cases %in% 132449:430591~"Fall 2020",
  Cases %in% 435801:796896~  "Winter 2020-2021",
  Cases %in% 801299:1186180~ "Spring 2021",
  Cases %in% 1186180:1309938~ "Summer 2021",
  Cases > 1309938~ "Fall 2021"
))
for (i in 1:max(row_number(CA$Cases))){
  CA$Cases[i] <- CA$Cases[i+1]-CA$Cases[i]
}
CA <- na.omit(CA)
```

Since I just got my data frame `CA` set up, I will now proceed with my data exploration on my `CA` data for Canada. The first thing I looked at was the overall distribution of the cases since the pandemic began by creating a boxplot for the number of cases. I used the `ggplot2` package for creating the boxplot.
```{r}
library(ggplot2)
g <- ggplot(data = CA, aes(x = Cases))
g+geom_boxplot()

g <- ggplot(data = CA, aes(x = Period, y = Cases))
g+geom_point(aes(col = Cases), position = "jitter", size = 1)+
  geom_boxplot(aes(col = Cases, alpha = 0.5))+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Boxplots for Covid cases in Canada for each season")
```
Based on the 2 boxplots, it is clearly shown that for the total distribution of COVID-19 cases in Canada, it is noted that the distribution of cases over the whole pandemic is skewed a little to the right with some outlying daily cases over 7500 and close to 10000. For the 2nd plot with the side-by-side boxplots for each season, we can see that Canada got the most cases of COVID-19 during the spring of 2021 and the distribution was roughly symmetric. It also shows that winter of 2020 had the lowest number of cases since it was the beginning of the pandemic along with summer of 2020. 

The next thing I did for the `CA` dataset was look at the general summary statistics for each period throughout the pandemic. From the summary statistics, it is correct that Spring 2021 had the highest number of cases on average with spring of 2021 having the highest spread of cases since it's standard deviation was so high.


```{r}
CA  %>% group_by(Period) %>% summarise("Min." = min(Cases), "1st Quartile" = quantile(Cases,0.25,na.rm = TRUE), "Mean" =mean(Cases), "Median" = median(Cases), "3rd Quartile"= quantile(Cases,0.75,na.rm=TRUE), "Max." = max(Cases), "standard deviation." = sd(Cases))
```
Another thing I did with the COVID data for the `CA` dataset was to look at a contigency table for COVID cases by `less than 500`, `500-1000`, `1000-2000`, `2000-3000`, `3000-4000`, `4000-6000`, and `6000+` to more closely examine how many cases were of each of these range. So first I added a new variable called `cases_range` that categorizes every COVID case in each of these ranges.

```{r}
CA <- CA %>%mutate(cases_range = case_when(Cases < 500~"less than 500",
      Cases %in% 500:1000 ~ "500-1000",
      Cases %in% 1001:2000~ "1000-2000",
      Cases %in% 2001:3000~ "2000-3000",
      Cases %in% 3001:4000~ "3000-4000",
      Cases %in% 4001:6000~ "4000-6000",
      Cases > 6000~ "6000+"))
knitr::kable(table(CA$cases_range, CA$Period))
```
After creating the contigency table, I made a bar plot to summarize the contigency table graphically.
```{r}
bar <- ggplot(data = CA, aes(x = cases_range))
bar+geom_bar(aes(fill = as.factor(Period)), position = "dodge")+
  scale_x_discrete(labels = c("1000-2000", "2000-3000", "3000-4000", "4000-6000", "500-1000", "6000+", "less than 500"))+
  scale_fill_discrete(name = "Season/Period of pandemic", labels = c("Fall 2020", "Fall 2021", "Spring 2020", "Spring 2021", "Summer 2020", "Summer 2021", "Winter 2020", "Winter 2020-2021"))+
  theme(axis.text.x = element_text(angle = 30))
```


Let's now look at the relationship between cases and deaths for the state of Illinois in the United States. Let's convert the total cases and deaths to cases for each row (by date). We also deleted the last row of data since we didn't know the number of cases for the first record since it only gave us the total number of cases overall. I also made a loop converting the overall total number of deaths to daily number of deaths using the same idea as before with the confirmed cases.

```{r, warning = FALSE}
Illinoisdata <- COVIDapi("Illinoistype")
Illinoisdata <- Illinoisdata %>% mutate(Totalcases = Confirmed)

 for (i in min(row_number(Illinoisdata$Confirmed)):max(row_number(Illinoisdata$Confirmed))){
  Illinoisdata$Confirmed[i] <- Illinoisdata$Confirmed[i+1]-Illinoisdata$Confirmed[i]
 }

#Create loop that makes recreates the total number of cases to be able to create the new variable caseratio

for (i in min(row_number(Illinoisdata$Totalcases)):max(row_number(Illinoisdata$Confirmed))){
   Illinoisdata$Totalcases[i] <- Illinoisdata$Totalcases[i+1]
}

  for (i in min(row_number(Illinoisdata$Deaths)):max(row_number(Illinoisdata$Deaths))){
    Illinoisdata$Deaths[i] <- Illinoisdata$Deaths[i+1]-Illinoisdata$Deaths[i]
  }
Illinoisdata <- na.omit(Illinoisdata)
```

I now will create a scatter plot for `Illinoisdata` to show what the relationship is between the number of cases and the number of deaths is for COVID-19 in Illinois over the past few months. We will need the `ggplot2` package from `tidyverse` here.
```{r}
library(ggplot2)

g <- ggplot(data = Illinoisdata, aes(x = Deaths, y = Confirmed))
g+geom_point()+
  geom_smooth(method = "lm", color = "blue")+
  labs(title = "Daily Confirmed COVID Cases vs COVID Deaths in Illinois")
```
From the plot, it shows that there is a strong positive linear relationship between COVID-19 cases and COVID-19 deaths in Illinois.



