Euromonitor International Coding Task
================
Said Maanan
2022-08-14

## Loading Data

Since the data in the **wimax raw data.csv** file contains too few
observations to allow us to confidently perform the analysis, we decided
to obtain new data from the ITU website.

``` r
library(tidyverse)
library(lubridate)
library(ggplot2)
library(prophet)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
```

``` r
ddd_dataset <- read_excel("data/ddd_dataset.xlsx")
wimax <- ddd_dataset[ddd_dataset$`Indicator name`=="Population covered by at least a 4G mobile network (%)",]
```

## Data Cleaning

We will remove excess columns, turn the years into columns, and sort the
data by country (alphabetically).

``` r
wimax <- wimax[order(wimax$Country),]
wimax <- spread(wimax[-c(1553, 1554), c("Country","Year","Value")], Year, value = Value)
```

Many countries have `NA`/`NULL`, therefore I will impute the missing
data.

## Data Imputation

``` r
wimax <- wimax %>% 
  pivot_longer(-Country, names_to = "year") %>% 
  mutate(value = value %>% as.numeric()) %>% 
  group_by(Country) %>% 
  fill(value, .direction = "updown") %>% 
  pivot_wider(names_from = year, values_from = value)
```

## Create Datasets

I create the data sets I will use in modelling in the format I need them
to be.

``` r
itu_names <- read_csv("data/itu-emi-countries.csv") %>% 
  rename(Country = `ITU Name`, Cemi = `EMI Name`)

coverage <- read_csv("data/3g data.csv") %>% select(c(1, 42:50)) %>% 
  select(CountryName, starts_with("Y")) %>% rename(Country = CountryName) %>% 
  pivot_longer(-Country, names_to = "ds", values_to = "Expln") %>% 
  mutate(ds = as.Date(gsub("^Y", "", ds), format = "%Y"))

n_data <- wimax %>% 
  left_join(itu_names, by = "Country") %>% 
  mutate(Country = Cemi) %>% select(-Cemi)

wimax <- wimax %>% 
  left_join(itu_names, by = "Country") %>% 
  mutate(Country = Cemi) %>% select(-Cemi) %>% 
  pivot_longer(-Country, names_to = "ds", values_to = "y") %>% 
  mutate(ds = as.Date(ds, format = "%Y")) %>% 
  inner_join(coverage, by = c("Country","ds"))
```

## Modelling

I run the prophet model.

``` r
make_model = function(df) {
  df$cap <- 100
  m <- prophet(growth = 'logistic')
  m <- add_regressor(m, name = 'Expln')
  m <- fit.prophet(m, df)
  return(m)
}

mdl <- wimax %>% 
  nest(-Country) %>% 
  mutate(model = map(data, make_model)) %>% 
  ungroup()
```

## Forecasting

I make forecasts, plots and save them.

``` r
expln <- read_csv("data/3g data.csv") %>% select(c(1, 42:60)) %>% 
  select(CountryName, starts_with("Y")) %>% 
  pivot_longer(-CountryName, names_to = "ds", values_to = "Expln") %>%
  mutate(ds = as.Date(gsub("^Y", "", ds), format = "%Y")) %>%
  rename(Country = CountryName)

make_frcst = function(m, df) {
  future <- make_future_dataframe(m, 10, freq = 'year', include_history = TRUE)
  future$Expln <- df$Expln
  future$cap <- 100
  fcst <- predict(m, future)
  return(fcst)
}

frs <- expln %>% 
  nest(-Country) %>% 
  rename(pred_data = data) %>% 
  inner_join(mdl, by = "Country") %>% 
  mutate(prediction = map2(model, pred_data, make_frcst)) %>% 
  mutate(plot = map2(model, prediction, plot)) %>% 
  ungroup()
```

## Display Forecasts

I iteratively plot the figures.

``` r
plots <- map2(frs$plot, frs$Country, ~(.x + labs(title = .y)))

for (plot in plots) {
  print(plot)
  cat('\n\n')
}
```

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-12.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-13.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-14.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-15.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-16.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-17.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-18.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-19.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-20.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-21.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-22.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-23.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-24.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-25.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-26.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-27.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-28.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-29.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-30.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-31.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-32.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-33.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-34.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-35.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-36.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-37.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-38.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-39.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-40.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-41.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-42.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-43.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-44.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-45.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-46.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-47.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-48.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-49.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-50.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-51.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-52.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-53.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-54.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-55.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-56.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-57.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-58.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-59.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-60.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-61.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-62.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-63.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-64.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-65.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-66.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-67.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-68.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-69.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-70.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-71.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-72.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-73.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-74.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-75.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-76.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-77.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-78.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-79.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-80.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-81.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-82.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-83.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-84.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-85.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-86.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-87.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-88.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-89.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-90.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-91.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-92.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-93.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-94.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-95.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-96.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-97.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-98.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-99.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-100.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-101.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-102.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-103.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-104.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-105.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-106.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-107.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-108.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-109.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-110.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-111.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-112.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-113.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-114.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-115.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-116.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-117.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-118.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-119.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-120.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-121.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-122.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-123.png)<!-- -->

![](Euromonitor_files/figure-gfm/unnamed-chunk-8-124.png)<!-- -->

## Put Results in CSV File

First we gather the data we want,

``` r
unnested <- unnest(frs, prediction)

unnested <- data.frame(Country = unnested$Country, Year = year(unnested$ds), Pred = unnested$yhat)

unnested <- spread(unnested, Year, value = Pred)[,c(1, 11:20)]

extradat <- read_csv("data/3g data.csv") %>% select(c(1:6))

unnested <- left_join(unnested, extradat, by = c("Country" = "CountryName"), all.x = TRUE)

unnested <- left_join(unnested, n_data, by = "Country", all.x = TRUE)

unnested <- unnested[, c(1, 12:25, 2:11)]
```

We need to remove the data that is unwanted.

``` r
exceptions <- read_csv("data/WiMAX exceptions.csv")

exceptions = exceptions[-c(10, 14), c(1, 3)]

exceptions$Years = gsub('Y','', exceptions$Years)

Count = exceptions$CountryName

Year  = exceptions$Years

for (i in seq_along(Count)) {
  unnested[unnested$Country == Count[i], ][[Year[i]]] <- NA
}
```

We can now save the data.

``` r
write.csv(unnested, "result.csv", row.names = FALSE)
```
