Euromonitor International Coding Task
================
Said Maanan
2022-08-15

## Loading Data

Since the data in the **wimax raw data.csv** file contains too few
observations to allow us to confidently perform the analysis, we decided
to obtain new data from the ITU website.

``` r
library(tidyverse)
library(lubridate)
library(ggplot2)
library(prophet)
library(betareg)
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

I fill the `NA` cells with non-`NA` cells after them.

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

coverage <- read_csv("data/3g data.csv") %>% select(c("CountryName", "Y2012":"Y2020")) %>% 
  rename(Country = CountryName) %>% pivot_longer(-Country, names_to = "ds", values_to = "Expln") %>% 
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
expln <- read_csv("data/3g data.csv") %>% select(c("CountryName", "Y2012":"Y2030")) %>% 
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
plots <- map2(frs$plot, frs$Country, ~(.x + labs(title = .y, x = "Years", y = "Coverage")))

for (plot in plots) {
  print(plot)
  cat('\n\n')
}
```

![](updated_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-12.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-13.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-14.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-15.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-16.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-17.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-18.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-19.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-20.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-21.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-22.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-23.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-24.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-25.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-26.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-27.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-28.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-29.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-30.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-31.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-32.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-33.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-34.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-35.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-36.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-37.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-38.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-39.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-40.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-41.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-42.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-43.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-44.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-45.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-46.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-47.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-48.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-49.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-50.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-51.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-52.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-53.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-54.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-55.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-56.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-57.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-58.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-59.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-60.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-61.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-62.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-63.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-64.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-65.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-66.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-67.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-68.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-69.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-70.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-71.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-72.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-73.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-74.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-75.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-76.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-77.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-78.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-79.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-80.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-81.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-82.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-83.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-84.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-85.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-86.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-87.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-88.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-89.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-90.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-91.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-92.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-93.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-94.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-95.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-96.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-97.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-98.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-99.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-100.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-101.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-102.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-103.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-104.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-105.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-106.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-107.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-108.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-109.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-110.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-111.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-112.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-113.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-114.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-115.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-116.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-117.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-118.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-119.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-120.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-121.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-122.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-123.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-8-124.png)<!-- -->

## Put Results in CSV File

First we gather the data we want.

``` r
extradat <- read_csv("data/3g data.csv") %>% select(c("CountryName":"Unit"))

unnested <- unnest(frs, prediction) %>% 
  mutate(Year = year(ds), Pred = yhat) %>% select(Country, Year, Pred) %>% 
  spread(Year, value = Pred) %>% select("Country", "2021":"2030") %>% 
  left_join(extradat, by = c("Country" = "CountryName"), all.x = TRUE) %>% 
  left_join(n_data, by = "Country", all.x = TRUE) %>% 
  select("Country","CountryCode":"2020","2021":"2030")
```

We need to remove the data that is unwanted.

``` r
exceptions <- read_csv("data/WiMAX exceptions.csv") %>% 
  filter(!row_number() %in% c(10, 14)) %>% 
  select(c(1,3)) %>% mutate(Years = gsub('Y','', Years))

Count <- exceptions$CountryName
Year  <- exceptions$Years

for (i in seq_along(Count)) {
  unnested[unnested$Country == Count[i], ][[Year[i]]] <- NA
}
```

We can now save the data.

``` r
write.csv(unnested, "result.csv", row.names = FALSE)
```

## Beta Regression

I saw that even when I use `prophet` and specify a capacity of 100 in
the model, some forecasts still exceed the 100 mark, therefore I will
implement a Beta regression model and see if the issue is fixed. I will
update this document with the results.

First I build the model for each data set.

``` r
betam <- function(df) {
  n     = dim(df)[1]
  df$y  = ((df$y/100)*(n-1)+0.5)/n
  m <- betareg(y ~ Expln + ds, data = df, link = "logit")
  return(m)
}

possbeta <- purrr::possibly(betam, otherwise = NA)

betamod <- wimax %>%
  mutate(ds = as.numeric(year(ds))) %>% nest(-Country) %>%
  mutate(model = map(data, possbeta)) %>%
  filter(!any(is.na(model))) %>% 
  ungroup()
```

The I make predictions using the model I built.

``` r
make_plots <- function(df1, df2) {
  
  gg <- ggplot(data = df2, aes(x = ds, y = q_0.5*100)) +
    geom_line(aes(), color = "blue") +
    geom_ribbon(aes(ymin = q_0.25*100, ymax = q_0.75*100), alpha = 0.3, fill = "blue") +
    geom_point(data = df1, aes(x = ds, y = y), colour = "red", size = 2) +
    scale_alpha(guide = 'none') + labs( x = "Year", y = "Coverage") +
    theme_bw()
  return(gg)
}

betaprd <- expln %>% 
  mutate(ds = as.numeric(year(ds))) %>% nest(-Country) %>%
  rename(pdat = data) %>% inner_join(betamod, by = "Country") %>% 
  mutate(frcst = map2(model, pdat, type = "quantile", at = c(0.25, 0.5, 0.75), predict)) %>% 
  mutate(pdat  = map2(pdat, frcst, ~ cbind(.x, .y))) %>% 
  ungroup()
```

Then iteratively plot the forecasts.

``` r
bplots <- betaprd %>% 
  mutate(plots = map2(data, pdat, make_plots)) %>% 
  mutate(ttldp = map2(plots, Country, ~(.x + labs(title = .y)))) %>% 
  ungroup()

for (bplot in bplots$ttldp) {
 print(bplot)
 cat('\n\n')
}
```

![](updated_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-8.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-9.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-10.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-11.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-12.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-13.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-14.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-15.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-16.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-17.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-18.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-19.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-20.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-21.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-22.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-23.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-24.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-25.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-26.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-27.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-28.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-29.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-30.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-31.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-32.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-33.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-34.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-35.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-36.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-37.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-38.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-39.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-40.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-41.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-42.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-43.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-44.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-45.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-46.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-47.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-48.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-49.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-50.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-51.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-52.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-53.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-54.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-55.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-56.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-57.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-58.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-59.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-60.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-61.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-62.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-63.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-64.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-65.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-66.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-67.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-68.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-69.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-70.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-71.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-72.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-73.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-74.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-75.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-76.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-77.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-78.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-79.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-80.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-81.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-82.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-83.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-84.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-85.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-86.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-87.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-88.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-89.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-90.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-91.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-92.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-93.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-94.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-95.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-96.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-97.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-98.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-99.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-100.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-101.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-102.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-103.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-104.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-105.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-106.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-107.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-108.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-109.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-110.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-111.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-112.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-113.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-114.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-115.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-116.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-117.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-118.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-119.png)<!-- -->

![](updated_files/figure-gfm/unnamed-chunk-14-120.png)<!-- -->

Here at least I made sure no forecast exceeds the 100 limit. I will save
the results.

``` r
betares <- unnest(betaprd, pdat) %>% 
  mutate(Year = ds, Pred = q_0.5*100) %>% select(Country, Year, Pred) %>% 
  spread(Year, value = Pred) %>% select("Country", "2021":"2030") %>% 
  left_join(extradat, by = c("Country" = "CountryName"), all.x = TRUE) %>%
  left_join(n_data, by = "Country", all.x = TRUE) %>% 
  select("Country","CountryCode":"2020","2021":"2030")


write.csv(betares, "result_beta.csv", row.names = FALSE)
```
