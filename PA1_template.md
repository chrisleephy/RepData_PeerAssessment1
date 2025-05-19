---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: true
  pdf_document: default
  word_document: default
---

## Loading and preprocessing the data


``` r
#Use fread() to automatically handles the .csv file
#transfer data to tibble.
#load packages

library(data.table)
library(knitr)
library(ggplot2)
suppressPackageStartupMessages(library(dplyr))

# Unzip the file to a temporary location
temp_file <- tempfile(fileext = ".csv")
unzip("activity.zip", files = "activity.csv", exdir = tempdir())

# Read the CSV file using fread
df <- as_tibble(fread(file.path(tempdir(), "activity.csv")))
```


``` r
#inspecting the read data frame
str(df);summary(df)
```

```
## tibble [17,568 × 3] (S3: tbl_df/tbl/data.frame)
##  $ steps   : int [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : IDate[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: int [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?


``` r
#histogram of the total number of steps taken each day
df_summary <- df %>% group_by(date) %>% 
  summarise(total_steps=sum(steps))

md <- median(df_summary$total_steps,na.rm=TRUE); me <- mean(df_summary$total_steps,na.rm=TRUE)

p1 <- df_summary %>% ggplot(.,mapping=aes(total_steps)) + geom_histogram(bins=20) + 
  labs(title="Total number of steps taken each day") +
  theme(plot.title = element_text(hjust = 0.5)) + 
geom_vline(aes(xintercept = md, linetype = "Median"), color = "red", linewidth = 1,alpha=0.5)+
  geom_vline(aes(xintercept = me, linetype = "Mean"), color = "blue", linewidth = 1,alpha=0.5)
print(p1)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
cat(sprintf("The median is %.3f and the mean is %.3f\n", md, me))
```

```
## The median is 10765.000 and the mean is 10766.189
```

from the plot and calculation, we can see that the mean and median is very close.

## What is the average daily activity pattern?

``` r
#load necessary package
library(lubridate)
library(hms)
```


``` r
#first covert interval to time type.

df$interval <- sprintf("%04d",df$interval)
df$interval <- paste0(substr(df$interval, 1, 2), ":", substr(df$interval, 3, 4))
df$interval <- parse_hm(df$interval)

#average the steps across all days, create df_timeseries for plotting, remove NA
df_timeseries <- df %>% group_by(interval) %>% summarise(.,average_steps=mean(steps,na.rm=TRUE))

#plot average_steps vs interval
tplot <- ggplot(data=df_timeseries,mapping=aes(x=interval,y=average_steps)) + geom_line() +
  labs(title="Timeseries for average steps across all days",x="Time interval",y="average steps") 

#the maxnium pair can be found by:
max_pair <- df_timeseries[which.max(df_timeseries$average_steps),]

#label it on the plot
p2 <- tplot + geom_text(data=max_pair,mapping=aes(x=interval,y=average_steps,label = paste0("Max: ", round(average_steps, 2))),color="red",size=4)
print(tplot)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### From above plot we can see that the maximum number of steps is 206.17 at time interval 08:35:00.


## Imputing missing values
first checking how many NA in each columns and the % of NA:

``` r
df %>% select(steps:interval) %>% is.na() %>% {rbind(colSums(.),colMeans(.))} %>% 
  `rownames<-`(c("colSums", "colMeans"))
```

```
##                 steps date interval
## colSums  2304.0000000    0        0
## colMeans    0.1311475    0        0
```
we can see that there are 2304 missing value in steps which is about 13%, and no missing value in date and interval, and be observation, the NA happens is whole day and the dataset contain a lots of 0, so I will simply imput the NA value as 0.

``` r
#select all the elements which is NA and replace by 0
#create the new dataframe df2 with NA replace by 0
df2 <- df;df2[is.na(df2)] <- 0

#plot the histogram again using the same code from part 1
df_summary2 <- df2 %>% group_by(date) %>% 
  summarise(total_steps=sum(steps))

md2 <- median(df_summary2$total_steps,na.rm=TRUE); me2 <- mean(df_summary2$total_steps,na.rm=TRUE)

p3 <- df_summary2 %>% ggplot(.,mapping=aes(total_steps)) + geom_histogram(bins=20) + 
  labs(title="Total number of steps taken each day") +
  theme(plot.title = element_text(hjust = 0.5)) + 
geom_vline(aes(xintercept = md2, linetype = "Median"), color = "red", linewidth = 1,alpha=0.5)+
  geom_vline(aes(xintercept = me2, linetype = "Mean"), color = "blue", linewidth = 1,alpha=0.5)

print(p3)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

``` r
cat(sprintf("The median is %.3f and the mean is %.3f\n", md2, me2))
```

```
## The median is 10395.000 and the mean is 9354.230
```
compare the plot to first part we can see that the median and mean is reduced as expected, as we replace NA with 0, so we did not just ignore NA, and the mean and median should drop.

## Are there differences in activity patterns between weekdays and weekends?
first convert the date to weekdays using weekdays function,
and create a function to check weekday or weekend

``` r
df2$date <- weekdays(df2$date)
is_weekend <- function(date) {
  Sys.setlocale("LC_TIME", "C")  # Set to English locale
  date %in% c("Saturday", "Sunday")
}
```
create a column weekday factor and calculate the mean_steps conditional on the factor

``` r
 df2$weekfactor <- factor(
  ifelse(is_weekend(df2$date), "Weekdend", "Weekday"),
  levels = c("Weekdend", "Weekday")
)

df2_mean <- df2 %>% group_by(weekfactor,interval) %>% summarise(mean_steps=mean(steps),.groups = "drop")
```
plot the mean_steps vs interval



``` r
p4 <- ggplot(df2_mean, aes(x = as.numeric(interval), y = mean_steps, group = weekfactor)) +
  geom_line() +
  facet_wrap(~ weekfactor, nrow = 2) +
  scale_x_continuous(labels = function(x) format(as.POSIXct(x, tz = "UTC"), "%H:%M")) +
  labs(x = "Time (HH:MM)", y = "Mean Steps") +
  theme_minimal()

print(p4)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
saving the figue to the figure directory

``` r
# Assuming p1, p2, p3, p4 are ggplot objects
plots <- list(p1, p2, p3, p4)
for (i in 1:4) {
  ggsave(paste0("figure/p", i, ".png"), plot = plots[[i]], width = 8, height = 6, dpi = 300)
}
```

```
## Warning: Removed 8 rows containing non-finite outside the scale range
## (`stat_bin()`).
```



