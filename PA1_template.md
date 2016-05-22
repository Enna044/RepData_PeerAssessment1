# PA1_template
Enna Martínez de Escobar  
22 de mayo de 2016  



##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

1.Load the data (i.e. read.csv())

```r
tb<-read.csv("activity.csv")
```

2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
tb$date<-as.Date(tb$date)
tb$weekday<-weekdays(tb$date)
tb$cat<-ifelse(tb$weekday=="sabado" | tb$weekday=="domingo","Weekend","Weekday")
```


##What is mean total number of steps taken per day?

1.Make a histogram of the total number of steps taken each day

```r
sumpas<-aggregate(steps~date,tb,sum)
barplot(height = sumpas$steps,names.arg = sumpas$date, xlab = "Date", ylab = "Steps", main = "Number of steps taken per day", col="red")
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->

```r
dev.copy(png, file="plot_q2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

2.Calculate and report the mean and median total number of steps taken per day

```r
meanst<-aggregate(steps~date,tb,mean,na.action = na.omit)
medst<-aggregate(steps~date,tb,median,na.action = na.omit)
names(meanst)[names(meanst)=="steps"] <- "Mean steps"
names(medst)[names(medst)=="steps"]<-"Median steps"
rep<-merge(meanst,medst)
library(pander)
```

```
## Warning: package 'pander' was built under R version 3.2.5
```

```r
pander(rep)
```


--------------------------------------
   date     Mean steps   Median steps 
---------- ------------ --------------
2012-10-02    0.4375          0       

2012-10-03    39.42           0       

2012-10-04    42.07           0       

2012-10-05    46.16           0       

2012-10-06    53.54           0       

2012-10-07    38.25           0       

2012-10-09    44.48           0       

2012-10-10    34.38           0       

2012-10-11    35.78           0       

2012-10-12    60.35           0       

2012-10-13    43.15           0       

2012-10-14    52.42           0       

2012-10-15     35.2           0       

2012-10-16    52.38           0       

2012-10-17    46.71           0       

2012-10-18    34.92           0       

2012-10-19    41.07           0       

2012-10-20    36.09           0       

2012-10-21    30.63           0       

2012-10-22    46.74           0       

2012-10-23    30.97           0       

2012-10-24    29.01           0       

2012-10-25    8.653           0       

2012-10-26    23.53           0       

2012-10-27    35.14           0       

2012-10-28    39.78           0       

2012-10-29    17.42           0       

2012-10-30    34.09           0       

2012-10-31    53.52           0       

2012-11-02    36.81           0       

2012-11-03     36.7           0       

2012-11-05    36.25           0       

2012-11-06    28.94           0       

2012-11-07    44.73           0       

2012-11-08    11.18           0       

2012-11-11    43.78           0       

2012-11-12    37.38           0       

2012-11-13    25.47           0       

2012-11-15    0.1424          0       

2012-11-16    18.89           0       

2012-11-17    49.79           0       

2012-11-18    52.47           0       

2012-11-19     30.7           0       

2012-11-20    15.53           0       

2012-11-21     44.4           0       

2012-11-22    70.93           0       

2012-11-23    73.59           0       

2012-11-24    50.27           0       

2012-11-25    41.09           0       

2012-11-26    38.76           0       

2012-11-27    47.38           0       

2012-11-28    35.36           0       

2012-11-29    24.47           0       
--------------------------------------

```r
rm(sumpas,meanst,medst,rep)
```

##What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mst<-aggregate(steps~interval,tb,mean,na.action = na.omit)
plot(mst$interval,mst$steps,type = "l",xlab = "5-minute interval",ylab = "Steps", col="blue",main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/plot3-1.png)<!-- -->

```r
dev.copy(png, file="plot_q3.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mst[mst$steps==max(mst$steps),1]
```

```
## [1] 835
```

```r
rm(mst)
```

##Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
is_na<-sum(is.na(tb$steps))
is.na
```

```
## function (x)  .Primitive("is.na")
```

```r
rm(is.na)
```

```
## Warning in rm(is.na): objeto 'is.na' no encontrado
```

2.Devise a strategy for filling in all of the missing values in the dataset. 

```r
na<-which(is.na(tb$steps))
mn<-mean(tb$steps,na.rm=TRUE)
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
tb2<-tb
tb2[na,"steps"]<-mn
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sumpas<-aggregate(steps~date,tb2,sum)
sumpas2<-aggregate(steps~date,tb,sum)
barplot(height=sumpas[,2],names.arg = sumpas$date, xlab = "Date", ylab = "Steps", main = "Number of steps taken per day", col="blue")
barplot(height=sumpas2[,2],names.arg = sumpas2$date, xlab = "Date", ylab = "Steps", main = "Number of steps taken per day", col="red", add = T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=2)
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

```r
dev.copy(png, file="plot_q4.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
meanst<-aggregate(steps~date,tb2,mean)
medst<-aggregate(steps~date,tb2,median)
names(meanst)[names(meanst)=="steps"] <- "Mean steps"
names(medst)[names(medst)=="steps"]<-"Median steps"
rep<-merge(meanst,medst)
library(pander)
pander(rep)
```


--------------------------------------
   date     Mean steps   Median steps 
---------- ------------ --------------
2012-10-01    37.38         37.38     

2012-10-02    0.4375          0       

2012-10-03    39.42           0       

2012-10-04    42.07           0       

2012-10-05    46.16           0       

2012-10-06    53.54           0       

2012-10-07    38.25           0       

2012-10-08    37.38         37.38     

2012-10-09    44.48           0       

2012-10-10    34.38           0       

2012-10-11    35.78           0       

2012-10-12    60.35           0       

2012-10-13    43.15           0       

2012-10-14    52.42           0       

2012-10-15     35.2           0       

2012-10-16    52.38           0       

2012-10-17    46.71           0       

2012-10-18    34.92           0       

2012-10-19    41.07           0       

2012-10-20    36.09           0       

2012-10-21    30.63           0       

2012-10-22    46.74           0       

2012-10-23    30.97           0       

2012-10-24    29.01           0       

2012-10-25    8.653           0       

2012-10-26    23.53           0       

2012-10-27    35.14           0       

2012-10-28    39.78           0       

2012-10-29    17.42           0       

2012-10-30    34.09           0       

2012-10-31    53.52           0       

2012-11-01    37.38         37.38     

2012-11-02    36.81           0       

2012-11-03     36.7           0       

2012-11-04    37.38         37.38     

2012-11-05    36.25           0       

2012-11-06    28.94           0       

2012-11-07    44.73           0       

2012-11-08    11.18           0       

2012-11-09    37.38         37.38     

2012-11-10    37.38         37.38     

2012-11-11    43.78           0       

2012-11-12    37.38           0       

2012-11-13    25.47           0       

2012-11-14    37.38         37.38     

2012-11-15    0.1424          0       

2012-11-16    18.89           0       

2012-11-17    49.79           0       

2012-11-18    52.47           0       

2012-11-19     30.7           0       

2012-11-20    15.53           0       

2012-11-21     44.4           0       

2012-11-22    70.93           0       

2012-11-23    73.59           0       

2012-11-24    50.27           0       

2012-11-25    41.09           0       

2012-11-26    38.76           0       

2012-11-27    47.38           0       

2012-11-28    35.36           0       

2012-11-29    24.47           0       

2012-11-30    37.38         37.38     
--------------------------------------

```r
rm(sumpas,meanst,medst,rep,tb2,na,mn)
```

##Are there differences in activity patterns between weekdays and weekends?

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
mst<-aggregate(steps~cat+interval,tb,mean,na.action = na.omit)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
ggplot(mst, aes(interval, steps))+geom_line()+facet_grid(cat~.)+xlab("5-minute interval")+ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/we-1.png)<!-- -->

```r
dev.copy(png, file="plot_q5.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
rm(mst)
```
