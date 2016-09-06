

## Loading and preprocessing the data
Read the csv file to viable "data"

```r
data<-read.csv("activity.csv" )
library(dplyr)
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

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
Delete rows with missing values. Get "dataDate".

```r
dataNoNa<-subset(data,!is.na(steps)&!is.na(date)&!is.na(interval)      )
dataDate<-dataNoNa
dataDate[,2]<-as.Date(dataNoNa[,2] )
summary(dataDate)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-29   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0
```

Group "dataDate" by date and sum steps

```r
Date<-group_by(dataDate, date)
dataTotal<-summarize(Date, StepsPerDay= sum(steps,na.rm=TRUE))
dataTotal<-as.data.frame(dataTotal)
head(dataTotal)
```

```
##         date StepsPerDay
## 1 2012-10-02         126
## 2 2012-10-03       11352
## 3 2012-10-04       12116
## 4 2012-10-05       13294
## 5 2012-10-06       15420
## 6 2012-10-07       11015
```

Make histgram

```r
hist(dataTotal$StepsPerDay, col='orange', main="with Na")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Calculate the mean and median total number of steps taken per day

```r
MeanSteps<-as.character(mean(dataTotal[,2],na.rm=TRUE))
MedianSteps<-median(dataTotal[,2],na.rm=TRUE)
```
The mean value is 10766.1886792453    
The median value is 10765

## What is the average daily activity pattern?
Group "dataDate" by interval.  Calculate mean values.

```r
Interval<-group_by(dataDate, interval)
dataAveraged<-summarize(Interval, StepsAveraged= mean(steps,na.rm=TRUE))
dataAveraged<-as.data.frame(dataAveraged)
```

Plot the data

```r
plot(dataAveraged$interval,dataAveraged$StepsAveraged, type="l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Calculate the max value

```r
MaxAverageInterval<-dataAveraged[match(max(dataAveraged$StepsAveraged),dataAveraged$StepsAveraged),1]
```
Max value comes from interval 835

## Imputing missing values
Make a array of rows with missing values. Count them.

```r
NaNumber<-sum(is.na(data[,1]))
```
The original data has 2304 NAs.  

Replace NA with the average steps of that interval.  Save as "dataNew"

```r
dataNew<-data
for(i in 1:nrow(data)){
	if (is.na(data[i,1])){
		currentInterval<-data[i,3]
		rn<-match(currentInterval,dataAveraged[,1])
		if(!is.na(rn)){temp<-dataAveraged[rn,2]}else{temp<-0}
		dataNew[i,1]<-temp
	}
}
head(dataNew)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
Make a histogram of the total number of steps taken each day

```r
Date<-group_by(dataNew, date)
dataTotal2<-summarize(Date, StepsPerDay= sum(steps,na.rm=TRUE))
dataTotal2<-as.data.frame(dataTotal2)
hist(dataTotal2$StepsPerDay, col='green', main="without Na")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
  
Calculate and report the mean and median total number of steps taken per day

```r
MeanSteps2<-as.character(mean(dataTotal2[,2],na.rm=TRUE))
MedianSteps2<-as.character(median(dataTotal2[,2],na.rm=TRUE))
```
The mean value of new data is 10766.1886792453, the same as the old mean value 10766.1886792453  
The median value of new data is 10766.1886792453, larger than the old median value 10765.

Compare these two hisgrams. 

```r
par(mfrow=c(1,2))
hist(dataTotal$StepsPerDay, col='orange',main="with Na", )
hist(dataTotal2$StepsPerDay, col='green', main="without Na")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

We find the first three columns from the left become taller after we replace Na with the average steps of that interval.  The other two columns on the right increase less.  The reason is:  suppose one day increases to the top two collumn.  That day should have large  original value before replacement otherwise it may only be able to stay at the left three collumns.  Large original value means a few Na.  Second, it should have a lot Na, otherwise it would not increase much after the replacement.  That's incoherent with the first implication.  Therefore, it rarely happens.  Therefore, the left three will increase a lot while the right two increase less.  Then, the median and mean value may not change much.


## Are there differences in activity patterns between weekdays and weekends?
Divide the data into two: dataD for weekdays, dataE for weekends.

```r
dataNew2<-dataNew
Days<-as.Date(dataNew$date)
DorE<-(weekdays(Days)=="星期児�<U+3E64>")|(weekdays(Days)=="星期旐�<U+3E35>")
dataNew3<-mutate(dataNew2, DorE=DorE)
dataD<-subset(dataNew3, DorE==FALSE)
dataE<-subset(dataNew3, DorE==TRUE)
summary(dataD)
summary(dataE)
```

```
## Error: invalid multibyte character in parser at line 3
```

Group each data by the day.

```r
Interval<-group_by(dataD, interval)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'dataD' not found
```

```r
dataDA<-summarize(Interval, StepsAveraged= mean(steps,na.rm=TRUE))
dataDA<-as.data.frame(dataDA)
dataDA$DorE<-rep(FALSE,nrow(dataDA))

Interval<-group_by(dataE, interval)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'dataE' not found
```

```r
dataEA<-summarize(Interval, StepsAveraged= mean(steps,na.rm=TRUE))
dataEA<-as.data.frame(dataEA)
dataEA$DorE<-rep(TRUE,nrow(dataEA))
```

Rbind them two.  The new dataset is saved as "dataNew4" 

```r
dataNew4<-rbind(dataDA,dataEA)
dataNew4$DorE<-factor(dataNew4$DorE, labels=c("Weekdays","Weekends"))
```
Plot the data

```r
library(lattice) 
xyplot(StepsAveraged~interval|DorE, dataNew4, layout=c(1,2), type='l' )
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)
