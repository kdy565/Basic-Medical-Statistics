getwd()
install.packages("readr")
library("readr")
data1500<-read_csv("sample_1500.csv")


##Wt1

###정규성 검정, for BMI
#Histogram: by probability
hist(data1500$Wt1, breaks = seq(min(data1500$Wt1)-1, max(data1500$Wt1)+1, by=1), prob=TRUE, col="skyblue")
Wt1range<-seq(min(data1500$Wt1),max(data1500$Wt1),length=max(max(data1500$Wt1)-min(data1500$Wt1),1))
ND<-dnorm(Wt1range,mean=mean(data1500$Wt1),sd=sd(data1500$Wt1))
lines(Wt1range, ND, lwd=2, col="magenta")

#Histogram: by frequency
hist(data1500$Wt1, breaks = seq(min(data1500$Wt1)-1, max(data1500$Wt1)+1, by=1), freq=TRUE, ylim=c(0,250), col="coral") 
Wt2range<-seq(min(data1500$Wt1), max(data1500$Wt1), length=max(max(data1500$Wt1)-min(data1500$Wt1),1)) 
binwidth <- diff(range(data1500$Wt1))/length(seq(min(data1500$Wt1), max(data1500$Wt1), by=1)) 
ND <- dnorm(Wt1range, mean=mean(data1500$Wt1), sd=sd(data1500$Wt1))
lines(Wt1range, ND * length(data1500$Wt1) * binwidth, lwd=2, col="darkgreen")

#Q-Q plot
qqnorm(data1500$Wt1)
qqline(data1500$Wt1)

#Shapiro-Wilk, Kolmogorov-Smirnov test
shapiro.test(data1500$Wt1)
ks.test(data1500$Wt1, "pnorm", mean=mean(data1500$Wt1), sd=sd(data1500$Wt1))


##Wt2

###정규성 검정, for BMI
#Histogram: by probability
hist(data1500$Wt2, breaks = seq(min(data1500$Wt2)-1, max(data1500$Wt2)+1, by=1), prob=TRUE, col="skyblue")
Wt2range<-seq(min(data1500$Wt2),max(data1500$Wt2),length=max(max(data1500$Wt2)-min(data1500$Wt2),1))
ND<-dnorm(Wt2range,mean=mean(data1500$Wt2),sd=sd(data1500$Wt2))
lines(Wt2range, ND, lwd=2, col="magenta")

#Histogram: by frequency
hist(data1500$Wt2, breaks = seq(min(data1500$Wt2)-1, max(data1500$Wt2)+1, by=1), freq=TRUE, ylim=c(0,250), col="coral") 
Wt2range<-seq(min(data1500$Wt2), max(data1500$Wt2), length=max(max(data1500$Wt2)-min(data1500$Wt2),1)) 
binwidth <- diff(range(data1500$Wt2))/length(seq(min(data1500$Wt2), max(data1500$Wt2), by=1)) 
ND <- dnorm(Wt2range, mean=mean(data1500$Wt2), sd=sd(data1500$Wt2))
lines(Wt2range, ND * length(data1500$Wt2) * binwidth, lwd=2, col="darkgreen")

#Q-Q plot
qqnorm(data1500$Wt2)
qqline(data1500$Wt2)

#Shapiro-Wilk, Kolmogorov-Smirnov test
shapiro.test(data1500$Wt2)
ks.test(data1500$Wt2, "pnorm", mean=mean(data1500$Wt2), sd=sd(data1500$Wt2))







univariate<-function(x){
  n<-length(x)-sum(is.na(x))
  missing<-sum(is.na(x))
  mean<-mean(x)
  sd<-sd(x)
  var<-var(x)
  se<-sqrt(var/n)
  min<-min(x)
  max<-max(x)
  median<-median(x)
  mode <- function(x) {
    nodup <- unique(x)
    if (length(which(tabulate(match(x, nodup))==max(tabulate(match(x, nodup)))))>=2 )
    {NA}
    else{nodup[which.max(tabulate(match(x, nodup)))]}
  }
  mode<-mode(x)
  lclm<-mean-qt(0.975, n-1)*se
  uclm<-mean+qt(0.975, n-1)*se
  
  P25<-data.frame(quantile(x)[2])[1,1]
  P50<-data.frame(quantile(x)[3])[1,1]
  P75<-data.frame(quantile(x)[4])[1,1]
  qrange<-data.frame(quantile(x)[4])[1,1]-data.frame(quantile(x)[2])[1,1]
  name<-c("N", "N of missing","Mean", "Standard Deviation", "Variation", "Standard Error", "Minimum", 
          "Maximum", "Median", "Mode", "95% Lower limit of mean (two-sided)", "95% Upper limit of mean(two-sided)", "P25", "P50", "P75", "Quantile Range")
  value<-c(n, missing, mean, sd, var, se, min, max, median, mode, lclm, uclm, P25, P50, P75, qrange)
  result<-cbind(name, value)
  print(result)
}

#Now for kurtosis and skewness
install.packages("moments")
library(moments)






df_hthxTrue<-data1500[data1500$HTHX==1,]
df_hthxFalse<-data1500[data1500$HTHX==0,]

univariate(df_hthxTrue$BMI)
skewness(df_hthxTrue$BMI)
kurtosis(df_hthxTrue$BMI)

univariate(df_hthxFalse$BMI)
skewness(df_hthxFalse$BMI)
kurtosis(df_hthxFalse$BMI)

