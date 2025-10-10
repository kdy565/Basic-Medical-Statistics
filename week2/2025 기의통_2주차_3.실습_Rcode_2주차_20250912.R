#이항분포 확률 계산 문제
for(i in 5:0){
  a <- dbinom(i, size = 10, prob = 0.7)
  print(a)
}

pbinom(5, size = 10, prob = 0.7)
sum(dbinom(0:5, size = 10, prob = 0.7))

##Start of Practice 25.09.12##
#####Working Directory 지정 
getwd() #현재 지정된 워킹디렉토리 확인
#각자 지정한 경로 불러오기; 역슬래시\ -> 슬래시로/ 변경 필요!

#####데이터 불러오기: CSV 사용시
install.packages("readr")
library("readr")
data1500<-read_csv("sample_1500.csv")


?seq #함수를 모를 때 ?함수명 으로 찾아보기 
#ChatGPT or 기타 LLM 활용해도 됨!

#####NOTES#####
#1. Variables are case sensitive
#2. Need internet for downloading the packages

#####Q2. 

###정규성 검정, for BMI
#Histogram: by probability
hist(data1500$BMI, breaks = seq(min(data1500$BMI)-1, max(data1500$BMI)+1, by=1), prob=TRUE, col="skyblue")
BMIrange<-seq(min(data1500$BMI),max(data1500$BMI),length=max(max(data1500$BMI)-min(data1500$BMI),1))
ND<-dnorm(BMIrange,mean=mean(data1500$BMI),sd=sd(data1500$BMI))
lines(BMIrange, ND, lwd=2, col="magenta")

#Histogram: by frequency
hist(data1500$BMI, breaks = seq(min(data1500$BMI)-1, max(data1500$BMI)+1, by=1), freq=TRUE, ylim=c(0,250), col="coral") 
BMIrange<-seq(min(data1500$BMI), max(data1500$BMI), length=max(max(data1500$BMI)-min(data1500$BMI),1)) 
binwidth <- diff(range(data1500$BMI))/length(seq(min(data1500$BMI), max(data1500$BMI), by=1)) 
ND <- dnorm(BMIrange, mean=mean(data1500$BMI), sd=sd(data1500$BMI))
lines(BMIrange, ND * length(data1500$BMI) * binwidth, lwd=2, col="darkgreen")

#Q-Q plot
qqnorm(data1500$BMI)
qqline(data1500$BMI)

#Shapiro-Wilk, Kolmogorov-Smirnov test
shapiro.test(data1500$BMI)
ks.test(data1500$BMI, "pnorm", mean=mean(data1500$BMI), sd=sd(data1500$BMI))

install.packages("nortest")
library("nortest")
lillie.test(data1500$BMI)

#####Q4. 
#최소, 최대, 첨도, 왜도,  평균, 표준오차, 표준편차, 분산

####기술통계량
#기술통계 함수 작성
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

#기술 통계량 계산: "data1500$Wt2"자리에 원하는 변수를 넣으면 된다.
univariate(data1500$BP_HIGH)

#Now for kurtosis and skewness
install.packages("moments")
library(moments)
kurtosis(data1500$BP_HIGH)
skewness(data1500$BP_HIGH)

#####Q5.
df_men<-data1500[data1500$sex==1,]#post comma accesses column(think x,y)
df_women<-data1500[data1500$sex==2,]

univariate(df_men$BP_HIGH)
skewness(df_men$BP_HIGH)
kurtosis(df_men$BP_HIGH)

univariate(df_women$BP_HIGH)
skewness(df_women$BP_HIGH)
kurtosis(df_women$BP_HIGH)


#End of Practice 24.03.15#