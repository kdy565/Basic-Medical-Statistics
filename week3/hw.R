getwd()

# 실습할 예제 데이터 불러오기
Data1 <- read.csv(file = "DATA1.csv", header = TRUE)
head(Data1)
View(Data1)

## (1) 정규성 확인

Data1_a <- Data1[Data1$BS1_1_re==1,]
Data1_b <- Data1[Data1$BS1_1_re==2,]

# 흡연자
qqnorm(Data1_a$HE_WC, main="current smoker")
qqline(Data1_a$HE_WC)

hist(Data1_a$HE_WC,main="current smoker", breaks = seq(min(Data1_a$HE_WC)-1, max(Data1_a$HE_WC)+1, by=1), freq=TRUE, ylim=c(0,10), col="coral") 
HE_WCrange<-seq(min(Data1_a$HE_WC), max(Data1_a$HE_WC), length=max(max(Data1_a$HE_WC)-min(Data1_a$HE_WC),1)) 
binwidth <- diff(range(Data1_a$HE_WC))/length(seq(min(Data1_a$HE_WC), max(Data1_a$HE_WC), by=1)) 
ND <- dnorm(HE_WCrange, mean=mean(Data1_a$HE_WC), sd=sd(Data1_a$HE_WC))
lines(HE_WCrange, ND * length(Data1_a$HE_WC) * binwidth, lwd=2, col="darkgreen")

# 비흡연자
qqnorm(Data1_b$HE_WC, main="non smoker")
qqline(Data1_b$HE_WC)

hist(Data1_b$HE_WC,main="non smoker", breaks = seq(min(Data1_b$HE_WC)-1, max(Data1_b$HE_WC)+1, by=1), freq=TRUE, ylim=c(0,15), col="coral") 
HE_WCrange<-seq(min(Data1_b$HE_WC), max(Data1_b$HE_WC), length=max(max(Data1_b$HE_WC)-min(Data1_b$HE_WC),1)) 
binwidth <- diff(range(Data1_b$HE_WC))/length(seq(min(Data1_b$HE_WC), max(Data1_b$HE_WC), by=1)) 
ND <- dnorm(HE_WCrange, mean=mean(Data1_b$HE_WC), sd=sd(Data1_b$HE_WC))
lines(HE_WCrange, ND * length(Data1_b$HE_WC) * binwidth, lwd=2, col="darkgreen")

# Boxplot 시각화화
install.packages("ggplot2")
library(ggplot2)

# Boxplot 흡연군
ggplot(data = Data1_a, aes(x = BS1_1_re, y = HE_WC)) +
  geom_boxplot(fill = "peachpuff", color = "orchid")

# Boxplot 비흡연군
ggplot(data = Data1_b, aes(x = BS1_1_re, y = HE_WC)) +
  geom_boxplot(fill = "tomato", color = "firebrick")

#합친 그림
ggplot(data = Data1, aes(x = factor(BS1_1_re), y = HE_WC)) +
  geom_boxplot(fill = "tomato", color = "firebrick") +
  xlab("BS1_1_re") + ylab("HE_WC")


## (2) 정규성 검정
## 1. Shapiro-Wilk test
shapiro.test(Data1_a$HE_WC) 
shapiro.test(Data1_b$HE_WC) 

## 2. Lilliefors test (수정된 Kolmogorov-Smirnov test)
library("nortest")
lillie.test(Data1_a$HE_WC) 
lillie.test(Data1_b$HE_WC) 

# 흡연군
summary(Data1_a$HE_WC)
length(Data1_a$HE_WC)
IQR(Data1_a$HE_WC)
sd(Data1_a$HE_WC)

# 비흡연군
summary(Data1_b$HE_WC)
length(Data1_b$HE_WC)
IQR(Data1_b$HE_WC)
sd(Data1_b$HE_WC)

## 1-2 등분산 검정 levene.test()
install.packages("lawstat")
library("lawstat")
levene.test(Data1$HE_WC, Data1$BS1_1_re, location="mean")

## (2) 독립표본 T검정
t.test(Data1_a$HE_WC,Data1_b$HE_WC,var.equal = TRUE)








# 실습할 예제 데이터 불러오기
Data4 <- read.csv(file = "DATA4.csv", header = TRUE)
head(Data4)
View(Data4)

# QQ
qqnorm(Data4$diff_trt, main="Data4$diff_trt")
qqline(Data4$diff_trt)

#히스토그램
hist(Data4$diff_trt,main="Data4$diff_trt", breaks = seq(min(Data4$diff_trt)-1, max(Data4$diff_trt)+1, by=1), freq=TRUE, ylim=c(0,7), col="coral") 
HE_WCrange<-seq(min(Data4$diff_trt), max(Data4$diff_trt), length=max(max(Data4$diff_trt)-min(Data4$diff_trt),1)) 
binwidth <- diff(range(Data4$diff_trt))/length(seq(min(Data4$diff_trt), max(Data4$diff_trt), by=1)) 
ND <- dnorm(HE_WCrange, mean=mean(Data4$diff_trt), sd=sd(Data4$diff_trt))
lines(HE_WCrange, ND * length(Data4$diff_trt) * binwidth, lwd=2, col="darkgreen")

# Boxplot 시각화화
install.packages("ggplot2")
library(ggplot2)
ggplot(data = Data4, aes(y = diff_trt)) +
  geom_boxplot(fill = "peachpuff", color = "orchid")

## (2) 정규성 검정
## 1. Shapiro-Wilk test
shapiro.test(Data4$diff_trt) 

## 2. Lilliefors test (수정된 Kolmogorov-Smirnov test)
library("nortest")
lillie.test(Data4$diff_trt) 

# 요약
summary(Data4$diff_trt)
length(Data4$diff_trt)
IQR(Data4$diff_trt)
sd(Data4$diff_trt)

## (3) 대응표본 T검정 (Paired t-test)
t.test(Data4$after_trt, Data4$before_trt, paired=TRUE) 

