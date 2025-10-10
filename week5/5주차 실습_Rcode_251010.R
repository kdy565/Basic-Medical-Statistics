# 2025.10.10  기초의학통계학 5주차 실습: 상관 및 회귀분석 1

setwd("~~~") # csv파일을 저장한 폴더 지정


############### 문제 1 ###############
# 1. 실습할 예제 데이터 불러오기

install.packages("readr")
library("readr")
data1<-read_csv("sample_1500.csv")
View(data1) #V는 대문자


#2. 산점도 그리기 (scatter plot)
plot(x=data1$BMI,y=data1$BP_LWST)
plot(data1$BMI,data1$BP_LWST)



############### 문제 2-1 ###############
#### 1. 정규성 검정

# 1) Q-Q plot
qqnorm(data1$BP_LWST)
qqline(data1$BP_LWST)

qqnorm(data1$BMI)
qqline(data1$BMI)

# 2) histogram
hist(data1$BP_LWST)

hist(data1$BMI)


# 3) statistical test
#Shapiro-Wilk test 
# H0: 해당 변수는 정규분포를 따른다, H1: 해당 변수는 정규분포를 따르지 않는다 
shapiro.test(data1$BP_LWST)
shapiro.test(data1$BMI)

#Lilliefors test (수정된 Kolmogorov-Smirnov test)
install.packages("nortest")
library("nortest")
lillie.test(data1$BP_LWST)
lillie.test(data1$BMI)


############### 문제 2-3 ###############
# Pearson's correlation
cor.test(data1$BP_LWST, data1$BMI)
cor.test(data1$BP_LWST, data1$BMI, method="pearson")



############### 문제 3 ###############
#단순회귀모형 (linear regression)
LR1<-lm(BP_LWST~BMI, data=data1)
summary(LR1)