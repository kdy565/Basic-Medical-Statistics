### Basic Medical Statistics Week 3 Practice (2025.09.19.)
### Continuous Data analysis1: Comparing 2 groups
#---------------------------------------------#

#Setting working directory 
getwd()


#---------------------------------------------#

############### Quest 1 ###############

# Loading data for today's practice 실습할 예제 데이터 불러오기
Data1 <- read.csv(file = "DATA1.csv", header = TRUE)
head(Data1)
View(Data1)

## 1-1. 음주 여부에 따른 두 군의 폐기능검사 점수
## (1) 정규성 확인: Q-Q plot 그리기
# 전체
qqnorm(Data1$HE_FEV1FVC)
qqline(Data1$HE_FEV1FVC, col = "magenta")

# 데이터 나누기 (BD1=1: 음주력 없음, BD1=2: 음주력 있음)
## 방법 1: subset
Data1_a <- subset(Data1, BD1==1)
Data1_b <- subset(Data1, BD1==2)

### [참고] 방법 2
Data1_c <- Data1[Data1$BD1==1,]
Data1_d <- Data1[Data1$BD1==2,]

# 비음주군
qqnorm(Data1_a$HE_FEV1FVC, main="비음주군")
qqline(Data1_a$HE_FEV1FVC, col = "coral")

# 음주군
qqnorm(Data1_b$HE_FEV1FVC, main="음주군")
qqline(Data1_b$HE_FEV1FVC, col = "skyblue")


## (2) 정규성 확인: Histogram 그리기
# 전체 데이터셋
hist(Data1$HE_FEV1FVC, prob=TRUE) #default option
hist(Data1$HE_FEV1FVC, freq=TRUE) #빈도수를 표현하는 frequency 옵션이 유리할 수도 있음; 다만, 정규곡선 생성 시 코드가 약간 달라짐 

# Histogram 위에 정규곡선 그리기 
FEV1FVCrange <- seq(min(Data1$HE_FEV1FVC), max(Data1$HE_FEV1FVC), 
                    length=max(max(Data1$HE_FEV1FVC)-min(Data1$HE_FEV1FVC),100))
ND <- dnorm(FEV1FVCrange, mean=mean(Data1$HE_FEV1FVC), sd=sd(Data1$HE_FEV1FVC))
lines(FEV1FVCrange, ND, lwd=2, col = "orange")  

?dnorm #모르는 함수의 기능 찾아보기 

#------------------심화 학습습---------------------------#
# 이상한 정규 곡선이 그려지는 경우들
hist(Data1$HE_FEV1FVC, freq=TRUE)

FEV1FVCrange <- seq(min(Data1$HE_FEV1FVC), max(Data1$HE_FEV1FVC), 
                    length=max(max(Data1$HE_FEV1FVC)-min(Data1$HE_FEV1FVC),100))

binwidth <- diff(range(Data1$HE_FEV1FVC))/length(seq(min(Data1$HE_FEV1FVC), max(Data1$HE_FEV1FVC)))
ND <- dnorm(FEV1FVCrange,mean=mean(Data1$HE_FEV1FVC),sd=sd(Data1$HE_FEV1FVC))
lines(FEV1FVCrange, ND * length(Data1$HE_FEV1FVC) * binwidth, lwd=2, col="darkgreen")

#히스토그램 y축(frequency)과 dnorm()의 출력 scale을 맞추는 부분 때문에 발생하는 문제
#문제원인
#hist(..., freq=TRUE) → y축은 도수(count).
#dnorm() → 확률밀도(density) 값을 반환.
#그래서 단순히 겹치면 dnorm() 값은 너무 작아서 눈에 안 보임.
#이를 맞추기 위해 * length(data) * binwidth로 스케일을 보정했는데, 여전히 히스토그램의 도수 스케일과 잘 안 맞을 수 있음.

#히스토그램을 빈도(도수, frequency)가 아니라 밀도(density)로 그리면, dnorm() 결과를 그대로 덧씌울 수 있음!
hist(Data1$HE_FEV1FVC,
     prob = TRUE,   # 또는 freq= FALSE 옵션-> y축을 density scale로 변경
     main = "Histogram with density",
     xlab = "FEV1/FVC",
     col = "black",
     border = "white")

curve(dnorm(x,
            mean = mean(Data1$HE_FEV1FVC),
            sd   = sd(Data1$HE_FEV1FVC)),
      col = "yellow", lwd = 2, add = TRUE)

# 반드시 freq=TRUE를 유지해서 그리고 싶을 때 (y축=빈도, frequency)
#이 경우에는 dnorm() 밀도를 count scale로 강제로 변환해야 함

h <- hist(Data1$HE_FEV1FVC, freq = TRUE,
          main = "Histogram with frequency",
          xlab = "FEV1/FVC",
          col = "purple", border = "white")

xfit <- seq(min(Data1$HE_FEV1FVC), max(Data1$HE_FEV1FVC), length = 100)
yfit <- dnorm(xfit, mean = mean(Data1$HE_FEV1FVC), sd = sd(Data1$HE_FEV1FVC))

# count scale로 맞추기
yfit <- yfit * diff(h$mids[1:2]) * length(Data1$HE_FEV1FVC)

lines(xfit, yfit, col = "gold", lwd = 2)

#--------------------심화 학습 끝-------------------------#


# 비음주군
hist(Data1_a$HE_FEV1FVC, main="비음주군", prob=TRUE)
FEV1FVCrange_a <- seq(min(Data1_a$HE_FEV1FVC), max(Data1_a$HE_FEV1FVC), 
                      length=max(max(Data1_a$HE_FEV1FVC)-min(Data1_a$HE_FEV1FVC),100))
ND_a <- dnorm(FEV1FVCrange_a, mean=mean(Data1_a$HE_FEV1FVC), sd=sd(Data1_a$HE_FEV1FVC))
lines(FEV1FVCrange_a, ND_a, lwd=2, col = "coral")  

# 음주군
hist(Data1_b$HE_FEV1FVC, main="음주군", prob=TRUE)
FEV1FVCrange_b <- seq(min(Data1_b$HE_FEV1FVC), max(Data1_b$HE_FEV1FVC), 
                      length=max(max(Data1_b$HE_FEV1FVC)-min(Data1_b$HE_FEV1FVC),100))
ND_b <- dnorm(FEV1FVCrange_b, mean=mean(Data1_b$HE_FEV1FVC), sd=sd(Data1_b$HE_FEV1FVC))
lines(FEV1FVCrange_b, ND_b, lwd=2, col = "skyblue") 


## (3) 정규성 검정
## 1. Shapiro-Wilk test
# H0: 비음주 군은 정규 분포를 따른다, H1: 비음주 군은 정규 분포를 따르지 않는다. 
shapiro.test(Data1_a$HE_FEV1FVC) 


# H0: 음주 군은 정규 분포를 따른다, H1: 음주 군은 정규 분포를 따르지 않는다. 
shapiro.test(Data1_b$HE_FEV1FVC)

## 2. Lilliefors test (수정된 Kolmogorov-Smirnov test)
install.packages("nortest")
library("nortest")

# H0: 비음주 군은 정규 분포를 따른다, H1: 비음주 군은 정규 분포를 따르지 않는다. 
lillie.test(Data1_a$HE_FEV1FVC) 

# H0: 음주 군은 정규 분포를 따른다, H1: 음주 군은 정규 분포를 따르지 않는다. 
lillie.test(Data1_b$HE_FEV1FVC)

## (4) 요약 통계량: 정규성이 기각되지 않으므로, 평균과 표준편차로 분포 요약 가능

summary(Data1$sex)

# 비음주군
length(Data1_a$HE_FEV1FVC)
mean(Data1_a$HE_FEV1FVC)
sd(Data1_a$HE_FEV1FVC)

# 음주군
length(Data1_b$HE_FEV1FVC)
mean(Data1_b$HE_FEV1FVC)
sd(Data1_b$HE_FEV1FVC)

### [참고] 다양한 기술통계량 구하기
#1) 기본 패키지의 SUMMARY 함수
noalc_result1 <- summary(Data1_a$HE_FEV1FVC)
noalc_result1
alc_result1 <- summary(Data1_b$HE_FEV1FVC)
alc_result1

#2) psych 패키지의 describe 함수
install.packages("psych")
library(psych)
noalc_result2 <- describe(Data1_a$HE_FEV1FVC)
noalc_result2
alc_result2 <- describe(Data1_b$HE_FEV1FVC)
alc_result2




## 1-2. 음주 여부에 따른 폐기능 점수의 차이가 있는가? 
## 등분산성을 만족한다면 독립표본 T검정 수행 

## (1) 등분산 검정
# H0: 비음주 군& 음주 군의 폐기능검사 점수 분산은 같다, H1: 두 군의 폐기능검사 점수 분산은 같지 않다 

## levene.test()
install.packages("lawstat")
library("lawstat")
levene.test(Data1$HE_FEV1FVC, Data1$BD1, location="mean")


## (2) 독립표본 T검정
# H0: 비음주 군& 음주 군의 폐기능검사 점수 평균은 같다, H1: 두 군의 폐기능검사 점수 평균은 같지 않다 
t.test(Data1_a$HE_FEV1FVC, Data1_b$HE_FEV1FVC, var.equal=TRUE)
#---------------------------------------------#

############### Quest 2 ###############

# 실습할 예제 데이터 불러오기
Data2 <- read.csv(file = "DATA2.csv", header = TRUE)
head(Data2)
View(Data2)

## 2-1. 음주 여부에 따른 두 군의 공복혈당수치 
## (1) 정규성 확인: Q-Q Plot, Histogram 그리기

# 비음주군, 음주군 나누기 
Data2_a <- Data2[Data2$BD1==1,]
Data2_b <- Data2[Data2$BD1==2,]

# 비음주군
qqnorm(Data2_a$HE_glu, main="비음주군")
qqline(Data2_a$HE_glu)
hist(Data2_a$HE_glu, main="비음주군", prob=TRUE, col = 'yellowgreen')

# 음주군
qqnorm(Data2_b$HE_glu, main="음주군")
qqline(Data2_b$HE_glu)
hist(Data2_b$HE_glu, main="음주군", prob=TRUE, col = 'orange')


## (2) 정규성 검정
## 1. Shapiro-Wilk test
# H0: 비음주 군의 공복혈당수치는는 정규 분포를 따른다, H1: 정규 분포를 따르지 않는다. 
shapiro.test(Data2_a$HE_glu) 
# H0: 음주 군의 공복혈당수치는는 정규 분포를 따른다, H1: 정규 분포를 따르지 않는다.
shapiro.test(Data2_b$HE_glu) 

## 2. Lilliefors test (수정된 Kolmogorov-Smirnov test)
library("nortest")
lillie.test(Data2_a$HE_glu) 
lillie.test(Data2_b$HE_glu) 


## (3) 요약 통계량: 정규성을 만족하지 않음, 중위수, 최소값, 최대값으로 요약
# 전체
summary(Data2$HE_glu)
length(Data2$HE_glu)
IQR(Data2$HE_glu)
median(Data2$HE_glu)
min(Data2$HE_glu)
max(Data2$HE_glu)

# 비음주군
summary(Data2_a$HE_glu)
length(Data2_a$HE_glu)
IQR(Data2_a$HE_glu)
median(Data2_a$HE_glu)
min(Data2_a$HE_glu)
max(Data2_a$HE_glu)

# 음주군
summary(Data2_b$HE_glu)
length(Data2_b$HE_glu)
IQR(Data2_b$HE_glu)
median(Data2_b$HE_glu)
min(Data2_b$HE_glu)
max(Data2_b$HE_glu)

### [참고] describe() 이용하기
total_result <- describe(Data2$HE_glu)
total_result
noalc_result2 <- describe(Data2_a$HE_glu)
noalc_result2
alc_result2 <- describe(Data2_b$HE_glu)
alc_result2

# Boxplot 시각화화
install.packages("ggplot2")
library(ggplot2)

# Boxplot 비음주군
ggplot(data = Data2_a, aes(x = BD1, y = HE_glu)) +
  geom_boxplot(fill = "peachpuff", color = "orchid")

# Boxplot 음주군
ggplot(data = Data2_b, aes(x = BD1, y = HE_glu)) +
  geom_boxplot(fill = "tomato", color = "firebrick")


## 2-2. 음주 여부에 따른 공복혈당 수치의 차이가 있는가? 
## Mann-Whitney U test (Wilcoxon Rank Sum Test)  
# H0: 비음주 군과 음주 군의 공복혈당 수치의 분포는 같다, H1: 두 군의 공복혈당 수치의 분포는 다르다.
wilcox.test(Data2_a$HE_glu, Data2_b$HE_glu, correct=FALSE)

#---------------------------------------------#



############### Quest 3 ###############

# 실습할 예제 데이터 불러오기
Data3 <- read.csv(file = "DATA3.csv", header = TRUE)
head(Data3)
View(Data3)

## 3-1. [모수적 방법] 재활치료 전후 신체기능 점수에 차이가 있는가 
## (1) 정규성 확인: Q-Q Plot, Histogram 그리기


qqnorm(Data3$diff)
qqline(Data3$diff, col = 'hotpink')
hist(Data3$diff, prob=TRUE)

diff <- seq(min(Data3$diff), max(Data3$diff), 
                    length=max(max(Data3$diff)-min(Data3$diff),3000))
ND3000 <- dnorm(diff, mean=mean(Data3$diff), sd=sd(Data3$diff))

lines(diff, ND3000, lwd=2, col = "purple")  

qqnorm(Data3$before)
qqline(Data3$before, col = 'orange')
hist(Data3$before, prob=TRUE)

qqnorm(Data3$after)
qqline(Data3$after, col = 'lightblue')
hist(Data3$after, prob=TRUE)

## (2) 정규성 검정
## 1. Shapiro-Wilk test
# H0: 재활치료 전후 신체기능 점수 분포는는 정규 분포를 따른다, H1: 정규 분포를 따르지 않는다. 
shapiro.test(Data3$diff) 

## 2. Lilliefors test
lillie.test(Data3$diff)

## (3) 대응표본 T검정 (Paired t-test)
# H0: 재활치료 전후 신체기능 점수 평균은 같다, H1: 재활치료 전후 신체기능 점수 평균은 다르다.
t.test(Data3$after, Data3$before, paired=TRUE) 

### [참고] boxplot 그리기
boxplot(Data3$before , Data3$after ,data=pt, 
        names=c("재활치료 전", "재활치료 후"), cex.names=10, col=c("orange","lightblue"), las=1)  


## 3-2. [비모수적 방법] 재활치료 전후 신체기능 점수에 차이가 있는가 
## (1) paired-sample Wilcoxon test (= Wilcoxon signed-rank test)
# H0: 재활치료 전후 신체기능 점수 중앙값은 같다, H1: 재활치료 전후 신체기능 점수 중앙값은 다르다.
wilcox.test(Data3$after, Data3$before, paired=TRUE, alternative="two.sided")  

### [참고] 
res<-wilcox.test(Data3$after, Data3$before, paired=TRUE, alternative="two.sided")
res$p.value
#---------------------------------------------#

#진짜 끝#

