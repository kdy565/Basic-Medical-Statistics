### 기초의학통계학 4주차 실습 (2025.09.26.)
### Basic Medical Statistics Week 3 Practice (2025.09.26.)
### 연속형 자료분석2: 세 군간 이상 비교
### Continuous Data analysis1: Comparing 3+ groups
#---------------------------------------------#

#자료 폴더(working directory) 지정하기
getwd()
#---------------------------------------------#

############### 문제 1 ###############

# 1. 실습할 예제 데이터 불러오기
Data <- read.csv(file = "Data.csv", header = TRUE)
head(Data)
View(Data)

# 2. 정규성 확인: (1) Q-Q plot, (2) Histogram 그리기 (3) 정규성 검정

# 소득수준별 데이터 나누기 
inc_1 <- subset(Data, income==1)
inc_2 <- subset(Data, income==2)
inc_3 <- subset(Data, income==3)
inc_4 <- subset(Data, income==4)

# (1) Q-Q plot 
qqnorm(inc_1$HE_CHOL, main="Income=1")
qqline(inc_1$HE_CHOL, col="magenta")
qqnorm(inc_2$HE_CHOL, main="Income=2")
qqline(inc_2$HE_CHOL, col="yellow")
qqnorm(inc_3$HE_CHOL, main="Income=3")
qqline(inc_3$HE_CHOL, col="purple")
qqnorm(inc_4$HE_CHOL, main="Income=4")
qqline(inc_4$HE_CHOL, col="blue")

# (2) Histogram
?hist
hist(inc_1$HE_CHOL, main="Income=1", ylim=c(0,0.012), prob=TRUE)
HECHOLrange_1 <- seq(min(inc_1$HE_CHOL), max(inc_1$HE_CHOL), 
                     length=max(max(inc_1$HE_CHOL)-min(inc_1$HE_CHOL),100))
ND_1 <- dnorm(HECHOLrange_1, mean=mean(inc_1$HE_CHOL), sd=sd(inc_1$HE_CHOL))
lines(HECHOLrange_1, ND_1, lwd=2, col="blue")  

hist(inc_2$HE_CHOL, main="Income=2", ylim=c(0,0.012), prob=TRUE)
HECHOLrange_2 <- seq(min(inc_2$HE_CHOL), max(inc_2$HE_CHOL), 
                     length=max(max(inc_2$HE_CHOL)-min(inc_2$HE_CHOL),100))
ND_2 <- dnorm(HECHOLrange_2, mean=mean(inc_2$HE_CHOL), sd=sd(inc_2$HE_CHOL))
lines(HECHOLrange_2, ND_2, lwd=2, col="blue")  

hist(inc_3$HE_CHOL, main="Income=3", ylim=c(0,0.012), prob=TRUE)
HECHOLrange_3 <- seq(min(inc_3$HE_CHOL), max(inc_3$HE_CHOL), 
                     length=max(max(inc_3$HE_CHOL)-min(inc_3$HE_CHOL),100))
ND_3 <- dnorm(HECHOLrange_3, mean=mean(inc_3$HE_CHOL), sd=sd(inc_3$HE_CHOL))
lines(HECHOLrange_3, ND_3, lwd=2, col="blue")  

hist(inc_4$HE_CHOL, main="Income=4", ylim=c(0,0.012), prob=TRUE)
HECHOLrange_4 <- seq(min(inc_4$HE_CHOL), max(inc_4$HE_CHOL), 
                     length=max(max(inc_4$HE_CHOL)-min(inc_4$HE_CHOL),100))
ND_4 <- dnorm(HECHOLrange_4, mean=mean(inc_4$HE_CHOL), sd=sd(inc_4$HE_CHOL))
lines(HECHOLrange_4, ND_4, lwd=2, col="blue")  

# (3-1) Shapiro-Wilk test
# H0: 해당 군은 정규 분포를 따른다, H1: 정규 분포를 따르지 않는다. 
shapiro.test(inc_1$HE_CHOL) 
shapiro.test(inc_2$HE_CHOL)
shapiro.test(inc_3$HE_CHOL)
shapiro.test(inc_4$HE_CHOL)

# (3-2) Lilliefors test (수정된 Kolmogorov-Smirnov test)
library("nortest")
lillie.test(inc_1$HE_CHOL) 
lillie.test(inc_2$HE_CHOL)
lillie.test(inc_3$HE_CHOL)
lillie.test(inc_4$HE_CHOL)


## 3. 요약 통계량: 정규성이 기각되지 않으므로, 평균과 표준편차로 분포 요약 가능
length(inc_1$HE_CHOL)
mean(inc_1$HE_CHOL)
sd(inc_1$HE_CHOL)

length(inc_2$HE_CHOL)
mean(inc_2$HE_CHOL)
sd(inc_2$HE_CHOL)

length(inc_3$HE_CHOL)
mean(inc_3$HE_CHOL)
sd(inc_3$HE_CHOL)

length(inc_4$HE_CHOL)
mean(inc_4$HE_CHOL)
sd(inc_4$HE_CHOL)

#---------------------------------------------#

############### 문제 2 ###############

## 소득수준에 따라 총 콜레스테롤 수치에 차이가 있는가
# 1. 실습할 예제 데이터 불러오기
# 2. 등분산성 검정

### Levene's test
# HO: 소득수준에 따른 네 집단 사이의 총 콜레스테롤 수치의 분산은 동일하다.
# H1: 소득수준에 따른 네 집단 사이의 총 콜레스테롤 수치의 분산은 같지 않다.

library("car")

levene_test<- leveneTest(HE_CHOL~ as.factor(income), data=Data)
print(levene_test)


## 3. ANOVA
# ANOVA 시행 이전 변수 전처리: 범주형 변수로의 변경
# 원래 '개인 소득 수준(income)' 변수는 정수형

Data$income <- as.factor(Data$income)
str(Data)

# HO: 소득수준에 따른 네 집단 사이의 총 콜레스테롤 수치의 모평균은 모두 동일하다.
# H1: 소득수준에 따른 네 집단 사이의 총 콜레스테롤 수치의 모평균이 모두 동일한 것은 아니다.

anova <- aov(HE_CHOL~income, data=Data)
summary(anova)

# 4. 사후 검정

# (1) Bonferroni T test
pairwise.t.test(Data$HE_CHOL, Data$income, p.adj="bonferroni")

# (2) Scheffe's multiple comparison-1
install.packages("DescTools")
library("DescTools")
ScheffeTest(anova)

# (2) Scheffe's multiple comparison-2
install.packages("agricolae")
library(agricolae)
scheffe.test(anova, "income", console=TRUE)

# (3) Tukey test: 그룹별로 표본 수가 같을 때만 사용
anova1 <- aov(HE_CHOL~factor(income), data=Data)
summary(anova1)
library(agricolae)
TukeyHSD(anova1, conf.level=0.95)

# (4) Duncan's multiple range test: 그룹별로 표본 수가 같을 때만 사용
library(agricolae)
duncan.test(anova1, "factor(income)", console=TRUE)

#---------------------------------------------#

############### 문제 3 ###############

# 소득수준에 따라 총 콜레스테롤 수치에 차이가 있는가: 비모수적 방법
# 1. 실습할 예제 데이터 불러오기
# 2. Krusal-Wallis Test

# HO: 소득수준에 따른 네 집단 사이의 총 콜레스테롤 수치의 중앙값은 모두 동일하다.
# H1: 소득수준에 따른 네 집단 사이의 총 콜레스테롤 수치의 중앙값이 모두 동일한 것은 아니다.

# 변수 income(정수형), income2(범주형) 모두 사용 가능

kruskal.test(HE_CHOL~income, data=Data)

# 3. 사후 검정 (1) Pairwise Wilcoxon Test
pairwise.wilcox.test(Data$HE_CHOL, Data$income, p.adjust.method="bonferroni", correct=FALSE)

# 3. 사후 검정 (2) Dunn's Test
library("DescTools")
DunnTest(HE_CHOL~income, data=Data)

#---------------------------------------------#

#끝#


