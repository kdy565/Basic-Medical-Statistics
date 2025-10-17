# 2025.10.17. 기초의학통계학 6주차 실습: 상관 및 회귀분석 2

# 0. 현재 환경에 있는 모든 객체(데이터, 변수 등) 삭제; 원하는 경우 수행
rm(list = ls())


# 2. 패키지 설치 (최초 1회만 실행하면 됨)
install.packages("readr")   # CSV 파일 읽기용 패키지
install.packages("car")     # 회귀진단, 공선성 검사 등

# 3. 패키지 불러오기
library(readr)
library(car)

# 4. 데이터 불러오기
data2 <- read_csv("data66.csv")

# 5. 데이터 확인 (옵션)
head(data2)      # 데이터 앞부분(기본 6행)을 보여주는 함수
str(data2)       # 데이터의 구조를 요약해서 보여주는 함수


########################### 문제 1 ###############################

# 문제 1-1 단순회귀모형 (linear regression) 적합하기
LR2 <- lm(HE_WC ~ age, data = data2)  # y = HE_WC, x = age
summary(LR2)

# 문제 1-3 잔차분석

# 1) 독립성 (Durbin-Watson test)   #car 패키지 필요
durbinWatsonTest(LR2$residuals)

# 2) 잔차 정규성 가정, 확인 (Q-Q plot 또는 Shapiro-Wilk test)
plot(LR2, 2)
shapiro.test(LR2$residuals)

# 3) 등분산성 (Homogeneity of variance)
plot(LR2, 3)

# 4) 선형성 확인
plot(LR2, 1)

####cf. 잔차분석 한번에 보기
par(mfrow = c(2, 2))
plot(LR2)


########################### 문제 2 ###############################

# 문제 2-1 
LR3<-lm(HE_WC~age + HE_BMI, data = data2)

durbinWatsonTest(LR3$residuals)

par(mfrow = c(2, 2))
plot(LR3)

vif(LR3)

summary(LR3)


# 문제 2-2

# 1) ALCOHL_d를를 범주형(factor)으로 변환
data2$ALCOHL_d <- as.factor(data2$ALCOHL_d)
str(data2$ALCOHL_d)

# 2) 주어진 가변수 ALCOHL_d 사용, 중회귀모형 적합
LR4 <- lm(HE_WC ~ age + ALCOHL_d, data = data2)

durbinWatsonTest(LR4$residuals)

par(mfrow = c(2, 2))
plot(LR4)

vif(LR4)

summary(LR4)


# 문제 2-3
LR5 <- lm(HE_WC ~ age + ALCOHL_d + age * ALCOHL_d, data = data2)

durbinWatsonTest(LR5$residuals)

par(mfrow = c(2, 2))
plot(LR5)

vif(LR5)

summary(LR5)


# 문제 2-4
# 종속변수 = HE_glu
# 독립변수 = HE_WC, HE_BMI

# 다중회귀: 혈당(HE_glu) ~ 허리둘레 + BMI
LR6 <- lm(HE_glu ~ HE_WC + HE_BMI, data = data2)
summary(LR6)

# 산점도 그리기 scatter plot)
plot(x=data2$HE_WC, y=data2$HE_BMI)

# Shapiro-Wilk test로 정규성검정 
shapiro.test(data2$HE_WC)
shapiro.test(data2$HE_BMI)

# (shapiro test로 정규성검정 시행시 정규성 만족하지 않았음) Kendall tau correlation 시행
cor.test(data2$HE_WC, data2$HE_BMI, method = "kendall")

# 다중회귀: 혈당(HE_glu) ~ 허리둘레 
LR7 <- lm(HE_glu ~ HE_WC, data = data2)
summary(LR7)

# 다중회귀: 혈당(HE_glu) ~ BMI
LR8 <- lm(HE_glu ~ HE_BMI, data = data2)
summary(LR8)



