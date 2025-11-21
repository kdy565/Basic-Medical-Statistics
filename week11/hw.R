install.packages("survival") # 기존에 설치되어있으면 생략
install.packages("survminer") # 기존에 설치되어있으면 생략
install.packages("autoReg") # 기존에 설치되어있으면 생략

library(survival)
library(survminer)
library(autoReg)
library(readr)


filename <- "sample_1500.csv"
data <- read_csv(filename) 



data$TS <- Surv(data$colorectum_cancer_fu, data$colorectum_cancer == 1)

# 자료구조 먼저 확인하기
head(data$colorectum_cancer_fu)
head(data$colorectum_cancer)
head(data$TS)

## 1) 범주형 변수에 대해 factor 지정
# 비흡연(0)에 비한 흡연(1)
data$smok <- factor(data$smok, levels = c(0, 1))
str(data$smok) # level 확인
# 여성(2)에 비한 남성(1)
data$sex <- factor(data$sex, levels = c(2, 1))
str(data$sex) # level 확인

data$ALCOHL <- factor(data$ALCOHL, levels = c(0, 1))
str(data$sex) # level 확인

## 자료구조 먼저 확인하기 (상세)
# 발생 건수 확인
table(data$colorectum_cancer)
# 추적시간의 분포 확인
summary(data$colorectum_cancer_fu)
# 흡연변수 확인 (범주형)
table(data$smok)
prop.table(table(data$smok)) # 비율 확인
# 성별변수 확인 (범주형)
table(data$sex)
prop.table(table(data$sex)) # 비율 확인
# 나이변수 확인 (연속형)
summary(data$age)
hist(data$age)

# 과제 1: Cox 비례위험모형 적합 (단변량)
# Surv(시간, 사건) ~ 독립변수
fit_hw1 <- coxph(Surv(colorectum_cancer_fu, colorectum_cancer == 1) ~ smok, data = data)

# 결과 확인
summary(fit_hw1)
# 또는
autoReg::gaze(fit_hw1)


# 과제 2: 다변량 Cox 비례위험모형 적합
# 성별(sex), 음주력(ALCOHL)을 범주형(factor)으로 지정
# 기준 범주(Reference) 확인: sex=2(여성) 기준이라면 relevel 필요할 수 있음 (기본은 숫자 순서)



# 모형 적합
fit_hw2 <- coxph(Surv(colorectum_cancer_fu, colorectum_cancer == 1) ~ smok + age + factor(sex) + factor(ALCOHL) + BMI, data = data)

# 결과 확인
summary(fit_hw2)
# 또는
autoReg::gaze(fit_hw2)



# autoReg 패키지의 loglogplot 사용
autoReg::loglogplot(fit_hw1)

# survival 패키지의 cox.zph 사용
cox.zph(fit_hw1)
