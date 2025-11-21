#############2023 기초의학통계학 및 실험#############

#############생존분석(2)#############

# 자동으로 현재 스크립트 파일이 있는 폴더로 working directory 지정
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# working directory 지정하기
# 각자의 폴더 경로로 수정하기
mydir <- "D:/Onedrive/2025-2/★기초의학통계/2025-2기의통/R"
setwd(mydir)

## 라이브러리 불러오기
# 필요한 패키지 불러오기
install.packages("survival") # 기존에 설치되어있으면 생략
install.packages("survminer") # 기존에 설치되어있으면 생략
install.packages("autoReg") # 기존에 설치되어있으면 생략

library(survival)
library(survminer)
library(autoReg)
library(readr)

# 프로젝트 폴더에 있는 파일 불러오기
# read_csv 함수는 '파일명.확장자'까지 기입
filename <- "sample_1500.csv"
data <- read_csv(filename) 

# 프로젝트 폴더가 아닌 다른 폴더에 있는 파일 불러오기
# 파일 경로 지정(/사용)   
# 각자의 폴더 경로로 수정하기
filepath <- file.path(mydir, filename)
data <- read_csv(filepath)

# 데이터 파악
str(data)

#####문제 1#####
###Cox 비례위험모형을 이용하여 흡연 여부에 따른 폐암 발생의 Hazard ratio(HR) 구하기

## 변수 설명
 # lung_cancer: 폐암 발생 여부 (0: 미발생 / 1: 발생)
 # lung_cancer_fu: 폐암 추적관찰 기간 (month)
 # smok: 흡연 여부 (0: 비흡연 / 1: 흡연)

## 1) 결과변수: 절단 정보와 생존시간 결합
# survival::Surv()함수: censored data에 + 붙여줌
# Surv(time, event)
data$TS <- Surv(data$lung_cancer_fu, data$lung_cancer == 1)

# 자료구조 먼저 확인하기
head(data$lung_cancer)
head(data$lung_cancer_fu)
head(data$TS)

## 자료구조 먼저 확인하기 (상세)
# 발생 건수 확인
table(data$lung_cancer)
# 추적시간의 분포 확인
summary(data$lung_cancer_fu)
# 
table(data$smok)
prop.table(table(data$smok)) # 비율 확인

#카플란-마이어 생존곡선 확인
ggsurvplot(survfit(data$TS ~ 1), data=data)
ggsurvplot(survfit(data$TS ~ data$smok), data=data)
           

## 2) Cox 비례위험모형 적합
# survival::coxph() 함수: Cox 비례위험모형 적합
# coxph(Surv(time, event) ~ x, 자료)
fit1 <- coxph(Surv(lung_cancer_fu, lung_cancer == 1) ~ smok, data = data) ; fit1

# 위와 동일한 표현
fit1 <- coxph(TS ~ smok, data = data) ; fit1


## 3) Hazard ratio(HR) 구하기
# summary 결과에서 흡연 여부에 따른 HR 확인
summary(fit1)

## exp(coef)이 HR로, HR = 1.23
## 즉, 비흐연자에 비해 흡연자의 폐암 발생 위험은 약 5.872배 높다
## 참고
# autoReg::gaze()함수: 정리된 table로 HR 보여줌
autoReg::gaze(fit1)



#####문제 2#####
###흡연 여부에 따른 폐암 발생의 Hazard ratio(HR)를 계산하려고 한다.
###나이 및 성별을 공변량으로 고려하는 Cox 비례위험모형을 이용하여 계산하시오.

## 변수 설명
 # lung_cancer: 폐암 발생 여부 (0: 미발생 / 1: 발생)
 # lung_cancer_fu: 폐암 추적관찰 기간 (month)
 # smok: 흡연 여부 (0: 비흡연 / 1: 흡연)
 # age: 나이 (세세)
 # sex: 성별 (0: 남 / 1: 여)

## 1) 범주형 변수에 대해 factor 지정
 # 비흡연(0)에 비한 흡연(1)
data$smok <- factor(data$smok, levels = c(0, 1))
str(data$smok) # level 확인
 # 여성(2)에 비한 남성(1)
data$sex <- factor(data$sex, levels = c(2, 1))
str(data$sex) # level 확인

## 자료구조 먼저 확인하기 (상세)
# 발생 건수 확인
table(data$lung_cancer)
# 추적시간의 분포 확인
summary(data$lung_cancer_fu)
# 흡연변수 확인 (범주형)
table(data$smok)
prop.table(table(data$smok)) # 비율 확인
# 성별변수 확인 (범주형)
table(data$sex)
prop.table(table(data$sex)) # 비율 확인
# 나이변수 확인 (연속형)
summary(data$age)
hist(data$age)

#카플란-마이어 생존곡선 확인
ggsurvplot(survfit(data$TS ~ 1), data=data)
ggsurvplot(survfit(data$TS ~ data$smok), data=data)

## 2) 공변량을 포함한 Cox 비례위험모형 적합
 # coxph(절단 정보가 결합된 생존기간 ~  covariates, 자료)
fit2 <- coxph(Surv(lung_cancer_fu, lung_cancer == 1) ~ smok + age + sex, data = data)
fit2 <- coxph(TS ~ smok + age + sex, data = data) # 위와 동일

## 3) Hazard ratio(HR) 구하기
summary(fit2)

## 참고
autoReg::gaze(fit2)
## 나이와 성별을 보정한 경우, 흡연 여부에 따른 폐암 발생의 HR은 5.917이며 통계적으로 유의함
## 나이는 통계적으로 유의하지 않음. 즉 위험률에 통계적으로 유의한 영향을 미친다고 할 수 없음
## 성별은 통계적으로 유의하며, 여성에 비해 남성이 폐암에 걸릴 HR은 3.647


#####문제 3#####
###문제 1에 사용한 모형이 proportional hazards assumption을 만족하는지
###log-log plot과 goodness of fit test를 이용하여 검정하시오.

## 1) Cox 비례위험모형 적합
fit1 <- coxph(Surv(lung_cancer_fu, lung_cancer == 1) ~ smok, data = data) ; fit1
fit1 <- coxph(TS ~ smok, data = data) ; fit1

## 2) log-log plot
#autoReg::loglogplot()함수: 로그-로그생존곡선 그리기
loglogplot(fit1)


## 두 그래프가 평행하므로 비례위험 가정만족

## 참고 1
# 심화 - 범주 3개 이상의 LML plot
# incm: 소득수준 (1: 하 / 2: 중하 / 3: 중상 / 4: 상)
data$incm <- factor(data$incm, levels=c(1,2,3,4))
str(data$incm)

fit3 <- coxph(Surv(lung_cancer_fu,lung_cancer==1) ~ incm, data=data)
autoReg::loglogplot(fit3)


## 3) Goodness of fit test
# survival::cox.zph() 함수: 비례위험 가정 평가
# cox.zph(coxph())
# 귀무가설: 비례위험 가정 만족
# 대립가설: 비례위험 가정 불만족
cox.zph(fit1)
## p-value가 0.76으로 귀무가설을 기각하지 못한다. 즉, 비례위험 가정을 만족한다.
## 시각적 평가와 통계적 평가를 종합적으로 고려

## 참고 2
# plot()함수로 로그-로그생존곡선 그리기
# survival::survfit() 함수로 생존함수 적합 후 plot() 함수에 대입
# plot(생존함수, fun="cloglog")
sfit <- survfit(Surv(lung_cancer_fu, lung_cancer == 1) ~ smok, data = data)
plot(sfit, fun="cloglog", col=c("red", "blue"), xlab="log(time)", ylab="log(-log(S(t)))", main="log-log plot")
legend("topleft", legend=c("비흡연", "흡연"), lty=1, col=c("red", "blue"))

## 참고 3
# survminer::ggsurvplot() 함수로 로그-로그생존곡선 그리기
# ggsurvplot(생존함수, fun = "cloglog", 자료)
sfit <- survfit(Surv(lung_cancer_fu, lung_cancer == 1) ~ smok, data = data)
ggsurvplot(sfit, fun = "cloglog", data = data)
