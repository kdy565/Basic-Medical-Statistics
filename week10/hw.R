install.packages("readr") #기존에 설치되어있으면 생략
library(readr)
install.packages("survival")
library(survival)

# 프로젝트 폴더에 있는 파일 불러오기
# read_csv 함수는 '파일명.확장자'까지 기입
filename <- "sample_1500.csv"
data <- read_csv(filename) 

# 데이터 파악
str(data)


########################################
### 과제 1
### HTHX(고혈압 과거력)에 따른 2년 생존율 / 중앙생존기간 / 생존곡선
########################################

## 변수 설명
# dth     : 사망 여부 (0: 생존 / 1: 사망)
# dth_fu3 : 사망 추적관찰 기간 (month)
# HTHX    : 고혈압 과거력 (0: 없음 / 1: 있음)

## 1) 생존함수 추정 (HTHX 별)
#  Surv(time, event) ~ HTHX
fit_HTHX <- survfit(Surv(dth_fu3, dth == 1) ~ HTHX, data = data) ; fit_HTHX
summary(fit_HTHX)


## 2) 2년(24개월) 생존율 구하기
km24 <- summary(fit_HTHX, times = 24)

km24$time      # 24
km24$strata    # "HTHX=0", "HTHX=1"
km24$surv      # 각 군의 2년 생존율 (0~1)

## 2년 생존율:
##  HTHX=0 (고혈압 과거력 없음)  : km24$surv[ km24$strata == "HTHX=0" ]
##  HTHX=1 (고혈압 과거력 있음)  : km24$surv[ km24$strata == "HTHX=1" ]


## 3) 중앙생존기간 구하기
fit_HTHX$table              # 각 군 요약
median_HTHX <- fit_HTHX$table[ , "median"]
median_HTHX

## 중앙생존기간:
##  HTHX=0 군 : median_HTHX["HTHX=0"]
##  HTHX=1 군 : median_HTHX["HTHX=1"]


## 4) Log-rank test (HTHX 간 생존율 차이 검정)
out_HTHX <- survdiff(Surv(dth_fu3, dth == 1) ~ HTHX, data = data)
out_HTHX

p_HTHX <- 1 - pchisq(out_HTHX$chisq, df = length(out_HTHX$n) - 1)
p_HTHX    # 대략 0.044 근처 값이 나와야 함


## 5) 생존곡선 그리기 (스크린샷처럼 ggsurvplot)
#  p-value와 신뢰구간, 중앙생존선(h+v)까지 표시
library(survminer)

ggsurvplot(
  fit_HTHX,
  data       = data,
  pval       = TRUE,                 # 왼쪽 아래 p = 0.044 표시
  conf.int   = TRUE,                 # 반투명 신뢰구간
  xlab       = "Time",
  ylab       = "Survival probability"
)




########################################
### 과제 2
### 성별(sex)에 따른 생존율 차이(Log-rank test) 및 생존곡선
########################################

## 변수 설명
# dth     : 사망 여부 (0: 생존 / 1: 사망)
# dth_fu3 : 사망 추적관찰 기간 (month)
# sex     : 성별 (1: 남자 / 2: 여자)

## 1) 생존함수 추정 (성별)
fit_sex <- survfit(Surv(dth_fu3, dth == 1) ~ sex, data = data) ; fit_sex
summary(fit_sex)


## 2) Log-rank test
out_sex <- survdiff(Surv(dth_fu3, dth == 1) ~ sex, data = data)
out_sex

p_sex <- 1 - pchisq(out_sex$chisq, df = length(out_sex$n) - 1)
p_sex   # 대략 0.04 근처 값이 나와야 함


## 3) 생존곡선 그리기 (스크린샷처럼)
ggsurvplot(
  fit_sex,
  data       = data,
  pval       = TRUE,                 # p = 0.04 표시
  conf.int   = TRUE,                 # 반투명 신뢰구간
  xlab       = "Time",
  ylab       = "Survival probability"
)
