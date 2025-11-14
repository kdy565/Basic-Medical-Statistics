#############2023 기초의학통계학 및 실험#############

#############생존분석(1)#############


install.packages("readr") #기존에 설치되어있으면 생략
library(readr)

# 프로젝트 폴더에 있는 파일 불러오기
 # read_csv 함수는 '파일명.확장자'까지 기입
filename <- "sample_1500.csv"
data <- read_csv(filename) 

# 데이터 파악
str(data)

## 변수 설명
 # dth: 사망 여부 (0: 생존 / 1: 사망)  -> 생존(dth == 0)이 censor
 # dth_fu3: 사망 추적관찰 기간 (month)


#####문제 1#####
###생명표법을 이용하여 1년 생존율(Survival probability) 구하기

## 1) 결과변수: 절단 정보와 생존시간 결합
install.packages("survival")
library(survival)

 # survival::Surv()함수: censored data에 + 붙여줌
  # Surv(time, event)
data$TS <- Surv(data$dth_fu3, data$dth == 1)

  # 확인하기
head(data$dth)
head(data$dth_fu3)
head(data$TS)


## 2) 생명표 그리기
install.packages("biostat3") 
library(biostat3)

 # biostat3::lifetab2() 함수: 생명표 그리기
  # lifetab2(절단 정보가 결합된 생존기간 ~ 1, 자료, 구간)
lifetable <- lifetab2(Surv(dth_fu3, dth == 1) ~ 1, data =  data, breaks = seq(0, 60, 2)) 
lifetable <- lifetab2(data$TS ~ 1, data = data, breaks = seq(0, 60, 2)) #위와 동일

## 3) 3년 생존율 구하기
 # tstop == 36인 생존율 확인
lifetable
     
  ## 3년 생존율: 0.876



###문제 2-1###
###Kaplan-Meier법을 이용하여 3년 생존율(Survival probability) 구하기

## 1) 생존함수 추정
 # survival::survfit() 함수: K-M 방법으로 추정된 생존함수
 # survfit(절단 정보가 결합된 생존기간 ~ 1, 자료)
fit <- survfit(TS ~ 1, data = data) ; fit
fit <- survfit(Surv(dth_fu3, dth) ~ 1, data = data) ; fit #위와 동일


## 2) 1년 생존율 구하기
 # summary 결과에서 35.4개월(36개월 직전)의 생존율이 3년 생존율
summary(fit)

  ## 1년 생존율: 0.878



###문제 2-2###
###Kaplan-Meier법을 이용하여 중앙생존기간(Median survival) 구하기

## 3) 중앙생존기간 구하기
 # median 확인
fit <- survfit(Surv(dth_fu3, dth) ~ 1, data = data) ; fit 

  ## 중앙생존기간: 58.5개월


## 4) 생존곡선 그리기
plot(fit, xlab = "time", ylab = "Survival probability")
abline(h=0.5, col ="red") # y=0.5 수평선 추가

 # 참고: 신뢰구간 없이 그리기
plot(fit, xlab = "time", ylab = "Survival probability", conf.int = FALSE)

 # 생존곡선 그리기2
install.packages("survminer")
library(survminer)

 # survminer::ggsurvplot() 함수: plot() 함수보다 보기 좋게 그려줌
 # ggurvplot(생존함수, 자료)
survminer::ggsurvplot(fit, data = data)
 # 참고: 신뢰구간 없이 그리기
survminer::ggsurvplot(fit, data = data, conf.int = FALSE)



###문제 3###
###흡연 여부에 따라 생존율에 유의미한 차이가 있는지 검정

## 변수 설명
 # dth: 사망 여부 (0: 생존 / 1: 사망) 
 # dth_fu: 사망 추적관찰 기간 (month)
 # smok: 흡연 여부 (0: 비흡연/ 1: 흡연)

## 1) 생존함수 추정
 # survival::survfit(Surv(time, event) ~ x, data)
fit_smok <- survfit(Surv(dth_fu3, dth == 1) ~ smok, data = data) ; fit_smok
summary(fit_smok)


## 2) Log-rank test
 # survival::survdiff() 함수: Log-Rank test
 # survdiff(Surv(time, evnet) ~ x, data)
out <- survdiff(Surv(dth_fu3, dth == 1) ~ smok, data = data)

# p-value 확인
out 

 ## p-value가 0.1로 귀무가설을 기각할 수 없다.


## 3) 생존곡선 그리기
ggsurvplot(fit_smok, data = data, pval = TRUE, legend=c(0.9,0.85))
 # 참고: 신뢰구간 추가
ggsurvplot(fit_smok, data = data, pval = TRUE, conf.int = TRUE) 

 ## p-value = 0.14로 유의하지 않아 귀무가설을 기각할 수 없다.


## 참고: plot() 함수로 두 군의 생존곡선 그리고 구분하기
 # red: 비흡연(smok==0), blue: 흡연(smork==1)
 # 작은 숫자 먼저 지정됨
plot(fit_smok, lty = 1:2, col = c("red", "blue"))
 # legend() 함수: plot에 legend 추가
legend("topright", c("비흡연", "흡연"), lty = 1:2, col = c("red", "blue")) 

## 4) 여러 범주의 생존곡선 
# incm: 소득 수준 (1:하 / 2:중하 / 3:중상 / 4:상)
fit_incm <- survfit(Surv(dth_fu3, dth == 1) ~ incm, data = data) ; fit_incm

# 생존곡선 그리기
ggsurvplot(fit_incm, data = data, pval = TRUE, legend=c(0.9,0.9))

