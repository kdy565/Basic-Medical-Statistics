# =========================
# 기의통 6주차 과제 1, 2 R 코드
# 데이터: sample_1500_3.csv
# =========================

# 0) 패키지 로드 ------------------------------------------------------------
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(lmtest)   # Durbin-Watson
  library(car)      # VIF, outlierTest
})

# 1) 데이터 불러오기 --------------------------------------------------------
df <- read_csv("sample_1500_3.csv")

# ------------------------------------------------------------------------------
# 과제 1 (1.1)
# 영양제투여 후 몸무게(Wt2)로 허리둘레(HE_WC)를 예측하는 단순선형회귀
# ------------------------------------------------------------------------------

# 단순회귀 적합
m1 <- lm(HE_WC ~ Wt2, data = df)

# 결과 요약
summary(m1)


# ------------------------------------------------------------------------------
# 과제 2 (2.1)
# 대상 모형: 과제 1-1에서 적합한 m1을 기준으로 수행
# ------------------------------------------------------------------------------

m2 <- lm(TOT_CHOLE ~ BP_HIGH + BP_LWST, data = df)

# 1) 독립성 (Durbin-Watson test)   #car 패키지 필요
durbinWatsonTest(m2$residuals)

# 2) 잔차 정규성 가정, 확인 (Q-Q plot 또는 Shapiro-Wilk test)
plot(m2, 2)
shapiro.test(m2$residuals)

# 3) 등분산성 (Homogeneity of variance)
plot(m2, 3)

# 4) 선형성 확인
plot(m2, 1)

####cf. 잔차분석 한번에 보기
par(mfrow = c(2, 2))
plot(m2)

#2.2

summary(m2)
