Data <- read.csv( file = "Data.csv", header = TRUE)
head(Data)
View(Data)


# 분석 변수 확인
# HE_CHOL: 총 콜레스테롤, BMI_C: 1=저체중, 2=정상, 3=과체중, 4=비만 (가정)

# 2. BMI_C별 데이터 나누기 
bmi_1 <- subset(Data, BMI_C==1)
bmi_2 <- subset(Data, BMI_C==2)
bmi_3 <- subset(Data, BMI_C==3)
bmi_4 <- subset(Data, BMI_C==4)

# 3. 정규성 확인: (1) Q-Q plot, (2) Histogram+정규밀도, (3) 정규성 검정
# (1) Q-Q plot
par(mfrow=c(2,2))
qqnorm(bmi_1$HE_CHOL, main="BMI_C=1"); qqline(bmi_1$HE_CHOL, col="red")
qqnorm(bmi_2$HE_CHOL, main="BMI_C=2");   qqline(bmi_2$HE_CHOL, col="blue")
qqnorm(bmi_3$HE_CHOL, main="BMI_C=3"); qqline(bmi_3$HE_CHOL, col="yellow")
qqnorm(bmi_4$HE_CHOL, main="BMI_C=4");   qqline(bmi_4$HE_CHOL, col="orange")

# (2) Histogram + 정규분포곡선
# (2) Histogram + 정규분포곡선  [빈도 기준으로 스케일 맞춤]
par(mfrow=c(2,2))

## BMI_C = 1
vals1 <- bmi_1$HE_CHOL
h1 <- hist(vals1, main="BMI_C=1", xlab="HE_CHOL", freq=TRUE)  # freq=TRUE: 빈도
x1 <- seq(min(vals1, na.rm=TRUE), max(vals1, na.rm=TRUE), length=200)
sd1 <- sd(vals1, na.rm=TRUE)
if (is.finite(sd1) && sd1 > 0) {
  binw1 <- mean(diff(h1$breaks), na.rm=TRUE)           
  y1 <- dnorm(x1, mean=mean(vals1, na.rm=TRUE), sd=sd1) *
    sum(!is.na(vals1)) * binw1                           
  lines(x1, y1, lwd=2, col="green")
}

## BMI_C = 2
vals2 <- bmi_2$HE_CHOL
h2 <- hist(vals2, main="BMI_C=2", xlab="HE_CHOL", freq=TRUE)
x2 <- seq(min(vals2, na.rm=TRUE), max(vals2, na.rm=TRUE), length=200)
sd2 <- sd(vals2, na.rm=TRUE)
if (is.finite(sd2) && sd2 > 0) {
  binw2 <- mean(diff(h2$breaks), na.rm=TRUE)
  y2 <- dnorm(x2, mean=mean(vals2, na.rm=TRUE), sd=sd2) *
    sum(!is.na(vals2)) * binw2
  lines(x2, y2, lwd=2, col="green")
}

## BMI_C = 3
vals3 <- bmi_3$HE_CHOL
h3 <- hist(vals3, main="BMI_C=3", xlab="HE_CHOL", freq=TRUE)
x3 <- seq(min(vals3, na.rm=TRUE), max(vals3, na.rm=TRUE), length=200)
sd3 <- sd(vals3, na.rm=TRUE)
if (is.finite(sd3) && sd3 > 0) {
  binw3 <- mean(diff(h3$breaks), na.rm=TRUE)
  y3 <- dnorm(x3, mean=mean(vals3, na.rm=TRUE), sd=sd3) *
    sum(!is.na(vals3)) * binw3
  lines(x3, y3, lwd=2, col="green")
}

## BMI_C = 4
vals4 <- bmi_4$HE_CHOL
h4 <- hist(vals4, main="BMI_C=4", xlab="HE_CHOL", freq=TRUE)
x4 <- seq(min(vals4, na.rm=TRUE), max(vals4, na.rm=TRUE), length=200)
sd4 <- sd(vals4, na.rm=TRUE)
if (is.finite(sd4) && sd4 > 0) {
  binw4 <- mean(diff(h4$breaks), na.rm=TRUE)
  y4 <- dnorm(x4, mean=mean(vals4, na.rm=TRUE), sd=sd4) *
    sum(!is.na(vals4)) * binw4
  lines(x4, y4, lwd=2, col="green")
}

# (3-1) Shapiro-Wilk test
# H0: 해당 군은 정규분포를 따른다 / H1: 따르지 않는다
shapiro.test(bmi_1$HE_CHOL) 
shapiro.test(bmi_2$HE_CHOL)
shapiro.test(bmi_3$HE_CHOL)
shapiro.test(bmi_4$HE_CHOL)

# (3-2) Lilliefors test (수정된 Kolmogorov-Smirnov test)
library("nortest")
lillie.test(bmi_1$HE_CHOL) 
lillie.test(bmi_2$HE_CHOL)
lillie.test(bmi_3$HE_CHOL)
lillie.test(bmi_4$HE_CHOL)

# 4. 군별 요약 통계량 (소수 둘째 자리 반올림)
#    정규성이 기각되지 않으면 평균±표준편차 중심으로, 기각되면 중앙값·IQR도 함께 보고
length(bmi_1$HE_CHOL)
mean(bmi_1$HE_CHOL)
sd(bmi_1$HE_CHOL)

length(bmi_2$HE_CHOL)
mean(bmi_2$HE_CHOL)
sd(bmi_2$HE_CHOL)

length(bmi_3$HE_CHOL)
mean(bmi_3$HE_CHOL)
sd(bmi_3$HE_CHOL)

length(bmi_4$HE_CHOL)
mean(bmi_4$HE_CHOL)
sd(bmi_4$HE_CHOL)

#---------------------------------------------#
#끝#


############### 과제 2 ###############

# 분석 변수
# HE_CHOL: 총 콜레스테롤
# BMI_C: 1=저체중, 2=정상, 3=과체중, 4=비만 (가정)
Data2 <- subset(Data, !is.na(HE_CHOL) & !is.na(BMI_C))
Data2$BMI_C <- factor(Data2$BMI_C, levels=c(1,2,3,4),labels=c("저체중","정상","과체중","비만"))

alpha <- 0.05

#---------------------------------------------#
# 1-1 등분산 검정 (Levene)
#   H0: 각 군의 분산은 서로 같다 (등분산)
#   H1: 적어도 한 군의 분산이 다르다 (비등분산)
library(car)
lev <- car::leveneTest(HE_CHOL ~ BMI_C, data=Data2)  # 기본: median 기준
lev_p <- as.data.frame(lev)$`Pr(>F)`[1]

cat("\n[1-1 Levene 등분산 검정]\n")
print(lev)
cat(sprintf("Levene p-value = %.5f\n", lev_p))
cat("등분산성 가정 만족 여부: ", ifelse(lev_p >= alpha, "만족(등분산)", "불만족(비등분산)"), "\n")

#---------------------------------------------#
# 1-2 ANOVA
#   H0: 모든 군의 모집단 평균은 같다
#   H1: 적어도 한 쌍의 군 평균이 다르다
# 등분산 가정 충족 → 일원분산분석(aov)
fit_aov <- aov(HE_CHOL ~ BMI_C, data=Data2)
anova_tab <- summary(fit_aov)
anova_p <- anova_tab[[1]][["Pr(>F)"]][1]

print(anova_tab)
cat(sprintf("ANOVA p-value = %.5f\n", anova_p))
cat("귀무가설 기각 여부: ", ifelse(anova_p < alpha, "기각 (군 평균 차이 존재)", "채택 (차이 근거 부족)"), "\n")
need_posthoc <- (anova_p < alpha)
anova_type <- "고전 ANOVA"

#---------------------------------------------#
# 1-3 사후 검정
# 필요없음
#---------------------------------------------#
#끝#