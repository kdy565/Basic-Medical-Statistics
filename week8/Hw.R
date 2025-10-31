# 8주차 실습 과제 코드

#과제 2
install.packages("gmodels")
install.packages("readr")
library(gmodels)
library(readr)

# 데이터 불러오기
data1 <- read_csv("HTN.csv")

# 변수 확인
str(data1$ALCOHL)
str(data1$HTN)

# 결측치
sum(is.na(data1$ALCOHL))
sum(is.na(data1$HTN))
data1_1 <- subset(data1, !is.na(ALCOHL) & !is.na(HTN))

# 기대빈도있는 교차표
CrossTable(data1_1$ALCOHL, data1_1$HTN, prop.chisq = FALSE, expected = TRUE)

# 카이제곱 검정
chisq_result <- chisq.test(data1_1$ALCOHL, data1_1$HTN, correct = FALSE)
chisq_result


#과제3

# 데이터 불러오기
data2 <- read_csv("sample_1500.csv")

# 변수 확인
str(data2$smok)
str(data2$lung_cancer)

# 결측치
sum(is.na(data2$smok))
sum(is.na(data2$lung_cancer))
data2_1 <- subset(data2, !is.na(smok) & !is.na(lung_cancer))

# 기대빈도있는 교차표
CrossTable(data2_1$smok, data2_1$lung_cancer, prop.chisq = FALSE, expected = TRUE)


# Fisher 검정 (기대빈도 5 미만 셀 존재 시)
fisher_result <- fisher.test(data2_1$smok, data2_1$lung_cancer)
fisher_result