### 통계분석실습 통계학과 기말 프로젝트

## 은행에서 예금을 수신하기 위한 데이터 기반 상품홍보 전략수립 프로젝트 ####


# I. 신원미상 고객 제거 : 결측치 줄이기 또는 결측값 대체
# II. 채무불이행 (yes) 고객 예측 모델 구현 : 예측변수 결정, 혼동행렬(채점표)을 만들 검증용(validation) 데이터 셋을 훈련용(test) 데이터 셋과 분리할 비율 결정 (7:3, 8:2, 9:1)
# III. 은행 수신상품 가입 여부 marketing(try or defer) 예측 모델 구현 : 분류의 문제
# IV. 예측 모델을 근거로 은행의 수익성 평가


# 거절할 것 같은 고객은 보류, 예금하기로 결정할 것 같은 고객에 대해서만 연락)하여 classification을 하는 learner


library(dplyr)
library(forcats)
library(ggplot2)
library(mice)
library(VIM)
library(DMwR)
  # Error : to install.packages "Data Mining with R"
  # Solution : remotes::install_github("cran/DMwR")
library(rsample)
library(caret)
library(vip)
library(car)
library(caTools)
library(rpart)
library(rpart.plot)
library(corrplot)
library(RColorBrewer)
library(rattle)

library(rsample)
library(ipred)
library(randomForest)
library(mlbench)
library(dunn.test)
library(HH)


getwd()
setwd("C:/PDSR/RforStatistics_FinalProject")
  # 파일경로
load("data_train.Rda")

{
Codebook = data.frame(
  features = colnames(data_train), 
  descripts = c(
    "나이", "type of job", "marital status", "교육수준", 
    
    "채무불이행 여부", "주택대출이 있는지 여부", "개인대출이 있는지 여부", 
    
    "telemarketing 수단", "마지막으로 진행한 contact했을 때의 월", "마지막으로 진행한 contact의 요일", "이번 홍보에서 해당 고객을 contact한 횟수", "지난 홍보와 관련하여 해당 고객을 contact하고 지나간 날수", "이번 홍보 이전에 해당 고객을 contact한 횟수", "이전 홍보로 해당 고객을 contact했을 때 마케팅 성공여부",
    
    "고용변동율", "소비자물가지수", "소비자신뢰지수", 
    "has the client subscribed a term deposit? (binary: 'yes','no')")
)}



# 1. 데이터 전처리 ####

# 1.1 결측값 탐색 ####

# 신원 정보
str(data_train)
summary(data_train$age)
ggplot(data_train, aes(age)) + geom_histogram(fill = "lightgreen", col = "black")
boxplot(data_train$age)$stat


table(data_train$job)
ggplot(data_train, aes(x = job, fill = job)) + geom_bar() + scale_fill_brewer(palette = "Set3")
  # unknown (34)
table(data_train$marital)
ggplot(data_train, aes(x = marital, fill = marital)) + geom_bar() + scale_fill_brewer(palette = "Paired")
  # unknown (14)
table (data_train$education)
ggplot(data_train, aes(x = education, fill = education)) + geom_bar() + scale_fill_brewer(palette = "Spectral")
  # unknown (158)


# 신용 정보
table(data_train$default) 
  # 740명 결측 (unknown)
table(data_train$housing) 
  # 96명 결측 (unknown)
table(data_train$loan) 
  # 96명 결측 (unknown)

which(data_train$default == "unknown" 
      & data_train$housing == "unknown" 
      & data_train$loan == "unknown") 
  # 16명 결측 (3개 열(변수)에 공통으로 결측된 행 위치)

which(data_train$housing == "unknown" 
      & data_train$loan == "unknown") 
  # 96명 결측 (2개 열(변수)에 공통으로 결측된 행 위치)


# 은행 마케팅 정보
table(data_train$pdays) 
table(data_train$pdays == 0)
data_train[which(data_train$pdays == 0), ]
  # 3731명 결측 (999) / 2명은 0일 경과 했으며(8월의 수요일) 상품 가입한 고객
table(data_train$poutcome)
table(data_train$poutcome == 0)
table(data_train$previous)
  # 3348명 결측 (nonexistent)

length(which(data_train$pdays == 999
             & data_train$poutcome == "nonexistent"))
length(which(data_train$previous == 0
             & data_train$poutcome == "nonexistent"))
  # 3348명 (2개 열(변수)에 공통으로 결측된 행 위치)
  # 컨택한 적 없어, 마케팅 성패 없고, 마지막 컨택 후 경과일도 없는 고객



# 1.2. 결측값 처리 및 EDA ####

levels(data_train$job)[levels(data_train$job) == "unknown"] <- NA
levels(data_train$marital)[levels(data_train$marital) == "unknown"] <- NA
levels(data_train$education)[levels(data_train$education) == "unknown"] <- NA

levels(data_train$housing)[levels(data_train$housing) == "unknown"] <- NA
levels(data_train$loan)[levels(data_train$loan) == "unknown"] <- NA
table(is.na(data_train$housing))
table(is.na(data_train$loan))

levels(data_train$poutcome)[levels(data_train$poutcome) == "nonexistent"] <- 0
table(data_train$poutcome)
table(data_train$previous)
data_train$pdays <- ifelse(data_train$pdays == 999, NA, data_train$pdays)
table(is.na(data_train$pdays))


# 1.2.1. 여신 서비스 이용 NA ####

length(which(is.na(data_train$loan) & data_train$default == "unknown"))
  # 대출 여부 알 수 없는 96명 중 80명은 채무불이행 없으며, 나머지 16명은 알 수 없음

table(data_train$default)
  # unknown 740 "yes" or "no" 예측
  # 은행의 전통적 수익구조 : 예대마진은 최종 비즈니스 목표
  # 수신상품 (예금) : 현금이 없으면 향후 중간에 해지 : 가입을 할지 여부만큼 중요한 정보
  # 채무불이행 했던 고객, 신용대출과 주택담보대출을 동시에 받아 유동성이 낮은 고객은 제거 여지 有

table(data_train$housing == "yes" & data_train$loan == "yes")
  # 344명은 주택담보대출과 신용대출을 동시에 받은 고객
table(data_train$housing == "yes" & data_train$loan == "yes" & data_train$default == "no")
  # 278명은 대출 이력 있으나 채무불이행 이력 없는 고객

# ** 278명 중 과거 컨택 안했으나 자발적으로 상품 가입한 고객 49명 탐색 **
data_yesdebt <- data_train[which(data_train$housing == "yes" & data_train$loan == "yes" & data_train$default == "no"), ]
data_yesdebt %>% filter(y == "yes") %>% group_by(pdays, previous, poutcome) %>% summarise(count = n())

data_yesdebt %>% filter(y == "yes" & poutcome == 0) %>% group_by(campaign) %>% summarise(count = n())
  # 해당 49명에 대한 이번 상품 마케팅 횟수
print(data_yesdebt %>% filter(y == "yes" & poutcome == 0) %>% group_by(campaign, marital, job) %>% summarise(count = n()), n = 29)
print(data_yesdebt %>% filter(y == "yes" & poutcome == 0) %>% group_by(marital, job) %>% summarise(count = n()), n = 29)
  # (married, admin) (married, blue-collar), (married, retired), (married, technician)
  # (single, admin)



# ** 채무불이행 여부 알 수 없는 고객 특성 탐색(대출 이력 있는 66명 대상) **
table(data_train$housing == "yes" & data_train$loan == "yes" & data_train$default == "unknown")
data_nulldebt <- data_train[which(is.na(data_train$housing) & is.na(data_train$loan)), ]

data_nulldebt %>% filter(y == "yes") %>% group_by(previous, poutcome) %>% summarise(count = n())
  # 상품 가입 고객 중 마케팅 안한 고객 비중 높음 (14/19)
data_nulldebt %>% filter(y == "yes") %>% group_by(marital) %>% summarise(count = n())
  # married 비중 높음
data_nulldebt %>% filter(y == "yes") %>% group_by(job) %>% summarise(count = n())
  # admin, blue-collar, technician

data_nulldebt %>% filter(y == "no") %>% group_by(marital) %>% summarise(count = n())
  # married, single 비중 높음
data_nulldebt %>% filter(y == "no") %>% group_by(job) %>% summarise(count = n())
  # blue-collar, admin, technician


# 1.2.2. 이전 은행수신상품(정기예금) 마케팅 NA #### 

length(which(data_train$previous == 0 & data_train$poutcome == 0))
  # 과거 컨택한 적 없어 마케팅 성패 없는 고객 3348명
length(which(is.na(data_train$pdays) & data_train$poutcome != 0 & data_train$previous != 0))
  # 과거 컨택하여 마케팅 성패는 있지만 마지막 컨택 후 경과일은 없는 고객 383명 탐색

data_pcontact <- data_train[which(is.na(data_train$pdays) & data_train$poutcome != 0 & data_train$previous != 0), ]
table(data_pcontact$poutcome == "failure" & data_pcontact$y == "no")
table(data_pcontact$poutcome == "failure" & data_pcontact$y == "yes")
  # 고객 383명 모두 과거 마케팅에 실패하였으나, 정기예금 가입한 고객은 113명
  # ** 113명은 컨택/마케팅 없이 자발적으로 정기예금에 가입한 고객(이하 '자기주도형') **

data_pcontact %>% filter(y == "yes") %>% group_by(default, housing) %>% summarise(nhousing = n())
data_pcontact %>% filter(y == "yes") %>% group_by(default, loan) %>% summarise(nloan = n())
  # ** 자기주도형, 연체 없는 고객 특성 : housing은 42:62, loan은 97:7 (no:yes) **


# 1.3. 데이터 시각화 및 결측값 처리(제거 또는 대체) ####
  
# 제거 가능한 신원미상 고객 탐색

mice_plot <- aggr(data_train, col = c("grey", "blue"), numbers = TRUE, sortVars = TRUE, labels = names(data_train), cex.axis = 0.7, gap = 3, ylab = c("결측치", "패턴"))

data_train[which(is.na(data_train$job) & is.na(data_train$marital)), ]
  # job & marital 둘다 결측 

table(is.na(data_train$job))
table(is.na(data_train$marital))
table(data_train$education)
data_train[which(is.na(data_train$job) & is.na(data_train$marital)), ]
  # job & education 둘다 결측
data_train[which(is.na(data_train$job) & is.na(data_train$marital)), ] %>% filter(y == "no")

data_train[which(is.na(data_train$marital) & is.na(data_train$education)), ]
  # marital & education 둘다 결측 
data_train[which(is.na(data_train$marital) & is.na(data_train$education)), ] %>% filter(y == "no")


data_train[which(is.na(data_train$job) & is.na(data_train$education)), ]
  # job & education 둘다 결측
data_train[which(is.na(data_train$job) & is.na(data_train$education)), ] %>% filter(y == "no")

# 제거 가능한 신원미상 고객 제거
which(is.na(data_train$job) & is.na(data_train$education) & data_train$y == "no")
data_train <- data_train[-c(1511, 1656, 2043, 2717, 3003, 3013, 3051, 3863), ]
which(is.na(data_train$marital) & is.na(data_train$education) & data_train$y == "no")
data_train <- data_train[-2270, ]
which(is.na(data_train$job) & is.na(data_train$marital) & data_train$y == "no")
data_train <- data_train[-c(2831, 3599), ]


# 1.4. 이상치 처리 ####

boxplot(data_train$emp.var.rate)$stat
  # 0
boxplot(data_train$cons.price.idx)$stat
  # 0
boxplot(data_train$cons.conf.idx)$stat
  # outlier
summary(data_train$cons.conf.idx)
which(data_train$cons.conf.idx > summary(data_train$cons.conf.idx)[5] + 1.5*IQR(data_train$cons.conf.idx)) 
  # 55개
  # 소비자 신뢰지수가 높을수록 은행 수신상품 가입할 여지가 있다고 판단





# 1.5. 상관분석 및 카이제곱 검정 ####

# numeric
names(data_train)
cor_trn <- cor(data_train[, c(1, 11, 12, 13, 16, 17)], use = "pairwise.complete.obs")
cor_trn <- round(cor_trn, 2)
corrplot(cor_trn,
         method = "number",
         type = "lower",
         order = "hclust",
          # 유사한 상관계수끼리 군집화
         addCofe.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F
          # 대각 행렬 제외
)

# factor : [, c(2,3,4,5,6,7,8,9,10,14,18)]
# NA's : [, c(2,3,4,6,7)]
# default를 예측하기 위한 두 범주형 변수의 카이제곱 검정

str(data_train)

# 1.5.1. job, marital, education, y ####
freq2.3 <- table(data_train[,2], data_train[,3])
proportions(freq2.3, margin = 1)
chisq.test(freq2.3)

freq2.4 <- table(data_train[,2], data_train[,4])
proportions(freq2.4, margin = 1)
chisq.test(freq2.4)

#
freq2.6 <- table(data_train[,2], data_train[,6])
proportions(freq2.6, margin = 1)
chisq.test(freq2.6)
  # p-value = 0.7267

freq2.7 <- table(data_train[,2], data_train[,7])
proportions(freq2.7, margin = 1)
chisq.test(freq2.7)
  # p-value = 0.1792
#

freq2.18 <- table(data_train[,2], data_train[,18])
proportions(freq2.18, margin = 1)
chisq.test(freq2.18)

freq3.18 <- table(data_train[,3], data_train[,18])
proportions(freq3.18, margin = 1)
chisq.test(freq3.18)

freq4.18 <- table(data_train[,4], data_train[,18])
proportions(freq4.18, margin = 1)
chisq.test(freq4.18)



# 1.5.2. housing :loan, contact / loan : housing ####
freq6.7 <- table(data_train[,6], data_train[,7])
proportions(freq6.7, margin = 1)
chisq.test(freq6.7)

freq6.14 <- table(data_train[,6], data_train[,14])
proportions(freq6.14, margin = 1)
chisq.test(freq6.14)
  # p-value = 0.4464

freq7.14 <- table(data_train[,7], data_train[,14])
proportions(freq7.14, margin = 1)
chisq.test(freq7.14)
  # p-value = 0.5626

freq6.8 <- table(data_train[,6], data_train[,8])
proportions(freq6.8, margin = 1)
chisq.test(freq6.8)

freq7.8 <- table(data_train[,7], data_train[,8])
proportions(freq7.8, margin = 1)
chisq.test(freq7.8)
  # p-value = 1

freq6.18 <- table(data_train[,6], data_train[,18])
proportions(freq6.18, margin = 1)
chisq.test(freq6.18)
  # p-value = 0.4361

freq7.18 <- table(data_train[,7], data_train[,18])
proportions(freq7.18, margin = 1)
chisq.test(freq7.18)
  # p-value = 0.5083






# 1.5.3. 결측치 대체 ####

job_mod <- mice(data_train[, c(2, 3, 4, 18)], method = "rf", printFlag = F)
job_mod <- complete(job_mod)
table(job_mod$job)
data_train$job <- job_mod$job
data_train$marital <- job_mod$marital
data_train$education <- job_mod$education
summary(data_train)

housing_mod <- mice(data_train[, c(6, 7, 8)], method = "rf", printFlag = F)
housing_mod <- complete(housing_mod)
table(housing_mod$housing)
table(housing_mod$loan)
data_train$housing <- housing_mod$housing
summary(data_train$housing)

loan_mod <- mice(data_train[, c(7, 6)], method = "rf", printFlag = F)
loan_mod <- complete(loan_mod)
table(loan_mod$loan)
data_train$loan <- loan_mod$loan
summary(data_train$loan)

mice_plot <- aggr(data_train, col = c("grey", "blue"), numbers = TRUE, sortVars = TRUE, labels = names(data_train), cex.axis = 0.7, gap = 3, ylab = c("결측치", "패턴"))





# 1.5.4. 범주형 파생변수 생성 후 가설검정 ####

data_train <- data_train %>% mutate(
  generation = case_when(age <= 19 ~ "teen", age <= 32 ~ "youth", age <= 38 ~ "midlife", age <= 47 ~ "senior", age <= 69 ~ "yold", age <= 92 ~ "old"))
data_train$generation <- factor(
  data_train$generation, 
  levels = c("teen", "youth", "midlife", "senior", "yold", "old"))
table(data_train$generation)
ggplot(data = data_train, aes(x = generation, fill = generation)) + geom_bar() + scale_fill_brewer(palette = "Pastel2")

# Trial and Error 1 (일원분산분석) ####

# 정규성 검토 (Ha 채택, 조건 불충족)
# 등분산성 검정 (H0 채택, 조건 충족)
  
hist((data_train$campaign)[levels(data_train$generation) == "teen"])
hist((data_train$campaign)[levels(data_train$generation) == "youth"])
hist((data_train$campaign)[levels(data_train$generation) == "midlife"])
hist((data_train$campaign)[levels(data_train$generation) == "senior"])
hist((data_train$campaign)[levels(data_train$generation) == "yold"])
hist((data_train$campaign)[levels(data_train$generation) == "old"])

shapiro.test((data_train$campaign)[levels(data_train$generation) == "youth"])

leveneTest(campaign ~ generation, data = data_train)
  # 2P-value = 0.2547

# 등분산성 조건 충족 전제한 일원분산분석 
# (H0 채택, 연령대(gen) 집단 간 campaign 표본평균의 차이는 통계적으로 유의하지 않다는 결과)
anova <- aov(campaign ~ generation, data = data_train)
summary(anova)
  # 2P-value = 0.211


# Trial and Error 2 (이원분산분석) ####

# 정규성 검토 (Ha 채택, 조건 불충족)
# 집단 간 등분산성 검정 (Ha 채택, 조건 불충족)

hist((data_train$previous)[levels(data_train$generation) == "teen"])
hist((data_train$previous)[levels(data_train$generation) == "youth"])
hist((data_train$previous)[levels(data_train$generation) == "midlife"])
hist((data_train$previous)[levels(data_train$generation) == "senior"])
hist((data_train$previous)[levels(data_train$generation) == "yold"])
hist((data_train$previous)[levels(data_train$generation) == "old"])

shapiro.test((data_train$previous)[levels(data_train$generation) == "youth"])

leveneTest(previous ~ generation, data = data_train)

# 이분산성 전제한 일원분산분석 (Welch's ANOVA)
# (Ha 채택, 연령대(gen) 집단 간 campaign 표본평균의 차이는 통계적으로 유의하다는 결과)
oneway.test(previous ~ generation, data = data_train, var.equal = FALSE)

# 사후분석 (Bonferroni Correction)
# old Mu = teen Mu > youth Mu = midlife Mu = senior Mu = yold Mu
# 초고령과 고등학생 세대의 과거 상품 마케팅 건수 모평균은 청년, 중년, 장년, 고령 세대의 모평균보다 큰 값을 가지는 결과

dunn.test(data_train$previous, data_train$generation, method = "bonferroni")

# IV1(gen)와 DV(previous)에 대하여 IV2(job / marital / education / housing / loan)이 상호작용효과가 있는지(Ha) 이원분산분석 수행
# (Ha 채택, marital / education / housing)

data_train %>% group_by(generation) %>% summarise(mean = mean(previous)) %>% arrange(mean)
data_train$generation <- factor(
  data_train$generation, 
  levels = c("senior", "youth", "midlife", "yold", "teen", "old"))
  # 일원분산분석에서 표본평균 가장 작은 순서로 집단 먼저 출력되도록 함


# 직업 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 없다

tanova_j <- aov(previous ~ generation * job, data = data_train)
summary(tanova_j)
# print(model.tables(tanova_j, "means"), digits = 2)
interaction.plot(data_train$generation, data_train$job, data_train$previous, bty='l', main = "interaction effect plot between Gen and Job", col = c(1:11))
# interaction2wt(previous ~ generation * job, data = data_train)


# 가족유형 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 있다

tanova_m <- aov(previous ~ generation * marital, data = data_train)
summary(tanova_m)
interaction.plot(data_train$generation, data_train$marital, data_train$previous, bty='l', main = "interaction effect plot between Gen and Marital", col = c(1:4))


# 교육수준 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 있다

tanova_e <- aov(previous ~ generation * education, data = data_train)
summary(tanova_e)
interaction.plot(data_train$generation, data_train$education, data_train$previous, bty='l', main = "interaction effect plot between Gen and Edu", col = c(1:7))


# 주택담보대출 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 있다

tanova_h <- aov(previous ~ generation * housing, data = data_train)
summary(tanova_h)
interaction.plot(data_train$generation, data_train$housing, data_train$previous, bty='l', main = "interaction effect plot between Gen and housing", col = c(1:2))

# 신용대출 변수는 세대 집단별 과거 캠페인 횟수 모평균 차이를 강화하는 상호작용효과가 없다

tanova_l <- aov(previous ~ generation * loan, data = data_train)
summary(tanova_l)
interaction.plot(data_train$generation, data_train$loan, data_train$previous, bty='l', main = "interaction effect plot between Gen and loan", col = c(1:2))


# Trial and Error 3 (카이제곱 독립성 검정 chi-squared) ####

py <- table(data_train$generation, data_train$poutcome)
cy <- table(data_train$generation, data_train$y)
prop.table(py, margin = 1)
prop.table(cy, margin = 1)

chisq.test(py)
  # 세대 변수는 과거 마케팅 성공 여부에 영향을 미친다
chisq.test(cy)
  # 세대 변수는 현재 정기예금 가입 여부에 영향을 미친다. 

py_df <- data_train %>% group_by(generation, poutcome) %>% summarise(count = n())
ggplot(data = py_df, aes(x = generation, y = count, fill = generation)) + geom_col() + scale_fill_brewer(palette = "YlGnBu") + facet_wrap(~poutcome) + geom_text(aes(label = count), position = position_dodge(width = 1)) + ggtitle("과거 은행 상품마케팅 성패에 따른 세대별 고객 빈도")

cy_df <- data_train %>% group_by(generation, y) %>% summarise(count = n())
ggplot(data = cy_df, aes(x = generation, y = count, fill = generation)) + geom_col() + scale_fill_brewer(palette = "Reds") + facet_wrap(~y) + geom_text(aes(label = count), position = position_dodge(width = 1)) + ggtitle("현재 은행 정기예금 가입에 따른 세대별 고객 빈도")


# 1.6. 예측변수(IV) 간 관계 파악을 위한 이항 로지스틱 회귀분석 ####

# 훈련용 및 검증용 데이터 분리 : (7:3) (6:4) (8:2)

set.seed(12345)
data_glm <- sample(c(1:dim(data_train)[1]), 
                   dim(data_train)[1] * 0.7)
data_trn <- data_train[data_glm, ]
data_trn[, c(5, 12)] <- NULL
data_val1 <- data_train[-data_glm, ]
data_val1[, c(5, 12)] <- NULL
  # 결측과 누락된 값이 많은 pdays, default 변수 분석 제외

glm1_trn <- glm(y ~ ., family = "binomial", data = data_trn)
summary(glm1_trn)
exp(coef(glm1_trn))
confint(glm1_trn)

# 모형 성능 평가 (혼동행렬)

glm1_pred <- predict(glm1_trn, data_val1, type = "response")
df_glm1_pred <- as.data.frame(glm1_pred)
  # 반환된 확률 결과의 데이터 프레임 작성
df_glm1_pred$default <- ifelse(df_glm1_pred$glm1_pred >= 0.5, df_glm1_pred$default <- "Yes", df_glm1_pred$default <- "No")
  # 0.5 임계값으로 설정하여 범주화
table(df_glm1_pred$default)

class(data_val1$y)
table(data_val1$y)
class(df_glm1_pred$default)
table(df_glm1_pred$default)
data_val1$y <- ifelse(data_val1$y == "yes", "Yes", "No")
data_val1$y <- as.factor(data_val1$y)
df_glm1_pred$default <- as.factor(df_glm1_pred$default)

confusionMatrix(data = df_glm1_pred$default, 
                reference = data_val1$y)


# 모형의 유의성 검정 (카이제곱 검정) / 이탈도

anova(glm1_trn, test = "Chisq")

# 모형 변수 간 다중공선성 확인(Variance Inflation Factor 즉, 분산팽창요인)
# IV 간 공통된 선형성

vif(glm1_trn)
  # 10보다 작아야 하는 vif 값(Kabocoff, R in Action)
  # 2보다 작아야 하는 vif의 양의 제곱근
  
# 모형에 영향력 있는 변수 내림차순 차트 시각화 (표준화 회귀계수의 추정치의 절댓값)

vip(glm1_trn, num_features = 30)

# 변수 중요도 높은 상위 5개 IV 데이터 시각화 (다중공선성 있는 변수 제외)

ggplot(data = data_train, aes(x = poutcome, y = y)) + geom_jitter(aes(col = y), height = 0.1, width = 0.1)
ggplot(data = data_train, aes(x = month, y = y)) + geom_jitter(aes(col = y), height = 0.1, width = 0.1)
ggplot(data = data_train, aes(x = job, y = y)) + geom_jitter(aes(col = y), height = 0.1, width = 0.1)
ggplot(data = data_train, aes(x = contact, y = y)) + geom_jitter(aes(col = y), height = 0.1, width = 0.1)
ggplot(data = data_train, aes(x = campaign, y = y)) + geom_jitter(aes(col = y), height = 0.1, width = 0.1)



# 1.7. 채무불이행 연체 예측 ~ loan ####

# 1.7.1. 의사결정 분류트리 ####

data_class <- data_train[, -c(8:18)]
data_class <- data_class %>% filter(default == "unknown")
dim(data_class)[2]-1
  # x 변수의 수
summary(data_class)

class_split <- initial_split(data_class, prop = 0.7, strata = "loan") 
  # loan or housing ?
class_trn <- training(class_split)
class_val <- testing(class_split)

ctree <- rpart(loan ~ ., data = class_trn, method = "class", control = rpart.control(cp = 0))
  # infinite split

ctree$cptable
# cp_opt <- ctree$ctable[which.min(ctree$cptable[, "xerror"]), "CP"]
# ctree_p <- prune(tree, cp = cp_opt)

class_ctree <- predict(ctree, newdata = class_val, type = "class") # 0.5 이상
prob_ctree <- predict(ctree, newdata = class_val, type = "prob") # Error

confusionMatrix(class_ctree, class_val$loan, positive = "yes")
  # precision(p.p.v) : 0.22222
  # recall(sensitivity) : 0.05556



# 1.7.2. 배깅 ####

class_bag <- bagging(formula = loan ~ ., data = class_val, nbagg = 100, coob = TRUE, control = rpart.control(minsplit = 2, cp = 0))
# summary(class_bag)

bag_pred <- predict(class_bag, newdata = class_val, type = "class")
confusionMatrix(bag_pred, class_val$loan, positive = "yes")



# 1.7.3. 랜덤 포레스트 ####
n_features <- dim(data_class)[2]-1
class_rf <- randomForest(loan ~ ., data = class_trn, mtry = floor(n_features), importance = TRUE)

rf_pred <- predict(class_rf, newdata = class_val, type = "response")
confusionMatrix(rf_pred, class_val$loan, positive = "yes")



# 1.7.4. 부스팅 ####
class_boost <- train(loan ~ ., data = class_trn, method = "xgbTree", trcontrol = trainControl("cv", number = 5))

class_boost$bestTune

boost_pred <- class_boost %>% predict(class_val)
confusionMatrix(boost_pred, 
                class_val$loan, 
                positive = "yes")





# 1.8. 현재 은행 수신상품 마케팅 시 가입 고객 분류기 ####

# 1차 의사결정 분류트리 모델링

set.seed(12345)
# data_train_df <- data_train %>% filter(default == "unknown")
data_sample <- sample(c(1:dim(data_train)[1]), dim(data_train)[1] * 0.7)
data_trn <- data_train[data_sample, ]
data_val <- data_train[-data_sample, ]

gtree <- rpart(y ~ ., data = data_trn)
summary(gtree)
fancyRpartPlot(gtree)

# 트리모델 지도학습 성능평가 혼동행렬 (정오표)

predtree <- predict(gtree, data_val, type = "class")
confusionMatrix(predtree, data_val$y)

# 가지치기 (Pruning)

printcp(gtree)
ptree <- prune(gtree, cp = 0.1)
prpred <- predict(ptree, data_val, type = "class")
fancyRpartPlot(ptree)
confusionMatrix(prpred, data_val$y)

# 랜덤 포레스트

data_trn$pdays <- NULL
data_val$pdays <- NULL

n_feature <- dim(data_trn)[2]-1
rf <- randomForest(y ~ ., data_trn, mtry = floor(n_feature), importance = TRUE)
rf
rfpred <- predict(rf, data_val)
confusionMatrix(rfpred, data_val$y)



# 1.9. 홍보 배제된 고객의 특성 탐색 ####



