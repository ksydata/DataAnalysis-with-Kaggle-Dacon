
### 경영데이터분석2 : 머신러닝을 활용한 비즈니스 모델 개발

library(readr)
library(dplyr)
library(mice)
library(VIM)
library(descr)
library(tidyr)
library(psych)
library(ggplot2)
library(corrplot)
library(car)
library(ecm)
  # Build an Error Correction Model
library(lmtest)
library(lm.beta)
library(tidyverse)
library(caret)


# 4 ~ 7 week. 자전거 대여 데이터 분석 ####

bicycle <- read.csv("bicycle.csv", header = T, na = NA)
bcCodeBook <- data.frame(
  feature = c("case", "time", "season", "holiday", "working", "weather", "temp", "atemp", "humidity", "windspeed", "casual", "registered", "total"),
  
  descript = c("대여 연/월/일/시에 따른 구분", "대여 일시", "spring, summer, fall, winter", "공휴일 여부 더미변수(1: Yes, 0: No)", "근무일 여부 더미변수(1: Yes, 0: No)", "sunny, cloudy, weak rain snow, strong rain snow", "기온(단위: ℃)", "체감기온(단위: ℃)", "습도(단위: %)", "풍속(단위: m/s)", "비회원 대여회수(단위: 회)", "등록회원 대여회수(단위: 회)", "전체 대여회수(단위: 회)")
)


# 1. 탐색적 데이터 분석 ####

# 1.1. 변수의 자료형/척도 변경 ####

glimpse(bicycle)
bicycle$time <- as.POSIXct(bicycle$time) 
  # dttm, as.Date는?
for (i in 3:6) {
  # 상수의 시간 복잡도는 O(1)
  bicycle[,i] <- as.factor(bicycle[,i])
  if (i == 3 | i == 6) {
    print(table(bicycle[,i]))
  }
}

# factor형 변수 순서 변경
bicycle$season <- factor(
  bicycle$season,
  levels = c("spring", "summer", "fall", "winter"))
bicycle$weather <- factor(
  bicycle$weather,
  levels = c("sunny", "cloudy", "weak rain snow", "strong rain snow"))
table(bicycle$season)
table(bicycle$weather)


# 1.2. 결측값 처리 ####

# 결측값 시각화
mice_plot <- aggr(bicycle, col = c("lightgrey", "blue", numbers = TRUE, sortVars = TRUE, labels = names(bicycle), cex.axis = 0.5, gap = 3, ylab = c("NA", "Pattern")))
  # NA's [, c(9, 10, 13)]

# 통계 요약치
summary(bicycle)
  # NA's humidity(8) windspeed(2) total(2)

# 결측값 대체 또는 제거
bicycle$total <- ifelse(
  is.na(bicycle$total), 
  bicycle$casual + bicycle$registered, 
    # 등록회원 및 비회원의 대여수 합
  bicycle$total)
table(is.na(bicycle$total))
  # 결측의 이유 : 누락

which(is.na(bicycle$humidity))
bicycle[c(67, 1029, 3641, 5212, 7123, 8017, 9173, 10741), ]
which(is.na(bicycle$windspeed))
bicycle[c(2075, 10803), ]

# 빈도 확인 및 유형별 평균값 비교
for (i in 3:6) {
  freq(bicycle[,i])
}
  # season(class 균형) 
  # weather(level 1 > 2 > 3 > 4)
  # holiday(NO > Yes) working(Yes < No) 

bicycle %>% group_by(season, weather) %>% summarise(mean_total = mean(total, na.rm = T)) %>% arrange(-mean_total)
  # 계절과 날씨 ~ 환경 변수의 유형별 표본평균 비교

# 결측값 제거 (수업 진행에 따른 임의 제거)
table(is.na(bicycle))
bicycle <- bicycle %>% drop_na()


# 1.3. 이상치 처리 및 정제 ####

descr_bc <- describe(bicycle[, c(1, 7:13)])
descr_bc <- descr_bc %>% mutate(LL = mean - 3*sd, UL = mean + 3*sd)
table(bicycle$total >= descr_bc[8, 15])
  # mean + 3*sd = 735.09 (147개의 이상치)

ggplot(bicycle, aes(y = total)) + geom_boxplot(col = "blue", outlier.color = "red")
boxplot(bicycle$total)$stat
  # Q3 + 1.5*IQR = 647

# 이상치 행 제거 (수업 진행에 따른 임의 제거)
bicycle <- bicycle %>% filter(total < descr_bc[8, 15])


# 1.4. 파생변수 생성 ####

bicycle <- bicycle %>% mutate(difference = registered - casual)
summary(bicycle$difference)


# 2.1. 상관분석 실행(상관계수 추정치 크기 및 부호와 유의성 검토) ####

corr <- corr.test(bicycle[, c(7:14)], method = "pearson", alpha = 0.05, use = "pairwise.complete.obs")
print(corr, short = FALSE)
  # 상관계수 추정치가 통계적 유의함 (2P-value < 0.05)
  # 가장 강한 양의 상관관계 (temp와 atemp의 r = 0.99)

cor <- cor(scale(bicycle[, c(7:14)]), use = "pairwise.complete.obs")
  # 변수 간 상대적 크기로 미치는 영향 제거하기 위한 데이터 0 ~ 1로 정규화
  # NA 있는 위치에서의 연산만 넘어가는 파라미터
corrplot(cor)


# 2.2. 다중회귀분석 ####

'''

H1 : temp -> total (+)
H2 : atemp -> total (+)
H3 : humidity -> total (-)
H4 : windspeed -> total (+)
H5 : difference -> total (+)

'''

lm1 <- lm(total # DV 
          ~ temp + atemp + humidity 
          + windspeed + difference, # IV
          data = bicycle)

summary(lm1)
  # windpeed P-value = 0.23481
plot(lm1)
  # Residuals vs Fitted (선형성)
  # Normal Q-Q (정규성)
  # Scale-Location (등분산성과 선형성)
  # Residuals vs Leverage (Cook's D, 등분산성)

  # [c(6720, 6721, 6722, 8915, 8917, 8918), ]

bicycle <- bicycle[-c(6720, 6721, 6722, 8915, 8917, 8918), ]


# 2.1. 선형성, 정규성, 등분산성 확인

lm1 <- lm(total
          ~ temp + atemp + humidity 
          + windspeed + difference, 
          data = bicycle)
plot(lm1)
summary(lm1)

ks.test(bicycle$total, pnorm,
        mean = mean(bicycle$total),
        sd = sd(bicycle$total))
  # Kolmogorov-Smirnov Test : n이 클 때 정규성 검토

shapiro.test(sample(bicycle$total, replace = FALSE, size = 5000))
  # n이 작을 때 정규성 검토

hist(bicycle$total, breaks = seq(0, 1000, 10), col =  "lightgreen")
hist(log(bicycle$total), breaks = seq(0, 10, 0.1), col =  "green")


# 2.2. 독립성(오차의 자기상관) 검토 : DW ~ 2

durbinWatsonTest(lm1)
  # 오차의 자기상관 문제 있음(잔차의 독립성 조건 불충족)
  # H4 기각(beta4 = 0) 
  # H1, H2, H3, H5 채택(beta1,2,5 > 0, beta3 < 0)


# 2.3. 모형 적합도 제고를 위한 다중회귀식 추정 (Bias & Variance) ####

# Yi(hat) = 44.63 + 1.125*X1i + 4.129*X2i - 1.608*X3i + 1.030*X5i : 추정된 다중회귀식
# R-squared :  0.7612,	Adjusted R-squared:  0.7611

summary(lm1)
print(step, short = FALSE)

flm1 <- step(lm1, direction = "forward")
  # IV 하나씩 추가 (전진선택법)
  # AIC (모형적합도 지표) = -2*log(likelihood, 우도) + 2*K

summary(flm1)

blm1 <- step(lm1, direction = "backward")
  # IV 하나씩 제거 (후진제거법)
summary(blm1)

slm1 <- step(lm1, direction = "both")
  # stepwise (단계적 혼합법)
summary(slm1)

# 모형 적합 결과 : f|b|s 모든 방법에서 AIC = 94349.55로 동일함. IV를 모두 포함한 다중회귀식이 가장 모형적합도 높음


# 2.4. 다중공선성과 IV 중요도 ####

vif(lm1)
  # vif >= 5.3
  # temp(40.101056), atemp (40.212054)
  # GVIF^( 1 / (2*Df) ) 값 < 2

lm2 <- lm(total ~ temp + humidity + windspeed + difference, data = bicycle)

10719/4
  # k:n = 1:30
summary(lm2)
  # H4 : windspeed -> total (+) 기각 강화
plot(lm2)
bicycle_new <- bicycle[-c(6928, 6931, 7654, 7655), ]
lm2 <- lm(total ~ temp + humidity + windspeed + difference, data = bicycle_new)

vif(lm2)
  # 다중공선성을 일으킨 변수 atemp 제거

lm2 <- lm(total ~ temp + humidity + windspeed + difference, data = bicycle_new)
ks.test(bicycle_new$total,
        pnorm, 
        mean = mean(bicycle_new$total), sd = sd(bicycle_new$total))
  # 정규성 검토 (조건 불충족)
durbinWatsonTest(lm2)
  # 오차의 자기상관 문제 있음: 회귀모델 잔차가 자동상관관계 있다는 결론

corr <- corr.test(bicycle[, c(7:14)], method = "pearson", alpha = 0.05, use = "pairwise.complete.obs")
print(corr, short = FALSE)
# 상관계수 추정치가 통계적 유의함 (2P-value < 0.05)
# 가장 강한 양의 상관관계 (temp와 atemp의 r = 0.99)

cor <- cor(scale(bicycle[, c(7:14)]), use = "pairwise.complete.obs")
# 변수 간 상대적 크기로 미치는 영향 제거하기 위한 데이터 0 ~ 1로 정규화
# NA 있는 위치에서의 연산만 넘어가는 파라미터
corrplot(cor)




# 표준화 회귀계수의 추정치의 절댓값 = IV 중요도
lm.beta(lm1) 
  # dif > atem > hum > tem 
lm.beta(lm2)
  # dif > tem > hum


# 2.5. IV 추가 1 및 베이스라인 모델, 더미변수 ####

# 2.5.1. working = new IV 추가 타당성 검토 ####

lm3 <- lm(total ~ temp + humidity + windspeed + difference + working, data = bicycle_new)
summary(lm3)
ks.test(bicycle_new$total, pnorm,
        mean = mean(bicycle_new$total),
        sd = sd(bicycle_new$total)) 
  # 정규성 조건 불충족, 가설검정 및 pp도표
plot(lm3)
  # 잔차의 등분산성을 Residuals vs Fitted 산점도로 확인
vif(lm3)
  # 다중공선성 문제 없음(IV간 상관관계)
durbinWatsonTest(lm2)
durbinWatsonTest(lm3)
  # 오차의 자기상관 문제 있음(잔차의 독립성 조건)
  # humidty와 windspeed 음의 상관계수 r = - 0.32
  # total과 difference 양의 상관계수 r = 0.81

cor <- cor(scale(bicycle[, c(7:14)]), use = "pairwise.complete.obs")
corrplot(cor)


summary(lm3)
  # R-squared : 0.8081, Adjusted R-squared : 0.808
summary(lm2)
  # R-squared : 0.7617, Adjusted R-squared : 0.7616

0.8081 - 0.7617
anova(lm2, lm3)
  # 0.0464만큼 결정계수 증가하였으며, 이는 통계적으로 유의미한 변화량
  # Adjusted R-squared 증가하여 조건 충족
  # lm3가 lm2가 설명력이 더 좋으며, work을 IV로 추가하기로 결정
  # 단, 모형의 간명성(parsimony) 훼손


# 2.5.2. lm3 추정 결과 ####

summary(lm3)
table(bicycle_new$working)
  # 앞에 출력되는 "더미 변수" working의 level을 reference로 잡음
  # dvi = 0 (No), dvi = 1 (Yes)일 때 Yi(hat)이 더 큰 경우는 근무할 때보다 휴일일 때 대여수가 더 높다


# 2.5.3. 오차의 자기/자동상관문제 해결 시도 ####

lm4 <- lm(total ~ temp + humidity + windspeed + working, data = bicycle_new)
  # total과 difference 양의 상관계수 r = 0.81인바 IV에서 difference 제외
  # 더빈-완슨 d검정은 설명변수가 상수 값을 가진다는 가정을 충족해야 사용 가능

summary(lm4)
ks.test(bicycle_new$total, pnorm,
        mean = mean(bicycle_new$total),
        sd = sd(bicycle_new$total))
  # 정규성 조건 불충족, 가설검정 및 pp도표
plot(lm4)
  # 잔차의 등분산성을 Residuals vs Fitted 산점도로 확인
vif(lm4)
  # 다중공선성 문제 없음(IV간 상관관계)
durbinWatsonTest(lm4)
  # 오차의 자기상관 문제 있으나 개선 (잔차의 독립성 조건 여전히 불충족)
  # 최소제곱 추정량에 대한 정당성 깨진 상태

bicycle_new$working <- as.integer(bicycle_new$working)
cor_new <- cor(bicycle_new[, c(5, 7:14)], use = "pairwise.complete.obs")
corrplot(cor_new,
         method = "number",
         type = "lower",
         order = "hclust",
         # 유사한 상관계수끼리 군집화
         tl.col = "black",
         tl.srt = 45,
         diag = F
         # 대각 행렬 제외
)

bicycle_new$working <- as.factor(bicycle_new$working)

durbinH(lm4, rownames(summary(lm4)$coefficients))
  # Check Durbin's h-statistic on linear model
  # 설명변수에 확률변수(Random Variable)가 있을 경우

lm4$coefficients
  # row.names(summary(model)$coef) == ylag1var
  # Error : In sqrt(n/(1 - n * v)) : NaNs produced
  # 데이터의 정규 분포에서 음수를 사용했기 때문에 NaN 문제 발생

is.nan(log(summary(lm4)$coefficients))
  # Estimate : humidity and workingYes

print(durbinH)


# 2.6. IV 추가 2 및 DV 예측 ####

# 2.6.1. season = new IV 추가 타당성 검토 ####

str(bicycle_new$season)
  # 출력 순서대로 숫자 1 ~ 4를 부여함
summary(bicycle_new$season)

lm5 <- lm(total ~ temp + humidity + windspeed + difference + working + season, data = bicycle_new)
summary(lm5)
  # 더미변수 season의 reference는 spring
  # R-squared : 0.8119, Adj R-squared : 0.8117
summary(lm3)
  # R-squared : 0.8081, Adj R-squared : 0.808

0.8119 - 0.8081
anova(lm3, lm5)
  # 결정계수 0.0038%p 증가 ~ 결정계수 변화량은 통계적으로 유의한바 lm5 > lm3가 설명력이 더 좋다.(goodness of fit)
  # 수정된 결정계수 증가

# lm5 
# Yi(hat) = 77.133613 + 6.565*X1i - 1.485*X3i + 1.100*X5i - 79.07*dv1i + 0.45*dv2i -26.95*dv3i + 8.183*dv4i


'''

H1 : temp -> total (+)
H2 : atemp -> total (+)
H3 : humidity -> total (-)
H4 : windspeed -> total (+)
H5 : difference -> total (+)

'''



# 2.7. 조절효과 ####




