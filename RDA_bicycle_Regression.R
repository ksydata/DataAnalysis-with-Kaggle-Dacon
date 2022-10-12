
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

library(tidyverse)
library(caret)


# 자전거 대여수 예측 데이터 분석 ####

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
  # 변수간 상이한 단위 고려한 정규화
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

# 2.5. IV 추가, 베이스라인 모델과 더미변수 ####

# 2.6. IV 추가 및 DV 예측 ####

# 2.7. 조절효과 ####



# 3. 다항식 회귀 (Polynomial Regression) ####

# 3.1. Split the data into training and test set ####

set.seed(123)

bicycle.sample <- bicycle$total %>%
  createDataPartition(p = 0.8, list = FALSE)
train_bc  <- bicycle[bicycle.sample, ]
val_bc <- bicycle[- bicycle.sample, ]

# 3.2. Build the model ####

deg <- 5
plm <- lm(total ~ poly(
  season, temp, humidity, windspeed, 
  working, difference, raw = TRUE), 
  data = train_bc)

ggplot(train_bc, aes(x = ) ) + geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, deg, raw = TRUE))


# 3.3. Make predictions and Model performance ####

pred <- plm %>% predict(val_bc)

RMSE = RMSE(pred, val_bc$total)
RMSE


# 3.4. Choose p based on the validation error ####

RMSE_vector <- numeric(10)
deg_vector <- 1:10

for (deg in deg_vector) {
  plm <- lm(total ~ poly(
    season, temp, humidity, windspeed, 
    working, difference, raw = TRUE), 
    data = train_bc)
  
  pred <- plm %>% predict(val_bc)
    # Make predictions 
  
  RMSE_vector[deg] = RMSE(pred, val_bc$total)
    # Model performance
}

data.frame(deg = deg_vector, RMSE = RMSE_vector)

# 3.5. What if we choose p based on the training error? ####

RMSE_vec<-numeric(10)

deg_vec<-1:10

for (deg in deg_vec){
  model <- lm(medv ~ poly(lstat, deg, raw = TRUE), data = train.data)
  
  # Make predictions
  predictions <- model %>% predict(train.data)
  # Model performance
  RMSE_vec[deg] = RMSE(predictions, train.data$medv)
  
}

data.frame(deg=deg_vec, RMSE=RMSE_vec)


# 3.6. Perform the 5 fold Cross Validation(K-폴드 교차검증, K = 5) ####

# fitControl <- trainControl(method = "cv(통제변수)", number = 5)

fitControl <- trainControl(
  method = "repeatedcv", number = 5, repeats = 5)

deg_vector <- 1:10
RMSE <- numeric(10)


# 3.7. Train the model using Polynomial Linear Regression ####

for (deg in deg_vector) {
  
  f <- bquote(total ~ poly(season, .(deg)))
  
  LinearRegressor <- caret::train(
    as.formula(f), data = train_bc, 
    method = "lm", trControl = fitControl)
  
  RMSE[deg]<- LinearRegressor$results$RMSE
  
}

Degree.RegParams <- data.frame(deg_vector, RMSE)

ggplot(data = Degree.RegParams, 
       aes(x = deg_vec, y = RMSE)) + geom_line() + xlab("deg")
