
# 경영데이터분석2 중간고사 대비 겸 통계분석실습 2차과제 작성 #
# https://www.kaggle.com/datasets/jillanisofttech/brain-stroke-dataset #


library(readr)
library(VIM)
library(psych)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

library(caret)
library(vip)
library(car) # core
library(lmtest)
library(lm.beta)


stroke <- read.csv("brain_stroke.csv", header = TRUE, sep = ",", na = NA)
codebook <- data.frame(
  features = colnames(stroke),
  descript = c("성별 : 남성, 여성 또는 기타", 
                "연령 : 환자의나이", 
                "고혈압 : 고혈압이없는 경우 0, 고혈압이있는 경우 1", 
                "심장 질환 : 환자가 심장 질환이없는 경우 0, 환자가 심장 질환이있는 경우 1",
                "결혼한 경우 : 아니오 또는 예",
                "직장 유형 : 어린이, 공무원, 무직, 사기업 직장인 또는 자영업자",
                "거주 유형 : 농촌 또는 도시",
                "평균 포도당 수준 : 혈액의 평균 포도당 수준",
                "bmi : 체질량 지수",
                " smoking_status : 과거 흡연, 흡연, 흡연 또는 알 수없음",
                "뇌졸중 : 환자가 뇌졸중을 앓은 경우 1 또는 그렇지 않은 경우 0")
)
codebook[12,] <- c("뇌졸중의 주요 위험 요인", 
                   "(1) 고혈압, (2) 혈중 콜레스테롤 농도, (3) 흡연, (4) 비만, (5) 당뇨, (5) 이전 TIA 허혈성 발작, (6) 말기 신장 질환 및 7) 심방 세동")



# 1. 탐색적 데이터 분석
# 1.1. 결측값 탐색 ####

str(stroke)
for ( i in 1:(dim(stroke)[2]) ) {
  if ( class(stroke[,i]) == "character" ) {
    stroke[,i] <- as.factor(stroke[,i])
  }
}
?aggr
mice_plot <- aggr(stroke, col = c("grey", "green"), bar = TRUE, numbers = TRUE, prop = TRUE,
                  combined = FALSE, varheight = TRUE, border = par("fg"), sortVars = FALSE,
                  ylabs = NULL, cex.lab = 0.7, cex.axis = par("cex"), 
                  cex.numbers = par("cex"), gap = 2)

'''
   [결측값 탐색]
   문자형 변수를 범주형으로 변환하는 for문과 if문
   결측값 시각화하는 mice_plot 차트 : 결측값 없으므로 이상치 탐색
'''
   


# 1.2. 이상치 탐색 ####

descr <- describe( stroke[, c(2:4, 8, 9, 11)] )
descr <- descr %>% mutate(UL = mean + 2.58*sd, LL = mean - 2.58*sd)
summary(stroke[, c(2:4, 8, 9, 11)])

ggplot(data = stroke, aes(y = age)) + geom_boxplot(col = "blue", outlier.color = "red")
ggplot(data = stroke, aes(y = hypertension)) + geom_boxplot(col = "blue", outlier.color = "red")
ggplot(data = stroke, aes(y = hypertension)) + geom_boxplot(col = "blue", outlier.color = "red")
ggplot(data = stroke, aes(y = heart_disease)) + geom_boxplot(col = "blue", outlier.color = "red")
ggplot(data = stroke, aes(y = stroke)) + geom_boxplot(col = "blue", outlier.color = "red")

ggplot(data = stroke, aes(y = avg_glucose_level)) + geom_boxplot(col = "purple", outlier.color = "orange")
boxplot(stroke$avg_glucose_level)$stat
ggplot(data = stroke, aes(y = bmi)) + geom_boxplot(col = "navy", outlier.color = "green")
boxplot(stroke$avg_glucose_level)$stat

       
'''
    [이상치 탐색]
    avg_glucose_level  
    bmi
'''

'''
    [에러 코드] 
    for ( i in 1:(dim(stroke)[2]) ) {
      if ( class(stroke[,i]) == "numeric" | "integer ) {
        descr[i,] <- describe(stroke[,i])
      }
    }
    [코드 작성의도와 원인]
    stroke 데이터 프레임의 연속형 변수의 통계 요약치 추출
    실수형, 논리형 변수만 적용가능하다는 결과값 반환
'''



# 1.3. 이상치 처리 ####

outlier <- stroke[(stroke$avg_glucose_level >= descr[4, 14]), ] 
outlier[181:219,] <- stroke[(stroke$bmi >= descr[5, 14]), ]
summary(outlier)
which(stroke$avg_glucose_level >= descr[4, 14] | stroke$bmi >= descr[5, 14])

which(duplicated(outlier) == TRUE)
outlier[c(181, 191, 203), ]
which(stroke$avg_glucose_level >= descr[4, 14] & stroke$bmi >= descr[5, 14])
stroke[c(18, 1412, 3342), ]
    # [중복되는 행 유무 확인]

stroke$outlier <- ifelse(stroke$avg_glucose_level > descr[4, 14] | stroke$bmi >= descr[5, 14], "outlier", "normal")
table(stroke$outlier)
    # [이상치 여부 이진 변수 생성]

''' 
    [에러 코드] 
    The matrix is not in [-1, 1]!
    
    cor <- corrplot(scale(stroke[, c(2,3,4,8,9,11)]),
                    method = "number",
                    type = "lower",
                    order = "hclust",
                    tl.col = "black", 
                    diag = F)
                      # [상관관계 있는 변수 선택하여 데이터 시각화 목적으로 수행]
                      # [유사한 상관계수끼리 군집화]
                      # [대각 행렬 제외]
'''

table(stroke$stroke)
ggplot(data = stroke, aes(x = avg_glucose_level, y = stroke, group = factor(work_type), col = factor(work_type))) + geom_point() + facet_wrap(~outlier)
    # [평균 포도당 지수와 뇌졸중 경험 여부, 직업 유형으로 범주화, 건강지수 이상치로 면 분할]
ggplot(data = stroke, aes(x = avg_glucose_level, y = bmi, group = factor(smoking_status), col = factor(smoking_status))) + geom_point() + facet_wrap(~outlier) + geom_smooth()
    # [평균 포도당 지수와 체질량 지수, 흡연 유형으로 범주화, 건강지수 이상치로 면 분할

table(stroke$heart_disease)
table(stroke$hypertension)
ggplot(data = stroke, aes(x = age, y = avg_glucose_level, group = factor(heart_disease), col = factor(heart_disease))) + geom_point() + facet_wrap(~outlier) + geom_smooth()
    # [연령과 평균 포도당 지수, 심혈관 질환 경험으로 범주화, 건강지수 이상치로 면 분할]

'''
    [이상치 처리기준]
    avg_glucose_level(혈액의 평균 포도당 수준) 181개
    bmi(체질량 지수) 39개
    [이상치의 데이터 패턴 탐색]
    뇌졸중에 걸릴 확률이 50%(예시) 이상인 사람 예측 목적의 분석
    [데이터 시각화]
'''

# 2. 다중회귀분석 ####

# 2.1. 다중선형회귀모형 lm1  ####

for ( i in c(3:4, 11:12) ) {
  stroke[,i] <- as.factor(stroke[,i])
}

lm1 <- lm(bmi ~ avg_glucose_level + age, data = stroke)
summary(lm1)
plot(lm1)

'''
    [다중선형회귀식 수립]
    H1 : avg_glucose_level -> bmi (+)
    H2 : age -> bmi (+)
    Yi(hat) = 22.30 + 0.0156 * X1i + 0.1046 * X2i
'''

ks.test(stroke$bmi, pnorm, 
        mean = mean(stroke$bmi), sd = sd(stroke$bmi))
summary(stroke$bmi)
hist( stroke$bmi, breaks = seq(0, 50, 5), col = "lightgreen" )
    # [n이 클 때 정규성 검토] n = 5000
    # 정규성 조건 불충족

durbinWatsonTest(lm1)
    # [H0 : 잔차의 자기상관관계 없다.]
    # D-W Statistic = 2.008, P-value (0.812) > 5%
    # 잔차의 독립성 요건 충족

vif(lm1)
    # [vif >= 5.3일 때 독립변수 간 상관관계 없다.]
    # 다중공선성 요건 충족



# 2.2. 모형적합도 제고를 위한 다중회귀식 lm1 추정 ####

f_lm1 <- step(lm1, direction = "forward")
b_lm1 <- step(lm1, direction = "backward")
s_lm1 <- step(lm1, direction = "both")

'''
    [AIC 모형적합도 지표] -2*log(우도) + 2*K
    목표 : 최소화
    전진선택법 : IV 하나씩 추가
    후진선택법 : IV 하나씩 제거
    단계적 혼합법(stepwise)
    [다중회귀모형 적합 결과]
    AIC = 18279.06으로 IV를 모두 포함한 식의 적합도가 높음.
'''

lm.beta(lm1)
    # [표준화 회귀계수의 추정치의 절댓값]
    # IV 중요도 : age > avg_glucose_level



# 2.3. IV 추가(더미변수) 및 타당성 검토 ####

table(stroke$hypertension)

lm2 <- lm(bmi ~ avg_glucose_level + age + hypertension, data = stroke)

shapiro.test(stroke$bmi)
vif(lm2)
durbinWatsonTest(lm2)
lm.beta(lm2)

summary(lm2)
summary(lm1)
anova(lm1, lm2)

'''
    [lm1 & lm2의 결정계수와 수정된 결정계수]
    lm2 : R-squared (0.1519) Adjusted R^2 (0.1514)
    lm1 : R-squared (0.1498) Adjusted R^2 (0.1495)
    수정된 결정계수 증가 요건 충족
    결정계수의 변화율이 통계적으로 유의한지 ANOVA로 검정결과 goodness of fit
    p-value (0.000414) < 0.05로 5% 오차범위를 감수하면서 통계적으로 유의한 차이
'''



# 2.4. IV추가 및 DV 예측 ####

22.30 + 0.0156 * 113.86 + 0.1046 * 70 + 1.1128 * 1

'''
    [case]
    avg_glucose_level = 113.86, age = 70, hypertension = 1
    Yi(hat) = 22.30 + 0.0156 * X1i + 0.1046 * X2i + 1.1128 * dv1i
    [prediction]
    bmi = 32.51102으로 3분위수에 해당하는 높은 값
'''




# 2.5. 계층적 회귀분석 : 조절효과 ####

'''
    [model 1] IV와 DV로만 구성 (lm1)
    [model 2] model 1에 MV인 hypertension 추가 구성(lm2)
    [model 3] model 2에 상호작용변수인 
    age * hypertension 추가하여 구성 (lm3)
    독립변수, 조절변수, 상호작용변수를 동시에 투입하여 종속변수에 미치는 영향력을 검증
'''

stroke$hypertension <- as.integer(stroke$hypertension)
stroke <- stroke %>% mutate(inter = age * hypertension)

lm3 <- lm(bmi ~ avg_glucose_level + age + hypertension + inter, data = stroke)

vif(lm3)
    # [inter의 영향으로 높아진 다중공선성]
durbinWatsonTest(lm3)
vip(lm3, num_features = 20, geom = "point")

stroke$hypertension <- as.factor(stroke$hypertension)
summary(lm3)
summary(lm2)
anova(lm2, lm3)

'''
    [Adj R^2 증가]
    [R^2 변화량 통계적으로 유의한 차이 anova] 
    0.1519 ~ 0.1758
    lm3 > lm2에 비해 모형적합도, 즉 설명력 높음
    
    [조절효과]
    H2 : age -> bmi (+)에 대하여 hypertension의
    조절효과가 존재한다. 양측검정 수행
    b4(beta4) 회귀계수 추정치 음수이며, 
    통계적으로 유의한바 고혈압 변수는
    고령일수록 체질량 지수가 증가한다는 기존 양의
    인과관계를 (-) "약화"한다.
    
    연령이 높아질수록 체질량 지수을 높이는 영향력이 
    과거 고혈압 경험을 한 경우에는 완화된다.
    고연령이라도 고혈압이 있으면 체질량 지수가 덜 높아진다는 결과
    
    b2 (+0.3612) * (-) 조절효과 =  b4 (-0.2509)
    cf.(-)*(-) = (+)
'''

