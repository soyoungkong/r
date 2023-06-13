# ------------------------------------------------------------
# R Script : 분류분석 (로지스틱회귀분석) 실습
# 작성자 : 김준기
# ------------------------------------------------------------

## 1. 데이터 불러오기
# 작업 디렉토리 설정
setwd("C:/workspace/github/r/01Fundamental/data")

# 데이터 불러오기
cancer_df <- read.csv(file = "DAT/cancer_data.csv",
                      header = TRUE,
                      sep = ",", 
                      stringsAsFactors = TRUE)

# 2. 데이터 탐색 및 전처리
# 데이터 구조
str(cancer_df)

# 데이터 확인
head(cancer_df)

# 패키지 로딩
library(dplyr)
# 패키지 확인
search()

# 데이터를 Id 변수의 오름차순 정렬
# 데이터 정렬 기능: arrange()
cancer_df <- cancer_df %>% arrange(Id)

# 데이터 확인
head(cancer_df)

# 데이터 정합성 확인
# Id 변수 중복 여부 확인
length(unique(cancer_df$Id))

# Id 변수 값에 대한 건수 확인
sort(table(cancer_df$Id))

# Id 변수 값에 대한 건수 중 중복된 Id 데이터 건수
Id.cnt <- sort(table(cancer_df$Id))
Id.cnt[Id.cnt > 1]

# 중복된 Id 데이터 건수의 합
sum(Id.cnt[Id.cnt > 1])

# 중복된 Id 값
duplicate.Id <- names(Id.cnt[Id.cnt > 1])
duplicate.Id

# 중복된 Id를 분석 데이터에서 제외
cancer_df <- cancer_df %>% filter(!(Id %in% duplicate.Id))

# 데이터 요약
summary(cancer_df)

# 행(row)에 결측치가 있을 경우 분석 대상에서 제외
cancer_df <- na.omit(cancer_df)

# 데이터 요약
summary(cancer_df)

# 종속변수(Class) 빈도 계산
table(cancer_df$Class)

# 종속변수(Class) 비율 계산
prop.table(table(cancer_df$Class))

# 종속변수 0, 1 값 지정
data.y <- as.factor(ifelse(cancer_df$Class == "malignant", 1, 0))
data.y

# 독립변수 지정 : Id와 종속변수 제외
x_df <- cancer_df %>% select(-c(Id, Class))

# 독립변수들간의 상관계수 계산
cor_result <- cor(x_df)
cor_result

# 패키지 로딩
library(corrplot)

# 상관관계 그래프 작성
corrplot(corr = cor_result, 
         method = "circle", 
         type = "lower",
         addCoef.col = "black")

# 연관성 높은 변수들로 군집 생성
corrplot(corr = cor_result, 
         order = "hclust",
         addrect = 2)

# 데이터프레임 생성
data_df <- data.frame(Class = data.y, x_df)

# 패키지 로딩
library(ggplot2)

# 박스플롯(Boxplot) 그래프
ggplot(data = data_df) +
  geom_boxplot(mapping = aes(x = Cl.thickness,
                             y = Class,
                             fill = Class))


# 3. 데이터 모델링 - 모형학습(로지스틱회귀분석)
# 로지스틱회귀모형 생성
logistic_model <- glm(formula = as.formula("Class ~ ."), 
                      data = data_df, 
                      family = binomial)

# 회귀계수 값
logistic_model

# 로지스틱회귀모형에 대한 정보들
summary(logistic_model)

# 변수선택법을 이용하여 모델 적합하기(후진제거법)
logistic_bwd_model <- step(object = logistic_model, 
                           direction = "backward")

# 변수선택법에 의한 로지스틱회귀모형에 대한 정보들
summary(logistic_bwd_model)

# 로지스틱회귀모형에 대한 정보들
# 종속변수의 적합값
logistic_bwd_model$fitted.values

# 로지스틱회귀모형에 대한 정보들 이름 보기
names(logistic_bwd_model)


# 4. 데이터 모델링 - 모형예측
# 1) 변수선택법에 의한 로지스틱회귀모형에 대한 예측
predict.bwd.p <- predict(object = logistic_bwd_model, 
                         newdata = data_df, 
                         type = "response")
predict.bwd.y <- ifelse(predict.bwd.p > 0.5, 1, 0) 

# 실제값과 예측값 비교
table(real = data_df$Class, predict = predict.bwd.y)

# 2) 변수선택법을 적용하지 않은 로지스틱회귀모형에 대한 예측
predict.p <- predict(object = logistic_model, 
                     newdata = data_df, 
                     type = "response")
predict.y <- ifelse(predict.p > 0.5, 1, 0) 

# 실제값과 예측값 비교
table(real = data_df$Class, predict = predict.y)

# 평가 지표 계산하기 - 정분류율, 민감도, 특이도
# 실제값과예측값 비교
table(real = data_df$Class, predict = predict.bwd.y)

# 정분류율(Accuracy)
(353 + 212) / 584

# 민감도(Sensitivity)
212 / (11 + 212)

# 특이도(Specificity)
353 / (353 + 8)
