#=========
# 데이터 확인
#=========
# 데이터 생성
df = data.frame(
  ID   = c('A001', 'A002', 'A003', 'A004', 'A005'), 
  제품 = c('노트북', 'TV', '노트북', '핸드폰', 'TV'), 
  수량 = c(1, 1, 2, 2, 1), 
  금액 = c(10000, 25000, 20000, 6400, 25000)
  )
df

str(df) # 데이터 전반적인 정보
dim(df) # 데이터 차원 dimension 행5, 열4  
summary(df) # 기초통계량 character는 기초통계량이 나올 것이 없음 

#=========
# 결측치 탐색
#=========
# 데이터 생성 (임의로 결측이 있는 것을 확인)
df = data.frame(
  ID   = c('A001', 'A002', 'A003', 'A004', 'A005'), 
  제품 = c(NA, NA, '노트북', '핸드폰', 'TV'), 
  수량 = c(1, 1, 2, 2, NA), 
  금액 = c(10000, 25000, 20000, 6400, 25000)
)
df

is.na(df) # 결측치 확인
colSums(is.na(df)) # 변수마다 결측치 수 확인


#=========
# 결측치 제거
#=========
df_1 = data.frame(
  x1 = c(15, 23, NA, 16, 24),
  x2 = c(234, 232, NA, 345, 303),
  x3 = c(10, 5, NA, 7, 7),
  x4 = c(NA, 53, NA, 54, 48)
  )
df_1

na.omit(df_1) # 결측치가 하나라도 있으면 관측치 제거


#=========
# 결측치 대체
#=========
df_1

## 평균값 대체
df_2 <- df_1 # 데이터 복사 
df_2[is.na(df_2$x3),"x3"] <- mean(df_2$x3, na.rm = T) 
#3번쨰 컬럼이 결측이 어느 위치에 있는지를 봄. x3에서 평균을 구함 
# 그냥 구하면 NA가 나오므로, 결측이 하나도 있으면 평균이 안됨. 
# na.rm = T 결측 제거하고 평균 계산. 
# 계산 한 평균값을 x3의 결측 자리에 넣어서 대체 7.25가 들어감
df_2

## 단일값 대체
df_2[is.na(df_2$x4),"x4"] <- -999 #누가 봐도 결측으로 보이도록 -999를 넣음 
df_2

## 선형회귀모형 대체
lm_fit <- lm(x4~., data = na.omit(df_1))
predict(lm_fit, df_1[1,])
df_1

## kNN 대체- 포뮬러가 복잡. 나중에 더 진행
library(class)
knn_2 <- knn(train = na.omit(df_1)[,1:3],
             test = df_1[1,1:3],
             cl = na.omit(df_1)[,4],
             k = 2)
# predict가 아니라 바로 여기서 다 진행함. 결측을 제거한 모든 데이터를 넣음. 
# cl자리에 y값을 넣음. 
# k = 주변에 몇개까지 볼 지 
df_1
knn_2


#=========
# 이상치 탐색
#=========
library(ggplot2)

head(iris)

# iris_df <- iris
#iris 데이터의 Sepal.Width 이상치 탐색

# 분포 확인 : 시각적으로 먼저 확인 
#상자그림
ggplot(iris, aes(x=Sepal.Width)) + geom_boxplot()

#히스토그램
ggplot(iris, aes(x=Sepal.Width)) + geom_histogram()

#산점도
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()

# 사분위수 범위
quantile(iris$Sepal.Width, 3/4)
quantile(iris$Sepal.Width, 1/4)

IQR(iris$Sepal.Width)

# 이상치 값 탐색
(lb <- quantile(iris$Sepal.Width, 1/4) - 1.5*IQR(iris$Sepal.Width))
(ub <- quantile(iris$Sepal.Width, 3/4) + 1.5*IQR(iris$Sepal.Width))

iris$Sepal.Width[which(iris$Sepal.Width < lb)] 
iris$Sepal.Width[which(iris$Sepal.Width > ub)] # 4.05보다 큰 값 

