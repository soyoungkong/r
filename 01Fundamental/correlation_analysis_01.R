# 작업 디렉토리 설정
setwd("C:/workspace/github/r/01Fundamental/data")
# 데이터 불러오기
iris_df <- read.csv(file = "DAT/iris_data.csv",
                    stringsAsFactors = TRUE)


# 두 변수간의 상관계수 계산
cor(iris_df$Petal.Length, iris_df$Petal.Width)


# 두 변수간의 상관계수 계산
cor(iris_df$Sepal.Length, iris_df$Sepal.Width)


# 상관관계 유의성 검정
cor.test(iris_df$Sepal.Length,
         iris_df$Sepal.Width)


# 패키지 로딩
library(dplyr)

# 품종별 두 변수간의 상관계수 계산
iris_df %>% 
  group_by(Species) %>% 
  summarize(COR = cor(Sepal.Length, 
                      Sepal.Width)) 


# 숫자형 변수들간의 상관계수 계산
cor(iris_df[, c(1:4)])


# 패키지 설치 : 최초 1회만 수행
install.packages("corrplot")

# 패키지 로딩
library(corrplot)

# 패키지 확인
search()


# 숫자형 변수들간의 상관계수 계산
iris_cor_result <- cor(iris_df[, c(1:4)])
iris_cor_result

# 상관관계 그래프 작성
# circle 형태로 표시
corrplot(corr = iris_cor_result)


# 숫자로 표시
corrplot(corr = iris_cor_result, 
         method = "number")
# circle 형태 + 숫자로 표시
corrplot(corr = iris_cor_result, 
         method = "circle", 
         type = "lower",
         addCoef.col = "black")


# 연관성 높은 변수들로 군집 생성
corrplot(corr = iris_cor_result, 
         order = "hclust", 
         addrect = 2)
