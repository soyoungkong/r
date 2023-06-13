# 패키지 설치 : 최초 1회만 수행
# install.packages("ggplot2")

# 패키지 로딩
library(ggplot2)
# 패키지 확인
search()


# 작업 디렉토리 설정
setwd("C:/workspace/github/r/01Fundamental/data")
# 데이터 불러오기
iris_df <- read.csv(file = "DAT/iris_data.csv",
                    stringsAsFactors = TRUE)


# 히스토그램(Histogram) 그래프
ggplot(data = iris_df) +
  geom_histogram(mapping = aes(x = Petal.Length))


# 히스토그램(Histogram) 그래프
# 막대에 품종을 구분하여 색상 표시 
ggplot(data = iris_df) +
  geom_histogram(mapping = aes(x = Petal.Length,
                               fill = Species))


# 산점도(Scatter Plot) 그래프
ggplot(data = iris_df) +
  geom_point(mapping = aes(x = Petal.Length, 
                           y = Petal.Width))


# 산점도(Scatter Plot) 그래프
# 포인트(데이터 값)에 품종을 구분하여 색상 표시 
ggplot(data = iris_df) +
  geom_point(mapping = aes(x = Petal.Length, 
                           y = Petal.Width, 
                           colour = Species))


# 산점도(Scatter Plot) 그래프 파일 저장
png(filename = "C:/workspace/github/r/01Fundamental/data/OUT/ggplot2.png", 
    width = 1024, height = 768)
ggplot(data = iris_df) +
  geom_point(mapping = aes(x = Petal.Length, 
                           y = Petal.Width, 
                           colour = Species))
dev.off()


# 패키지 설치 : 최초 1회만 수행
#install.packages("plotly")

# 패키지 로딩
library(plotly)
# 패키지 확인
search()


# ggplot 그래프 작성
p1 <- ggplot(data = iris_df) +
  geom_point(mapping = aes(x = Petal.Length, 
                           y = Petal.Width, 
                           colour = Species))
# 동적 ggplot 그래프 작성
ggplotly(p1)
