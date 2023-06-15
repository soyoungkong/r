## 데이터 정제
cereal <- read.csv("C:/workspace/github/r/02statistics/data/cereals.csv")
#9개 변수 사용
cereal <- cereal[,c("calories","protein","fat","sodium","fiber","carbo","sugars","potass","vitamins")]
cereal <- na.omit(cereal)

library(stats)
cereal_fit <- princomp(cereal, cor = T)
summary(cereal_fit)

## 주성분 점수
cereal_fit$loadings

## scree plot
plot(cereal_fit, type = "lines")

## 각 데이터의 주성분 값 
head(cereal_fit$scores)

cor(cereal)

plot(cereal)