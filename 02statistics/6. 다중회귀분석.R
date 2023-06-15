## 다중회귀분석
str(iris)
iris_lm <- lm(Sepal.Width~Petal.Length+Sepal.Length+Petal.Width, data=iris)
iris$Sepal.Length;
summary(iris_lm)
plot(iris_lm$fitted.values, iris_lm$residuals)

## 독립성(Durbin-Watson test)
dwtest(iris_lm)

## 정규성 
qqnorm(iris$Sepal.Length);qqline(iris$Sepal.Length);
shapiro.test(iris$Sepal.Width)

levels(iris$Species)
iris_lm2 <- lm(Sepal.Width~., data=iris) ## 모든 설명변수를 입력
summary(iris_lm2)
#species 포함해도 모든 변수들이 유의미하다. 
#level이 3개라서 더미 variable은 2개가 나온다 
