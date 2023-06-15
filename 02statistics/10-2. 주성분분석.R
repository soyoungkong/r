## 데이터 정제
data(longley)

cor(longley)

longley_lm <- lm(GNP~.,data = longley)
summary(longley_lm)

longley_fit <- princomp(longley[,-2], cor = T) #두번째 변수를 제외하고 
summary(longley_fit)

## 주성분 점수
longley_fit$loadings

## 각 데이터의 주성분 값 
head(longley_fit$scores)

longley2 <- longley
longley2[,-2] <- longley_fit$scores

longley_lm2 <- lm(GNP~., data = longley2)
summary(longley_lm2)

#주성분분석은 해석상의 다소 어려움이 있다. 