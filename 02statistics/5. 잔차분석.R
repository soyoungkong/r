x1 <- c(10,8,13,9,11,14,6,4,12,7,5)
y1 <- c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)

x2 <- c(10,8,13,9,11,14,6,4,12,7,5)
y2 <- c(9.14,8.14,8.74,8.77,9.26,8.1,6.13,3.1,9.13,7.26,4.74)

## 분석 결과 
lm1 <- lm(y1~x1); lm2 <- lm(y2~x2)
summary(lm1);summary(lm2)
#서로 유사하게 나옴. 
lm1$coefficients #회귀계수만 출력 
lm1$fitted.values

# 두개의 데이터 셋은 유사한 형태를 가진다? 아래에서 검증
## 선형성(자료의 형태)
plot(x1, y1);lines(x1, lm1$fitted.values, col = "red"); 
plot(x2, y2);lines(x2, lm2$fitted.values, col = "red")
#두번째는 잔차분석을 위반한다 .

## 등분산성(잔차 산점도)
#residuals : 잔차  (실제값-x값)
plot(lm1$fitted.values, lm1$residuals); 
plot(lm2$fitted.values, lm2$residuals);

#install.packages("lmtest")
library(lmtest)
dwtest(lm1); dwtest(lm2)
#?dwtest : 설명

## 정규성(Q-Q plot)
qqnorm(y1);qqline(y1);
qqnorm(y2);qqline(y2)
#plot을 그려서 보지만, 정확히 통계적으로 수치적으로 유의미하는 것이 중요함. 
shapiro.test(y1) #0.05보다 커서 정규성 과정에 만족
shapiro.test(y2) #0.05보다 작아서 정규성 과정에 위배 
