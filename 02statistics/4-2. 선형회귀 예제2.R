#예제2
x <- c(1,0,0,1,1,1,0,1,0,0,1,0,0,0,1)
y <- c(4,7,9,9.5,5,5,7,3,6,7.5,3.5,8,6,5,2)
## LSE
beta1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
beta0 <- mean(y)-beta1*mean(x)
beta0;beta1 

## 회귀계수 
R_square <- 1-sum((y-(beta0+beta1*x))^2)/sum((y-mean(y))^2)
R_square

## lm 함수 "여성", "남성"
x <- factor(x=c("o", "x", "x", "o", "o", "o", "x", "o", "x", "x", "o", "x", "x", "x", "o"), levels = c("x", "o"))
y <- c(4,7,9,9.5,5,5,7,3,6,7.5,3.5,8,6,5,2)
lse_result <- lm(y~x)
summary(lse_result)

# xo : factor 
# p값도 다 0.05이하로 유의미하게 나옴 

