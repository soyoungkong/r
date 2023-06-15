# 예제 1
y <- c(30,25,34,35,31,29,33,23,36,30,32,29,34,30,28,29,33,24,30,30)
x <- c(42,38,51,53,40,37,41,29,52,39,45,34,47,35,44,48,47,30,29,34)
x
y
mean(x)
sum(x)/length(x) # mean과 같음 

# LSE 
beta1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
beta0 <- mean(y)-beta1*mean(x)
beta0;beta1 
beta1
beta0
#반올림 함수 
round(beta0, 3) #세 자리 까지 반올림(0이 생략)


## 회귀계수 
beta0+beta1*x
R_square <- 1-sum((y-(beta0+beta1*x))^2)/sum((y-mean(y))^2)
R_square


## lm 함수 
# install.package("stats") # 내장함수라서 있을 것임 
# library("stats")
# search()
lse_result <- lm(y~x) #y: 종속변수, ~는 관계, x: 설명변수
summary(lse_result)
#Coefficients를 본다. 
#P값으로 모두 값이 의미한지, 아닌지 => P가 작을 수록 유의미하다 
# ***는 0.01보다 작은 것 
# 통상적으로 0.05를 기준으로 0.05보다 작으면 유의미하다고 판단한다. 
