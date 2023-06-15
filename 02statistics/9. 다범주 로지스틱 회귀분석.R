alligator <- read.csv("C:/workspace/github/r/02statistics/data/alligator.csv")
str(alligator)

#install.packages("VGAM")
library(VGAM)

all_fit <- vglm(choice ~ length, family = multinomial, data = alligator)
summary(all_fit)

all_fit@extra$colnames.y

## 예측 확률
# new data를 안하면 원래 있던 예측값에 대해서 표현됨. 
all_predict <- predict(all_fit, type = "response")[order(alligator$length),]
all_predict; # 길이에 따라서 순서가 바뀜 

x <- sort(alligator$length)
plot(x, all_predict[,1], type = "l", col = "red", ylim = c(0,1), xlab = "length", ylab = "probabilty") #어류
lines(x, all_predict[,2], col = "blue") #연체류일 확률 
lines(x, all_predict[,3], col = "black") # 기타
