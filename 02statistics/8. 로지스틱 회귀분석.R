#로지스틱 회귀분석
ucla <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(ucla)
#glm : generalized linear 
ucla_logit <- glm(admit ~ gre + gpa, data = ucla, family = "binomial") 
summary(ucla_logit)

## 예측 합격 확률
predict(ucla_logit, newdata = data.frame(gre = c(200, 600), gpa = 3.5), type = "response")
