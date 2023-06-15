## 변수선택
#install.packages("MASS")
library(MASS)
str(Boston)

#
lm_previus <- lm(medv~., data = Boston)
lm_update <- lm(medv~.-crim, data = Boston) #.: 종속변수를 제외한 모든 변수, -는 모든 것에서 crim만 제외 
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

#adj.r값이 크도록 계속 진행한다

# zn을 제외하고 회귀분석
lm_update <- lm(medv~.-zn, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

#indus 값을 제외하고 회귀분석
lm_update <- lm(medv~.-indus, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
#true가 나옴. 



lm_previous <- lm_update
lm_update <- lm(medv~.-indus-crim, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

lm_update <- lm(medv~.-indus-zn, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

lm_update <- lm(medv~.-indus-chas, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

lm_update <- lm(medv~.-indus-nox, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

lm_update <- lm(medv~.-indus-rm, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

# indus, age를 뺸 경우 
lm_update <- lm(medv~.-indus-age, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
# true가 나옴 

# 기본 모델로 지정 
lm_previous <- lm_update
lm_update <- lm(medv~.-indus-age-crim, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

lm_update <- lm(medv~.-indus-age-zn, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared

lm_update <- lm(medv~.-indus-age-chas, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
lm_update <- lm(medv~.-indus-age-nox, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
lm_update <- lm(medv~.-indus-age-rm, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
lm_update <- lm(medv~.-indus-age-dis, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
lm_update <- lm(medv~.-indus-age-rad, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
lm_update <- lm(medv~.-indus-age-tax, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
lm_update <- lm(medv~.-indus-age-ptratio, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
lm_update <- lm(medv~.-indus-age-black, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
lm_update <- lm(medv~.-indus-age-lstat, data = Boston)
summary(lm_previus)$adj.r.squared < summary(lm_update)$adj.r.squared
## [1] FALSE
summary(lm_previous)


# 하나씩 하면 힘드므로 step으로 진행 
boston_fm <-lm(medv~., data=Boston)
boston_rm <- step(boston_fm, direction="backward")
summary(boston_rm)
