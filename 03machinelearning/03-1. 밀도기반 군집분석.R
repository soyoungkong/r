iris2 <- iris[-5]
#install.packages("dbscan")
ds <- dbscan(iris2, eps=0.42, MinPts = 5)
table(ds$cluster, iris$Species)

plot(ds, iris2)
plot(ds, iris2[c(1,4)])


#새로운 데이터를 입력할 경우 
set.seed(435) # 435는 아무 값이나 주어도 됨. 

#임의의 값을 추출하기 위한 seed를 설정
idx <- sample(1:nrow(iris), 10)


newData <- iris[idx, -5]

newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)

myPred <- predict(ds,iris2, newData)

#주어진 값을 클러스터링 결과에 대입해 예측 
plot(iris2[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)
table(myPred, iris$Species[idx])


