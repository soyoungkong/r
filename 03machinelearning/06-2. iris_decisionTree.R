#iris 데이터 살펴보기
colnames(iris)
levels(iris$Species)

#install.packages("rpart.plot")
library(rpart) #rpart()함수 포함 패키지
library(rpart.plot) #rpart.plot()함수 포함패키지
train <- sample(1:150, 100) #무작위로 100개 추출 (학습데이터)
tree <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + 
                  Petal.Width, data=iris, subset =train, method = "class")
rpart.plot(tree)
summary(tree)
printcp(tree)
pruned_tree <- prune(tree, cp = 0.1) #cp =0.1로 가지치기
rpart.plot(pruned_tree)
predict(pruned_tree, iris[-train,], type = "class")

#정오분류표(confusion matrix) 작성
(tt <- table(iris$Species[-train], predict(pruned_tree, iris[-train,], type = "class")))
