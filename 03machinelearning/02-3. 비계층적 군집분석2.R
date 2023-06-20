crime_k  <- read.csv("C:/workspace/github/r/03machinelearning/data/Crime.csv", row.names=2)

#최단 연결법 
hc <- hclust(dist(x)^2, method="single")
plot(hc1, hang=1, main="dendrogram : single")
hc1.result <- cutree(hc1, k=c1.num)

# K-means 
crime_k <- kmeans(x, centers=3) #군집수 
attributes(crime_k)
crime_k$cluster


# K-medoid
# PAM using pamk() : k의 값을 자동으로 지정 
# install.packages("fpc")
library(fpc) # pamk() 사용 
library(cluster) # pam() 사용 
iris2 <- iris[-5]
pamk.result <- pamk(iris2)
#생성된 클러스터 개수를 확인 
pamk.result$nc 
table(pamk.result$pamobject$clustering, iris$Species)

#한 화면에 두 개의 그래프를 표시 
layout(matrix(c(1,2), 1,2))
plot(pamk.result$pamobject)

#PAM Using pam() with k=3
pam.result <- pam(iris2,3)
table(pam.result$clustering, iris$Species)

#한 화면에 그래프가 하나만 나오도록 변경 
layout(matrix(1))
plot(pam.result)
