#install.packages("keras")
library(keras)
#----------------------------------
# 환경변수 못 잡는 경우 : 
# 내 PC - 속성 - 고급 시스템 설정 - 환경변수 - 시스템 변수 쪽에 변수명 conda, 경로는 C에 User - 사용자명 - anaconda3
#----------------------------------
fashion_mnist <- keras::dataset_fashion_mnist()

#fashion_mnist$train$x[1,,]
#fashion_mnist$train$y[1]
#fashion_mnist$train$y[1:10]

train_x <- fashion_mnist$train$x %>%array_reshape(c(60000,28*28)) #6만장, 
train_x <- train_x/255 #최소값 0, 최대값 255로 만들어줌. 
train_y <- fashion_mnist$train$y %>%to_categorical() #y는 reshape 과정이 필요없음. 

test_x <- fashion_mnist$test$x %>%array_reshape(c(10000,28*28))
test_x <- test_x/255
test_y <- fashion_mnist$test$y %>%to_categorical()


par(mfrow=c(3,5), mar=c(0,0,0,0), mai=c(0,0,0,0))

for(I in 1:15){
  train_image_tmp <- 
    as.raster(fashion_mnist$train$x[I,,], 
              max=255)
  plot(train_image_tmp)
}

dev.off()



# 모델 정의 
#모델 시작
Model_classification <- keras_model_sequential()
Model_classification %>% 
  layer_dense(units=256,
              input_shape=c(784),
              activation="relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units=128,
              activation="relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(units=10,
              activation="softmax")

summary(Model_classification)



Model_classification %>% compile(
  loss='categorical_crossentropy',
  optimizer=optimizer_rmsprop(),
  metrics='accuracy'
)

hitory <- Model_classification %>% fit(
  train_x, train_y,
  batch_size=128,
  epochs=30,
  validation_data = list(test_x, test_y)
)

Model_classification %>% evaluate(test_x, test_y)

y_hat <- Model_classification %>% 
  predict(test_x) %>%
  apply(1, which.max)

res <- table(fashion_mnist$test$y, y_hat)
dimnames(res) <- list(c(0:9), c(0:9))
names(dimnames(res)) <- c("True", "Predicted")
print(res)
