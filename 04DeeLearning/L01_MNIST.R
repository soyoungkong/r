# L01_MNIST.R

# The MNIST dataset which contains 70,000 grayscale images in 10 categories, low resolution (28 by 28 pixels)
# https://yann.lecun.com/exdb/mnist/

# source : Deep Learning with R, 2nd Ed. 
#          Chapter 2. The Mathematical Building Blocks of Neural Networks


##### 0. Setup #####

tensorflow::as_tensor(1)

library(tensorflow)
library(keras)

##### 1. MNIST #####

### data load ### 
mnist <- dataset_mnist()

train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

str(train_images)
str(train_labels)
str(test_images)
str(test_labels)

train_images <- array_reshape(train_images, c(60000, 28 * 28)) #3차원을 1차원으로 만듦. 
train_images <- train_images / 255 #그래픽이므로 255로 나눈다. 
test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255

str(train_images)
str(train_labels)
str(test_images)
str(test_labels)

train_images[1,] #정수를 0~1사이로 바꿔서 한다. 
#이미지 분석은 무조건 0~1사이로 바꿔서 한다. 


### build the model ### 모형 제작 
#케라스는 쓰는 이유는 (텐서플로우는 상세하게 설정 가능) 텐서플로우 위에 있는 함수라서 
#텐서플로우를 좀 더 쉽게 쓰게 하기 위해서 한다. 
model <- keras_model_sequential(list(
  layer_dense(units = 512, activation = "relu"),
  layer_dense(units = 10, activation = "softmax") # 아웃풋. 0~9까지 숫자로 표현 
))

compile(model,
        optimizer = "rmsprop",
        loss = "sparse_categorical_crossentropy", #2개면 cross, 2개 이상이면 sparare.
        metrics = "accuracy") 

# accuracy : 목적은 정확하게 찾는 것임. 
### training : epochs(6만개의 데이터를 몇 번 학습할 것이냐) = 5 ###
# batch_size : 128개 데이터만 학습하고 가중치를 그 학습에 넣어주고. 크게 할 수록 속도가 빨라진다. 속도나 빠르나 정밀도가 떨어짐 
# epoch이 제일 중요함. 
fit(model, train_images, train_labels, epochs = 5, batch_size = 128)

### test ###

# predict specific data
test_digits <- test_images[1:10, ]
predictions <- predict(model, test_digits)
str(predictions)
predictions[1, ]

which.max(predictions[1, ])
predictions[1, 8]

test_labels[1]

# test all data 
metrics <- evaluate(model, test_images, test_labels)
metrics["accuracy"]

##### End of Document #####
