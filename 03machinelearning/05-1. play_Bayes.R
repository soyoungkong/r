# Load the required package
library(e1071)

# Create the weather dataset
weather <- data.frame(
  Weather = c("Sunny", "Sunny", "Cloudy", "Rainy", "Rainy", "Rainy", "Cloudy", "Sunny", "Sunny", "Rainy", "Sunny", "Cloudy", "Cloudy", "Rainy"),
  Temperature = c("Hot", "Hot", "Hot", "Moderate", "Cool", "Cool", "Cool", "Moderate", "Cool", "Moderate", "Moderate", "Moderate", "Hot", "Moderate"),
  Humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal", "High", "Normal", "Normal", "Normal", "High", "Normal", "High"),
  Wind = c("Weak", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong"),
  play = c("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")
)

# Create the Naive Bayes classifier
nb_model <- naiveBayes(play ~ ., data = weather)

# Create a new test instance
new_instance <- data.frame(
  Weather = "Sunny",
  Temperature = "Hot",
  Humidity = "High",
  Wind = "Weak"
)

# Predict the class label for the new instance
predicted_class <- predict(nb_model, newdata = new_instance)

# Print the predicted class label
cat("Predicted play:", predicted_class)

weather_test <- data.frame(
  Weather = c("Sunny", "Sunny", "Cloudy", "Rainy", "Rainy"),
  Temperature = c("Cool", "Cool", "Moderate", "Cool", "Moderate"),
  Humidity = c("Normal", "Normal", "Normal", "High", "Normal"),
  Wind = c("Weak", "Strong", "Weak", "Weak", "Weak"),
  play = c( "Yes", "Yes", "Yes", "No","No"))

predicted_class <- predict(nb_model, newdata = weather_test)

 #실제값과 예측값을 비교
table(predicted_class, weather_test$play) #5개중에서 2개를 맞춤. 60% 정확도

