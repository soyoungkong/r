# Load the required package
library(e1071)

# Load the iris dataset
data(iris)

# Split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))  # 70% for training
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Create the Naive Bayes classifier
nb_model <- naiveBayes(Species ~ ., data = train_data)

# Predict the class labels for the test dataset
predicted_class <- predict(nb_model, newdata = test_data)

# Compare the predicted and actual classes
accuracy <- sum(predicted_class == test_data$Species) / nrow(test_data)
cat("Accuracy:", accuracy * 100, "%")

# output results
# Load the required package
library(e1071)

# Load the iris dataset
data(iris)

# Split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))  # 70% for training
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Create the Naive Bayes classifier
nb_model <- naiveBayes(Species ~ ., data = train_data)

# View the actual data
cat("Actual Data:\n")
print(test_data)

# Predict the class labels for the test dataset
predicted_class <- predict(nb_model, newdata = test_data)

# Add the predicted class labels to the test dataset
test_data$Predicted_Species <- predicted_class

# View the predicted data
cat("\nPredicted Data:\n")
print(test_data)

# Load the required package
library(e1071)

# Create the weather dataset
weather <- data.frame(
  Weather = c("Sunny", "Sunny", "Cloudy", "Rainy", "Rainy", "Rainy", "Cloudy", "Sunny", "Sunny", "Rainy", "Sunny", "Cloudy", "Cloudy", "Rainy"),
  Temperature = c("Hot", "Hot", "Hot", "Moderate", "Cool", "Cool", "Cool", "Moderate", "Cool", "Moderate", "Moderate", "Moderate", "Hot", "Moderate"),
  Humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal", "High", "Normal", "Normal", "Normal", "High", "Normal", "High"),
  Wind = c("Weak", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong"),
  Exercise = c("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")
)

# Create the Naive Bayes classifier
nb_model <- naiveBayes(Exercise ~ ., data = weather)

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
cat("Predicted Exercise:", predicted_class)

#diabetes
# Load the required package
library(e1071)

# Load the diabetes dataset (assuming it is already available in R)
data(diabetes)

# Split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(diabetes), 0.7 * nrow(diabetes))  # 70% for training
train_data <- diabetes[train_indices, ]
test_data <- diabetes[-train_indices, ]

# Create the Naive Bayes classifier
nb_model <- naiveBayes(Outcome ~ ., data = train_data)

# Predict the class labels for the test dataset
predicted_class <- predict(nb_model, newdata = test_data)

# Compare the predicted and actual classes
accuracy <- sum(predicted_class == test_data$Outcome) / nrow(test_data)
cat("Accuracy:", accuracy * 100, "%")

library(mlbench)
data(diabetes)


