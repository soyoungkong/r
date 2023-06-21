# Install and load the titanic package
#install.packages("titanic")
library(titanic)

# Load the titanic dataset
data(titanic_train)
head(titanic_train)
# Create a decision tree model
library(rpart)
decision_tree <- rpart(Survived ~ Pclass + Sex + Age, data = titanic_train, method = "class")

# Plot the decision tree
plot(decision_tree)
text(decision_tree, use.n = TRUE)

