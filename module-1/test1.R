# Set seed for reproducibility
install.packages("data.table")
library(LKT)
set.seed(123)
val<-largerawsample
# Create a hypothetical dataset

# Define the number of folds
k <- 5

# Get unique student IDs
unique_students <- unique(val$Anon.Student.Id)

# Randomly shuffle the student IDs
shuffled_students <- sample(unique_students)

# Split student IDs into k folds
folds <- split(shuffled_students, cut(seq_along(shuffled_students), k, labels = FALSE))

# Function to train a simple model and calculate accuracy
train_and_evaluate <- function(train_data, test_data) {
  # Simple decision stump model (one-level decision tree)
  model <- rpart(Outcome ~ Feature1 + Feature2, data = train_data, control = rpart.control(maxdepth = 1))
  
  # Predict on test data
  predictions <- predict(model, test_data, type = "class")
  
  # Calculate accuracy
  accuracy <- mean(predictions == test_data$Outcome)
  return(accuracy)
}

# Perform k-fold cross-validation
accuracies <- numeric(k)

for (i in 1:k) {
  # Get test student IDs for this fold
  test_student_ids <- folds[[i]]
  
  # Create test and train datasets
  test_data <- subset(data, StudentID %in% test_student_ids)
  train_data <- subset(data, !(StudentID %in% test_student_ids))
  
  # Train the model and evaluate accuracy
  accuracies[i] <- train_and_evaluate(train_data, test_data)
}

# Calculate and print average accuracy across all folds
average_accuracy <- mean(accuracies)
print(paste("Average Accuracy:", round(average_accuracy, 3)))