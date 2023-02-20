# Load the required packages
library(transformers)
library(datasets)

# Load the Iris dataset
data(iris)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Define the model architecture
model <- transformer_model(
  input_shape = ncol(train_data) - 1,
  output_shape = 3,
  num_heads = 4,
  num_layers = 4,
  hidden_dim = 32,
  dropout = 0.2,
  activation = "gelu"
)

# Define the loss function and optimizer
loss_fn <- keras::sparse_categorical_crossentropy
optimizer <- keras::optimizer_adam(lr = 0.001)

# Compile the model
model %>% compile(
  loss = loss_fn,
  optimizer = optimizer,
  metrics = c("accuracy")
)

# Train the model
history <- model %>% fit(
  x = train_data[, -5],
  y = as.numeric(train_data[, 5]) - 1,
  epochs = 50,
  batch_size = 8,
  validation_split = 0.2,
  verbose = 0
)

# Evaluate the model on the test data
results <- model %>% evaluate(
  x = test_data[, -5],
  y = as.numeric(test_data[, 5]) - 1
)

# Print the test accuracy
cat("Test accuracy:", results$accuracy, "\n")
