https://hastie.su.domains/ISLR2/keras-instructions.html


install.packages("ISLR2")

tryCatch(
  remove.packages(c("keras", "tensorflow", "reticulate")),
  error = function(e) "Some or all packages not previously installed, that's ok!"
)

install.packages("keras", repos = 'https://cloud.r-project.org')


write('RETICULATE_AUTOCONFIGURE=FALSE', file = "~/.Renviron", append = TRUE)
write(sprintf('RETICULATE_MINICONDA_PATH=%s',
              normalizePath("~/islr-miniconda", winslash = "/", mustWork = FALSE)),
      file = "~/.Renviron", append = TRUE)

Sys.setenv(RETICULATE_AUTOCONFIGURE='FALSE',
           RETICULATE_MINICONDA_PATH=normalizePath("~/islr-miniconda", winslash = "/", mustWork = FALSE))

source(system.file("helpers", "install.R", package = "ISLR2"))


reticulate::install_miniconda()

keras::install_keras()

install_tensorflow()

install_miniconda_and_tensorflow()
#################

https://github.com/rstudio/keras/issues/1311


# MNIST classification (muliticlass)

library(keras)

# devtools::install_github("rstudio/tensorflow")
library(tensorflow)

# install_tensorflow()

mnist <- dataset_mnist()

train_images <- mnist$train$x

digit <- train_images[5,,]
plot(as.raster(digit, max = 255))

train_images <- array_reshape(train_images, c(60000, 28*28))
train_images <- train_images/255
train_labels <- mnist$train$y

test_images <- mnist$test$x
test_images <- array_reshape(test_images, c(10000, 28*28))
test_images <- test_images/255
test_labels <- mnist$test$y

network <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(28*28)) %>% 
  layer_dense(units = 10, activation = "softmax")

network %>% compile(
  optimizer = "rmsprop",
  loss = "sparse_categorical_crossentropy",
  metrics = c("accuracy")
)

network %>% fit(train_images, train_labels, epochs = 30, batch_size = 128)

metrics <- network %>% evaluate(test_images, test_labels)

network %>% predict(test_images[1:10,]) %>% k_argmax()

res <- as.numeric(network %>% predict(test_images[1:10,]) %>% k_argmax())
res
test_labels[1:10]


#IMBD set 63 strana (binary classification problem)

imbd <- dataset_imdb(num_words = 10000)
train_data <- imbd$train$x
train_labels <- imbd$train$y
test_data <- imbd$test$x
test_labels <- imbd$test$y

vectorize_sequences <- function(sequences, dimension = 10000){
  
  results <- matrix(0, nrow=length(sequences), ncol = dimension)
  for(i in 1:length(sequences))
    
    results[i, sequences[[i]]] <- 1
  results
}

x_train <-vectorize_sequences(train_data) 
x_test <- vectorize_sequences(test_data)

x_train[1,]

y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)


model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

val_indices <- 1:10000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# history <- model %>% fit(
#   x_train,
#   y_train,
#   epochs=20,
#   batch_size=512
# )



history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs=20,
  batch_size=512,
  validation_data = list(x_val, y_val)
)

history$params
str(history)
history$metrics$accuracy


model %>% fit(
     x_train,
     y_train,
     epochs=4,
     batch_size=512
   )  

results <- model %>% evaluate(x_test, y_test)
results

# Reuters data set (classification to 46 groups) 74 strana

# Add the code in future

# Boston regression 81 strana

# U sustini iskopiraj ovaj primjer odozgo i primjeni ga na svoj skup

