

model <- keras_model_sequential() %>% 
  layer_dense(units = 80, activation = "relu", input_shape = c(42)) %>%
  layer_dropout(rate = 0.6) %>%
  layer_dense(units = 40, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 5, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

val_indices <- 1:5000
x_val <- X_train[val_indices,]
partial_x_train <- X_train[-val_indices,]
 
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)


history <- model %>% fit(
   partial_x_train,
   partial_y_train,
   epochs=30,
 # batch_size=5000,
   validation_data = list(x_val, y_val)
 )

history$params
str(history)
history$metrics$accuracy

plot(history , smooth = FALSE)


which.max(history$metrics$val_accuracy)



model %>% fit(
  X_train,
  y_train,
  epochs=which.max(history$metrics$val_accuracy),
  #batch_size=1000
)  

results <- model %>% evaluate(X_test, y_test)
results


pred1 <- predict(model, X_test)
# model %>% predict(X_test) 
##########

# RNN

x_train <- X_train
y_train 
x_test <- X_test
y_test

# Define the model architecture
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 64, input_shape = c(42, 1)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile the model
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

# Train the model
history <- model %>% fit(
  x_train, y_train,
  epochs = 50,
  batch_size = 32,
  validation_data = list(x_test, y_test)
)

# Evaluate the model on the testing set
scores <- model %>% evaluate(x_test, y_test, verbose = 0)
cat("Accuracy: ", scores[2], "\n")

# Plot the training history
plot(history)

predy <- predict(model, X_test) > 0.5
mean(abs(y_test == as.numeric(predy)))


predrnn <- predict(model, X_test)
###
# Convolution Neural Networks

model <- keras_model_sequential()
model %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(42, 1)) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile the model
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

# Train the model
history <- model %>% fit(
  x_train, y_train,
  epochs = 50,
  batch_size = 32,
  validation_data = list(x_test, y_test)
)

# Evaluate the model on the testing set
scores <- model %>% evaluate(x_test, y_test, verbose = 0)
cat("Accuracy: ", scores[2], "\n")

# Plot the training history
plot(history)

predCNN <- predict(model, X_test)


####Evaluation

cut_nn <- cutoff_nn(pred1)

i<-cut_nn
pred1=ifelse(pred1>i,"TA","Direct")
t<-table(t.pred,ClassTable[-train,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP
1-mean(t.pred==ClassTable[-train,"marks"])

pred1 <- as.factor(pred1)
(keras1 <- c(classificationMetrics(test.data[,"marks"],pred1,posClass = "TA"),AUC=roc.curve(test.data[,"marks"],pred1,FALSE)$auc,classificationMetrics(test.data[,"marks"],pred1,posClass = "TA","totU",cbM)))


cut_nn <- cutoff_nn(predrnn)

i<-cut_nn
predrnn=ifelse(predrnn>i,"TA","Direct")
t<-table(t.pred,ClassTable[-train,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP
1-mean(t.pred==ClassTable[-train,"marks"])

predrnn <- as.factor(predrnn)
(RNN <- c(classificationMetrics(test.data[,"marks"],predrnn,posClass = "TA"),AUC=roc.curve(test.data[,"marks"],predrnn,FALSE)$auc,classificationMetrics(test.data[,"marks"],predrnn,posClass = "TA","totU",cbM)))


cut_nn <- cutoff_nn(predCNN)

i<-cut_nn
predCNN=ifelse(predCNN>i,"TA","Direct")
t<-table(t.pred,ClassTable[-train,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP
1-mean(t.pred==ClassTable[-train,"marks"])

predCNN <- as.factor(predCNN)
(CNN <- c(classificationMetrics(test.data[,"marks"],predCNN,posClass = "TA"),AUC=roc.curve(test.data[,"marks"],predCNN,FALSE)$auc,classificationMetrics(test.data[,"marks"],predCNN,posClass = "TA","totU",cbM)))


