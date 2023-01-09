library(torch)

# Deep learning with tensors and neural networks

#https://amiles.netlify.app/2020/10/simple-examples-with-torch.en-us/
#https://mlverse.github.io/torch/articles/loading-data.html
#https://mlverse.github.io/torch/articles/examples/dataset.html
#https://anderfernandez.com/en/blog/how-to-create-neural-networks-with-torch-in-r/
#https://anderfernandez.com/en/blog/how-to-create-neural-networks-with-torch-in-r/
#https://blogs.rstudio.com/ai/posts/2020-09-29-introducing-torch-for-r/


dataset <- ClassTable[,-1]
dataset$model_class <- as.numeric(dataset$marks=='TA')
dataset <- dataset[,-42]
View(dataset)
dim(dataset)
train_idx <- train

df_dataset <- dataset(
  name = "dataset",

  initialize = function(df, feature_variables, response_variable) {
    
    self$df <- df[, feature_variables]
    self$response_variable <- df[[response_variable]]
  },
  
  .getitem = function(index) {
    response <- torch_tensor(self$response_variable[index], dtype = torch_float())
    x <- torch_tensor(as.numeric(self$df[index,]))
    
    list(x = x, y = response)
  },
  
  .length = function() {
    length(self$response_variable)
  }
)

feature_var <- c(colnames(dataset)[1:41])

response <- 'model_class'

dataset_train <- df_dataset(dataset[train_idx,], 
                             feature_variables = feature_var, 
                             response_variable = response)

dataset_test <- df_dataset(dataset[-train_idx,],
                            feature_variables = feature_var, 
                            response_variable = response)

dataset_train$.getitem(100)

dl_train <- dataloader(dataset_train, shuffle = TRUE)
dl_test <-  dataloader(dataset_test)

net <- nn_module(
  "Net",
  initialize = function() {
    self$fc1 <- nn_linear(length(feature_var), 16)
    self$fc2 <- nn_linear(16, 8)
    self$fc3 <- nn_linear(8, 1)
  },
  forward = function(x) {
    x %>% 
      self$fc1() %>% 
      nnf_relu() %>% 
      self$fc2() %>% 
      nnf_relu() %>%
      self$fc3() 
  }
)

model <- net()

optimizer <- optim_adam(model$parameters)

for (epoch in 1:10) {
  
  l <- c()
  
  for (b in enumerate(dl_train)) {
    optimizer$zero_grad()
    output <- model(b[[1]])
    loss <- nnf_binary_cross_entropy_with_logits(output,b[[2]])
    loss$backward()
    optimizer$step()
    l <- c(l, loss$item())
  }
  
  cat(sprintf("Loss at epoch %d: %3f\n", epoch, mean(l)))
}

model$eval()


test_losses <- c()

for (b in enumerate(dl_test)) {
  output <- model(b[[1]])
  loss <- nnf_binary_cross_entropy_with_logits(output, b[[2]])
  test_losses <- c(test_losses, loss$item())
}

mean(test_losses)


# Placeholder vector for probabilities
out_log_odds = c()

for (b in enumerate(dl_test)) {
  
  # get log odds
  output <- model(b[[1]])
  
  # convert to df and append
  log_odds = output$data() %>% as.array() %>% .[,1]
  out_log_odds <- c(out_log_odds, log_odds)
  prob <-exp(out_log_odds)/(1+exp(out_log_odds))
}

head(out_log_odds)
head(prob);summary(prob)

# get initial class prediction from log odds and append
predicted <- as.numeric(prob>0.5)

head(predicted)
mean(predicted ==dataset[-train_idx,"model_class"])
####

# Best cut off
cut_nn <- cutoff(prob)

i<-cut_nn
t.pred=ifelse(prob>i,"TA","Direct")
t<-table(t.pred,ClassTable[-train,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP
1-mean(t.pred==ClassTable[-train,"marks"])

t.pred <- as.factor(t.pred)
(Teval <- c(classificationMetrics(test.data[,"marks"],t.pred,posClass = "TA"),AUC=roc.curve(test.data[,"marks"],t.pred,FALSE)$auc,classificationMetrics(test.data[,"marks"],t.pred,posClass = "TA","totU",cbM)))
