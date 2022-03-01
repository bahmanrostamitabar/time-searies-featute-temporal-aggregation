imp <- importance(rf2)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(3, 3))
for (i in 1:9) {
  partialPlot(rf2, ClassTable, which.class = "TA", impvar[i], xlab=impvar[i],ylab="Partial Dependence", main = NULL)
}
par(op)


library(plotmo)
plotmo(rf2, pmethod="apartdep",type="prob",col.persp="cyan",degree1 = c(1,2,3,4),degree2 = c(1,2,3,4)) # plot first 4 partial dependencies and 4 interactions


1/(1+exp(0))

#IML

data("Boston", package = "MASS")
head(Boston)

set.seed(42)
library("iml")
library("randomForest")
data("Boston", package = "MASS")
rf <- randomForest(medv ~ ., data = Boston, ntree = 50)

X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)

imp <- FeatureImp$new(predictor, loss = "mae")
library("ggplot2")
plot(imp)

ale <- FeatureEffect$new(predictor, feature = "lstat")
ale$plot()

effs <- FeatureEffects$new(predictor)
plot(effs)

#Pretvaranje log oddsa u vjerovatnocu
a <- exp(-0.3)
a/(1+a)

#edarf


# FeatureImp also works with multiclass classification.
# In this case, the importance measurement regards all classes
library("rpart")
tree <- rpart(Species ~ ., data = iris)
X <- iris[-which(names(iris) == "Species")]
y <- iris$Species
mod <- Predictor$new(tree, data = X, y = y, type = "prob")

# For some models we have to specify additional arguments for the predict function
imp <- FeatureImp$new(mod, loss = "ce")
plot(imp)

# For multiclass classification models, you can choose to only compute
# performance for one class.
# Make sure to adapt y
mod <- Predictor$new(tree,
                     data = X, y = y == "virginica",
                     type = "prob", class = "virginica"
)
imp <- FeatureImp$new(mod, loss = "ce")
plot(imp)

ale2 <- FeatureEffect$new(mod, feature = "Sepal.Length")
ale2$plot()

effs <- FeatureEffects$new(mod)
plot(effs)

#Proba na svojim podacima
http://xai-tools.drwhy.ai/iml.html

a <- 1000
train.data1[1:a,]
rfx=randomForest(marks~.,data=train.data1[1:a,],mtry=10,ntree=10, importance=TRUE);rfx

X <- train.data1[1:a,][-which(names(train.data1) == "marks")]
y <- train.data1[1:a,]$marks

mod1 <- Predictor$new(rfx, data = X, y = y, type = "prob")

# For some models we have to specify additional arguments for the predict function
imp <- FeatureImp$new(mod, loss = "ce")
plot(imp)

# For multiclass classification models, you can choose to only compute
# performance for one class.
# Make sure to adapt y
mod2 <- Predictor$new(rfx,
                     data = X, y = y == "TA",
                     type = "prob", class = "TA"
)
imp <- FeatureImp$new(mod1, loss = "ce")
plot(imp)

ale2 <- FeatureEffect$new(mod1, feature = "curvature",method = "pdp")
ale2$plot()

effs <- FeatureEffects$new(mod2,method = "pdp")
plot(effs,ncol=5)
effs$plot(features=c("curvature","nonlinearity"))
effs$plot(features=impvar[1:8], ncol=2,nrow=4)
  

ale3 <- FeatureEffect$new(mod2, feature = c("curvature","nonlinearity"),method = "pdp")
ale3$plot(show.data = TRUE)

ale4 <- FeatureEffect$new(mod1, feature = c("curvature","nonlinearity"))
ale4$plot()
###


###pravljenje slika 300dpi
png("Fig xx.png",  width = 731, height = 495, units = 'mm', res = 300)

effs1 <- FeatureEffects$new(mod1,method = "pdp")
effs1$plot(features=impvar[1:6], ncol=2,nrow=3)

dev.off()

# Septembar 2021 iz plotmo paketa
library(randomForest) 
library(earth) # for ozone1 data
data(ozone1)
oz <- ozone1[, c("O3", "humidity", "temp", "ibt")]
rf.mod <- randomForest(O3 ~ ., data=oz)
plotmo(rf.mod)
plotmo(rf.mod, pmethod = "apartdep")# Vidis da su male razlike

partialPlot(rf.mod, oz, temp) # Mala razlika sem sto je skala ista
partialPlot(rf.mod, oz, ibt) # 



