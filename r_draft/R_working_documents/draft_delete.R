
library(gbm)
set.seed(1)
library(MASS)
library(ISLR)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston ,subset=train)
boost.boston=gbm(medv~.,data=Boston,distribution="gaussian",n.trees=5000,interaction.depth=4)
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")
summary(boost.boston)
library(randomForest)
rf.boston=randomForest(medv~.,data=Boston ,subset=train,mtry=6,importance =TRUE)
importance(rf.boston)
varImpPlot(rf.boston)
fit.best <- rf.boston
fit.best

imp <- importance(rf.boston)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 2))
for (i in 1:4) {
partialPlot(rf.boston, Boston, impvar[i], xlab=impvar[i], ylab = "Partial Dependence",
main=paste("Partial Dependence on", impvar[i]))
}

library(plotmo)

plotmo(rf.boston, all1 = TRUE,all2=TRUE)#Ako zelis sve odjednom
plotmo(rf.boston,pmethod="partdep")# a ovjde on valjda po difoltu bira 10 tak najboljih

plotmo(rf.boston,pmethod="partdep",degree1=c(1,3,4))

plotmo(rf.boston,pmethod="partdep",degree1=c(1))
plotmo(rf.boston,pmethod="partdep",degree1=c(1),degree2 = c(1))

plotmo(rf.boston,pmethod="partdep",degree1=c(0),degree2 = c(1))


rf2=randomForest(marks~.,data=train.data,mtry=2,ntree=10);rf2
plotmo(rf2,pmethod="partdep",degree1=c(1,3,4))


library(earth) # for ozone1 data
data(ozone1)
oz <- ozone1[, c("O3", "humidity", "temp", "ibt")]

library(randomForest) 
rf.mod <- randomForest(O3 ~ ., data=oz)
plotmo(rf.mod, pt.col=2,smooth.col=2,grid.col="red")
plotmo(rf.mod, persp.ticktype="detailed", persp.nticks=3,col.persp="cyan")
partialPlot(rf.mod, oz, temp) # ovo je za samo pojedinacne slike koje zelis da vidis.
