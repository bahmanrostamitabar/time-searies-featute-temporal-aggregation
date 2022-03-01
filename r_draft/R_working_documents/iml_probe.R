
https://cran.r-project.org/web/packages/iml/iml.pdf



data("Boston", package = "MASS")
library("randomForest")
rf <- randomForest(medv ~ ., data = Boston, ntree = 50)
mod <- Predictor$new(rf, data = Boston)
# Compute the accumulated local effects for the first feature
eff <- FeatureEffect$new(mod, feature = "rm", grid.size = 30)
eff$plot()

# Again, but this time with a partial dependence plot and ice curves
eff <- FeatureEffect$new(mod,
                         feature = "rm", method = "pdp+ice",
                         grid.size = 30
)
plot(eff)


# Since the result is a ggplot object, you can extend it:
library("ggplot2")
plot(eff) +
  # Adds a title
  ggtitle("Partial dependence") +
  # Adds original predictions
  
  geom_point(
    data = Boston, aes(y = mod$predict(Boston)[[1]], x = rm),
    color = "pink", size = 0.5
  )
# If you want to do your own thing, just extract the data:
eff.dat <- eff$results
head(eff.dat)
# You can also use the object to "predict" the marginal values.
eff$predict(Boston[1:3, ])
# Instead of the entire data.frame, you can also use feature values:
eff$predict(c(5, 6, 7))
# You can reuse the pdp object for other features:
eff$set.feature("lstat")
plot(eff)
# Only plotting the aggregated partial dependence:
eff <- FeatureEffect$new(mod, feature = "crim", method = "pdp")
eff$plot()
# Only plotting the individual conditional expectation:
eff <- FeatureEffect$new(mod, feature = "crim", method = "ice")
eff$plot()
# Accumulated local effects and partial dependence plots support up to two
# features:
eff <- FeatureEffect$new(mod, feature = c("crim", "lstat"))
plot(eff)
# FeatureEffect plots also works with multiclass classification
rf <- randomForest(Species ~ ., data = iris, ntree = 50)
mod <- Predictor$new(rf, data = iris, type = "prob")
# For some models we have to specify additional arguments for the predict
# function
plot(FeatureEffect$new(mod, feature = "Petal.Width"))
# FeatureEffect plots support up to two features:
eff <- FeatureEffect$new(mod, feature = c("Sepal.Length", "Petal.Length"))
eff$plot()
# show where the actual data lies
eff$plot(show.data = TRUE)
# For multiclass classification models, you can choose to only show one class:
mod <- Predictor$new(rf, data = iris, type = "prob", class = 1)
plot(FeatureEffect$new(mod, feature = "Sepal.Length"))


###
# We train a random forest on the Boston dataset:
library("rpart")
data("Boston", package = "MASS")
rf <- rpart(medv ~ ., data = Boston)
mod <- Predictor$new(rf, data = Boston)
# Compute the accumulated local effects for all features
eff <- FeatureEffects$new(mod)
eff$plot()
## Not run:
# Again, but this time with a partial dependence plot
eff <- FeatureEffects$new(mod, method = "pdp")
eff$plot()
# Only a subset of features
eff <- FeatureEffects$new(mod, features = c("nox", "crim"))
eff$plot()
# You can access each FeatureEffect individually
eff.nox <- eff$effects[["nox"]]
eff.nox$plot()
# FeatureEffects also works with multiclass classification
rf <- rpart(Species ~ ., data = iris)
mod <- Predictor$new(rf, data = iris, type = "prob")
FeatureEffects$new(mod)$plot(ncol = 2)
FeatureEffects$new(mod, method = "pdp")$plot(ncol = 2)