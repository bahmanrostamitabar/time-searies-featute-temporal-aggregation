library("iml")

a <- 30000
X <- train.data1[1:a,][-which(names(train.data1) == "marks")]
y <- train.data1[1:a,]$marks
y <- as.factor(ifelse(y=="Direct","Aggregate data","Aggregate forecasts"))
rf2x <- rf2; rf2x$classes <- c("Aggregate data","Aggregate forecasts")

mod1 <- Predictor$new(rf2x, data = X, y = y, type = "prob")

mod2 <- Predictor$new(rf2x,
                      data = X, y = y == "Aggregate forecasts",
                      type = "prob", class = "Aggregate forecasts"
)

impo <- FeatureImp$new(mod2, loss = "ce")
plot(impo)

effs2 <- FeatureEffects$new(mod2,method = "pdp")
#plot(effs2)

#effs2$plot(features=impvar[1:6], ncol=2,nrow=3)

#the most common features for practitioners
#effs2$plot(features=c("seasonal_strength","trend","mean","var","CV","linearity","nonlinearity"), ncol=3)

#postoji i opcija da imas razlicite y skale, pogledaj ako ti bude trebalo detaljnije 
# https://cran.r-project.org/web/packages/iml/iml.pdf

#Ako zelis da se vide oba klasifikaciona tjemena
effs1 <- FeatureEffects$new(mod1,method = "pdp")
#effs1$plot(features=impvar[1:6], ncol=2,nrow=3)

png("Fig_pdp1.png",  width = 200, height = 120, units = 'mm', res = 300)

effs1$plot(features=impvar[1:6], ncol=2,nrow=3)

dev.off()

png("Fig_pdp2.png",  width = 200, height = 120, units = 'mm', res = 300)

effs2$plot(features=impvar[1:6], ncol=2,nrow=3)

dev.off()

png("Fig_pdp3.png",  width = 200, height = 120, units = 'mm', res = 300)

effs2$plot(features=c("seasonal_strength","trend","mean","var","CV","linearity","nonlinearity"), ncol=3)

dev.off()

######

#Fig pd3b obe strane
png("Fig_pdp3b.png",  width = 200, height = 120, units = 'mm', res = 300)

effs1$plot(features=c("seasonal_strength","trend","mean","var","CV","linearity"), ncol=2,nrow=3)

dev.off()

