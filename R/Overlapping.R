library(M4comp2018)
library(tsfeatures)
library(fpp2)
library(purrr)
library(tidyverse)
library(patchwork)
library(TTR)
library(ggpubr)

# this is function to calculate the non-overlpping form the last observation--------
Overlapping_end <- function(dataVector,m) 
{
  data<- dataVector[length(dataVector):1]
  aggregate<- SMA(data,n=m)*m
  aggregate1<- aggregate[length(aggregate):1]
  OverAggregare <- aggregate1[!is.na(aggregate1)]
  return(OverAggregare)
}

nonOverlapping <- function(dataVector,m=2) {
  data<- dataVector[length(dataVector):1]
  data<- data[1:(m*floor(length(data)/m))]
  Aggvector<- matrix(data, nrow = m)
  aggregated<- colSums (Aggvector, na.rm = FALSE, dims = 1)
  aggregatedDemand<-aggregated[length(aggregated):1]
  return(aggregatedDemand)
}

feature <- c("trend", "spike", "linearity", "curvature", "entropy", "x_acf1","x_acf10","diff1_acf1","diff1_acf10","seasonal_strength", "peak", "trough", "seas_acf1")
aag_level <- c(2,3,4,6,12)
m <- length(aag_level)
Monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)

#N <- 10
N <- length(Monthly_M4)
M4series<-list(rep(NA,N))
imena<- sector <- c(rep(NA,N))
for(k in(1:N)){
  M4series[[k]]<-Monthly_M4[[k]]$x
  imena[k]<-Monthly_M4[[k]]$st
  sector[k]<-Monthly_M4[[k]]$type
}

fq_original <- frequency(M4series[[1]])
names(M4series)<-imena
features<-tsfeatures(M4series)

cbind(agg_level=rep(1,nrow(features)),features)-> features_original
#------- create categories 1=very high,....4=very low"-----

my_feature <- function(features_original,feature) {
  features_original %>%  select(agg_level, feature) %>% rename(feature=feature) %>% 
    mutate(cat=case_when(feature<quantile(feature)[2] ~ 4, feature >= quantile(feature)[2] & feature <= quantile(feature)[3] ~ 3,
                         feature >= quantile(feature)[3] & feature <= quantile(feature)[4] ~ 2, feature>quantile(feature)[4] ~ 1)) -> cat_feature
  names(cat_feature) <- c("agg_level",feature,"cat")
  cat_feature
}


#--- calculate aggregate features



M4series_agg <- list()
for (i in 1:length(aag_level)) 
{
  M4series %>% map(~ ts(Overlapping_end(dataVector=.,m=aag_level[i]), frequency = fq_original/aag_level[i])) ->M4series_agg[[i]]
}
M4series_agg %>% map(~tsfeatures(.)) -> tsfearure_agg
tsfearure_agg1 <- list()
for (i in 1:length(tsfearure_agg)) {
  cbind(agg_level=rep(aag_level[i],nrow(tsfearure_agg[[i]])),tsfearure_agg[[i]]) -> tsfearure_agg1[[i]]
}


tsfearure_agg12 <- tsfearure_agg1[[length(aag_level)]] %>% mutate(seasonal_strength=rep(NA,N), peak=rep(NA,N), trough=rep(NA,N),seas_acf1=rep(NA,N)) %>% select(agg_level:e_acf10,seasonal_strength, peak, trough, everything())
tsfearure_agg2 <- tsfearure_agg1[[1]]
for (i in 2:(length(tsfearure_agg1)-1)) {
  tsfearure_agg2 <- rbind(tsfearure_agg2,tsfearure_agg1[[i]])
}
tsfearure_agg2 <- rbind(tsfearure_agg2,tsfearure_agg12)

#--- calculate each feature seperately for all series 

all_features <- function(features_original,tsfearure_agg2, feature,m) {

  features_original %>%  select(agg_level, feature) %>% rename(feature=feature) %>% 
    mutate(cat=case_when(feature<quantile(feature)[2] ~ 4, feature >= quantile(feature)[2] & feature <= quantile(feature)[3] ~ 3,
                         feature >= quantile(feature)[3] & feature <= quantile(feature)[4] ~ 2, feature>quantile(feature)[4] ~ 1)) -> cat_feature
  
  names(cat_feature) <- c("agg_level",feature,"cat")
  
  tsfearure_agg2 %>% select(agg_level, feature) ->feature_agg
  cbind(feature_agg,cat=rep(cat_feature[["cat"]],m)) -> feature_cat_agg
  feature_m <- rbind(cat_feature,feature_cat_agg)
  # features_m12 <- as.data.frame(cbind(rep(aag_level[m],length(tsfearure_agg12[[feature]])), tsfearure_agg12[[feature]],cat_feature[["cat"]]))
  # names(features_m12) <- names(feature_m)
  # rbind(feature_m,features_m12) -> trend_all
  feature_m
}


f_ts_all <- list()
for (i in 1:length(feature)) {
  f_ts <- all_features(features_original,tsfearure_agg2, feature[i],m)
  names(f_ts) <- c("agg_level","feature","cat")
  f_ts_all[[i]] <- f_ts
}





myname <- feature
my_ggplot_fun <- function(data,myname) {
  ggplot(data = data, aes(x=factor(agg_level), y=Value))+
    geom_line(aes(group=factor(cat), colour=factor(cat)))+geom_point()+
    scale_x_discrete(breaks = c(1,2,3,4,6,12),labels=c("Monthly","2-Monthly","Quarterly","4-Monthly", "Biannual","Annual"))+labs(x="Aggregation level", title = paste("The effect of temporal aggregation on", myname))+
    scale_color_discrete(name = myname, labels=c("Very gigh","High","Low","Very low"))
}



p <- list()
for (i in 1:length(feature)) {
  as.data.frame(f_ts_all[i]) %>% group_by(cat,agg_level) %>% summarise(Value=mean(feature, na.rm = TRUE) )-> my_data
  p[[i]] <- my_ggplot_fun(my_data,myname[i])
}

# my_ggplot_fun1 <- function(data,myname) {
#   ggplot(data = data, aes(x=factor(agg_level), y=feature))+
#     geom_boxplot()
# }
# p <- list()
# for (i in 1:length(feature)) {
#   as.data.frame(f_ts_all[1]) -> my_data
#   
#   p[[i]] <- my_ggplot_fun1(my_data,myname[i])
# }

my_plot <- ggarrange(p[[1]], p[[2]] , p[[3]],p[[4]] , p[[5]] , p[[6]],p[[7]] , p[[8]], p[[9]], p[[10]], ncol=3, nrow=4, common.legend = TRUE, legend="bottom")

# my_plot <- (p[[1]] | p[[2]] | p[[3]]) /
#   (p[[4]] | p[[5]] | p[[6]]) / 
#   (p[[7]] | p[[8]] | p[[9]])/
#   p[[10]]
# 
# my_plot+plot_annotation(title = "patchwork") &theme(legend.position = "top")

ggsave("feature.plot.png",my_plot)



