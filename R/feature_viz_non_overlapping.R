library(M4comp2018)
library(tsfeatures)
library(fpp2)
library(purrr)
library(tidyverse)
library(patchwork)
library(TTR)
library(ggridges)
library(gghalves)
library(tsibble)
library(fabletools)
library(fable)
library(echos)

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

feature <- c("stl_features", "acf_features","pacf_features","heterogeneity","arch_stat",
             "hurst","entropy",
             "mean", "var","flat_spots","crossing_points","stability",
             "nonlinearity", "lumpiness",
             "unitroot_kpss", "unitroot_pp",
             "max_level_shift","max_var_shift")

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
features <- bind_cols(
  tsfeatures(M4series,
             c("acf_features","pacf_features","heterogeneity","entropy","lumpiness","arch_stat",
               "flat_spots","crossing_points","hurst","stability","nonlinearity", "unitroot_kpss", "unitroot_pp")),
  tsfeatures(M4series,"stl_features", s.window='periodic', robust=TRUE),
  tsfeatures(M4series,c("mean","var"), scale=FALSE, na.rm=TRUE),
  tsfeatures(M4series,
             c("max_level_shift","max_var_shift"), trim=TRUE))
  
features_selected <- features %>% select(mean, var,trend,seasonal_strength, entropy,      
lumpiness,flat_spots,crossing_points,nonlinearity,stability,
hurst,spike,linearity,curvature,peak,
trough,x_acf1,x_acf10,diff1_acf1,diff1_acf10,     
diff2_acf1,diff2_acf10,seas_acf1,x_pacf5,diff1x_pacf5,
diff2x_pacf5,seas_pacf,unitroot_kpss,unitroot_pp,arch_acf,
garch_acf,arch_r2,garch_r2,ARCH.LM,e_acf1,           
e_acf10,max_level_shift,time_level_shift,max_var_shift,time_var_shift)

cbind(agg_level=rep(1,nrow(features)),features)-> features_original
features_original <- features_original[ ,!(colnames(features_original) %in% c("seasonal_period","nperiods","max_kl_shift","time_kl_shift"))]
features_original <- features_original %>% mutate(cv=sqrt(var)/mean)
# f_final <- features_original %>% select(-agg_level)
# 
# f_final1<- f_final %>% select(mean, var,cv,trend,seasonal_strength, entropy,      
#            lumpiness,flat_spots,crossing_points,nonlinearity,stability,
#            hurst,spike,linearity,curvature,peak,
#            trough,x_acf1,x_acf10,diff1_acf1,diff1_acf10,     
#            diff2_acf1,diff2_acf10,seas_acf1,x_pacf5,diff1x_pacf5,
#            diff2x_pacf5,seas_pacf,unitroot_kpss,unitroot_pp,arch_acf,
#            garch_acf,arch_r2,garch_r2,ARCH.LM,e_acf1,           
#            e_acf10,max_level_shift,time_level_shift,max_var_shift,time_var_shift)
# 
# write_rds(f_final1, "feature_monthly.rds")
# f_final1 <- read_rds("feature_monthly.rds")

feature_original <- f_final1 %>% mutate(agg_level=rep(1,48000)) %>% 
  select(agg_level,mean, var,cv,trend,seasonal_strength, entropy,      
         lumpiness,flat_spots,crossing_points,nonlinearity,stability,
         hurst,spike,linearity,curvature,peak,
         trough,x_acf1,x_acf10,diff1_acf1,diff1_acf10,     
         diff2_acf1,diff2_acf10,seas_acf1,x_pacf5,diff1x_pacf5,
         diff2x_pacf5,seas_pacf,unitroot_kpss,unitroot_pp,arch_acf,
         garch_acf,arch_r2,garch_r2,ARCH.LM,e_acf1,           
         e_acf10,max_level_shift,time_level_shift,max_var_shift,time_var_shift)


#------- create categories 1=very high,....4=very low"-----

# my_feature <- function(features_original,feature) {
#   features_original %>%  select(agg_level, feature) %>% rename(feature=feature) %>% 
#     mutate(cat=case_when(feature<quantile(feature)[2] ~ 4, feature >= quantile(feature)[2] & feature < quantile(feature)[3] ~ 3,
#                          feature >= quantile(feature)[3] & feature < quantile(feature)[4] ~ 2, feature>=quantile(feature)[4] ~ 1)) -> cat_feature
#   names(cat_feature) <- c("agg_level",feature,"cat")
#   cat_feature
# }


#--- calculate aggregate features

#the following code should run for each m,1,2,3,4,5 separately from line 117 to 135
M4series_agg <- list()
for (i in 1:length(aag_level)) 
{
  M4series %>% map(~ ts(nonOverlapping(dataVector=.,m=aag_level[i]), 
                        frequency = fq_original/aag_level[i])) ->M4series_agg[[i]]
}
#M4series_agg <- M4series_agg[[1]]
#M4series_agg <- M4series_agg[[2]]
# M4series_agg <- M4series_agg[[3]]
# M4series_agg <- M4series_agg[[4]]
M4series_agg <- M4series_agg[[5]]
tsfearure_agg <- bind_cols(
  tsfeatures(M4series_agg,
             c("acf_features","pacf_features","heterogeneity","entropy","lumpiness","arch_stat",
               "flat_spots","crossing_points","hurst","stability","nonlinearity", "unitroot_kpss", "unitroot_pp")),
  tsfeatures(M4series_agg,"stl_features", s.window='periodic', robust=TRUE),
  tsfeatures(M4series_agg,c("mean","var"), scale=FALSE, na.rm=TRUE),
  tsfeatures(M4series_agg,
             c("max_level_shift","max_var_shift"), trim=TRUE))

tsfearure_agg1 <- cbind(agg_level=rep(aag_level[1],nrow(tsfearure_agg)),tsfearure_agg)
tsfearure_agg2 <- cbind(agg_level=rep(aag_level[2],nrow(tsfearure_agg)),tsfearure_agg)
tsfearure_agg3 <- cbind(agg_level=rep(aag_level[3],nrow(tsfearure_agg)),tsfearure_agg)
tsfearure_agg4 <- cbind(agg_level=rep(aag_level[4],nrow(tsfearure_agg)),tsfearure_agg)
tsfearure_agg5 <- cbind(agg_level=rep(aag_level[5],nrow(tsfearure_agg)),tsfearure_agg)

tsfearure_agg14 <- rbind(tsfearure_agg1,tsfearure_agg2,tsfearure_agg3,tsfearure_agg4)
tsfearure_agg14ready <- tsfearure_agg14 %>% 
  select(agg_level,seasonal_strength,peak,trough,seas_acf1,seas_pacf, everything())

#we remove seasonal_period,nperiods because they are not very informative
tsfearure_agg2_6 <- as_tibble(tsfearure_agg14ready) %>% select(-c(seasonal_period,nperiods))

tsfearure_agg15 <- as_tibble(tsfearure_agg15)
tsfearure_agg2_12 <- tsfearure_agg2_6 %>% bind_rows(tsfearure_agg15) %>% mutate(cv=sqrt(var)/mean)
#this give us aggregated features for aggregation leve (2,3,4,6,12)
tsfearure_agg2_12 <- tsfearure_agg2_12 %>% select(agg_level,mean, var,cv,trend,seasonal_strength, entropy,      
                                                                                     lumpiness,flat_spots,crossing_points,nonlinearity,stability,
                                                                                     hurst,spike,linearity,curvature,peak,
                                                                                     trough,x_acf1,x_acf10,diff1_acf1,diff1_acf10,     
                                                                                     diff2_acf1,diff2_acf10,seas_acf1,x_pacf5,diff1x_pacf5,
                                                                                     diff2x_pacf5,seas_pacf,unitroot_kpss,unitroot_pp,arch_acf,
                                                                                     garch_acf,arch_r2,garch_r2,ARCH.LM,e_acf1,           
                                                                                     e_acf10,max_level_shift,time_level_shift,max_var_shift,time_var_shift)


#some features are missing at annual level, so they would not be calculated! we replace them by NA
tsfearure_agg15 <- tsfearure_agg5 %>% 
  mutate(seasonal_strength=rep(NA,N), peak=rep(NA,N), trough=rep(NA,N),seas_acf1=rep(NA,N),seas_pacf=rep(NA,N)) %>% 
  select(agg_level,seasonal_strength, peak, trough,seas_acf1,seas_pacf,
         everything())

tsfearure_agg_final <- rbind(tsfearure_agg14ready,tsfearure_agg15)

write_rds(tsfearure_agg15, "feature_annual.rds")

fm <- read_rds("feature_monthly.rds")
fa <- read_rds("feature_annual.rds")

#--- calculate each feature seperately for all series 

all_features <- function(features_original,tsfearure_agg2, feature,m) {

  features_original %>%  select(agg_level, feature) %>% rename(feature=feature) %>% 
    mutate(cat=case_when(feature<quantile(feature)[2] ~ 4, feature >= quantile(feature)[2] & feature < quantile(feature)[3] ~ 3,
                         feature >= quantile(feature)[3] & feature < quantile(feature)[4] ~ 2, feature>=quantile(feature)[4] ~ 1)) -> cat_feature
  
  names(cat_feature) <- c("agg_level",feature,"cat")
  
  tsfearure_agg2 %>% select(agg_level, feature) ->feature_agg
  cbind(feature_agg,cat=rep(cat_feature[["cat"]],m)) -> feature_cat_agg
  feature_m <- rbind(cat_feature,feature_cat_agg)
  # features_m12 <- as.data.frame(cbind(rep(aag_level[m],length(tsfearure_agg12[[feature]])), tsfearure_agg12[[feature]],cat_feature[["cat"]]))
  # names(features_m12) <- names(feature_m)
  # rbind(feature_m,features_m12) -> trend_all
  feature_m
}


feature <- c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
             "mean", "ARCH.LM", "cv" ,"stability","linearity",
             "max_level_shift","e_acf10","unitroot_kpss","diff2_acf1","var","entropy","e_acf1",
             "seas_acf1","diff1_acf10" ,"diff2_acf10","lumpiness", "max_var_shift","diff1x_pacf5","diff2x_pacf5","x_pacf5","diff1_acf1",
             "trend","arch_acf","garch_acf","arch_r2","garch_r2",
             "time_var_shift","time_level_shift","spike",
             "seasonal_strength","x_acf1","x_acf10",
             "hurst","crossing_points","flat_spots",
             "peak","trough")
write_csv(as_tibble(feature),"feature.csv")

f_ts_all <- list()
for (i in 1:length(feature)) {
  f_ts <- all_features(feature_original,tsfearure_agg2_12, feature[i],m)
  names(f_ts) <- c("agg_level","feature","cat")
  f_ts_all[[i]] <- f_ts
}
write_rds(f_ts_all,"m4monthly_feature_for_plots.rds")
myname <- feature
my_ggplot_fun <- function(data,myname) {
  ggplot(data = data, aes(x=factor(agg_level), y=Value,shape=factor(cat)))+
    geom_line(aes(group=factor(cat)), size=.1)+geom_point()+
    scale_x_discrete(breaks = c(1,2,3,4,6,12),labels=c("Monthly","Bi-monthly","Quarterly","4-monthly", "Semi-annual","Annual"))+
    labs(x="Aggregation level", title = paste( myname))+
    scale_shape_discrete(name = NULL, labels=c("Very high","High","Low","Very low"))
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
p1 <- p[[1]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p2 <- p[[2]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p3 <-  p[[3]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p4 <-  p[[4]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p5 <-  p[[5]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p6 <-  p[[6]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())

mp <- (p1 | p2 | p3) &theme(legend.position = "")
mpp <- (p4 | p5 | p6) &theme(legend.position = "")


#mp1 <- mp+plot_layout(guides = 'collect') &theme(legend.position = "bottom")
p7 <- p[[7]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p8 <- p[[8]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p9 <-  p[[9]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p10 <-  p[[10]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p11 <-  p[[11]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p12 <-  p[[12]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
mp1 <- (p7 | p8 | p9) &theme(legend.position = "")
mp1p <- (p10 | p11 | p12) &theme(legend.position = "")

p13<- p[[13]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p14 <- p[[14]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p15 <-  p[[15]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p16 <-  p[[16]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p17 <-  p[[17]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p18 <-  p[[18]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
mp2 <- (p13 | p14 | p15) &theme(legend.position = "")
mp2p <- (p16 | p17 | p18) &theme(legend.position = "")

p19<- p[[19]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p20 <- p[[20]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p21 <-  p[[21]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p22 <-  p[[22]] +theme_classic()+ theme(axis.text.x = element_text(angle = 90))
p23 <-  p[[23]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 90))
p24 <-  p[[24]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 90))
mp3 <- (p19 | p20 | p21) &theme(legend.position = "")
mp3p <- (p22 | p23 | p24) &theme(legend.position = "bottom")

my_plot <- (mp / mpp / mp1 /mp1p / mp2 /mp2p/ mp3 /mp3p) +plot_layout(guides = 'collect') &theme(legend.position = "bottom")

fig.asp <-  1.4
fig.width <- 7.5
fig.height <- fig.width*fig.asp
ggsave(filename="mp_category_all1.pdf",my_plot,
       width = fig.width, 
       height = fig.height ,
       dpi = 600, units="in")

#---------
p25 <- p[[25]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p26 <- p[[26]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p27 <-  p[[27]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p28 <-  p[[28]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p29 <-  p[[29]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p30 <-  p[[30]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())

mp_2p <- (p25 | p26 | p27) &theme(legend.position = "")
mpp_2p <- (p28 | p29 | p30) &theme(legend.position = "")


#mp1 <- mp+plot_layout(guides = 'collect') &theme(legend.position = "bottom")
p31<- p[[31]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p32 <- p[[32]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p33 <-  p[[33]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p34 <-  p[[34]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p35<-  p[[35]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p36<-  p[[36]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
mp1_2p <- (p31 | p32 | p33) &theme(legend.position = "")
mp1p_2p <- (p34 | p35 | p36) &theme(legend.position = "")

p37<- p[[37]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p38 <- p[[38]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p39 <-  p[[39]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p40 <-  p[[40]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p41 <-  p[[41]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
mp2_2p <- (p37 | p38 | p39) &theme(legend.position = "")
mp2p_2p <- (p40 | p41) &theme(legend.position = "")

p19<- p[[19]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p20 <- p[[20]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p21 <-  p[[21]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p22 <-  p[[22]] +theme_classic()+ theme(axis.text.x = element_text(angle = 90))
p23 <-  p[[23]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 90))
p24 <-  p[[24]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 90))
mp3_2p <- (p19 | p20 | p21) &theme(legend.position = "")
mp3p_2p <- (p22 | p23 | p24) &theme(legend.position = "bottom")

my_plot <- (mp_2p / mp_2p / mp1_2p /mp1p_2p / mp2_2p /mp2p_2p/ mp3_2p /mp3p_2p) +plot_layout(guides = 'collect') &theme(legend.position = "bottom")

fig.asp <-  1.4
fig.width <- 7.5
fig.height <- fig.width*fig.asp
ggsave(filename="mp_category_all2.pdf",my_plot,
       width = fig.width, 
       height = fig.height ,
       dpi = 600, units="in")
#----------------
p13 <- p[[22]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p14 <- p[[2]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p15 <-  p[[6]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p16 <-  p[[36]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p17 <-  p[[24]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p18 <-  p[[21]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
mp2 <- (p13 | p14 | p15 | p16 | p17 | p18) &theme(legend.position = "")

mp2 <- (p13 | p14 | p15) &theme(legend.position = "")
mp2p <- (p16 | p17 | p18) &theme(legend.position = "")

p19 <- p[[23]] +theme_classic()+theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p20 <- p[[7]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_blank(),axis.title.x = element_blank())
p21 <-  p[[4]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_blank(),axis.title.x = element_blank())
p22 <-  p[[5]] +theme_classic()+ theme(axis.text.x = element_text(angle = 90))
p23 <-  p[[18]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 90))
p24 <-  p[[19]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 90))

#mp3 <- (p19 | p20 | p21 | p22 | p23 | p24) +plot_layout(guides = 'collect') &theme(legend.position = "bottom")

mp3 <- (p19 | p20 | p21) &theme(legend.position = "")
mp3p <- (p22 | p23 | p24) &theme(legend.position = "bottom")

my_plot <- (mp2 / mp2p /mp3 / mp3p) +plot_layout(guides = 'collect') &theme(legend.position = "bottom")

fig.asp <-  .98
fig.width <- 7.5
fig.height <- fig.width*fig.asp

ggsave(filename="mp_2_category.pdf",my_plot,
       width = fig.width, 
       height = fig.height ,
       dpi = 600, units="in")

my_plot+plot_annotation(title = "patchwork") &theme(legend.position = "top")



ggsave("feature.plot.png",my_plot)




##----- DISTRIBUTION

feature_m_boxplot<- rbind(features_original,tsfearure_agg2)
my_boxplot_fun <- function(data,myname) {
  ggplot(data = data, aes(x=factor(agg_level), y=Value))+
    geom_boxplot()+
    scale_x_discrete(breaks = c(1,2,3,4,6,12),labels=c("Monthly","2-Monthly","Quarterly","4-Monthly", "Biannual","Annual"))+
    labs(x="Aggregation level", title = paste("The effect of temporal aggregation on", myname)
         )
}

p <- list()
for (i in 1:length(feature)) {
  as.data.frame(feature_m_boxplot) %>% select(agg_level,myname[i]) %>% rename(Value=myname[i]) -> my_data
  p[[i]] <- my_boxplot_fun(my_data,myname[i])
}
my_plot <- (p[[1]] | p[[2]] | p[[3]]) /
  (p[[4]] | p[[5]] | p[[6]]) / 
  (p[[7]] | p[[8]] | p[[9]])/
  (p[[10]]|p[[11]])

my_plot+plot_annotation(title = "patchwork") &theme(legend.position = "top")


my_density_fun <- function(data,myname) {
  ggplot(data = data, aes(Value,after_stat(count),fill=factor(agg_level)))+
    geom_density(position = "fill")+
    labs(title = paste("The effect of temporal aggregation on", myname)
    )
}


my_density_fun <- function(data,myname) {
  ggplot(data = data, aes(x=Value,y=factor(agg_level),fill=factor(agg_level)))+
    geom_density_ridges()
    
}

p <- list()
for (i in 1:length(feature)) {
  as.data.frame(feature_m_boxplot) %>% select(agg_level,myname[i]) %>% rename(Value=myname[i]) -> my_data
  p[[i]] <- my_density_fun(my_data,myname[i])
}
my_plot <- (p[[1]] | p[[2]] | p[[3]]) /
  (p[[4]] | p[[5]] | p[[6]]) / 
  (p[[7]] | p[[8]] | p[[9]])/
  (p[[10]]|p[[11]])

my_plot+plot_annotation(title = "patchwork") &theme(legend.position = "top")



my_violine_fun <- function(data,myname) {
  ggplot(data = data, aes(x=factor(agg_level), y=Value))+
    geom_violin()+
    scale_x_discrete(breaks = c(1,2,3,4,6,12),labels=c("Monthly","2-Monthly","Quarterly","4-Monthly", "Biannual","Annual"))+
    labs(x="Aggregation level", title = paste("The effect of temporal aggregation on", myname)
    )
}

p <- list()
for (i in 1:length(feature)) {
  as.data.frame(feature_m_boxplot) %>% select(agg_level,myname[i]) %>% rename(Value=myname[i]) -> my_data
  p[[i]] <- my_violine_fun(my_data,myname[i])
}
my_plot <- (p[[1]] | p[[2]] | p[[3]]) /
  (p[[4]] | p[[5]] | p[[6]]) / 
  (p[[7]] | p[[8]] | p[[9]])/
  (p[[10]]|p[[11]])

my_plot+plot_annotation(title = "patchwork") &theme(legend.position = "top")

##-----------gghalve


my_violine_fun <- function(data,myname) {
  ggplot(data = data, aes(x=factor(agg_level), y=Value, colour=factor(agg_level)))+
    geom_half_point(side = "l", size=.3)+
    geom_half_boxplot(side = "l", width=.5, alpha=.3,nudge=.1)+
    geom_half_violin(aes(fill=factor(agg_level)), side = "r")+
    guides(fill=FALSE, colour=FALSE)+
    coord_flip()
}


p <- list()
for (i in 1:length(feature)) {
  as.data.frame(feature_m_boxplot) %>% select(agg_level,myname[i]) %>% rename(Value=myname[i]) -> my_data
  p[[i]] <- my_violine_fun(my_data,myname[i])
}
my_plot <- (p[[1]] | p[[2]] | p[[3]]) /
  (p[[4]] | p[[5]] | p[[6]]) / 
  (p[[7]] | p[[8]] | p[[9]])/
  (p[[10]]|p[[11]])

my_plot+plot_annotation(title = "patchwork") &theme(legend.position = "top")
