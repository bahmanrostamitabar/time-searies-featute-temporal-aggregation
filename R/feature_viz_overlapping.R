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

cbind(agg_level=rep(1,48000),features)-> features_original
#features_original <- features_original[ ,!(colnames(features_original) %in% c("seasonal_period","nperiods","max_kl_shift","time_kl_shift"))]
features_original <- features_original %>% mutate(cv=sqrt(var)/mean)
features_original_ordered <- features_original %>% 
  select(agg_level,mean, var,cv,trend,seasonal_strength, entropy,      
         lumpiness,flat_spots,crossing_points,nonlinearity,stability,
         hurst,spike,linearity,curvature,peak,
         trough,x_acf1,x_acf10,diff1_acf1,diff1_acf10,     
         diff2_acf1,diff2_acf10,seas_acf1,x_pacf5,diff1x_pacf5,
         diff2x_pacf5,seas_pacf,unitroot_kpss,unitroot_pp,arch_acf,
         garch_acf,arch_r2,garch_r2,ARCH.LM,e_acf1,           
         e_acf10,max_level_shift,time_level_shift,max_var_shift,time_var_shift)
#write_rds(features_original_ordered,"data/features_original.rds")
features_original_ordered <- read_rds("data/features_original.rds")
#------- create categories 1=very high,....4=very low"-----

# my_feature <- function(features_original,feature) {
#   features_original %>%  select(agg_level, feature) %>% rename(feature=feature) %>% 
#     mutate(cat=case_when(feature<quantile(feature)[2] ~ 4, feature >= quantile(feature)[2] & feature < quantile(feature)[3] ~ 3,
#                          feature >= quantile(feature)[3] & feature < quantile(feature)[4] ~ 2, feature>=quantile(feature)[4] ~ 1)) -> cat_feature
#   names(cat_feature) <- c("agg_level",feature,"cat")
#   cat_feature
# }


#--- calculate aggregate features



M4series_agg <- list()
for (i in 1:length(aag_level)) 
{
  M4series %>% map(~ ts(Overlapping_end(dataVector=.,m=aag_level[i]), 
                        frequency = fq_original)) ->M4series_agg[[i]]
}


#M4series_agg1 <- M4series_agg[[1]]
#M4series_agg2 <- M4series_agg[[2]]
#M4series_agg3 <- M4series_agg[[3]]
#M4series_agg4 <- M4series_agg[[4]]
M4series_agg5 <- M4series_agg[[5]]

tsfearure_agg <- bind_cols(
  tsfeatures(M4series_agg5,
             c("acf_features","pacf_features","heterogeneity","entropy","lumpiness","arch_stat",
               "flat_spots","crossing_points","hurst","stability","nonlinearity", "unitroot_kpss", "unitroot_pp")),
  tsfeatures(M4series_agg5,"stl_features", s.window='periodic', robust=TRUE),
  tsfeatures(M4series_agg5,c("mean","var"), scale=FALSE, na.rm=TRUE),
  tsfeatures(M4series_agg5,
             c("max_level_shift","max_var_shift"), trim=TRUE))

# M4series_agg %>% map(~tsfeatures(.,c("acf_features","pacf_features","heterogeneity","entropy","lumpiness","arch_stat",
#                                      "flat_spots","crossing_points","hurst","stability","nonlinearity", "unitroot_kpss", "unitroot_pp"))) -> tsfearure_agg
#tsfearure_agg1 <- cbind(agg_level=rep(aag_level[1],nrow(tsfearure_agg)),tsfearure_agg)
#tsfearure_agg2 <- cbind(agg_level=rep(aag_level[2],nrow(tsfearure_agg)),tsfearure_agg)
#tsfearure_agg3 <- cbind(agg_level=rep(aag_level[3],nrow(tsfearure_agg)),tsfearure_agg)
#tsfearure_agg4 <- cbind(agg_level=rep(aag_level[4],nrow(tsfearure_agg)),tsfearure_agg)
tsfearure_agg5 <- cbind(agg_level=rep(aag_level[5],nrow(tsfearure_agg)),tsfearure_agg)

tsfearure_agg15 <- rbind(tsfearure_agg1,tsfearure_agg2,tsfearure_agg3,tsfearure_agg4,tsfearure_agg5)
tsfearure_agg_all <- tsfearure_agg15 %>% mutate(cv=sqrt(var)/mean)

tsfearure_annual_overlapping <- tsfearure_agg5 %>% mutate(cv=sqrt(var)/mean) %>%
  select(agg_level,mean, var,cv,trend,seasonal_strength, entropy,      
         lumpiness,flat_spots,crossing_points,nonlinearity,stability,
         hurst,spike,linearity,curvature,peak,
         trough,x_acf1,x_acf10,diff1_acf1,diff1_acf10,     
         diff2_acf1,diff2_acf10,seas_acf1,x_pacf5,diff1x_pacf5,
         diff2x_pacf5,seas_pacf,unitroot_kpss,unitroot_pp,arch_acf,
         garch_acf,arch_r2,garch_r2,ARCH.LM,e_acf1,           
         e_acf10,max_level_shift,time_level_shift,max_var_shift,time_var_shift)
write_rds(tsfearure_annual_overlapping,"data/tsfearure_annual_overlapping.rds")
tsfearure_agg_all_ordered <- tsfearure_agg_all %>% 
  select(agg_level,mean, var,cv,trend,seasonal_strength, entropy,      
         lumpiness,flat_spots,crossing_points,nonlinearity,stability,
         hurst,spike,linearity,curvature,peak,
         trough,x_acf1,x_acf10,diff1_acf1,diff1_acf10,     
         diff2_acf1,diff2_acf10,seas_acf1,x_pacf5,diff1x_pacf5,
         diff2x_pacf5,seas_pacf,unitroot_kpss,unitroot_pp,arch_acf,
         garch_acf,arch_r2,garch_r2,ARCH.LM,e_acf1,           
         e_acf10,max_level_shift,time_level_shift,max_var_shift,time_var_shift)

all_m4montjly_feature <- features_original_ordered %>% rbind(tsfearure_agg_all_ordered)  



agg12_f <- tsfearure_agg15123  %>%  select(mean, var,cv,trend,seasonal_strength, entropy,      
       lumpiness,flat_spots,crossing_points,nonlinearity,stability,
       hurst,spike,linearity,curvature,peak,
       trough,x_acf1,x_acf10,diff1_acf1,diff1_acf10,     
       diff2_acf1,diff2_acf10,seas_acf1,x_pacf5,diff1x_pacf5,
       diff2x_pacf5,seas_pacf,unitroot_kpss,unitroot_pp,arch_acf,
       garch_acf,arch_r2,garch_r2,ARCH.LM,e_acf1,           
       e_acf10,max_level_shift,time_level_shift,max_var_shift,time_var_shift)
write_rds(agg12_f, "feature_annual.rds")

fm <- read_rds("feature_monthly.rds")
fa <- read_rds("feature_annual.rds")

#--- calculate each feature seperately for all series 

all_features <- function(features_original_ordered,tsfearure_agg_all, feature,m) {
  
  features_original_ordered %>%  select(agg_level, feature) %>% rename(feature=feature) %>% 
    mutate(cat=case_when(feature<quantile(feature)[2] ~ 4, feature >= quantile(feature)[2] & feature < quantile(feature)[3] ~ 3,
                         feature >= quantile(feature)[3] & feature < quantile(feature)[4] ~ 2, feature>=quantile(feature)[4] ~ 1)) -> cat_feature
  
  names(cat_feature) <- c("agg_level",feature,"cat")
  
  tsfearure_agg_all %>% select(agg_level, feature) ->feature_agg
  cbind(feature_agg,cat=rep(cat_feature[["cat"]],m)) -> feature_cat_agg
  feature_m <- rbind(cat_feature,feature_cat_agg)
  # features_m12 <- as.data.frame(cbind(rep(aag_level[m],length(tsfearure_agg12[[feature]])), tsfearure_agg12[[feature]],cat_feature[["cat"]]))
  # names(features_m12) <- names(feature_m)
  # rbind(feature_m,features_m12) -> trend_all
  feature_m
}




f_ts_all <- list()
for (i in 1:length(feature)) {
  f_ts <- all_features(features_original_ordered,tsfearure_agg_all, feature[i],m)
  names(f_ts) <- c("agg_level","feature","cat")
  f_ts_all[[i]] <- f_ts
}

write_rds(f_ts_all,"data/feature_ts_all_overlapping.rds")
f_ts_all <- read_rds("data/feature_ts_all_overlapping.rds")

feature <- c("mean", "var","cv","trend","seasonal_strength", "entropy",      
             "lumpiness","flat_spots","crossing_points","nonlinearity","stability",
             "hurst","spike","linearity","curvature","peak",
             "trough","x_acf1","x_acf10","diff1_acf1","diff1_acf10",     
             "diff2_acf1","diff2_acf10","seas_acf1","x_pacf5","diff1x_pacf5",
             "diff2x_pacf5","seas_pacf","unitroot_kpss","unitroot_pp","arch_acf",
             "garch_acf","arch_r2","garch_r2","ARCH.LM","e_acf1",           
             "e_acf10","max_level_shift","time_level_shift","max_var_shift","time_var_shift")

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
  as.data.frame(f_ts_all[i]) %>% 
    group_by(cat,agg_level) %>% 
    summarise(Value=mean(feature, na.rm = TRUE) )-> my_data
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
p1 <- p[[15]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p2 <- p[[10]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p3 <-  p[[28]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p4 <-  p[[30]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p5 <-  p[[1]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p6 <-  p[[35]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())

mp <- (p1 | p2 | p3) &theme(legend.position = "")
mpp <- (p4 | p5 | p6) &theme(legend.position = "")


#mp1 <- mp+plot_layout(guides = 'collect') &theme(legend.position = "bottom")
p7 <- p[[3]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank())
p8 <- p[[11]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p9 <-  p[[14]] +theme_classic()+ theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
p10 <-  p[[38]] +theme_classic()+ theme(axis.text.x = element_text(angle = 90))
p11 <-  p[[37]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 90))
p12 <-  p[[29]] +theme_classic()+ theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 90))
mp1 <- (p7 | p8 | p9) &theme(legend.position = "")
mp1p <- (p10 | p11 | p12) &theme(legend.position = "bottom")



my_plot <- (mp / mpp / mp1 /mp1p) +plot_layout(guides = 'collect') &theme(legend.position = "bottom")

fig.asp <-  .98
fig.width <- 7.5
fig.height <- fig.width*fig.asp


ggsave(filename="overlapping_feature1.pdf",my_plot,
       width = fig.width, 
       height = fig.height ,
       dpi = 600, units="in")



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


ggsave(filename="overlapping_feature2.pdf",my_plot,
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
