library(tidyverse)

# visualise monthly M4 data features
feature_monthly <- read_rds("feature_monthly.rds")
fm <- as_tibble(feature_monthly)

fm_lomg <- fm %>% pivot_longer(cols = 1:ncol(fm), names_to = "featute", values_to = "value")

all <- c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
                "mean", "ARCH.LM", "cv" ,"stability","linearity",
                "max_level_shift","e_acf10","unitroot_kpss","diff2_acf1","var","entropy","e_acf1",
                "seas_acf1","diff1_acf10" ,"diff2_acf10","lumpiness", "max_var_shift","diff1x_pacf5","diff2x_pacf5","x_pacf5","diff1_acf1",
                "trend","arch_acf","garch_acf","arch_r2","garch_r2",
                "time_var_shift","time_level_shift","spike",
                "seasonal_strength","x_acf1","x_acf10",
                "hurst","crossing_points","flat_spots",
                "peak","trough")


selection1 <- c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
                "mean", "ARCH.LM", "cv" ,"stability","linearity",
                "max_level_shift","e_acf10","unitroot_kpss","diff2_acf1","var","entropy","e_acf1",
                "seas_acf1","diff1_acf10" ,"diff2_acf10","lumpiness")

selection2 <- c("max_var_shift","diff1x_pacf5","diff2x_pacf5","x_pacf5","diff1_acf1",
           "trend","arch_acf","garch_acf","arch_r2","garch_r2",
           "time_var_shift","time_level_shift","spike",
           "seasonal_strength","x_acf1","x_acf10",
           "hurst","crossing_points","flat_spots",
           "peak","trough")


fm_known <- fm_lomg %>% filter(featute %in% selection1)
fm_known$featute <- factor(fm_known$featute, levels = selection1)

p <- ggplot(fm_known, aes(x=value))+
  geom_freqpoly(bins = 50, size=.25)+
  geom_rug(size=.1)+
  facet_wrap(vars(featute), scales = "free", ncol = 4)+
 ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Value" , y="Number of series")
ggsave(filename="featurets1.png",p,
       width = 16, 
       height = 20 ,
       dpi = 300, units="cm")

p <- ggplot(fm_known, aes(value))+
  geom_freqpoly(bins = 50)+
  geom_rug(sides="b")+
  facet_wrap(vars(featute), scales = "free", ncol = 4)+
  ggthemes::theme_clean()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Featur's value" , y="Number of series")

