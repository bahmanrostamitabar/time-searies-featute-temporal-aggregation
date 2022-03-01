library(tidyverse)
AD <- readr::read_rds("AD.rds") %>% as_tibble() %>% select(Annual) %>% rename(Annual_AD=Annual)
AF <- readr::read_rds("AF.rds") %>% as_tibble() %>% select(Annual) %>% rename(Annual_AF=Annual)

all <- c("Approach","curvature","nonlinearity","seas_pacf","unitroot_pp" ,
         "mean", "ARCH.LM", "cv" ,"stability","linearity",
         "max_level_shift","e_acf10","unitroot_kpss","diff2_acf1","var","entropy","e_acf1",
         "seas_acf1","diff1_acf10" ,"diff2_acf10","lumpiness", "max_var_shift","diff1x_pacf5","diff2x_pacf5","x_pacf5","diff1_acf1",
         "trend","arch_acf","garch_acf","arch_r2","garch_r2",
         "time_var_shift","time_level_shift","spike",
         "seasonal_strength","x_acf1","x_acf10",
         "hurst","crossing_points","flat_spots",
         "peak","trough")

rmsse <- AD %>% bind_cols(AF) %>% mutate(Approach= if_else(Annual_AD<Annual_AF, "AD","AF")) %>% select(Approach)

rmsse_feature <- rmsse %>% bind_cols(feature_monthly)
write_rds(rmsse_feature,"rmsse_feature.rds")
rmsse_feature <- read_rds("rmsse_feature.rds")

selection1 <- c("Approach","curvature","nonlinearity","seas_pacf","unitroot_pp" ,
                "mean", "ARCH.LM", "cv" ,"stability","linearity",
                "max_level_shift","e_acf10","unitroot_kpss","diff2_acf1","var","entropy","e_acf1",
                "seas_acf1","diff1_acf10" ,"diff2_acf10","lumpiness")
# 
# selection2 <- c("max_var_shift","diff1x_pacf5","diff2x_pacf5","x_pacf5","diff1_acf1",
#                 "trend","arch_acf","garch_acf","arch_r2","garch_r2",
#                 "time_var_shift","time_level_shift","spike",
#                 "seasonal_strength","x_acf1","x_acf10",
#                 "hurst","crossing_points","flat_spots",
#                 "peak","trough")
selection1 <- c("Approach","curvature","nonlinearity","seas_pacf","unitroot_pp" ,
           "mean", "ARCH.LM", "cv" ,"stability","linearity",
           "max_level_shift")
selection2 <- c("Approach","e_acf10", "unitroot_kpss","diff2_acf1",
  "var", "entropy", "e_acf1", "seas_acf1","diff1_acf10" ,"diff2_acf10","lumpiness")

first20 <- rmsse_feature %>% select(any_of(selection1))


all <-rmsse_feature %>%  select(all_of(all))

#
png(
  "pair.png",
  width     = 18,
  height    = 15,
  units     = "cm",
  res       = 120,
  pointsize = 10
)

pairs(first20[, 2:21],
      col=ifelse(all$Approach=="AD", "#000000", "#E69F00"),
      pch = 19,                                                
      cex = 0.2,                                               
      labels = c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
                 "mean", "ARCH.LM", "cv" ,"stability","linearity",
                 "max_level_shift","e_acf10","unitroot_kpss","diff2_acf1","var","entropy","e_acf1",
                 "seas_acf1","diff1_acf10" ,"diff2_acf10","lumpiness"),  
      gap = 0.3,
      upper.panel = NULL               # Turn off the upper panel above the diagonal
)

dev.off()

library(ggplotify)
p <- as.ggplot(function() pairs(first20[, 2:11],
                                col=ifelse(first20$Approach=="AD", "#000000", "#E69F00"),
                                pch = 19,                                                
                                cex = 0.6,                                               
                                labels = c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
                                           "mean", "ARCH.LM", "cv" ,"stability","linearity",
                                           "max_level_shift"),  
                                gap = 0.3,
                                upper.panel = NULL               # Turn off the upper panel above the diagonal
))

p1 <- p+scale_color_manual("Method",values = c(AD = "#000000",AF="#E69F00"),
                           labels = c(AD = "AD",AF="AF"))+
  theme(legend.position = "bottom")+
  ggthemes::theme_few()


ggsave(filename="pairplot.png",p1,
       width = 20, 
       height = 20 ,
       dpi = 600, units="cm")
pdf("rplot.pdf",
    width = 550, 
    height = 550) 
# 2. Create a plot
pairs(first20[, 2:11],
      col=ifelse(first20$Approach=="AD", "#000000", "#E69F00"),
      pch = 19,                                                
      cex = 0.8,                                               
      labels = c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
                 "mean", "ARCH.LM", "cv" ,"stability","linearity",
                 "max_level_shift"),  
      gap = 0.3,
      upper.panel = NULL               # Turn off the upper panel above the diagonal
)
# Close the pdf file
dev.off()

library(GGally)
first20$Approach <- factor(first20$Approach)
p <- ggpairs(first20[, 2:11],
        columnLabels = c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
                         "mean", "ARCH.LM", "cv" ,"stability","linearity",
                         "max_level_shift"),
        aes(color =  first20$Approach),   # Separate data by levels of vs
        upper = NULL)

p+scale_color_manual(values = c("#000000", "#E69F00"))+
  +scale_fill_manual(values = c("#000000", "#E69F00"))


pair_plot <- first20 %>% 
  GGally::ggpairs(columns = 2:20, 
                  ggplot2::aes(colour=factor(Approach)))

ggplot <- function(...) ggplot2::ggplot(...) + scale_color_manual(values = c("#000000", "#E69F00"))
library(GGally)
ggpairs(first20, 
        columns=, c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
                    "mean", "ARCH.LM", "cv" ,"stability","linearity",
                    "max_level_shift"),
        colour='Approach',
        lower=list(continuous='points'), 
        axisLabels='none',  
        upper=list(continuous='blank')
)

library(RColorBrewer)
library(car)
my_colors <- c("#000000", "#E69F00")
p <- scatterplotMatrix(~curvature+nonlinearity+seas_pacf+unitroot_pp+
           mean+ARCH.LM+cv+stability+linearity+
           max_level_shift|Approach, data=first20 , 
                  reg.line="" , col=my_colors , cex=1.5 , 
                  pch=c(15,16) , 
                  main="Scatter plot with Three Cylinder Options"
)

ggsave(filename="featurets1.pdf",pair_plot,
       width = 20, 
       height = 20 ,
       dpi = 600, units="cm")

feature_monthly <- read_rds("feature_monthly.rds")
fm <- as_tibble(feature_monthly)
AD <- AD[,-1]
AF <- AF[,-1]


effect <- read_rds("wetransfer_rds_2022-02-17_1703/Effects.rds")

eff <- as_tibble(effect[[2]])
for (i in (2:42)) {
  eff <- eff %>% bind_rows(as_tibble(effect[[i]]))
}
eff %>% filter(.feature=="CV") %>% View()
 fe <- unique(eff$.feature)

 known <- c("curvature","nonlinearity","seas_pacf","unitroot_pp" ,
            "mean", "ARCH.LM", "CV" ,"stability","linearity",
            "max_level_shift","e_acf10", "unitroot_kpss","diff2_acf1",
            "var", "entropy", "e_acf1", "seas_acf1","diff1_acf10" ,"diff2_acf10")
 
            "lumpiness","flat_spots","crossing_points","nonlinearity","stability","x_acf1","x_acf10","x_pacf5","seas_pacf",
            "seas_acf1","hurst","spike","linearity","curvature")
 
 
p <- ggplot(eff, aes(y=.value,x=.borders))+
  geom_line()+
  facet_wrap(vars(.feature), scales = "free", ncol = 4)+
  scale_y_continuous(labels = scales::label_percent())+
  ggthemes::theme_few()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Value" , y="Probability")
ggsave(filename="pd.pdf",p,
       width = 20, 
       height = 40 ,
       dpi = 600, units="cm")
