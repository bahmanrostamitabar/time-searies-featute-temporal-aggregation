library(tidyverse)
AD <- readr::read_rds("AD.rds") %>% as_tibble() %>% mutate(Approach=rep("AD",48000)) %>% select(-Monthly)
AF <- readr::read_rds("AF.rds") %>% as_tibble() %>% mutate(Approach=rep("AF",48000)) %>% select(-Monthly)

all_adaf <- AD %>% bind_rows(AF) 
all_adaf <- all_adaf %>% mutate(case_when(
  `Temporal granularity` == "2-Monthly", "Bi-Monthly",
  `Temporal granularity` == "Quarterly", "Quarterly",
  `Temporal granularity` == "4-Monthly", "4-Monthly",
  `Temporal granularity` == "Biannual", "Semi-annual",
  `Temporal granularity` == "Annual", "Annual",
))
"Semi-annual","Annual"
all_adaf_longer <- all_adaf %>% pivot_longer(cols = 1:5, names_to = "Temporal granularity", 
                          values_to = "RMSSE") %>% mutate(`Temporal granularity`=case_when(
                            `Temporal granularity` == "2-Monthly" ~ "Bi-Monthly",
                            `Temporal granularity` == "Quarterly"~ "Quarterly",
                            `Temporal granularity` == "4-Monthly"~ "4-Monthly",
                            `Temporal granularity` == "Biannual"~ "Semi-annual",
                            `Temporal granularity` == "Annual"~ "Annual"
                          ))
  
  
  
all_adaf_longer <- all_adaf_longer %>%  mutate(Approach=factor(Approach),
                                                          `Temporal granularity`=factor(`Temporal granularity`, levels=c("Bi-Monthly","Quarterly","4-Monthly","Semi-annual","Annual")))

p <- ggplot(all_adaf_longer, aes(x=`Temporal granularity`, y=RMSSE, fill=Approach))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = quantile(all_adaf_longer$RMSSE, c(0.1, 0.97)))+
  scale_fill_manual(name="Approach", 
                              values = c(AD="white", AF="grey"), 
                    labels=c(AD="AD", AF="AF"))+
  ggthemes::theme_few()+
  theme(legend.position = "bottom")

ggsave(filename="box_plot_rmsse.png",p,
       width = 11, 
       height = 7 ,
       dpi = 300, units="cm")

AD <- readr::read_rds("AD.rds")
AF <- readr::read_rds("AF.rds") 

combo <- cbind(AD[,"2-Monthly"],AF[,"2-Monthly"],AD[,"Quarterly"],AF[,"Quarterly"],AD[,"4-Monthly"],AF[,"4-Monthly"],AD[,"Biannual"],AF[,"Biannual"],AD[,"Annual"],AF[,"Annual"])
colnames(combo) <- c("2-Monthly","2-Monthly","Quarterly","Quarterly","4-Monthly","4-Monthly","Biannual","Biannual","Annual","Annual")

boxplot(combo,col=c(0,"grey",0,"grey",0,"grey",0,"grey",0,"grey",0,"grey"))
legend("topright", legend=c("AD", "AF"),col=c("black","grey"), lty=1, cex=0.8)
