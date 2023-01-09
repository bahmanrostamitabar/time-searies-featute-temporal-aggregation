# -iyvrsti features extended
# we wanted to determine the accuracy frontier which can be achived by
# standard ML andstatistical methods. In this manner we sat the stage
# for more advance NN to see can they make a break through and achive further gains!
  
  # Projekat sa packratom obezbjedi
  
  # Razmisli dal da dodajes jos jednu neuronsku mrezu

#Shiny preuredi sa novim tabelama

# Vidjeti dal se ova mreya moye nayvati nnet feedforward neural network (FNN)


library(forcats)
library(tidyr)

table_fig <- table_CB %>% as_tibble() %>% pivot_longer(cols = 2:14, names_to = "ML model",values_to = "Overall RMSSE")

utility_plot <- table_fig[1:13,] %>%
  ggplot(aes(x=  fct_reorder(`ML model`,`Overall RMSSE`) , y=`Overall RMSSE`)) +
  geom_point(colour="black", size=3) +
  geom_segment(color="grey", lty=3, aes(x=`ML model`,
                    xend=`ML model`,
                    y=0, yend=`Overall RMSSE`))+
  coord_flip()+
  ggthemes::theme_few()+
  labs(x="Classification models (including ML)")

utility_plot

ggsave(filename="utility.png",utility_plot,
       width = 14,
       height = 9,
       dpi = 300, units="cm")
  
CB <- table_CB[,-c(1:3)]
kable(CB,format="latex",booktabs=TRUE,caption = "Utility accuracy comparison", align = "ccccccccc")%>%
  kable_styling(latex_options="scale_down")%>%
  #row_spec(1, bold = T, color = "white", background = "red")
  column_spec(8,bold = T, color = "black")
#https://stackoverflow.com/questions/53341155/coloring-rows-with-kableextra-based-on-cell-values
#https://haozhu233.github.io/kableExtra/awesome_table_in_html.html