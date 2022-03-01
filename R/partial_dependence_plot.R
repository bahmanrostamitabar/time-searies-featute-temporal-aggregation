partial_dependency_plot <- read_rds("fig9x.rds")
data_pd <- partial_dependency_plot$patches
p <- data_pd$plots
p1 <- p+ggthemes::theme_few()
p1[[1]]
for (i in (1:41)) {
  p[[i]] <- p[[i]]+ggthemes::theme_few()+ 
    theme(axis.title.y = element_blank(), 
          axis.text.x = element_text(angle = 90))+
    scale_y_continuous(labels = scales::label_percent(accuracy=1))
}

pfinal <- (p[[1]]+p[[2]]+p[[3]]+p[[4]]+
  p[[5]]+p[[6]]+p[[7]]+p[[8]]+
  p[[9]]+p[[10]]+p[[11]]+p[[12]]+
  p[[13]]+p[[14]]+p[[15]]+p[[16]]+
  p[[17]]+p[[18]]+p[[19]]+p[[20]]) %>%
  add_global_label(Ylab = "Probability")


ggsave(filename="pfinal.png",pfinal,
       width = 20, 
       height = 20 ,
       dpi = 300, units="cm")

pfinal1 <- (p[[21]]+p[[22]]+p[[23]]+p[[24]]+
             p[[25]]+p[[26]]+p[[27]]+p[[28]]+
             p[[29]]+p[[30]]+p[[31]]+p[[32]]+
             p[[33]]+p[[34]]+p[[35]]+p[[36]]+
             p[[37]]+p[[38]]+p[[39]]+p[[40]]+p[[41]]) %>%
  add_global_label(Ylab = "Probability")

ggsave(filename="pfinal1.png",pfinal1,
       width = 20, 
       height = 24 ,
       dpi = 300, units="cm")

add_global_label <- function(pwobj, Xlab = NULL, Ylab = NULL, Xgap = 0.03, Ygap = 0.03, ...) {
  ylabgrob <- patchwork::plot_spacer()
  if (!is.null(Ylab)) {
    ylabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Ylab, angle = 90, ...) +
      theme_void()
  }
  if (!is.null(Xlab)) {
    xlabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Xlab, ...) +
      theme_void()
  }
  if (!is.null(Ylab) & is.null(Xlab)) {
    return((ylabgrob + patchworkGrob(pwobj)) + 
             patchwork::plot_layout(widths = 100 * c(Ygap, 1 - Ygap)))
  }
  if (is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = c(0, 100),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  if (!is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = 100 * c(Ygap, 1 - Ygap),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  return(pwobj)
}
