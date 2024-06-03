pca.plot <- function(nipals.pca, type = c("ind", "var"), cohort = c("Stanford", "UMD"), choice = c(1,2)){
  PC <- colnames(pca_stan$scores)[choice]
  
  if(type == "ind"){
    gg <- ggplot(nipals.pca$scores)+
      geom_point(aes(x = get(PC[1]), y = get(PC[2])))+
      geom_vline(aes(xintercept = 0),
                 linetype = "dashed", col = "grey40")+
      geom_hline(aes(yintercept = 0),
                 linetype = "dashed", col = "grey40")+
      labs(x = paste0("PC",choice[1]), y = paste0("PC",choice[2]),
           title = paste0("Scores (", cohort,")"))
  } else if(type == "var"){
    gg <- ggplot(nipals.pca$loadings)+
      aes(xend = get(PC[1]), yend = get(PC[2]), x = 0, y = 0)+
      geom_segment(arrow = arrow(angle = 12, length = unit(0.3, "cm")))+
      geom_vline(aes(xintercept = 0),
                 linetype = "dashed", col = "grey40")+
      geom_hline(aes(yintercept = 0),
                 linetype = "dashed", col = "grey40")+
      labs(x = paste0("PC",choice[1]), y = paste0("PC",choice[2]),
           title = paste0("Loadings (", cohort,")"))
  }
  
  return(gg)
}
