### CCA functions ###

## Canonical correlations barplot
CcaBarplot <- function(cca, cohort, col){
  ggplot()+
    aes(x = as.factor(1:length(cca)), y = cca) +
    geom_col(position = "identity", fill = col)+
    geom_label(aes(label = round(sqrt(cca),2)),
               vjust = 1, alpha = 0.7, cex = 3)+
    scale_y_continuous(labels = scales::label_percent())+
    labs(x = " ", y = "Canonical correlation coefficient",
         title = paste0(cohort, " canonical correlations"))
}

## Column coordinates on variates space
cca.loads <- function(cca, comp = c(1,2), palette, cohort = c("Stanford", "UMD")){
  #Dataframe of scores
  dfX <- as.data.frame(cca$scores$corr.X.yscores)
  dfY <- as.data.frame(cca$scores$corr.Y.xscores)
  
  colnames(dfX) <- str_c("CC", 1:ncol(dfX))
  colnames(dfY) <- str_c("CC", 1:ncol(dfY))
  
  #Colnames variable
  CC <- str_c("CC", comp)
  
  #Creating labs for ggplot
  labs <- str_c("Variate ", comp)
  
  #ggplot
  ggX <- 
    tibble(variab = rownames(dfX),
           dfX,
           block = "Metabolites") |>
    mutate(evid = ifelse(variab %in% c("lactate", "tyramine", "putrescine", "spermidine", "cysteine", "hippurate"), variab, NA)) |>
    mutate(varlab = ifelse(!(variab %in% c("lactate", "tyramine", "putrescine", "spermidine", "cysteine", "hippurate")), variab, NA)) |>
    ggplot()+
    geom_segment(aes(xend = get(CC[1]), yend = get(CC[2]), x = 0, y = 0, col = evid),
                 arrow = arrow(angle = 12, length = unit(0.3, "cm")))+
    geom_text_repel(aes(x = get(CC[1]), y = get(CC[2]), label = evid),
                    box.padding = 0.5, size = 3)+
    geom_text_repel(aes(x = get(CC[1]), y = get(CC[2]), label = varlab), 
                    box.padding = 0.5, size = 3)+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    labs(x = labs[1], y = labs[2],
         title = paste0(cohort, " canonical variates"),
         subtitle = "Metabolites")+
    theme(legend.position = "none")
  
  ggY <- ggplot(dfY)+
    geom_segment(aes(xend = get(CC[1]), yend = get(CC[2]), x = 0, y = 0),
                 col = palette[2], arrow = arrow(angle = 12, length = unit(0.3, "cm")))+
    geom_text_repel(aes(x = get(CC[1]), y = get(CC[2]), label = rownames(dfY)), 
                    box.padding = 0.5, size = 3)+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    labs(x = labs[1], y = labs[2], subtitle = "Subcommunity")
  
  gg <- ggX+ggY+
    plot_layout(guides = "collect")
  
  return(gg)
}
