#Interstructure
Interstructure <- function(RV.coo, Dcol, cohort = c("Stanford", "UMD")){
  data <- RV.coo
  
  data |>
    data.frame(Data = rownames(data)) |>
    ggplot()+
    aes(xend = S1, yend = S2, x = 0, y = 0, col = Data)+
    geom_segment(arrow = arrow(angle = 12, length = unit(0.3, "cm")))+
    scale_fill_manual(values = Dcol, aesthetics = c("colour", "fill"))+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    #Drawing a circle:
    annotate("path", x = 0 + 1*cos(seq(0,2*pi,length.out=100)),
             y= 0 + 1*sin(seq(0,2*pi,length.out=100)))+
    #xlim(c(-1,1))+ ylim(c(-1,1))+
    coord_fixed(ratio = 1, xlim = c(-1.5,1.5))+
    geom_text(label = c("Metabolites", "Sub-community"), aes(x = S1, y = S2), nudge_y = 0.2)+
    labs(title = paste0(cohort, " interstructure"),
         #subtitle = "projection of my tables in the _S_ principal component subspace",
         x = "S1", y = "S2")+
    theme(legend.position = "none")
}

#Scree plot (percent variance and cumulative)
scree <- function(eigentable, cohort = c("Stanford", "UMD"), col){
  eigentable |>
    #filter(row_number() <= 10 | n() <= 10) |>
    ggplot()+
    aes(x = comp) +
    geom_col(aes(y = perc.var), position = "identity", fill = col)+
    geom_point(aes(y = cum), col = col, alpha = 0.4, size = 1)+
    geom_path(aes(y = cum, group = ""), alpha = 0.4)+
    scale_y_continuous(labels = scales::label_percent(),
                       limits = c(0,1))+
    labs(x = "Component", y = "Variance explained",
         title = paste0(cohort, " STATIS variance decomposition"))
}

#Intrastructure
Intrastructure <- function(C.T4, comp = c(1,2), level = 1:4, eigentable, palette, cohort = c("Stanford", "UMD")){
  #Data selection
  C.T4 <- C.T4[,comp]
  
  #Data formatting
  C.T4 <- C.T4 |>
    mutate(Component = str_extract(rownames(C.T4), "[A-Za-z]+"),
           Number = rep(1:4,2)) |>
    filter(Number == level)
  
  Cp <- str_c("C", comp)
  
  #Plot
  C.T4 |>
    ggplot()+
    aes(xend = get(Cp[1]), yend = get(Cp[2]), x = 0, y = 0, col = Component)+
    geom_segment(arrow = arrow(angle = 12, length = unit(0.3, "cm")))+
    scale_fill_manual(values = palette, aesthetics = c("colour", "fill"))+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_label(aes(x = get(Cp[1]), y = get(Cp[2]), 
                   label = Component), vjust = -0.8)+
    annotate("path", x = 0 + 1*cos(seq(0,2*pi,length.out=100)),
             y= 0 + 1*sin(seq(0,2*pi,length.out=100)))+
    coord_fixed(ratio = 1, xlim = c(-1.5,1.5))+
    labs(title = paste0(cohort, " intrastructure"),
         x = paste0(Cp[1], " (", round(eigentable$perc.var[comp[1]]*100,1), "%)"),
         y = paste0(Cp[2], " (", round(eigentable$perc.var[comp[2]]*100,1), "%)"),
         subtitle= "")+
    theme(legend.position = "none")
}

#Row coordinates on compromise space
Compromise <- function(C.li, comp = c(1,2), eigentable, cohort = c("Stanford", "UMD"), ...){
  #Selection
  C.li <- C.li[,comp]
  
  #Colnames variable
  Cp <- str_c("C", comp)
  
  #Plot
  C.li |>
    ggplot()+
    aes(x = get(Cp[1]), y = get(Cp[2]))+
    geom_point(col = "azure4")+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    labs(title = paste0(cohort, " compromise (row coordinates)"),
         x = paste0(Cp[1], " (", round(eigentable$perc.var[comp[1]]*100,1), "%)"),
         y = paste0(Cp[2], " (", round(eigentable$perc.var[comp[2]]*100,1), "%)"),
         subtitle = "each dot is a sample")
}

#Column coordinates on compromise space
Col.coord <- function(statis, comp = c(1,2), eigentable, palette, cohort = c("Stanford",)){
  #Data selection
  C.Co <- statis$C.Co
  C.Co <- C.Co[,comp]
  
  #Colnames variable
  Cp <- str_c("C", comp)
  
  #Plot
  tibble(variab = rownames(C.Co),
         data.frame(C.Co),
         block = statis$TC$T) |>
    mutate(evid = ifelse(variab %in% c("lactate", "tyramine", "putrescine", "spermidine", "cysteine", "hippurate"), variab, NA)) |>
    mutate(varlab = ifelse(!(variab %in% c("lactate", "tyramine", "putrescine", "spermidine", "cysteine", "hippurate")), variab, NA)) |>
    ggplot()+
    geom_segment(aes(xend = get(Cp[1]), yend = get(Cp[2]), x=0,y=0, col = evid),
                 arrow = arrow(angle = 12, length = unit(0.3, "cm")))+
    geom_vline(aes(xintercept = 0), linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0), linetype = "dashed", col = "grey60")+
    ggrepel::geom_text_repel(aes(x = get(Cp[1]), y = get(Cp[2]), label = varlab),
                             box.padding = 0.5, size = 3)+
    ggrepel::geom_text_repel(aes(x = get(Cp[1]), y = get(Cp[2]), label = evid),
                             box.padding = 0.5, size = 3)+
    #scale_fill_manual(na.value = "grey90", aesthetics = c("colour", "fill"))+
    facet_wrap(facets=vars(block), scales = "free")+
    labs(title = paste0(cohort, " column coordinates"),
         x = paste0(Cp[1], " (", round(eigentable$perc.var[comp[1]]*100,1), "%)"),
         y = paste0(Cp[2], " (", round(eigentable$perc.var[comp[2]]*100,1), "%)"))+
    theme(legend.position = "none")
}
