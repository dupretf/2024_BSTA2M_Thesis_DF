### SCA functions ###

## Variance decomposition plot
vardecomp <- function(vartab, comp.max = 10, cohort = c("Stanford", "UMD"), col){
  vartab[1:comp.max,] |>
    ggplot()+
    aes(x = as.factor(1:comp.max)) +
    geom_col(aes(y = perc.var), fill = col)+
    geom_point(aes(y = cum), col = col, alpha = 0.4)+
    geom_path(aes(y = cum, group = ""), alpha = 0.4)+
    scale_y_continuous(labels = scales::label_percent(),
                       limits = c(0,1))+
    labs(x = "Component", y = "Percentage of variance explained",
         title = paste0(cohort, " SCA variance decomposition"))
}

## Scores plot
scores.plot <- function(sca.mod, comp = c(1,2), cohort = c("Stanford", "UMD")){
  #selecting relevant scores
  sc <- scores(sca.mod)[,comp]
  #selecting corresponding variance explained
  p.var <- sca.mod$explvar[comp]
  
  #formatting column names
  colnames(sc) <- str_c("Comp", comp)
  #Creating labs for ggplot
  labs <- str_c("Comp ", comp, " (", round(p.var,1), "%)")
  
  #ggplot
  gg <- ggplot(sc)+
    aes(x = get(colnames(sc)[1]), y = get(colnames(sc)[2]))+
    geom_point()+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    labs(x = labs[1], y = labs[2],
         title = paste0(cohort, " scores plot"))
  
  return(gg)
}

## Two blocks loadings plot
loading.plot <- function(sca.mod, comp = c(1,2), palette = c("black", "black"), cohort = c("Stanford", "UMD")){
  #Selecting block
  loads1 <- sca.mod$blockLoadings[[1]]
  loads2 <- sca.mod$blockLoadings[[2]]
  
  #Extracting percent of variance explained
  explvar1 <- attr(loads1, which = "explvar")
  explvar2 <- attr(loads2, which = "explvar")
  
  perc.var1 <- scale(explvar1, center = F, scale = sum(explvar1))*100
  perc.var2 <- scale(explvar2, center = F, scale = sum(explvar2))*100
  
  #Creating labs
  labs1 <- paste0(attr(loads1, which = "dimnames")[[2]], " (",
                  round(perc.var1[comp],1), "%)")
  labs2 <- paste0(attr(loads2, which = "dimnames")[[2]], " (",
                  round(perc.var2[comp],1), "%)")
  
  #Renaming columns
  colnames(loads1) <- str_c("PC", 1:length(explvar1))
  colnames(loads2) <- str_c("PC", 1:length(explvar2))
  
  #Comp
  PC <- str_c("PC", comp)
  
  #plot1
  gg1 <-
    data.frame(block = "Metabo",
               variab = rownames(loads1),
               loads1) |> tibble() |>
    mutate(evid = ifelse(variab %in% c("lactate", "tyramine", "putrescine", "spermidine", "cysteine", "hippurate"), variab, NA)) |>
    mutate(varlab = ifelse(!(variab %in% c("lactate", "tyramine", "putrescine", "spermidine", "cysteine", "hippurate")), variab, NA)) |>
    ggplot()+
    geom_segment(aes(xend = get(PC[1]), yend = get(PC[2]), x=0,y=0, col = evid),
                 arrow = arrow(angle = 12, length = unit(0.3, "cm")))+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    labs(x = labs1[comp[1]], y = labs1[comp[2]],
         subtitle = "Metabolite")+
    ggrepel::geom_text_repel(aes(x = get(PC[1]), y = get(PC[2]), label = varlab),
                             box.padding = 0.5, size = 3)+
    ggrepel::geom_text_repel(aes(x = get(PC[1]), y = get(PC[2]), label = evid),
                             box.padding = 0.5, size = 3)+
    labs(title = paste0(cohort, " loadings plot"))+
    theme(legend.position= "none")
  #plot1
  gg2 <- 
    ggplot(loads2)+
    aes(xend = get(PC[1]), yend = get(PC[2]), x = 0, y = 0)+
    geom_segment(arrow = arrow(angle = 12, length = unit(0.3, "cm")),
                 col = palette[2])+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    labs(x = labs2[comp[1]], y = labs2[comp[2]],
         subtitle = "Subcommunity")+
    geom_text_repel(aes(x = get(PC[1]), y = get(PC[2]),
                        label = rownames(loads2)), cex = 3)
  
  #Both plots
  gg <- (gg1+gg2)
  #Return
  return(gg)
}

#Correlations
corr.plot <- function(sca.mod, comp = c(1,2), palette = c("blue", "red"), cohort = c("Stanford", "UMD")){
  #Selecting block
  loads1 <- sca.mod$blockLoadings[[1]]
  loads2 <- sca.mod$blockLoadings[[2]]
  
  #Extracting percent of variance explained
  #explvar1 <- attr(loads1, which = "explvar")
  #explvar2 <- attr(loads2, which = "explvar")
  
  #Scaling loads
  #s.loads1 <- scale(loads1, center = F, scale = 1/sqrt(sca.mod$explvar))
  #s.loads2 <- scale(loads2, center = F, scale = 1/sqrt(sca.mod$explvar))
  s.loads1 <- loads1
  s.loads2 <- loads2
  
  #Creating labs
  labs <- paste0(attr(loads1, which = "dimnames")[[2]])
  
  #Renaming columns
  colnames(s.loads1) <- str_c("PC", 1:dim(loads1)[2])
  colnames(s.loads2) <- str_c("PC", 1:dim(loads2)[2])
  
  #Combining
  Combin <- rbind(
    data.frame(Set = "Metabolite",
               s.loads1),
    data.frame(Set = "Subcommunity",
               s.loads2)
  )
  
  #Comp
  PC <- str_c("PC", comp)
  
  #Circle parameters
  xc = 0; yc = 0; r = 1
  
  #Plot
  gg <- 
    ggplot(Combin)+
    aes(xend = get(PC[1]), yend = get(PC[2]), x = 0, y = 0, col = Set)+
    geom_segment(arrow = arrow(angle = 12, length = unit(0.3, "cm")))+
    scale_fill_manual(values = palette, aesthetics = c("colour", "fill"))+
    geom_vline(aes(xintercept = 0),
               linetype = "dashed", col = "grey60")+
    geom_hline(aes(yintercept = 0),
               linetype = "dashed", col = "grey60")+
    labs(x = labs[comp[1]], y = labs[comp[2]])+
    geom_text_repel(aes(x = get(PC[1]), y = get(PC[2]),
                        label = rownames(Combin)), cex = 3)+
    annotate("path", x = xc + r*cos(seq(0,2*pi,length.out=100)),
             y= yc + r*sin(seq(0,2*pi,length.out=100)))+
    coord_fixed(ratio = 1, xlim = c(-1.5,1.5))+
    labs(title = paste0(cohort, " loadings plot"))
  
  #Return
  return(gg)
}
