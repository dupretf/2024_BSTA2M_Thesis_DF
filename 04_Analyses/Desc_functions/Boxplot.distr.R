Boxplot.distr <- function(df, transformation = c("none", "asinh", "log10"), cohort = c("Stanford", "UMD")){
  #Applying transformation
  if(transformation == "none"){
    data <- df
  } else if(transformation == "asinh"){
    data <- asinh(df)
  } else if(transformation == "log10"){
    data <- log10(df)
  }
  
  #Some parameters
  n <- nrow(df)
  p <- ncol(df)
  title <- paste0("Distribution of ", cohort, " metabolites")
  subtitle <- paste0("(scale : ", transformation, ")")
  
  #Switching to long format
  long.data <- 
    data |>
    pivot_longer(cols = 1:p, names_to = "Metabolites", values_to = "l10Peak")
  
  #Ggplot
  long.data |>
    ggplot() +
    aes(x = Metabolites, y = l10Peak)+
    geom_boxplot(fill = CohCol[cohort], 
                 outlier.color = CohCol[cohort],
                 linewidth = 0.3)+
    labs(y = "Peak intensity", 
         title = title,
         subtitle = subtitle)+
    theme(legend.position = "bottom", 
          axis.text.x = element_blank())
}
