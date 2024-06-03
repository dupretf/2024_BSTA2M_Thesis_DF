MeanVarPlot <- function(df, cohort = c("Stanford", "UMD"), scale, x.y.log10 = TRUE){
  #Computing column mean and variance
  var <- apply(df, 2, FUN = var, na.rm = T)
  mean <- apply(df, 2, FUN = mean, na.rm = T)
  #Plotting var against mean
  gg <- 
    ggplot()+aes(x = mean, y = var)+ 
    geom_point(col = CohCol[cohort])+
    geom_abline(intercept = 0, slope = 0, col = "lightgreen")+
    geom_abline(intercept = 0, slope = 1, col = "orange")+
    geom_abline(intercept = 0, slope = 2, col = "red")+
    geom_smooth(method = "lm", formula= y~x, se = T,
                col = "grey40", linetype = "dashed")+
    labs(title = paste0("Mean-variance plot of ", cohort, " metabolites (", scale, ")"))
  #Adapting x-axis and y-axis to be continuous or log10
  if(x.y.log10 == TRUE){
    gg <- gg + scale_y_log10() + scale_x_log10()+
      labs(x = "log10(mean Intensity)", y = "log10(Intensity variance)")
  } else {gg <- gg}
  
  return(gg)
}
