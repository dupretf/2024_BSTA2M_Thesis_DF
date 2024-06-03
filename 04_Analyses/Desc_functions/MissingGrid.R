MissingGrid <- function(df, decreasing = c(TRUE, FALSE), cohort = c("Stanford", "UMD")){
  gg <- 
    df[order(rowMeans(is.na(df)), decreasing = decreasing),
       order(colMeans(is.na(df)), decreasing = decreasing)] |>
    vis_miss() +
    labs(subtitle = paste(cohort, "ordered by proportion of missing values by sample and metabolite"),
         y = "Samples", x = "Metabolites")+
    theme(axis.text.x = element_blank())
  
  return(gg)
}