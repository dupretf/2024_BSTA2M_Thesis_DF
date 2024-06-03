scale.function <- function(df, choice = c("autoscaling", "pareto",
                                          "range", "vast", "level",
                                          "log10", "asinh")){
  #Copying dataframe
  out <- df
  #Choice and for-loop
  if(choice == "autoscaling"){
    out <- scale(x = out, center = T, scale = T)
  } else if (choice == "pareto"){
    x.sf <- apply(out, 2, FUN = sd, na.rm = T)
    out <- scale(out, center = T, scale = sqrt(x.sf))
  } else if (choice == "range"){
    x.sf <- diff(apply(out, 2, FUN = range, na.rm = T))
    out <- scale(out, center = T, scale = x.sf)
  } else if (choice == "vast"){
    x.sf <- apply(out, 2, FUN = sd, na.rm = T)
    x.mean <- apply(out, 2, FUN = mean, na.rm = T)
    sf <- x.sf*(x.sf/x.mean)
    out <- scale(out, center = T, scale = sf)
  } else if (choice == "level"){
    x.mean <- apply(out, 2, FUN = mean, na.rm = T)
    out <- scale(out, center = T, scale = x.mean)
  } else if (choice == "log10"){
    out <- log10(out)
  } else if (choice == "asinh"){
    out <- asinh(out)
  }
  #Rectifying dataframe
  out <- data.frame(out, row.names = rownames(df))
  colnames(out) <- colnames(df)
  
  return(out)
}
