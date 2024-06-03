DataProcessingForPCA <- function(df, log10 = TRUE){
  for(i in 1:length(df)){
    df[is.na(df[,i]),i] <- min(na.omit(df[,i]))/2
  }
  if(log10 == TRUE){
    data <- log10(df)
  } else {data <- df}
  return(data)
}
