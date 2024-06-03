ComputePropBact <- function(MAE, pattern = "Lactobacillus"){
  #SE in which to find VM information
  se <- MAE[["VM"]]
  #dataframe with count data
  count <- assay(se)
  #Total count per sample
  countSum <- se$SampleSum
  #sum of counts that respect the pattern
  countPat <- colSums(count[str_detect(rownames(se), pattern = pattern), ])
  #Prop of lactobacillus
  Prop_Lacto <- countPat/countSum
  names(Prop_Lacto) <- colnames(count)
  
  return(Prop_Lacto)
}
