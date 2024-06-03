Transposer <- function(se, percent = 100){
  t.data <- data.frame(t(assay(se)))
  colnames(t.data) <- rowData(se)$BIOCHEMICAL
  t.data <- t.data[colMeans(is.na(t.data)) < (percent/100)]
}