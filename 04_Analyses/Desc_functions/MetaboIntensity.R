MetaboIntensity <- function(assay, pattern = "^lactate"){
  row.names <- rownames(assay)
  assay[str_detect(row.names, pattern = pattern), ]
}