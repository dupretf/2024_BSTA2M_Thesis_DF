perform_lmFit <- function(SE, formula, transformation = c("none", "log10", "asinh")) {
  if(transformation == "none"){
    object <- assay(SE)
  } else if (transformation == "log10"){
    object <- log10(assay(SE))
  } else if (transformation == "asinh"){
    object <- asinh(assay(SE))
  }
  #Design matrix
  design <- model.matrix(formula, data = colData(SE))
  #Models
  fit <- lmFit(object, design = design)
  fit <- eBayes(fit)
  #Testing differential expression
  results <- decideTests(fit)
  # Return the results
  return(list(fit = fit, design = design, results = results))
}
