eigenvalue.table <- function(nipals.pca){
  Eigenvalues = nipals.pca$eig
  Variance.explained = 100*(Eigenvalues/sum(Eigenvalues))
  Cumulative.variance = cumsum(Variance.explained)
  tibble(Eigenvalues = Eigenvalues,
         Variance.explained = Variance.explained,
         Cumulative.variance = Cumulative.variance)
}