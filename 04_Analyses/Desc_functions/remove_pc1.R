remove_pc1 <- function(pca) {
  scores <- pca$x
  scores[,1] <- 0
  xnew <- scores %*% t(pca$rotation)
  xnew |> as.data.frame() |> I()
}
remove_pc1_nipals <- function(pca) {
  scores <- pca$scores
  scores[,1] <- 0
  xnew <- scores %*% solve(pca$loadings)
  xnew |> as.data.frame() |> I() |> set_colnames(rownames(pca$loadings))
}
