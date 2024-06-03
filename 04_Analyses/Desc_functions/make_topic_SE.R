make_topic_SE <- function(mae, assay_name, lda_model) {
  
  assay <- lda_model$gamma %>% t()
  assay_rowdata <- lda_model$beta %>% exp()
  
  
  topic_assay <- list()
  topic_assay[[assay_name]] <-
    SummarizedExperiment::SummarizedExperiment(
      assay = assay,
      rowData = assay_rowdata
    )
  
  topic_assay
}