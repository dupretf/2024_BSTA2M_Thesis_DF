get_beta_long <- function(mae, assayname) {
  rowdata <- SummarizedExperiment::rowData(mae[[assayname]])
  rowdata %>% 
    as.data.frame() %>% 
    set_colnames(colnames(rowdata)) %>% 
    mutate(topic = rownames(rowdata) %>% factor()) %>% 
    pivot_longer(-topic, names_to = "taxa", values_to = "prop") # %>% 
  # mutate(taxa = taxa %>% str_replace_all("\\."," "))
}