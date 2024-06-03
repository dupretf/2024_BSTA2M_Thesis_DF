
plot_topic_betas <- function(mae, assayname, threshold = 1/100) {
  
  beta_long <- get_beta_long(mae, assayname)
  beta_long <- 
    beta_long %>% 
    arrange(taxa, -prop) %>% 
    group_by(taxa) %>% 
    mutate(max_topic = topic[1]) %>% 
    ungroup() %>% 
    arrange(max_topic, -prop) %>% 
    mutate(taxa = taxa %>% factor(., levels = unique(taxa)) %>% fct_rev())
  
  ggplot(beta_long %>% filter(prop > threshold), 
         aes(x = topic, y = taxa, size = prop, col = topic)) +
    geom_point() +
    scale_size(
      "Proportion in\nsub-community", range = c(0, 6), limits = c(0, 1), 
      breaks = c(1/100, 1/10, 1/4, 1/2, 1),
      labels = c("1/100", "1/10","1/4","1/2","1")
      ) +
    xlab("") + ylab("") + 
    scale_color_manual(values = get_topic_colors(levels(beta_long$topic))) +
    guides(col = "none") # size = "none"
  
}
