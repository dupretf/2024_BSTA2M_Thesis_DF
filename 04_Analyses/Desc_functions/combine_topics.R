combine_topics <- 
  function(optimal_K, labelled_lda_models, Lacto_topics_gamma, Lacto_topics_beta){
    
    gamma <- labelled_lda_models[[optimal_K]]$gamma
    gamma <- gamma[match(rownames(Lacto_topics_gamma), rownames(gamma)),]
    gamma[is.na(gamma)] <- 0
    gamma <- gamma * (1-rowSums(Lacto_topics_gamma))
    gamma <- cbind(Lacto_topics_gamma, gamma)
    gamma <- gamma[,sort(colnames(gamma))]
    
    beta <- labelled_lda_models[[optimal_K]]$beta
    beta <- beta[, match(rownames(Lacto_topics_beta), colnames(beta))]
    colnames(beta) <- rownames(Lacto_topics_beta)
    beta[is.na(beta)] <- -Inf
    beta <- rbind(Lacto_topics_beta %>% t() %>% log(), beta)
    beta <- beta[sort(rownames(beta)),]
    
    list(gamma = gamma, beta = beta)
  }