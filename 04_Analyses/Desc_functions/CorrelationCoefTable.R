CorrPval <- function(){
  cor.est.stan <- c()
  cor.est.umd <- c()
  cor.pval.stan <- c()
  cor.pval.umd <- c()
  cor.umd <- c()
  cor.test.stan <- list()
  cor.test.umd <- list()
  umd <- data.frame(umd)
  stan <- data.frame(stan)
  for(i in 1:ncol(stan)){
    #Stanford
    if(is.numeric(stan[,i])){
      if(sum(is.na(stan[,i])) >= 150){
        cor.est.stan[i] <- NA
        cor.pval.stan[i] <- NA
      } else{
        cor.test.stan[[i]] <- cor.test(x = stan[,i], y = stan$Mean,
                                       alternative = "two.sided",
                                       method = "spearman")
        cor.est.stan[i] <- cor.test.stan[[i]]$estimate
        cor.pval.stan[i] <- cor.test.stan[[i]]$p.value
      }
    } else{
      cor.est.stan[i] <- NA
      cor.pval.stan[i] <- NA
    }
  }
  for(i in 1:ncol(umd)){
    #UMD
    if(is.numeric(umd[,i])){
      if(sum(is.na(umd[,i])) >= 150){
        cor.est.umd[i] <- NA
        cor.pval.umd[i] <- NA
      } else{
        cor.test.umd[[i]] <- cor.test(x = umd[,i], y = umd$Mean,
                                      alternative = "two.sided",
                                      method = "spearman")
        cor.est.umd[i] <- cor.test.umd[[i]]$estimate
        cor.pval.umd[i] <- cor.test.umd[[i]]$p.value
      }
    } else{
      cor.est.umd[i] <- NA
      cor.pval.umd[i] <- NA
    }
  }
  data.frame(Variable = colnames(stan), Estimate.STAN = cor.est.stan, Pval.STAN = cor.pval.stan, Estimate.UMD = cor.est.umd, Pval.UMD = cor.pval.umd) |> pander(missing = "", caption = "Spearman's correlation coefficient and associated p-value between the log10(mean intensity) of metabolites and the different characteristics")
}

