rtest <- function(MB.stan, VM.stan, MB.umd, VM.umd, nrepet = 5000){
  res.stan <- ade4::RV.rtest(data.frame(MB.stan), data.frame(VM.stan), nrepet = nrepet)
  res.umd <- ade4::RV.rtest(data.frame(MB.umd), data.frame(VM.umd), nrepet = nrepet)
  
  data.frame(Cohort = c("Stanford", "UMD"), 
             estimate = c(res.stan$obs, res.umd$obs), 
             pvalue = c(res.stan$pvalue, res.umd$pvalue)) |> 
    pander(caption = "RV coefficient estimate and pvalue using Monte Carlo test")
}
