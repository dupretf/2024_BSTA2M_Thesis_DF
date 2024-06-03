CorTibble <- function(SE, MAE, cohort = c("STANFORD", "UMD")){
  tibble <- 
    tibble(Set = cohort,
           Age = SE$Age,
           BMI = SE$BMI,
           Gestat = SE$GestationalAge_days,
           pH = SE$PH,
           Reprod = SE$Reprod_status,
           cycle_nb = SE$cycle_nb,
           cycle_length = SE$cycle_length,
           cycleday = SE$cycleday,
           Prop_Lacto = ComputePropBact(MAE = MAE),
           Mean = colMeans(assay(SE), na.rm = T))
  
  return(tibble)
}