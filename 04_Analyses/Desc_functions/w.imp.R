#Imputation to half the minimum with weights corresponding to age
# and pH correlation to mean intensity

w.imp <- function(df, SE, cohort = c("Stanford", "UMD")){
  if(cohort == "Stanford"){
    Age <- SE$Age
    names(Age) <- SE$SampleID
    
    weight.age <- Age/(median(Age, na.rm = TRUE))
    
    for(i in 1:length(weight.age)){
      if(is.na(weight.age[i])){weight.age[i] <- 1}
    }
    
    factor <- weight.age*0.5
    
  }else if(cohort == "UMD"){
    Age <- SE$Age
    names(Age) <- SE$SampleID
    pH <- SE$PH
    names(pH) <- SE$SampleID
    
    weight.age <- Age/(median(Age, na.rm = TRUE))
    weight.ph <- pH/(median(pH, na.rm = TRUE))
    
    for(i in 1:length(weight.age)){
      if(is.na(weight.age[i])){weight.age[i] <- 1}
      if(is.na(weight.ph[i])){weight.ph[i] <- 1}
    }
    
    factor <- weight.age*(1/weight.ph)*0.5
  }
  
  colvect <- c()
  min <- c()
  imp <- c()
  for(j in 1:ncol(df)){
    colvect <- df[,j]
    names(colvect) <- rownames(df)
    
    min <- min(na.omit(colvect))
    imp <- factor*min
    
    colvect[is.na(colvect)] <- 
      imp[names(imp) %in% names(colvect[is.na(colvect)])]
    
    df[,j] <- colvect
  }
  
  return(df)
}