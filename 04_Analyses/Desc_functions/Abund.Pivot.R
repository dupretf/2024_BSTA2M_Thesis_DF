#Function for finding the n most abundant microbes 
# in the relative abundance dataset 'relative.df'
# and associating the characteristics of that microbe
# (Kingdom, Phylum, Class, etc.)

n.most.abund <- function(relative.df, MAE, n, usename = TRUE, long.format = FALSE){
  #Column selection in rowdata
  selected.col <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "ASV_usename")
  #Defining a large datatable with relative abundance, rowdata and mean abundance per feature
  data <- cbind(relative.df,
                rowData(MAE[["VM"]])[,selected.col],
                Overall.abund = rowMeans(relative.df, na.rm = T)) |>
    data.frame(check.names = F, row.names = rownames(relative.df))
  
  ###---###
  #Finding n highest abundant features / species
  Top.n <- data |> 
    top_n(n = n, Overall.abund) |> 
    rownames()
  #Labelling n highest abundant species by their genus + species name
  Most.abund.genspe <- c()
  for(i in 1:length(data$Overall.abund)){
    if(rownames(data)[i] %in% Top.n){
      Most.abund.genspe[i] <- paste(data$Genus[i], data$Species[i])
    } else{Most.abund.genspe[i] <- "Other"}
  }
  
  ###---###
  #Finding n highest abundant features / species
  Top.n <- data |> 
    top_n(n = n, Overall.abund) |> 
    dplyr::select(ASV_usename) |>
    unlist()
  #Labelling n highest abundant species by their species name
  Most.abund.usename <- c()
  for(i in 1:length(data$Overall.abund)){
    if(data$ASV_usename[i] %in% Top.n){
      Most.abund.usename[i] <- data$ASV_usename[i]
    } else{Most.abund.usename[i] <- "Other"}
  }
  
  ###---###
  #Saving either genus+species column or usename column
  if(usename == TRUE){
    Most.abund <- Most.abund.usename
  }else if(usename == FALSE){
    Most.abund <- Most.abund.genspe
  }
  #Joing Most.abund to large datatable
  data <- cbind(data, Most.abund)
  
  ###---###
  #Longer format
  if(long.format == TRUE){
    res <- pivot_longer(data = data,
                        cols = 1:ncol(relative.df), names_to = "Sample",
                        values_to = "Relative.Abundance")
  } else{
    res <- data
  }
  
  #Return
  return(res)
}

n.most.abund2 <- function(relative.df, taxtable, n, usename = TRUE, long.format = FALSE){
  #Column selection in rowdata
  selected.col <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "ASV_usename")
  #Defining a large datatable with relative abundance, rowdata and mean abundance per feature
  data <- cbind(relative.df,
                taxtable[,selected.col],
                Overall.abund = rowMeans(relative.df, na.rm = T)) |>
    data.frame(check.names = F, row.names = rownames(relative.df))
  
  ###---###
  #Finding n highest abundant features / species
  Top.n <- data |> 
    top_n(n = n, Overall.abund) |> 
    rownames()
  #Labelling n highest abundant species by their genus + species name
  Most.abund.genspe <- c()
  for(i in 1:length(data$Overall.abund)){
    if(rownames(data)[i] %in% Top.n){
      Most.abund.genspe[i] <- paste(data$Genus[i], data$Species[i])
    } else{Most.abund.genspe[i] <- "Other"}
  }
  
  ###---###
  #Finding n highest abundant features / species
  Top.n <- data |> 
    top_n(n = n, Overall.abund) |> 
    select(ASV_usename) |>
    unlist()
  #Labelling n highest abundant species by their species name
  Most.abund.usename <- c()
  for(i in 1:length(data$Overall.abund)){
    if(data$ASV_usename[i] %in% Top.n){
      Most.abund.usename[i] <- data$ASV_usename[i]
    } else{Most.abund.usename[i] <- "Other"}
  }
  
  ###---###
  #Saving either genus+species column or usename column
  if(usename == TRUE){
    Most.abund <- Most.abund.usename
  }else if(usename == FALSE){
    Most.abund <- Most.abund.genspe
  }
  #Joing Most.abund to large datatable
  data <- cbind(data, Most.abund)
  
  ###---###
  #Longer format
  if(long.format == TRUE){
    res <- pivot_longer(data = data,
                        cols = 1:ncol(relative.df), names_to = "Sample",
                        values_to = "Relative.Abundance")
  } else{
    res <- data
  }
  
  #Return
  return(res)
}
