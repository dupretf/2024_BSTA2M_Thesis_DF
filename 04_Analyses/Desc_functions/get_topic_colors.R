get_topic_colors <- function(topic_names){

  possible_colors <-
    c("steelblue","green3", "chartreuse3", "seagreen3","mediumseagreen", "forestgreen", "springgreen3", "springgreen4", "seagreen",
                 "violet","deeppink","orange", "darkorange",
                 "indianred1", "indianred3",
                 "red","red3", "turquoise3",
                 "cadetblue2", "lightblue1", "lightblue", "lightskyblue")

  # topic_colors <-
  #   bind_rows(
  #     tibble(topic = "I", color = "dodgerblue3"),
  #     tibble(topic = "II", color = "navy"),
  #     tibble(topic = "III", color = "seagreen3"),
  #     tibble(topic = "IV", color = "red"),
  #     tibble(topic = "IV-A", color = "deeppink"),
  #     tibble(topic = "IV-B", color = "orange"),
  #     tibble(topic = "IV-O.a", color = "red1"),
  #     tibble(topic = "IV-O.b", color = "red3"),
  #     tibble(topic = "V", color = "turquoise3"),
  #     tibble(topic = "VI", color = "lightblue1")
  #   )
  # 
  # topic_colors <-
  #   bind_rows(
  #     tibble(topic = "I", color = "orange1"),
  #     tibble(topic = "II", color = "deepskyblue"), # "#4FC3F7" # very similar
  #     tibble(topic = "III", color = "forestgreen"),
  #     tibble(topic = "IV", color = "dodgerblue2"),
  #     tibble(topic = "IV-A", color = "#CD93D8"), # plum would work too // burlywood2
  #     tibble(topic = "IV-B", color = "dodgerblue3"),
  #     tibble(topic = "IV-O.a", color = "slateblue3"), # #FDD835
  #     tibble(topic = "IV-O.b", color = "violetred4" ),
  #     tibble(topic = "V", color = "tomato"),
  #     tibble(topic = "VI", color = "cadetblue2")
  #   )
  
  topic_colors <-
    bind_rows(
      tibble(topic = "I", color = "orange"),
      tibble(topic = "II", color = "gold"), 
      tibble(topic = "III", color = "mediumseagreen"),   # forestgreen
      tibble(topic = "IV", color = "dodgerblue2"),
      tibble(topic = "IV-A", color = "lightskyblue"), #  #CD93D8  # turquoise3
      tibble(topic = "IV-B", color = "dodgerblue3"), # dodgerblue2 # dodgerblue3
      tibble(topic = "IV-O.a", color = "slateblue4"),
      tibble(topic = "IV-O.b", color = "plum" ),
      tibble(topic = "IV-B.a", color = "deeppink"),
      tibble(topic = "IV-B.b", color = "darkorchid"),
      tibble(topic = "IV-C1", color = "khaki"),
      tibble(topic = "V", color = "tomato"),
      tibble(topic = "VI", color = "tomato4")
    )
  

 
  topic_colors$color[match(topic_names, topic_colors$topic)]

}
