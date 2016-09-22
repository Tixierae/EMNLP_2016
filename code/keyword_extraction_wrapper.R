keyword_extraction_wrapper = function (g, do_inflexion){
  
  g_data_frame = get.data.frame(g, what="both")
  nodes = g_data_frame$vertices[,c("comm","core_no")]
  nodes = nodes[order(nodes[,"core_no"],decreasing=TRUE),]
  
  # extract keywords (main core or truss or inflexion method)
  extracted_keywords = keyword_extraction(nodes, community_algo = "none", do_inflexion = do_inflexion)$extracted_keywords
  
  output = list(extracted_keywords = extracted_keywords)
  
}