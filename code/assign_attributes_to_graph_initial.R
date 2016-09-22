assign_attributes_to_graph_initial = function(edges_df, weighted){
	
	# make graph out of data frame

	if (weighted == TRUE){

		g = graph.data.frame(edges_df, directed=TRUE)
		g = simplify(g, remove.multiple=FALSE)
		E(g)$label = as.character(E(g)$weight)

	} else {
	
    # selecting only the first two columns avoids assigning a "weight" attributes to edges
		g = graph.data.frame(edges_df[,1:2], directed=TRUE)
		g = simplify(g, remove.multiple=FALSE)

	}

	v_g_name = V(g)$name
	l_v_g_name = length(v_g_name)
	
	output=list(g=g, v_g_name=v_g_name, l_v_g_name=l_v_g_name, weighted=weighted)
	
}