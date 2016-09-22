from_terms_to_keywords <-
function(terms_list, window_size, to_overspan, to_build_on_processed, community_algo, weighted_comm, directed_comm, rw_length, size_threshold, degeneracy, directed_mode, do_inflexion) {

	edges_df = from_terms_to_graph(terms_list = terms_list, w = window_size, overspan = to_overspan, processed = to_build_on_processed)$output

	output = assign_attributes_to_graph_initial(edges_df, weighted = TRUE)

	g = output$g
	v_g_name = output$v_g_name
	l_v_g_name = output$l_v_g_name

	if (community_algo!="none"){
    
	  # perform community detection
		output = assign_attributes_to_graph_comm_a(g=g, v_g_name=v_g_name, l_v_g_name=l_v_g_name, input_community=community_algo, weight_comm=weighted_comm, directed_comm = directed_comm, rw_length = rw_length)

		g = output$g
		membership = output$membership
		my_sizes = output$sizes
		
		# perform graph decomposition
		g = assign_attributes_to_graph_comm_b(g, membership, my_sizes, size_threshold, degeneracy, directed_mode)$g

	} else {
	  
		g = assign_attributes_to_graph_nocomm(g, degeneracy, directed_mode, v_g_name, l_v_g_name)$g
	  
	}

	# for compatibility with visNetwork
	V(g)$id = v_g_name
	V(g)$label = v_g_name
	V(g)$group = V(g)$core_no

	# retain useful information
	g_data_frame = get.data.frame(g, what="both")

	nodes = g_data_frame$vertices[,c("comm","core_no")]

	# sort data frame by core number
	nodes = nodes[order(nodes[,"core_no"],decreasing=TRUE),]

	output = keyword_extraction(nodes, community_algo = community_algo, do_inflexion = do_inflexion)

	output = list(output=output, g=g)

}
