assign_attributes_to_graph_nocomm = function(g, degeneracy, directed_mode, v_g_name, l_v_g_name){

	# perform k-core decomposition
	cores = cores_dec(g, degeneracy, directed_mode)$cores
	
	# get node membership
	membership = 0*1:l_v_g_name

	# assign membership to each node
	for (i in 1:l_v_g_name){
		membership[i] = as.numeric(cores[v_g_name[i]])
	}

	V(g)$core_no = membership
	V(g)$comm = rep(NA, l_v_g_name)

	output=list(g=g)
	
}