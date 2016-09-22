cores_dec = function(g, degeneracy, directed_mode){
  # This function performs unweighted or weighted k-core or k-truss decomposition of a graph
  # The function returns an ordered list of 2-tuples where each term is associated with its core number
  # "g" is a graph with edge weights (optional)
  # "degeneracy" can be "unweighted_k_core", "weighted_k_core" or "k_truss"
  # "directed" is Boolean
  # "directed_mode" can be "all", "in", or "out"
  
  # With respect to keyword extraction:
  # - select unweighted k-core when recall is more important than precision (larger main core)
  # - select weighted k-core when precision is more important than recall (smaller but stronger main core)
  # - note that overall (in terms of F1 score), weighted k-core performs better
  # - k-truss has not been tested yet for keyword extraction, but it for sure returns a smaller an denser subgraph than unweighted k-core
  
  if (degeneracy == "weighted_k_core"){
    
    # initialize list that will contain the core numbers
    cores_g = 1:length(V(g)$name)*0
    names(cores_g) = V(g)$name
    
    # k-core decomposition for weighted graphs (generalized k-cores)
    # based on Batagelj and Zaversnik's (2002) algorithm #4
    
    # create and heapify list of initial weighted degrees (nodes are alphabetically ordered for consistency with Python)
    V(g)$weight = strength(g, mode = directed_mode)
    heap_g = V(g)$weight
    names(heap_g) = V(g)$name
    heap_g=heap_g[order(names(heap_g))]
    heap_g=heapify(heap_g, min=TRUE)$heap
    
    while (length(heap_g)>0) {
      
      top = heap_g[1]
      name_top = names(top)
      # save hop-1 neighborhood of heap top element (vertice of minimum weighted degree)
      neighbors_top = setdiff(names(neighborhood(g, order=1, nodes=name_top)[[1]]), name_top)
      # order neighbors by alphabetical order (for consistency with Python)
      neighbors_top = sort(neighbors_top)
      
      # set core number of heap top element as its value (weighted degree)
      cores_g[name_top] = V(g)$weight[which(V(g)$name==name_top)]
      # delete top vertice from graph
      g = delete_vertices(g, name_top)
      
      gstr = strength(g, mode = directed_mode)
      
      if (length(neighbors_top)>0){
        # iterate over neighbors of top element
        for (i in 1:length(neighbors_top)){
          name_n = neighbors_top[i]
          max_n = max(cores_g[name_top], gstr[name_n])
          V(g)$weight[which(V(g)$name==name_n)] = max_n
          
          # update heap
          heap_g = V(g)$weight
          names(heap_g) = V(g)$name
          heap_g=heap_g[order(names(heap_g))]
          heap_g=heapify(heap_g, min=TRUE)$heap
          
        }
      } else {
        # update heap
        heap_g = V(g)$weight
        names(heap_g) = V(g)$name
        heap_g=heap_g[order(names(heap_g))]
        heap_g=heapify(heap_g, min=TRUE)$heap
      }
      
    }
    
  } else  if (degeneracy == "unweighted_k_core") {
    
    # k-core decomposition for unweighted graphs
    # based on Batagelj and Zaversnik's (2002) algorithm #1
    cores_g = graph.coreness(g, mode = directed_mode)
    
  } else  if (degeneracy == "k_truss"){
    
    # k-truss decomposition
    # based on Wang and Cheng (2012) algorithm #2
    
    # save current working directory
    overall_wd = getwd()
    
    # each worker needs to read and write files on its own directory - not in the common, shared directory
    temp_wd = tempdir()
    setwd(temp_wd)
    
    file.copy(from=paste0(overall_wd,"/k_truss.exe"),to=paste0(temp_wd,"/k_truss.exe"))
    file.copy(from=paste0(overall_wd,"/ktruss.out"),to=paste0(temp_wd,"/ktruss.out"))
    
    # create input text file
    to_input = get.edgelist(g, names=FALSE) # we want vertice IDs
    to_input = rbind(c(vcount(g), ecount(g)) ,to_input)
    write.table(to_input,file="edgelist.txt",row.names=FALSE, col.names=FALSE, sep=" ")
    
    # run k_truss decomposition
    
    operating_system = .Platform$OS.type
    
    if (operating_system=="unix"){
      
      # calls to a Linux environment - shinyapps
      system("chmod a+x ktruss.out")
      system("./ktruss.out edgelist")
      
    } else if (operating_system=="windows"){
      
      system(paste("cmd.exe /c", "k_truss.exe edgelist"), intern = FALSE, wait = TRUE)
      
    }
    
    # read output text file back in
    lines = readLines("edgelist-out.txt")
    
    # set back to the old working directory
    setwd(overall_wd)
    
    # only retain the lines of interest (truss number of each edge)
    lines_split = lapply(lines, function(x){unlist(strsplit(x, split=" "))})
    lines_split_sizes = unlist(lapply(lines_split, length))
    to_retain = which(lines_split_sizes==1)
    lines_retained = lines[to_retain]
    
    first_line = unlist(strsplit(unlist(lines_split[1]), split="\\."))
    lines_retained = c(unlist(strsplit(first_line[length(first_line)],split="n"))[2],lines_retained)
    
    # structure into data frame (source, target, truss number)
    truss_numbers_df = read.table(text = gsub("[[:punct:]]", " ", grep(",", lines_retained, value = TRUE)))
    
    # convert node IDs into node names
    my_names = V(g)$name
    
    truss_numbers_df_names = t(apply(truss_numbers_df,1,function(x){c(my_names[x[1]],my_names[x[2]],as.numeric(x[3]))}))
    
    # add the 2-truss (the graph itself)
    edge_list_g = get.edgelist(g)
    two_truss = cbind(edge_list_g,rep(2,nrow(edge_list_g)))
    full_truss_numbers_df_names = rbind(two_truss,truss_numbers_df_names)
    
    # compute node truss numbers (max k of the k-trusses the node belongs to)
    node_truss_numbers = unlist(lapply(my_names, function(x){max(as.numeric(full_truss_numbers_df_names[c(which(full_truss_numbers_df_names[,1]==x),which(full_truss_numbers_df_names[,2]==x)),3]))}))
    
    # for compatibility with the other subfunctions
    cores_g = node_truss_numbers
    names(cores_g) = my_names
    
  }
  
  # sort vertices by decreasing core number
  cores_g = sort(cores_g, decreasing = TRUE)
  
  output = list(cores = cores_g)
  
}