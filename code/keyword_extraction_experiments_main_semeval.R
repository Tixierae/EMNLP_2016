my_path_root = paste0(getwd(), "/keyword_extraction")

######################
##### packages #######
######################

lapply(list("foreach","doParallel","igraph","tm","SnowballC","stringr","openNLP"), library, character.only=TRUE)

######################
##### functions ######
######################

lapply(list("get_elbow_point.R","best_level_density.R","keyword_extraction.R","my_metrics.R","tagPOS.R","cleaning.text.R","from_terms_to_keywords.R","heapify.R","from_terms_to_graph.R","assign_attributes_to_graph_initial.R","assign_attributes_to_graph_nocomm.R","cores_dec.R"), function(x) {source(paste0(my_path_root,"/code/",x))})

########################
##### experiments ######
########################

# all data sets can be found here: https://github.com/snkim/AutomaticKeyphraseExtraction

# * start

# read the SMART list that was used in the coreword paper (also accessible with tm package stopwords("en"))
my_stopwords = readLines("http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")

# * end

#################
#    Semeval    #
#################

# data can be found here: https://github.com/boudinfl/centrality_measures_ijcnlp13

unique_names_candidates = list.files(paste0(my_path_root,"/data/semeval"))
unique_names = unique(unlist(lapply(unique_names_candidates, function(x) unlist(strsplit(x, split="\\."))[1])))

# remove last element (name of the folder "references")

unique_names = head(unique_names, -1)

write.csv(unique_names, file = paste0(my_path_root,"/data/output/semeval/unique_names.csv"), row.names=FALSE)

unique_names = as.character(read.csv(paste0(my_path_root,"/data/output/semeval/unique_names.csv"), header=TRUE)[,1])

all_keywords = readLines(paste0(my_path_root,"/data/semeval/references/semeval.ref"))
all_keywords_names = lapply(all_keywords, function(x) unlist(strsplit(x, split="\\."))[1])

abstracts = list()
keywords = list()
i = 1

for (name in unique_names){
  
  match_index = which(all_keywords_names==name)
  
  # keywords
  all_keywords_temp = unlist(strsplit(all_keywords[match_index], split="\t"))[2]
  all_keywords_temp = unlist(strsplit(all_keywords_temp, split=";"))
  keywords[[i]] = unique(unlist(lapply(all_keywords_temp, function(x) unlist(strsplit(x, split=" ")))))
  
  # documents
  doc_lines = readLines(paste0(my_path_root,"/data/semeval","/",name,".txt.pre"))
  
  doc_lines_to_save = list()
  
  k = 1
  
  for (doc_line in doc_lines){
  
  doc_line_tokens = unlist(strsplit(doc_line, split=" "))
    
  selected_tokens = unlist(lapply(doc_line_tokens, function(x){
  
    splitted_token = unlist(strsplit(x, split="/"))
    # keep only nouns and adjectives
    if (splitted_token[2]%in%c("jj","nn","jjs","nns","nnp")){
      return(splitted_token[1])
    }
  
  }
  ))
    
  doc_lines_to_save[[k]] = paste(selected_tokens, collapse=" ")
  k = k + 1
  
  }
  
  abstracts[[i]] = paste(doc_lines_to_save, collapse=" ")
  
  i = i + 1
  
  if (i %% 10 == 0){
    print(i)
  }
  
}

save(abstracts, file = paste0(my_path_root,"/data/output/semeval/processed_abstracts"))
save(keywords, file = paste0(my_path_root,"/data/output/semeval/processed_keywords"))

# * start

load(paste0(my_path_root,"/data/output/semeval/processed_abstracts"))
load(paste0(my_path_root,"/data/output/semeval/processed_keywords"))

# compute length of abstracts in number of words
lengthes_abstracts_semeval = unlist(lapply(abstracts, function(x) length(unlist(strsplit(x, split=" ")))))

boxplot(lengthes_abstracts_semeval)
save(lengthes_abstracts_semeval, file = paste0(my_path_root,"/data/output/semeval/size_abstracts_semeval"))

################ clean abstracts

terms_lists = list()
i = 1

for (abstract in abstracts){
  
  terms_lists[[i]] = cleaning.text(X=abstract,custom=my_stopwords,pos=FALSE,stem=TRUE)
  
  i = i+1
  if (i %% 20 == 0){
    print(i)
  }
  
}

save(terms_lists, file = paste0(my_path_root,"/data/output/semeval/terms_lists"))

load(paste0(my_path_root,"/data/output/semeval/terms_lists"))

################ build graphs for each abstract and all window sizes

# w corresponds to (real window size - 2)

w_min = 1
w_max = 20

ptm = proc.time()
nc = detectCores()
cl = makeCluster(nc)
registerDoParallel(cl)

all_graphs = foreach(kk = 1:length(terms_lists), .packages = c("igraph","hash"), .export = c("w_min","w_max","terms_lists","heapify","from_terms_to_graph","assign_attributes_to_graph_initial")) %dopar% {     
  
  terms_list = terms_lists[[kk]]
  
  graphs_windows = list()
  j = 1
  
  for (my_window in (w_min+2):(w_max+2)){
    
    # build graph
    edges_df = from_terms_to_graph(terms_list = terms_list, w = my_window, overspan = TRUE, processed = TRUE)$output
    graphs_windows[[j]] = assign_attributes_to_graph_initial(edges_df, weighted = TRUE)
    j = j+1
    
  }
  
  graphs_windows
  
}  

stopCluster(cl)
proc.time() - ptm

save(all_graphs, file = paste0(my_path_root,"/data/output/semeval/all_graphs"))

load(paste0(my_path_root,"/data/output/semeval/all_graphs"))

################ collect statistics about each graph

################ perform weighted k-core and k-truss decomposition of the graphs

# set working directory to where the k_truss .exe files are
setwd(paste0(my_path_root,"/code"))

ptm = proc.time()

cl = makeCluster(nc)
registerDoParallel(cl)

decomposed_graphs = foreach(kk = 1:length(all_graphs), .packages = c("igraph"), .export = c("w_min","w_max","heapify","assign_attributes_to_graph_initial","assign_attributes_to_graph_nocomm","cores_dec", "all_graphs")) %dopar% {
  
  # each element of "my_graph" contains, for a given abstract, the graphs for all window sizes 
  my_graph = all_graphs[[kk]]
  
  g_dec_windows = list()
  
  l = 1
  
  for (w in w_min:w_max){
    
    g = my_graph[[w]]$g
    v_g_name = my_graph[[w]]$v_g_name
    l_v_g_name = my_graph[[w]]$l_v_g_name
    
    # decompose graph
    g_k_core = assign_attributes_to_graph_nocomm(g, "weighted_k_core", "all", v_g_name, l_v_g_name)$g
    g_k_truss = assign_attributes_to_graph_nocomm(g, "k_truss", "all", v_g_name, l_v_g_name)$g
    
    g_dec_windows[[l]] =   list(g_k_core, g_k_truss)
    
    l = l + 1
    
  }
  
  g_dec_windows
  
}

stopCluster(cl)
proc.time() - ptm

save(decomposed_graphs, file = paste0(my_path_root,"/data/output/Hulth2003/decomposed_graphs"))


################ extract keywords for each method and each window size

load(paste0(my_path_root,"/data/output/Hulth2003/decomposed_graphs_hulth"))

# decomposed_graphs[[abstract]][[window]][[degeneracy_method (core or truss)]]

w_min = 1
w_max = 12

ptm = proc.time()

cl = makeCluster(nc)
registerDoParallel(cl)

all_keywords_hulth_final = foreach(kk = 1:length(decomposed_graphs), .packages = c("igraph"), .export = c("w_min","w_max","keyword_extraction","decomposed_graphs","get_elbow_point","best_level_density")) %dopar% {
  
  my_graph = decomposed_graphs[[kk]]
  
  keywords_graph = list()
  
  for (w in w_min:w_max){
    
    my_graph_window = my_graph[[w]]
    
    keywords_window = list()
    
    for (core_or_truss in 1:2){
      
      g = my_graph_window[[core_or_truss]]
      
      # extract keywords
      
      # TextRank percentage
      trp = keyword_extraction(g, "TR", use_elbow=FALSE, use_percentage=TRUE, percentage=0.33, number_to_retain=NA, which_nodes=NA)$e
      
      # TextRank elbow
      tre = keyword_extraction(g, "TR", use_elbow=TRUE, use_percentage=NA, percentage=NA, number_to_retain=NA,  which_nodes=NA)$e
      
      # main core
      main = keyword_extraction(g, "main", use_elbow=NA, use_percentage=NA, percentage=NA, number_to_retain=NA,  which_nodes=NA)$e
      
      # inflexion method
      inf = keyword_extraction(g, "inf", use_elbow=NA, use_percentage=NA, percentage=NA, number_to_retain=NA,  which_nodes=NA)$e
      
      # CoreRank percentage and elbow
      crpe = keyword_extraction(g, "CR", use_elbow=NA, use_percentage=NA, percentage=0.33, number_to_retain=NA, which_nodes="all")$e
      
      # density (always with elbow)
      dens = keyword_extraction(g, "dens", use_elbow=NA, use_percentage=NA, percentage=NA, number_to_retain=NA, which_nodes=NA)$e
      
      # save results
      keywords_window[[core_or_truss]] = list(trp, tre, main, inf, crpe, dens)
      
    }
    
    keywords_graph[[w]] =  keywords_window
    
  }
  
  keywords_graph
  
}

stopCluster(cl)
proc.time() - ptm

save(all_keywords_hulth_final, file = paste0(my_path_root,"/data/output/Hulth2003/all_keywords_hulth_final"))

################ compute core distributions, and density at each hierarchy level for each graph and each window size

# decomposed_graphs[[abstract]][[window]][[core or truss]]

ptm = proc.time()

cl = makeCluster(nc)
registerDoParallel(cl)

core_dens_distributions = foreach(kk = 1:length(decomposed_graphs), .packages = c("igraph"), .export = c("w_min","w_max","decomposed_graphs")) %dopar% {
  
  my_graph = decomposed_graphs[[kk]]
  
  core_truss_no_windows = list()
  density_level_windows = list()
  
  for (w in w_min:w_max){
    
    my_graph_window = my_graph[[w]]
    
    my_graph_window_core = my_graph_window[[1]]
    my_graph_window_truss = my_graph_window[[2]]
    
    core_no =  V(my_graph_window_core)$core_no
    truss_no =  V(my_graph_window_truss)$core_no  # even though the name is "core_no" it is the truss numbers that are returned
    
    core_truss_no_windows[[w]] = list(core_no, truss_no)
    
    density_level_windows_levels_core = list()
    i = 1
    for (level in levels(as.factor(core_no))){
      density_level_windows_levels_core[[i]] = graph.density(induced_subgraph(my_graph_window_core, which(V(my_graph_window_core)$core_no>=as.numeric(level))))
      i = i + 1
    }
    
    density_level_windows_levels_core = unlist(density_level_windows_levels_core)
    names(density_level_windows_levels_core) = levels(as.factor(core_no))
    
    density_level_windows_levels_truss = list()
    i = 1
    for (level in levels(as.factor(truss_no))){
      density_level_windows_levels_truss[[i]] = graph.density(induced_subgraph(my_graph_window_truss, which(V(my_graph_window_truss)$core_no>=as.numeric(level))))
      i = i + 1
    }
    
    density_level_windows_levels_truss = unlist(density_level_windows_levels_truss)
    names(density_level_windows_levels_truss) = levels(as.factor(truss_no))
    
    density_level_windows[[w]] = list(density_level_windows_levels_core, density_level_windows_levels_truss)
    
  }
  
  list(core_truss_no_windows, density_level_windows)
  
}

stopCluster(cl)
proc.time() - ptm

save(core_dens_distributions, file = paste0(my_path_root,"/data/output/Hulth2003/core_dens_distributions_hulth"))


# core_dens_distributions[[abstract]][[core/truss number or density distributions]][[window size]][[core or truss]]

load(paste0(my_path_root,"/data/output/Hulth2003/core_dens_distributions_hulth"))

core_or_truss_name_bis = c("core","truss")
i = 1

for (core_or_truss in 1:2){
  
  pdf(paste0(my_path_root,"/data/output/Hulth2003/",core_or_truss_name_bis[[i]],"_distributions_hulth.pdf"),width=10,height=17,paper="a4")
  
  par(mfrow=c(3,2),oma=c(0,0,2,0))
  
  mtext(paste0(core_or_truss_name_bis[[i]], " distribution, Hulth 2003"), outer=TRUE)
  
  for(w in w_min:w_max){
    
    hist(unlist(lapply(core_dens_distributions, function(x) x[[1]][[w]][[core_or_truss]])), main=paste0("window of size ", w+2), xlab=paste0(core_or_truss_name_bis[[i]], " number"), ylab="probability", prob=TRUE)
    
  }
  
  
  
  dev.off()
  
  i = i +1
  
}


# plot density evolution for each core
my_xlim = c(1,25)
my_ylim = c(0,0.8)

core_or_truss_name = c("core","truss")
i = 1

for (core_or_truss in 1:2){
  
  pdf(paste0(my_path_root,"/data/output/Hulth2003/",core_or_truss_name[[i]],"_density_evolution_hulth.pdf"),width=10,height=17,paper="a4")
  
  par(mfrow=c(3,2))
  
  abstract = 1
  w = 1
  for (w in w_min:w_max){
    
    temp = core_dens_distributions[[abstract]][[2]][[w]][[core_or_truss]] 
    
    plot(as.numeric(names(temp)), temp, main = paste0("window of size ", w+2), xlab = paste0(core_or_truss_name[[i]]), ylab="density", type="l", xlim = my_xlim, ylim = my_ylim, col="light grey", lwd=0.5)
    
    for (abstract in 2:length(abstracts)){
      
      temp = core_dens_distributions[[abstract]][[2]][[w]][[core_or_truss]] 
      
      lines(as.numeric(names(temp)), temp, col="light grey", lwd=0.5)
      
    }
    
  }
  
  dev.off()
  
  i = i +1
  
}


load(paste0(my_path_root,"/data/output/semeval/all_keywords_semeval"))

length(all_keywords_semeval[[1]][[1]][[1]])


################ compute scores for each method and each window size

load(paste0(my_path_root,"/data/output/Hulth2003/all_keywords_hulth_new_2"))

# all_keywords_hulth[[abstract]][[core or truss]][[window size]][[method]]

# all_keywords_hulth_new[[abstract]][[window]][[core or truss]][[method]]

# if [[method==4]]: c("all","in","out") * seq(0.1,0.6,by=0.1) for a total length of 18

# all_keywords_hulth_final[[abstract]][[window]][[core or truss]][[method]]

# method: trp, tre, main, inf, crpe(list crp, cre), dens

# if method==5, percentage then rank

load(paste0(my_path_root,"/data/output/Hulth2003/all_keywords_hulth_final"))


load(paste0(my_path_root,"/data/output/semeval/processed_keywords"))

all_scores = list()
# best_indexes = list()

i = 1

for (all_keyword in all_keywords_semeval){
  
  # for a given abstract...
  scores_temp_core = list()
  scores_temp_truss = list()
  
  golden_keywords = unique(tolower(wordStem(keywords[[i]])))
  
  for (w in w_min:w_max){
    
    # ... and for a given window size, compute scores for each method: trp, tre, main, inf, crpe(list crp, cre), dens
    # "my_metrics" returns in that order: precision, recall, and F1-score
    temp_list_core_1 = my_metrics(all_keyword[[w]][[1]][[1]][1:15], golden_keywords)
    temp_list_truss_1 = my_metrics(all_keyword[[w]][[2]][[1]][1:15], golden_keywords)
    
    temp_list_core_2_0 = lapply(all_keyword[[w]][[1]][2:4], function(x) my_metrics(x, golden_keywords))
    temp_list_truss_2_0 = lapply(all_keyword[[w]][[2]][2:4], function(x) my_metrics(x, golden_keywords))
    
    temp_list_core_2_a = my_metrics(all_keyword[[w]][[1]][[5]][[1]][1:15], golden_keywords)
    temp_list_truss_2_a = my_metrics(all_keyword[[w]][[2]][[5]][[1]][1:15], golden_keywords)
    
    temp_list_core_2_b = my_metrics(all_keyword[[w]][[1]][[5]][[2]], golden_keywords)
    temp_list_truss_2_b = my_metrics(all_keyword[[w]][[2]][[5]][[2]], golden_keywords)
    
    temp_list_core_3 = my_metrics(all_keyword[[w]][[1]][[6]], golden_keywords)
    temp_list_truss_3 = my_metrics(all_keyword[[w]][[2]][[6]], golden_keywords)
    
    scores_temp_core[[w]] = c(list(temp_list_core_1),temp_list_core_2_0,list(temp_list_core_2_a),list(temp_list_core_2_b),list(temp_list_core_3))
    scores_temp_truss[[w]] = c(list(temp_list_truss_1),temp_list_truss_2_0,list(temp_list_truss_2_a),list(temp_list_truss_2_b),list(temp_list_truss_3))
    
  }
  
  all_scores[[i]] = list(scores_temp_core, scores_temp_truss)
  
  if (i%%20==0){
    print(i)
  }
  
  i = i + 1
  
}

save(all_scores, file=paste0(my_path_root,"/data/output/semeval/all_scores_final_to-number"))

# save(best_indexes, file=paste0(my_path_root,"/data/output/Hulth2003/all_scores_hulth_new_2_best_indexes"))


# all_keywords_hulth_density_elbow[[abstract]][[core or truss]][[window size]]

all_scores_density_elbow = list()

i = 1

for (all_keyword in all_keywords_hulth_density_elbow){
  
  # for a given abstract...
  scores_temp_core = list()
  scores_temp_truss = list()
  
  for (w in w_min:w_max){
    # ... and for a given window size, compute scores for each of the 6 methods
    # "my_metrics" returns in that order: precision, recall, and F1-score
    scores_temp_core[[w]] = my_metrics(all_keyword[[1]][[w]], unique(tolower(wordStem(keywords[[i]]))))
    scores_temp_truss[[w]] = my_metrics(all_keyword[[2]][[w]], unique(tolower(wordStem(keywords[[i]]))))
  }
  
  if (i%%20==0){
    print(i)
  }
  
  all_scores_density_elbow[[i]] = list(scores_temp_core, scores_temp_truss)
  
  i = i + 1
  
}

save(all_scores_density_elbow, file=paste0(my_path_root,"/data/output/Hulth2003/all_scores_density_elbow_hulth_new"))


# all_scores[[abstract]][[core or truss]][[window size]][[method]][[precision, recall and F1 score]]

load(paste0(my_path_root,"/data/output/Hulth2003/all_scores_hulth_new_2"))
load(paste0(my_path_root,"/data/output/Hulth2003/all_scores_hulth_new_2_best_indexes"))

# all_scores[[abstract]][[core or truss]][[window]][[method]][[prec, rec, F1 score]]
# best_indexes[[abstract]][[window]]c(core or truss)

# for k-core
for (w in w_min:w_max){
  print(summary(as.factor(unlist(lapply(best_indexes, function(x) x[[w]][1])))))
}

# it seems that it is always 1 which gives the best results (regardless of window size)
# 1 corresponds to "all", my_weight = 0.1

# for k-truss
for (w in w_min:w_max){
  print(summary(as.factor(unlist(lapply(best_indexes, function(x) x[[w]][2])))))
}

# again, it seems that it is always 1 which gives the best results (regardless of window size)
# 1 corresponds to "all", my_weight = 0.1

# so we need to recompute all the scores for these values of parameters


all_scores_fair = list()

i = 1

for (all_keyword in all_keywords_hulth_new){
  
  # for a given abstract...
  scores_temp_core = list()
  scores_temp_truss = list()
  
  golden_keywords = unique(tolower(wordStem(keywords[[i]])))
  
  for (w in w_min:w_max){
    
    temp_list_cr_core = lapply(all_keyword[[w]][[1]][[4]], my_metrics, golden_keywords)
    temp_list_cr_truss = lapply(all_keyword[[w]][[2]][[4]], my_metrics, golden_keywords)
    
    # return F1 scores
    f1_scores_core = unlist(lapply(temp_list_cr_core, function(x) x[[3]]))
    f1_scores_truss = unlist(lapply(temp_list_cr_truss, function(x) x[[3]]))
    
    scores_temp_core[[w]] = c(lapply(all_keyword[[w]][[1]][1:3], my_metrics, golden_keywords), temp_list_cr_core[1])
    scores_temp_truss[[w]] = c(lapply(all_keyword[[w]][[2]][1:3], my_metrics, golden_keywords), temp_list_cr_truss[1])
    
  }
  
  if (i%%20==0){
    print(i)
  }
  
  all_scores_fair[[i]] = list(scores_temp_core, scores_temp_truss)
  
  i = i + 1
  
}


load(paste0(my_path_root,"/data/output/Hulth2003/all_scores_hulth_final"))

# all_scores[[abstract]][[core or truss]][[window]][[method]][[prec, rec, F1]]

scores_method = list()

for (method in 1:7){
  
  scores_window_core = list()
  scores_window_truss = list()
  
  for (w in w_min:w_max){
    
    scores_window_metric_core = list()
    scores_window_metric_truss = list()
    
    for (metric in 1:3){
      
      scores_window_metric_core[[metric]] = mean(unlist(lapply(all_scores, function(x) x[[1]][[w]][[method]][[metric]])))
      scores_window_metric_truss[[metric]] = mean(unlist(lapply(all_scores, function(x) x[[2]][[w]][[method]][[metric]])))
      
    }
    
    scores_window_core[[w]] = scores_window_metric_core
    scores_window_truss[[w]] = scores_window_metric_truss
    
  }
  
  scores_method[[method]] = list(scores_window_core, scores_window_truss)
  
}

# scores_method[[method]][[core or truss]][[window]][[prec, rec, F1]]

my_ylim = list(c(0.45, 0.75), c(0.35, 0.75), c(0.4, 0.6))
my_lty = rep(c(1,2,4,5,6),2)
my_palette = rainbow(n=length(method_names))
core_or_truss = c("weighted k-core", "k-truss")
metrics = c("precision","recall","F1-score")
method_names = c("TRP", "TRE", "main", "inf", "CRP", "CRE", "dens")

for (i in 1:length(core_or_truss)){
  
  name = core_or_truss[i]
  
  pdf(paste0(my_path_root,"/data/output/Hulth2003/comparison_final_",name,".pdf"),width=15,height=15,paper="a4")
  
  par(mfrow=c(2,2),oma =c(0,0,2,0),mar=c(5,2,5,2))
  
  for (j in 1:length(metrics)){
    
    metric = metrics[j]
    
    y_values = list()
    for (w in w_min:w_max){
      y_values[[w]] = scores_method[[1]][[i]][[w]][[j]]
    }
    
    plot(w_min:w_max+2, y_values, main = paste0(" average ",metric), xlab="window size", ylab=paste0(metric), type="l", lty=my_lty[1], ylim=my_ylim[[j]], lwd=2, col = my_palette[1])
    grid(lwd=2)
    
    for(k in 2:length(method_names)){
      
      y_values = list()
      for (w in w_min:w_max){
        y_values[[w]] = scores_method[[k]][[i]][[w]][[j]]
      }
      
      lines(w_min:w_max+2, y_values, lty=my_lty[k], lwd=2, col=my_palette[k])
      
    }
    
  }
  
  plot.new()
  legend("topleft", legend = method_names, lty=my_lty, col=my_palette, lwd=2)
  
  mtext(paste0(name,", Hulth 2003"), outer=TRUE)
  
  dev.off()
  
}

################ compare performance across methods and window sizes - in terms of F1-score. Conduct hypothesis testing

k = 3 # (for F1-score)

# conduct hypothesis testing for statistical significance
method_names = c("TR", "W K-C", "K-T", "K-T INF", "W K-C INF")

statistical_tests = list()

for (w in w_min:w_max){
  
  statistical_tests_temp = matrix(nrow=5, ncol=5)
  colnames(statistical_tests_temp) = method_names
  rownames(statistical_tests_temp) = colnames(statistical_tests_temp)
  
  for (i in 1:nrow(statistical_tests_temp)){
    
    for (j in 1:ncol(statistical_tests_temp)){
      
      if (i!=j){
        
        # Mann-Whitney U test. The Null hypothesis is: "the difference in the two population mean ranks is due to random sampling, assuming that the two populations have identical distributions"
        # one-sided (alternative="greater"): the first population mean (x, row) is greater to the second population mean (y, column) due to chance
        statistical_tests_temp[i,j] = round(wilcox.test(x=as.numeric(my_df_save[[k]][[w]][method_names[i]][,1]), y=as.numeric(my_df_save[[k]][[w]][method_names[j]][,1]), paired=FALSE, alternative="greater")$p.value, 5)
        
      } else {
        
        statistical_tests_temp[i,j] = NA
        
      }
      
    }
    
  }
  
  statistical_tests[[w]] = statistical_tests_temp
  
}

save(statistical_tests, file = paste0(my_path_root,"/data/output/Hulth2003/statistical_tests"))

# find best performing method for each window size

average_scores_df = list()

for (my_sub_scores_index in my_sub_scores_indexes){
  
  average_scores_df_temp = matrix(nrow=(w_max - w_min +1), ncol = length(method_names))
  colnames(average_scores_df_temp) = method_names
  rownames(average_scores_df_temp) = paste0("window size = ", w_min:w_max+2)
  
  for (w in 1:w_max){
    
    average_scores_df_temp[w,] = apply(my_df_save[[my_sub_scores_index]][[w]], 2, mean)
    
  }
  
  write.csv(average_scores_df_temp, file=paste0(my_path_root,"/data/output/Hulth2003/average_",my_sub_scores[my_sub_scores_index],"_evolution_with_window_size.csv"))
  
  average_scores_df[[my_sub_scores_index]] = average_scores_df_temp
  
}


# open PDF device
pdf(paste0(my_path_root,"/data/output/Hulth2003/metrics_evolution.pdf"),width=15,height=15,paper="a4r")

n_row = nrow(average_scores_df[[1]])
my_ylims = list(c(0.45,0.7),c(0.35,0.75),c(0.38,0.58))

par(mfrow=c(2,2))

for (my_sub_scores_index in my_sub_scores_indexes){
  
  df_temp = average_scores_df[[my_sub_scores_index]]
  metric_temp = my_sub_scores[my_sub_scores_index]
  
  plot(x = 1:n_row, y = df_temp[,1], ylab=paste0("average ",metric_temp), xlab="window size", main=paste0("average ",metric_temp," evolution with window size"), ylim=my_ylims[[my_sub_scores_index]], type = "l", lty = 1, lwd = 2, xaxt = "n")
  
  axis(1, at=1:n_row, labels=as.character(1:n_row+2))
  
  for (i in 2:length(method_names)){
    
    lines(x = 1:n_row, y = df_temp[,i], lty = i, lwd = 2)
    
  }
  
  grid()
  
}

plot.new()
legend("topleft", legend = method_names, lty = 1:length(method_names), lwd = rep(2, length(method_names)))

# close plotting device
dev.off()

# # area under the curves plot
# pdf(paste0(my_path_root,"/data/output/Hulth2003/auc.pdf"),width=15,height=15,paper="a4r")
# 
# prec_df = read.csv(paste0(my_path_root,"/data/output/Hulth2003/average_precision_evolution_with_window_size.csv"))
# rec_df = read.csv(paste0(my_path_root,"/data/output/Hulth2003/average_recall_evolution_with_window_size.csv"))
# 
# for (w in w_min:w_max){
#   
#   plot(x=as.numeric(rec_df[,2]) , y=as.numeric(prec_df[,2]), lty = 1, lwd = 2)
#   
#   lines(x=rec_df[,i] , y=prec_df[,i], lty = i, lwd = 2)
#   
#   prec_temp = average_scores_df[[1]]
#   rec_temp = average_scores_df[[2]]
#   
# }
# 
# dev.off()