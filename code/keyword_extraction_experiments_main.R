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

####################
#    Hulth 2003    #
####################

# since our approach is unsupervised, we use the test set (500 abstracts) of the Hulth 2003 data set like in the coreword paper and the TextRank paper

unique_names_candidates = list.files(paste0(my_path_root,"/data/Hulth2003/validation"))
unique_names = unique(unlist(lapply(unique_names_candidates, function(x) unlist(strsplit(x, split="\\."))[1])))

write.csv(unique_names, file = paste0(my_path_root,"/data/output/Hulth2003/unique_names.csv"), row.names=FALSE)

unique_names = as.character(read.csv(paste0(my_path_root,"/data/output/Hulth2003/unique_names.csv"), header=FALSE)[,1])

abstracts = list()
keywords = list()
i = 1

for (name in unique_names){
  
  keyword_candidates = readLines(paste0(my_path_root,"/data/Hulth2003/validation","/",name,".uncontr"))
  keyword_candidates = str_trim(gsub("[\t]"," ",keyword_candidates))
  
  keywords_temp = unlist(lapply(keyword_candidates, function(x) {
    
    # remove \t at beginning of new line and remove leading and trailing whitespace
    x = str_trim(gsub("[\t]"," ", x))
    
    # split keyphrases into keywords (keeping intra-word dashes)
    x = unlist(strsplit(x, split=";"))
    x = unlist(strsplit(x, split=" "))
    
    # remove empty elements
    x = x[which(nchar(x)>1)]
    
    # convert to lower case
    x = tolower(x)
    
  }))
  
  keywords[[i]] = keywords_temp
  
  abstract_lines = readLines(paste0(my_path_root,"/data/Hulth2003/validation","/",name,".abstr"))
  # remove \t at beginning of new line 
  abstract_lines_cleaned = unlist(lapply(abstract_lines, function(x) str_trim(gsub("[\t]"," ", x))))
  # include title as a separate sentence (the first one)
  abstract_body = paste(abstract_lines_cleaned[2:length(abstract_lines_cleaned)], collapse = " ")
  abstract_temp = paste(abstract_lines_cleaned[1], abstract_body, sep=". ")
  
  abstracts[[i]] = abstract_temp
  
  i = i+1
  
  if (i %% 10 == 0){
    print(i)
  }
  
}

save(abstracts, file = paste0(my_path_root,"/data/output/Hulth2003/processed_abstracts"))
save(keywords, file = paste0(my_path_root,"/data/output/Hulth2003/processed_keywords"))

# * start

load(paste0(my_path_root,"/data/output/Hulth2003/processed_abstracts"))
load(paste0(my_path_root,"/data/output/Hulth2003/processed_keywords"))

abstracts_hulth=abstracts
keywords_hulth=keywords

load(paste0(my_path_root,"/data/output/Marujo2012/processed_abstracts"))
load(paste0(my_path_root,"/data/output/Marujo2012/processed_keywords"))

abstracts_mar=abstracts
keywords_mar=keywords

load(paste0(my_path_root,"/data/output/Semeval/processed_abstracts"))
load(paste0(my_path_root,"/data/output/Semeval/processed_keywords"))

abstracts_sem=abstracts
keywords_sem=keywords

to_plot_ab = list(unlist(lapply(abstracts_hulth, function(x) length(unlist(strsplit(x, split=" "))))),unlist(lapply(abstracts_mar, function(x) length(unlist(strsplit(x, split=" "))))),unlist(lapply(abstracts_sem, function(x) length(unlist(strsplit(x, split=" "))))))
to_plot_ke = list(unlist(lapply(keywords_hulth, function(x) length(unlist(strsplit(x, split=" "))))),unlist(lapply(keywords_mar, function(x) length(unlist(strsplit(x, split=" "))))),unlist(lapply(keywords_sem, function(x) length(unlist(strsplit(x, split=" "))))))

unlist(lapply(to_plot_ke, mean))/unlist(lapply(to_plot_ab, mean))

pdf(paste0(my_path_root,"/data/output/comparison_size_number_final.pdf"),width=7,height=5,paper="a4r")

par(mfrow=c(1,2),par(mar=c(8,4,8,4)))

boxplot(to_plot_ab, outline = FALSE, xaxt="n",boxwex=0.5, cex.axis=0.7)
grid()
par(new=TRUE)
boxplot(to_plot_ab, outline = FALSE, xaxt="n", ylab="number of words", main="document size distributions",boxwex=0.5,col="white",cex.main=0.8,cex.axis=0.7)
axis(1, at=1:3, labels=c("Hulth2003","Marujo2012","Semeval"), cex.axis=0.7, cex.lab=0.7)

boxplot(to_plot_ke, outline = FALSE, xaxt="n",boxwex=0.5, cex.axis=0.7)
grid()
par(new=TRUE)
boxplot(to_plot_ke, outline = FALSE, xaxt="n", ylab="number of keywords", main="number of manually assigned keyword \n distributions",boxwex=0.5,col="white",cex.main=0.8,cex.axis=0.7)
axis(1, at=1:3, labels=c("Hulth2003","Marujo2012","Semeval"), cex.axis=0.7, cex.lab=0.7)

dev.off()



# compute length of abstracts in number of words
lengthes_abstracts_hulth = unlist(lapply(abstracts, function(x) length(unlist(strsplit(x, split=" ")))))
save(lengthes_abstracts_hulth, file = paste0(my_path_root,"/data/output/Hulth2003/size_abstracts_Hulth"))

load(paste0(my_path_root,"/data/output/Hulth2003/size_abstracts_Hulth"))

# do a common boxplot at the end showing the length distributions of documents from all data sets
# correlation between abstract size and number of keywords manually assigned
number_keywords_human_hulth = unlist(lapply(keywords, function(x) length(unique(wordStem(x)))))

# open PDF device
pdf(paste0(my_path_root,"/data/output/Hulth2003/size_number.pdf"),width=7,height=7,paper="a4r")
# do a paired plot with Krapi2009
plot(lengthes_abstracts_hulth, number_keywords_human_hulth, xlab="abstract size in words", ylab="number of keywords manually assigned", main="Hulth 2003")
# close device (saves .pdf file to working directory)
dev.off()



################ clean abstracts

terms_lists = list()
i = 1

for (abstract in abstracts){
  
  terms_lists[[i]] = cleaning.text(X=abstract,custom=my_stopwords,pos=TRUE,stem=TRUE)
  
  i = i+1
  if (i %% 20 == 0){
    print(i)
  }
  
}

# save(terms_lists, file = paste0(my_path_root,"/data/output/Hulth2003/terms_lists"))

# * start

load(paste0(my_path_root,"/data/output/Hulth2003/terms_lists"))


################ build graphs for each abstract and all window sizes

# w corresponds to (real window size - 2)

w_min = 1
w_max = 12

ptm = proc.time()
nc = detectCores()
cl = makeCluster(nc)
registerDoParallel(cl)

all_graphs = foreach(kk = 1:length(terms_lists), .packages = c("igraph"), .export = c("w_min","w_max","terms_lists","heapify","from_terms_to_graph","assign_attributes_to_graph_initial")) %dopar% {     
  
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

save(all_graphs, file = paste0(my_path_root,"/data/output/Hulth2003/all_graphs"))

load(paste0(my_path_root,"/data/output/Hulth2003/all_graphs"))

################ collect statistics about each graph

ptm = proc.time()
cl = makeCluster(nc)
registerDoParallel(cl)

all_graphs_stats = foreach(kk = 1:length(all_graphs), .packages = c("igraph"), .export = c("w_min","w_max","all_graphs")) %dopar% {     
  
  my_graph = all_graphs[[kk]]
  
  graphs_stats = list()
  
  for (j in w_min:w_max){
    
    my_graph_window = my_graph[[j]]$g
    
    graphs_stats[[j]] = list(vertice_count = vcount(my_graph_window), edge_count = ecount(my_graph_window), density = round(ecount(my_graph_window)/(vcount(my_graph_window)*(vcount(my_graph_window)-1)),4), triangle_count = length(triangles(my_graph_window)))
    
  }
  
  graphs_stats
  
}

stopCluster(cl)
proc.time() - ptm

save(all_graphs_stats, file = paste0(my_path_root,"/data/output/Hulth2003/all_graphs_stats_final"))

################ plot statistics

load(paste0(my_path_root,"/data/output/Hulth2003/all_graphs_stats_final"))

load(paste0(my_path_root,"/data/output/Hulth2003/all_graphs"))

# all_graphs_stats[[abstract]][[window]][[metric]]

# open PDF device
pdf(paste0(my_path_root,"/data/output/Hulth2003/graph_metrics_final.pdf"),width=10,height=10,paper="a4")

par(mfrow=c(4,2))
par(mar=c(2,3.5,2,3.5), oma=c(0,0,2,0))

metric_names = c("vertice count", "edge count", "density", "triangle count")
metric=1
for (k in 1:8){
  
  if (k%%2==0){
    plot.new()
  } else {
  
  if (metric == 1){
    
    boxplot(unlist(lapply(all_graphs_stats, function(x) x[[1]][[metric]])), main = metric_names[1], outline=FALSE)
    grid()
    
  } else {
  
to_plot = data.frame(matrix(nrow=length(all_graphs), ncol=(w_max-w_min+1)))

for (col in 1:ncol(to_plot)){

to_plot[,col] = unlist(lapply(all_graphs_stats, function(x) x[[col]][[metric]]))

}

boxplot(to_plot, main = metric_names[metric], xlab="window size", xaxt = "n", outline=FALSE)
axis(side=1, at=1:12, labels=as.character(1:12+2))
grid()

  }
    metric = metric + 1
}
}

mtext("Hulth 2003", outer=TRUE)

dev.off()


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

all_keywords_hulth_final_dens = foreach(kk = 1:length(decomposed_graphs), .packages = c("igraph"), .export = c("w_min","w_max","keyword_extraction","decomposed_graphs","get_elbow_point","best_level_density")) %dopar% {
  
  my_graph = decomposed_graphs[[kk]]
  
  keywords_graph = list()
  
  for (w in w_min:w_max){
    
    my_graph_window = my_graph[[w]]
    
    keywords_window = list()
    
    for (core_or_truss in 1:2){
      
      g = my_graph_window[[core_or_truss]]
      
      # extract keywords
      
#       # TextRank percentage
#       trp = keyword_extraction(g, "TR", use_elbow=FALSE, use_percentage=TRUE, percentage=0.33, number_to_retain=NA, which_nodes=NA)$e
#       
#       # TextRank elbow
#       tre = keyword_extraction(g, "TR", use_elbow=TRUE, use_percentage=NA, percentage=NA, number_to_retain=NA,  which_nodes=NA)$e
#       
#       # main core
#       main = keyword_extraction(g, "main", use_elbow=NA, use_percentage=NA, percentage=NA, number_to_retain=NA,  which_nodes=NA)$e
#       
#       # inflexion method
#       inf = keyword_extraction(g, "inf", use_elbow=NA, use_percentage=NA, percentage=NA, number_to_retain=NA,  which_nodes=NA)$e
#       
#       # CoreRank percentage and elbow
#       crpe = keyword_extraction(g, "CR", use_elbow=NA, use_percentage=NA, percentage=0.33, number_to_retain=NA, which_nodes="all")$e
#       
      # density (always with elbow)
      dens = keyword_extraction(g, "dens", use_elbow=NA, use_percentage=NA, percentage=NA, number_to_retain=NA, which_nodes=NA)$e
      
#       # save results
#       keywords_window[[core_or_truss]] = list(trp, tre, main, inf, crpe, dens)
      
      keywords_window[[core_or_truss]] = dens
      
    }
    
    keywords_graph[[w]] =  keywords_window
    
  }
  
  keywords_graph
  
}

stopCluster(cl)
proc.time() - ptm

save(all_keywords_hulth_final, file = paste0(my_path_root,"/data/output/Hulth2003/all_keywords_hulth_final"))

################ for each graph and each window size: degree, core and truss number distributions

# decomposed_graphs[[abstract]][[window]][[core or truss]]

load(paste0(my_path_root,"/data/output/Hulth2003/decomposed_graphs_hulth"))

ptm = proc.time()
cl = makeCluster(nc)
registerDoParallel(cl)

distributions = foreach(kk = 1:length(decomposed_graphs), .packages = c("igraph"), .export = c("w_min","w_max","decomposed_graphs")) %dopar% {
  
  my_graph = decomposed_graphs[[kk]]
  
  distributions_window = list()
  
  for (w in w_min:w_max){
    
    my_graph_window = my_graph[[w]]
    
    core_no = V(my_graph_window[[1]])$core_no
    truss_no = V(my_graph_window[[2]])$core_no  # even though the name is "core_no" it is the truss numbers that are returned
    degrees = as.numeric(degree(my_graph_window[[1]])) # 1 or 2, does not matter
    
    distributions_window[[w]] = list(core_no, truss_no, degrees)
  }

  distributions_window
  
}

stopCluster(cl)
proc.time() - ptm

save(distributions, file = paste0(my_path_root,"/data/output/Hulth2003/distributions_hulth_final"))

load(paste0(my_path_root,"/data/output/Hulth2003/distributions_hulth_final"))

# distributions[[abstract]][[window]][[core, truss or degree]]

core_or_truss_name_bis = c("core number","truss number","degree")
i = 1

pdf(paste0(my_path_root,"/data/output/Hulth2003/",core_or_truss_name_bis[[i]],"_distributions_hulth_final.pdf"),width=10,height=17,paper="a4")
par(mfrow=c(5,2),oma=c(0,0,2,0), mar=c(4,4.5,2,4.5))

core_or_truss = 1

for (k in 1:10){
  
  if (k%%2==0){
    plot.new()
  } else {
  
    if (k==7){
      # size of the main core
    
      to_plot = data.frame(matrix(nrow=length(all_graphs), ncol=(w_max-w_min+1)))
      
      for (col in 1:ncol(to_plot)){
        
        to_plot[,col] = unlist(lapply(distributions, function(x) {
          
          decomp_temp = x[[col]][[1]]
          return(length(which(decomp_temp==max(decomp_temp))))
          
        }))
        
      }
      
      boxplot(to_plot, main = "main core size", xlab="window size", xaxt = "n", outline=FALSE)
      axis(side=1, at=1:12, labels=as.character(1:12+2))
      grid()
      
    } else {
      
      
      if (k==9){
        # size of the main truss
        
        to_plot = data.frame(matrix(nrow=length(all_graphs), ncol=(w_max-w_min+1)))
        
        for (col in 1:ncol(to_plot)){
          
          to_plot[,col] = unlist(lapply(distributions, function(x) {
            
            decomp_temp = x[[col]][[1]]
            return(length(which(decomp_temp==max(decomp_temp))))
            
          }))
          
        }
        
        boxplot(to_plot, main = "main truss size", xlab="window size", xaxt = "n", outline=FALSE)
        axis(side=1, at=1:12, labels=as.character(1:12+2))
        grid()
        
      } else {
      
      
  to_plot = data.frame(matrix(nrow=length(all_graphs), ncol=(w_max-w_min+1)))
  
  for (col in 1:ncol(to_plot)){
    
    to_plot[,col] = unlist(lapply(distributions, function(x) mean(x[[col]][[core_or_truss]])))
    
  }
  
  boxplot(to_plot, main = core_or_truss_name_bis[[i]], xlab="window size", xaxt = "n", outline=FALSE)
  axis(side=1, at=1:12, labels=as.character(1:12+2))
  grid()
  
  i = i +1
  
  core_or_truss = core_or_truss + 1
  
      }
    }
  }
  
}

mtext("average distributions Hulth 2003", outer=TRUE)

dev.off()



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


################ compute scores for each method and each window size

load(paste0(my_path_root,"/data/output/Hulth2003/all_keywords_hulth_new_2"))

# all_keywords_hulth[[abstract]][[core or truss]][[window size]][[method]]

# all_keywords_hulth_new[[abstract]][[window]][[core or truss]][[method]]

# if [[method==4]]: c("all","in","out") * seq(0.1,0.6,by=0.1) for a total length of 18

# all_keywords_hulth_final[[abstract]][[window]][[core or truss]][[method]]

# method: trp, tre, main, inf, crpe(list crp, cre), dens

# if method==5, percentage then rank

load(paste0(my_path_root,"/data/output/Hulth2003/all_keywords_hulth_final"))

all_scores_dens = list()
# best_indexes = list()

i = 1

for (all_keyword in all_keywords_hulth_final_dens){
  
  # for a given abstract...
  scores_temp_core = list()
  scores_temp_truss = list()
  
  golden_keywords = unique(tolower(wordStem(keywords[[i]])))
  
  for (w in w_min:w_max){
    
    # ... and for a given window size, compute scores for each method: trp, tre, main, inf, crpe(list crp, cre), dens
    # "my_metrics" returns in that order: precision, recall, and F1-score
#     temp_list_core_1 = lapply(all_keyword[[w]][[1]][1:4], my_metrics, golden_keywords)
#     temp_list_truss_1 = lapply(all_keyword[[w]][[2]][1:4], my_metrics, golden_keywords)
#     
#     temp_list_core_2_a = my_metrics(all_keyword[[w]][[1]][[5]][[1]], golden_keywords)
#     temp_list_truss_2_a = my_metrics(all_keyword[[w]][[2]][[5]][[1]], golden_keywords)
#     
#     temp_list_core_2_b = my_metrics(all_keyword[[w]][[1]][[5]][[2]], golden_keywords)
#     temp_list_truss_2_b = my_metrics(all_keyword[[w]][[2]][[5]][[2]], golden_keywords)
#     
#     temp_list_core_3 = my_metrics(all_keyword[[w]][[1]][[6]], golden_keywords)
#     temp_list_truss_3 = my_metrics(all_keyword[[w]][[2]][[6]], golden_keywords)
#     
#     scores_temp_core[[w]] = c(temp_list_core_1,list(temp_list_core_2_a),list(temp_list_core_2_b),list(temp_list_core_3))
#     scores_temp_truss[[w]] = c(temp_list_truss_1,list(temp_list_truss_2_a),list(temp_list_truss_2_b),list(temp_list_truss_3))
#     
    scores_temp_core = my_metrics(all_keyword[[w]][[1]], golden_keywords)
    scores_temp_truss = my_metrics(all_keyword[[w]][[2]], golden_keywords)
  }
  
  all_scores_dens[[i]] = list(scores_temp_core, scores_temp_truss)
  
  if (i%%20==0){
    print(i)
  }
  
  i = i + 1
  
}

save(all_scores, file=paste0(my_path_root,"/data/output/Hulth2003/all_scores_hulth_final"))

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

scores_window_core_dens = list()
scores_window_truss_dens = list()

save(scores_method, file = paste0(my_path_root,"/data/output/semeval/scores_method_semeval_top_number"))


load(paste0(my_path_root,"/data/output/semeval/scores_method_semeval_top_number"))

load(paste0(my_path_root,"/data/output/Hulth2003/scores_method_hulth"))

load(paste0(my_path_root,"/data/output/Marujo2012/scores_method_marujo_final_larger_w"))

# scores_method[[method]][[core_or_truss]][[window]][[prec, rec, f1]]

(trp, tre, main, inf, crpe[[1]], crpe[[2]], dens)

# truss, F1 score
for (w in 1:12){
scores_w = unlist(lapply(scores_method, function(x) x[[2]][[w]][[3]]))
best_index = which(scores_w==max(scores_w))
print(paste0("window = ", w, ";", best_index,", ", round(scores_w[best_index],4)))
}

# hulth, k-truss, window of size 9+2, 0.5609 in F1 score
# marujo, k-core, window of size 11+2, 0.5294 in F1 score
# semeval, k-truss, window of size 18+2, 0.3898 in F1 score


df = data.frame(matrix(unlist(lapply(scores_method, function(x) x[[2]][[9]])), nrow=7, byrow=T))
df = df*100
colnames(df) = c("precision","recall","F1-score")
rownames(df) = c("TRP", "TRE", "main", "inf", "CRP", "CRE", "dens")

xtable(df)

for (w in 1:12){
  scores_w = unlist(lapply(scores_method, function(x) x[[2]][[w]][[3]]))
  best_index = which(scores_w==max(scores_w))
  print(paste0(best_index,", ", round(scores_w[best_index],4)))
}

# w=6, for k-core (0.5567)
# w=9, for k-truss (0.561)

# all_scores[[abstract]][[core or truss]][[window]][[method]][[prec, rec, F1]]

# test for statistical significance

# Mann-Whitney U test. The Null hypothesis is: "the difference in means is due to random sampling"
# one-sided (alternative="greater"): the 1st mean (x, row) is greater than the 2nd mean (y, column) due to chance

load(paste0(my_path_root,"/data/output/semeval/all_scores_final_to-number"))

load(paste0(my_path_root,"/data/output/Hulth2003/all_scores_hulth_final"))

load(paste0(my_path_root,"/data/output/Marujo2012/all_scores_marujo_final"))


w=18
k=2
tests = matrix(ncol=7,nrow=7)
for (method in 1:7){
  for (method_other in 1:method){
x = unlist(lapply(all_scores, function(x) x[[k]][[w]][[method]][[3]]))
y = unlist(lapply(all_scores, function(x) x[[k]][[w]][[method_other]][[3]]))

tests[method,method_other] = round(wilcox.test(x=x, y=y, paired=FALSE)$p.value, 5)

  }
}

colnames(df1) = c("TRP", "TRE", "m", i, "CRP", "CRE", "d")
rownames(df1) = c("TRP", "TRE", "m", "i", "CRP", "CRE", "d")

tests = t(tests)

index_3 = tests[]<0.001
index_2 = (tests[]>=0.001)&(tests[]<0.01)
index_1 = (tests[]>=0.01)&(tests[]<0.05)
index_0 = tests[]>=0.05

tests[index_3] = "***"
tests[index_2] = "**"
tests[index_1] = "*"
tests[index_0] = "ns"

df3 = tests

write.table(tests_core_6, file=paste0(my_path_root,"/data/output/Hulth2003/hypothesis_testing_hulth_core_8.csv"))
tests_core_6

# scores_method[[method]][[core_or_truss]][[window]][[prec, rec, f1]]

for (method in 1:7){
print(round(scores_method[[method]][[k]][[w]][[3]],3))
}


w=9
k=2
tests_truss_9 = matrix(ncol=7,nrow=7)
for (method in 1:7){
  for (method_other in 1:method){
    x = unlist(lapply(all_scores, function(x) x[[k]][[w]][[method]][[3]]))
    y = unlist(lapply(all_scores, function(x) x[[k]][[w]][[method_other]][[3]]))
    
    tests_truss_9[method,method_other] = round(wilcox.test(x=x, y=y, paired=FALSE, alternative="greater")$p.value, 5)
    
  }
}

colnames(tests_truss_9) = c("TRP", "TRE", "main", "inf", "CRP", "CRE", "dens")
rownames(tests_truss_9) = c("TRP", "TRE", "main", "inf", "CRP", "CRE", "dens")

write.table(tests_truss_9, file=paste0(my_path_root,"/data/output/Hulth2003/hypothesis_testing_hulth_truss_11.csv"))
tests_truss_9

# scores_method[[method]][[core_or_truss]][[window]][[prec, rec, f1]]

for (method in 1:7){
  print(round(scores_method[[method]][[k]][[w]][[3]],3))
}


for (w in w_min:w_max){
  
  scores_window_metric_core = list()
  scores_window_metric_truss = list()
  
  for (metric in 1:3){
    
    scores_window_metric_core[[metric]] = mean(unlist(lapply(all_scores, function(x) x[[1]][[w]][[metric]])))
    scores_window_metric_truss[[metric]] = mean(unlist(lapply(all_scores, function(x) x[[2]][[w]][[metric]])))
    
  }
  
  scores_window_core_dens[[w]] = scores_window_metric_core
  scores_window_truss_dens[[w]] = scores_window_metric_truss
  
}

scores_method[[7]]=list(scores_window_core_dens,scores_window_truss_dens)

save(scores_method, file=paste0(my_path_root,"/data/output/Hulth2003/scores_method_hulth_final_new_density"))

# scores_method[[method]][[core or truss]][[window]][[prec, rec, F1]]

my_ylim = list(c(0.1, 0.65), c(0.15, 0.8), c(0.15, 0.4))
my_lty = rep(c(1,2,4,5,6),2)
method_names = c("TRP", "TRE", "main", "inf", "CRP", "CRE", "dens")
my_pch = c(0,1,2,3,4,5,6,7)
core_or_truss = c("weighted k-core", "k-truss")
metrics = c("precision","recall","F1-score")
my_palette = rainbow(n=length(method_names))

for (i in 1:length(core_or_truss)){
  
  name = core_or_truss[i]
  
  pdf(paste0(my_path_root,"/data/output/Semeval/comparison_final_",name,"_semeval_test_bis.pdf"),width=15,height=15,paper="a4")
  
  par(mfrow=c(2,2),oma =c(0,0,2,0),mar=c(5,2,5,2))
  
  for (j in 1:length(metrics)){
    
    metric = metrics[j]
    
    y_values = list()
    for (w in w_min:w_max){
      y_values[[w]] = scores_method[[1]][[i]][[w]][[j]]
    }
    
    plot(w_min:w_max+2, y_values, main = paste0(" average ",metric), xlab="window size", ylab=paste0(metric), type="b", lty=my_lty[1], pch = my_pch[1], ylim=my_ylim[[j]], col=my_palette[1])
    grid(lwd=2)
    
    for(k in 2:length(method_names)){
      
      y_values = list()
      
      for (w in w_min:w_max){
        y_values[[w]] = scores_method[[k]][[i]][[w]][[j]]
      }
      
      lines(w_min:w_max+2, y_values, lty=my_lty[k], pch=my_pch[k], type="b", col=my_palette[k])
      
    }
    
  }
  
  plot.new()
  legend("topleft", legend = method_names, lty=my_lty, pch=my_pch, col=my_palette)
  
  mtext(paste0(name,", Semeval"), outer=TRUE)
  
  dev.off()
  
}


my_ylim = list(c(0.45, 0.75), c(0.3, 0.8), c(0.40, 0.6))
my_lty = rep(c(1,2,4,5,6),2)
method_names = c("TRP", "TRE", "main", "inf", "CRP", "CRE", "dens")
my_pch = c(0,1,2,3,4,5,6,7)
core_or_truss = c("weighted k-core", "k-truss")
metrics = c("precision","recall","F1-score")
my_palette = rainbow(n=length(method_names))


pdf(paste0(my_path_root,"/data/output/Hulth2003/prec_rec_Hulth_2003.pdf"),width=15,height=15,paper="a4")

par(mfrow=c(1,2))

for (core_or_truss in 1:2){

  precision_values = list()
  recall_values = list()
  
  for (w in w_min:w_max){
    precision_values[[w]] = scores_method[[1]][[core_or_truss]][[w]][[1]]
    recall_values[[w]] = scores_method[[1]][[core_or_truss]][[w]][[2]]
  }
  
  plot(unlist(recall_values),rev(unlist(precision_values)),type="b",lty=my_lty[[1]],pch=my_pch[[1]],ylab="precision",xlab="recall")
  
  for (method in 2:7){
    
    precision_values = list()
    recall_values = list()
    
    for (w in w_min:w_max){
      precision_values[[w]] = scores_method[[method]][[core_or_truss]][[w]][[1]]
      recall_values[[w]] = scores_method[[method]][[core_or_truss]][[w]][[2]]
    }
    
    lines(unlist(precision_values),unlist(recall_values),type="b",lty=my_lty[[method]],pch=my_pch[[method]]) 
  
  }
}
  
dev.off()

################ generate comparison boxplots for each method and each window size

my_ylim = c(0,1)
my_levels = seq(my_ylim[1], my_ylim[length(my_ylim)], by=0.2)

# for sub_score (precision, recall and F1-score)
my_sub_scores = c("precision", "recall", "F1-score")
my_sub_scores_indexes = 1:3

k = 1

my_df_save = list()

for (my_sub_score in my_sub_scores){
  
  my_sub_score_index = my_sub_scores_indexes[k]
  
  my_df_save_temp = list()
  
  # open PDF device
  pdf(paste0(my_path_root,"/data/output/Hulth2003/","comparison_window_",my_sub_score,".pdf"),width=20,height=7,paper="a4r")
  
  layout(t(1:((w_max-w_min+1)+1)),widths=c(0.025,rep((1-0.025)/(w_max-w_min+1), (w_max-w_min+1))))
  par(oma=c(1,0.5,2,0.5),mar=rep(0.1,4),cex=1)
  
  plot.new()
  mtext(paste0("Hulth 2003 ", my_sub_score), outer=TRUE)
  
  abline(v=0)
  abline(h=my_levels-my_ylim[1])
  text(x=rep(-0.3,4), y = my_levels-my_ylim[1]+0.01, labels = as.character(my_levels), pos=4, cex=0.5)
  
  for (w in w_min:w_max){
    
    # for window size w
    
    my_df_temp = as.data.frame(matrix(nrow = 500, ncol = 5))
    
    for (my_row in 1:nrow(my_df_temp)){
      # iterate over abstracts (fill as rows)
      
      for (my_column in 1:ncol(my_df_temp)){
        # iterate over methods (fill as columns)
        
        # f1-score is the third element of the scores of each method (precision and recall)
        my_df_temp[my_row, my_column] = all_scores[[my_row]][[w]][[my_column]][[my_sub_score_index]]
        
      }
      
    }
    
    # actual order: k_core, text_rank, k_truss, k_core_inf, k_truss_inf
    # we want to plot in this order: "textRank", "k-core", "k_truss", "k_truss_inf", "k_core_inf"
    my_df_temp = my_df_temp[c(2,1,3,5,4)]
    
    my_df_save_temp[[w]] = my_df_temp
    
    colnames(my_df_save_temp[[w]]) = c("TR", "K-C", "K-T", "K-T INF", "K-C INF")
    
    boxplot(my_df_temp, axes=FALSE, col=my_cols, outline=FALSE, cex=0.1, ylim=my_ylim)
    mtext(paste0("w_size = ",w+2), 1, 0, cex=0.75)
    grid(lty=5)
    par(new=TRUE)
    # re-plot to avoid having the grid over the plot
    boxplot(my_df_temp, axes=FALSE, col=my_cols, outline=FALSE, cex=0.1, ylim=my_ylim)
    
    if (k !=2){
      if (w == w_min){
        legend("topleft",legend=my_colnames, fill=my_cols, border=rep("black",4), cex=0.7, bg="white")
      }
    } else {
      
      if (w == w_max){
        legend("topright",legend=my_colnames, fill=my_cols, border=rep("black",4), cex=0.7, bg="white")
      }
      
    }
    
  }
  
  # close device (saves .pdf)
  dev.off()
  
  my_df_save[[k]] = my_df_save_temp
  k = k + 1
  
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

load(paste0(my_path_root,"/data/output/Hulth2003/scores_method_hulth_final_new_density"))

# scores_method[[method]][[core or truss]][[window]][[prec, rec, F1]]


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

# the third index is for F1-score
apply(average_scores_df[[3]], 2, max)
which(average_scores_df[[3]][,"k_truss_inf"]==max(apply(average_scores_df[[3]], 2, max)))

# for precision weighted k-core is the best (window of size 3), followed by textRank
apply(average_scores_df[[1]], 2, max)
which(average_scores_df[[1]][,"k-core"]==max(apply(average_scores_df[[1]], 2, max)))

# for recall k-truss inf is head and shoulders above the rest (window of size 14), followed by k-core inf
apply(average_scores_df[[2]], 2, max)
which(average_scores_df[[2]][,"k_truss_inf"]==max(apply(average_scores_df[[2]], 2, max)))

# implications: 
# - the best performing technique (0.559 in F1-score) is k_truss_inf, for a window of size 11
# - the problem is that recall is high (0.72) but precision is quite low (0.49)

average_scores_df[[1]][9,]
average_scores_df[[2]][9,]
average_scores_df[[3]][9,]

# Conversely, the advantage of k-core-inf is that it gets very close to the best performing method (0.556 for w=8)
# and the precision and recall are more balanced (0.53 and 0.68 resp.)

average_scores_df[[1]][6,]
average_scores_df[[2]][6,]
average_scores_df[[3]][6,]

# - overall, the inflexion method consistently leads to better results regardless of the algorithm
# - performance improves or stay stable for all methods as window size increases
# - simple k-truss is initially the worst but gets better than simple k_core and TextRank for larger window sizes

# write the statistical tests corresponding to the two best window sizes
write.csv(statistical_tests[[9]], file=paste0(my_path_root,"/data/output/Hulth2003/hypothesis_testing_best_window_size_(11).csv"))
write.csv(statistical_tests[[6]], file=paste0(my_path_root,"/data/output/Hulth2003/hypothesis_testing_best_window_size_(8).csv"))