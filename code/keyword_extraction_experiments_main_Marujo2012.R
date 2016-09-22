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

# read the SMART list that was used in the coreword paper
my_stopwords = readLines("http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")

# * end

#####################
#    Marujo 2012    #
#####################

# 450 news stories (train set)
# the keywords for each story are the most commonly selected by Amazon Turk workers (max score of 10)
# https://arxiv.org/ftp/arxiv/papers/1306/1306.4886.pdf

# many of the keywords are verbs. Therefore we also try without POS-tagging
# for instance, regarding "art_and_culture-20893614", "kisses", "met", "dated", "made"


unique_names_candidates = list.files(paste0(my_path_root,"/data/Marujo2012/train/"))
unique_names = unique(unlist(lapply(unique_names_candidates, function(x) paste(head(unlist(strsplit(x, split="-")),-1), collapse="-"))))

index_to_keep = unlist(lapply(unique_names, function(x) grepl("\\d",x)))
unique_names = unique_names[index_to_keep]

write.csv(unique_names, file = paste0(my_path_root,"/data/output/Marujo2012/unique_names.csv"), row.names=FALSE)

unique_names = as.character(read.csv(paste0(my_path_root,"/data/output/Marujo2012/unique_names.csv"), header=TRUE)[,1])

abstracts = list()
keywords = list()
i = 1

for (name in unique_names){
  
  keyword_candidates = readLines(paste0(my_path_root,"/data/Marujo2012/train/",name,".key"))
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
  
  paper_lines = readLines(paste0(my_path_root,"/data/Marujo2012/train/",name,".txt"))
  abstracts[[i]] = paste(paper_lines, collapse=" ")
  
  i = i+1
  
  if (i %% 10 == 0){
    print(i)
  }
  
}

save(abstracts, file = paste0(my_path_root,"/data/output/Marujo2012/processed_abstracts"))
save(keywords, file = paste0(my_path_root,"/data/output/Marujo2012/processed_keywords"))

load(paste0(my_path_root,"/data/output/Marujo2012/processed_abstracts"))
load(paste0(my_path_root,"/data/output/Marujo2012/processed_keywords"))

# compute length of abstracts in number of words
lengthes_abstracts_marujo = unlist(lapply(abstracts, function(x) length(unlist(strsplit(x, split=" ")))))
save(lengthes_abstracts_marujo, file = paste0(my_path_root,"/data/output/Marujo2012/size_abstracts_Marujo"))

load(paste0(my_path_root,"/data/output/Hulth2003/size_abstracts_Hulth"))
load(paste0(my_path_root,"/data/output/Krapi2009/size_abstracts_Krapi"))

load(paste0(my_path_root,"/data/output/Marujo2012/size_abstracts_Marujo"))


boxplot(lengthes_abstracts_marujo)

# remove the two outliers (length above 2000 words)
index_remove = which(lengthes_abstracts_marujo>2000)

lengthes_abstracts_marujo = lengthes_abstracts_marujo[-index_remove]
abstracts = abstracts[-index_remove]
keywords = keywords[-index_remove]

all_graphs = all_graphs[-index_remove]


# do a common triplot at the end comparing the length distributions of documents from all data sets to number of extracted keywords
# correlation between abstract size and number of keywords manually assigned
number_keywords_human_marujo = unlist(lapply(keywords, length))

# open PDF device
pdf(paste0(my_path_root,"/data/output/comparisons_size_number.pdf"),width=8,height=14,paper="a4")
par(mfrow=c(3,2))

# these plots tell us two things:

# - the number of keywords assigned by humans vary according to document length, but not with a fixed ratio. Therefore extracting a fixed percentage of keywords from a document like TR is not optimal, degeneracy algos provide a more natural, human-like alternative
# - the three data sets we use vary greatly (Hulth contains short docs with a few keywords, Marujo contains short documents with many keywords, and Krapi long docs with few keywords). therefore the tasks vary every time and our results show good robustness of our proposed technique

plot(lengthes_abstracts_hulth, number_keywords_human_hulth, xlab="abstract size in words", ylab="number of keywords manually assigned", main="Hulth2003", sub=paste("n =", length(number_keywords_human_hulth)))
grid()
plot.new()
plot(lengthes_abstracts_krapi, number_keywords_human_krapi, xlab="document size in words", ylab="number of keywords manually assigned", main="Krapi2009", sub=paste("n =", length(number_keywords_human_krapi)))
grid()
plot.new()
plot(lengthes_abstracts_marujo, number_keywords_human_marujo, xlab="story size in words", ylab="number of keywords manually assigned", main="Marujo2012", sub=paste("n =", length(number_keywords_human_marujo)))
grid()

# close device (saves .pdf file to working directory)
dev.off()


# * start

load(paste0(my_path_root,"/data/output/Marujo2012/processed_abstracts"))
load(paste0(my_path_root,"/data/output/Marujo2012/processed_keywords"))

# * end


################ clean abstracts

terms_lists_no_pos = list()

i = 1

for(abstract in abstracts){
  
  terms_lists_no_pos[[i]] = cleaning.text(X=abstract, custom=my_stopwords, pos=FALSE, stem=TRUE)
  i = i + 1
  
  if(i%%50==0){
    print(i)
  }
  
}

save(terms_lists_no_pos, file = paste0(my_path_root,"/data/output/Marujo2012/terms_lists_no_pos"))

# * start

load(paste0(my_path_root,"/data/output/Marujo2012/terms_lists_no_pos"))


################ build graphs for each abstract and all window sizes

# w corresponds to (real window size - 2)

w_min = 1
w_max = 12

w_min = 13
w_max = 18

# build graphs for window sizes larger than 14
ptm = proc.time()
nc = detectCores()
cl = makeCluster(nc)
registerDoParallel(cl)

all_graphs_larger_w = foreach(kk = 1:length(terms_lists_no_pos), .packages = c("igraph"), .export = c("w_min","w_max","terms_lists_no_pos","heapify","from_terms_to_graph","assign_attributes_to_graph_initial")) %dopar% {     
  
  terms_list = terms_lists_no_pos[[kk]]
  
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

save(all_graphs_larger_w, file = paste0(my_path_root,"/data/output/Marujo2012/all_graphs_larger_w"))

load(paste0(my_path_root,"/data/output/Marujo2012/all_graphs"))

all_graphs = all_graphs[-index_remove]

load(paste0(my_path_root,"/data/output/Marujo2012/all_graphs_larger_w"))


# decompose graphs


ptm = proc.time()
cl = makeCluster(nc)
registerDoParallel(cl)

decomposed_graphs_larger_w = foreach(kk = 1:length(all_graphs_larger_w), .packages = c("igraph"), .export = c("w_min","w_max","assign_attributes_to_graph_nocomm","cores_dec", "all_graphs_larger_w")) %dopar% {
  
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

save(decomposed_graphs_larger_w, file = paste0(my_path_root,"/data/output/Marujo2012/decomposed_graphs_larger_w"))


################ extract keywords for each method and each window size

load(paste0(my_path_root,"/data/output/Marujo2012/decomposed_graphs_marujo"))

# decomposed_graphs[[abstract]][[window]][[degeneracy_method (core or truss)]]

w_min = 1
w_max = 12

ptm = proc.time()

cl = makeCluster(nc)
registerDoParallel(cl)

all_keywords_marujo_final = foreach(kk = 1:length(decomposed_graphs), .packages = c("igraph"), .export = c("w_min","w_max","keyword_extraction","decomposed_graphs","get_elbow_point","best_level_density")) %dopar% {
  
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

################ compute scores for each method and each window size

load(paste0(my_path_root,"/data/output/Marujo2012/all_keywords_marujo_final"))


# all_keywords_marujo_final[[abstract]][[window]][[core or truss]][[method]]

# method: trp, tre, main, inf, crpe(list crp, cre), dens

# if method==5, percentage then rank

all_scores = list()

i = 1

for (all_keyword in all_keywords_marujo_final){
  
  # for a given abstract...
  scores_temp_core = list()
  scores_temp_truss = list()
  
  golden_keywords = unique(tolower(wordStem(keywords[[i]])))
  
  for (w in w_min:w_max){
    
    # ... and for a given window size, compute scores for each method: trp, tre, main, inf, crpe(list crp, cre), dens
    # "my_metrics" returns in that order: precision, recall, and F1-score
    temp_list_core_1 = lapply(all_keyword[[w]][[1]][1:4], my_metrics, golden_keywords)
    temp_list_truss_1 = lapply(all_keyword[[w]][[2]][1:4], my_metrics, golden_keywords)
    
    temp_list_core_2_a = my_metrics(all_keyword[[w]][[1]][[5]][[1]], golden_keywords)
    temp_list_truss_2_a = my_metrics(all_keyword[[w]][[2]][[5]][[1]], golden_keywords)
    
    temp_list_core_2_b = my_metrics(all_keyword[[w]][[1]][[5]][[2]], golden_keywords)
    temp_list_truss_2_b = my_metrics(all_keyword[[w]][[2]][[5]][[2]], golden_keywords)
    
    temp_list_core_3 = my_metrics(all_keyword[[w]][[1]][[6]], golden_keywords)
    temp_list_truss_3 = my_metrics(all_keyword[[w]][[2]][[6]], golden_keywords)
    
    scores_temp_core[[w]] = c(temp_list_core_1,list(temp_list_core_2_a),list(temp_list_core_2_b),list(temp_list_core_3))
    scores_temp_truss[[w]] = c(temp_list_truss_1,list(temp_list_truss_2_a),list(temp_list_truss_2_b),list(temp_list_truss_3))
    
  }
  
  all_scores[[i]] = list(scores_temp_core, scores_temp_truss)
  
  if (i%%20==0){
    print(i)
  }
  
  i = i + 1
  
}

save(all_scores, file=paste0(my_path_root,"/data/output/Marujo2012/all_scores_marujo_final"))

load(paste0(my_path_root,"/data/output/Marujo2012/all_scores_marujo_final"))

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

save(statistical_tests, file = paste0(my_path_root,"/data/output/Marujo2012/statistical_tests"))

# find best performing method for each window size

load(paste0(my_path_root,"/data/output/Marujo2012/scores_method_marujo_final_larger_w"))

my_ylim = list(c(0.45, 0.7), c(0.15, 0.75), c(0.25, 0.55))
my_lty = rep(c(1,2,4,5,6),2)
method_names = c("TRP", "TRE", "main", "inf", "CRP", "CRE", "dens")
my_pch = c(0,1,2,3,4,5,6,7)
core_or_truss = c("weighted k-core", "k-truss")
metrics = c("precision","recall","F1-score")
my_palette = rainbow(n=length(method_names))

for (i in 1:length(core_or_truss)){
  
  name = core_or_truss[i]
  
  pdf(paste0(my_path_root,"/data/output/Marujo2012/comparison_final_",name,"_Marujo_2012.pdf"),width=15,height=15,paper="a4")
  
  par(mfrow=c(2,2),oma =c(0,0,2,0),mar=c(5,2,5,2))
  
  for (j in 1:length(metrics)){
    
    metric = metrics[j]
    
    y_values = list()
    for (w in w_min:w_max){
      y_values[[w]] = scores_method[[1]][[i]][[w]][[j]]
    }
    
    for (w in 1:6){
      y_values[[w+w_max]] = scores_method_larger[[1]][[i]][[w]][[j]]
    }
    
    plot(w_min:(w_max+6)+2, y_values, main = paste0(" average ",metric), xlab="window size", ylab=paste0(metric), type="b", lty=my_lty[1], pch = my_pch[1], ylim=my_ylim[[j]], col=my_palette[1])
    grid(lwd=2)
    
    for(k in 2:length(method_names)){
      
      y_values = list()
      
      for (w in w_min:w_max){
        y_values[[w]] = scores_method[[k]][[i]][[w]][[j]]
      }

      for (w in 1:6){
        y_values[[w+w_max]] = scores_method_larger[[k]][[i]][[w]][[j]]
      }
      
      lines(w_min:(w_max+6)+2, y_values, lty=my_lty[k], pch=my_pch[k], type="b", col=my_palette[k])
      
    }
    
  }
  
  plot.new()
  legend("topleft", legend = method_names, lty=my_lty, pch=my_pch, col=my_palette)
  
  mtext(paste0(name,", Marujo 2012"), outer=TRUE)
  
  dev.off()
  
}