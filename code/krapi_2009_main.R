my_path_root = paste0(getwd(), "/Krapi_2009")

text="A method for solution of systems of linear algebraic equations with m-dimensional lambda matrices. A system of linear algebraic equations with m-dimensional lambda matrices is considered. The proposed method of searching for the solution of this system lies in reducing it to a numerical system of a special kind."

######################
##### packages #######
######################

lapply(list("foreach","doParallel","igraph","tm","SnowballC","stringr"), library, character.only=TRUE)

######################
##### functions ######
######################

lapply(list("from_terms_to_keywords.R","keyword_extraction.R","heapify.R","from_terms_to_graph.R","assign_attributes_to_graph_initial.R","assign_attributes_to_graph_nocomm.R","assign_attributes_to_graph_comm_a.R","assign_attributes_to_graph_comm_b.R","cores_dec.R"), function(x) {source(paste0(my_path_root,"/code/",x))})


# read the SMART list that was used in the coreword paper
my_stopwords = read.csv()

file_names = list.files(path=paste0(getwd(),"/Krapi_2009"))

# there are .txt (papers) and .key (keywords) files (2304 papers total)

papers_logical = lapply(file_names, function(x){grepl("\\<txt\\>",x)})
index_paper = which(papers_logical==TRUE)
paper_names = file_names[index_paper]

keyword_logical = lapply(file_names, function(x){grepl("\\<key\\>",x)})
index_keyword = which(keyword_logical==TRUE)
keyword_names = file_names[index_keyword]

# get the text, clean and write text to file for each paper

setwd("C:/Users/UncleTony/Documents/Krapi_2009")

i=0

for (filename in paper_names){

i = i+1
if (i%%100 == 0){ 
print(i)
}

my_lines = readLines(filename)

logical = lapply(my_lines, function(x){length(unlist(strsplit(x, split=" ")))>1})
index = which(logical==TRUE)
my_lines = my_lines[index]

paper_ID = unlist(strsplit(filename, split="\\."))[1]

write(paste(my_lines, collapse=" "), file = paste0("cleaned_papers/",paper_ID,"_cleaned.txt"))

}

######################################3

setwd("C:/Users/UncleTony/Documents/Krapi_2009/cleaned_papers")

# read and store all papers in a list

cleaned_paper_names = list.files(path=getwd())

nc = detectCores()
cl = makeCluster(nc)
registerDoParallel(cl)

ptm = proc.time()

my_papers = foreach(i = 1:length(cleaned_paper_names), .export = c("cleaned_paper_names")) %dopar% {                                                      
  filename = cleaned_paper_names[i]
  readLines(filename)
}

proc.time() - ptm
stopCluster(cl)

# build customized list of stopwords

# clean and split all documents into words
my_papers_words = lapply(my_papers,function(X){
    
    # convert to lower case
    x = tolower(X)
    # remove extra white space
    x = gsub("\\s+"," ",x)
    # replace slashes and (backslashes followed by t) with white space
    x = gsub("[/\t]"," ",x)
    
    # remove between word dashes
    x = gsub("- ", " ", x, perl = TRUE) 
    x = gsub(" -", " ", x, perl = TRUE)
    
    # remove parentheses
    x = gsub("\\(", " ", x, perl = TRUE) 
    x = gsub("\\)", " ", x, perl = TRUE)
    
    # remove 1 letter words
    x = gsub(" *\\b(?<!-)\\p{L}{1,2}(?!-)\\b *", " ", x, perl=T)
    
    # remove punctuation
    x = gsub("[^[:alnum:][:space:]-]", "", x)
    
    # remove apostrophes that are not intra-word
    x = gsub("' ", " ", x, perl = TRUE)
    x = gsub(" '", " ", x, perl = TRUE)
  
    # remove leading and trailing white space
    x = str_trim(x,"both")
    
    # remove extra white space
    x = gsub("\\s+"," ",x)
    
    lapply(unlist(strsplit(x, split=" ")),wordStem)
}
)
    
my_papers_words = lapply(my_papers_words, unlist)

my_papers_unique_words = unique(unlist(my_papers_words))

#unique_word_counts = lapply(my_papers_unique_words, function(x) {length(which(unlist(lapply(my_papers_words, function(y) {x %in% y}))==TRUE))})

nc = detectCores()
cl = makeCluster(nc)
registerDoParallel(cl)

unique_word_counts = foreach(i = 1:length(my_papers_unique_words), .export = c("my_papers_unique_words","my_papers_words")) %dopar% {                                                      
  length(which(unlist(lapply(my_papers_words, function(y) {my_papers_unique_words[i] %in% y}))==TRUE))
}

stopCluster(cl)

custom_stopwords = 

# save to .csv
  
# add these custom stopwords to current list of (common) English stopwords
my_stopwords = c(my_stopwords, custom_stopwords)

nc = detectCores()
cl = makeCluster(nc)
registerDoParallel(cl)


# randomly sample 10 papers

random_index = sample.int(length(my_papers), 10, replace=FALSE)

my_papers_for_test = my_papers[random_index]

ptm = proc.time()

edge_lists = foreach(i = 1:10, .packages = c("combinat","stringr","openNLP","SnowballC","NLP"),.export = c("cleaning.text","tagPOS","my_stopwords","my_papers")) %dopar% {                                                      
# read ith document
my_paper = my_papers[[i]]
# clean-up text
terms_list = cleaning.text(X=my_paper,custom=my_stopwords,pos=TRUE,stem=TRUE)
# construct edge list
test=from.terms.to.graph(terms_list,w=3,overspan=FALSE,processed=FALSE)$output
}

# write edgelists to files

lapply(edge_lists, function(x) {})

write.table

write(paste(my_lines, collapse=" "), file = paste0("cleaned_papers/",paper_ID,"_cleaned.txt"))




proc.time() - ptm
stopCluster(cl)

output = assign.attributes.to.graph.initial(test, weighted = TRUE)
g = output$g
v_g_name = output$v_g_name
l_v_g_name = output$l_v_g_name
g = assign.attributes.to.graph.nocomm(g, "weighted_k_core", FALSE, "all", v_g_name, l_v_g_name)$g
# retain useful information
nodes = get.data.frame(g, what="vertices")
# sort data frame by core number
nodes = nodes[order(nodes[,"core_no"],decreasing=TRUE),]
core_numbers = nodes[,"core_no"]

levels = rev(summary(as.factor(core_numbers)))

cumsum_levels = cumsum(levels)
barplot(cumsum_levels)

title(main="weighted")

# my_delta = between-level increase in size when moving from Q-1 to Q

delta_q=1:length(levels)*0
delta_q_plus_one=1:length(levels)*0

for (i in 2:length(levels)){
  delta_q[i] = cumsum_levels[i] - cumsum_levels[i-1]
  delta_q_plus_one[i] = cumsum_levels[i+1] - cumsum_levels[i]
}

all_diffs = (delta_q/delta_q_plus_one)[1:10]

best_level = which(all_diffs==min(all_diffs, na.rm=TRUE))

extracted_keywords = rownames(nodes[1:cumsum_levels[best_level],])

extracted_keywords = rownames(nodes[1:cumsum_levels[1],])

# "1005395_cleaned.txt"
# in addtion to Krapi: DUC corpus for summarization?
# data set used by Grineva??
# try to use clustering even for single topic documents?
# try the data sets reported in Bougouin et al. 2013
# they compare keyphrases without stemming, which seems harder

setwd("C:/Users/UncleTony/Documents/Krapi_2009")

golden_keywords = readLines(paste0(unlist(strsplit("1005058_cleaned",split="_"))[1],".key"))
golden_keywords = unlist(lapply(golden_keywords, function(x) {unlist(strsplit(x,split=" "))}))
golden_keywords = wordStem(golden_keywords)

n_hits = length(which(extracted_keywords%in%golden_keywords==TRUE))
n_misses = length(which(golden_keywords%in%extracted_keywords==FALSE))
n_false_alarms = length(which(extracted_keywords%in%golden_keywords==FALSE))

precision = n_hits/(n_hits+n_false_alarms)
recall = n_hits/(n_hits+n_misses)

F1_score = 2*(precision*recall)/(precision+recall)

index = which(is.na(nodes[,"core_no"]))
nodes[index,"core_no"] = rep("out of main comms",length(index))
u_core_values = unique(nodes[,"core_no"])
nodes[,"group"] = nodes[,"core_no"]
nodes_bis = nodes

u_core_values = reac3()$u_core_values
hierarchy_level = input$selected_core + 1
hierarchy_level_bis = input$selected_core_cumulative + 1 
terms_unprocessed = reac1()$terms_list$unprocessed
nodes = reac3()$nodes_bis