sent_token_annotator = Maxent_Sent_Token_Annotator()
word_token_annotator = Maxent_Word_Token_Annotator()
pos_tag_annotator = Maxent_POS_Tag_Annotator()

cleaning.text <-
function(X, custom, pos, stem) {
	# required packages: "stringr","openNLP","SnowballC"
	# required functions: "tagPOS.R"

	# this function performs standard preprocessing steps
	# it returns both the unprocessed and processed tokenized text
	# the difference between the two lies in stopwords removal, POS screening (nouns and adjectives), and stemming
	# if none of these parameters has been set to TRUE by the user, then the unprocessed and processed character vectors are the same

	# remove extra white space
	x = gsub("\\s+"," ",X)
	# replace slashes and (backslashes followed by t) with white space
	x = gsub("[/\t]"," ",x)

	# split into sentences
	s = as.String(x)
	boundaries = annotate(s, sent_token_annotator)
	sentences = s[boundaries]
	
	# remove between word dashes
	x = gsub("- ", " ", x, perl = TRUE) 
	x = gsub(" -", " ", x, perl = TRUE)

	# remove parentheses
	x = gsub("\\(", " ", x, perl = TRUE) 
	x = gsub("\\)", " ", x, perl = TRUE)
	
	# remove punctuation but keep commas, semicolons, periods, exclamation marks, question marks, intra-word dashes and apostrophes (e.g., "I'd like")

	# the subset of the aforementioned punctuation marks corresponding to sentence stop signs will be used for graph building if overspan==FALSE

	# the full set of the aforementioned punctuation marks will be used for keyphrase reconstruction

	x = gsub("[^[:alnum:][:space:],;.!?'-:]", " ", x)
	
	# remove plus and star signs
	x = gsub("+", " ", x, fixed = TRUE)
	x = gsub("*", " ", x, fixed = TRUE)

	# remove apostrophes that are not intra-word
	x = gsub("' ", " ", x, perl = TRUE)
	x = gsub(" '", " ", x, perl = TRUE)

	# collapse "I'd" into "Id"
	x = gsub("'", "", x, perl = TRUE)

	# remove numbers (integers and floats) but not dates like 2015
	x = gsub("\\b(?!(?:18|19|20)\\d{2}\\b(?!\\.\\d))\\d*\\.?\\d+\\b"," ", x, perl=T)

	# the following feature was removed because it created issues with POS tagging
	# we consider e.g. and i.e. as single units and do not perform the separation for them
	# x = gsub("\\b(?:e\\.g\\.|i\\.e\\.)(*SKIP)(*F)|[[:blank:]]*([.]{2,}|[.,:;!?])[[:blank:]]*", " \\1 ", x, perl=T)

	# remove "e.g." and "i.e."
	x = gsub("\\b(?:e\\.g\\.|i\\.e\\.)", " \\1 ", x, perl=T)

	# separate remaining punctuation from words,
	# differentiating between ellipsis (suspension points) and periods
	# punctuation marks are also separated from other marks (to ensure 1-to-1 matching with POS tags)
	x = gsub("[[:blank:]]*([.]{2,}|[.,:;!?])[[:blank:]]*", " \\1 ", x, perl=T)

	# replace "...." by "..."
	x = gsub("(\\.\\.\\.\\.)", " \\.\\.\\. ", x, perl=T)

	# replace ".." by "."
	x = gsub("(\\.\\.\\.)(*SKIP)(*F)|(\\.\\.)", " \\. ", x, perl=T)
     
	# replace slashes and (backslashes followed by t) with white space
	x = gsub("[/\t]"," ",x)

	# remove leading and trailing white space
	x = str_trim(x,"both")

	# remove extra white space
	x = gsub("\\s+"," ",x)

	# convert to lower case
	x = tolower(x)

	# tokenize
	x = unlist(strsplit(x,split=" "))
	# make a copy of tokens without further preprocessing
	xx = x

	if (pos == TRUE) {
		# retain nouns, adjectives, and useful punctuation marks
		x_tagged = tagPOS(x)$output
		index = which(x_tagged%in%c("NN","NNS","NNP","NNPS","JJ","JJS","JJR",":",".",","))
		if (length(index)>0){
			x = x[index]
		}

	}

	# remove stopwords
	index = which(x %in% custom)
	if (length(index)>0){
		x = x[-index]
	}

	if (stem == TRUE){
		x = wordStem(x)
		xx = wordStem(xx)
	}

	# remove blank elements
	index = which(x=="")
	if (length(index)>0){
		x = x[-index]
	}
		
	index = which(xx=="")
	if (length(index)>0){
		xx = xx[-index]
	}
	
	output = list(unprocessed=xx, processed=x, sentences=sentences)

}