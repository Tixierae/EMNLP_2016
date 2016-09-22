my_metrics = function(my_keywords, golden_keywords){
  
  n_hits = length(which(my_keywords%in%golden_keywords==TRUE))
  n_misses = length(which(golden_keywords%in%my_keywords==FALSE))
  n_false_alarms = length(which(my_keywords%in%golden_keywords==FALSE))
  
  precision = n_hits/(n_hits+n_false_alarms)
  recall = n_hits/(n_hits+n_misses)
  
  F1_score = 2*(precision*recall)/(precision+recall)
  
  if (is.na(F1_score)){
    # if denominator is 0 (meaning there are no hits) return 0
    F1_score = 0
  }
  
  return(list(precision, recall, F1_score))
  
}