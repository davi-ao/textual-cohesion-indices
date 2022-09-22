############################################
# Get hypernyms of given word from Wordnet #
############################################
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

if (!require(wordnet)) {
  install.packages("wordnet")
}

setDict('./wn3.1.dict/dict/')

get_word_hypernyms = function(word, POS) {
  if (POS %in% c('PROPN', 'PRON')) {
    return(NA)
  } else {
    pos_f = POS
    
    if (pos_f == 'ADJ') {
      pos_f = 'ADJECTIVE'
    } else if (pos_f == 'ADV') {
      pos_f = 'ADVERB'
    }
    
    tryCatch({
      filter = getTermFilter('ExactMatchFilter', word, T)
      terms = getIndexTerms(pos_f, 1, filter)
      synsets = getSynsets(terms[[1]])
      
      sapply(synsets, function(s) {
        relatedSynsets = getRelatedSynsets(s, '@')
        
        sapply(relatedSynsets, getWord)
      }) %>%
        unlist() %>%
        unique() %>%
        paste0(collapse = '|')
    }, error = function(e) {
      return(NA)
    })
  }
}
