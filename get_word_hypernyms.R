############################################
# Get hypernyms of given word from Wordnet #
############################################
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(wordnet)) {
  install.packages("wordnet")
  library(wordnet)
}

setDict('./wn3.1.dict/dict/')

get_word_hypernyms = function(word, POS) {
  if (POS %in% c('PROPN', 'PRON', 'CD', 'FW')) {
    return(NA)
  } else {
    pos_f = POS
    
    if (pos_f %in% c('ADJ', 'JJ', 'JJR', 'JJS')) {
      pos_f = 'ADJECTIVE'
    } else if (pos_f %in% c('ADV', 'RB', 'RBR', 'RBS')) {
      pos_f = 'ADVERB'
    } else if (pos_f %in% c('NN', 'NNS', 'NNP', 'NNPS')) {
      pos_f = 'NOUN'
    } else if (pos_f %in% c('VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ')) {
      pos_f = 'VERB'
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
