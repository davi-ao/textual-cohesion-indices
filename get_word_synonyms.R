###########################################
# Get synonyms of given word from Wordnet #
###########################################
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(wordnet)) {
  install.packages("wordnet")
  library(wordnet)
}

setDict('./wn3.1.dict/dict/')

get_word_synonyms = function(word, POS) {
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
    
    synonyms(word, pos_f) %>%
      paste0(collapse = '|')
  }
}
