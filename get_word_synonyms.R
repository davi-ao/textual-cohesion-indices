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
  if (POS %in% c('PROPN', 'PRON')) {
    return(NA)
  } else {
    pos_f = POS
    
    if (pos_f == 'ADJ') {
      pos_f = 'ADJECTIVE'
    } else if (pos_f == 'ADV') {
      pos_f = 'ADVERB'
    }
    
    synonyms(word, pos_f) %>%
      paste0(collapse = '|')
  }
}
