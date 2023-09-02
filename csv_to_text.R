library(tidyverse)

dir = './corpora/oanc_csv/'
files = list.files(dir)

for (f in files) {
  sentences = read_csv(paste0(dir, f)) %>% 
    group_by(clique_id) %>%
    summarise(sentence = paste(lemma, collapse = ' ')) %>%
    .$sentence
    
  write_file(sentences %>% paste(collapse = '. '),
             paste0('./corpora/oanc_txt/', f %>% str_sub(1, -5), '.txt'))
}

dir = './corpora/oanc_csv_pseudotexts/'
files = list.files(dir)

for (f in files) {
  sentences = read_csv(paste0(dir, f)) %>% 
    group_by(clique_id) %>%
    summarise(sentence = paste(lemma, collapse = ' ')) %>%
    .$sentence
  
  write_file(sentences %>% paste(collapse = '. '),
             paste0('./corpora/oanc_txt/', f %>% str_sub(1, -5), '.txt'))
}
