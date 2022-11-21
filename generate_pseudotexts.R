library(tidyverse)

dir = 'corpora/oanc_csv/'
files = list.files(dir)
sentences = tibble(
  lemma = character(),
  upos = character(),
  clique_id = double(),
  stem = character(),
  synonyms = character(),
  hypernyms = character(),
  text = character()
)

for (file in files) {
  text = read_csv(paste0(dir, file))
  text_name = str_sub(file, 0, -5)
  
  sentences = sentences %>%
    bind_rows(text %>%
                mutate(text = text_name))
}

sentences = sentences %>%
  mutate(text_clique_id = paste0(text, '_', clique_id)) %>%
  group_by(text)

for (i in 1:length(files)) {
  sentences %>%
    filter(text_clique_id %in% (
      sentences %>%
        sample_n(1) %>%
        .$text_clique_id
    )) %>%
    group_by(text_clique_id) %>%
    mutate(clique_id = cur_group_id()) %>%
    write_csv(paste0('corpora/oanc_csv_pseudotexts/pseudotext_', 
                     str_pad(i, 2, 'left', '0'), 
                     '.csv'))
}
