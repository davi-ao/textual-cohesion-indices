library(tidyverse)

dir = 'corpus/csv/'
files = list.files(dir)
sentences = tibble(
  token = character(),
  lemma = character(),
  upos = character(),
  clique = double(),
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
  mutate(clique_id = paste0(text, '_', clique))

text_lenghts = sentences %>%
  group_by(text) %>%
  summarise(q_n = clique %>% unique() %>% length()) %>%
  ungroup() %>%
  mutate(cumulative = cumsum(q_n))

random_clique_ids = sample(sentences$clique_id %>% unique())

for (i in 1:nrow(text_lenghts)) {
  sentences %>%
    filter(clique_id %in% 
             random_clique_ids[
               (text_lenghts$cumulative[i] - (text_lenghts$q_n[i] - 1)):
                 text_lenghts$cumulative[i]
             ]) %>%
    write_csv(paste0('corpus/csv_pseudotexts/', text_lenghts$text[i], '.csv'))
}
