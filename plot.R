library(tidyverse)
library(jtools)

theme_set(theme_apa())

indices_oanc = tibble(
  text = character(),
  clique_id = double(),
  index = character(),
  v = double(),
  e = double()
)

dir = 'corpora/indices_oanc/'
files_indices_oanc_lemma = list.files(dir, 'lemma_*')
files_indices_oanc_root = list.files(dir)[!list.files(dir) %in% files_indices_oanc_lemma]

for (f in files_indices_oanc_root) {
  indices_oanc = indices_oanc %>%
    bind_rows(read_csv(paste0(dir, f)))
}

indices_oanc = indices_oanc %>%
  mutate(vertices_type = 'root')

indices_pseudotext = tibble(
  text = character(),
  clique_id = character(),
  index = character(),
  v = double(),
  e = double()
)

dir = 'corpora/indices_pseudotexts/'
files_indices_pseudo = list.files(dir)

for (f in files_indices_pseudo) {
  indices_pseudotext = indices_pseudotext %>%
    bind_rows(read_csv(paste0(dir, f)) %>%
                select(-clique))
}

# Prepare tibble for plotting
indices = indices_oanc %>%
  mutate(genre = text %>% 
           str_match('([a-z]+)_') %>% 
           .[,2]) %>%
  mutate(clique_id = clique_id %>% as.character(),
         corpus = 'oanc') %>%
  bind_rows(indices_pseudotext %>%
              mutate(corpus = 'pseudo')) %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(index = index %>%
           as_factor() %>%
           recode('global' = 'θ - Global Backward Cohesion',
                  'local' = 'λ - Local Backward Cohesion',
                  'pairwise' = 'ρ - Mean Pairwise Cohesion'),
         type = type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion'))

# Plot probability distribution of cohesion indices
indices %>%
  ggplot(aes(value, ..scaled.., fill = corpus, group = corpus)) +
  facet_wrap(type ~ index, scales = 'free') +
  geom_density(alpha = .5) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2')

ggsave('oanc_vertices_type.png', 
       device = 'png', 
       width = 24.7, 
       height = 16, 
       units = 'cm', 
       dpi = 300)
