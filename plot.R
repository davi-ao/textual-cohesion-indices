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

dir = 'corpora/oanc_indices/'
files_indices_oanc = list.files(dir)

for (f in files_indices_oanc_root) {
  indices_oanc = indices_oanc %>%
    bind_rows(read_csv(paste0(dir, f)))
}

indices_pseudotext = tibble(
  text = character(),
  clique_id = double(),
  index = character(),
  v = double(),
  e = double()
)

dir = 'corpora/oanc_indices_pseudotexts/'
files_indices_pseudo = list.files(dir, 'pseudotext_*')

for (f in files_indices_pseudo) {
  indices_pseudotext = indices_pseudotext %>%
    bind_rows(read_csv(paste0(dir, f)))
}

# Prepare tibble for plotting
indices = indices_oanc %>%
  mutate(genre = text %>% 
           str_match('([a-z]+)_') %>% 
           .[,2],
         corpus = 'text') %>%
  bind_rows(indices_pseudotext %>%
              mutate(genre = 'pseudo',
                     corpus = 'pseudo')) %>%
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
