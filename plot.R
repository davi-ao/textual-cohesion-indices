library(tidyverse)

text1_indices = read_csv('corpus/indices/S0020025508004520.csv') %>%
  group_by(clique) %>%
  mutate(clique_id = cur_group_id(),
         text = 'Real') %>%
  ungroup()
pseudotext1_indices = read_csv('corpus/indices_pseudotexts/S0020025508004520.csv') %>%
  group_by(clique_id) %>%
  mutate(clique_id = cur_group_id(),
         text = 'Random') %>%
  ungroup()

indices = text1_indices %>%
  select(-clique) %>%
  bind_rows(pseudotext1_indices %>%
              select(-clique))

# Plot of cohesion indices (point and line)
indices %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(index = index %>%
           as_factor() %>%
           recode('global' = 'θ - Global Backward Cohesion',
                  'local' = 'λ - Local Backward Cohesion',
                  'pairwise' = 'ρ - Mean Pairwise Cohesion'),
         type = type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion')) %>%
  ggplot(aes(clique_id, value, color = index, linetype = index, shape = index)) +
  facet_grid(rows = vars(type), cols = vars(text), scales = 'free_y') +
  #geom_line() +
  geom_point() +
  geom_smooth(se = F) +
  xlab('Clique') +
  ylab('Value') +
  theme(legend.position = 'bottom', legend.direction = 'vertical') +
  scale_color_brewer(palette = 'Dark2')

# Plot of cohesion indices (boxplot)
indices %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(index = index %>%
           as_factor() %>%
           recode('global' = 'θ - Global Backward Cohesion',
                  'local' = 'λ - Local Backward Cohesion',
                  'pairwise' = 'ρ - Mean Pairwise Cohesion'),
         type = type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion')) %>%
  ggplot(aes(text, value, fill = index)) +
  facet_grid(rows = vars(type), scales = 'free_y') +
  geom_boxplot() +
  ylab('Value') +
  theme(legend.position = 'bottom', legend.direction = 'vertical') +
  scale_color_brewer(palette = 'Dark2')

#ggsave('FiguraX.png', 
#       device = 'png', 
#       width = 16, 
#       height = 16, 
#       units = 'cm', 
#       dpi = 300)