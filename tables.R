# Variables created in plots.R
library(tidyverse)

mean_indices = indices %>%
  mutate(index = index %>%
           recode('θ - Global Backward Cohesion' = 'Text Global Backward Cohesion',
                  'λ - Local Backward Cohesion' = 'Text Local Backward Cohesion',
                  'ρ - Mean Pairwise Cohesion' = 'Text Mean Pairwise Cohesion'),
         corpus = corpus %>%
           as_factor() %>%
           recode('text' = 'Texts',
                  'pseudo' = 'Pseudotexts'),
         genre = genre %>%
           as_factor() %>%
           recode('berlitz' = 'Travel guides',
                  'biomed' = 'Biomedical research articles',
                  'icic' = 'Letters',
                  'media' = 'Government media',
                  'plos' = 'Scientific and medical articles',
                  'pseudo' = 'Pseudotexts',
                  'slate' = 'Magazine articles')) %>%
  group_by(text, index, corpus, type, genre) %>%
  summarize(`Mean indices` = mean(value)) %>%
  group_by(index, type, genre) %>%
  summarize(`Overall Mean indices` = mean(`Mean indices`),
            SD = sd(`Mean indices`),
            Median = median(`Mean indices`)) 

mean_indices %>%
  print(n = Inf)
