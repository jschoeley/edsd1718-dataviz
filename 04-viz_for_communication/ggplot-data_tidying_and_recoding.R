library(tidyverse)

life <- read_delim('Book1.csv', delim = ';')

life_long <-
  life %>%
  gather(... = -Age) %>%
  separate(key, into = c('state', 'sex', 'cohort'), sep = '_') %>%
  mutate(sex = ifelse(is.na(sex), 'm', sex),
         cohort = ifelse(is.na(cohort),
                         str_extract(state, pattern = '[0-9]+'),
                         cohort),
         cohort = ifelse(!str_detect(cohort, 'ch'),
                         str_c('ch', cohort),
                         cohort),
         state = str_extract(state, pattern = '[A-Za-z]+'))

life_long %>% View()

ggplot(life_long) +
  geom_line(aes(x = Age, y = value, color = sex)) +
  facet_grid(state~cohort, scales = 'free_y')

life_long %>%
  mutate(state = fct_relevel(state, 'union')) %>%
  ggplot() +
  geom_line(aes(x = Age, y = value, color = sex)) +
  facet_grid(state~cohort, scales = 'free_y')

life_long %>%
  mutate(state = fct_relevel(state, 'union', 'Marr', 'cohab')) %>%
  ggplot() +
  geom_line(aes(x = Age, y = value, color = sex)) +
  facet_grid(state~cohort, scales = 'free_y')

life_long %>%
  mutate(state = fct_relevel(state, 'union', 'Marr', 'cohab'),
         state = fct_recode(state,
                            'In union' = 'union',
                            'Married' = 'Marr',
                            'Cohabiting' = 'cohab',
                            'Divorced' = 'Div',
                            'Separated' = 'S'),
         cohort = fct_recode(cohort,
                             '1959' = 'ch59',
                             '1980' = 'ch80'),
         sex = fct_recode(sex, 'Males' = 'm', 'Females' = 'w')) %>%
  ggplot() +
  geom_line(aes(x = Age, y = value, color = sex)) +
  facet_grid(state~cohort, scales = 'free_y')

