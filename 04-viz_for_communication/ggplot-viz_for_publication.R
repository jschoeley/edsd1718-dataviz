#' ---
#' title: "Polishing your plot for publication."
#' author: "Jonas Schöley"
#' date: "February 1st, 2017"
#' output:
#'   github_document:
#'     toc: true
#' ---

#'## Eastern European life-expectancy changes since 1980

library(tidyverse)

e0 <-
  read_csv('e0.csv') %>%
  filter(cntry %in% c('BGR', 'CZE', 'HUN', 'POL', 'DEUTE'),
         sex == 'male', year >= 1980) %>%
  group_by(cntry) %>%
  mutate(e0std = e0 - e0[year == 1980]) %>%
  ungroup()
  
ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry))

#'## Spell it out

e0 <-
  e0 %>%
  mutate(cntry = fct_recode(cntry,
                            'Bulgaria' = 'BGR',
                            'Czech Republic' = 'CZE',
                            'East Germany' = 'DEUTE',
                            'Hungary' = 'HUN',
                            'Poland' = 'POL'))

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry))

#'## Use order

e0 <-
  e0 %>%
  mutate(cntry = fct_relevel(cntry,
                             'East Germany',
                             'Czech Republic',
                             'Poland',
                             'Hungary',
                             'Bulgaria'))

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry))

#'## Use direct annotation

cntry_labels <-
  e0 %>%
  group_by(cntry) %>%
  filter(year == max(year)) %>%
  ungroup()

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry),
            data = cntry_labels)

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry), show.legend = FALSE) +
  geom_text(aes(x = year, y = e0std, label = cntry),
            data = cntry_labels)

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry), show.legend = FALSE) +
  geom_text(aes(x = year, y = e0std, label = cntry),
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017))

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry), show.legend = FALSE) +
  geom_text(aes(x = year, y = e0std, label = cntry),
            nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017))

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels, aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry),
            nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017)) +
  guides(color = FALSE)

#'## Avoid clutter

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels, aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry),
            nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017)) +
  guides(color = FALSE) +
  theme_classic()

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels, aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017)) +
  guides(color = FALSE) +
  theme_classic() +
  theme(panel.grid.major.y =
          element_line(color = 'grey90', linetype = 'dotted'),
        panel.grid.minor.y =
          element_line(color = 'grey90', linetype = 'dotted'))

#'## Highlight important features

ggplot(e0, aes(x = year, y = e0std)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = 'grey90') +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels, aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017)) +
  guides(color = FALSE) +
  theme_classic() +
  theme(panel.grid.major.y =
          element_line(color = 'grey90', linetype = 'dotted'),
        panel.grid.minor.y =
          element_line(color = 'grey90', linetype = 'dotted'))

#'## Adjust scale breaks

ggplot(e0, aes(x = year, y = e0std)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = 'grey90') +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels, aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous('Year', breaks = c(seq(1980, 2010, 5), 2013),
                     limits = c(1980, 2017)) +
  scale_y_continuous('Change in life-expectancy from 1980',
                     breaks = seq(-2, 10, 1)) +
  guides(color = FALSE) +
  theme_classic() +
  theme(panel.grid.major.y =
          element_line(color = 'grey90', linetype = 'dotted'))

#'## Show context

e0_nato <-
  read_csv('e0.csv') %>%
  filter(cntry %in% c('DEUTW', 'DNK', 'SWE',
                      'ITA', 'ISL', 'GBRTENW', 'ESP',
                      'NLD', 'LUX', 'BEL', 'PRT'),
         sex == 'male') %>%
  group_by(cntry) %>%
  mutate(e0std = e0 - e0[year == 1980])

ggplot(e0, aes(x = year, y = e0std)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = 'grey90', alpha = 0.5) +
  geom_line(aes(group = cntry), color = 'grey90', data = e0_nato) +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels, aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous('Year', breaks = c(seq(1980, 2010, 5), 2013),
                     limits = c(1980, 2017)) +
  scale_y_continuous('Change in life-expectancy from 1980',
                     breaks = seq(-2, 10, 1),
                     limits = c(-2, 10)) +
  guides(color = FALSE) +
  theme_classic() +
  theme(panel.grid.major.y =
          element_line(color = 'grey90', linetype = 'dotted'))

#'## Set the aspect ratio

ggplot(e0, aes(x = year, y = e0std)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = 'grey90', alpha = 0.5) +
  geom_line(aes(group = cntry), color = 'grey90', data = e0_nato) +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels, aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous('Year', breaks = c(seq(1980, 2010, 5), 2013),
                     limits = c(1980, 2020)) +
  scale_y_continuous('Change in life-expectancy from 1980',
                     breaks = seq(-2, 10, 1),
                     limits = c(-2, 10)) +
  guides(color = FALSE) +
  theme_classic() +
  theme(panel.grid.major.y =
          element_line(color = 'grey90', linetype = 'dotted')) +
  coord_fixed(2, expand = FALSE)

#ggsave(filename = 'pe0.pdf', width = unit(10, 'cm'), height = unit(5, 'cm'))
