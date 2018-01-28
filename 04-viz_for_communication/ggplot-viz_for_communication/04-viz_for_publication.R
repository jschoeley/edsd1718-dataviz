#' ---
#' title: "Polishing your plot for publication. 10 principles and how to implement them in ggplot."
#' author: "Jonas Schöley"
#' date: "June 19, 2017"
#' output:
#'   github_document:
#'     toc: true
#' ---

#' Finishing a plot for publication usually starts with a graphic which was
#' hastily created during exploratory data analysis and polishing this graphic
#' so that it communicates a clear message to the readership.
#' 
#' There are several general guidelines which aid us in the process of making a
#' plot fit for publication.

#' - avoid clutter
#' - use highlighting
#' - use direct annotation
#' - show context
#' - use intuitive units
#' - spell it out
#' - properly size labels and data
#' - export with high resolution
#' - export as PDF or JPG
#' - consider a data-driven aspect-ratio

#'## Eastern European life-expectancy changes since 1980

e0 <-
  read_csv('ggplot_practical/data/e0.csv') %>%
  filter(cntry %in% c('BGR', 'CZE', 'HUN', 'POL', 'DEUTE'),
         sex == 'male', year >= 1980) %>%
  group_by(cntry) %>%
  mutate(e0std = e0 - e0[year == 1980])
  
ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry))

cntry_labels <-
  e0 %>%
  group_by(cntry) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(cntry = recode(cntry,
                        'BGR' = 'Bulgaria',
                        'CZE' = 'Czech Republic',
                        'DEUTE' = 'East Germany',
                        'HUN' = 'Hungary',
                        'POL' = 'Poland'))

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry)) +
  geom_text(aes(x = year, y = e0std, label = cntry), data = cntry_labels)

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry), show.legend = FALSE) +
  geom_text(aes(x = year, y = e0std, label = cntry), data = cntry_labels)

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry), show.legend = FALSE) +
  geom_text(aes(x = year, y = e0std, label = cntry), data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017))


ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry), show.legend = FALSE) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017))

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017)) +
  guides(color = FALSE)

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017)) +
  guides(color = FALSE) +
  theme_classic()

ggplot(e0, aes(x = year, y = e0std)) +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017)) +
  guides(color = FALSE) +
  theme_classic() +
  theme(panel.grid.major.y =
          element_line(color = 'grey90', linetype = 'dotted'),
        panel.grid.minor.y =
          element_line(color = 'grey90', linetype = 'dotted'))


ggplot(e0, aes(x = year, y = e0std)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = 'grey90') +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels) +
  geom_text(aes(x = year, y = e0std, label = cntry),nudge_y = 0.3,
            data = cntry_labels) +
  scale_x_continuous(limits = c(1980, 2017)) +
  guides(color = FALSE) +
  theme_classic() +
  theme(panel.grid.major.y =
          element_line(color = 'grey90', linetype = 'dotted'),
        panel.grid.minor.y =
          element_line(color = 'grey90', linetype = 'dotted'))


ggplot(e0, aes(x = year, y = e0std)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = 'grey90') +
  geom_line(aes(color = cntry)) +
  geom_point(data = cntry_labels) +
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

e0_nato <-
  read_csv('ggplot_practical/data/e0.csv') %>%
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
  geom_point(data = cntry_labels) +
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

--------------------------------------------------------------------------------

#'## Avoid clutter
#'
#' The data should be the most prominent element on your plot. While scales,
#' labels and annotation are important they should not distract from the data.
#' Here's a plot with annotations so prominent, that they inhibit a clear view
#' of the data.

library(tidyverse)

colchero <- read_csv('ggplot_practical/data/colchero.csv')
colchero$label <- str_replace(colchero$label, fixed('\\n'), replacement = '\n')

ggplot(colchero, aes(e0_f, s0_f)) +
  ggrepel::geom_text_repel(aes(label = label)) +
  geom_point()

ggplot(colchero, aes(e0_f, s0_f)) +
  ggrepel::geom_text_repel(aes(label = label), size = 3) +
  geom_point()


ggplot(colchero, aes(e0_f, s0_f)) +
  ggrepel::geom_text_repel(aes(label = label), size = 3, color = 'grey70') +
  geom_point()

ggplot(colchero, aes(e0_f, s0_f)) +
  ggrepel::geom_text_repel(aes(label = label), size = 3, color = 'grey70') +
  geom_point() +
  theme_classic()


ggplot(colchero, aes(e0_f, s0_f)) +
  ggrepel::geom_text_repel(aes(label = label), size = 3, color = 'grey70') +
  geom_point() +
  theme_classic() +
  theme(panel.grid.major.y =
          element_line(color = 'grey90', linetype = 'dotted'),
        panel.grid.minor.y =
          element_line(color = 'grey90', linetype = 'dotted'))

--------------------------------------------------------------------------------

#'## Highlight important information

ageing <- read_csv('ggplot_practical/data/ageing.csv', comment = '#')

ggplot(ageing, aes(x = year)) +
  geom_line(aes(y = total_under30)) +
  geom_line(aes(y = total_over60)) +
  theme_classic()

ggplot(ageing, aes(x = year)) +
  annotate('rect', xmin = 2014, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = 'grey90') +
  annotate('text', x = 2015, y = 47, label = 'projected',
           color = 'grey50', hjust = 0) +
  geom_line(aes(y = total_under30)) +
  geom_line(aes(y = total_over60)) +
  theme_classic()

--------------------------------------------------------------------------------

#'## Contextualize information

sweden <- read_csv('ggplot_practical/data/sweden.csv')

sweden_past_today <- filter(sweden, Year %in% range(Year))

ggplot(sweden_past_today, aes(x = Age, group = Year)) +
  geom_line(data = filter(sweden_past_today, Sex == 'Female'), aes(y = Nx)) +
  geom_line(data = filter(sweden_past_today, Sex == 'Male'), aes(y = -Nx)) +
  coord_flip() +
  theme_minimal()

sweden_in_between <- filter(sweden, Year %in% seq(1800, 2000, 50))

ggplot(sweden_past_today, aes(x = Age, group = Year)) +
  geom_line(data = filter(sweden_in_between, Sex == 'Female'),
            color = 'grey70', aes(y = Nx)) +
  geom_line(data = filter(sweden_in_between, Sex == 'Male'),
            color = 'grey70', aes(y = -Nx)) +
  geom_line(data = filter(sweden_past_today, Sex == 'Female'),
            size = 1, aes(y = Nx)) +
  geom_line(data = filter(sweden_past_today, Sex == 'Male'),
            size = 1, aes(y = -Nx)) +
  coord_flip() +
  theme_minimal()

--------------------------------------------------------------------------------

#'## Use intuitive units

logage <- read_csv('ggplot_practical/data/logage.csv', comment = '#')

#' It's common to use log-transforms when fitting a model. Here's an example of
#' fitting a linear regression to remaining life-expectancy with log-age as the
#' independent variable.

logage$fit <- lm(ex~logage, logage)$fitted

#' A lazy way to plot the result is to use the untransformed log-values on the
#' x-axis, requiring the reader to exponentiate the x-axis labels to interpret
#' them.

ggplot(logage, aes(x = logage)) +
  geom_point(aes(y = ex)) +
  geom_line(aes(y = fit))

#' The plot becomes much easier to read by using a logged x-scale with standard
#' age labels. Instead of letting the reader to the exponentiation, we took care
#' of it.

ggplot(logage, aes(x = exp(logage))) +
  geom_point(aes(y = ex)) +
  geom_line(aes(y = fit)) +
  scale_x_continuous("Age", breaks = seq(50, 90, 5), trans = 'log')

#' In this case one could also consider using a linear age scale with standard
#' age labels. This model shows the curvature in the model and in the data.

ggplot(logage, aes(x = exp(logage))) +
  geom_point(aes(y = ex)) +
  geom_line(aes(y = fit)) +
  scale_x_continuous("Age", breaks = seq(50, 90, 5))

--------------------------------------------------------------------------------

#'## Properly size labels and data

--------------------------------------------------------------------------------

#'## Export with high resolution

--------------------------------------------------------------------------------

#'## Avoid JPG for graphics

--------------------------------------------------------------------------------

#'## Use proper mathematical notation

--------------------------------------------------------------------------------

#'## Present complex plots cumulatively

--------------------------------------------------------------------------------

#'## Spell it out

--------------------------------------------------------------------------------

#'## Choose a proper aspect ratio
