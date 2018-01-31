# Init --------------------------------------------------------------------

library(tidyverse)
library(forcats)
library(plotly)

load('infant.RData')

infant %>% View()

infant <-
  infant %>%
  mutate(death = ifelse(is.na(age_at_death_d), FALSE, TRUE))

# NAs ---------------------------------------------------------------------

infant %>%
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather() %>%
  ggplot(aes(x = fct_reorder(key, value), y = value)) +
  geom_col() +
  coord_flip()

infant %>%
  group_by(date_of_delivery_y) %>%
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(... = -date_of_delivery_y) %>%
  ggplot(aes(x = key, y = value)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~date_of_delivery_y)

infant %>%
  group_by(date_of_delivery_y) %>%
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(... = -date_of_delivery_y) %>%
  ggplot(aes(x = date_of_delivery_y, y = value)) +
  geom_line(aes(color = key))
ggplotly()

infant %>%
  group_by(sex) %>%
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(... = -sex) %>%
  ggplot(aes(x = key, y = value)) +
  geom_point(aes(color = sex)) +
  coord_flip()

infant %>%
  group_by(race_and_hispanic_orig_of_mother_c4) %>%
  summarise_all(funs(sum(is.na(.))/n())) %>%
  gather(... = -race_and_hispanic_orig_of_mother_c4) %>%
  ggplot(aes(x = key, y = value)) +
  geom_point(aes(color = race_and_hispanic_orig_of_mother_c4)) +
  coord_flip()

#' - Missing in `age_at_death` means survival
#' - prominent missing in:
#'   - alcohol, tobacco use
#'   - birth injury
#'   - prolonged labor
#'   - education of mother
#'   - prenatal care
#' - Missings depend on year and origin of mother

# Gestation at birth ------------------------------------------------------

infant %>%
  ggplot(aes(x = gestation_at_delivery_w)) +
  geom_histogram(aes(y = ..density..)) +
  facet_wrap(~date_of_delivery_y)

infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(p = sum(gestation_at_delivery_w > 42, na.rm = TRUE)/n()) %>% 
  ggplot(aes(x = date_of_delivery_y, y = p)) +
  geom_line() +
  geom_point()

infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(p = sum(gestation_at_delivery_w < 38, na.rm = TRUE)/n()) %>% 
  ggplot(aes(x = date_of_delivery_y, y = p)) +
  geom_line() +
  geom_point()

infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(p = sum(gestation_at_delivery_w == 40, na.rm = TRUE)/n()) %>% 
  ggplot(aes(x = date_of_delivery_y, y = p)) +
  geom_line() +
  geom_point()

# Sex ratio at birth ------------------------------------------------------

infant %>%
  group_by(date_of_delivery_y, race_and_hispanic_orig_of_mother_c4) %>%
  summarise(sr = sum(sex == 'Male')/sum(sex == 'Female')) %>% 
  ggplot(aes(x = date_of_delivery_y, y = sr)) +
  geom_line(aes(color = race_and_hispanic_orig_of_mother_c4))

infant %>%
  group_by(race_and_hispanic_orig_of_mother_c4,
           education_of_mother_c4) %>%
  summarise(sr = sum(sex == 'Male')/sum(sex == 'Female')) %>%
  ggplot(aes(x = education_of_mother_c4, y = sr)) +
  geom_point(aes(color = race_and_hispanic_orig_of_mother_c4)) +
  coord_flip() +
  scale_y_continuous(limits = c(0.9 ,1.1))

# Infant mortality rates by year and strata -------------------------------

infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(mx = sum(death)/n())

# aggregate(death ~ date_of_delivery_y,
#           FUN = function (x) {sum(x)/length(x)},
#           data = infant)

infant %>%
  group_by(date_of_delivery_ym) %>%
  summarise(mx = sum(death)/n()) %>%
  ggplot(aes(x = date_of_delivery_ym, y = mx)) +
  geom_line() +
  geom_smooth()

#' Linear downward trend in infant mortality.

infant %>%
  group_by(date_of_delivery_ym, sex) %>%
  summarise(mx = sum(death)/n()) %>%
  ggplot(aes(x = date_of_delivery_ym, y = mx, color = sex)) +
  geom_line() +
  geom_smooth()

#' - downward trend symmetric for both sexes
#' - higher male infant mortality

# seasonality?
infant %>%
  group_by(date_of_delivery_y, date_of_delivery_m) %>%
  summarise(mx = sum(death)/n()) %>%
  ggplot(aes(x = date_of_delivery_m, y = mx,
             color = as.factor(date_of_delivery_y))) +
  geom_smooth(se = FALSE)

#' No apparent seasonality in infant death rates.

#' Deaton style analysis
infant %>%
  filter(!is.na(race_and_hispanic_orig_of_mother_c2),
         !is.na(education_of_mother_c2)) %>%
  group_by(date_of_delivery_y,
           sex,
           race_and_hispanic_orig_of_mother_c2,
           education_of_mother_c2) %>%
  summarise(mx = sum(death)/n()) %>%
  ggplot(aes(x = date_of_delivery_y, y = mx, color = sex)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(education_of_mother_c2~race_and_hispanic_orig_of_mother_c2,
             scales = 'free_y')
  # facet_wrap(education_of_mother_c2~race_and_hispanic_orig_of_mother_c2) +
  # facet_grid(education_of_mother_c2~race_and_hispanic_orig_of_mother_c2)

infant %>%
  filter(!is.na(race_and_hispanic_orig_of_mother_c2),
         !is.na(education_of_mother_c2)) %>%
  group_by(date_of_delivery_y,
           sex,
           race_and_hispanic_orig_of_mother_c2,
           education_of_mother_c2) %>%
  summarise(mx = sum(death)/n()) %>%
  group_by(sex,
           race_and_hispanic_orig_of_mother_c2,
           education_of_mother_c2) %>%
  do(broom::tidy(lm(mx~date_of_delivery_y, data = .))) %>%
  filter(term == 'date_of_delivery_y') %>%
  ggplot(aes(y = estimate,
             x = interaction(sex,
                             race_and_hispanic_orig_of_mother_c2,
                             education_of_mother_c2))) +
  geom_pointrange(aes(ymin = estimate-1.96*std.error,
                      ymax = estimate+1.96*std.error)) +
  coord_flip()
  
# Relationship between level and distribution of infant mortality ---------

infant %>%
  group_by(date_of_delivery_ym) %>%
  summarise(imr = sum(death)/n(),
            p_neo = sum(age_at_death_d <= 14, na.rm = TRUE)/sum(death)) %>%
  mutate(diff_imr = c(diff(imr), NA),
         diff_p_neo = c(diff(p_neo), NA)) %>%
  ggplot(aes(x = diff_imr, y = diff_p_neo)) +
  geom_point()

# p-hacking
infant %>%
  group_by(date_of_delivery_ym, race_and_hispanic_orig_of_mother_c4) %>%
  summarise(imr = sum(death)/n(),
            p_neo = sum(age_at_death_d <= 14, na.rm = TRUE)/sum(death)) %>%
  mutate(diff_imr = c(diff(imr), NA),
         diff_p_neo = c(diff(p_neo), NA)) %>%
  ggplot(aes(x = diff_imr, y = diff_p_neo)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~race_and_hispanic_orig_of_mother_c4)
