library(tidyverse)
library(forcats)
#install.packages("plotly")
library(plotly)

load('infant.RData')

#infant %>% View()

# NA ----------------------------------------------------------------------
a <- c(NA, 1, 4, NA)
sum(is.na(a))/length(a)

infant %>%
  summarise_all(
    funs(sum(is.na(.))/length(.))
  ) %>%
  gather() %>%
  ggplot(aes(x = key, y = value)) +
  geom_col() +
  coord_flip()



infant %>%
  group_by(date_of_delivery_y) %>%
  summarise_all(
    funs(sum(is.na(.))/length(.))
  ) %>%
  gather(... = -date_of_delivery_y) %>%
  ggplot(aes(x = key, y = value)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~date_of_delivery_y)
  
infant %>%
  group_by(date_of_delivery_y) %>%
  summarise_all(
    funs(sum(is.na(.))/length(.))
  ) %>%
  gather(... = -date_of_delivery_y) %>%
  ggplot(aes(x = date_of_delivery_y,
             y = value)) +
  geom_line(aes(color = key))
  #ggplotly()
  
#' - missings increase after 2005
#' - missings in education of mother

infant %>%
  group_by(race_and_hispanic_orig_of_mother_c4) %>%
  summarise_all(
    funs(sum(is.na(.))/length(.))
  ) %>%
  gather(... = -race_and_hispanic_orig_of_mother_c4) %>%
  ggplot(aes(x = value, y = key,
             color = race_and_hispanic_orig_of_mother_c4)) +
  geom_point()

#' - no sex specific missings
#' - missings dependend on race and origin

# Infant mortality --------------------------------------------------------

infant <-
  infant %>%
  mutate(death = ifelse(is.na(age_at_death_d), 0, 1))

infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(imr = sum(death)/n())

# aggregate(death ~ date_of_delivery_y,
#           FUN = function (x) {sum(x)/length(x)},
#           data = infant)

infant %>%
  group_by(date_of_delivery_ym) %>%
  summarise(imr = sum(death)/n()) %>%
  ggplot(aes(x = date_of_delivery_ym, y = imr)) +
  geom_line() +
  geom_smooth()


infant %>%
  group_by(date_of_delivery_ym, sex) %>%
  summarise(imr = sum(death)/n()) %>%
  ggplot(aes(x = date_of_delivery_ym, y = imr,
             color = sex)) +
  geom_line() +
  geom_smooth()


infant %>%
  group_by(date_of_delivery_ym, sex) %>%
  summarise(imr = sum(death)/n()) %>%
  ggplot(aes(x = date_of_delivery_ym, y = imr,
             color = sex)) +
  geom_line() +
  geom_smooth()


infant %>%
  filter(
    !is.na(education_of_mother_c2),
    !is.na(race_and_hispanic_orig_of_mother_c2)
  ) %>%
  group_by(date_of_delivery_y, sex,
           education_of_mother_c2,
           race_and_hispanic_orig_of_mother_c2) %>%
  summarise(imr = sum(death)/n()) %>%
  ggplot(aes(x = date_of_delivery_y, y = imr,
             color = sex)) +
  geom_line() +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_grid(education_of_mother_c2 ~ race_and_hispanic_orig_of_mother_c2)

# Gestation at birth ------------------------------------------------------

infant %>%
  ggplot() +
  geom_histogram(aes(x = gestation_at_delivery_w,
                     y = ..density..)) +
  facet_wrap(~date_of_delivery_y)

a <- 1:6
sum(a>2)/length(a)


infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(
    p = sum(gestation_at_delivery_w > 42,
            na.rm = TRUE)/n()) %>%
  ggplot(aes(x = date_of_delivery_y,
             y = p)) +
  geom_line()
ggplotly()

infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(
    p = sum(gestation_at_delivery_w < 38,
            na.rm = TRUE)/n()) %>%
  ggplot(aes(x = date_of_delivery_y,
             y = p)) +
  geom_line()
ggplotly()


infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(
    p = sum(gestation_at_delivery_w == 39,
            na.rm = TRUE)/n()) %>%
  ggplot(aes(x = date_of_delivery_y,
             y = p)) +
  geom_line()
ggplotly()


infant %>%
  group_by(date_of_delivery_y) %>%
  summarise(
    p = sum(gestation_at_delivery_w == 40,
            na.rm = TRUE)/n()) %>%
  ggplot(aes(x = date_of_delivery_y,
             y = p)) +
  geom_line()
ggplotly()

#' - coding issue gestation at birth 2003?

# Level and shape of infant mortality -------------------------------------

c(diff(c(1, 5, 6, 7, 9)), NA)

infant %>%
  group_by(date_of_delivery_ym) %>%
  summarise(imr = sum(death)/n(),
            p = sum(age_at_death_d < 7,
                    na.rm = TRUE)/sum(death)) %>%
  mutate(diff_imr = c(diff(imr), NA),
         diff_p = c(diff(p), NA)) %>%
  ggplot() +
  geom_point(aes(x = diff_imr, y = diff_p))
