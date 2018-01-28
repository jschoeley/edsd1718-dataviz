# Init --------------------------------------------------------------------

library(HMDHFDplus)
library(dplyr)
library(ggplot2)

# HMD credentials
hmd_username <- "***"
hmd_password <- "***"

# Download HMD Period Lifetables ------------------------------------------

hmd_cntry <- getHMDcountries()

# download female and male HMD lifetables and save in long format
data_frame(cntry = hmd_cntry) %>%
  # for each country...
  group_by(cntry) %>%
  do(
    {
      # ...download female lifetables
      female_lt <- readHMDweb(CNTRY = .$cntry,
                              username = hmd_username,
                              password = hmd_password,
                              item = "fltper_1x1")
      # ...download male lifetables
      male_lt <- readHMDweb(CNTRY = .$cntry,
                            username = hmd_username,
                            password = hmd_password,
                            item = "mltper_1x1")
      # ...combine female and male lifetables into long format data frame
      bind_rows(mutate(female_lt, sex = "female"),
                mutate(male_lt, sex = "male"))
    }
  ) -> hmd_lt

#save(hmd_lt, file = "./priv/data/hmd_lt.Rdata")
load("./priv/data/hmd_lt.Rdata")

# Lifespan Equality Functions ---------------------------------------------

#' Life Expectancy Lost at Age x due to Death
#'
#' @details Assuming single year age groups and an open ended
#' last age group.
EDaggerx <- function (x, ax = 0.5, ex) {
  c(1 - ax[-length(ax)] + ex[-1],
    ex[length(ex)])
}

#' Total Life Expectancy Lost due to Death
EDagger <- function (dx, edaggerx) {
  sum(dx*edaggerx)
}

#' Keyfitz's Entropy
KeyfzEntro <- function (edagger, e0) {
  edagger / e0
}

# Summarise Lifetables by e0 and Lifespan Equality ------------------------

# country codes and names
cntry_code_name <-
  c("Australia" = "AUS",
    "Austria" = "AUT",
    "Belarus" = "BLR",
    "Belgium" = "BEL",
    "Bulgaria" = "BGR",
    "Canada" = "CAN",
    "Chile" = "CHL",
    "Czech Republic" = "CZE",
    "Denmark" = "DNK",
    "East Germany" = "DEUTE",
    "England & Wales" = "GBRTENW",
    "Estonia" = "EST",
    "Finland" = "FIN",
    "France" = "FRATNP",
    "Greece" = "GRC",
    "Hungary" = "HUN",
    "Iceland" = "ISL",
    "Ireland" = "IRL",
    "Israel" = "ISR",
    "Italy" = "ITA",
    "Japan" = "JPN",
    "Latvia" = "LVA",
    "Lithuania" = "LTU",
    "Luxembourg" = "LUX",
    "Netherlands" = "NLD",
    "New Zealand" = "NZL_NP",
    "Northern Ireland" = "GBR_NIR",
    "Norway" = "NOR",
    "Poland" = "POL",
    "Portugal" = "PRT",
    "Russia" = "RUS",
    "Scotland" = "GBR_SCO",
    "Slovakia" = "SVK",
    "Slovenia" = "SVN",
    "Spain" = "ESP",
    "Sweden" = "SWE",
    "Switzerland" = "CHE",
    "Taiwan" = "TWN",
    "U.S.A." = "USA",
    "Ukraine" = "UKR",
    "West Germany" = "DEUTW")

hmd_lt %>% ungroup() %>%
  # remove duplicate populations
  filter(
    # Exclude the German total population data as it overlaps with data for
    # east and west Germany but has the shorter timeline.
    cntry != "DEUTNP",
    # Exclude French civil population data as it overlaps with total population
    # data and most country data is only available for total populations anyway.
    cntry != "FRACNP",
    # Exclude New Zealand Maori and Non-Maori population data as it overlaps
    # with total population data.
    cntry != "NZL_MA",
    cntry != "NZL_NM",
    # Exclude Great Britain total population and England & Wales civilian
    # population as they overlap with data from England & Wales, Northern
    # Ireland and Scotland.
    cntry != "GBR_NP",
    cntry != "GBRCENW") %>%
  mutate(cntry = factor(cntry, levels = cntry_code_name)) %>%
  group_by(cntry, Year, sex) %>%
  mutate(edaggerx = EDaggerx(x = Age, ax = ax, ex = ex)) %>%
  summarise(
    e0         = ex[Age == 0],
    edagger    = EDagger(dx/100000, edaggerx),
    keyfzentro = -log(KeyfzEntro(edagger, e0))
  ) %>% na.omit() -> hmd_lt_summary

# label countries with real name and period range
hmd_lt_summary %>% group_by(cntry) %>%
  # period data range for each country
  summarise(min_year = min(Year),
            max_year = max(Year),
            range    = paste0(min_year, "-", max_year)) -> hmd_lt_summary_period_range
hmd_lt_summary %>%
  mutate(cntry_name =
           factor(cntry,
                  levels = cntry_code_name,
                  labels = paste0(names(cntry_code_name),
                                  "\n",
                                  hmd_lt_summary_period_range$range))
  ) -> hmd_lt_summary

# order factor levels by maximum life expectancy
hmd_lt_summary$cntry_name <- with(hmd_lt_summary, reorder(cntry_name, e0, function (x) -max(x)))

# earliest year for each cntry*sex
hmd_lt_summary %>% group_by(cntry_name, sex) %>%
  slice(which.min(Year)) -> hmd_lt_summary_earliest
# most recent year for each cntry*sex
hmd_lt_summary %>% group_by(cntry_name, sex) %>%
  slice(which.max(Year)) -> hmd_lt_summary_latest

# Plot --------------------------------------------------------------------

# small multiple paths
ggplot(hmd_lt_summary, aes(x = e0, y = keyfzentro)) +
  geom_path(aes(colour = sex)) +
  geom_point(aes(colour = sex), fill = "white", size = 1.4, shape = 21,
             data = hmd_lt_summary_earliest) +
  geom_point(aes(fill = sex),
             colour = "black", size = 1.4, shape = 21,
             data = hmd_lt_summary_latest) +
  scale_colour_manual("", values = c(female = "#D23737", male = "#3191C9")) +
  scale_fill_manual("", values = c(female = "#D23737", male = "#3191C9")) +
  scale_x_continuous(expression(textstyle(Life~Expectancy)~e[0]),
                     breaks = range,
                     labels = function (x) format(x, digits = 0),
                     expand = c(0,0)) +
  scale_y_continuous(expression(textstyle(Lifespan~Equality)~e^d/e[0]),
                     breaks = range,
                     labels = function (x) format(x, digits = 1, nsmall = 1),
                     expand = c(0,0)) +
  facet_wrap(~cntry_name, scales = "free") +
  theme_classic() +
  theme(aspect.ratio = 1,
        legend.position = c(0.95, 0.1),
        strip.text = element_text(face = "bold"),
        strip.background = element_blank())

ggsave("./fig/timepaths.pdf",
       width = unit(10, "inch"), height = unit(10, "inch"))
