library(tidyverse)
library(geobr)
library(tidylog)
library(sf)
library(purrr)
library(crul)
library(readxl)

ba <- read_municipality(code_muni = "BA",
                        year = 2018)

ba <- ba %>%
  mutate(toupper(name_muni))

candidates <- read_excel("origem.xlsx")

ba_cand <- candidates %>%
  filter(UF == "BA") %>%
  rename("toupper(name_muni)" = "CIDADE",
         "abbrev_state" = "UF") 

ba_ethnic_data <- ba %>%
  left_join(ba_cand,
            c("toupper(name_muni)",
              "abbrev_state"))

ba_ethnic_data <-
ba_ethnic_data %>%
  mutate(origem = case_when(origem == "Ibérico" ~ "Iberian",
                            origem == "Italiano" ~ "Italian",
                            origem == "Oriente Médio" ~ "Syrian-Lebanese",
                            origem == "Oriente médio" ~ "Syrian-Lebanese",
                            origem == "Germânico" ~ "German",
                            origem == "Eslavo" ~ "Slav",
                            origem == "Japonês" ~ "Japanese",
                            origem == "Francês" ~ "French",
                            TRUE ~ origem))

ba_ethnic_data %>%
  st_as_sf(coords=c("geom")) %>%
  ggplot() +
  geom_sf(aes(fill=origem))+
  ggtitle("Mapa da origem étnica") +
  theme_minimal()