library(tidyverse)
library(geobr)
library(tidylog)
library(sf)
library(purrr)
library(crul)
library(readxl)

SP_municipalities <- read_municipality(code_muni = "SP",
                                       year = 2018)

SP_municipalities <-
SP_municipalities %>%
  mutate(toupper(name_muni))
  
candidates <- read_excel("origem.xlsx")

candidates_SP <- candidates %>%
  filter(UF == "SP") %>%
  rename("toupper(name_muni)" = "CIDADE",
         "abbrev_state" = "UF") 
  
sp_ethnic_data <- SP_municipalities %>%
  left_join(candidates_SP,
            c("toupper(name_muni)",
                "abbrev_state"))

sp_ethnic_data <-
sp_ethnic_data %>%
  mutate(origem = case_when(origem == "Ibérico" ~ "Iberian",
                            origem == "Italiano" ~ "Italian",
                            origem == "Oriente Médio" ~ "Syrian-Lebanese",
                            origem == "Oriente médio" ~ "Syrian-Lebanese",
                            origem == "Germânico" ~ "German",
                            origem == "Eslavo" ~ "Slav",
                            origem == "Japonês" ~ "Japanese",
                            origem == "Francês" ~ "French",
                            TRUE ~ origem))

sp_ethnic_data %>%
  filter(origem == "Japanese")

sp_ethnic_data %>%
  st_as_sf(coords=c("geom")) %>%
  ggplot() +
  geom_sf(aes(fill=origem))+
  ggtitle("Mapa da origem étnica") +
  theme_minimal()
