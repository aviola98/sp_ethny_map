library(tidyverse)
library(geobr)
library(tidylog)
library(sf)
library(purrr)
library(crul)
library(readxl)

#downloading SP data from geombr
SP_municipalities <- read_municipality(code_muni = "SP",
                                       year = 2018)

#changing the case of name_muni for future purpouses 
SP_municipalities <-
SP_municipalities %>%
  mutate(toupper(name_muni))
  
#opening candidates database
candidates <- read_excel("origem.xlsx")

#changing the names in order to do the join operation and filtering the data for SP only
candidates_SP <- candidates %>%
  filter(UF == "SP") %>%
  rename("toupper(name_muni)" = "CIDADE",
         "abbrev_state" = "UF") 
 
#joining databases 
sp_ethnic_data <- SP_municipalities %>%
  left_join(candidates_SP,
            c("toupper(name_muni)",
                "abbrev_state"))

#translating from portuguese to english 
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

#map
sp_ethnic_data %>%
  st_as_sf(coords=c("geom")) %>%
  ggplot() +
  geom_sf(aes(fill=origem))+
  ggtitle("Mapa da origem étnica") +
  theme_minimal()
