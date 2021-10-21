library(tidyverse)
library(geobr)
library(tidylog)
library(sf)
library(purrr)
library(crul)
library(readxl)
library(RColorBrewer)

estados <- c("PR","SC","RS")

#Sul <-  estados %>%
  #map(read_municipality, 2018)

pr <- read_municipality(code_muni = "PR",
                        year = 2018)
pr <- pr %>%
  mutate(toupper(name_muni))

sc <- read_municipality(code_muni = "SC",
                        year = 2018)
sc <- sc %>%
  mutate(toupper(name_muni))

rs <- read_municipality(code_muni = "RS",
                        year = 2018)
rs<- rs %>%
  mutate(toupper(name_muni))


candidates <- read_excel("origem.xlsx")


pr_cand <- candidates %>%
  filter(UF == "PR") %>%
  rename("toupper(name_muni)" = "CIDADE",
         "abbrev_state" = "UF") 

pr_ethnic_data <- pr %>%
  left_join(pr_cand,
            c("toupper(name_muni)",
              "abbrev_state"))

pr_ethnic_data <-
  pr_ethnic_data %>%
  mutate(origem = case_when(origem == "Ibérico" ~ "Iberian",
                            origem == "Italiano" ~ "Italian",
                            origem == "Oriente Médio" ~ "Syrian-Lebanese",
                            origem == "Oriente médio" ~ "Syrian-Lebanese",
                            origem == "Germânico" ~ "German",
                            origem == "Eslavo" ~ "Slav",
                            origem == "Japonês" ~ "Japanese",
                            origem == "Francês" ~ "French",
                            TRUE ~ origem))

pr_ethnic_data %>%
  st_as_sf(coords=c("geom")) %>%
  ggplot() +
  geom_sf(aes(fill=origem))+
  ggtitle("Mapa da origem étnica") +
  theme_minimal()

sc_cand <- candidates %>%
  filter(UF == "SC") %>%
  rename("toupper(name_muni)" = "CIDADE",
         "abbrev_state" = "UF") 

sc_ethnic_data <- sc %>%
  left_join(sc_cand,
            c("toupper(name_muni)",
              "abbrev_state"))

sc_ethnic_data <-
  sc_ethnic_data %>%
  mutate(origem = case_when(origem == "Ibérico" ~ "Iberian",
                            origem == "Italiano" ~ "Italian",
                            origem == "Oriente Médio" ~ "Syrian-Lebanese",
                            origem == "Oriente médio" ~ "Syrian-Lebanese",
                            origem == "Germânico" ~ "German",
                            origem == "Eslavo" ~ "Slav",
                            origem == "Japonês" ~ "Japanese",
                            origem == "Francês" ~ "French",
                            TRUE ~ origem))

sc_ethnic_data %>%
  st_as_sf(coords=c("geom")) %>%
  ggplot() +
  geom_sf(aes(fill=origem))+
  ggtitle("Mapa da origem étnica") +
  theme_minimal()

rs_cand <- candidates %>%
  filter(UF == "RS") %>%
  rename("toupper(name_muni)" = "CIDADE",
         "abbrev_state" = "UF") 

rs_ethnic_data <- rs %>%
  left_join(rs_cand,
            c("toupper(name_muni)",
              "abbrev_state"))

rs_ethnic_data <-
  rs_ethnic_data %>%
  mutate(origem = case_when(origem == "Ibérico" ~ "Iberian",
                            origem == "Italiano" ~ "Italian",
                            origem == "Oriente Médio" ~ "Syrian-Lebanese",
                            origem == "Oriente médio" ~ "Syrian-Lebanese",
                            origem == "Germânico" ~ "German",
                            origem == "Eslavo" ~ "Slav",
                            origem == "Japonês" ~ "Japanese",
                            origem == "Francês" ~ "French",
                            TRUE ~ origem))

colors1 <- colorRampPalette(c("red", "green", "orange","blue","black","pink","yellow",
                                         "beige","grey"))(groups) 

rs_ethnic_data %>%
  st_as_sf(coords=c("geom")) %>%
  ggplot() +
  geom_sf(aes(fill=origem))+
  ggtitle("Mapa da origem étnica") +
  theme_minimal() +
  scale_color_brewer(palette="Dark2")

View(rs_cand %>%
       group_by(origem,NM_CANDIDATO) %>%
       summarise(origem))



View(sc_cand %>%
  group_by(origem,NM_CANDIDATO) %>%
  summarise(origem))
