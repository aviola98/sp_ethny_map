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

